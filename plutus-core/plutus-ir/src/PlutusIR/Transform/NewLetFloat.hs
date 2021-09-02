{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -W -Wwarn=unused-top-binds #-}
module PlutusIR.Transform.NewLetFloat (floatTerm) where

import           Control.Lens            hiding (Strict)
import           Data.Set.Lens              (setOf)
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Coerce
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import           Data.Semigroup.Foldable
import qualified Data.Set                as S
import           GHC.Exts
import qualified PlutusCore              as PLC
import qualified PlutusCore.Constant     as PLC
import qualified PlutusCore.Name         as PLC
import           PlutusIR
import           PlutusIR.Purity
import           PlutusIR.Subst


{- Note [Let Floating pass]

The goal of this pass is to move (float) let-bindings as outwards as possible,
without breaking the scoping & meaning of the original PIR term.

This transformation (a.k.a. full laziness), together with a possible implementation
is described in Peyton Jones, Simon, Will Partain, and Andre Santos. "Let-Floating: Moving Bindings to Give Faster Programs."
In Proceedings of the First ACM SIGPLAN International Conference on Functional Programming, 1-12.
ICFP '96. New York, NY, USA: ACM, 1996. https://doi.org/10.1145/232627.232630.

An implementation, as described in the paper, is comprised of two "passes":

1) a "mark" pass to travers the term tree and
  - in case of lam/Lam, mark this lam/Lam name with current depth, and
    increase the depth for the lam/Lam's-abstraction body term and recurse.
  - in case of a Letrecgroup, collect the free term&type variables and mark every let-introduced name
    with the maximum depth among all the free variables (the free variables should be already marked)
  - in case of letnonrec group, you can treat it the same as (letrec g in letrec gs)

2) a "float-back" pass which, given the collected marks,
   traverses the term tree again and whenever a let(rec or nonrec) is encountered,
   decides locally if it is worth to float the current let outwards at its marked depth.
   If yes, the let-group's binding is floated exactly outside a lambda abstraction that has lam_depth=let_depth+1

There are some  differences with the paper's described implementation above, namely:

a) we use 3 passes. the 1st pass is similar to the original; a second pass
"cleans" the term from all the to-be-floated lets and stores them in a separate table.
the 3rd pass is responsible to float back the removed lets inside the reducted (cleaned) term
according to their markers. So we use an extra pass because we float back lets in a global fashion,
instead of deciding locally.

b) Since the 3rd (float-back) pass operates on a reducted (cleaned) term, we have lost
the original location of the lets, so we cannot float them "right outside" the **maximum-independent lambda-abstraction**,
but we float them "right inside" the maximum **dependent** lambda-abstraction's body. This has the downside
of allocating&holding the lets for longer than needed, but will not alter the meaning of the original PIR term.

c) Since PIR has strict (compared to the paper's lazy-only lang), we have to make
sure that any let-group containing at least one **effectful** (i.e. non-pure) strict binding is
not floated at all. This does not mean that such an "effectful" let
will appear in the same absolute location as the original term: an outside/parent let may float around,
changing the child's (effectful let) absolute location; however, the child's relative location to the parent *must* remain the same.
See also 'unmovable'.

Since another let variable may depend on such *effectful* let and to preserve the execution order,
we treat an effectful let the same as lam/Lam "anchor", by increasing the depth both on entering any of its rhs'ses
*and* inside its inTerm. See also 'PosType'. Thus, the dependent let will be etierh floated right together with a depending-let group or
right inside its depending-let inTerm.
-}

-- | Position of a marker.
-- 1) Since we act globally the depth is not enough anymore (not globally unique)
-- and for that we also use a "represenative" identifier (PLC.Unique).
-- 2) Since we have lets as anchors, we also need the extra 'PosType' to signify
-- the let's marker position (at rhs or at inTerm).
type Pos = (Depth
           , PLC.Unique -- The lambda name or Lambda tyname or Let's representative unique
           , PosType
           )

data PosType = LamBody -- ^ big, small lam or let body
             | LetRhs -- ^ let rhs or top
             deriving (Eq, Ord, Show)


-- Arbitrary: return a unique from the introduced uniques of the given letgroup.
representativeBindingUnique
    :: (PLC.HasUnique name PLC.TermUnique, PLC.HasUnique tyname PLC.TypeUnique)
    => NE.NonEmpty (Binding tyname name uni fun a) -> PLC.Unique
representativeBindingUnique =
    -- Arbitrary: select the first unique from the representative binding
    head . toListOf bindingIds . representativeBinding
  where
      --  Arbitrary: a binding to be used a representative binding in MARKING the group of bindings.
      representativeBinding :: NE.NonEmpty (Binding tyname name uni fun a) -> Binding tyname name uni fun a
      representativeBinding = NE.head


-- | The first pass has a reader context of current depth, and (term&type)variables in scope.
type MarkCtx = (Depth, Scope)
type Depth = Int

-- Every term and type variable in current scope
-- is paired with its own computed marker (maximum dependent position)
-- OPTIMIZE: use UniqueMap instead
type Scope = M.Map PLC.Unique Pos

-- The result of the first pass is the union (superset) of all computed scopes.
-- This is larger than needed, because we don't really care about lambda/Lambda markers
-- in the later passes, but for simplicity we keep it.
type Marks = M.Map PLC.Unique Pos

data LetProduct tyname name uni fun a =
    -- same as 'PlutusIR.Core.Type.Term' 's Let datacontructor
    LetProduct a Recursivity (NE.NonEmpty (Binding tyname name uni fun a))  (Term tyname name uni fun a)

data LetNaked tyname name uni fun a = LetNaked {
    _letNAnn :: a,
    _letNRec :: Recursivity,
    _letNBs :: NE.NonEmpty (Binding tyname name uni fun a)
    }
makeLenses ''LetNaked

strip :: LetProduct tyname name uni fun a -> LetNaked tyname name uni fun a
strip (LetProduct a r bs _) = LetNaked a r bs

mark :: forall tyname name uni fun a.
      (Ord tyname, Ord name, PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique, PLC.ToBuiltinMeaning uni fun)
     => Term tyname name uni fun a -> Marks
mark = flip runReader initCtx . go
    where
      initCtx :: MarkCtx
      initCtx = (topDepth, mempty)

      go :: Term tyname name uni fun a -> Reader MarkCtx Marks
      go = \case
          -- lam/Lam are treated the same.
          LamAbs _ n _ tBody  -> withLam n $ go tBody
          TyAbs _ n _ tBody   -> withLam n $ go tBody

          -- break down let nonrecs
          -- by rule: {let nonrec (b:bs) in t} === {let nonrec b in let nonrec bs in t}
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)

          -- main operation.
          Let ann r bs tIn -> do
            (depth, scope) <- ask
            let letP = LetProduct ann r bs tIn
                letU = representativeBindingUnique bs
            if unmovable letP
              then do
                  -- since it is unmovable, the floatpos is a new anchor
                  let newDepth = depth+1
                      toPos = (newDepth, letU,)

                  marked1 <- withDepth (+1) $
                               ifRec r (withBs bs $ toPos LetRhs) $
                                  mconcat <$> traverse go (bs^..traversed.bindingSubterms)

                  marked2 <- withDepth (+1) $
                      withBs bs (toPos LamBody) $ go tIn

                  -- don't add any marks
                  pure $ marked1 <> marked2

              else do
                  let letN = strip letP

                      -- if Rec then here-bindings are not free
                      freeVars = ifRec r (S.\\ setOf (traversed.bindingIds) bs) $
                                    calcFreeVars letN

                      -- the heart of the algorithm: the future position to float this let to,
                      -- is determined as the maximum among its dependencies (free vars).
                      newPos@(newD,_,_) =  maxPos $ M.restrictKeys scope freeVars

                  marks1 <- withDepth (const newD) $
                             ifRec r (withBs bs newPos) $
                               mconcat <$> traverse go (bs^..traversed.bindingSubterms)

                  marks2 <- withBs bs newPos $
                      go tIn

                  -- add here a new mark
                  pure $ M.singleton letU newPos <> marks1 <> marks2

          t -> mconcat <$> traverse go (t^..termSubterms)

      -- A helper to apply a function iff recursive
      ifRec :: Recursivity -> (b -> b) -> b -> b
      ifRec = \case
          Rec -> ($)
          NonRec -> id




calcFreeVars :: forall tyname name uni fun a.
             (Ord tyname, Ord name, PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
             => LetNaked tyname name uni fun a
             -> S.Set PLC.Unique
calcFreeVars (LetNaked _ r bs) = foldMap1 calcBinding bs
    where
      -- given a binding return all its free term *AND* free type variables
      calcBinding :: Binding tyname name uni fun a -> S.Set PLC.Unique
      calcBinding b =
           -- OPTIMIZE: safe to change to S.mapMonotonic?
           S.map (^.PLC.theUnique) (fvBinding b)
          <> S.map (^.PLC.theUnique) (ftvBinding r b)


-- only unique based, slow but more flexible
removeLets :: forall tyname name uni fun a.
            (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
           => Marks
           -> Term tyname name uni fun a
           -> (M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a)) -- letterms
             , Term tyname name uni fun a)
removeLets ms = go
    where
      go :: Term tyname name uni fun a
         -> (M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))
           , Term tyname name uni fun a)
      go = \case
          -- break down let nonrecs
          -- by rule: {let nonrec (b:bs) in t} === {let nonrec b in let nonrec bs in t}
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)
          Let a r bs tIn ->
              let
                  (r1s, bs') = NE.unzip $ goBinding <$> bs
                  r1 = M.unionsWith (<>) r1s
                  (r2, tIn') = go tIn
              in case M.lookup (representativeBindingUnique bs) ms of
                  Nothing  -> (M.unionWith (<>) r1 r2, Let a r bs' tIn')
                  Just pos -> (M.insertWith (<>) pos (pure $ LetNaked a r bs') (M.unionWith (<>) r1 r2), tIn')

          Apply a t1 t2 ->
              let
                  (r1, t1') = go t1
                  (r2, t2') = go t2
              in (M.unionWith (<>) r1 r2, Apply a t1' t2')

          TyInst a t ty -> flip (TyInst a) ty `second` go t
          TyAbs a tyname k t -> TyAbs a tyname k `second` go t
          LamAbs a name ty t -> LamAbs a name ty `second` go t
          IWrap a ty1 ty2 t -> IWrap a ty1 ty2 `second` go t
          Unwrap a t -> Unwrap a `second` go t

          t -> (mempty, t)

      goBinding :: Binding tyname name uni fun a
                -> (M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))
                  , Binding tyname name uni fun a)
      goBinding = \case
              TermBind x s d t -> TermBind x s d `second` go t
              b -> (mempty, b)

floatBackLets :: forall tyname name uni fun a.
                (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
              => M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))
              -> Term tyname name uni fun a
              -> Term tyname name uni fun a
floatBackLets letholesTable = flip runReader topDepth . goTop
    where
          goTop  :: Term tyname name uni fun a -> Reader Depth (Term tyname name uni fun a)
          goTop = mergeLetsInM topPos

          go  :: Term tyname name uni fun a -> Reader Depth (Term tyname name uni fun a)
          go = \case
              LamAbs a n ty tBody -> goLam (LamAbs a n ty) (n^.PLC.theUnique) tBody
              TyAbs a n k tBody   -> goLam (TyAbs a n k) (n^.PLC.theUnique) tBody
              Let a r bs tIn      -> do
                  let letU = representativeBindingUnique bs
                  k <- goLetRhs (Let a r) letU bs
                  goLam k letU tIn
              t                  -> t & termSubterms go

          goLam :: (Term tyname name uni fun a -> Term tyname name uni fun a) -- ^ continuation
                -> PLC.Unique -- ^ lam/Lam unique
                -> Term tyname name uni fun a -- ^ lam/Lam body
                -> Reader Depth (Term tyname name uni fun a) -- ^ r
          goLam k u tBody = local (+1) $ do
              lamPos <- (,u,LamBody) <$> ask
              -- NOTE that we do not run go(floated lets) because that would increase the depth,
              -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
              k <$> mergeLetsInM lamPos tBody

          goLetRhs :: (NE.NonEmpty (Binding tyname name uni fun a) -> Term tyname name uni fun a -> Term tyname name uni fun a) -- ^ continuation
                   -> PLC.Unique -- ^ let's representative unique
                   -> NE.NonEmpty (Binding tyname name uni fun a) -- ^ let's bindings
                   -> Reader Depth (Term tyname name uni fun a -> Term tyname name uni fun a) -- ^ r
          goLetRhs k u bs = local (+1) $ do
              letPos <- (,u,LetRhs) <$> ask
              k <$> mergeBindingsM letPos bs

          mergeLetsInM :: Pos
                       -> Term tyname name uni fun a
                       -> Reader Depth (Term tyname name uni fun a)
          mergeLetsInM pos t =
              case M.lookup pos letholesTable of
                  Just letholes -> do
                      -- NOTE that we do not run go(floated lets) because that would increase the depth,
                      -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
                      letholes' <-  traverseOf (traversed.letNBs.traversed.bindingSubterms) go letholes
                      mergeLetsIn letholes' <$> go t
                  Nothing ->  go t

          mergeBindingsM :: Pos
                         -> NE.NonEmpty (Binding tyname name uni fun a)
                         -> Reader Depth (NE.NonEmpty (Binding tyname name uni fun a))
          mergeBindingsM pos bs = do
              bs' <- traverseOf (traversed.bindingSubterms) go bs
              case M.lookup pos letholesTable of
                  Just letholes -> do
                      -- NOTE that we do not run go(floated lets) because that would increase the depth,
                      -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
                      letholes' <-  traverseOf (traversed.letNBs.traversed.bindingSubterms) go letholes
                      pure $ mergeBindings letholes' <> bs'
                  Nothing       -> pure bs'



-- | this has the side-effect that no "directly" nested let rec or nonrec inside another let's rhs can appear,
-- e.g. no:  let {x = 1 + let {y = 3} in y} in ...
-- EXCEPT if the nested let is intercepted by a l/Lambda (depends on a lambda of the parent-let's rhs)
-- e.g. ok: let {x = \ z -> let {y = z+1} in y} in ...
mergeBindings :: NE.NonEmpty (LetNaked tyname name uni fun a)
              -> NE.NonEmpty (Binding tyname name uni fun a)
mergeBindings = foldMap1 (^.letNBs)
    -- we ignore annotations and recursivity, the letholes *must be* merged together with a Recursive let bindings
    -- the order of (<>) does not matter because it is a recursive let-group anyway.

mergeLetsIn :: NE.NonEmpty (LetNaked tyname name uni fun a) -> Term tyname name uni fun a -> Term tyname name uni fun a
mergeLetsIn ls =
                 -- TODO: use some semigroup annotation-joining
                 -- arbitrary: use the annotation of the first let of the floated lets as the annotation of the new let
                 Let (NE.head ls^.letNAnn)
                 Rec -- needs to be rec because we don't do dependency resolution at this pass, See Note [LetRec splitting pass]
                 (mergeBindings ls)


floatTerm :: (PLC.ToBuiltinMeaning uni fun,
            PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique,
            Ord tyname, Ord name
            )
          => Term tyname name uni fun a -> Term tyname name uni fun a
floatTerm t =
    let ms = mark t
        (letholes, t') = removeLets ms t
    in floatBackLets letholes t'

-- CONSTANTS

-- for simplicity, the top position is also linked to a unique number
-- chosen to not clash with any actual uniques of names/tynames of the program
topUnique :: PLC.Unique
topUnique = coerce (-1 :: Int)

-- arbitrarily chosen
topDepth :: Depth
topDepth = -1

-- arbitrary chosen as LetRhs, because top conceptually can be thought as a global let-rhs.
topType :: PosType
topType = LetRhs

topPos :: Pos
topPos = (topDepth, topUnique, topType)

-- HELPERS

maxPos :: M.Map k Pos -> Pos
maxPos = foldr max topPos . M.elems

withDepth :: (r ~ MarkCtx, MonadReader r m)
          => (Depth -> Depth) -> m a -> m a
withDepth = local . over _1

withLam :: (r ~ MarkCtx, MonadReader r m, PLC.HasUnique name unique)
        => name
        -> m a -> m a
withLam n = local $ \ (d, scope) ->
    let u = n^.PLC.theUnique
        d' = d+1
        pos' = (d', u, LamBody)
    in (d', M.insert u pos' scope)

withBs :: (r ~ MarkCtx, MonadReader r m, PLC.HasUnique name PLC.TermUnique, PLC.HasUnique tyname PLC.TypeUnique)
       => NE.NonEmpty (Binding tyname name uni fun a3)
       -> Pos
       -> m a -> m a
withBs bs pos = local $ second (M.fromList [(bid,pos) | bid <- bs^..traversed.bindingIds] <>)


unmovable :: PLC.ToBuiltinMeaning uni fun => LetProduct tyname name uni fun a -> Bool
unmovable = \case
    LetProduct _ Rec bs _ ->  any mayHaveEffects bs
    LetProduct _ NonRec (b  NE.:|[]) _  -> mayHaveEffects b
    LetProduct _ NonRec _ _ -> error "should not happen because we prior break down nonrecs"

-- | Returns if a binding's rhs is strict and may have effects (see Value.hs)
-- See Note [Purity, strictness, and variables]
-- We could maybe do better here, but not worth it at the moment
mayHaveEffects :: PLC.ToBuiltinMeaning uni fun => Binding tyname name uni fun a -> Bool
mayHaveEffects (TermBind _ Strict _ t') = not $ isPure (const NonStrict) t'
mayHaveEffects _                        = False

-- an alternative (but extreme) predicate to 'mayHavEffects' is to treat *all strict* bindings as unmovable.
-- isStrictBinding (TermBind _ Strict _  _) = True
-- isStrictBinding _                        = False


-- NOTES:
-- 1) no adjacent let-nonrec merging, it is left for the new letmerge pass
--

-- MISSING, TODO:
-- let-group splitting and correct ordering based on dep.resolution; right now is 1 big letrec at every floating position
-- dep.resolution does not necessarily need depgraph,scc,topsort, it can be done by an extra int dep inside the Marks
-- parameterization: unmovable,nofulllaziness, don'tfloatattop
-- semigrouping the annotations into bigger ones when creating let groups

-- OPTIMIZE:
-- Skip marking big Lambdas, as outlined in the paper
-- recursive descend for removelets, floatbacklets does not shrink search space; fix: keep a state, and will help as a safeguard check for left-out floatings
-- use some better/safer free-vars calculation?

