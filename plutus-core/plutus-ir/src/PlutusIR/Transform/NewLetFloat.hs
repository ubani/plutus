{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -W -Wwarn #-}
module PlutusIR.Transform.NewLetFloat (floatTerm) where

import           Control.Lens            hiding (Strict)
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Coerce
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import           Data.Semigroup.Foldable
import qualified Data.Set                as S
import           Data.Set.Lens           (setOf)
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
type Pos = ( Depth
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

-- The result of the first pass is a subset(union of all computed scopes).
-- This subset contains only the marks of the floatable lets.
type Marks = Scope

-- | A "naked" let, without its inTerm.
-- We use this structure
-- 1) to determine if a let is movable/unmovable. See 'unmovable'.
-- 2) to calculate freevars/tyvars of a let.
-- 2) to store it in the 'FloatTable'.
data LetNaked tyname name uni fun a = LetNaked {
    _letAnn :: a,
    _letRec :: Recursivity,
    _letBs  :: NE.NonEmpty (Binding tyname name uni fun a)
    }
makeLenses ''LetNaked

-- a store of lets to be floated at their new position
type FloatTable tyname name uni fun a = M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))

-- | The 1st pass of marking floatable lets
mark :: forall tyname name uni fun a.
      (Ord tyname, Ord name, PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique, PLC.ToBuiltinMeaning uni fun)
     => Term tyname name uni fun a
     -> Marks
mark = flip runReader initCtx . go
    where
      initCtx :: MarkCtx
      initCtx = (topDepth, mempty)

      go :: Term tyname name uni fun a -> Reader MarkCtx Marks
      go = \case
          -- lam/Lam are treated the same.
          LamAbs _ n _ tBody  -> withLam n $ go tBody
          TyAbs _ n _ tBody   -> withLam n $ go tBody

          -- first, break down let nonrecs.
          -- by rule: {let nonrec (b:bs) in t} === {let nonrec b in let nonrec bs in t}
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)

          -- main operation: for letrec or single letnonrec
          Let ann r bs tIn -> do
            (depth, scope) <- ask
            let letN = LetNaked ann r bs
                letU = representativeBindingUnique bs
            if unmovable letN
              then do
                  -- since it is unmovable, the floatpos is a new anchor
                  let newDepth = depth+1
                      toPos = (newDepth, letU,)

                  -- let is effectful so it acts as anchor, both in rhses and inTerm
                  withDepth (+1) $ do
                      -- visit the rhs'es
                      marksRhs <-
                          -- if rec, then its bindings are in scope in the rhs'es
                          ifRec r (withBs bs $ toPos LetRhs) $
                                  mconcat <$> traverse go (bs^..traversed.bindingSubterms)

                       -- visit the inTerm
                      marksIn <-
                          -- bindings are inscope in the InTerm for both rec&nonrec
                          withBs bs (toPos LamBody) $ go tIn

                      -- don't add any marks, just propagate
                      pure $ marksRhs <> marksIn

              else do
                  let freeVars =
                          -- if Rec, remove the here-bindings from free
                          ifRec r (S.\\ setOf (traversed.bindingIds) bs) $
                             calcFreeVars letN

                      -- The "heart" of the algorithm: the future position to float this let to
                      -- is determined as the maximum among its dependencies (free vars).
                      floatPos@(floatDepth,_,_) =  maxPos $ M.restrictKeys scope freeVars

                  -- visit the rhs'es
                  marksRhs <-
                      -- IMPORTANT: inside the rhs, act like the current depth
                      -- is the future floated depth of this rhs.
                      withDepth (const floatDepth) $
                          ifRec r (withBs bs floatPos) $
                             mconcat <$> traverse go (bs^..traversed.bindingSubterms)

                  -- visit the inTerm
                  marksIn <- withBs bs floatPos $
                      go tIn

                  -- collect here the new mark and propagate it
                  pure $ M.singleton letU floatPos <> marksRhs <> marksIn

          -- descend and collect
          t -> mconcat <$> traverse go (t^..termSubterms)


-- | Given a 'LetNaked' , it calculate its free vars and free tyvars and collects them in a set.
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


-- | The second pass of cleaning the term of the floatable lets, and placing them in a separate map
-- OPTIMIZE: use State for building the FloatTable, and for reducing the Marks
removeLets :: forall tyname name uni fun a.
            (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
           => Marks
           -> Term tyname name uni fun a
           -> (FloatTable tyname name uni fun a -- the floatable lets
             , Term tyname name uni fun a) -- the cleaned-up, reducted term
removeLets marks = go
    where
      go :: Term tyname name uni fun a
         -> (M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))
           , Term tyname name uni fun a)
      go = \case
          -- first, break down let nonrecs.
          -- by rule: {let nonrec (b:bs) in t} === {let nonrec b in let nonrec bs in t}
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)

          -- main operation: for letrec or single letnonrec
          Let a r bs tIn ->
              let
                  -- go to rhse's and collect their floattable + cleanedterm
                  (fRhs, bs') = M.unionsWith (<>) `first`
                                   NE.unzip (goBinding <$> bs)
                  -- go to inTerm and collect its floattable + cleanedterm
                  (fIn, tIn') = go tIn
                  fBoth = M.unionWith (<>) fRhs fIn
              in case marks M.!? representativeBindingUnique bs  of
                  -- this is not a floatable let
                  -- propagate the floattable and KEEP this let in the cleaned term
                  Nothing  -> (fBoth, Let a r bs' tIn')
                  -- floatable let found.
                  -- move this let inside the floattable and just return the traversed interm
                  Just pos -> (M.insertWith (<>) pos (pure $ LetNaked a r bs') fBoth, tIn')

          Apply a t1 t2 ->
              let (f1, t1') = go t1
                  (f2, t2') = go t2
              in (M.unionWith (<>) f1 f2, Apply a t1' t2')

          -- descend and collect
          TyInst a t ty -> flip (TyInst a) ty `second` go t
          TyAbs a tyname k t -> TyAbs a tyname k `second` go t
          LamAbs a name ty t -> LamAbs a name ty `second` go t
          IWrap a ty1 ty2 t -> IWrap a ty1 ty2 `second` go t
          Unwrap a t -> Unwrap a `second` go t

          -- no term inside here, nothing to do
          t@Var{} -> (mempty, t)
          t@Constant{} -> (mempty, t)
          t@Builtin{} -> (mempty, t)
          t@Error{} -> (mempty, t)

      goBinding :: Binding tyname name uni fun a
                -> (M.Map Pos (NE.NonEmpty (LetNaked tyname name uni fun a))
                  , Binding tyname name uni fun a)
      goBinding = \case
              TermBind x s d t -> TermBind x s d `second` go t
              -- no term inside here, nothing to do
              b@TypeBind{}     -> (mempty, b)
              b@DatatypeBind{} -> (mempty, b)

-- | The 3rd and last pass that, given the result of 'removeLets', places the lets back (floats) at the right marked positions.
floatBackLets :: forall tyname name uni fun a.
                (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
              => FloatTable tyname name uni fun a -- the lets to be floated
              -> Term tyname name uni fun a -- the cleanedup, reducted term
              -> Term tyname name uni fun a -- the final, floated, and correctly-scoped term
floatBackLets fTable =
    -- our reader context is only the depth this time.
    flip runReader topDepth . goTop
  where
    -- first check if we have something to float in the top
    goTop :: Term tyname name uni fun a -> Reader Depth (Term tyname name uni fun a)
    goTop = floatLam topUnique <=< go

    go  :: Term tyname name uni fun a -> Reader Depth (Term tyname name uni fun a)
    go = \case
        -- lam/Lam anchor, increase depth
        LamAbs a n ty tBody -> local (+1) $
            LamAbs a n ty <$> (floatLam (n^.PLC.theUnique) =<< go tBody)
        -- lam/Lam anchor, increase depth
        TyAbs a n k tBody -> local (+1) $
            TyAbs a n k <$> (floatLam (n^.PLC.theUnique) =<< go tBody)
        -- Unmovable-let anchor, increase depth
        -- note that we do not touch the recursivity of an unmovable-let
        Let a r bs tIn   -> local (+1) $ do
            Let a r
                <$> (floatRhs (representativeBindingUnique bs) =<< traverseOf (traversed.bindingSubterms) go bs)
                -- act the same as lam/Lam: float right inside
                <*> (floatLam (representativeBindingUnique bs) =<< go tIn)

        -- descend
        t                  -> t & termSubterms go

    floatLam u m = do
        herePos <- (,u, LamBody) <$> ask
        -- make a brand new let-group comprised of all the floatable lets just inside the lam/Lam/letInTerm
        floatBy makeNewLet herePos m

    floatRhs u m = do
        -- we cannot float the lets originally found inside any of the unmovable-let rhs'es back in those rhs'es
        -- because we lost the information on which specific rhs they belonged to!
        herePos <- (,u, LetRhs) <$> ask
        floatBy (\ floatableNaked unmovableBindings ->
                     -- instead we merge them together with the same level as the unmovable-let's bindings using this safe local transformation:
                     -- letrec Unmovable = (let Floatable = e in b) in c ====> letrec { Floatable = e ; Unmovable = b } in c
                     mergeNaked floatableNaked <> unmovableBindings
                     -- How can we apply this letrec-only transformation for any let?
                     -- Well if the floatable lets were originally in some unmovable-let's rhs, this implies that the unmovable let is a letrec.
                     -- We do not touch the recursivity of an unmovable let.
                ) herePos m

    floatBy :: (NE.NonEmpty (LetNaked tyname name uni fun a) -> b -> b) -- ^ how to merge the floated bindings into a term or bindings
             -> Pos -- ^ current floating position
             -> b -- ^ with what to combine
             -> Reader Depth b -- ^ the combined result
    floatBy mergeFunc herePos next =
        -- is there something to be floated here?
        case fTable M.!? herePos  of
            -- just descend
            Nothing -> pure next
            -- all the naked-lets to be floated here
            Just letsNaked -> do
                -- visit their rhs'es for any potential floatings as well
                -- NOTE: we do not directly run go(floated-lets) because that would increase the depth,
                -- and the floated lets are not anchors: instead we run go on the floated-let bindings' subterms
                letsNaked' <- traverseOf (traversed.letBs.traversed.bindingSubterms) go letsNaked
                -- apply the merging with the visit result. This is what floats them back to the pir.
                pure $ mergeFunc letsNaked' next

-- | this has the side-effect that no "directly" nested let rec or nonrec inside another let's rhs can appear,
-- e.g. no:  let {x = 1 + let {y = 3} in y} in ...
-- EXCEPT if the nested let is intercepted by a l/Lambda (depends on a lambda of the parent-let's rhs)
-- e.g. ok: let {x = \ z -> let {y = z+1} in y} in ...
mergeNaked :: NE.NonEmpty (LetNaked tyname name uni fun a)
           -> NE.NonEmpty (Binding tyname name uni fun a)
mergeNaked = foldMap1 (^.letBs)
    -- we ignore annotations and recursivity, the letholes *must be* merged together with a Recursive let bindings
    -- the order of (<>) does not matter because it is a recursive let-group anyway.

makeNewLet :: NE.NonEmpty (LetNaked tyname name uni fun a) -> Term tyname name uni fun a -> Term tyname name uni fun a
makeNewLet ls =
                 -- TODO: use some semigroup annotation-joining
                 -- arbitrary: use the annotation of the first let of the floated lets as the annotation of the new let
                 Let (NE.head ls^.letAnn)
                 Rec -- needs to be rec because we don't do dependency resolution at this pass, See Note [LetRec splitting pass]
                 (mergeNaked ls)


-- | The compiler pass of the algorithm (comprised of 3 connected passes).
floatTerm :: (PLC.ToBuiltinMeaning uni fun,
            PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique,
            Ord tyname, Ord name
            )
          => Term tyname name uni fun a -> Term tyname name uni fun a
floatTerm t =
    mark t
    & flip removeLets t
    & uncurry floatBackLets

-- CONSTANTS

-- for simplicity, the top position is also linked to a unique number
-- chosen to not clash with any actual uniques of names/tynames of the program
topUnique :: PLC.Unique
topUnique = coerce (-1 :: Int)

-- arbitrarily chosen
topDepth :: Depth
topDepth = -1

-- arbitrary chosen as LamBody, because top can be imagined as a global inbody (of an empty letterm)
topType :: PosType
topType = LamBody

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

-- A helper to apply a function iff recursive
ifRec :: Recursivity -> (a -> a) -> a -> a
ifRec = \case
    Rec    -> ($)
    NonRec -> id



unmovable :: PLC.ToBuiltinMeaning uni fun => LetNaked tyname name uni fun a -> Bool
unmovable = \case
    LetNaked _ Rec bs              ->  any mayHaveEffects bs
    LetNaked _ NonRec (b  NE.:|[]) -> mayHaveEffects b
    LetNaked _ NonRec _            -> error "should not happen because we prior break down nonrecs"

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

