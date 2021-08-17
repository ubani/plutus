{-# LANGUAGE LambdaCase #-}
module PlutusIR.Transform.NewLetFloat (floatTerm) where

import           Control.Lens            hiding (Strict)
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


-- don't break down rec groups
-- | Selects a binding to be used a representative binding in MARKING the group of bindings.
representativeBinding :: NE.NonEmpty (Binding tyname name uni fun a) -> Binding tyname name uni fun a
representativeBinding = NE.head

-- this is used for two purposes
-- 1) to mark the max-position of a var in scope
-- 2) add lambdas
type Scope = M.Map PLC.Unique Pos

type MarkCtx = (Depth, Scope)

type Pos = (Depth, PLC.Unique, PosType)
type Depth = Int

data PosType = LamBody -- big, small lam or let body
             | LetRhs -- let rhs or top
             deriving (Eq, Ord, Show)

type Marks = M.Map PLC.Unique Pos

type LetHoled tyname name uni fun a = (a, NE.NonEmpty (Binding tyname name uni fun a))


mark :: PLC.ToBuiltinMeaning uni fun => Term TyName Name uni fun a -> Marks
mark t = runReader (go t) (topDepth, mempty)
    where
      go :: PLC.ToBuiltinMeaning uni fun => Term TyName Name uni fun a -> Reader MarkCtx Marks
      go = \case
          LamAbs _ n _ tBody  -> withLam n $ go tBody
          TyAbs _ n _ tBody   -> withLam n $ go tBody
          -- breaks down the NonRec true-groups
          -- break down let nonrec true group
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)
          l@(Let _ r bs tIn) ->
            let letU = head $ representativeBinding bs^..bindingIds
            in
              if unmovable l
              then do
                  -- since it is unmovable, the floatpos is a new anchor
                  (d, _) <- ask
                  let newDepth = d+1
                  marked1 <- withDepth (+1) $ (case r of Rec -> withBs bs (newDepth, letU, LetRhs); NonRec -> id) (mconcat <$> traverse go (bs^..traversed.bindingSubterms))
                  marked2 <- withDepth (+1) $ withBs bs (newDepth,letU, LamBody) $ go tIn
                  -- don't add any marks
                  pure $ marked1 <> marked2
              else do
                  (_,scope) <- ask
                  let freeVars = (case r of Rec ->  (S.\\ fromList (bs^..traversed.bindingIds)); NonRec -> id) $ calcFreeVars r bs
                      newPos@(newD,_,_) =  maxPos $ M.restrictKeys scope freeVars
                  marks1 <- withDepth (const newD) $ (case r of Rec -> withBs bs newPos; NonRec -> id) (mconcat <$> traverse go (bs^..traversed.bindingSubterms))
                  marks2 <- withBs bs newPos $ go tIn
                  -- add here a new mark
                  pure $ M.singleton letU newPos <> marks1 <> marks2
          t' -> mconcat <$> traverse go (t'^..termSubterms)

calcFreeVars :: (Ord name, Ord tyname, PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
             => Recursivity
             -> NE.NonEmpty (Binding tyname name uni fun ann)
             -> S.Set PLC.Unique
calcFreeVars r = foldMap1 $ \b ->
    -- OPTIMIZE: safe to change to S.mapMonotonic?
    S.map (^.PLC.theUnique) (fvBinding b)
    <> S.map (^.PLC.theUnique) (ftvBinding r b)


-- only unique based, slow but more flexible
removeLets :: Marks -> Term TyName Name uni fun a -> (M.Map Pos (NE.NonEmpty (LetHoled TyName Name uni fun a)) -- letterms
                            , Term TyName Name uni fun a)
removeLets ms = go
    where
      go = \case
          -- break down let nonrec true group
          Let a NonRec (b NE.:| bs) tIn | not (null bs) ->
                -- TODO: downside, I break down nonrec true-groups here, use the let to
                go (Let a NonRec (pure b) $ Let a NonRec (fromList bs) tIn)
          Let a r bs tIn ->
              let
                  (r1s, bs') = NE.unzip $ fmap goBinding bs
                  r1 = M.unionsWith (<>) r1s
                  (r2, tIn') = go tIn
              in case M.lookup (head $ representativeBinding bs^..bindingIds) ms of
                  Nothing  -> (M.unionWith (<>) r1 r2, Let a r bs' tIn')
                  Just pos -> (M.insertWith (<>) pos (pure (a,bs')) (M.unionWith (<>) r1 r2), tIn')

          TyAbs a tyname k t -> second (TyAbs a tyname k) (go t)
          LamAbs a name ty t -> second (LamAbs a name ty) (go t)
          Apply a t1 t2 ->
              let
                  (r1, t1') = go t1
                  (r2, t2') = go t2
              in (M.unionWith (<>) r1 r2, Apply a t1' t2')
          TyInst a t ty -> second (flip (TyInst a) ty) (go t)
          IWrap a ty1 ty2 t -> second (IWrap a ty1 ty2) (go t)
          Unwrap a t -> second (Unwrap a) (go t)
          t' -> (mempty, t')

      goBinding (TermBind x s d t)  =
         let (m, t') = go t
         in (m, TermBind x s d t')
      goBinding b = (mempty, b)

floatBackLets :: -- | remove result
                (M.Map Pos (NE.NonEmpty (LetHoled TyName Name uni fun a)) -- letterms
                , Term TyName Name uni fun a)
              -> Term TyName Name uni fun a
floatBackLets (letholesTable,t) =
    runReader (case M.lookup topPos letholesTable of
                    Just letholes -> do
                        -- toplevel lets
                        -- NOTE that we do not run go(floated lets) because that would increase the depth,
                        -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
                        letholes' <-  traverseOf (traversed._2.traversed.bindingSubterms) go letholes
                        mergeLetsIn letholes' <$> go t
                    Nothing -> go t
              ) topDepth
    where go = \case
              LamAbs a n ty tBody -> goLam (LamAbs a n ty) (n^.PLC.theUnique) tBody
              TyAbs a n k tBody   -> goLam (TyAbs a n k) (n^.PLC.theUnique) tBody
              Let a r bs tIn      -> do
                  let letU = head $ representativeBinding bs^..bindingIds
                  k <- goLetRhs (Let a r) letU bs
                  goLam k letU tIn
              t'                  -> t' & termSubterms go

          goLam k u tBody = local (+1) $ do
              depth <- ask
              tBody' <- go tBody
              k <$> case M.lookup (depth, u, LamBody) letholesTable of
                        Just letholes -> do
                            -- NOTE that we do not run go(floated lets) because that would increase the depth,
                            -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
                            letholes' <-  traverseOf (traversed._2.traversed.bindingSubterms) go letholes
                            pure $ mergeLetsIn letholes' tBody'
                        Nothing ->  pure tBody'
          goLetRhs k u bs = local (+1) $ do
              depth <- ask
              bs' <-  traverseOf (traversed.bindingSubterms) go bs
              k <$> case M.lookup (depth, u, LetRhs) letholesTable of
                             Just letholes -> do
                                 -- NOTE that we do not run go(floated lets) because that would increase the depth,
                                 -- but the floated lets are not anchors, instead we run go on the floated-let bindings' subterms
                                 letholes' <-  traverseOf (traversed._2.traversed.bindingSubterms) go letholes
                                 pure $ mergeBindings letholes' bs'
                             Nothing       -> pure bs'

-- | this has the side-effect that no "directly" nested let rec or nonrec inside another let's rhs can appear,
-- e.g. no:  let {x = 1 + let {y = 3} in y} in ...
-- EXCEPT if the nested let is intercepted by a l/Lambda (depends on a lambda of the parent-let's rhs)
-- e.g. ok: let {x = \ z -> let {y = z+1} in y} in ...
mergeBindings :: NE.NonEmpty (LetHoled TyName Name uni fun a)
              -> NE.NonEmpty (Binding TyName Name uni fun a)
              -> NE.NonEmpty (Binding TyName Name uni fun a)
mergeBindings ls =
    -- we ignore annotations and recursivity, the letholes *must be* merged together with a Recursive let bindings
    -- the order of (<>) does not matter because it is a recursive let-group anyway.
    (foldMap1 (^._2) ls <>)


mergeLetsIn :: NE.NonEmpty (LetHoled TyName Name uni fun a) -> Term TyName Name uni fun a -> Term TyName Name uni fun a
mergeLetsIn ls =
                 -- arbitrarily use the annotation of the first let of the floated lets as the annotation of the new let
                 Let (NE.head ls^._1)
                 Rec -- needs to be rec because we don't do dep resolution currently (in rec, bindings order does not matter)
                 (foldMap1 (^._2) ls)

floatTerm :: PLC.ToBuiltinMeaning uni fun => Term TyName Name uni fun a -> Term TyName Name uni fun a
floatTerm t = floatBackLets $ removeLets (mark t) t

-- NOTES:
-- 1) no adjacent let-nonrec merging, it is left for the new letmerge pass
-- 2) algo breaks down let-nonrec grouppings, so the letmerge pass should be applied
--
-- 2) compared to "Let-floating: moving bindings to give faster programs", the algorithm
-- does not float right outside the free-lamdba, but right inside the dependent lambda
-- or right inside a dependent let-in or right together with a dependent let-rhs

-- MISSING, TODO:
-- let-group splitting and correct ordering based on dep.resolution; right now is 1 big letrec at every floating position
-- dep.resolution does not necessarily need depgraph,scc,topsort, it can be done by an extra int dep inside the Marks
-- parameterization: unmovable,nofulllaziness, don'tfloatattop
-- semigrouping the annotations into bigger ones when creating let groups

-- OPTIMIZE:
-- Skip marking big Lambdas, as outlined in the paper
-- recursive descend for removelets, floatbacklets does not shrink search space; fix: keep a state, and will help as a safeguard check for left-out floatings
-- use some better/safer free-vars calculation?

-- HELPERS

-- dummy unique to signify toplevel
topUnique :: PLC.Unique
topUnique = coerce (-1 :: Int)

topDepth :: Depth
topDepth = -1

topType :: PosType
topType = LetRhs -- does not mean much, but can top can best be seen as a global let-rhs

topPos :: (Depth, PLC.Unique, PosType)
topPos = (topDepth, topUnique, topType)

maxPos :: M.Map k (Depth, PLC.Unique, PosType)
       -> (Depth, PLC.Unique, PosType)
maxPos env = foldr max topPos $ M.elems env

withLam :: (MonadReader (a1, M.Map PLC.Unique (a1, PLC.Unique, PosType)) m,
 Num a1, PLC.HasUnique s unique) => s -> m a2 -> m a2
withLam n = local $ \ (d, scope) ->
    let u = n^.PLC.theUnique
        d' = d+1
        pos' = (d', u, LamBody)
    in (d', M.insert u pos' scope)

withDepth :: (MonadReader s m, Field1 s s a1 b) => (a1 -> b) -> m a2 -> m a2
withDepth f = local (over _1 f)

withBs :: (MonadReader (p a1 (M.Map PLC.Unique a2)) m, Bifunctor p,
 Traversable f, PLC.HasUnique tyname PLC.TypeUnique,
 PLC.HasUnique name PLC.TermUnique) => f (Binding tyname name uni fun a3) -> a2 -> m a4 -> m a4
withBs bs pos = local $ second (M.fromList [(bid,pos) | bid <- bs^..traversed.bindingIds] <>)

unmovable :: PLC.ToBuiltinMeaning uni fun => Term tyname name uni fun a -> Bool
unmovable (Let _ NonRec (b  NE.:|[]) _) = mayHaveEffects b
unmovable (Let _ Rec bs _)              = any mayHaveEffects bs
unmovable (Let _ NonRec _ _)            = error "should not happen because we rely in breaking down nonrecs"
unmovable _                             = error "not total fix me"

-- | Returns if a binding's rhs is strict and may have effects (see Value.hs)
mayHaveEffects
    :: PLC.ToBuiltinMeaning uni fun
    => Binding tyname name uni fun a
    -> Bool
-- See Note [Purity, strictness, and variables]
-- We could maybe do better here, but not worth it at the moment
mayHaveEffects (TermBind _ Strict _ t') = not $ isPure (const NonStrict) t'
mayHaveEffects _                        = False

-- a more extreme way is to treat all strict bindings as unmovable
-- isStrictBinding (TermBind _ Strict _  _) = True
-- isStrictBinding _                        = False


