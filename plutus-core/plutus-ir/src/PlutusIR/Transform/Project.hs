{-# LANGUAGE LambdaCase #-}
{-|
A trivial simplification that cancels prod/proj pairs.

This can mostly only occur if we've inlined both datatype constructors and destructors
and we're deconstructing something we just constructed. This is probably rare,
and should anyway better be handled by something like case-of-known constructor.
But it's so simple we might as well include it just in case.
-}
module PlutusIR.Transform.Project (
  projectCancel
  ) where

import PlutusIR

import Control.Lens (transformOf)
import Data.List.Extra ((!?))

{-|
A single non-recursive application of prod/proj cancellation.
-}
projectStep
    :: Term tyname name uni fun a
    -> Term tyname name uni fun a
projectStep = \case
    t@(Proj _ i (Prod _ es)) -> case es !? i of
        Just e  -> e
        Nothing -> t
    t                        -> t

{-|
Recursively apply prod/proj cancellation.
-}
projectCancel
    :: Term tyname name uni fun a
    -> Term tyname name uni fun a
projectCancel = transformOf termSubterms projectStep
