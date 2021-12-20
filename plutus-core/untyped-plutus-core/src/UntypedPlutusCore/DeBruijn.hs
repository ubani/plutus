{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Support for using de Bruijn indices for term names.
module UntypedPlutusCore.DeBruijn
    ( Index (..)
    , DeBruijn (..)
    , NamedDeBruijn (..)
    , FreeVariableError (..)
    , AsFreeVariableError (..)
    , deBruijnTerm
    , unDeBruijnTerm
    , unNameDeBruijn
    , fakeNameDeBruijn
    , deBruijnInitIndex
    ) where

import PlutusCore.DeBruijn.Internal

import PlutusCore.Name
import PlutusCore.Quote
import UntypedPlutusCore.Core

import Control.Lens hiding (Index, index)
import Control.Monad.Except
import Control.Monad.Reader

{- Note [Comparison with typed deBruijn conversion]
This module is just a boring port of the typed version.
-}

-- | Convert a 'Term' with and 'Name's into a 'Term' with 'NamedDeBruijn's.
deBruijnTerm
    :: (AsFreeVariableError e, MonadError e m)
    => Term Name uni fun ann -> m (Term NamedDeBruijn uni fun ann)
deBruijnTerm = runDeBruijnT . deBruijnTermM

-- | Convert a 'Term' with 'NamedDeBruijn's into a 'Term' with 'Name's.
unDeBruijnTerm
    :: (MonadQuote m, AsFreeVariableError e, MonadError e m)
    => Term NamedDeBruijn uni fun ann -> m (Term Name uni fun ann)
unDeBruijnTerm = runDeBruijnT . unDeBruijnTermM

deBruijnTermM
    :: (MonadReader Levels m, AsFreeVariableError e, MonadError e m)
    => Term Name uni fun ann
    -> m (Term NamedDeBruijn uni fun ann)
deBruijnTermM = \case
    -- variable case
    Var ann n -> Var ann <$> nameToDeBruijn n
    -- binder cases
    LamAbs ann n t -> declareUnique n $ do
        n' <- nameToDeBruijn n
        withScope $ LamAbs ann n' <$> deBruijnTermM t
    -- boring recursive cases
    Apply ann t1 t2 -> Apply ann <$> deBruijnTermM t1 <*> deBruijnTermM t2
    Delay ann t -> Delay ann <$> deBruijnTermM t
    Force ann t -> Force ann <$> deBruijnTermM t
    -- boring non-recursive cases
    Constant ann con -> pure $ Constant ann con
    Builtin ann bn -> pure $ Builtin ann bn
    Error ann -> pure $ Error ann

unDeBruijnTermM
    :: (MonadReader Levels m, MonadQuote m, AsFreeVariableError e, MonadError e m)
    => Term NamedDeBruijn uni fun ann
    -> m (Term Name uni fun ann)
unDeBruijnTermM = \case
    -- variable case
    Var ann n -> Var ann <$> deBruijnToName n
    -- binder cases
    LamAbs ann n t ->
        -- See NOTE: [DeBruijn indices of Binders]
        declareBinder $ do
            n' <- deBruijnToName $ set index deBruijnInitIndex n
            withScope $ LamAbs ann n' <$> unDeBruijnTermM t
    -- boring recursive cases
    Apply ann t1 t2 -> Apply ann <$> unDeBruijnTermM t1 <*> unDeBruijnTermM t2
    Delay ann t -> Delay ann <$> unDeBruijnTermM t
    Force ann t -> Force ann <$> unDeBruijnTermM t
    -- boring non-recursive cases
    Constant ann con -> pure $ Constant ann con
    Builtin ann bn -> pure $ Builtin ann bn
    Error ann -> pure $ Error ann
