-- | While the flexible pretty-printing infrastructure is useful when you want it,
-- it's helpful to have an implementation of the default Pretty typeclass that
-- does the default thing.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module UntypedPlutusCore.Core.Instance.Pretty.Default () where

import PlutusPrelude

import PlutusCore.Pretty.Classic

import UntypedPlutusCore.Core.Instance.Pretty.Classic ()
import UntypedPlutusCore.Core.Type

import Universe

instance
        ( PrettyClassic name
        , HasHiddenValueOf uni, GShow uni, Pretty (HiddenValueOf uni), Pretty fun
        , Pretty ann
        ) => Pretty (Term name uni fun ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic name
        , HasHiddenValueOf uni, GShow uni, Pretty (HiddenValueOf uni), Pretty fun
        , Pretty ann
        ) => Pretty (Program name uni fun ann) where
    pretty = prettyClassicDef
