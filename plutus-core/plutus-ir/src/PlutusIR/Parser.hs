{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module PlutusIR.Parser
    ( parse
    , parseQuoted
    , program
    , plcProgram
    , typ
    , Parser
    , SourcePos
    ) where

import PlutusCore qualified as PLC
import PlutusCore.Parser.ParserCommon
import PlutusIR as PIR
import PlutusIR.MkPir qualified as PIR
import PlutusPrelude
import Prelude hiding (fail)

import Control.Monad.Combinators.Expr
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Text (Text)
import Text.Megaparsec hiding (ParseError, State, many, parse, some)
import Text.Megaparsec.Char (string)

recursivity :: Parser Recursivity
recursivity = inParens $ (wordPos "rec" >> return Rec) <|> (wordPos "nonrec" >> return NonRec)

strictness :: Parser Strictness
strictness = inParens $ (wordPos "strict" >> return Strict) <|> (wordPos "nonstrict" >> return NonStrict)

-- TODO move these to parser common
-- | A PLC @Type@ to be parsed. ATM the parser only works
-- for types in the @DefaultUni@ with @DefaultFun@.
type PType = PLC.Type TyName PLC.DefaultUni SourcePos

varType :: Parser PType
varType = TyVar <$> getSourcePos <*> tyName

funType :: Parser PType
funType = TyFun <$> wordPos "fun" <*> typ <*> typ

allType :: Parser PType
allType = TyForall <$> wordPos "all" <*> tyName <*> kind <*> typ

lamType :: Parser PType
lamType = TyLam <$> wordPos "lam" <*> tyName <*> kind <*> typ

ifixType :: Parser PType
ifixType = TyIFix <$> wordPos "ifix" <*> typ <*> typ

builtinType :: Parser PType
builtinType = TyBuiltin <$> wordPos "con" <*> defaultUniType

appType :: Parser PType
appType = do
    pos  <- getSourcePos
    fn   <- typ
    args <- some typ
    pure $ foldl' (TyApp pos) fn args

kind :: Parser (Kind SourcePos)
kind = inParens (typeKind <|> funKind)
    where
        typeKind = Type <$> wordPos "type"
        funKind  = KindArrow <$> wordPos "fun" <*> kind <*> kind

-- | Parser for @PType@.
typ :: Parser PType
typ = choice
    [inParens typ
    , varType
    , funType
    , ifixType
    , allType
    , builtinType
    , lamType
    , inBrackets appType
    ]

defaultUniType :: Parser (PLC.SomeTypeIn PLC.DefaultUni)
defaultUniType = choice
  [ inParens defaultUniType
  , PLC.SomeTypeIn PLC.DefaultUniInteger <$ string "integer"
  , PLC.SomeTypeIn PLC.DefaultUniByteString <$ string "bytestring"
  , PLC.SomeTypeIn PLC.DefaultUniString <$ string "string"
  , PLC.SomeTypeIn PLC.DefaultUniUnit <$ string "unit"
  , PLC.SomeTypeIn PLC.DefaultUniBool <$ string "bool"
  , PLC.SomeTypeIn PLC.DefaultUniProtoList <$ string "list"
  , PLC.SomeTypeIn PLC.DefaultUniProtoPair <$ string "pair"
  -- , PLC.SomeTypeIn DefaultUniApply <$ string "?" TODO need to make this an operator
  , PLC.SomeTypeIn PLC.DefaultUniData <$ string "data" ]

varDecl :: Parser (VarDecl TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
varDecl = inParens $ VarDecl <$> wordPos "vardecl" <*> name <*> typ

tyVarDecl :: Parser (TyVarDecl TyName SourcePos)
tyVarDecl = inParens $ TyVarDecl <$> wordPos "tyvardecl" <*> tyName <*> kind

datatype :: Parser (Datatype TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
datatype = inParens $ Datatype <$> wordPos "datatype"
    <*> tyVarDecl
    <*> many tyVarDecl
    <*> name
    <*> many varDecl

binding
    :: Parser (Binding TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
binding = inParens $
    (try $ wordPos "termbind" >> TermBind <$> getSourcePos <*> strictness <*> varDecl <*> term)
    <|> (wordPos "typebind" >> TypeBind <$> getSourcePos <*> tyVarDecl <*> typ)
    <|> (wordPos "datatypebind" >> DatatypeBind <$> getSourcePos <*> datatype)

-- A small type wrapper for parsers that are parametric in the type of term(PIR/PLC) they parse
type Parametric
    = forall term. PIR.TermLike term TyName Name PLC.DefaultUni PLC.DefaultFun
    => Parser  (term SourcePos)
    -> Parser  (term SourcePos)

absTerm :: Parametric
absTerm tm = PIR.tyAbs <$> wordPos "abs" <*> tyName <*> kind <*> tm

lamTerm :: Parametric
lamTerm tm = PIR.lamAbs <$> wordPos "lam" <*> name <*> typ <*> tm

conTerm :: Parametric
conTerm _tm = PIR.constant <$> wordPos "con" <*> constant

iwrapTerm :: Parametric
iwrapTerm tm = PIR.iWrap <$> wordPos "iwrap" <*> typ <*> typ <*> tm

builtinTerm :: Parametric
builtinTerm _term = PIR.builtin <$> wordPos "builtin" <*> builtinFunction

unwrapTerm :: Parametric
unwrapTerm tm = PIR.unwrap <$> wordPos "unwrap" <*> tm

errorTerm :: Parametric
errorTerm _tm = PIR.error <$> wordPos "error" <*> typ

letTerm :: Parser (Term TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
letTerm = Let <$> wordPos "let" <*> recursivity <*> NE.some (try binding) <*> term

appTerm :: Parametric
appTerm tm = PIR.mkIterApp <$> getSourcePos <*> tm <*> some tm

tyInstTerm :: Parametric
tyInstTerm tm = PIR.mkIterInst <$> getSourcePos <*> tm <*> some typ

-- Note that PIR programs do not actually carry a version number
-- we (optionally) parse it all the same so we can parse all PLC code
program :: Parser (Program TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
program = whitespace >> do
    prog <- inParens $ do
        p <- wordPos "program"
        option () $ void version
        Program p <$> term
    notFollowedBy anySingle
    return prog

plcProgram :: Parser (PLC.Program TyName Name PLC.DefaultUni PLC.DefaultFun SourcePos)
plcProgram = whitespace >> do
    prog <- inParens $ PLC.Program <$> wordPos "program" <*> version <*> plcTerm
    notFollowedBy anySingle
    return prog
