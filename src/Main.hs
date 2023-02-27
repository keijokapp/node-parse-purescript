{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances, InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import System.Environment
import Language.PureScript.Environment (NameKind (Private, Public, External))
import Language.PureScript.CST.Parser (parse)
import Language.PureScript.CST.Convert (convertModule)
import Data.Aeson
import Data.Text (pack)
import Text.Pretty.Simple (pPrint)
import Language.PureScript (Binder (NullBinder, LiteralBinder, VarBinder, ConstructorBinder, OpBinder, BinaryNoParensBinder, ParensInBinder, NamedBinder, PositionedBinder, TypedBinder), Literal (NumericLiteral, StringLiteral, CharLiteral, BooleanLiteral, ArrayLiteral, ObjectLiteral), Module (Module), Declaration (DataDeclaration, DataBindingGroupDeclaration, TypeSynonymDeclaration, KindDeclaration, RoleDeclaration, TypeDeclaration, ValueDeclaration, BoundValueDeclaration, BindingGroupDeclaration, ExternDeclaration, ExternDataDeclaration, FixityDeclaration, ImportDeclaration, TypeClassDeclaration, TypeInstanceDeclaration), GuardedExpr (GuardedExpr), Guard (ConditionGuard, PatternGuard), Expr (Literal, UnaryMinus, BinaryNoParens, Parens, Accessor, ObjectUpdate, ObjectUpdateNested, Abs, App, Unused, Var, IfThenElse, Constructor, Case, TypedValue, Let, Do, Ado, TypeClassDictionary, DeferredDictionary, DerivedInstancePlaceholder, AnonymousArgument, Hole, PositionedValue, Op), WhereProvenance (FromWhere, FromLet), DataConstructorDeclaration (DataConstructorDeclaration, dataCtorAnn, dataCtorName, dataCtorFields), KindSignatureFor (DataSig, NewtypeSig, TypeSynonymSig, ClassSig), RoleDeclarationData (RoleDeclarationData, rdeclSourceAnn, rdeclIdent, rdeclRoles), TypeDeclarationData (tydeclSourceAnn, tydeclIdent, tydeclType, TypeDeclarationData), ValueDeclarationData (ValueDeclarationData, valdeclSourceAnn, valdeclIdent, valdeclName, valdeclBinders, valdeclExpression), PathTree (PathTree), PathNode (Leaf, Branch), CaseAlternative (CaseAlternative, caseAlternativeBinders, caseAlternativeResult), DoNotationElement (DoNotationValue, DoNotationBind, DoNotationLet, PositionedDoNotationElement), AssocList (runAssocList), InstanceDerivationStrategy (KnownClassStrategy, NewtypeStrategy), ValueFixity (ValueFixity), TypeFixity (TypeFixity), TypeInstanceBody (DerivedInstance, NewtypeInstance, ExplicitInstance))

instance ToJSON Module where
  toJSON (Module sourceSpan comments moduleName declarations exports) = object [ "sourceSpan" .= sourceSpan, "comments" .= comments, "moduleName" .= moduleName, "declarations" .= declarations, "exports" .= exports ]

instance ToJSON Declaration where
  toJSON (DataDeclaration a1 a2 a3 a4 a5) = toJSON ("DataDeclaration" :: String, a1, a2, a3, a4, a5)
  toJSON (DataBindingGroupDeclaration a1) = toJSON ("DataBindingGroupDeclaration" :: String, a1)
  toJSON (TypeSynonymDeclaration a1 a2 a3 a4) = toJSON ("TypeSynonymDeclaration" :: String, a1, a2, a3, a4)
  toJSON (KindDeclaration a1 a2 a3 a4) = toJSON ("KindDeclaration" :: String, a1, a2, a3, a4)
  toJSON (RoleDeclaration a1) = toJSON ("RoleDeclaration" :: String, a1)
  toJSON (TypeDeclaration a1) = toJSON ("TypeDeclaration" :: String, a1)
  toJSON (ValueDeclaration a1) = toJSON ("ValueDeclaration" :: String, a1)
  toJSON (BoundValueDeclaration a1 a2 a3) = toJSON ("BoundValueDeclaration" :: String, a1, a2, a3)
  toJSON (BindingGroupDeclaration a1) = toJSON ("BindingGroupDeclaration" :: String, a1)
  toJSON (ExternDeclaration a1 a2 a3) = toJSON ("ExternDeclaration" :: String, a1, a2, a3)
  toJSON (ExternDataDeclaration a1 a2 a3) = toJSON ("ExternDataDeclaration" :: String, a1, a2, a3)
  toJSON (FixityDeclaration a1 a2) = toJSON ("FixityDeclaration" :: String, a1, a2)
  toJSON (ImportDeclaration a1 a2 a3 a4) = toJSON ("ImportDeclaration" :: String, a1, a2, a3, a4)
  toJSON (TypeClassDeclaration a1 a2 a3 a4 a5 a6) = toJSON ("TypeClassDeclaration" :: String, a1, a2, a3, a4, a5, a6)
  toJSON (TypeInstanceDeclaration a1 a2 _ a4 a5 a6 a7 a8 a9) = toJSON ("TypeInstanceDeclaration" :: String, a1, a2, a4, a5, a6, a7, a8, a9)

instance ToJSON DataConstructorDeclaration where
  toJSON (DataConstructorDeclaration { dataCtorAnn, dataCtorName, dataCtorFields }) = object [ "dataCtorAnn" .= dataCtorAnn, "dataCtorName" .= dataCtorName, "dataCtorFields" .= dataCtorFields ]

instance ToJSON KindSignatureFor where
  toJSON DataSig = "DataSig"
  toJSON NewtypeSig = "NewtypeSig"
  toJSON TypeSynonymSig = "TypeSynonmSig"
  toJSON ClassSig = "ClassSig"

instance ToJSON RoleDeclarationData where
  toJSON (RoleDeclarationData { rdeclSourceAnn, rdeclIdent, rdeclRoles }) = object [ "rdeclSourceAnn" .= rdeclSourceAnn, "rdeclIdent" .= rdeclIdent, "rdeclRoles" .= rdeclRoles ]

instance ToJSON TypeDeclarationData where
  toJSON (TypeDeclarationData { tydeclSourceAnn, tydeclIdent, tydeclType }) = object [ "tydeclSourceAnn" .= tydeclSourceAnn, "tydeclIdent" .= tydeclIdent, "tydeclType" .= tydeclType ]

instance ToJSON (ValueDeclarationData [GuardedExpr]) where
  toJSON (ValueDeclarationData {valdeclSourceAnn, valdeclIdent, valdeclName, valdeclBinders, valdeclExpression}) = object [
    "valdeclSourceAnn" .= valdeclSourceAnn,
    "valdeclIdent" .= valdeclIdent,
    "valdeclName" .= valdeclName,
    "valdeclBinders" .= valdeclBinders,
    "valdeclExpression" .= valdeclExpression
    ]

instance ToJSON NameKind where
  toJSON Private = "Private"
  toJSON Public = "Public"
  toJSON External = "External"

instance ToJSON Binder where
  toJSON NullBinder = toJSON ("NullBinder" :: String)
  toJSON (LiteralBinder a1 a2) = toJSON ("LiteralBinder" :: String, a1, a2)
  toJSON (VarBinder a1 a2) = toJSON ("VarBinder" :: String, a1, a2)
  toJSON (ConstructorBinder a1 a2 a3) = toJSON ("ConstructorBinder" :: String, a1, a2, a3)
  toJSON (OpBinder a1 a2) = toJSON ("OpBinder" :: String, a1, a2)
  toJSON (BinaryNoParensBinder a1 a2 a3) = toJSON ("BinaryNoParensBinder" :: String, a1, a2, a3)
  toJSON (ParensInBinder a1) = toJSON ("ParensInBinder" :: String, a1)
  toJSON (NamedBinder a1 a2 a3) = toJSON ("NamedBinder" :: String, a1, a2, a3)
  toJSON (PositionedBinder a1 a2 a3) = toJSON ("PositionedBinder" :: String, a1, a2, a3)
  toJSON (TypedBinder a1 a2) = toJSON ("TypedBinder" :: String, a1, a2)

instance ToJSON a => ToJSON (Literal a) where
  toJSON (NumericLiteral a1) = toJSON ("NumbericLiteral" :: String, a1)
  toJSON (StringLiteral a1) = toJSON ("StringLiteral" :: String, a1)
  toJSON (CharLiteral a1) = toJSON ("CharLiteral" :: String, a1)
  toJSON (BooleanLiteral a1) = toJSON ("BooleanLiteral" :: String, a1)
  toJSON (ArrayLiteral a1) = toJSON ("ArrayLiteral" :: String, a1)
  toJSON (ObjectLiteral a1) = toJSON ("ObjectLioteral" :: String, a1)

instance ToJSON GuardedExpr where
  toJSON (GuardedExpr a1 a2) = toJSON ("CoardedExpr" :: String, a1, a2)

instance ToJSON Guard where
  toJSON (ConditionGuard a1) = toJSON ("ConditionGuard" :: String, a1)
  toJSON (PatternGuard a1 a2) = toJSON ("PatternGuard" :: String, a1, a2)

instance ToJSON Expr where
  toJSON (Literal a1 a2) = toJSON ("Literal" :: String, a1, a2)
  toJSON (UnaryMinus a1 a2) = toJSON ("UnaryMinus" :: String, a1, a2)
  toJSON (BinaryNoParens a1 a2 a3) = toJSON ("BinaryNoParens" :: String, a1, a2, a3)
  toJSON (Parens a1) = toJSON ("Parens" :: String, a1)
  toJSON (Accessor a1 a2) = toJSON ("Accessor" :: String, a1, a2)
  toJSON (ObjectUpdate a1 a2) = toJSON ("ObjectUpdate" :: String, a1, a2)
  toJSON (ObjectUpdateNested a1 a2) = toJSON ("ObjectUpdateNested" :: String, a1, a2)
  toJSON (Abs a1 a2) = toJSON ("Abs" :: String, a1, a2)
  toJSON (App a1 a2) = toJSON ("App" :: String, a1, a2)
  toJSON (Unused a1) = toJSON ("Unused" :: String, a1)
  toJSON (Var a1 a2) = toJSON ("Var" :: String, a1, a2)
  toJSON (Op a1 a2) = toJSON ("Op" :: String, a1, a2)
  toJSON (IfThenElse a1 a2 a3) = toJSON ("IfThenElse" :: String, a1, a2, a3)
  toJSON (Constructor a1 a2) = toJSON ("Constructor" :: String, a1, a2)
  toJSON (Case a1 a2) = toJSON ("Case" :: String, a1, a2)
  toJSON (TypedValue a1 a2 a3) = toJSON ("TypedValue" :: String, a1, a2, a3)
  toJSON (Let a1 a2 a3) = toJSON ("Let" :: String, a1, a2, a3)
  toJSON (Do a1 a2) = toJSON ("Do" :: String, a1, a2)
  toJSON (Ado a1 a2 a3) = toJSON ("Ado" :: String, a1, a2, a3)
  toJSON (TypeClassDictionary a1 a2 a3) = error "Got TypeClassDictionary"
  toJSON (DeferredDictionary a1 a2) = toJSON ("DeferredDictionary" :: String, a1, a2)
  toJSON (DerivedInstancePlaceholder a1 a2) = toJSON ("DerivedInstancePlaceholder" :: String, a1, a2)
  toJSON AnonymousArgument = toJSON ["AnonymousArgument" :: String]
  toJSON (Hole a1) = toJSON ("Hole" :: String, a1)
  toJSON (PositionedValue a1 a2 a3) = toJSON ("PositionedValue" :: String, a1, a2, a3)

instance ToJSON WhereProvenance where
  toJSON FromWhere = "FromWhere"
  toJSON FromLet = "FromLet"

instance ToJSON t => ToJSON (PathTree t) where
  toJSON :: ToJSON t => PathTree t -> Value
  toJSON (PathTree a) = toJSON $ runAssocList a

instance ToJSON t => ToJSON (PathNode t) where
  toJSON (Leaf a1) = toJSON ("Leaf" :: String, a1)
  toJSON (Branch a1) = toJSON ("Branch" :: String, a1)

instance ToJSON CaseAlternative where
  toJSON (CaseAlternative { caseAlternativeBinders, caseAlternativeResult }) = object [ "caseAlternativeBinders" .= caseAlternativeBinders, "caseAlternativeResult" .= caseAlternativeResult ]

instance ToJSON DoNotationElement where
  toJSON (DoNotationValue a1) = toJSON ("DoNotationValue" :: String, a1)
  toJSON (DoNotationBind a1 a2) = toJSON ("DoNotationValue" :: String, a1, a2)
  toJSON (DoNotationLet a1) = toJSON ("DoNotationValue" :: String, a1)
  toJSON (PositionedDoNotationElement a1 a2 a3) = toJSON ("DoNotationValue" :: String, a1, a2, a3)

instance ToJSON InstanceDerivationStrategy where
  toJSON KnownClassStrategy = "KnownClassStrategy"
  toJSON NewtypeStrategy = "NewtypeStrategy"

instance ToJSON ValueFixity where
  toJSON (ValueFixity a1 a2 a3) = toJSON ("ValueFixity" :: String, a1, a2, a3)

instance ToJSON TypeFixity where
  toJSON (TypeFixity a1 a2 a3) = toJSON ("TypeFixity" :: String, a1, a2, a3)

-- instance ToJSON ChainId where
--   toJSON (ChainId a) = toJSON ("ChainId" :: String, a)

instance ToJSON TypeInstanceBody where
  toJSON DerivedInstance = "DerivedInstance"
  toJSON NewtypeInstance = "NewtypeInstance"
  toJSON (ExplicitInstance declarations) = toJSON declarations

main :: IO ()
main = do
  source <- pack <$> getContents
  args <- getArgs

  let filename = case args of
        (filename:_) -> filename
        _ -> ""

  print source

  let parseResult = parse source

  case parseResult of
    (_, Right mod) -> do
      let astMod = convertModule filename mod

      pPrint astMod
      print astMod

      print $ encode astMod

      -- let errors = runWriter $ lint astMod

      -- print "lint errors:"
      -- print errors
    _ -> mempty

  -- pPrint parseResult
