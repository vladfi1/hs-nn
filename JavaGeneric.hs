{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fcontext-stack=200 #-}

module JavaGeneric where

import Language.Java.Syntax

import Generics.SOP
import Generics.SOP.TH

--import List
import Generics.SOP.Dict
import Grammar

import GHC.Exts (Constraint)

-- use a type family to break the cyclic class
-- also to do case analysis on the type
type family AllGeneric' x :: Constraint where
  AllGeneric' Char = ()
  AllGeneric' Int = ()
  AllGeneric' Double = ()
  AllGeneric' Integer = ()
  AllGeneric' x = (Generic x, All2 AllGeneric (Code x))

class AllGeneric' x => AllGeneric x
instance AllGeneric' x => AllGeneric x

concat <$> traverse deriveGeneric
  [''CompilationUnit
  ,''TypeDecl
  ,''PackageDecl
  ,''Name
  ,''InterfaceDecl
  ,''TypeParam
  ,''RefType
  ,''Type
  ,''PrimType
  ,''Modifier
  ,''InterfaceBody
  ,''MemberDecl
  ,''VarDecl
  ,''VarInit
  ,''VarDeclId
  ,''MethodBody
  ,''ImportDecl
  ,''Ident
  ,''FormalParam
  ,''Exp
  ,''TypeArgument
  ,''WildcardBound
  ,''Op
  ,''MethodInvocation
  ,''Literal
  ,''Lhs
  ,''FieldAccess
  ,''ConstructorBody
  ,''ExplConstrInv
  ,''ClassType
  ,''ClassDecl
  ,''ClassBody
  ,''Annotation
  ,''ArrayInit
  ,''ArrayIndex
  ,''AssignOp
  ,''Block
  ,''BlockStmt
  ,''Stmt
  ,''SwitchBlock
  ,''SwitchLabel
  ,''Decl
  ,''ElementValue
  ,''EnumBody
  ,''EnumConstant
  ,''ForInit
  ,''Catch
  ]

allGeneric :: Dict AllGeneric CompilationUnit
allGeneric = Dict

type GenericTypes =
  [ CompilationUnit
  , TypeDecl
  , PackageDecl
  , Name
  , InterfaceDecl
  , TypeParam
  , RefType
  , Type
  , PrimType
  , Modifier
  , InterfaceBody
  , MemberDecl
  , VarDecl
  , VarInit
  , VarDeclId
  , MethodBody
  , ImportDecl
  , Ident
  , FormalParam
  , Exp
  , TypeArgument
  , WildcardBound
  , Op
  , MethodInvocation
  , Literal
  , Lhs
  , FieldAccess
  , ConstructorBody
  , ExplConstrInv
  , ClassType
  , ClassDecl
  , ClassBody
  , Annotation
  , ArrayInit
  , ArrayIndex
  , AssignOp
  , Block
  , BlockStmt
  , Stmt
  , SwitchBlock
  , SwitchLabel
  , Decl
  , ElementValue
  , EnumBody
  , EnumConstant
  , ForInit
  , Catch
  , Bool
  , [Char]
  , [(Ident, [TypeArgument])]
  , (Ident, [TypeArgument])
  , [TypeArgument]
  , [(Ident, ElementValue)]
  , (Ident, ElementValue)
  , [VarInit]
  , [VarDecl]
  , [TypeParam]
  , [TypeDecl]
  , [SwitchBlock]
  , [RefType]
  , [Modifier]
  , [MemberDecl]
  , [ImportDecl]
  , [Ident]
  , [FormalParam]
  , [Argument]
  , [EnumConstant]
  , [Decl]
  , [Catch]
  , [BlockStmt]
  , [Exp]
  , Maybe [Exp]
  , Maybe Exp
  , Maybe WildcardBound
  , Maybe VarInit
  , Maybe Type
  , Maybe RefType
  , Maybe PackageDecl
  , Maybe Ident
  , Maybe ForInit
  , Maybe ExplConstrInv
  , Maybe ClassBody
  , Maybe Block
  ]


type PrimTypes = [Char, Int, Integer, Double]
type AllTypes = Char ': Int ': Integer ': Double ': GenericTypes

javaComplete :: Complete PrimTypes GenericTypes
javaComplete = Dict
