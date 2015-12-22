{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module JavaTH where

import Language.Haskell.TH
import Language.Java.Syntax
--import Data.Derive.TopDown.StandaloneDerive
import Generics.SOP
import Generics.SOP.TH

import Control.Monad.State
import Control.Monad.Trans

import Data.Set
import Data.Map

--derivings True [''Generic] ''CompilationUnit

{-
allChildren :: Name -> [Name]

allChildren name = do
  TyConI dec <- reify name
  let DataD ctxt name bndrs cons derivs = dec
-}

data A = A1 (B C) | A2 C | A3
data B b = B1 A | B2 | B3 b
data C = C1 | C2 (B A)

deriveGeneric ''A
deriveGeneric ''B
--deriveGeneric ''C


searchName name = searchInfo <$> (lift $ reify name)

searchInfo (TyConI dec) = searchDec dec 
searchInfo (PrimTyConI name arity unlifted) = return ()
searchInfo info = fail (show info)

searchDec (DataD ctxt name bndrs cons derivs) = forM_ cons (searchCon bndrs)
searchDec (TySynD name bndrs typ) = searchType bndrs typ
searchDec dec = fail (show dec)

searchCon env (NormalC name ts) = forM_ ts (searchType bndrs . snd)
searchCon env (RecC name ts) = forM_ ts (searchType bndrs . \(_, _, t) -> t)
searchCon env  con = fail (show con)

resolveType env = f where
  f (AppT t1 t2) = AppT (f t1) (f t2)
  f (VarT name) = env ! name
  f typ = typ

searchType typ = do
  new <- notMember typ <$> get
  

searchType (ConT name) = searchName name
searchType (AppT t1 t2) = searchType t1 >> searchType t2
searchType (VarT _) = return ()
searchType ListT = return ()
searchType (TupleT _) = return ()

searchType bndrs typ = fail (show typ)

primitives = Data.Set.fromList [''Char, ''Int, ''Float, ''Double, ''String, ''Integer]

javaSyntax = do
  names <- execStateT (searchName ''CompilationUnit) primitives
  
  return $ toList (difference names primitives)

{-
appTyVars :: Name -> [TyVarBndr] -> Q Type
appTyVars n = go (conT n)
  where
    go :: Q Type -> [TyVarBndr] -> Q Type
    go t []                  = t
    go t (PlainTV  v   : vs) = go [t| $t $(varT v) |] vs
    go t (KindedTV v _ : vs) = go [t| $t $(varT v) |] vs
-}

--conInfo :: Con -> Q (Name, [Q Type])
--conInfo (NormalC n ts) = return (n, map (return . (\(_, t)    -> t)) ts)
--conInfo (RecC    n ts) = return (n, map (return . (\(_, _, t) -> t)) ts)
--conInfo (InfixC (_, t) n (_, t')) = return (n, map return [t, t'])
--conInfo (ForallC _ _ _) = fail "Existentials not supported"

