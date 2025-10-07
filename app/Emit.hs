module Emit (emit, Emission (..), Instruction (..)) where

import Control.Monad.State.Lazy
import Data.List
import Parse

data Instruction = LoadNamePush String | PushInt Integer | LoadStringPush Int | Call String deriving (Show)

data Emission = Emission
  { emissionStrtab :: [String],
    emissionInsts :: [Instruction]
  }
  deriving (Show)

emptyEmission :: Emission
emptyEmission =
  Emission
    { emissionStrtab = [],
      emissionInsts = []
    }

type Emitter = State Emission ()

pushInst :: Instruction -> Emission -> Emission
pushInst inst (Emission s is) =
  Emission
    { emissionStrtab = s,
      emissionInsts = is ++ [inst]
    }

pushStrtab :: String -> Emission -> Emission
pushStrtab str (Emission s is) =
  Emission
    { emissionStrtab = s ++ [str],
      emissionInsts = is
    }

literal :: LiteralT -> Emitter
literal (LiteralAtom (AtomID name)) = modify (pushInst (LoadNamePush name))
literal (LiteralInteger int) = modify (pushInst (PushInt int))
literal (LiteralString str) = do
  strtab <- emissionStrtab <$> get
  case elemIndex str strtab of
    Just i -> modify (pushInst (LoadStringPush i))
    Nothing -> do
      let i = length strtab
      modify (pushStrtab str)
      modify (pushInst (LoadStringPush i))

sexpr :: SExprT -> Emitter
sexpr (SExprLiteral lit) = literal lit
sexpr (SExprInner (AtomID op) args) = do
  mapM_ sexpr (reverse args)
  modify (pushInst (Call op))

sexprs :: [SExprT] -> Emitter
sexprs = foldr ((*>) . sexpr) (return ())

ast :: AST -> Emitter
ast (ASTSExprs es) = sexprs es

emit :: AST -> Emission
emit a = execState (ast a) emptyEmission
