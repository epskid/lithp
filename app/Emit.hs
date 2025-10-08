module Emit (emit, Emission (..), Instruction (..)) where

import Control.Monad.State.Lazy
import Data.List
import Parse

data Instruction
  = PushName String
  | PushInt Integer
  | PushString Int
  | Call Int String
  deriving (Show)

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
pushInst inst e@(Emission {emissionInsts = is}) =
  e
    { emissionInsts = is ++ [inst]
    }

pushStrtab :: String -> Emission -> Emission
pushStrtab str e@(Emission {emissionStrtab = s}) =
  e
    { emissionStrtab = s ++ [str]
    }

literal :: LiteralT -> Emitter
literal (LiteralAtom (AtomID name)) = modify (pushInst (PushName name))
literal (LiteralInteger int) = modify (pushInst (PushInt int))
literal (LiteralString str) = do
  strtab <- emissionStrtab <$> get
  case elemIndex str strtab of
    Just i -> modify (pushInst (PushString i))
    Nothing -> do
      let i = length strtab
      modify (pushStrtab str)
      modify (pushInst (PushString i))

sexpr :: SExprT -> Emitter
sexpr (SExprLiteral lit) = literal lit
sexpr (SExprInner (AtomID op) args) = do
  mapM_ sexpr (reverse args)
  modify (pushInst (Call (length args) op))

sexprs :: [SExprT] -> Emitter
sexprs = mapM_ sexpr

ast :: AST -> Emitter
ast (ASTSExprs es) = sexprs es

emit :: AST -> Emission
emit a = execState (ast a) emptyEmission
