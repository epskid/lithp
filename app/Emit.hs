module Emit (Instruction) where

data Value
  = ImmInteger Integer
  | ImmFloat Float
  | ImmString String
  | ValueRef String
  deriving (Show)

data Instruction
  = Label String
  | Return
  | Call String [Value]
  | PushScope
  | Ref String Value
  | PopScope
  deriving (Show)
