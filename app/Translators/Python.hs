module Translators.Python (translator) where

import Data.List
import Emit
import Translators.Common

translator :: Translator
translator =
  Translator
    { translateEmission = emission
    }

strtab :: [String] -> String
strtab tab =
  let ss = map show tab
   in "strtab = [" ++ intercalate "," ss ++ "]\n"

inst :: Instruction -> String
inst (PushName name) = "stack.append(" ++ name ++ ")"
inst (PushInt int) = "stack.append(" ++ show int ++ ")"
inst (PushString idx) = "stack.append(strtab[" ++ show idx ++ "])"
inst (Call argc name) =
  let pops = replicate argc "stack.pop()"
   in "stack.append(scope[" ++ show name ++ "](" ++ intercalate "," pops ++ "))"

insts :: [Instruction] -> String
insts is =
  let ss = map inst is
   in "stack = []\n"
        ++ "scope = {\n"
        ++ "'println': lambda x: print(x),\n"
        ++ "'+': lambda x, y: x + y,\n"
        ++ "}\n"
        ++ unlines ss

emission :: Emission -> String
emission (Emission s i) = strtab s ++ insts i
