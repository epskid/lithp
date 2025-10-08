module Translators.Python (translator) where

import Emit
import Translators.Common
import Data.List

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
inst (LoadNamePush name) = "stack.append(" ++ name ++ ")"
inst (LoadStringPush idx) = "stack.append(strtab[" ++ show idx ++ "])"
inst (PushInt int) = "stack.append(" ++ show int ++ ")"
inst (Call name) = "stack.append(scope[" ++ show name ++ "]())"

insts :: [Instruction] -> String
insts is =
  let ss = map inst is
   in "stack = []\n"
        ++ "scope = {\n"
        ++ "'println': lambda: print(stack.pop()),\n"
        ++ "'+': lambda: stack.pop() + stack.pop(),\n"
        ++ "}\n"
        ++ unlines ss

emission :: Emission -> String
emission (Emission s i) = strtab s ++ insts i
