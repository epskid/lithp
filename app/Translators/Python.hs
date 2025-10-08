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
inst (PushFloat fl) = "stack.append(" ++ show fl ++ ")"
inst (PushString idx) = "stack.append(strtab[" ++ show idx ++ "])"
inst (Call argc name) =
  let pops = replicate argc "stack.pop()"
   in "stack.append(intrinsics[" ++ show name ++ "](" ++ intercalate "," pops ++ "))"

insts :: [Instruction] -> String
insts is =
  let ss = map inst is
   in "stack = []\n"
        ++ "intrinsics = {\n"
        ++ "'print': lambda x: print(x),\n"
        ++ "'+': lambda x, y: x + y,\n"
        ++ "'-': lambda x, y: x - y,\n"
        ++ "'*': lambda x, y: x * y,\n"
        ++ "'/': lambda x, y: x / y,\n"
        ++ "'format': lambda s, *args: s.format(*args),\n"
        ++ "}\n"
        ++ unlines ss

emission :: Emission -> String
emission (Emission s i) = strtab s ++ insts i
