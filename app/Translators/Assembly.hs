module Translators.Assembly (translator) where

import Emit
import Translators.Common

prelude :: String
prelude =
  "format ELF64\n"
    ++ "public _start\n"
    ++ "extrn puts\n"

translator :: Translator
translator =
  Translator
    { translateEmission = emission
    }

strtab :: [String] -> String
strtab tab =
  let ss = zipWith wrapIt [0 ..] tab
   in "section '.data' writable\n"
        ++ unlines ss
  where
    wrapIt :: Integer -> String -> String
    wrapIt i s = "_s" ++ show i ++ " db \"" ++ s ++ "\", 0"

inst :: Instruction -> String
inst (Call name) = "call " ++ name
inst (LoadStringPush idx) = "mov rdi, _s" ++ show idx ++ "\npush rdi"
inst (PushInt int) = "mov rdi, " ++ show int ++ "\npush rdi"

insts :: [Instruction] -> String
insts is =
  let ss = map inst is
   in "section '.text' executable\n"
        ++ "println:\n"
        ++ "push rbp\n"
        ++ "mov rbp, rsp\n"
        ++ "mov rdi, [rbp + 16]\n"
        ++ "xor eax, eax\n"
        ++ "call puts\n"
        ++ "pop rbp\n"
        ++ "ret 16\n"
        ++ "_start:\n"
        ++ unlines ss
        ++ "xor rdi, rdi\n"
        ++ "mov rax, 60\n"
        ++ "syscall\n"

emission :: Emission -> String
emission (Emission s i) = prelude ++ insts i ++ strtab s
