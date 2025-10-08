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

systemVRegs :: [String]
systemVRegs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

inst :: Instruction -> String
inst (PushName name) = "mov rdi, _lithp_" ++ name ++ "\npush rdi"
inst (PushInt int) = "mov rdi, " ++ show int ++ "\npush rdi"
inst (PushString idx) = "mov rdi, _s" ++ show idx ++ "\npush rdi"
inst (Call argc name) =
  let loadArgs = [if i < argc then "pop " ++ reg else "xor " ++ reg ++ ", " ++ reg | (reg, i) <- zip systemVRegs [0 ..]]
   in unlines loadArgs ++ "call " ++ name

insts :: [Instruction] -> String
insts is =
  let ss = map inst is
   in "section '.text' executable\n"
        ++ "_start:\n"
        ++ unlines ss
        ++ "xor rdi, rdi\n"
        ++ "mov rax, 60\n"
        ++ "syscall\n"

emission :: Emission -> String
emission (Emission s i) = prelude ++ insts i ++ strtab s
