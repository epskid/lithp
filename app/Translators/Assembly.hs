module Translators.Assembly (translator) where

import Data.Map (Map, fromList, member, (!))
import Emit
import Translators.Common

prelude :: String
prelude =
  "format ELF64\n"
    ++ "public _start\n"

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

intrinsics :: Map String String
intrinsics =
  fromList
    [ ("+", "add rdi, rsi\npush rdi"),
      ("-", "sub rdi, rsi\npush rdi"),
      ("*", "mov rax, rdi\nimul rsi\npush rax"),
      ("/", "mov rax, rdi\nidiv rsi\npush rax")
    ]

inst :: Instruction -> String
inst (PushName name) = "mov rax, _lithp_" ++ name ++ "\npush rax"
inst (PushInt int) = "mov rax, " ++ show int ++ "\npush rax"
inst (PushFloat _) = error "floats are unsupported in assembly"
inst (PushString idx) = "mov rax, _s" ++ show idx ++ "\npush rax"
inst (Call argc name)
  | name `member` intrinsics = intrinsics ! name
  | otherwise =
      let loadArgs = [if i < argc then "pop " ++ reg else "xor " ++ reg ++ ", " ++ reg | (reg, i) <- zip systemVRegs [0 ..]]
       in unlines loadArgs ++ "call " ++ name ++ "\npush rax"

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
