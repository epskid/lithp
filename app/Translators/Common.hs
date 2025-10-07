module Translators.Common (Translator (..), translate) where

import Emit

translate :: Translator -> Emission -> String
translate = translateEmission

newtype Translator = Translator
  { translateEmission :: Emission -> String
  }
