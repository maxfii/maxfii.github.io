{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_typst (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "typst"
version :: Version
version = Version [0,6,1] []

synopsis :: String
synopsis = "Parsing and evaluating typst syntax."
copyright :: String
copyright = "Copyright 2023 John MacFarlane"
homepage :: String
homepage = ""
