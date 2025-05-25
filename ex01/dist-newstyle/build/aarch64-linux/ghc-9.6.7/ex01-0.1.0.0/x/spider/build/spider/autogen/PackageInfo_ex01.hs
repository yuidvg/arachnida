{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ex01 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ex01"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Web scraper for images"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
