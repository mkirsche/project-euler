{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Digit.Test
import                   Test.Framework

main = htfMain htf_importedTests
