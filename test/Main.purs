module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (fromMaybe, Maybe(Just))
import Matrix (repeat, zipWith, height, width, empty, fromArray, get)
import Node.Process (PROCESS)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main ∷ ∀ e. Eff (process ∷ PROCESS , console ∷ CONSOLE | e) Unit
main = run [consoleReporter] do
  describe "purescript-matrices" do
    describe "Creating matrices" do
      it "repeat" do
        let m = repeat 2 2 5
        Just m `shouldEqual` fromArray [[5, 5], [5, 5]]
      it "creates a matrix from an array" do
        let m = fromMaybe empty (fromArray [[1,2,3], [4,5,6]])
        width m `shouldEqual` 3
        height m `shouldEqual` 2
        get 1 0 m `shouldEqual` Just 2
    describe "Operations on matrices" do
      it "zips two matrices" do
        let m = fromMaybe empty (zipWith (+) (repeat 2 2 1) (repeat 2 2 1))
        m `shouldEqual` repeat 2 2 2
