{-
  In this experiment I am trying to emulate the manual assembly
  of deeply nested and injected dependencies with the help of TH
  and config ADTs

  (+) Supports values to be injected
  (+) Supports functions to be injected
  (++) Supports overriding of arbitrary number and depth of dependencies
  (++) Compile time type checking (despites strings being used, those too are checked)
  (+) Supports type variables 
  (+) Theoretically also supports surgically only overriding some subsets of dependencies

  (+) emulates how a human would do DI by hand, and does the hard work automatically

  (-) Limited module support, requires re-exporting
    (?) Could it be possible to pass the default dependencies along in `Defs` somehow?
      That could get rid of this issue
  (?) How is performance impacted? Does GHC notice `f (g x) (g x)`? 
  
  [ ] TODO: make multiple argumetns work
  [ ] TODO: reorder arguments of override
-}

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import qualified Data.Map as Map
import Data.Dynamic
import Data.Maybe
import Main10TH
import Main10App
import Language.Haskell.TH


($>) = flip ($)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "# mapDefs"

  mapDefs (const "2" ) (Leaf "1") `shouldBe` (Leaf "2")
  mapDefs (const 2) (Leaf "1") `shouldBe` (Leaf 2)
  
  putStrLn ""
  putStrLn "# convertDefsToExp"

  convertDefsToExp (Leaf "a") `shouldBe` (VarE $ mkName "a")

  convertDefsToExp (Cons "a" [Leaf "b"]) `shouldBe`
    (AppE (VarE $ mkName "a") (VarE $ mkName "b"))

  convertDefsToExp (Cons "a" [Leaf "b", Leaf "c"]) `shouldBe`
    (AppE (AppE (VarE $ mkName "a") (VarE $ mkName "b")) (VarE $ mkName "c"))

  putStrLn ""
  putStrLn "# override fn"

  override (Leaf "a") "a" "b" `shouldBe` Leaf "b"
  override (Leaf "a") "a" "c" `shouldBe` Leaf "c"
  override (Leaf "a") "b" "c" `shouldBe` Leaf "a"

  override (Cons "b" [Leaf "a"]) "x" "c" `shouldBe` (Cons "b" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "b" "c" `shouldBe` (Cons "c" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c"])

  override (Cons "b" [Leaf "a", Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c", Leaf "c"])

  putStrLn ""
  putStrLn "# assemble"
  
  $(assemble barD) `shouldBe` 2

  putStrLn ""
  putStrLn "# mocking"

  let
    fooDMock = Leaf "fooMock"
    fooMock = 33 
  $(assemble $ override barD "foo" "fooMock") `shouldBe` 34

  putStrLn ""
  putStrLn "# type variable support"

  $(assemble $ idTestD) `shouldBe` 3

  let
    idDMock = Leaf "idMock"
    idMock = (+1) 
  $(assemble $ override idTestD "id" "idMock") `shouldBe` 4

  putStrLn ""
  putStrLn "# module support"
  
  $(assemble $ testModuleD) `shouldBe` 12


shouldBe actual expected | actual == expected = putStrLn $ "OK " ++ show actual
                         | otherwise          = error $ "FAIL " ++ show actual ++ " /= " ++ show expected
