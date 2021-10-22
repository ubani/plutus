{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- {-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Main where

import           Data.Semigroup
import           Data.Text                                (Text)
import qualified PlutusTx
import qualified PlutusTx.Prelude                         as PlutusTx
import           System.TimeIt                            (timeItNamed)
-- import Control.Exception (evaluate)
import           Control.Monad.Except                     (runExceptT)
import qualified Plutus.V1.Ledger.Scripts                 as Ledger
import qualified PlutusCore                               as PLC
import qualified PlutusCore.Evaluation.Machine.ExBudget   as PLC
import           PlutusTx.Code                            (CompiledCode, getPlc, sizePlc)
import qualified PlutusTx.Evaluation                      as PlutusTx
import           PlutusTx.TH                              (compile)
import           Prelude                                  (($))
import qualified Prelude
import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

{-# INLINABLE mkValidator #-}
mkValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkValidator _ r _
    | r PlutusTx.== PlutusTx.toBuiltinData (42 :: Prelude.Integer) = ()
    | Prelude.otherwise = PlutusTx.traceError "wrong redeemer"

validator :: Ledger.Validator
validator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

{-# INLINABLE myCompiledFunction #-}
-- compiledMySum = $$(compile [|| PlutusTx.sum ([] :: [PlutusTx.Integer]) ||])
myCompiledFunction :: CompiledCode Prelude.Bool
-- myCompiledFunction = $$(PlutusTx.compile [|| Prelude.True PlutusTx.Bool.&& Prelude.True ||])
myCompiledFunction = $$(PlutusTx.compile [|| (1 :: Prelude.Integer) PlutusTx.== 1 ||])

main :: Prelude.IO ()
main = do
  Prelude.putStrLn "Evaluate each function with lists of 10K, 20K, 30K, 40K and 50K elements"
  Prelude.putStrLn ""

  Prelude.putStrLn "-----------------------------------------------------------------------------"
  timeItNamed "foldMap (: []) [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMap (: []) (range 1 10000) ||])
  timeItNamed "foldMap (: []) [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMap (: []) (range 1 20000) ||])
  timeItNamed "foldMap (: []) [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMap (: []) (range 1 30000) ||])
  timeItNamed "foldMap (: []) [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMap (: []) (range 1 40000) ||])
  timeItNamed "foldMap (: []) [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMap (: []) (range 1 50000) ||])
  Prelude.putStrLn ""

  timeItNamed "foldMapTR (: []) [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMapTR (: []) (range 1 10000) ||])
  timeItNamed "foldMapTR (: []) [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMapTR (: []) (range 1 20000) ||])
  timeItNamed "foldMapTR (: []) [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMapTR (: []) (range 1 30000) ||])
  timeItNamed "foldMapTR (: []) [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMapTR (: []) (range 1 40000) ||])
  timeItNamed "foldMapTR (: []) [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.foldMapTR (: []) (range 1 50000) ||])
  Prelude.putStrLn ""

  Prelude.putStrLn "-----------------------------------------------------------------------------"
  timeItNamed "PlutusTx.filter even [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filter PlutusTx.even (range 1 100000) ||])
  timeItNamed "PlutusTx.filter even [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filter PlutusTx.even (range 1 200000) ||])
  timeItNamed "PlutusTx.filter even [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filter PlutusTx.even (range 1 300000) ||])
  timeItNamed "PlutusTx.filter even [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filter PlutusTx.even (range 1 400000) ||])
  timeItNamed "PlutusTx.filter even [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filter PlutusTx.even (range 1 500000) ||])
  Prelude.putStrLn ""

  timeItNamed "filterTR even [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filterTR PlutusTx.even (range 1 100000) ||])
  timeItNamed "filterTR even [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filterTR PlutusTx.even (range 1 200000) ||])
  timeItNamed "filterTR even [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filterTR PlutusTx.even (range 1 300000) ||])
  timeItNamed "filterTR even [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filterTR PlutusTx.even (range 1 400000) ||])
  timeItNamed "filterTR even [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.filterTR PlutusTx.even (range 1 500000) ||])
  Prelude.putStrLn ""

  timeItNamed "myFilter even [1..10000]" $ Prelude.print $ runPlc $$(compile [|| myFilter PlutusTx.even (range 1 100000) ||])
  timeItNamed "myFilter even [1..20000]" $ Prelude.print $ runPlc $$(compile [|| myFilter PlutusTx.even (range 1 200000) ||])
  timeItNamed "myFilter even [1..30000]" $ Prelude.print $ runPlc $$(compile [|| myFilter PlutusTx.even (range 1 300000) ||])
  timeItNamed "myFilter even [1..40000]" $ Prelude.print $ runPlc $$(compile [|| myFilter PlutusTx.even (range 1 400000) ||])
  timeItNamed "myFilter even [1..50000]" $ Prelude.print $ runPlc $$(compile [|| myFilter PlutusTx.even (range 1 500000) ||])
  Prelude.putStrLn ""

  Prelude.putStrLn "-----------------------------------------------------------------------------"
  timeItNamed "PlutusTx.find ((==) 0) [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.find ((PlutusTx.==) 0) (range 1 10000) ||])
  timeItNamed "PlutusTx.find ((==) 0) [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.find ((PlutusTx.==) 0) (range 1 20000) ||])
  timeItNamed "PlutusTx.find ((==) 0) [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.find ((PlutusTx.==) 0) (range 1 30000) ||])
  timeItNamed "PlutusTx.find ((==) 0) [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.find ((PlutusTx.==) 0) (range 1 40000) ||])
  timeItNamed "PlutusTx.find ((==) 0) [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.find ((PlutusTx.==) 0) (range 1 50000) ||])
  Prelude.putStrLn ""

  timeItNamed "findTR ((==) 0) [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.findTR ((PlutusTx.==) 0) (range 1 10000) ||])
  timeItNamed "findTR ((==) 0) [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.findTR ((PlutusTx.==) 0) (range 1 20000) ||])
  timeItNamed "findTR ((==) 0) [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.findTR ((PlutusTx.==) 0) (range 1 30000) ||])
  timeItNamed "findTR ((==) 0) [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.findTR ((PlutusTx.==) 0) (range 1 40000) ||])
  timeItNamed "findTR ((==) 0) [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.findTR ((PlutusTx.==) 0) (range 1 50000) ||])
  Prelude.putStrLn ""

  timeItNamed "myFind ((==) 0) [1..10000]" $ Prelude.print $ runPlc $$(compile [|| myFind ((PlutusTx.==) 0) (range 1 10000) ||])
  timeItNamed "myFind ((==) 0) [1..20000]" $ Prelude.print $ runPlc $$(compile [|| myFind ((PlutusTx.==) 0) (range 1 20000) ||])
  timeItNamed "myFind ((==) 0) [1..30000]" $ Prelude.print $ runPlc $$(compile [|| myFind ((PlutusTx.==) 0) (range 1 30000) ||])
  timeItNamed "myFind ((==) 0) [1..40000]" $ Prelude.print $ runPlc $$(compile [|| myFind ((PlutusTx.==) 0) (range 1 40000) ||])
  timeItNamed "myFind ((==) 0) [1..50000]" $ Prelude.print $ runPlc $$(compile [|| myFind ((PlutusTx.==) 0) (range 1 50000) ||])
  Prelude.putStrLn ""

  Prelude.putStrLn "-----------------------------------------------------------------------------"
  timeItNamed "PlutusTx.sum [1..10000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.sum (range 1 10000) ||])
  timeItNamed "PlutusTx.sum [1..20000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.sum (range 1 20000) ||])
  timeItNamed "PlutusTx.sum [1..30000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.sum (range 1 30000) ||])
  timeItNamed "PlutusTx.sum [1..40000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.sum (range 1 40000) ||])
  timeItNamed "PlutusTx.sum [1..50000]" $ Prelude.print $ runPlc $$(compile [|| PlutusTx.sum (range 1 50000) ||])
  Prelude.putStrLn ""

  timeItNamed "mySum [1..10000]" $ Prelude.print $ runPlc $$(compile [|| mySum (range 1 10000) ||])
  timeItNamed "mySum [1..20000]" $ Prelude.print $ runPlc $$(compile [|| mySum (range 1 20000) ||])
  timeItNamed "mySum [1..30000]" $ Prelude.print $ runPlc $$(compile [|| mySum (range 1 30000) ||])
  timeItNamed "mySum [1..40000]" $ Prelude.print $ runPlc $$(compile [|| mySum (range 1 40000) ||])
  timeItNamed "mySum [1..50000]" $ Prelude.print $ runPlc $$(compile [|| mySum (range 1 50000) ||])
  Prelude.putStrLn ""

  Prelude.putStrLn "-----------------------------------------------------------------------------"

--   -- timeItNamed "mySum" $ print $ mySum ls
--   -- timeItNamed "mySum'" $ print $ mySum' ls
--   -- timeItNamed "PlutusTx.sum" $ print $ PlutusTx.sum ls
--   -- timeItNamed "Prelude.sum" $ print $ sum ls

--   -- timeItNamed "PlutusTx.any" $ print $ PlutusTx.any (\n -> n > 4000000) ls
--   -- timeItNamed "any" $ print $ any (\n -> n > 4000000) ls
--   -- timeItNamed "myAny" $ print $ myAny (\n -> n > 4000000) ls

--   -- timeItNamed "PlutusTx.find" $ print $ PlutusTx.find (\n -> n > 4000000) ls
--   -- timeItNamed "find" $ print $ find (\n -> n > 4000000) ls
--   -- timeItNamed "myFind" $ print $ myFind (\n -> n > 4000000) ls

--   -- timeItNamed "PlutusTx.elem" $ print $  PlutusTx.elem 0 ls
--   -- timeItNamed "elem" $ print $ elem 0 ls
--   -- timeItNamed "myElem" $ print $ myElem 0 ls
--   -- timeItNamed "myElem'" $ print $ myElem' 0 ls

  Prelude.return ()

runPlc :: CompiledCode a  -> Prelude.Maybe (PLC.ExBudget, [Text])
runPlc compiledCode =
  let programE = PLC.runQuote
               $ runExceptT @PLC.FreeVariableError
               $ UPLC.unDeBruijnProgram
               $ getPlc compiledCode
   in case programE of
        Prelude.Left _ -> Prelude.Nothing
        Prelude.Right program ->
          let (logOut, UPLC.TallyingSt _ budget, _) = PlutusTx.evaluateCekTrace program
           in Prelude.Just (budget, logOut)

{-# INLINABLE range #-}
range :: Prelude.Integer -> Prelude.Integer -> [Prelude.Integer]
range begin end = go end []
  where
    go i acc | i PlutusTx.>= begin = go (i PlutusTx.- 1) (i : acc)
    go _ acc = acc

{-# INLINABLE myFind #-}
myFind :: (a -> Prelude.Bool) -> [a] -> Prelude.Maybe a
myFind _ [] = Prelude.Nothing
myFind f (a:_) | f a = Prelude.Just a
myFind f (_:as) = myFind f as

{-# INLINABLE myFilter #-}
myFilter :: (a -> Prelude.Bool) -> [a] -> [a]
myFilter p ls =
  let go' (x:xs) ys | p x               = next $ x:ys
                    | Prelude.otherwise = next ys
                   where next = go' xs
      go' []     ys                   = PlutusTx.reverse ys
   in go' ls []

{-# INLINABLE mySum #-}
mySum :: (PlutusTx.AdditiveMonoid a) => [a] -> a
mySum xs = getSum $ go xs PlutusTx.mempty
  where go [] s     = s
        go (a:as) s = go as (Sum a PlutusTx.<> s)

-- myElem :: Eq a => a -> [a] -> Bool
-- myElem x xs = go xs False
--   where go _ True = True
--         go [] s = s
--         go (a:as) _ = go as $! x == a

-- myElem' :: Eq a => a -> [a] -> Bool
-- myElem' = myAny . (==)

-- myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f xs = go xs False
--   where go _ True = True
--         go [] s = s
--         go (a:as) _ = go as $! f a
