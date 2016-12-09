{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TestDouble where

import Frames
import Frames.CSV
import Data.Vinyl
import Control.Monad.IO.Class
import Control.Arrow (first, second)
import Frames.ColumnTypeable
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import qualified Data.Text.IO as T


declareColumn "col_c" ''Double
tableTypes' rowGen "test.csv"

  -- type Row =
  -- Record '["col_a" :-> Bool, "col_b" :-> Int, "col_c" :-> Double]
  -- 	-- Defined at C:\Users\cody\Frames\src\Frames\TestDouble.hs:8:1

-- testReadRow :: Rec Maybe '["col_a" :-> Bool, "col_b" :-> Int, "col_c" :-> Double]
-- testReadRow = readRow defaultParser "1,2,\"1,300,400.10\""

-- -- bug (??? is it, because it is just marked as nothing) in readRow when a column is blank and contains a space
-- bugReadRow :: Rec Maybe '["col_a" :-> Int, "col_b" :-> Int, "col_c" :-> Double]
-- bugReadRow = readRow defaultParser "1, ,\"1,300,400.10\""


bugTokenizeRow = tokenizeRow defaultParser  "1, ,\"1,300,400.10\""
-- outputs: ["1","","1,300,400.10"]

-- go deeper into callstack
-- λ> readRec (tokenizeRow defaultParser "1, ,\"1,300,400.10\"") :: Rec Maybe  '["col_a" :-> Bool, "col_b" :-> Int, "col_c" :-> Double]
-- {Just col_a :-> True, Nothing, Just col_c :-> 1300400.1}
-- above col_a was wrongfully a Bool... based on our current data below that was wrong:
{-
col_a,col_b,col_c
1,9,"1,000,000.00"
2,8,"300,000.00"
3,7,"0"
4,6,0
-}
-- λ> readRec (tokenizeRow defaultParser "4,6,0") :: Rec Maybe  '["col_a" :-> Int, "col_b" :-> Int, "col_c" :-> Double]
-- {Just col_a :-> 4, Just col_b :-> 6, Just col_c :-> 0.0}
-- λ> readRec (tokenizeRow defaultParser "4,6,\"0\"") :: Rec Maybe  '["col_a" :-> Int, "col_b" :-> Int, "col_c" :-> Double]
-- {Just col_a :-> 4, Just col_b :-> 6, Just col_c :-> 0.0}
-- λ> readRec (tokenizeRow defaultParser "4,6,\"300,000,000\"") :: Rec Maybe  '["col_a" :-> Int, "col_b" :-> Int, "col_c" :-> Double]
-- {Just col_a :-> 4, Just col_b :-> 6, Just col_c :-> 3.0e8}
-- λ> readRec (tokenizeRow defaultParser "4,6,\"300,000.00\"") :: Rec Maybe  '["col_a" :-> Int, "col_b" :-> Int, "col_c" :-> Double]
-- {Just col_a :-> 4, Just col_b :-> 6, Just col_c :-> 300000.0}
-- testReadColheaders :: forall a. (ColumnTypeable a, Monoid a)=> IO (Q Type)
-- testReadColheaders = do
--   colInfo <- readColHeaders defaultParser "test.csv" :: forall a. (ColumnTypeable a, Monoid a) => IO [(T.Text, a)]
--   let colInfo' = colInfo :: [(T.Text, Q Type)]
--   pure $ recDec' colInfo'
--   -- where recDec' = recDec . map (second colType) :: forall b. ColumnTypeable b => [(Text, b)] -> Q Type
--   where recDec' cols = recDec (f cols)
--         -- f = map (second colType) :: forall b. ColumnTypeable b => [(Text, b)] -> Q Type
--         f cols = map (second colType) cols
  -- where recDec' = recDec . map (second colType) :: (ColumnTypeable a => ([(T.Text, a)] -> Q Type))
  -- cols <- readColHeaders defaultParser "test.csv" :: (Monoid a, ColumnTypeable a) => IO [(T.Text, a)]
  -- -- >>= (recDec . map (second colType) :: ColumnTypeable a=>[(T.Text, a)] -> IO (Q Type))
  -- pure $ map (second colType) (cols :: Monoid a => [(T.Text, a)])

-- try testing inference on a single row
f :: forall s. (ColumnTypeable s) => s -> _
f s = map inferType . tokenizeRow defaultParser $ s

-- C:\Users\cody\Frames\src\Frames\TestDouble.hs:77:1: Could not deduce (ColumnTypeable b) …
--     from the context (ColumnTypeable s)
--       bound by the inferred type for `f': ColumnTypeable s => s -> [b]
--       at C:\Users\cody\Frames\src\Frames\TestDouble.hs:77:1-51
--     When checking that `f' has the specified type
--       f :: forall s b. ColumnTypeable s => s -> [b]
--     Probable cause: the inferred type is ambiguous
-- C:\Users\cody\Frames\src\Frames\TestDouble.hs:77:51: Couldn't match expected type `Text' with actual type `s' …
--       `s' is a rigid type variable bound by
--           the inferred type of f :: ColumnTypeable b => s -> [b]
--           at C:\Users\cody\Frames\src\Frames\TestDouble.hs:76:13
--     Relevant bindings include
--       s :: s
--         (bound at C:\Users\cody\Frames\src\Frames\TestDouble.hs:77:3)
--       f :: s -> [b]
--         (bound at C:\Users\cody\Frames\src\Frames\TestDouble.hs:77:1)
--     In the second argument of `($)', namely `s'
--     In the expression: map inferType . tokenizeRow defaultParser $ s

-- -- f :: MonadIO m => P.Producer (Rec Maybe '["col_a" :-> Bool, "col_b" :-> Int, "col_c" :-> Double]) m ()
-- f :: MonadIO m => P.Producer (Rec Maybe '["col_a" :-> Bool, "col_b" :-> Int, "col_c" :-> Double]) m ()
-- f = readTableMaybe "test.csv"
