{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Polysemy
import           Polysemy.Error

import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R

{- ############# polysemy ################ -}

data Resource (m :: * -> *) a where
  GetInt :: Resource m Int

makeSem ''Resource

program :: Members '[Resource, Error String] r => Sem r Int
program = getInt `catch` const (pure 1)

runErroringInt :: Member (Error String) r => Sem (Resource ': r) a -> Sem r a
runErroringInt = interpret \case
  GetInt -> throw "error"

result :: Either String Int
result = run $ runError $ runErroringInt $ program
{- Right 1 -}

{- ######### MTL ########### -}

program2 :: (E.MonadError String m, R.MonadReader Int m) => m Int
program2 = do
  R.ask `E.catchError` const (pure 1)

result2 :: Either String Int
result2 = R.runReaderT program2 (error "error")
{- Right *** Exception: error -}

main :: IO ()
main = undefined
