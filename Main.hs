{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

class MonadResource m where
  getInt' ::  m Int

program2 :: (E.MonadError String m, MonadResource m) => m Int
program2 = getInt' `E.catchError` const (pure 1)

newtype ResourceT m a = ResourceT { runResourceT :: m a }
  deriving (Functor, Applicative, Monad)

deriving instance E.MonadError e m => E.MonadError e (ResourceT m)

instance E.MonadError String m => MonadResource (ResourceT m) where
  getInt' = ResourceT $ E.throwError "foo"

result2 :: Either String Int
result2 = runResourceT program2
{- Right 1 -}

main :: IO ()
main = undefined
