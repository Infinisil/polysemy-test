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

import qualified Control.Monad.Freer.Error as FE
import qualified Control.Monad.Freer as F

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

{- ######### Free ########### -}

data ResourceF a where
  GetInt' :: ResourceF Int

getInt'' :: F.Member ResourceF effs => F.Eff effs Int
getInt'' = F.send GetInt'

program3 :: (F.Member ResourceF effs, F.Member (FE.Error String) effs) => F.Eff effs Int
program3 = getInt'' `FE.catchError` \(_ :: String) -> pure 1

runErroringInt' :: F.Member (FE.Error String) effs => F.Eff (ResourceF ': effs) a -> F.Eff effs a
runErroringInt' = F.interpret $ \GetInt' -> FE.throwError "bar"

result3 :: Either String Int
result3 = F.run $ FE.runError $ runErroringInt' program3
{- Left "bar" -}

main :: IO ()
main = undefined
