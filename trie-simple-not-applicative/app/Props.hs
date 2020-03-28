{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}
module Props(
  T,
  propFunctor,
  propApplicativeLeftU,
  propApplicativeRightU,
  propApplicativeAssoc
) where

import           Control.Monad
import           Control.Applicative(liftA2)
import           Test.QuickCheck (Arbitrary, Property, (===))

data A
data B
data C

newtype T p a = T a
  deriving (Show, Eq, Ord, Arbitrary) via (a)

propFunctor
  :: forall f. (Functor f,
                forall a. Eq a => Eq (f a),
                forall a. Show a => Show (f a))
  => f Int -> Property
propFunctor fa = fmap id fa === fa

propApplicativeLeftU
  :: forall f. (Applicative f,
                forall a. Eq a => Eq (f a),
                forall a. Show a => Show (f a))
  => T A Int -> f (T B Int) -> Property
propApplicativeLeftU a fb = liftA2 (const id) (pure a) fb === fb

propApplicativeRightU
  :: forall f. (Applicative f,
                forall a. Eq a => Eq (f a),
                forall a. Show a => Show (f a))
  => f (T A Int) -> T B Int -> Property
propApplicativeRightU fa b = liftA2 const fa (pure b) === fa

propApplicativeAssoc
  :: forall f. (Applicative f,
                forall a. Eq a => Eq (f a),
                forall a. Show a => Show (f a))
  => f (T A Int) -> f (T B Int) -> f (T C Int) -> Property
propApplicativeAssoc fa fb fc =
  fa `pair` (fb `pair` fc) === fmap assoc ((fa `pair` fb) `pair` fc)
  where pair = liftA2 (,)
        assoc ((a,b),c) = (a,(b,c))
