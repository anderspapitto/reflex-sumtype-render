{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module ReflexHelpers where

import Control.Monad.Identity
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Maybe (isJust)
import Data.Proxy
import Data.Type.Equality

import Generics.SOP
import qualified GHC.Generics as GHC

import Reflex hiding (HList)
import Reflex.Dom hiding (HList)

-- begin library code

data Tup2List :: * -> [*] -> * where
  Tup0 :: Tup2List () '[]
  Tup1 :: Tup2List x '[ x ]
  TupS :: Tup2List r (x ': xs) -> Tup2List (a, r) (a ': x ': xs)

instance GEq (Tup2List t) where
  geq Tup0     Tup0     = Just Refl
  geq Tup1     Tup1     = Just Refl
  geq (TupS x) (TupS y) = undefined

newtype GTag t i = GTag { unTag :: NS (Tup2List i) (Code t) }

instance GEq (GTag t) where
  geq (GTag (Z x)) (GTag (Z y)) = undefined -- x `geq` y

type GTagVal t = DSum (GTag t) I

toDSum :: Generic a => a -> DSum (GTag a) I
toDSum = fromNPI (\t x -> GTag t :=> I x) . unSOP . from
  where
    fromNPI
      :: (forall i . NS (Tup2List i) xss -> i -> r)
      -> NS (NP I) xss
      -> r
    fromNPI k = \case
      (Z x) -> conFromNPI  (k . Z) x
      (S q) -> fromNPI (k . S) q

    conFromNPI
      :: (forall a. Tup2List a xs -> a -> r)
      -> NP I xs
      -> r
    conFromNPI k = \case
      Nil            -> k Tup0 ()
      I x :* Nil     -> k Tup1 x
      I x :* y :* ys -> conFromNPI (\t v -> k (TupS t) (x, v)) (y :* ys)

fromDSum :: Generic a => DSum (GTag a) I -> a
fromDSum (GTag t :=> I x) = to . SOP . hmap (toNPI x) $ t
  where
    toNPI :: a -> Tup2List a xs -> NP I xs
    toNPI x = \case
      Tup0   ->                           Nil
      Tup1   ->                    I x :* Nil
      TupS t -> let (y, ys) = x in I y :* toNPI ys t

data DynamicWrapper t m a = DynamicWrapper (m (Dynamic t a))

renderSumType
  :: forall t m u. (MonadWidget t m, Generic u)
  => (forall b. GTag u b -> Dynamic t b -> m ())
  -> Dynamic t u
  -> m (Event t ())
renderSumType renderAnything =
  dyn . fmap toAction . uniqDynBy sameConstructor . toNestedDyn . fmap toDSum
  where
    toAction :: DSum (GTag u) (DynamicWrapper t m) -> m ()
    toAction (t :=> DynamicWrapper x) = x >>= renderAnything t

    sameConstructor :: DSum (GTag u) a -> DSum (GTag u) a -> Bool
    sameConstructor (t1 :=> _) (t2 :=> _) = isJust (t1 `geq` t2)

    toNestedDyn
      :: Dynamic t (DSum (GTag u) I)
      -> Dynamic t (DSum (GTag u) (DynamicWrapper t m))
    toNestedDyn d = makeNestedDyn <$> d
      where
        makeNestedDyn :: DSum (GTag u) I -> DSum (GTag u) (DynamicWrapper t m)
        makeNestedDyn (t :=> I x) =
          t :=> DynamicWrapper (holdDyn x (eventsForTag t d))

    eventsForTag
      :: GTag u a
      -> Dynamic t (DSum (GTag u) I)
      -> Event t a
    eventsForTag tag = fmapMaybe tagToJust . updated
      where tagToJust (t :=> I x) = case t `geq` tag of
              Just Refl -> Just x
              Nothing   -> Nothing

-- BEGIN example user-provided section

data MyState2
data MyState3a
data MyState3b

data UsersSumType
  = First
  | Second MyState2
  | Third MyState3a MyState3b
  deriving GHC.Generic

instance Generic UsersSumType

renderMyState1 :: Dynamic t () -> m ()
renderMyState1 = undefined

renderMyState2 :: Dynamic t MyState2 -> m ()
renderMyState2 = undefined

renderMyState3 :: Dynamic t (MyState3a, MyState3b) -> m ()
renderMyState3 = undefined

renderUsersSumType
  :: MonadWidget t m
  => GTag UsersSumType a
  -> Dynamic t a
  -> m ()
renderUsersSumType = \case
  GTag       (Z       Tup0)    -> renderMyState1
  GTag    (S (Z       Tup1))   -> renderMyState2
  GTag (S (S (Z (TupS Tup1)))) -> renderMyState3
