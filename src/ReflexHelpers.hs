{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ReflexHelpers where

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Maybe (isJust)
import Data.Type.Equality

import Generics.SOP
import qualified GHC.Generics as GHC

import Reflex hiding (HList)
import Reflex.Dom hiding (HList)

-- BEGIN library code

data Quux i xs where Quux :: Quux (NP I xs) xs

newtype GTag t i = GTag { unTag :: NS (Quux i) (Code t) }

instance GEq (GTag t) where
  -- I don't know how to do this
  geq (GTag (S x)) (GTag (S y)) = undefined

toDSum :: Generic a => a -> DSum (GTag a) I
toDSum = fromNPI (\t x -> GTag t :=> I x) . unSOP . from
  where
    fromNPI
      :: (forall i . NS (Quux i) xss -> i -> r)
      -> NS (NP I) xss
      -> r
    fromNPI k = \case
      (Z x) -> conFromNPI (k . Z) x
      (S q) ->    fromNPI (k . S) q

    conFromNPI
      :: (forall a. Quux a xs -> a -> r)
      -> NP I xs
      -> r
    conFromNPI k = \case
      Nil        -> k Quux Nil
      I x :* Nil -> k Quux (I x :* Nil)
      I x :* xs  -> conFromNPI (\Quux _ -> k Quux (I x :* xs)) xs

data DynamicWrapper t m a = DynamicWrapper (m (Dynamic t a))

renderSumType
  :: forall t m u r. (MonadWidget t m, Generic u)
  => (forall b. GTag u b -> Dynamic t b -> m (Event t r))
  -> Dynamic t u
  -> m (Event t r)
renderSumType renderAnything dynState = do
  switchPromptly never =<<
    ( dyn
      . fmap toAction
      . uniqDynBy sameConstructor
      . toNestedDyn
      . fmap toDSum
    ) dynState
  where
    toAction :: DSum (GTag u) (DynamicWrapper t m) -> m (Event t r)
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
      where tagToJust (t :=> I x) = (\Refl -> x) <$> t `geq` tag

-- BEGIN example user-provided section

data MyState2
data MyState3a
data MyState3b
data MyState3c

data UsersEventType

data UsersSumType
  = First
  | Second MyState2
  | Third MyState3a MyState3b MyState3c
  deriving GHC.Generic

instance Generic UsersSumType

renderMyState1
  :: forall t m. MonadWidget t m
  => Dynamic t (NP I '[])
  -> m (Event t UsersEventType)
renderMyState1 d = undefined

renderMyState2
  :: forall t m. MonadWidget t m
  => Dynamic t (NP I '[MyState2])
  -> m (Event t UsersEventType)
renderMyState2 d =
  let dynMyState2 = ((\(I x :* Nil) -> x) <$> d) :: Dynamic t MyState2
  in undefined

renderMyState3
  :: forall t m. MonadWidget t m
  => Dynamic t (NP I '[MyState3a, MyState3b, MyState3c])
  -> m (Event t UsersEventType)
renderMyState3 d =
  let dynA = ((\(I a :* I b :* I c :* Nil) -> a) <$> d) :: Dynamic t MyState3a
      dynB = ((\(I a :* I b :* I c :* Nil) -> b) <$> d) :: Dynamic t MyState3b
      dynC = ((\(I a :* I b :* I c :* Nil) -> c) <$> d) :: Dynamic t MyState3c
  in undefined

renderUsersSumType
  :: MonadWidget t m
  => GTag UsersSumType a
  -> Dynamic t a
  -> m (Event t UsersEventType)
renderUsersSumType = \case
  GTag       (Z Quux)   -> renderMyState1
  GTag    (S (Z Quux))  -> renderMyState2
  GTag (S (S (Z Quux))) -> renderMyState3

dynState :: Dynamic t UsersSumType
dynState = undefined

render :: MonadWidget t m => m (Event t UsersEventType)
render = renderSumType renderUsersSumType dynState
