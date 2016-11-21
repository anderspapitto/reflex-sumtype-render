{-# LANGUAGE Haskell2010
, DataKinds
, DeriveGeneric
, GADTs
, KindSignatures
, LambdaCase
, RankNTypes
, ScopedTypeVariables
, TypeOperators
#-}

module ReflexHelpers where

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Maybe (isJust)
import Generics.SOP
import qualified GHC.Generics as GHC

import Reflex hiding (tag)
import Reflex.Dom hiding (tag)

-- BEGIN library code

type GTag t = GTag_ (Code t)
newtype GTag_ t (as :: [*]) = GTag (NS ((:~:) as) t)

instance GEq (GTag_ t) where
  geq (GTag (Z Refl)) (GTag (Z Refl)) = Just Refl
  geq (GTag (S x))    (GTag (S y))    = GTag x `geq` GTag y
  geq _               _               = Nothing

toDSum :: forall t . Generic t => t -> DSum (GTag t) (NP I)
toDSum = foo . unSOP . from
  where
    foo :: ()
        => NS (NP I) (Code t)
        -> DSum (GTag t) (NP I)
    foo = undefined

data DynamicWrapper t m a = DynamicWrapper (m (Dynamic t (NP I a)))

renderSumType
  :: forall t m u r. (MonadWidget t m, Generic u)
  => (forall b. GTag u b -> Dynamic t (NP I b) -> m (Event t r))
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
      :: Dynamic t (DSum (GTag u) (NP I))
      -> Dynamic t (DSum (GTag u) (DynamicWrapper t m))
    toNestedDyn d = makeNestedDyn <$> d
      where
        makeNestedDyn :: DSum (GTag u) (NP I) -> DSum (GTag u) (DynamicWrapper t m)
        makeNestedDyn (t :=> x) =
          t :=> DynamicWrapper (holdDyn x (eventsForTag t d))

    eventsForTag
      :: GTag u a
      -> Dynamic t (DSum (GTag u) (NP I))
      -> Event t (NP I a)
    eventsForTag tag = fmapMaybe tagToJust . updated
      where tagToJust (t :=> x) = (\Refl -> x) <$> t `geq` tag

-- BEGIN example user-provided section

data MyState2
data MyState3a
data MyState3b
data MyState3c

data UsersSumType
  = First
  | Second MyState2
  | Third MyState3a MyState3b MyState3c
  deriving GHC.Generic

instance Generic UsersSumType

data UsersEventType

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
  let dynState = ((\(I x :* Nil) -> x) <$> d)
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
  -> Dynamic t (NP I a)
  -> m (Event t UsersEventType)
renderUsersSumType = \case
  GTag       (Z Refl)   -> renderMyState1
  GTag    (S (Z Refl))  -> renderMyState2
  GTag (S (S (Z Refl))) -> renderMyState3
  _                     -> error "I wish I could tell GHC this is impossible"

g_dynState :: Dynamic t UsersSumType
g_dynState = undefined

render :: MonadWidget t m => m (Event t UsersEventType)
render = renderSumType renderUsersSumType g_dynState

{-# Potential improvements

- use `fan` to improve efficiency
- completely remove the need for the end user to use generics-sop types
- break out a 'core' which is pure Reflex, and a Reflex-Dom shell

#-}
