{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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

-- BEGIN user-provided section

data MyState1
data MyState2
data MyState3

data UsersSumType = First MyState1 | Second MyState2 | Third MyState3 deriving GHC.Generic

instance Generic UsersSumType

data Tup2List :: * -> [*] -> * where
  Tup0 :: Tup2List () '[]
  Tup1 :: Tup2List x '[ x ]
  TupS :: Tup2List r (x ': xs) -> Tup2List (a, r) (a ': x ': xs)

newtype GTag t i = GTag { unTag :: NS (Tup2List i) (Code t) }

instance GEq (GTag t) where
  geq (GTag (Z Tup1)) (GTag (Z Tup1)) = Just Refl

renderMyState1 :: Dynamic t MyState1 -> m ()
renderMyState1 = undefined

renderMyState2 :: Dynamic t MyState2 -> m ()
renderMyState2 = undefined

renderMyState3 :: Dynamic t MyState3 -> m ()
renderMyState3 = undefined

-- this section must also be provided by the user, but ideally could
-- be generated automatically

convertToDSum :: UsersSumType -> DSum (GTag UsersSumType) I
convertToDSum (First  x) = (GTag       (Z Tup1))   :=> I x
convertToDSum (Second x) = (GTag    (S (Z Tup1)))  :=> I x
convertToDSum (Third  x) = (GTag (S (S (Z Tup1)))) :=> I x

-- renderAnything
--   :: MonadWidget t m
--   => (GTag UsersSumType) a
--   -> Dynamic t a
--   -> m ()
renderAnything t = undefined

-- END user-provided section

data RenderFuncWrapper t m a = RenderFuncWrapper (Dynamic t a -> m ())
data    DynamicWrapper t m a = DynamicWrapper (m (Dynamic t a))

foo :: forall t m u a. MonadWidget t m => () -> ()
foo = id
  where
    eventsForTag
      :: forall t m u a. MonadWidget t m
      => Proxy m
      -> Dynamic t (DSum (GTag u) I)
      -> GTag u a
      -> Event t a
    eventsForTag _ d tag = fmapMaybe tagToJust . updated $ d
      where tagToJust (t :=> I x) = case t `geq` tag of
              Just Refl -> Just x
              Nothing   -> Nothing

    toNestedDyn
      :: forall t m u. MonadWidget t m
      => Dynamic t (DSum (GTag u) I)
      -> Dynamic t (DSum (GTag u) (DynamicWrapper t m))
    toNestedDyn d = makeNestedDyn <$> d
      where
        makeNestedDyn :: DSum (GTag u) I -> DSum (GTag u) (DynamicWrapper t m)
        makeNestedDyn (t :=> I x) =
          t :=> DynamicWrapper (holdDyn x (eventsForTag (Proxy :: Proxy m) d t))

-- renderSumType
--   :: forall t m. MonadWidget t m
--   => Dynamic t (DSum (GTag u) (NP I))
--   -> m (Event t ())
-- renderSumType = dyn . fmap toAction . uniqDynBy sameConstructor . toNestedDyn
--   where
--     toAction :: DSum (GTag u) (NP (DynamicWrapper t m)) -> m ()
--     toAction (t :=> DynamicWrapper x) = x >>= renderAnything t
--
--     sameConstructor (t1 :=> _) (t2 :=> _) = isJust $ t1 `geq` t2
--
--     toNestedDyn
--       :: Dynamic t (DSum (GTag u) (NP I))
--       -> Dynamic t (DSum (GTag u) (NP (DynamicWrapper t m)))
--     toNestedDyn d = makeNestedDyn <$> d
--       where makeNestedDyn (t :=> x) =
--               t :=> DynamicWrapper (holdDyn x (eventsForTag d t))
--
--     eventsForTag
--       :: Dynamic t (DSum (GTag u) (NP I))
--       -> (GTag u) (NS I a)
--       -> Event t a
--     eventsForTag d tag = fmapMaybe tagToJust . updated $ d
--       where tagToJust (t :=> x) = case t `geq` tag of
--               Just Refl -> Just x
--               Nothing   -> Nothing
