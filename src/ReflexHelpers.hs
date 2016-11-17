module ReflexHelpers where

import Control.Monad.Identity
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Maybe (isJust)
import Data.Type.Equality

import Reflex
import Reflex.Dom

-- BEGIN user-provided section

data MyState1
data MyState2
data MyState3

data UsersSumType = First MyState1 | Second MyState2 | Third MyState3

renderMyState1 :: Dynamic t MyState1 -> m ()
renderMyState1 = undefined

renderMyState2 :: Dynamic t MyState2 -> m ()
renderMyState2 = undefined

renderMyState3 :: Dynamic t MyState3 -> m ()
renderMyState3 = undefined

-- this section must also be provided by the user, but ideally could
-- be generated automatically

data Tag a where
  ALeft  :: Tag MyState1
  ARight :: Tag MyState2
  AThird :: Tag MyState3

convertToDSum :: UsersSumType -> DSum Tag Identity
convertToDSum (First  x) = ALeft  :=> Identity x
convertToDSum (Second x) = ARight :=> Identity x
convertToDSum (Third  x) = AThird :=> Identity x

instance GEq Tag where
  geq ALeft  ALeft  = Just Refl
  geq ARight ARight = Just Refl
  geq AThird AThird = Just Refl
  geq _      _      = Nothing

renderAnything
  :: MonadWidget t m
  => Tag a
  -> Dynamic t a
  -> m ()
renderAnything t = case t of
  ALeft  -> renderMyState1
  ARight -> renderMyState2
  AThird -> renderMyState3

-- END user-provided section

data RenderFuncWrapper t m a = RenderFuncWrapper (Dynamic t a -> m ())
data    DynamicWrapper t m a = DynamicWrapper (m (Dynamic t a))

renderSumType
  :: forall t m. MonadWidget t m
  => Dynamic t (DSum Tag Identity)
  -> m (Event t ())
renderSumType = dyn . fmap toAction . uniqDynBy sameConstructor . toNestedDyn
  where
    toAction :: DSum Tag (DynamicWrapper t m) -> m ()
    toAction (t :=> DynamicWrapper x) = x >>= renderAnything t

    sameConstructor (t1 :=> _) (t2 :=> _) = isJust $ t1 `geq` t2

    toNestedDyn
      :: Dynamic t (DSum Tag Identity)
      -> Dynamic t (DSum Tag (DynamicWrapper t m))
    toNestedDyn d = makeNestedDyn <$> d
      where makeNestedDyn (t :=> Identity x) =
              t :=> DynamicWrapper (holdDyn x (eventsForTag d t))

    eventsForTag
      :: Dynamic t (DSum Tag Identity)
      -> Tag a
      -> Event t a
    eventsForTag d tag = fmapMaybe tagToJust . updated $ d
      where tagToJust (t :=> Identity x) = case t `geq` tag of
              Just Refl -> Just x
              Nothing   -> Nothing
