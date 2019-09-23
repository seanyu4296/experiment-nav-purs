module ExpN.ReactNavigation.Flow where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free as Free
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..))
import Data.Generic.Rep as G
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log, warn)
import Foreign (Foreign, unsafeToForeign)
import React.Basic (Component, JSX, ReactComponent, createComponent, makeStateless, toReactComponent)
import React.Basic.Events (handler_)
import React.Basic.Native as RN
import Unsafe.Coerce (unsafeCoerce)

foreign import kind NavType

foreign import data ROOT :: NavType

foreign import data SWITCH :: NavType

foreign import data STACK :: NavType

foreign import data BOTTOM_TAB :: NavType

foreign import data Navigation :: Type -> Type

foreign import dispatch :: forall o. Navigation o -> Foreign -> Effect Unit

-- dispatch :: forall o. Navigation o -> Foreign -> Effect Unit
-- dispatch nav thing = (unsafeCoerce nav).dispatch thing
foreign import getAction :: forall route hint o a. Navigation o -> (a -> Flow route hint o)

-- Add HINT
data FlowM hint (route :: Type -> Type) a
  = NavPush (route a) hint
  | NavGoBack
  | NavEff (Effect a)
  | NavAff (Aff a)

derive instance functorFlowM :: Functor route => Functor (FlowM hint route)

newtype Flow hint (route :: Type -> Type) a
  = Flow (Free (FlowM hint route) a)

derive instance newtypeFlow :: Newtype (Flow hint route a) _

derive newtype instance functorFlow :: Functor (Flow route hint)

derive newtype instance applyFlow :: Apply (Flow hint route)

derive newtype instance bindFlow :: Bind (Flow hint route)

derive newtype instance applicativeFlow :: Applicative (Flow hint route)

derive newtype instance monadFlow :: Monad (Flow hint route)

liftFFlow :: forall hint route a. FlowM hint route a -> Flow hint route a
liftFFlow = Flow <<< liftF

class
  Monad (n hint route) <= MonadNav n (hint :: Type) (route :: Type -> Type) where
  navPush :: forall a. route a -> hint -> n hint route a

instance flowMonadNav :: MonadNav Flow hint route where
  navPush r h = do
    value <- liftFFlow $ NavPush r h
    pure $ value

instance flowMonadEffect :: MonadEffect (Flow hint route) where
  liftEffect eff = liftFFlow <<< NavEff $ eff

instance flowMonadAff :: MonadAff (Flow hint route) where
  liftAff aff = liftFFlow <<< NavAff $ aff

type FlowNav (navType :: NavType) (route :: Type -> Type) initialOut out hint
  = { fromTheStart :: MonadNav Flow hint route => initialOut -> Flow hint route out
    , notFromTheStart :: MonadNav Flow hint route => hint -> Flow hint route out
    , initialRoute :: route initialOut
    }

newtype FlowScreen (route :: Type -> Type) (hint :: Type) o
  = FlowScreen (ReactComponent { navigation :: Navigation o })

-- Implementation
-- dont i need to unwrap this
class ToPushAction r hint where
  toPushAction :: forall a. r a -> hint -> PushAction a

class GenericToPushAction g a | g -> a where
  genToPushAction :: g -> PushAction a

instance genToPushActionSum ::
  ( GenericToPushAction l a
  , GenericToPushAction r a
  ) =>
  GenericToPushAction (Sum l r) a where
  genToPushAction (Inl l) = genToPushAction l
  genToPushAction (Inr r) = genToPushAction r

instance genToPushActionNoInfo ::
  IsSymbol routeName =>
  GenericToPushAction (Constructor routeName (Argument (i -> a))) a where
  genToPushAction (Constructor (Argument f)) =
    { type: "Navigation/PUSH"
    , routeName: reflectSymbol (SProxy :: SProxy routeName)
    , params:
      { action: unsafeCoerce f
      , hint: "entershortcode"
      }
    }

instance genToPushActionWithInfo ::
  IsSymbol routeName =>
  GenericToPushAction (Constructor routeName (Product (Argument info) (Argument (i -> a)))) a where
  genToPushAction (Constructor (Product (Argument information) (Argument f))) =
    { type: "Navigation/PUSH"
    , routeName: reflectSymbol (SProxy :: SProxy routeName)
    , params:
      { action: unsafeCoerce f
      --      , info: information
      , hint: "TODO"
      }
    }

-- I dont need the "r" in the GenericToPushAction because it is already in the Generic in the form of g
genericToPushAction ::
  forall r a g.
  Generic (r a) g =>
  GenericToPushAction g a =>
  r a ->
  PushAction a
genericToPushAction = genToPushAction <<< G.from

type PushAction a
  = { type :: String
    , routeName :: String
    , params ::
      { action :: Foreign -> a
      , hint :: String
      --      , info :: Maybe i
      }
    }

resumeFlow ::
  forall route hint o.
  Functor route =>
  ToPushAction route hint =>
  MonadNav Flow hint route =>
  Navigation o ->
  Flow hint route o ->
  Effect Unit
resumeFlow nav flow = loop flow
  where
  loop f = case Free.resume (unwrap f) of
    Left (NavPush r h) -> dispatch nav $ unsafeToForeign (toPushAction r h)
    Left (NavAff aff) ->
      runAff_
        ( case _ of
            Left err -> warn "AFF ERR HANDLING"
            Right res -> loop (Flow res)
        )
        aff
    Left (NavEff eff) -> eff >>= loop <<< Flow
    Left (NavGoBack) -> dispatch nav $ unsafeToForeign { type: "Navigation/BACK" }
    Right _ -> log "DONE"

type PageProps a
  = { submit :: a -> Effect Unit
    }

withPagePropsC :: forall a. Component a
withPagePropsC = createComponent "FlowScreen"

withPageProps ::
  forall a o hint route.
  Functor route =>
  ToPushAction route hint =>
  (PageProps a -> JSX) -> FlowScreen route hint o
withPageProps comp = FlowScreen $ toReactComponent identity withPagePropsC { render }
  where
  render self =
    let
      -- get the action
      action :: a -> Flow hint route o
      action = trace (getAction self.props.navigation) (const $ getAction self.props.navigation)
    in
      comp
        { submit: \a -> resumeFlow self.props.navigation (action a)
        }
