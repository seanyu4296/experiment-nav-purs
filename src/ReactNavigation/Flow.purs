module ExpN.ReactNavigation.Flow where

import Prelude
import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free as Free
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..))
import Data.Generic.Rep as G
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log, warn)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import React.Basic (Component, JSX, ReactComponent, createComponent, makeStateless, toReactComponent)
import React.Basic.Events (handler_)
import React.Basic.Native as RN
import Record (get)
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import kind NavType

foreign import data ROOT :: NavType

foreign import data SWITCH :: NavType

foreign import data STACK :: NavType

foreign import data BOTTOM_TAB :: NavType

foreign import data Navigation :: Type -> Type

foreign import data JSNavPushActionType :: Type

jsNavPushAction :: JSNavPushActionType
jsNavPushAction = unsafeCoerce "Navigation/PUSH"

foreign import dispatch :: forall o. Navigation o -> Foreign -> Effect Unit

-- dispatch :: forall o. Navigation o -> Foreign -> Effect Unit
-- dispatch nav thing = (unsafeCoerce nav).dispatch thing
foreign import getAction :: forall route hint o a. Navigation o -> (a -> Flow route hint o)

-- Add HINT
data FlowM hint (route :: Type -> Type) a
  = NavPush (route a) hint
  | NavPushH (route a) hint
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
  toPushAction :: forall a. r a -> hint -> JSNavPushAction a

class GenToPushPartial g a | g -> a where
  genToPushPartial :: g -> NavPushPartial a

instance genToPushPartialSum ::
  ( GenToPushPartial l a
  , GenToPushPartial r a
  ) =>
  GenToPushPartial (Sum l r) a where
  genToPushPartial (Inl l) = genToPushPartial l
  genToPushPartial (Inr r) = genToPushPartial r

instance genToPushPartialNoInfo ::
  IsSymbol routeName =>
  GenToPushPartial (Constructor routeName (Argument (i -> a))) a where
  genToPushPartial (Constructor (Argument f)) =
    { routeName: reflectSymbol (SProxy :: SProxy routeName)
    , action: unsafeCoerce f -- a -> Flow
    }

instance genToPushPartialWithInfo ::
  IsSymbol routeName =>
  GenToPushPartial (Constructor routeName (Product (Argument info) (Argument (i -> a)))) a where
  genToPushPartial (Constructor (Product (Argument information) (Argument f))) =
    { routeName: reflectSymbol (SProxy :: SProxy routeName)
    , action: unsafeCoerce f
    }

-- I dont need the "r" in the GenericToPushAction because it is already in the Generic in the form of g
genericToPushAction ::
  forall r a g hint hg.
  Generic (r a) g =>
  Generic hint hg =>
  GenHint hg =>
  GenToPushPartial g a =>
  r a ->
  hint ->
  JSNavPushAction a
genericToPushAction route hint =
  { routeName
  , type: jsNavPushAction
  , params:
    { action
    , hint: hintString
    }
  }
  where
  hintString = toPath <<< G.from $ hint

  pp@{ routeName, action } = genToPushPartial <<< G.from $ route

type NavPushPartial a
  = { routeName :: String
    , action :: Foreign -> a
    }

type NavPushHint
  = { path :: String, query :: Object String }

class GenHint a where
  toPath :: a -> NavPushHint

instance genHintSum ::
  ( GenHint l
  , GenHint r
  ) =>
  GenHint (Sum l r) where
  toPath (Inl l) = toPath l
  toPath (Inr r) = toPath r

instance genHintConstructorArgR ::
  ( IsSymbol hintName
  , Row.Lacks "key" r
  , RowToList r rl
  , ToQuery rl r
  ) =>
  GenHint (Constructor hintName (Argument { | r })) where
  toPath (Constructor (Argument val)) = { path, query }
    where
    path = reflectSymbol (SProxy :: SProxy hintName)

    query = toQuery (RLProxy :: RLProxy rl) val

class ToQuery (rl :: RowList) (r :: #Type) where
  toQuery :: RLProxy rl -> { | r } -> Object String

instance toQueryNil :: ToQuery Nil r where
  toQuery _ _ = FO.empty

instance toQueryCons ::
  ( IsSymbol name
  , ToQueryString typ
  , Row.Cons name typ etc r
  , ToQuery tailrl r
  ) =>
  ToQuery (Cons name typ tailrl) r where
  toQuery _ r = FO.insert key value tail
    where
    sp = SProxy :: SProxy name

    key = reflectSymbol sp

    value = toQueryString $ get sp r

    tail = toQuery (RLProxy :: RLProxy tailrl) r

class ToQueryString a where
  toQueryString :: a -> String

instance toQueryStringString :: ToQueryString String where
  toQueryString = identity

instance toQueryStringBoolean :: ToQueryString Boolean where
  toQueryString true = "true"
  toQueryString false = "false"

instance toQueryStringInt :: ToQueryString Int where
  toQueryString i = show i

instance toQueryStringNumber :: ToQueryString Number where
  toQueryString n = show n

-- Hints should be only with name and a flat record maximum usage of Primitive such as (Maybe, Boolean, Int, String)
-- Hints should be with a name and an argument that is a record with key not being used
-- hint -> String
-- String -> Maybe hint (if this fails use the fromTheStart if not use notFromTheStart)
-- TODO: create generic hint thingy
-- TODO: create a function for translating string -> maybe hint
-- if Just add new state
-- There is a translator for Just hint -> (hint -> Flow ...) -> NavigationPartialState
-- maybe it makes sense there is no aff support but loadAuthToken sa top
-- if first screen is Aff -> just do loading?? where to run the aff?
-- the aff runs on the initialRoute so to achieve this create something like
-- : add some state to the initialRoute that needs to run didMount then setState new cb
-- : modify withpageProps
type JSNavPushAction a
  = { routeName :: String
    -- TODO: this can be a better type
    , type :: JSNavPushActionType
    , params ::
      { action :: Foreign -> a
      , hint :: NavPushHint
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
    Left (NavPushH r h) -> dispatch nav $ unsafeToForeign (toPushAction r h)
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
      action = getAction self.props.navigation
    in
      comp
        { submit: \a -> resumeFlow self.props.navigation (action a)
        }
 -- TODO: implemenet codec for hint -- TODO: implement codec for info -- TODO: implement other actions -- TODO: implemenet getting info for first screen??? -- TODO: navigator and sub navigators is there a correlation with hoisting check navigation-ex comments regarding use of static -- TODO: implement deriving of url in purescript how do u derive nested navigators