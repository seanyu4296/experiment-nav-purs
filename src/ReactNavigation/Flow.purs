module ExpN.ReactNavigation.Flow where

import Prelude
import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free as Free
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..))
import Data.Generic.Rep as G
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Number as Number
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
import Record (get, insert, delete)
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
  = NavPush (route a) (Maybe hint)
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
  navPush_ :: forall a. route a -> n hint route a
  navPush :: forall a. route a -> hint -> n hint route a
  navPush' :: forall a. route a -> Maybe hint -> n hint route a

instance flowMonadNav :: MonadNav Flow hint route where
  navPush r h = navPush' r (Just h)
  navPush_ r = navPush' r Nothing
  navPush' r h = liftFFlow $ NavPush r h

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
  toPushAction :: forall a. r a -> Maybe hint -> JSNavPushAction a

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

-- shortCode <#>
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
  Maybe hint ->
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
  -- TODO: Change this to use maybe
  hintString = case hint of
    Just h -> Nullable.notNull <<< toPath <<< G.from $ h
    Nothing -> Nullable.null

  pp@{ routeName, action } = genToPushPartial <<< G.from $ route

type NavPushPartial a
  = { routeName :: String
    , action :: Foreign -> a
    }

type NavPushHint
  = { path :: String, query :: Object String }

class GenHint a where
  -- Rename toJsHint -- fromJsHint
  toPath :: a -> NavPushHint
  toHint :: NavPushHint -> Maybe a

instance genHintSum ::
  ( GenHint l
  , GenHint r
  ) =>
  GenHint (Sum l r) where
  toPath (Inl l) = toPath l
  toPath (Inr r) = toPath r
  toHint nph = toHint nph

instance genHintConstructorArgR ::
  ( IsSymbol hintName
  , Row.Lacks "key" r
  , RowToList r rl
  , QueryParamCodec rl r
  ) =>
  GenHint (Constructor hintName (Argument { | r })) where
  toPath (Constructor (Argument val)) = { path, query }
    where
    path = reflectSymbol (SProxy :: SProxy hintName)

    query = toQuery (RLProxy :: RLProxy rl) val
  toHint { path, query } = case path == hn of
    true -> do
      record <- fromQuery (RLProxy :: RLProxy rl) query
      pure $ (Constructor (Argument record) :: Constructor hintName (Argument { | r }))
    false -> Nothing
    where
    hn = reflectSymbol (SProxy :: SProxy hintName)

class QueryParamCodec (rl :: RowList) (r :: #Type) | rl -> r where
  toQuery :: RLProxy rl -> { | r } -> Object String
  fromQuery :: RLProxy rl -> Object String -> Maybe { | r }

instance queryParamCodecNil :: QueryParamCodec Nil () where
  toQuery _ _ = FO.empty
  fromQuery _ _ = Just {}

instance queryParamCodecCons ::
  ( IsSymbol name
  , QueryStringCodec typ
  , Row.Cons name typ tailR rows -- rows is the summation of all the rows, tailR is the remaining rows unprocessed,
  , Row.Lacks name tailR
  , QueryParamCodec tailrl tailR
  ) =>
  QueryParamCodec (Cons name typ tailrl) rows where
  toQuery _ r = FO.insert key value tail
    where
    sp = SProxy :: SProxy name

    key = reflectSymbol sp

    value = toQueryString $ get sp r

    -- How did this work???
    tail = toQuery (RLProxy :: RLProxy tailrl) (delete sp r)
  fromQuery _ o = do
    let
      sp = SProxy :: SProxy name

      keyN = reflectSymbol sp
    val <- FO.lookup keyN o >>= fromQueryString
    tail <- fromQuery (RLProxy :: RLProxy tailrl) o
    pure $ insert sp val tail

class QueryStringCodec a where
  toQueryString :: a -> String
  fromQueryString :: String -> Maybe a

instance queryStringCodecString :: QueryStringCodec String where
  toQueryString = identity
  fromQueryString = case _ of
    "" -> Nothing
    s -> Just s

instance queryStringCodecBoolean :: QueryStringCodec Boolean where
  toQueryString true = "true"
  toQueryString false = "false"
  fromQueryString = case _ of
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing

instance queryStringCodecInt :: QueryStringCodec Int where
  toQueryString i = show i
  fromQueryString = Int.fromString

instance queryStringCodecNumber :: QueryStringCodec Number where
  toQueryString n = show n
  fromQueryString = Number.fromString

-- Maybe (Maybe shouldnt be here it should be in toQuery)???
instance queryStringCodecMaybe :: QueryStringCodec a => QueryStringCodec (Maybe a) where
  toQueryString = case _ of
    Just v -> toQueryString v
    Nothing -> ""
  fromQueryString = case _ of
    "" -> Nothing
    x -> fromQueryString x

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
      , hint :: Nullable NavPushHint
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
    -- EnterShortCode (String -> Free ()) -- { params : { cb: String -> Free () }}
    -- esc <#> \a -> { params: { cb: a }}
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
      action = getAction self.props.navigation
    -- if there is path and path can be converted to hint then use notFromTheStart run with path
    in
      comp
        { submit: \a -> resumeFlow self.props.navigation (action a)
        }
 {-
 -- // TODO: implemenet codec for hint
 -- TODO: URL -> Maybe Hint -> Correct state or behavior
 -- TODO: whats the difference between hint and info?
 -- TODO: implement codec for info
 -- TODO: implement other actions
 -- TODO: implemenet getting info for first screen???
 -- TODO: navigator and sub navigators is there a correlation with hoisting check navigation-ex comments regarding use of static
 -- TODO: implement deriving of url in purescript how do u derive nested navigators
 -}