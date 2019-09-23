module ExpN.Flows.ScanQr where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import ExpN.ReactNavigation.Flow (class MonadNav, class ToPushAction, Flow, FlowScreen(..), genericToPushAction, navPush, withPageProps)
import ExpN.Screens.QrScanner (qrScanner)
import ExpN.Screens.ShortCode (shortCode)

type ReceiptD
  = { points :: Int
    }

data ScanQrHint
  -- if fail to parse receipt from query string, the flow starts from initialRoute
  = ViewReceipt ReceiptD
  | EnterShortCode (Maybe String)

data ScanQrRoute a
  = QrScanner (QrScannerAction -> a)
  | ShortCode (String -> a)
  | Receipt ReceiptD (Unit -> a)

derive instance functorSQR :: Functor (ScanQrRoute)

derive instance genericSQR :: Generic (ScanQrRoute a) _

instance toPushRouteSQR :: ToPushAction ScanQrRoute ScanQrHint where
  toPushAction r h = genericToPushAction r

data QrScannerAction
  = QrScannerESC
  | QrScannerD String

qrscannerR :: ScanQrRoute QrScannerAction
qrscannerR = QrScanner identity

shortCodeR :: ScanQrRoute String
shortCodeR = ShortCode identity

receiptR :: ReceiptD -> ScanQrRoute Unit
receiptR receipt = Receipt receipt identity

claim :: String -> Aff (Either Unit ReceiptD)
claim s = (pure $ Right { points: 1234 })

fromTheStart :: MonadNav Flow ScanQrHint ScanQrRoute => QrScannerAction -> Flow ScanQrHint ScanQrRoute Unit
fromTheStart = case _ of
  QrScannerESC -> onShortCode Nothing
  QrScannerD d -> submission d
  where
  --   showReceipt receipt = navPush (receiptR receipt) receipt
  onShortCode code = do
    sc <- navPush (shortCodeR) (EnterShortCode code)
    logShow ("onShortCode:" <> sc)
    submission sc

  submission d = do
    logShow ("SUBMission:" <> d)
    res <- liftAff $ claim d
    navPush (shortCodeR) (EnterShortCode Nothing) *> pure unit
    navPush (shortCodeR) (EnterShortCode Nothing) *> pure unit
    navPush (shortCodeR) (EnterShortCode Nothing) *> pure unit
    case res of
      Right r -> navPush (shortCodeR) (EnterShortCode Nothing) *> pure unit
      Left l -> pure unit

realQrScanner :: FlowScreen ScanQrRoute ScanQrHint Unit
realQrScanner = withPageProps qrScanner

realShortCode :: FlowScreen ScanQrRoute ScanQrHint Unit
realShortCode = withPageProps shortCode
 {- realShortCode :: FlowScreen ScanQrRoute ScanQrHint Unit
-- realShortCode = withPageProps shortCode






-- scanQrFlow :: FlowNav STACK ScanQrRoute ScanQrAction Unit ScanQrHint
-- scanQrFlow =
--   { fromTheStart
--   , notFromTheStart
--   , initialRoute: scanQrR
--   }
--   where
--   fromTheStart = case _ of
--     ScanQrESC -> onShortCode Nothing
--     ScanQrD d -> submission d
--   notFromTheStart h = case h of
--     ViewReceipt receipt -> showReceipt receipt
--     EnterShortCode code -> onShortCode code
--   -- Supporting functions
--   showReceipt receipt = navPush (receiptR receipt) (ViewReceipt receipt)
--   onShortCode code = do
--     sc <- navPush (shortCodeR) (EnterShortCode code)
--     submission sc
--   submission d = do
--     res <- liftAff $ claim d
--     logShow res
--     case res of
--       Right r -> showReceipt res
--       Left l -> pure unit
-- Study FREE????
-- Create a function for initialRouteParams?
-- Why route needs to be a Functor? because of Free since FlowM has instance of Functor

-- component

-- create ToPushRoute generic
-- navigation push has a default action.action for action to run for child router
-- initialRouteParams ::
--   forall hint route o initialOut.
--   Functor route =>
--   ToPushAction route hint =>
--   MonadNav Flow hint route =>
--   ( initialOut ->
--     Flow hint route o
--   ) ->
--   Navigation o ->
--   initialOut ->
--   Effect Unit
-- initialRouteParams ff nav initOut = case Free.resume (unwrap (ff initOut)) of
--   Left (NavPush r h) -> dispatch nav $ unsafeToForeign (toPushAction r h)
--   Left (NavAff aff) -> logShow "??? NavAff ??? "
--   Left (NavEff eff) -> logShow "???? NavEff ??"
--   Left (NavGoBack) -> logShow "???? NavGoBack ??"
--   Right _ -> logShow "DONE"

-- testRP :: Navigation Unit -> ScanQrAction -> Effect Unit
-- testRP = initialRouteParams fromTheStart

-- testR nav = initialRouteParams fromTheStart nav ScanQrESC
 -- testRPRan = testRP ScanQrESC

 -}