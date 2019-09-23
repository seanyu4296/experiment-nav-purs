/*

type Flow hint = Generic hint =>
  { fromTheStart :: RealFlow
  , notFromTheStart :: hint -> RealFlow  // initialRoute -> with preloaded screen
  , fromurl :: URL -> hint
  // maybe this is generalizable just some part basically GenericRep -> Hint
  // basically if you declare a generic rep for the "hint" you can URL -> hint?
  // maybe this is the toRouteParam ni jv
  }
navPush :: Route -> ScanQrHint -> NavM

Version 2:
scanQrFlow :: Flow ScanQrHint
scanQrFlow =
  { fromTheStart
  , notFromTheStart
  }
  where
    fromTheStart value = case value of
      EnterShortCode -> onShortCode
      Scan d -> submission p

    notFromTheStart hint = case hint of
      ViewReceipt receipt -> showReceipt receipt
      EnterShortCode -> onShortCode
      ReceiptError -> showError

    initialize hint = case hint of
      Just h -> notFromTheStart h
      Nothing -> fromTheStart

    --- supporting functions

    showError = navDialog <Text>HI</Text>
    onShortCode = do
      sc <- navPush rs.enterShortCode h.entershortCode
      submission sc

    submission p = do
      x <- apiCall p
      case x of
        Right r -> showReceipt receipt
        Left l -> errorDialog

    showReceipt r = navPushI rs.receiptScreen h.viewReceipt r



Version 1:
scanQrFlow hint = case hint of
  ViewReceipt receipt -> showReceipt receipt
  EnterShortCode -> onShortCode
  FromInitialScreen value -> case value of
    EnterShortCode -> onShortCode
    Scan d -> submission p
where
  onShortCode = do
    sc <- navPush rs.enterShortCode
    submission sc

  submission p = do
    x <- apiCall p
    case x of
      Right r -> showReceipt receipt
      Left l -> errorDialog

  showReceipt r = navPushI rs.receiptScreen r
*/
