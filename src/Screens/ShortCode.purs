module ExpN.Screens.ShortCode where

import Prelude
import ExpN.ReactNavigation.Flow (PageProps)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.Events (handler_)
import React.Basic.Native as RN

c :: forall t1. Component t1
c = createComponent "ShortCode"

shortCode :: PageProps String -> JSX
shortCode = makeStateless c render
  where
  render props = RN.view_ [ RN.button { onPress: handler_ $ props.submit "hi", title: "TEST" } ]
