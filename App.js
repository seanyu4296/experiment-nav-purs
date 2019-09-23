import React from 'react';
import ScanQr from './ScanQr';
import ViewReceipt from './ViewReceipt';
import {
  realShortCode,
  realQrScanner,
  fromTheStart,
} from './output/ExpN.Flows.ScanQr';
import { createBrowserApp } from './createBrowserApp';
import createCustomNav from './createCustomNav';
console.log(realShortCode);
const CustomNav = createCustomNav(
  'scanqr',
  {
    Receipt: {
      screen: ViewReceipt,
    },
    ShortCode: {
      screen: realShortCode,
    },
    QrScanner: {
      screen: realQrScanner,
    },
  },
  {
    // reflectSymbol something
    initialRouteName: 'QrScanner',
    fromTheStart: fromTheStart,
  }
);
const Root = createBrowserApp(CustomNav);

export default Root;
