import React from 'react';
import ScanQr from './ScanQr';
import ViewReceipt from './ViewReceipt';
import { realShortCode } from './output/ExpN.ScanQr';
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
    ScanQr: {
      screen: ScanQr,
    },
  },
  {
    initialRouteName: 'ScanQr',
  }
);
const Root = createBrowserApp(CustomNav);

export default Root;
