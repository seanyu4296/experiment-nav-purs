import React from 'react';
import {
  realShortCode,
  realQrScanner,
  realReceipt,
  fromTheStart,
  notFromTheStart,
  onShortCode,
  scanQrDecodeHint
} from './output/ExpN.Flows.ScanQr';
import { createBrowserApp } from './createBrowserApp';
import createCustomNav from './createCustomNav';

const ScanQrFlow = createCustomNav(
  'scan-qr',
  {
    Receipt: {
      screen: realReceipt,
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
    notFromTheStart: onShortCode,
    hintDecode: scanQrDecodeHint
  }
);
const Root = createBrowserApp(ScanQrFlow);

export default Root;
