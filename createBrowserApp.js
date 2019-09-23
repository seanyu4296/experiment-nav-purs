import {
  createBrowserHistory,
  createHashHistory,
  createMemoryHistory,
} from 'history';
import React from 'react';
import {
  NavigationActions,
  getNavigation,
  NavigationProvider,
} from '@react-navigation/core';
import { SafeAreaProvider } from 'react-native-safe-area-context';

/* eslint-disable import/no-commonjs */
const queryString = require('query-string');

const getPathAndParamsFromLocation = (location) => {
  const path = encodeURI(location.pathname.substr(1));
  const params = queryString.parse(location.search);
  return { path, params };
};

const matchPathAndParams = (a, b) => {
  if (a.path !== b.path) {
    return false;
  }
  if (queryString.stringify(a.params) !== queryString.stringify(b.params)) {
    return false;
  }
  return true;
};

function getHistory(history) {
  if (typeof history === 'string') {
    switch (history) {
      case 'browser':
        return createBrowserHistory();
      case 'hash':
        return createHashHistory();
      case 'memory':
        return createMemoryHistory();
      default:
        throw new Error(
          '@react-navigation/web: createBrowserApp() Invalid value for options.history ' +
            history
        );
    }
  }
  return history || createBrowserHistory();
}

function deriveBrowserAction(a, ti, ps, s) {
  function getNewURLFromLatestRoute(treeInfo, state) {
    // TODO: this should be written in purescript
    const s = state;
    console.log('getNewURLFromLatestRoute:', state);
    if (state) {
      const hintS =
        s.routes[s.index].params && s.routes[s.index].params.hint
          ? s.routes[s.index].params.hint.path
          : '';
      const q = queryString.stringify({
        key: s.routes[s.index].key,
        ...(hintS ? s.routes[s.index].params.hint.query : {}),
      });
      return treeInfo.flowName + '/' + hintS + '?' + q;
    }
    return '';
  }
  switch (a.type) {
    case 'Navigation/PUSH':
      return {
        type: 'PUSH',
        newURL: '/' + getNewURLFromLatestRoute(ti, s),
      };
    case 'Navigation/BACK':
      return {
        type: 'BACK',
        newURL: '/' + getNewURLFromLatestRoute(ti, s),
      };
    case 'INIT-PATH-AND-PARAMS':
      return {
        type: 'REPLACE',
        newURL: '/' + getNewURLFromLatestRoute(ti, s),
      };
    default:
      return {
        type: 'NONE',
      };
  }
}
function translateBrowserAction(history, browserAction) {
  switch (browserAction.type) {
    case 'PUSH':
    case 'BACK':
    case 'REPLACE':
      history.replace(browserAction.newURL, { fromJs: true });
    case 'NONE':
      return;
    default:
      return;
  }
}

export function createBrowserApp(App, { history: historyOption } = {}) {
  const history = getHistory(historyOption);
  let currentPathAndParams = getPathAndParamsFromLocation(history.location);
  console.log(currentPathAndParams);
  const initAction =
    App.router.getActionForPathAndParams(
      currentPathAndParams.path,
      currentPathAndParams.params
    ) || NavigationActions.init();

  const setHistoryListener = (selff) => {
    const { setState, state, dispatch } = selff;
    history.listen((location, action) => {
      console.log('LOCATION:', location, action);
      switch (action) {
        case 'POP':
          // if the key is in the store put it in the
          // Should check if forward or back
          const path = encodeURI(location.pathname.substr(1));
          const qs = queryString.parse(location.search);
        //          dispatch({ type: 'BROWSER_ACTION', key: qs.key, path });
        default:
          break;
      }
    });
  };

  class WebApp extends React.Component {
    // state = { nav: App.router.getStateForAction(initAction) };
    constructor(props) {
      super(props);
      const initState = App.router.getStateForAction(initAction);

      translateBrowserAction(
        history,
        deriveBrowserAction(
          initAction,
          App.router.getTreeInfo(initState),
          null,
          initState
        )
      );
      // derive change to url?
      // /scanqr/entersc?key={key}
      // /scanqr/viewreceipt?key={key}
      // /scanqr
      // Refresh
      // -- url -> hint -> screen change -> update url to add the key using replace?
      // normal push
      // -- state change -> (prevstate + newState + tree info) -> url change (what should be the url here?)
      // AWMEN it seems the approach only supports for pure refresh
      // -- how to mark what the hint is for a certain step???? or ok lng kahit hnd?
      // -- e.g. entershortcode screen (or we indiciate what is the hint to push to the url?) what is the hint for that step?
      // /catalog/catalog-item-{key}
      // /catalog/catalog-item?id=1234
      // -- Wala naman ata pake yun url since for first and second screen lang?
      // /scanqr/catalog/catalog-item?id=1234
      console.log('INITIAL STATE:', initState);
      this.state = {
        nav: initState,
      };
    }
    _title = document.title;
    _actionEventSubscribers = new Set();
    componentDidMount() {
      setHistoryListener(this);
      //      this.updateTitle();
      this._actionEventSubscribers.forEach((subscriber) =>
        subscriber({
          type: 'action',
          action: initAction,
          state: this.state.nav,
          lastState: null,
        })
      );
    }
    componentDidUpdate() {
      // this.updateTitle();
    }
    updateTitle() {
      const { state } = this._navigation;
      if (state) {
        const childKey = state.routes[state.index].key;
        const activeNav = this._navigation.getChildNavigation(childKey);
        const opts = App.router.getScreenOptions(activeNav);
        this._title = opts.title || opts.headerTitle;
        if (this._title) {
          document.title = this._title;
        }
      }
    }

    _onNavigationStateChange(prevNav, nav, action) {
      if (typeof this.props.onNavigationStateChange === 'function') {
        this.props.onNavigationStateChange(prevNav, nav, action);
      }
    }

    render() {
      this._navigation = getNavigation(
        App.router,
        this.state.nav,
        this.dispatch,
        this._actionEventSubscribers,
        () => this.props.screenProps,
        () => this._navigation
      );
      return (
        <SafeAreaProvider>
          <NavigationProvider value={this._navigation}>
            {this.state.nav ? (
              <App {...this.props} navigation={this._navigation} />
            ) : null}
          </NavigationProvider>
        </SafeAreaProvider>
      );
    }
    dispatch = (action) => {
      const lastState = this.state.nav;
      const newState = App.router.getStateForAction(action, lastState);

      function LogNav(s, a) {
        this.STATE = s;
        this.ACTION = a;
        this.ACTIONTYPE = a.type;
      }
      console.log(new LogNav(newState, action));
      const dispatchEvents = () =>
        this._actionEventSubscribers.forEach((subscriber) =>
          subscriber({
            type: 'action',
            action,
            state: newState,
            lastState,
          })
        );
      if (newState && newState !== lastState) {
        this.setState({ nav: newState }, () => {
          this._onNavigationStateChange(lastState, newState, action);
          dispatchEvents();
        });

        // -- If there is a hint for that step insert it?? but how??? - through state?
        // /onboarding/entermobile?key=id-123124125
        // /onboarding?key=12412125
        if (action.type !== 'BROWSER_ACTION') {
          translateBrowserAction(
            history,
            deriveBrowserAction(
              action,
              App.router.getTreeInfo(newState),
              lastState,
              newState
            )
          );
        }
        // if stack look at routes and index and routeName, /scanqr/entermobile-1234
        // if bottom tabs look at routes and index, /home/catalog
        // if switch look at routes and index, /top/login

        // treeInfo + newState + lastState = effect for the url
        // TODO: is this really correct? maybe we should take into consideration if its a pop or push?
        // THIS IS FOR URL CHANGING
        //
        // const pathAndParams =
        //   App.router.getPathAndParamsForState &&
        //   App.router.getPathAndParamsForState(newState);
        // if (
        //   pathAndParams &&
        //   !matchPathAndParams(pathAndParams, currentPathAndParams)
        // ) {
        //   currentPathAndParams = pathAndParams;
        //   history.push(
        //     `/${pathAndParams.path}?${queryString.stringify(
        //       pathAndParams.params
        //     )}`
        //   );
        // }
      } else {
        dispatchEvents();
      }
    };
  }
  return WebApp;
}
