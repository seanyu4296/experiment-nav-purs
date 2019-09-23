import { createStackNavigator } from 'react-navigation-stack';
import { StyleSheet, Text, View, Button } from 'react-native';
import React from 'react';

// console.log(testRPRan());
// Flow -> InitialState?
export default function createCustomNav(flowName, initialRoutes, config) {
  const MyStack = createStackNavigator(initialRoutes);
  class CustomNavigator extends React.Component {
    static flow = {
      common: {
        showReceipt: ({ navigation, out }) => {
          console.log('showReceipt');
          return navigation.push('ViewReceipt', {
            action: null,
            hint: 'viewreceipt',
          });
        },
        submission: ({ navigation, out }) => {
          console.log('submission');
          return CustomNavigator.flow.common.showReceipt({ navigation, out });
        },
        onShortcode: ({ navigation, out }) =>
          navigation.push('ShortCode', {
            hint: 'EnterShortCode',
            action: CustomNavigator.flow.common.submission,
          }),
        showError: ({ navigation, out, tools }) =>
          tools.dialog(() => <Text>HI</Text>),
      },
      // kulang pa ang value from initial screen
      fromTheStart: ({ navigation, out }) => {
        switch (out.type) {
          case 'ShortCode':
            return CustomNavigator.flow.common.onShortcode({ navigation, out });
          case 'Scan':
            return CustomNavigator.flow.common.submission({ navigation, out });
          default:
            return null;
        }
      },
      notFromTheStart: (hint) => {
        const {
          showReceipt,
          onShortcode,
          showError,
        } = CustomNavigator.flow.common;
        switch (hint.type) {
          case 'viewreceipt':
            return showReceipt;
          case 'entershortcode':
            return onShortcode;
          case 'receipterror':
            return showError;
          default:
            return () => {};
        }
      },
    };
    // this is very naive pa
    static getHintFromPath = (path) => {
      let subP = path.split('/')[1];
      return {
        type: subP,
      };
    };

    static getInitRoute = () => {
      // const { fromTheStart } = CustomNavigator.flow;
      // array of actions from callbacks then gobble up to state
      const initRoute = {
        routeName: config.initialRouteName,
        key: config.initialRouteName + '-init',
        params: {
          action: config.fromTheStart,
        },
      };
      return initRoute;
    };
    static navigationToDeriveState = {
      // everything should return AdditionalState
      push: (routeName, params) => {
        return {
          index: 1,
          dialog: null,
          routes: [{ routeName, key: 'second-route-key-manual', params }],
        };
      },
      popToTop: () => {},
    };
    static router = {
      ...MyStack.router,
      getStateForAction: (action, lastState) => {
        switch (action.type) {
          // have to seperate back and forward since
          case 'BROWSER_ACTION':
          // check flowName using action.path each navigator is a layer?
          // stack - index + store + routes + key = 0 1 2 (matches a store)
          // switch - switch is easy find the hint that matches
          // tabs - find the hint that matches
          // maybe no support for browser forward since what is stepping forward?
          // added feature?
          // console.log('LAST STATE before change:', lastState);
          // const x = lastState.store.find((value, i) => {
          //   return value.key === action.key;
          // });
          // if (x) {
          //   // forward
          //   return {
          //     ...lastState,
          //     index: lastState.routes.length - 1,
          //     routes: [...lastState.routes, x],
          //     store: lastState.store.filter((value) => {
          //       return value.key == action.key;
          //     }),
          //   };
          // } else {
          //   // back
          //   return lastState;
          //   return {
          //     ...lastState,
          //     index: lastState.routes.length - 2,
          //     routes: lastState.routes.slice(0, -1),
          //     store: [...lastState.store, ...lastState.routes.slice(-1)],
          //   };
          // }
          case 'BROWSER_FORWARD':
          case 'INIT-PATH-AND-PARAMS':
            const initRoute = CustomNavigator.getInitRoute();
            const initRoutes = [initRoute];
            if (action.path.startsWith(flowName)) {
              // possible are
              // - navDialog
              // - navPush
              // / navPopToTop
              // / navReset
              // / navBack

              const hint = CustomNavigator.getHintFromPath(action.path);
              if (hint.type) {
                /* type AdditionalState =
                  { dialog :: JSX
                  , routes: Array Route
                  , index: Number
                  }
                */
                const additionalState = CustomNavigator.flow.notFromTheStart(
                  hint
                )({ navigation: CustomNavigator.navigationToDeriveState });
                const addRoutes = additionalState ? additionalState.routes : [];
                return {
                  index: 0,
                  ...lastState,
                  ...additionalState,
                  routes: [...initRoutes, ...addRoutes],
                };
              }
              return {
                ...lastState,
                store: [],
                index: 0,
                routes: initRoutes,
              };
            } else {
              // empty state if flow name does not match the first segment of the url
              return null;
            }
          default:
            return MyStack.router.getStateForAction(action, lastState);
        }
      },
      getActionForPathAndParams: (path, params) => {
        return {
          type: 'INIT-PATH-AND-PARAMS',
          path,
        };
      },
      getTreeInfo: (state) => {
        // if stack only look at the active route?
        // if there is no router it is a screen
        // -- if there is look at getTreeInfo to know if switch, stack or tabs
        return {
          type: 'STACK',
          flowName,
          children: [],
        };
      },
    };
    render() {
      const { navigation } = this.props;

      return <MyStack navigation={navigation} />;
    }
  }
  return CustomNavigator;
}

// browser history control
// initialization to some place
// constraining to not do some things
