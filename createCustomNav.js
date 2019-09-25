import { createStackNavigator } from 'react-navigation-stack';
import { StyleSheet, Text, View, Button } from 'react-native';
import React from 'react';

/* eslint-disable import/no-commonjs */
const queryString = require('query-string');

export default function createCustomNav(flowName, routeConfigs, config) {
  const MyStack = createStackNavigator(routeConfigs);
  class CustomNavigator extends React.Component {
    static flowName = flowName; // for traversing down the state
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
          case 'CLEAR-INIT-PATH':
            return {
              ...lastState,
              initialPath: null,
            };
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
            let pathArr = action.path.split('/');
            if (pathArr[0].startsWith(flowName)) {
              let remainingPath = pathArr.slice(1);
              let baseNewState = {
                ...lastState,
                index: 0,
                routes: [initRoute],
              };
              if (remainingPath.length == 1) {
                const [path, query] = remainingPath[0].split('?');
                return {
                  ...baseNewState,
                  hintFromPath: {
                    path,
                    query: queryString.parse(query),
                  },
                };
              } else if (remainingPath.length > 1) {
                // process it as a subflow to match
                const childFlowNames = {};
                const routeNames = Object.keys(routeConfigs);
                routeNames.forEach((routeName) => {
                  const routeConfig = routeConfigs[routeName];
                  const screen =
                    routeConfig && routeConfig.screen
                      ? routeConfig.screen
                      : routeConfig;
                  if (screen && screen.flowName) {
                    childFlowNames[screen.flowName] = routeName;
                  } else {
                  }
                });
                const matchedFlowName = childFlowNames[remainingPath[0]];
                if (matchedFlowName) {
                  // if matched a child flow initialize that flow traverse down
                  const matchedFlowRoute = childFlowNames[matchedFlowName];
                  const childFlowState = matchedFlowRoute.router.getStateForAction(
                    {
                      type: 'INIT-PATH-AND-PARAMS',
                      path: remainingPath.slice(-1).join('/'),
                    }
                  );
                  return {
                    index: 1, // is this correct to assume that the childFlowState is index 1??? maybe this differs for each navigator
                    ...baseNewState,
                    routes: [...baseNewState.routes, childFlowState],
                  };
                }
                return baseNewState;
              } else {
                // if nothing matches just normal initialState
                return baseNewState;
              }
            } else {
              // empty state if flow name does not match the first segment of the url
              return null;
            }
          default:
            return MyStack.router.getStateForAction(action, lastState);
        }
      },
      getActionForPathAndParams: (path, params) => {
        // this action should behave correctly for children?
        /* { type: "", path: "", childActions}
        // roughly getStateForAction will call children getStateForAction
         */
        return {
          type: 'INIT-PATH-AND-PARAMS',
          path,
        };
      },
    };
    componentDidMount() {
      if (
        this.props.navigation.state &&
        this.props.navigation.state.hintFromPath
      ) {
        console.log(
          'DECODED HINT:',
          config.hintDecode(this.props.navigation.state.hintFromPath)
        );
      }
    }
    render() {
      const { navigation } = this.props;
      // if there is path -> run notFromTheStart -> clear path through running an action?
      // prevent from rendering the view since default StackViews does not support null state;
      return navigation.state && <MyStack navigation={navigation} />;
    }
  }
  return CustomNavigator;
}

// browser history control
// initialization to some place
// constraining to not do some things
