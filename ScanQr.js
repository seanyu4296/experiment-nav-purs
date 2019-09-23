import React from 'react';
import { StyleSheet, Text, View, Button } from 'react-native';
import withPageProps from './withPageProps';

class ScanQr extends React.Component {
  render() {
    return (
      <View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
        <Text>ScanQr</Text>
        <Button
          title="submit"
          onPress={() => {
            this.props.onAction({ type: 'EnterShortCode', val: 'hi' });
          }}
        />
      </View>
    );
  }
}
// TODO: Convert this to purescript
// TODO: Fix code to be cleaner
// TODO: create navigator creators
export default withPageProps(ScanQr);

// should not be able to push two screens [1,2] only no [1,2,3]
