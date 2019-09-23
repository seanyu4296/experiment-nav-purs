import React from 'react';
import { StyleSheet, Text, View, Button } from 'react-native';
import { withPageProps } from './output/ExpN.ScanQr';
class EnterShortCode extends React.Component {
  render() {
    return (
      <View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
        <Text>EnterShortCode</Text>
        <Button
          title="asdf"
          onPress={() => this.props.submit('FAKESHORTCODE')}
        />
      </View>
    );
  }
}

export default withPageProps(EnterShortCode);
