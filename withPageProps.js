import React from 'react';

function withPageProps(Component) {
  class Wrapper extends React.Component {
    onAction = (out) => {
      // Action is a i -> flow
      console.log(this.props.navigation.getParam('action'));
      this.props.navigation.getParam('action')({
        navigation: this.props.navigation,
        out,
      });
    };
    render() {
      return <Component onAction={this.onAction} />;
    }
  }
  return Wrapper;
}

export default withPageProps;
