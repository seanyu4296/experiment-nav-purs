exports.getAction = (navigation) => {
  return navigation.getParam('action');
};
exports.dispatch = (navigation) => (payload) => () =>
  navigation.dispatch(payload);
