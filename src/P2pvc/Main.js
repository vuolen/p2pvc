exports.getUserMediaImpl = (constraints) => () => navigator.mediaDevices.getUserMedia(constraints);

exports.setSrcObjectById = (id) => (mediaStream) => () => document.getElementById(id).srcObject = mediaStream;