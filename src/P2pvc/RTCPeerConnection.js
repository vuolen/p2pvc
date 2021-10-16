exports.mkRTCPeerConnection = () => {
    const conn = new RTCPeerConnection();
    conn.createOffer = conn.createOffer.bind(conn);
    conn.setLocalDescription = (desc) => () => conn.setLocalDescription(desc);
    conn.setRemoteDescription = (desc) => () => conn.setRemoteDescription(desc);
    return conn;
}