module P2pvc.RTCPeerConnection where

import Prelude ((<<<), Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))


type RTCSessionDescription = {sdp :: String}

type RTCPeerConnection = {
    createOffer :: Effect (Promise RTCSessionDescription), 
    setLocalDescription :: RTCSessionDescription -> Effect Unit,
    setRemoteDescription :: RTCSessionDescription -> Effect Unit,
    localDescription :: Maybe RTCSessionDescription,
    remoteDescription :: Maybe RTCSessionDescription 
}

createOffer :: RTCPeerConnection -> Aff RTCSessionDescription
createOffer = toAffE <<< _.createOffer

foreign import mkRTCPeerConnection :: Effect RTCPeerConnection