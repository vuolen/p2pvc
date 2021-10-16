module P2pvc.Main where

import Prelude (Unit, bind, const, unit, discard, ($), (<<<), (<>), pure)
import P2pvc.RTCPeerConnection
import Effect (Effect)
import Effect.Class.Console (errorShow, logShow, log)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type MediaConstraints = {audio :: Boolean, video :: Boolean}
type MediaStream = {active :: Boolean, id :: String}

foreign import getUserMediaImpl :: MediaConstraints -> Effect (Promise MediaStream)
getUserMedia :: MediaConstraints -> Aff MediaStream
getUserMedia = toAffE <<< getUserMediaImpl

foreign import setSrcObjectById :: String -> MediaStream -> Effect Unit

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem val f = case val of 
                    Just x -> f x
                    _ -> HH.text ""

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Initialize | Call | RSDInput String
type State = {
    peerConnection :: Maybe RTCPeerConnection
}

component =
    H.mkComponent 
        {
            initialState: const {peerConnection: Nothing}, 
            render,
            eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
        }
    where
    render state = 
        let
            localSDP = do
                conn <- state.peerConnection
                desc <- conn.localDescription
                pure desc.sdp
        in
            HH.div
                [ HP.id "root" ] 
                [ 
                    HH.h1_ [ HH.text "p2pvc" ],
                    HH.video [HP.id "remoteVideo"] [],
                    HH.video [HP.id "localVideo", HP.autoplay true, HP.muted true] [],
                    HH.br_,
                    HH.text "Share this code: ",
                    maybeElem localSDP (\text -> HH.input [HP.value text, HP.readOnly true]),
                    HH.h4_ [HH.text "OR"],
                    HH.text "Enter shared code here and press the button: ",
                    HH.input [HE.onValueInput (\str -> RSDInput str)],
                    HH.button [HP.id "submitCode", HE.onClick \_ -> Call] [HH.text "Submit code"]
                ]
    
    handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
    handleAction Initialize = do
        mediaStream <- H.liftAff $ getUserMedia {audio: true, video: true}
        H.liftEffect $ setSrcObjectById "localVideo" mediaStream
        
        peerConnection <- H.liftEffect $ mkRTCPeerConnection
        localDescription <- H.liftAff $ createOffer peerConnection
        H.liftEffect $ peerConnection.setLocalDescription localDescription
        H.modify_ _ { peerConnection = Just peerConnection }
    
    handleAction Call = do
        peerConnection <- H.gets _.peerConnection
        H.liftEffect $ log ("Calling ")
        

    handleAction (RSDInput str) = do
        peerConnection <- H.gets _.peerConnection 
        case peerConnection of 
            Just peerConnection -> do
                H.liftEffect $ peerConnection.setRemoteDescription {sdp: str}
                H.modify_ _ { peerConnection = Just peerConnection }
            Nothing -> H.liftEffect $ log "RSDInput without peerConnection" 
