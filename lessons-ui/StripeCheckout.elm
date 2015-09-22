module StripeCheckout where

import StartApp
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick, onWithOptions, Options)
import Html.Attributes
import Json.Decode exposing (Decoder, Value, value)
import Effects exposing (Effects)
import Task exposing (Task, andThen, succeed)
import Debug

type alias Model =
    { key : PublishableKey
    , info : FormInfo
    , confirmation : Maybe (Result ServerResponse String)
    }

type alias ServerResponse =
    { chargeID : String
    }

type alias PublishableKey = String

type alias FormInfo =
    { amount : Maybe Int
    , name : Maybe String
    , description : Maybe String
    , imageUrl : Maybe String
    , locale : Maybe String
    , currency : Maybe String -- Could define a currency ADT
    , panelLabel : Maybe String
    , zipCode : Bool
    , billingAddress : Bool
    , shippingAddress : Bool
    , email : Maybe String
    , allowRememberMe : Bool
    , bitcoin : Bool
    , alipay : Maybe Bool -- has actual states 'auto', true, and false
    , alipayReusable :  Bool
    }

type alias Token = String

init : PublishableKey -> (Model, Effects Action)
init pk =
    (
      { key = pk
      , info = emptyInfo
      , confirmation = Nothing
      }
    , Effects.task (succeed Configure)
    )

emptyInfo : FormInfo
emptyInfo =
    { amount = Nothing
    , name = Nothing
    , description = Nothing
    , imageUrl = Nothing
    , locale = Nothing
    , currency = Nothing
    , panelLabel = Nothing
    , zipCode = False
    , billingAddress = False
    , shippingAddress = False
    , email = Nothing
    , allowRememberMe = False
    , bitcoin = False
    , alipay = Nothing
    , alipayReusable = False
    }



noFx : model -> (model, Effects a)
noFx model =
    (model, Effects.none)





type Action
    = NoOp
    | Configure -- sent in initialization, sends pubkey to js
    | Open -- sent by user to open the modal, sends other params to js, which then calls open, also sets token callback
    | TokenRecieved (Maybe (Token,Int)) -- sent by js token callback
    | SubmitToServer (Token,Int) -- sent by user, can send user defined Json Body
    | RecieveFromServer (Maybe ServerResponse)
    | Close -- sent by Recieve to close the modal via port to js
    | Choose Int String -- user supplied product parameters

finish : a -> Task x Action
finish = always <| succeed NoOp

update : Action -> Model -> (Model, Effects Action)
update address model =
    case address of
        NoOp ->
            noFx model
        Configure ->
            ( model
            , Effects.task
                ( Signal.send configMailbox.address model.key
                    `andThen` finish
                )
            )
        Open ->
            ( model
            , Effects.task
                ( Signal.send openMailbox.address model.info
                    `andThen` finish
                )
            )
        TokenRecieved maybeToken ->
            case maybeToken of
                Just pair ->
                        ( model
                        , Effects.task (succeed (SubmitToServer pair) )
                        )
                Nothing ->
                    noFx model
        SubmitToServer pair ->
            Debug.crash "Not yet implemented SubmitToServer"
        RecieveFromServer maybeResult ->
            case maybeResult of
                Just result ->
                    Debug.crash "Not yet implemented RecieveFromServer Just"
                Nothing ->
                    Debug.crash "Not yet implemented RecieveFromServer Nothing"
        Close -> -- no need to actually call this in the main flow, form closes after submit, could listen on a key
            Debug.crash "Not yet implemented Close"
        Choose newAmt newDes ->
            let info = model.info
            in
                noFx { model |
                        info <- { info | amount <- Just newAmt
                                , description <- Just newDes
                                }
                     }


configMailbox : Signal.Mailbox PublishableKey
configMailbox =
    Signal.mailbox ""


openMailbox : Signal.Mailbox FormInfo
openMailbox =
    Signal.mailbox emptyInfo


view : Signal.Address Action -> Model -> Html
view address model =
        div []
            [ button [onClick address (Choose 2500 "1 hour")] [text "1 hour"]
            , button [onClick address (Choose 4500 "2 hours")] [text "2 hours"]
            , button [onClick address (Choose 7000 "3 hours")] [text "3 hours"]
            , button [onClick address Open] [text "Checkout"] -- may have to prevent default
            , text ( "Current Amount: " ++
                        ( model.info.amount
                            |> Maybe.withDefault 0
                            |> toString
                        )
                   )
            , confirmationBox model.confirmation
            ]


confirmationBox : Maybe (Result ServerResponse String) -> Html
confirmationBox maybeResult =
    case maybeResult of
        Just result ->
            case result of
                Ok response ->
                    div []
                        [ text "You're all set! Save your charge ID: " ++ (toString result.chargeID)]
                Err msg ->
                    div [] [text "Yikes! Your charge didn't go through, contact Nick asap"]
        Nothing ->
            div [] []




app =
    StartApp.start
        { init = init "pk_test_Y31x7Mqyi1iY63IQb95IAORm"
        , update = update
        , view = view
        , inputs = [incomingToken']
        }


port configureStripe : Signal PublishableKey
port configureStripe = Signal.dropRepeats configMailbox.signal


port openStripe : Signal FormInfo
port openStripe = openMailbox.signal

-- TODO add a Nothing case
incomingToken' : Signal Action
incomingToken' = Signal.map (\pair -> TokenRecieved (Just pair) ) incomingToken


port incomingToken : Signal (Token,Int)


port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks


main : Signal Html
main = --Signal.constant (div [] [text "Yolo"])
    app.html
