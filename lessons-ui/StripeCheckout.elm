module StripeCheckout where

import StartApp
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick, onWithOptions, Options)
import Html.Attributes
import Json.Decode exposing (value)
import Effects exposing (Effects)
import Task exposing (Task, andThen, succeed)
import Debug

type alias Model =
    { key : PublishableKey
    , info : FormInfo
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
    -- | SubmitToServer Token -- sent by user, can send user defined Json Body
    -- | RecieveFromServer Json.Decode.Value Json.Decode.Decoder a
    -- | Close -- sent by Recieve to close the modal via port to js
    | Amount Int -- set the amount
    | Description String

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
                Just (token,amount) ->
                    let log = Debug.log "Here ya go: " (token,amount)
                    in
                        noFx model
                Nothing ->
                    noFx model
        Amount newAmt ->
            let info = model.info
            in
                noFx { model |
                        info <- { info | amount <- Just newAmt}
                     }
        Description newDes ->
            let info = model.info
            in
                noFx { model |
                        info <- { info | description <- Just newDes}
                     }




{-|
    1. When page is loaded, load stripejs
    2. As soon as possible, set the pkey and send a configure js (might have to Configure on load if errors)
    3. onClick of the button, send an Open, which somehow talks to a port to open with the passed in options in js
       this also sets the "token" callback to send the token over the another port
    4. When the

|-}

configMailbox : Signal.Mailbox PublishableKey
configMailbox =
    Signal.mailbox ""


openMailbox : Signal.Mailbox FormInfo
openMailbox =
    Signal.mailbox emptyInfo



view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ button [onClick address (Amount 5)] [text "Set 5"]
        , button [onClick address (Amount 10)] [text "Set 10"]
        , button [onClick address (Amount 0)] [text "Set 0"]
        , button [onClick address Configure] [text "Configure"]
        , button [onClick address Open] [text "Pay with Stripe"] -- may have to prevent default
        , text ( "Current Amount: " ++
                    (  model.info.amount
                    |> Maybe.withDefault 0
                    |> toString
                    )
               )
        ]

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

incomingToken' : Signal Action
incomingToken' = Signal.map (\pair -> TokenRecieved (Just pair) ) incomingToken


port incomingToken : Signal (Token,Int)


port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks


main : Signal Html
main = --Signal.constant (div [] [text "Yolo"])
    app.html
