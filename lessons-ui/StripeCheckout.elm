module StripeCheckout where

import StartApp
import Html exposing (Html, text, div, button, header, footer)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, style)
import Json.Decode exposing (Decoder, string, object1, (:=))
import Json.Encode as E
import Effects exposing (Effects)
import Task exposing (Task, andThen, succeed)
import Debug
import Http exposing (Body, Error, defaultSettings, fromJson)
import Window
import Signal exposing ((<~))

import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Shadow as Shadow
import Css.Background as Background
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Dimension as Dimension
import Css.Margin as Margin
import Color exposing (Color, rgba, complement)


type alias Model =
    { key : PublishableKey
    , info : FormInfo
    , confirmation : Maybe (Result String ChargeSuccess) -- TODO: add another signal that intermitently switches this back to nothing
    }


type alias PublishableKey = String


type alias FormInfo =
    { amount : Int
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
    ( { key = pk
      , info = emptyInfo
      , confirmation = Nothing
      }
    , Effects.task (succeed Configure)
    )

emptyInfo : FormInfo
emptyInfo =
    { amount = 0
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
    | RecieveFromServer (Maybe ChargeSuccess)
    | Close -- sent by Recieve to close the modal via port to js
    | Choose Int String -- user supplied product parameters


finish : a -> Task x Action
finish = always <| succeed NoOp


-- TODO: Work in the optional opened and closed callbacks to get a full picture of the modal's state
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
                        , SubmitToServer pair
                            |> succeed
                            |> Effects.task
                        )
                Nothing ->
                    noFx model

        SubmitToServer pair ->
            ( model
            , postCharge pair
            )

        RecieveFromServer maybeResult ->
            case maybeResult of
                Just result ->
                    noFx { model | confirmation <- Just (Ok result)}
                Nothing ->
                    noFx { model | confirmation <- Just (Err "HEY YOU FUCKED UP")}

        Close -> -- no need to actually call, modal closes after submit, iframe js already listening for esc
            ( model
            , Effects.task
                ( Signal.send closeMailbox.address ()
                    `andThen` finish
                )
            )

        Choose newAmt newDes ->
            let info = model.info
            in
                noFx { model |
                        info <- { info |
                                    amount <- newAmt
                                ,   description <- Just newDes
                                }
                     }


type alias ChargeSuccess =
    { chargeID : String
    }


postCharge : (Token,Int) -> Effects Action
postCharge pair =
    jsonPost decodeResponse "http://localhost:8081/charge" (encodeRequest pair)
        |> Task.toMaybe
        |> Task.map RecieveFromServer
        |> Effects.task


jsonPost : Decoder value -> String -> Body -> Task Error value
jsonPost decoder url body =
    let request =
        { verb = "POST"
        , headers = [ ("Content-Type", "application/json") ]
        , url = url
        , body = body
        }
    in
        fromJson decoder (Http.send defaultSettings request)


encodeRequest : (Token,Int) -> Body
encodeRequest (token,amount) =
    E.object
        [ ("stripeToken",  E.string token)
        , ("amount", E.int amount)
        ]
        |> E.encode 4
        |> Http.string


decodeResponse : Decoder ChargeSuccess
decodeResponse =
    object1 ChargeSuccess
        ("chargeID" := string)


configMailbox : Signal.Mailbox PublishableKey
configMailbox =
    Signal.mailbox ""


openMailbox : Signal.Mailbox FormInfo
openMailbox =
    Signal.mailbox emptyInfo


closeMailbox : Signal.Mailbox ()
closeMailbox =
    Signal.mailbox ()

-- in order to make an (Int,Int) argument to view, I'd have to stop using start app and map it in manually


view : Signal.Address Action -> Model -> Html
view address model =
    div
    [ style
        <| column
        <| Text.color (rgba 255 255 255 1 ) []
    ]
    [ setViewport
    , div [ style <| end [] ] [ text "header" ]
    , div
      [ style <| flex [] ]
      [ div [ style <| side [] ] [text "left"]
      , div
        [ style <| content [] ]
        [ selector address model ]
      , div [style <| side [] ] [text "right"]
      ]
    , div [style <| end [] ] [text "footer"]
    ]



flex : Styles -> Styles
flex styles =
  display Display.Flex styles


end : Styles -> Styles
end styles =
  styles
    -- |> Dimension.height 50
    |> Background.color (rgba 26 188 156 1)
    |> centered
    |> Flex.grow 1


side : Styles -> Styles
side styles =
  styles
    -- |> Dimension.height 50
    |> Background.color (rgba 52 152 219 1)
    |> centered
    |> Flex.grow 1


content : Styles -> Styles
content styles =
  styles
    -- |> Dimension.width 200
    |> Dimension.height 200
    |> Background.color (rgba 52 73 94 1)
    |> centered
    |> Flex.grow 3


column : Styles -> Styles
column styles =
  styles
    |> flex
    |> Flex.direction Flex.Column


centered : Styles -> Styles
centered styles =
  styles
    |> flex
    |> Flex.alignItems Flex.AICenter
    |> Flex.justifyContent Flex.JCCenter


selector : Signal.Address Action -> Model -> Html
selector address model =
        div []
            [ button [onClick address (Choose 2500 "1 hour")] [text "1 hour"]
            , button [onClick address (Choose 4500 "2 hours")] [text "2 hours"]
            , button [onClick address (Choose 7000 "3 hours")] [text "3 hours"]
            , button [onClick address Open] [text "Checkout"] -- may have to prevent default
            , text ( "Current Amount: " ++ toString model.info.amount )
            , confirmationBox model.confirmation
            ]



confirmationBox : Maybe (Result String ChargeSuccess) -> Html
confirmationBox maybeResult =
    case maybeResult of
        Just result ->
            case result of
                Err msg ->
                    text "Yikes! Your charge didn't go through, contact Nick asap"
                Ok response ->
                    text ("You're all set! Save your charge ID: " ++ response.chargeID)
        Nothing ->
            text ""




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


port closeStripe : Signal ()
port closeStripe = closeMailbox.signal


-- TODO add a Nothing case
incomingToken' : Signal Action
incomingToken' = Signal.map (\pair -> TokenRecieved (Just pair) ) incomingToken


port incomingToken : Signal (Token,Int)


port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks


main : Signal Html
main =
    app.html
