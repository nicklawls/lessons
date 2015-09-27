module Main where

import StartApp
import Html exposing (Html, text,p, div, button, header, footer)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, hidden, disabled)
import Json.Decode exposing (Decoder, string, object1, (:=))
import Json.Encode as Encode
import Effects exposing (Effects)
import Task exposing (Task, andThen, succeed)
import Http exposing (Body, Error, defaultSettings, fromJson)
import Style exposing (..)
import Css exposing (setViewport)

-- Refactoring plan
{-|

type alias Model =
    { key : PublishableKey
      locale : Locale
      checkout : CheckoutState -- nee FormInfo, repurposed
      selector : Selector.Model -- user defined form component
      confirm : Confirmation.Model -- user defined confirmation component
    }


that recieves a token only, passes it to the server with some extra specified info
from the Form.Model and maybe the CheckoutState
|-}

type alias Model =
    { key : PublishableKey
    , locale : Locale
    , info : FormInfo
    , confirmation : Maybe (Result String ChargeSuccess) -- TODO: add another signal that intermitently switches this back to nothing
    }


type alias PublishableKey = String

type alias Locale = String

type alias FormInfo =
    { amount : Int
    , name : Maybe String
    , description : Maybe String
    , imageUrl : Maybe String
    , currency : Maybe String -- Could define a currency ADT
    , panelLabel : Maybe String
    , zipCode : Bool
    , billingAddress : Bool
    , shippingAddress : Bool
    , email : Maybe String
    , allowRememberMe : Bool
    , bitcoin : Bool
    , alipay : Maybe Bool -- has actual states 'auto', true, and false
    , alipayReusable : Bool
    }

defaultInfo : FormInfo
defaultInfo =
    { amount = 0
    , name = Just "Bass Drum Lessons"
    , description = Nothing
    , imageUrl = Nothing
    , currency = Nothing
    , panelLabel = Nothing
    , zipCode = True
    , billingAddress = False
    , shippingAddress = False
    , email = Nothing
    , allowRememberMe = True
    , bitcoin = False
    , alipay = Nothing
    , alipayReusable = False
    }


-- TODO: Send the full token structure
type alias Token = String


type alias ChargeRequest = (Token,Int)


type alias ChargeSuccess =
    { chargeID : String
    }


init : PublishableKey -> Locale -> (Model, Effects Action)
init key locale =
    ( { key = key
      , locale = locale
      , info = defaultInfo
      , confirmation = Nothing
      }
    , Effects.task (succeed Configure)
    )


noFx : model -> (model, Effects a)
noFx model =
    (model, Effects.none)


type Action
    = NoOp -- nothing
    | Configure -- supply necessary checkout params to stripe
    | Open -- copy the optional params and open the checkout form
    | TokenDispatch (Maybe Token) -- send the token to wherever it needs to go
    | RecieveFromServer (Maybe ChargeSuccess) -- recive comfirmation of charge
    | Close -- close the modal
    | Choose Int String -- change the stripe form


-- TODO: Work in the optional opened and closed callbacks to get a full picture of the modal's state
update : Action -> Model -> (Model, Effects Action)
update address model =
    case address of
        NoOp ->
            noFx model

        Configure ->
            ( model
            , Signal.send configMailbox.address (model.key, model.locale)
                |> Task.map (always NoOp)
                |> Effects.task
            )

        Open ->
            ( model
            , Signal.send openMailbox.address model.info
                |> Task.map (always NoOp)
                |> Effects.task
            )

        TokenDispatch maybeToken ->
            case maybeToken of
                Just token ->
                        ( model
                        , postCharge (token, model.info.amount)
                        )
                Nothing -> -- TODO: Stripe error, might want a Result
                    noFx model

        RecieveFromServer maybeResult ->
            case maybeResult of
                Just result ->
                    noFx { model | confirmation <- Just (Ok result)}
                Nothing ->
                    noFx { model | confirmation <- Just (Err "HEY YOU FUCKED UP")}

        Close -> -- no need to actually call, modal closes after submit, iframe js already listening for esc
            ( model
            , Signal.send closeMailbox.address ()
                |> Task.map (always NoOp)
                |> Effects.task
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


postCharge : ChargeRequest -> Effects Action
postCharge chargeRequest =
    jsonPost decodeResponse "http://localhost:8081/charge" (encodeRequest chargeRequest)
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


encodeRequest : ChargeRequest -> Body
encodeRequest (token,amount) =
    Encode.object
        [ ("stripeToken",  Encode.string token)
        , ("amount", Encode.int amount)
        ]
        |> Encode.encode 4
        |> Http.string


decodeResponse : Decoder ChargeSuccess
decodeResponse =
    object1 ChargeSuccess
        ("chargeID" := string)


-- in order to make an (Int,Int) argument to view, I'd have to stop using start app and map it in manually
view : Signal.Address Action -> Model -> Html
view address model =
    div
    [ containerStyle ]
    [ setViewport
    , header [ topStyle ] [ text "header" ]
    , div [ midStyle ]
      [ div
        [ contentStyle ]
        [ selector address model.info.amount
        , checkoutButton address model.info.amount
        , confirmationBox model.confirmation
        ]
      ]
    , footer [ bottomStyle ] [text "footer"]
    ]


selector : Signal.Address Action -> Int -> Html
selector address amount =
    div [selectorStyle]
        [ div [] [text "How many lessons would you like to buy?"]
        , buttonRow address
        , div [] [text ( "Total: " ++ formatAmount amount )]
        ]


formatAmount : Int -> String
formatAmount amount =
    "$" ++ toString (toFloat amount / 100)


buttonRow : Signal.Address Action -> Html
buttonRow address =
        div [ buttonRowStyle ]
            [ button [onClick address (Choose 2500 "1 hour")] [text "1 hour"]
            , button [onClick address (Choose 4500 "2 hours")] [text "2 hours"]
            , button [onClick address (Choose 7000 "3 hours")] [text "3 hours"]
            ]


checkoutButton : Signal.Address Action -> Int -> Html
checkoutButton address amount =
    div [checkoutButtonStyle]
        [ button
            [ onClick address Open
            , disabled (amount <= 0)
            ]
            [text "Checkout"]
        ]


confirmationBox : Maybe (Result String ChargeSuccess) -> Html
confirmationBox maybeResult =
    let lines =
        case maybeResult of
            Just result ->
                case result of
                    Err msg ->
                        [text "Yikes! Your charge didn't go through, contact Nick asap"]
                    Ok response ->
                        [ p [] [text "You're all set!"]
                        , p [] [text ("Save your charge ID: " ++ response.chargeID)]
                        ]
            Nothing ->
                [text ""]
    in
        div [confirmationBoxStyle] lines




app =
    StartApp.start
        { init = init "pk_test_Y31x7Mqyi1iY63IQb95IAORm" "auto"
        , update = update
        , view = view
        , inputs = [incomingToken']
        }


configMailbox : Signal.Mailbox (PublishableKey, Locale)
configMailbox =
    Signal.mailbox ("", "")


openMailbox : Signal.Mailbox FormInfo
openMailbox =
    Signal.mailbox defaultInfo


closeMailbox : Signal.Mailbox ()
closeMailbox =
    Signal.mailbox ()


port configureStripe : Signal (PublishableKey, Locale)
port configureStripe = Signal.dropRepeats configMailbox.signal


port openStripe : Signal FormInfo
port openStripe = openMailbox.signal


port closeStripe : Signal ()
port closeStripe = closeMailbox.signal


incomingToken' : Signal Action
incomingToken' =
    let tokenState tok =
            if | tok == "" -> TokenDispatch Nothing
               | otherwise -> TokenDispatch (Just tok)
    in
        Signal.map tokenState incomingToken


port incomingToken : Signal Token


port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks


main : Signal Html
main = app.html
