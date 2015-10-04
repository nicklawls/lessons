module Main where

import StartApp
import Html exposing (Html, text, p, div, button, header, footer, h1, h2, h3, h4, input)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (style, hidden, disabled, placeholder, value)
import Json.Decode exposing (Decoder, string, object1, (:=))
import Json.Encode as Encode
import Effects exposing (Effects)
import Task exposing (Task, andThen, succeed)
import Http exposing (Body, Error, defaultSettings, fromJson)
import Style exposing (..)
import Css exposing (setViewport)

{-| Refactoring plan

1. Substitue FormState with a Maybe String and a Maybe ChargeSuccess, update the view acoordingly
2.


|-}

type alias Model =
    { key : PublishableKey
    , locale : Locale
    , user : UserInfo
    , info : FormInfo
    , state : FormState
    }


type alias UserInfo = (Name,Email)
type alias Name = String
type alias Email = String
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


-- custom type to define the possible form states, more understandable than Maybe (Result String ChargeSuccess)
type FormState
    = Ready
    | Waiting
    | Failed String
    | Succeeded ChargeSuccess


-- TODO: Send the full token structure
type alias Token = String


type alias ChargeRequest =
    { stripeToken : Token
    , amount : Int
    , name : Name
    , email : Email
    }


type alias ChargeSuccess =
    { chargeID : String
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


init : PublishableKey -> Locale -> (Model, Effects Action)
init key locale =
    ( { key = key
      , locale = locale
      , info = defaultInfo
      , user = ("","")
      , state = Ready
      }
    , Effects.task (succeed Configure)
    )


type Action
    = NoOp -- nothing
    | Configure -- supply necessary checkout params to stripe
    | Open -- copy the optional params and open the checkout form
    | TokenDispatch (Maybe Token) -- send the token to wherever it needs to go
    | Confirm (Maybe ChargeSuccess) -- recive comfirmation of charge
    | Close -- close the modal
    | NewName Name
    | NewEmail Email
    | Choose Int String -- change the stripe form


noFx : model -> (model, Effects a)
noFx model =
    (model, Effects.none)


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
                    let (name,email) = model.user
                    in  ( { model | state <- Waiting }
                        , postCharge (ChargeRequest token model.info.amount name email)
                        )
                Nothing -> -- Stripe has an error for bad connectivity while modal is open, so will only hit if connection goes down between hitting Pay and js sending the token back
                    noFx { model | state <- Failed "Looks like Stripe is having some issues, try again later" }

        Confirm maybeResult ->
            case maybeResult of
                Just result ->
                    noFx { model | state <- Succeeded result }
                Nothing ->
                    noFx { model | state <- Failed "There's been an error talking to the server, tell Nick ASAP!" } -- TODO: maybe add action and effect to return to ready state after timeout

        Close -> -- no need to actually call, stripe's iframe already listening for esc and clicks
            ( model
            , Signal.send closeMailbox.address ()
                |> Task.map (always NoOp)
                |> Effects.task
            )

        NewName name ->
            noFx { model | user <- (name, snd model.user) }

        NewEmail email ->
            noFx { model | user <- (fst model.user, email) }

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
    jsonPost decodeResponse "https://lessonsapi.herokuapp.com/charge" (encodeRequest chargeRequest)
        |> Task.toMaybe
        |> Task.map Confirm
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
encodeRequest req =
    Encode.object
        [ ("stripeToken",  Encode.string req.stripeToken)
        , ("amount", Encode.int req.amount)
        , ("name", Encode.string req.name)
        , ("email", Encode.string req.email)
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
    let content =
        case model.state of
            Ready ->
                [ userInput address model.user
                , selector address
                , checkoutButton address model.info.amount model.user
                ]
            Waiting ->
                [ div [messageStyle] [text "Please Wait..."] ]
            Failed error ->
                [ div [messageStyle] [text error] ] -- TODO: maybe do nothing here

            Succeeded chargeSuccess ->
                [ confirmationBox chargeSuccess ]
    in
        div [ containerStyle ]
            [ setViewport
            , header [ topStyle ] [ h1 [] [text "Pay For Lessons With Nick"] ]
            , div [ contentStyle ] content
            , footer [ bottomStyle ] [text "© Nick Lawler 2015" ] -- TODO: get current year
            ]


userInput : Signal.Address Action -> UserInfo -> Html
userInput address (name,email) =
    div [userInputStyle]
        [ h2 [] [text "Who are You?"]
        , input
            [ placeholder "Name"
            , value name
            , on "input" targetValue (Signal.message address << NewName)
            , inputStyle
            ]
            []
        , input
            [ placeholder "Email"
            , value email
            , on "input" targetValue (Signal.message address << NewEmail)
            , inputStyle
            ]
            []
        ]

selector : Signal.Address Action -> Html
selector address =
    div [selectorStyle]
        [ h2 [decreaseMargin] [text "How many Lessons?"]
        , h4 [] [text "Each lesson is an hour long, $/hour decreases with more lessons"]
        , buttonRow address
        ]


buttonRow : Signal.Address Action -> Html
buttonRow address =
        div [ buttonRowStyle ]
            [ button [onClick address (Choose 2500 "1 Hour-Long Lesson"), buttonStyle] [text "1"]
            , button [onClick address (Choose 4500 "2 Hours of Lessons"), buttonStyle] [text "2"]
            , button [onClick address (Choose 7000 "3 Hours of Lessons"), buttonStyle] [text "3"]
            ]

-- TODO: Replace ad-hoc user check with actual validation
checkoutButton : Signal.Address Action -> Int -> UserInfo -> Html
checkoutButton address amount (name,email) =
    div [checkoutButtonStyle]
        [ h2 [] [text ( formatAmount amount )]
        , button
            [ onClick address Open
            , disabled (amount <= 0 || name == "" || email == "")
            ]
            [text "Pay with Card"]
        ]


formatAmount : Int -> String
formatAmount amount =
    "$" ++ toString (toFloat amount / 100)


confirmationBox : ChargeSuccess -> Html
confirmationBox chargeSuccess =
    div [confirmationBoxStyle]
        [ h4 [] [text "You're all set! If we haven't set up dates and times yet, I'll be in touch soon"]
        , h4 [] [text ("Might want to save this ID: " ++ chargeSuccess.chargeID)]
        ]

app : StartApp.App Model
app =
    StartApp.start
        { init = init "pk_live_KtyvCapyvK7xuuwf6C5MpoOr" "auto"
        , update = update
        , view = view
        , inputs = [incomingToken']
        }


configMailbox : Signal.Mailbox (PublishableKey, Locale)
configMailbox =
    Signal.mailbox ("","")


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
