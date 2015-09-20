module StripeCheckout (Model, init, Action, update, view) where
{-| A Stripe Checkout Button that plugs directly into the Elm Architecture

This was a good experiment

I've concluded that in the simple integration case, stripe's
complete control of the button is too prohibitive: Elm needs
a sense of the button's state at all times in order to intecept
the form submit.

Here's my new idea:

Build the model, view, and update around the custom integration.

Create actions for each part of the configure, open, close cycle

Have those actions control a conversation with checkout.js over ports

Have js send the token back to elm, who can then initiate an http post to the server

create any custom elm attributes to simplify the interactions.

|-}


import Html exposing (Html, node, form, Attribute)
import Html.Events exposing (onSubmit, onWithOptions, Options)
import Html.Attributes exposing ( attribute, action, method, enctype,src,class)
import Json.Decode exposing (value)
-- TODO what if the parent component recieves the token, then decides what to do with it?


-- update : Context -> Action -> Model -> Model
-- view : Context -> Model -> Html

-- a full model describing the state of the checkout form

{-| Foobar

|-}
type alias Model =
    { amount : Int
    , name : String
    , description : String
    , stripePublishableKey : String
    , chargeUrl : String
    , status : String
    }

{-| Foobar

|-}
init : Model
init =
    { amount = 0
    , name = "Bob's Burgers"
    , description = "Burger"
    , stripePublishableKey = "pk_test_Y31x7Mqyi1iY63IQb95IAORm"
    , chargeUrl = "http://localhost:8081/charge"
    , status = "not done"
    }

-- should represent the address in the parent after submit has completed
-- TODO: Options for redirect
--      - include a user-configurable redirect url and handle it in the server

{-| Foobar

|-}
type alias Context =
    { afterSubmit : Signal.Address Action
    }

-- TODO: Add more possible actions
type Action
    = NoOp
    | Completed String

{-| Foobar

|-}
update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        Completed str ->
            {model | status <- str}


{-| Foobar

|-}
view : Signal.Address Action -> Model -> Html
view address model =
    let options =
        { stopPropagation = True
        , preventDefault = True
        }
    in
    Html.div [] [Html.text model.status,
        form
            [ action model.chargeUrl
            , method "POST"
            , enctype "application/x-www-form-urlencoded"
            , onWithOptions "submit" options value
                (\_ -> Signal.message address (Completed "donezo"))
            , onSubmit address (NoOp)
            , attribute "onsubmit" "foo"
            ]

            [ script
                [ src "https://checkout.stripe.com/checkout.js"
                , class "stripe-button"
                , dataKey model.stripePublishableKey
                , dataName model.name
                , dataDescription model.description
                , dataAmount model.amount
                , dataLocale "auto"
                 --, dataImage .img/documentation/checkout/marketplace.png
                ]
                [
                 -- TODO add input fields acoording server requirements
                ]
            ]
        ]
dataKey : String -> Attribute
dataKey =
    attribute "data-key"


dataName : String -> Attribute
dataName =
    attribute "data-name"


dataDescription : String -> Attribute
dataDescription =
    attribute "data-description"


dataAmount : Int -> Attribute
dataAmount =
    toString >>
        attribute "data-amount"


dataLocale : String -> Attribute
dataLocale =
    attribute "data-locale"


script : List Attribute -> List Html -> Html
script =
    node "script"
