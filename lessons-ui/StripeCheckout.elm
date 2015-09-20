module StripeCheckout (Model, init, Action, update, view) where
{-| A Stripe Checkout Button that plugs directly into the Elm Architecture

# Model
@docs Model, init

# Update
@docs Action, update

# View
@docs view

|-}


import Html exposing (Html, node, form, Attribute)
import Html.Events exposing (onSubmit)
import Html.Attributes exposing ( attribute, action, method, enctype,src,class)

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
    }

-- should represent the address in the parent after submit has completed

{-| Foobar

|-}
type alias Context =
    { afterSubmit : Signal.Address Action
    }

-- TODO: Add more possible actions
type Action
    = NoOp

{-| Foobar

|-}
update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model


{-| Foobar

|-}
view : Signal.Address Action -> Model -> Html
view address model =
    form
        [ action model.chargeUrl
        , method "POST"
        , enctype "application/x-www-form-urlencoded"
        , onSubmit address NoOp
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
             -- TODO add input fields acoording to model
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
