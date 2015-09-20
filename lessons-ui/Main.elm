import StartApp.Simple
import StripeCheckout as SC
import Html exposing (div, text, Html)


type alias Model =
    { title : String
    , checkoutButton : SC.Model
    }

init : Model
init =
    { title = "HOBOS"
    , checkoutButton = SC.init
    }

type Action
    = NoOp
    | FormStuff SC.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ text model.title
        , SC.view (Signal.forwardTo address FormStuff) model.checkoutButton
        ]

main =
    StartApp.Simple.start
        { model = init
        , update = update
        , view = view
        }
