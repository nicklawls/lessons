import StartApp
import StripeCheckout as SC
import Html exposing (div, text, Html)
import Effects


type alias Model =
    { checkout : SC.Model
    }


init : Model
init =
    { checkout = SC.init "pk_test_Y31x7Mqyi1iY63IQb95IAORm"
    }


type Action
    = NoOp
    | FormStuff SC.Action


update : Action -> Model -> (Model, Effects SC.Action)
update action model =
    case action of
        NoOp ->
            (model, Effects.none)
        FormStuff action ->
            SC.update action model.checkout


view : Signal.Address Action -> Model -> Html
view address model =
    SC.view (Signal.forwardTo address FormStuff) model.checkout


main =
    StartApp.Simple.start
        { model = init
        , update = update
        , view = view
        }
