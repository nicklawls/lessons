var app = Elm.fullscreen(Elm.StripeCheckout, {incomingToken:["",0]}); // have to provide initial values for incoming (from elm's perspective) signals

app.ports.configureStripe.subscribe(configureStripeHandler);
app.ports.openStripe.subscribe(openStripeHandler);
app.ports.closeStripe.subscribe(closeStripeHandler);

var formState = {};
var handler = null;


function configureStripeHandler (recievedKey) {
    handler = StripeCheckout.configure({
        key: recievedKey,
        token: function (token) {
            app.ports.incomingToken.send([token.id,formState.amount]);
        }
    });
}


function openStripeHandler(recievedInfo) {
    formState = recievedInfo;
    handler.open(formState);
}

function closeStripeHandler() {
    handler.close()
}
