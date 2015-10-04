var formInfo = {}; // for debugging purposes
var handler = null;
var tok = null;

window.addEventListener(
    'load',
    function () {
        // have to provide initial values for incoming (from elm's perspective) signals
        var app = Elm.fullscreen(Elm.Main, {incomingToken:null});

        app.ports.configureStripe.subscribe(configureStripeHandler);
        app.ports.openStripe.subscribe(openStripeHandler);
        app.ports.closeStripe.subscribe(closeStripeHandler);


        function configureStripeHandler (keyLocaleArr) {
            handler = StripeCheckout.configure({
                key: keyLocaleArr[0],
                locale: keyLocaleArr[1],
                token: function (token) {
                    tok = token;
                    app.ports.incomingToken.send({
                        tokenId: token.id,
                        email: token.email
                    });
                }
            });
        }


        function openStripeHandler(recievedInfo) {
            formInfo = recievedInfo;
            handler.open(formInfo);
        }


        function closeStripeHandler() {
            handler.close()
        }
    },
    false
);
