window.addEventListener(
    'load',
    function () {
        var app = Elm.fullscreen(Elm.Main, {incomingToken:""}); // have to provide initial values for incoming (from elm's perspective) signals

        app.ports.configureStripe.subscribe(configureStripeHandler);
        app.ports.openStripe.subscribe(openStripeHandler);
        app.ports.closeStripe.subscribe(closeStripeHandler);

        var formState = {};
        var handler = null;


        function configureStripeHandler (keyLocaleArr) {
            handler = StripeCheckout.configure({
                key: keyLocaleArr[0],
                locale: keyLocaleArr[1],
                token: function (token) {
                    app.ports.incomingToken.send(token.id);
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
    },
    false
);
