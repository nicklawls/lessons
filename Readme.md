# Lessons

A small single page application for managing marching bass drum lessons.

The frontend is an Elm application that talks to Stripe Checkout over a port in order to procure a transaction token. It then fires it off an HTTP POST to the server (written in Haskell with servant), which then executes the charge against Stripe's api and dumps a trello card in a personal trello list if the charge was successful.


## Build and Run

Assuming you have `elm` and `stack` installed, clone the repo and run `stack setup` to get the correct GHC.

The server requires the following environment variables to be set:

* `STRIPE_SECRET_KEY` is your test or live stripe key.
* `TRELLO_DEVELOPER_PUBLIC_KEY` and `TRELLO_MEMBER_TOKEN` are the key and token acquired from the usual Trello auth flow.
* `LESSONS_LIST_ID` is the `id` of the Trello list you want your lesson cards deposited in.
* `LESSONS_ENV` can be either `TEST` or `PROD`, it just decides which logger to use.
* `PORT` is the desired port.

Use `stack build` to build the `lessons` server executable. `stack exec lessons` will get it running on the specified port. Pick a port other than 8000.

For now, you'll have to manually change your stripe publishable key and the url of the server in `Main.elm`. I'm currently looking for a better way to configure this. Use `elm make ui/Main.elm --output ui/app.js` to build the ui, and run `elm reactor` to start a dev server on localhost 8000. Use the reactor's interface to navigate into ui and open up `index.html` to view the app.
