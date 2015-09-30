# Lessons

A small single page application for managing marching bass drum lessons.

The frontend is an Elm application that talks to Stripe Checkout over a port in order to procure a transaction token. It then fires it off an HTTP POST to the server (written with servant), which then executes the charge against Stripe's api and dumps a trello card in a personal trello list if the charge was successful.


## Build and run yourself in 4 easy steps

Use `stack install` to build and install the `lessons` server executable. `stack exec lessons` will get it running on port 443.

Use `elm make ui/Main.elm --output dist/app.js` to build the ui, and run `elm reactor` to start a dev server on localhost 8000. Use the reactor's interface to navigate into ui and open up `index.html` to view the app.
