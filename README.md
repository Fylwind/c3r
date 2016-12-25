Get the keys
------------

First of all, [see if you have any apps already](https://apps.twitter.com).

If you don't, then [create one](https://apps.twitter.com/app/new).
Fill the form with whatever you want.  You can leave *Callback URL* blank.

If you need write access (i.e. to post tweets, fave tweets, etc),
be sure to enable that in the Permissions tab of the app configuration.

Once you have an app, you can obtain the keys from the
*Keys and Access Tokens* tab of the app configuration, located at
`https://apps.twitter.com/app/<id>/keys`

Open `Key.hs.EXAMPLE` in a text editor and enter the keys.
Save the file, but without the `.EXAMPLE` suffix.

Build the program
-----------------

Create a sandbox:

    cabal sandbox init
    cabal sandbox add-source calico

Make sure dependencies are installed:

    cabal install --dependencies-only

To compile the program:

    cabal build

Run the program
---------------

    C3R_DEBUG=t C3R_LOG=t cabal run

Strip the environment variables when live.
