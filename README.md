# GHCJS Starter Project

See [LICENSE](./LICENSE) for terms of use.

Made by [Chris Allen](http://bitemyapp.com) ([Twitter](https://twitter.com/bitemyapp)), author of the book [Haskell Programming from first principles](http://haskellbook.com).

This is intended to be a _bare_ starter project. If I make new ones with examples/bindings to other frameworks, I'll make other git respositories and link them here.

## Getting started

### Install GHCJS

```
$ cd ./frontend && stack setup
```

This will build GHCJS from scratch and boot the basic GHCJS libraries, this _will_ take some time.

### Compile and symlink assets

```
$ make frontend
```

To auto-rebuild frontend assets, use `frontend-watch`:

```
$ make frontend-watch
```

### GHCJSi

Keep in mind `GHCJSi` hasn't had quite as much time to get a shake-out as `GHCi` so if you run into any issues, _please_ report it to [the GHCJS project](https://github.com/ghcjs/ghcjs)!

First, go to frontend directory.

```
$ cd frontend
```

Then, launch REPL.

```
$ stack ghci
```

If the version of stack is below 1.2.0, it's possible that the above command doesn't work.
If the command above didn't work, you could try the following command.

```
$ stack exec -- ghcjs --interactive
```

You should see a warning about:

```
socket.io not found, browser session not available
```

Install socket.io with Node.js:

```
$ npm install socket.io
```

Then, export `NODE_PATH` permanently for the current shell session, so GHCJS can find your node packages.

```
export NODE_PATH=$(pwd)/node_modules
```

Now, starting GHCJS should be free of the socket.io warning:

```
$ stack ghci
GHCJSi, version 0.2.0-7.10.3: http://www.github.com/ghcjs/ghcjs/  :? for help
Prelude>
```

Alternatively, instead of exporting `NODE_PATH`, you could execute the following command

```
$ NODE_PATH=./node_modules stack ghci
```

If you didn't want to care about installing socket.io and setting `NODE_PATH`, you could just execute the following command in the directory that contains `Makefile`.

```
$ make ghcjsi
```

Now, connect your browser to the exposed web server at `localhost:6400`, and load the `src` dir to see it in action. There'll be a slight delay the first time you `GHCJSi` to evaluate something, but then it should work fine:

```
Prelude> :l src/Lib.hs
[1 of 1] Compiling Lib              ( src/Lib.hs, interpreted )
Ok, modules loaded: Lib.
*Lib> :set -XOverloadedStrings
*Lib> console_log "blah"
*Lib>
```

If you check your browser's inspector/JS console, you should see "blah" in the log.

### Build/run backend

For building the web app binary:

```
make backend
```

Lately I use `app/DevelMain.hs` and `update` to fire up and reload the web app server, but you can try `yesod devel` to auto-rerun the app:

```
cd backend && stack install yesod-bin && stack exec -- yesod devel
```
