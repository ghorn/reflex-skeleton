Reflex version
==============

It uses the reflex plateform.

Gtk: Build / Run
================

```
nix-build -A ghc.Reflex
./result/bin/run-skeleton
```

(Note, this uses Webkit GTK 2, so it may sucks on high DPI setting).

Warp: Build / Run
=================

Modify `default.nix` and set `useWarp = true`. Then:

```
nix-build -A ghc.Reflex
./result/bin/run-skeleton
```

Then open your browser on `http://localhost:3003`.


Hacking
=======

```
nix-shell -A shells.ghc
cabal new-repl
```

And then run `main`. Depending on your `useWarp` setting, open your browser or directly use the desktop apps.

Note: you may need to remove the `ghc.environment` file which breaks everything.


Details
=======

`RunSkeleton.hs` is copy/pasted from the GTK implementation.
