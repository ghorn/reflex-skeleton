Reflex version
==============

It uses the reflex plateform.

Gtk: Build / Run
================

```
nix-build -A ghc.skeleton
./result/bin/run-skeleton
```

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

`RunSkeleton.hs` is modified from the GTK implementation; this file and the other Nix files are based on guibou's reflex branch.
