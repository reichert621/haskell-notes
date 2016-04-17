### Notes

To test the functions in `notes.hs`:

```
# go to directory
$ cd path/to/haskell-notes

# start up GHCi
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help

# load notes.hs
Prelude> :l notes
[1 of 1] Compiling Main             ( notes.hs, interpreted )
Ok, modules loaded: Main.

# test quicksort function
*Main> quicksort [2,4,6,3,5,7,1,8]
[1,2,3,4,5,6,7,8]

```
