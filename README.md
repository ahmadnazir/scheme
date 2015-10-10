# Write yourself a Scheme in 48 hours #

Exercises for the tutorial:
[Write yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). I
think it is going to take me something like 48 days to go through it
all

# Setup #

cabal sandbox init
cabal install parsec

# Run #

## Repl ##

cabal repl

## Compile ##

```
	cabal exec ghc Main # seems like I don't need --make switch
	./Main 'input string'
```

## Quick compile and Run ##

```
	cabal exec ghc Main && ./Main `more samples/test.txt`
```

