The idea is to find all anagrams of an input.

"TinyDictionary.txt" is an 850-word basic English dictionary, in all lower case.
There are a few extras at the bottom for testing. Why Hitler? Try "mother in law" as input...

Anagrams are defined as a list of dictionary entries. Each entry can occur more than once.

Also, for each permutation, all matches should be found.
i.e.
[t,h,e,m,i,c,e] = [[the,mice],[them,ice]]

The `main.hs` algorithm works reasonably well, even if it's a bit messy,
Example :
```
Main> main
Source sentence:
lll oo w d e h r
hello world
```

The naïve algorithm is very easy, but quickly becomes unusably slow, being on the order of n!

