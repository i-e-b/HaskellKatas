The idea is to find all anagrams of an input.

"TinyDictionary.txt" is an 850-word basic English dictionary, in all lower case.

Anagrams are defined as a list of dictionary entries. Each entry can occur more than once.

Also, for each permutation, all matches should be found.
i.e.
[t,h,e,m,i,c,e] = [[the,mice],[them,ice]]



The naïve algorithm is very easy, but quickly becomes unusably slow, being on the order of n!
Next algorithm to try is the hash table (see [http://blog.lojic.com/2007/10/22/solving-anagrams-in-ruby/])