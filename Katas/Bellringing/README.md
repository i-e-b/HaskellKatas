The Bellringing Kata
====================
Background
----------
Alice, Bob, Carla, David, Ethel, Frank, Greta and Humbert belong to the bellringing society at St Edith’s church. They get twice a week to practise their change ringing on the eight bells in the church tower.
Today, they are practising a bellringing method (or pattern) called Plain Bob Major. This one is nice and easy, but it takes a bit of discipline to get it running really smoothly.
To start with, they ring the bells in sequence, Alice playing the highest bell (1), and Hubert playing the lowest (8):
```
1 2 3 4 5 6 7 8
```
Then, on an agreed signal, each of them swaps places in the sequence with the person next to them:
```
2 1 4 3 6 5 8 7
```
Next, the people in first and last places (Bob and Greta in this case) stay put, while the rest swap:
```
2 4 1 6 3 8 5 7
```
They repeat this pattern until they have done fifteen changes, each time playing a different sequence of bells:
```
4 2 6 1 8 3 7 5
4 6 2 8 1 7 3 5
6 4 8 2 7 1 5 3
6 8 4 7 2 5 1 3
8 6 7 4 5 2 3 1
8 7 6 5 4 3 2 1
7 8 5 6 3 4 1 2
7 5 8 3 6 1 4 2
5 7 3 8 1 6 2 4
5 3 7 1 8 2 6 4
3 5 1 7 2 8 4 6
3 1 5 2 7 4 8 6
1 3 2 5 4 7 6 8
```
At this point, continuing the sequence would get them, back to the beginning, so they throw in a new change, keeping the first two places the same (Alice and Carla), and changing everyone else:
```
1 3 5 2 7 4 8 6
```
At this point, they return to the beginning of the pattern, but of course with a different starting pattern.
After 122 changes, they are back to the starting pattern, and can start from the beginning again, or break for a well deserved cup of tea.
Here is a diagram of Plain Bob Major. The coloured lines trace the positions of various bells through the pattern, and the two little diagrams (BOB and SINGLE) show wildcards that can be thrown in to lengthen the performance:

The task
--------

Your task is to write a piece of code that outputs the sequence of bells played by the St Edith bellringers.

Starting out

I suggest you start with a method with this signature:
public IEnumerable<int> GetBells();

Also, as the sequence can repeat infinitely, you will want to implement this IEnumerable using yield.
Don’t feel constrained to keeping this signature if the code tells you otherwise, but it should give you a good starting point.
Key tests
It is probably a good idea to start by testing individual bells:
Assert.That(GetBells().ElementAt(0), Is.EqualTo(1));
Assert.That(GetBells().ElementAt(1), Is.EqualTo(2));

After a while you may choose to test an entire line in the sequence; Skip and Take will help you here:
Assert.That(GetBells().Skip(8).Take(8), Is.EqualTo(new[]{2, 1, 4, 3, 6, 5, 8, 7});

The lines worth particularly worth testing are:
0 = 1 2 3 4 5 6 7 8
1 = 2 1 4 3 6 5 8 7
2 = 2 4 1 6 3 8 5 7
15 = 1 3 2 5 4 7 6 8
16 = 1 3 5 2 7 4 8 6
112 = 1 2 3 4 5 6 7 8


The bellringing method in more detail
The method is summarised as 
X.18.X.18.X.18.X.18(12)(14)(1234)
The numbers here refer to position in the sequence, not the pitch of the bell.
X means that all bells change places: 1 and 2, 3 and 4, 5 and 6, 7 and 8.
In the other notations, the numbers indicate which positions stay put:
18 means that the first and last bell stay put, and the other bells change: 2 and 3, 4 and 5, 6 and 7.
12 means that the first and second bell stay put, and the other bells change: 3 and 4, 5 and 6, 7 and 8.
1234 means that the first four bells stay put, and the other bells change: 5 and 6, 7 and 8. 
You perform this method by reading the first bit (Xs and 18s) forwards and backwards, then doing 12 before going back to the first bit:
X.18.X.18.X.18.X.18.X.18.X.18.X.18.X.12
X.18.X.18.X.18.X.18.X.18.X.18.X.18.X.12
X.18.X.18.X.18.X.18.X.18.X.18.X.18.X.12
…
The other bracketed parts (12 and 1234) can be introduced to extend the method.

Extra tasks (not necessarily for the kata session)

Write an implementation that uses 14 instead of 12 for the 122nd change. How many changes are needed to get back to the beginning now?
Write an implementation that uses 1234 instead of 14 for the final change. How many changes are needed now?
Write an implementation that takes the method notation as a parameter.
Different methods can be performed on different numbers of bells. Write an implementation that takes the number of bells as a parameter.
Find a synthesiser that can be hooked up to your project, and a use it to do a performance.

Bonus
------
If you want to hear a Plain Bob Major performed, go here: 
http://www.youtube.com/watch?v=xbUsa9ICwqg
