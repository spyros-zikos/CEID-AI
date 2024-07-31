PROJECT TITLE: The 7 tourists and one flashlight

PURPOSE OF PROJECT: Make a program in prolog that will model the movement of 7 tourists from one side of the river to the other taking under account the constraints that are imposed by the problem.

DATE: Jan 2023

AUTHORS: Spyros


USER INSTRUCTIONS:
You can see the demo for the execution of the program at pages 9-15 in the documentation.
To run it yourself go to the directory of the "tourists.pl" file, open command line and type:
-> swipl
-> consult('tourists.pl')
Then enter the initial state of the problem:
-> 7.
-> up.
-> 12.
-> up.
-> 9.
-> up.
-> 8.
-> up.
-> 9.
-> up.
-> 10.
-> down.
-> 15.
-> down.
-> 11.
-> down.
-> down.
Then execute commands to move tourists from one shore of the river to another.


The following are some inputs given to the program in order to inialize it and reach the end successfully.
----------
7.
up.
12.
up.
9.
up.
8.
up.
9.
up.
10.
down.
15.
down.
11.
down.
down.
move.
2.
up.
11.
15.
move.
3.
down.
15.
12.
8.
move.
1.
up.
8.
move.
3.
down.
8.
10.
11.
move.
1.
up.
8.
move.
3.
down.
8.
9.
9.
----------



Note:
swipl version: 9.0.3, threaded, 64 bits