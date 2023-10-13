:- abolish(location/2).
:- abolish(count/2).
:- abolish(torch_location/1).
:- abolish(travel_time/2).
:- abolish(counter/1).
:- abolish(touristNo/1).
:- abolish(touristNoPr/1).
:- abolish(goal/1).

:-style_check(-singleton).

:- dynamic(location/2).
:- dynamic(count/2).
:- dynamic(torch_location/1).
:- dynamic(travel_time/2).
:- dynamic(counter/1).
:- dynamic(touristNo/1).
:- dynamic(touristNoPr/1).
:- dynamic(goal/1).

counter(0).


suma(X, Y, Z) :- Z is X + Y.
suba(X, Y, Z) :- Z is X - Y.


fix_init_count(Dir) :- 
	count(Dir, D),
	retractall(count(Dir, _)),
	suma(D, 1, Piz),
	assertz(count(Dir, Piz)).

add_torch :- 
    write('Location of the torch (up/down): '),
    read(X), count(X, N), N>0, 
    assertz(torch_location(X)),!;
    write('Not a valid location!\n'), add_torch.

reduce_tour_count :- touristNo(X), 
    retractall(touristNo(X)),
    suba(X,1,S), assertz(touristNo(S)).

add_tourist(No, Location, Time) :- 
    assertz(location(No,Location)),
    assertz(travel_time(No,Time)).
    

add_tourists :- touristNo(X), X = 0.
add_tourists :-
    touristNo(X), X =\= 0,
    touristNoPr(Y),
    suba(Y,X,Z), suma(Z,1,W),
    write('Location of tourist number '),
    write(W),
    write(': '),read(Location),
    write('Time needed for this tourist '),
    write('to pass the river: '),
    read(Time),
    add_tourist(X,Location,Time),
    fix_init_count(Location),
    reduce_tour_count, add_tourists.

insTour :-
    retractall(location(X,_)),
    retractall(travel_time(X,_)),
    retractall(touristNo(X)),
    retractall(touristNoPr(X)),
    retractall(torch_location(X)),
    retractall(count(X,_)),
    retractall(goal(X)),

    write('Total number of tourists: '),
    read(TourNo), integer(TourNo), TourNo>0,

    assertz(count(up,0)),
    assertz(count(down,0)),
    assertz(touristNo(TourNo)),
    assertz(touristNoPr(TourNo)),
    add_tourists, add_torch, 

    write('Side of the river that all tourists must '),
    write('end up to (up/down): '), 
    read(Goal), (Goal=up;Goal=down), assertz(goal(Goal)),

    pr, write('To move type \'move.\'\n'),
    write('To print current state type \'pr.\'\n'),
    !;
    write('Invalid Input!\n'),insTour.


prints(S, X) :- X=up, not(location(S,X)),!.
prints(S, X) :- X=up, location(S,X), travel_time(S,Y), 
    tab(3), write(Y),!.
prints(S, X) :- X=down, not(location(S,X)),!.
prints(S, X) :- X=down, location(S,X), travel_time(S,Y), 
    tab(3), write(Y).

printp(X) :- X=up, torch_location(up), 
    tab(3), write('Torch'),!.
printp(X) :- X=up, torch_location(down),!.			 
printp(X) :- X=down, torch_location(down), 
    tab(3), write('Torch'),!.
printp(X) :- X=down, torch_location(up),!.


printUpDown(X, D) :- touristNoPr(N), X>N, !.
printUpDown(X, D) :- location(X, D),prints(X,D),
    suma(X,1,S),printUpDown(S, D),!;
    suma(X,1,S),printUpDown(S, D).


pr :- nl, nl,
    printUpDown(1, up),
    printp(up),
    nl,
    write('---------------|  |-------------------------'),nl,
    write('               |  |                         '),nl,
    write('---------------|  |-------------------------'),nl,
    printUpDown(1, down),
    printp(down), nl, nl,
    write('Total time: '), counter(W), write(W),
    write('.'),
    nl,nl,nl,!.


equals_up_down(X,R) :- X=up,R=1,!;X=down,R=1,!;R=0.

allow_movement(Num, Dir, Out) :- 

    (integer(Num),
    (Dir=up,
        torch_location(down),
        count(down,DownNo),
        (Num=1,count(up,C),C=\=0;Num=\=1),
        DownNo - Num =\= 1,
        DownNo - Num >= 0;
    Dir=down, 
        torch_location(up),
        count(up,UpNo),
        (Num=1,count(down,C),C=\=0;Num=\=1),
        UpNo - Num =\= 1,
        UpNo - Num >= 0
    ),
    (Num = 1; Num = 2; Num = 3), Out=1,!);

    not(integer(Num)), 
        write('Invalid number of tourists!'),Out=0, !;
    Num < 1, write('Invalid number of tourists!'),Out=0,!;
    Num > 3, 
    write('Too many tourists! The bridge might '),
        write('collapse!'),Out=0, !;

    equals_up_down(Dir,R), R=0, write('Not a valid '), 
        write('direction!'),Out=0,!;

    Dir=up,Num=1,count(up,C),C=0, 
    write('Cant leave one person alone! '), 
        write('Too dangerous!'),Out=0, !;
    Dir=down,Num=1,count(down,C),C=0, 
    write('Cant leave one person alone! '),
        write('Too dangerous!'),Out=0, !;

    Dir=up,count(down,C),suba(C, Num, E), E < 0, 
    write('Not enough tourists for the transit '),
        write('to happen!'),Out=0, !;
    Dir=down,count(up,C),suba(C, Num, E), E < 0, 
    write('Not enough tourists for the transit '),
        write('to happen!'),Out=0, !;

    Dir=up,torch_location(up),
    write('Cant move in the dark...!'),Out=0, !;
    Dir=down,torch_location(down),
    write('Cant move in the dark...!'),Out=0, !;
    Dir=up,count(down,C),suba(C, Num, E), E = 1, 
    write('Cant leave one person alone! '), 
        write('Too dangerous!'),Out=0, !;
    Dir=down,count(up,C),suba(C, Num, E), E = 1, 
    write('Cant leave one person alone! '),
        write('Too dangerous!'),Out=0, !.


fix_count(Num, Dir) :- 
    count(up, Up),
    count(down, Down),
    retractall(count(up, _)),
    retractall(count(down, _)),
    (Dir=up,
		
        suma(Up, Num, Piz),
        suba(Down, Num, Pizz)
	;
    Dir=down,
        suba(Up, Num, Piz),
        suma(Down, Num, Pizz)
    ),
    assertz(count(up, Piz)),
    assertz(count(down, Pizz)).


move_torch(Dir) :- 
    retractall(torch_location(_)),
    assertz(torch_location(Dir)).


other_side(X, R) :- X=up,R=down,!; X=down,R=up,!.

max(A,B,C) :- A>=B, C=A,!; A<B, C=B.

move_one_person(Dir, F) :-
    read(F),travel_time(X, F), other_side(Dir, DirR),
        location(X,DirR), 
        retractall(location(X, DirR)),
        assertz(location(X, Dir)),!;
    write('Tourist does not exists :('),nl,
        write('Try Again: '),move_one_person(Dir,F).

update_time(T) :- counter(X), retractall(counter(X)),
    suma(X,T,S), assertz(counter(S)).

move_da_people(Num,Dir) :-
    Num=1, write('Time of the tourist: '),
        move_one_person(Dir, T), nl,
        write('->Transition time: '), write(T), nl,
        update_time(T),!;
	
    Num=2, write('Time of the first tourist: '),
        move_one_person(Dir, T1),
        write('Time of the second tourist: '),
        move_one_person(Dir, T2),
        max(T1,T2,T), nl,
        write('->Transition time: '), write(T), nl,
        update_time(T),!;

    Num=3, write('Time of the first tourist: '),
        move_one_person(Dir, T1),
        write('Time of the second tourist: '),
        move_one_person(Dir, T2),
        write('Time of the third tourist: '),
        move_one_person(Dir, T3),
        max(T1,T2,T12), max(T12,T3,T), nl,
        write('->Transition time: '), write(T), nl,
        update_time(T),!.

finalState :- goal(Goal), count(Goal, X), touristNoPr(N), 
    X=N, write('\nGOAL STATE ACHIEVED!!!\n'),
    write('All tourists are in the corrent '),
    write('side of the river!!!\n'),!;
    goal(Goal), count(Goal, X),!.

move :-
    write('Number of tourists to travel (1-3): '), 
    read(Tr_count),
    write('Travel to direction (up/down): '), read(Dir),
	
    allow_movement(Tr_count, Dir, Out), 
    Out=1,
        fix_count(Tr_count, Dir),
        move_torch(Dir),
        move_da_people(Tr_count,Dir),pr,finalState,!;
    Out=0,pr,!.



:-initialization(insTour).