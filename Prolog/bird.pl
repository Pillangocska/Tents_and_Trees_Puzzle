% Specific birds
bird(penguin, peggy).  % Peggy is a penguin.
bird(sparrow, sammy).  % Sammy is a sparrow.

% Penguins in general
penguin(X) :- bird(penguin, X).

% Flies rule
flies(X) :- bird(_, X), \+ penguin(X).

/* TESTS
| ?- flies(penguin).
no
| ?- flies(sammy). 
yes
| ?- flies(peggy). 
no
| ?- penguin(peggy).
yes
| ?- penguin(sammy). 
no
*/