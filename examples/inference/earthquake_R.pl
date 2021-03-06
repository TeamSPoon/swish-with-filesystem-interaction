/*
This program models the occurrence of an earthquake depending on its possible 
causes.
From
F. Riguzzi and N. Di Mauro. Applying the information bottleneck to statistical 
relational learning. Machine Learning, 86(1):89-114, 2012
*/
:- use_module(library(pita)).
:- use_module(library(cplint_r)).

:- pita.

:- begin_lpad.

earthquake(X, strong) : 0.3 ; earthquake(X, moderate) : 0.5 :-
  fault_rupture(X).
% if an earthquake at a site X is caused only by the rupture of a 
% geological fault, we have a strong earthquake with probability 0.3, 
% a moderate earthquake with probability 0.5 and no earthquake with probability 
% 0.2

earthquake(X, strong) : 0.2 ; earthquake(X, moderate) : 0.6 :-
  volcanic_eruption(X).
% if an earthquake at a site X is caused only by a volcanic eruption, 
% we have a strong earthquake with probability 0.2, 
% a moderate earthquake with probability 0.6 and no earthquake with probability 
% 0.2


fault_rupture(stromboli).
% there is a fault rupture at stromboli

volcanic_eruption(stromboli).
% there is a volcanic eruption at stromboli

volcanic_eruption(eyjafjallajkull).
% there is a volcanic eruption at eyjafjallajkull

:- end_lpad.

/** <examples>

?- prob(earthquake(stromboli,strong),Prob).  % what is the probability of a strong 
% earthquake at stromboli?
% expected result 0.43999999999999995
?- prob(earthquake(stromboli,moderate),Prob).  % what is the probability of a moderate
% earthquake at stromboli?
% expected result 0.7999999999999998
?- prob(earthquake(eyjafjallajkull,strong),Prob).  % what is the probability of a strong 
% earthquake at eyjafjallajkull?
% expected result 0.2
?- prob(earthquake(eyjafjallajkull,moderate),Prob). % what is the probability of a moderate
% earthquake at eyjafjallajkull?
% expected result 0.6
?- prob_bar_r(earthquake(stromboli,strong)).  % what is the probability of a strong 
% earthquake at stromboli?
% expected result 0.43999999999999995
?- prob_bar_r(earthquake(stromboli,moderate)).  % what is the probability of a moderate
% earthquake at stromboli?
% expected result 0.7999999999999998
?- prob_bar_r(earthquake(eyjafjallajkull,strong)).  % what is the probability of a strong 
% earthquake at eyjafjallajkull?
% expected result 0.2
?- prob_bar_r(earthquake(eyjafjallajkull,moderate)). % what is the probability of a moderate
% earthquake at eyjafjallajkull?
% expected result 0.6


*/
