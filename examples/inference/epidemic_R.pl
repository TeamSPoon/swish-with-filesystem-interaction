/*
Model of the development of an epidemic or a pandemic.
From 
E. Bellodi and F. Riguzzi. Expectation Maximization over binary decision 
diagrams for probabilistic logic programs. Intelligent Data Analysis, 
17(2):343-363, 2013.
*/
:- use_module(library(pita)).
:- use_module(library(cplint_r)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

epidemic : 0.6; pandemic : 0.3 :- flu(_), cold.
% if somebody has the flu and the climate is cold, there is the possibility 
% that an epidemic arises with probability 0.6 and the possibility that a
% pandemic arises with probability 0.3

cold : 0.7.
% it is cold with probability 0.7

flu(david).
flu(robert).
% david and robert have the flu for sure

:- end_lpad.

/** <examples>

?- prob(epidemic,Prob).  % what is the probability that an epidemic arises?
% expected result 0.588
?- prob(pandemic,Prob).  % what is the probability that a pandemic arises?
% expected result 0.357
?- prob_bar_r(epidemic).  % what is the probability that an epidemic arises?
% expected result 0.588
?- prob_bar_r(pandemic).  % what is the probability that a pandemic arises?
% expected result 0.357
?- bdd_dot_string(epidemic,BDD,Var).

?- bdd_dot_string(pandemic,BDD,Var).


*/
