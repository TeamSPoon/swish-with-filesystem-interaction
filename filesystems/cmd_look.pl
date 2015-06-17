
in_pengines:- relative_frame(context_module,pengines,_).

/** <examples>

?- listing(in_pengines).

?- isa(X,tAgent).

?-  in_pengines.

?- foc_current_agent(ID).

?- foc_current_agent(ID),
  with_output_to(string(S),look_as(ID)).

*/


