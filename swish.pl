/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_app,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(settings)).

:- if(exists_source(rdfql(sparql_csv_result))).
:- use_module(rdfql(sparql_csv_result)).
:- endif.

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(swish, Dir)).

:- use_module(lib/messages).
:- use_module(lib/paths).
:- use_module(lib/config, []).
:- use_module(lib/page, []).
:- use_module(lib/storage).
:- use_module(lib/include).
:- use_module(lib/swish_csv).
:- use_module(lib/examples).
:- use_module(lib/profiles).
:- use_module(lib/filesystems).
:- use_module(lib/highlight).
:- use_module(lib/markdown).
:- use_module(lib/chat, []).
:- use_module(lib/template_hint, []).
:- use_module(lib/tutorial).
:- use_module(library(aleph)).
:- use_module(library(sldnfdraw)).

		 /*******************************
		 *	       CORS		*
		 *******************************/

% By default, enable CORS

:- set_setting_default(http:cors, [*]).


		 /*******************************
		 *         LOCAL CONFIG		*
		 *******************************/

%!	load_config
%
%	Load files from config-enabled if  present. Currently loads from
%	a single config-enabled directory, either  found locally or from
%	the swish directory.

load_config :-
	absolute_file_name(config_enabled(.), Path,
			   [ file_type(directory),
			     access(read),
			     file_errors(fail)
			   ]), !,
	atom_concat(Path, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	maplist(ensure_loaded, Files).
load_config.

:- initialization(load_config, now).


		 /*******************************
		 *	      CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,
	swish_config:source_alias/2.

%%	swish_config:config(?Config, ?Value) is nondet.
%
%	All solutions of this predicate are  available in the JavaScript
%	object config.swish.config. Config must be an  atom that is also
%	a valid JavaScript identifier. Value  must   be  a value that is
%	valid for json_write_dict/2. Most configurations  are also saved
%	in the application preferences. These   are  marked [P]. Defined
%	config parameters:
%
%	  - show_beware
%	  [P] If `true`, show the *Beware* modal dialog on startup
%	  - tabled_results
%	  [P] If `true`, check the _table results_ checkbox by default.
%	  - application
%	  Name of the Pengine application.
%	  - csv_formats
%	  [P] CSV output formats offered. For example, ClioPatria
%	  defines this as [rdf,prolog]. The first element is default.
%	  - community_examples
%	  Allow marking saved programs as example.  If marked, the
%	  programs are added to the Examples menu.
%	  - public_access
%	  If lib/authenticate.pl is loaded and this flag is `true`,
%	  _all_ access to SWISH demands authentication.  If false,
%	  only running queries and saving files is restricted. Note
%	  that this flag has no effect if no authentication module is
%	  loaded.
%	  - include_alias
%	  Alias for searching files for `:- include(Alias(Name)).`
%	  - ping
%	  Ping pengine status every N seconds.  Updates sparkline
%	  chart with stack usage.
%	  - notebook
%	  Dict holding options for notebooks.
%        - nb_eval_script
%        Evaluate scripts in HTML cells of notebooks?
%	  - chat
%	  Activate the chat interface

% Allow other code to overrule the defaults from this file.
term_expansion(swish_config:config(Config, _Value), []) :-
	clause(swish_config:config(Config, _), _).


swish_config:config(show_beware,        false).
swish_config:config(tabled_results,     false).
swish_config:config(application,        swish).
swish_config:config(csv_formats,    [rdf, prolog]).
% swish_config:config(csv_formats,        [prolog]).

% Allows users to extend the Examples menu by ticking the Example
% checkbox.
:- if(\+ exists_source(cliopatria(hooks))).
swish_config:config(community_examples, true).
:- endif.
swish_config:config(public_access,      false).
swish_config:config(include_alias,	example).
swish_config:config(ping,		10).
swish_config:config(notebook,		_{eval_script: true}).
swish_config:config(chat,		true).

%%	swish_config:source_alias(Alias, Options) is nondet.
%
%	Specify access for files below a given _alias_. Options define
%
%	  - access(Access)
%	  One of `read` or `both`.  Default is `read`.
%	  - if(Condition)
%	  Provide additional conditions.  Defined conditions are:
%	    - loaded
%	    Only provide access to the file if it is loaded.


% setup HTTP session management
:- use_module(lib/session).

		 /*******************************
		 *	        CSV		*
		 *******************************/

:- multifile
	swish_csv:write_answers/2,
	swish_csv:write_answers/3.

swish_csv:write_answers(Answers, VarTerm) :-
        Answers = [H|_],
        functor(H, rdf, _), !,
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            []).

swish_csv:write_answers(Answers, VarTerm, Options) :-
        Answers = [H|_],
        functor(H, rdf, _),
	option(page(1), Options), !,
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            [ bnode_state(_-BNodes)
	    ]),
	nb_setval(rdf_csv_bnodes, BNodes).
swish_csv:write_answers(Answers, VarTerm, Options) :-
        Answers = [H|_],
        functor(H, rdf, _),
	option(page(Page), Options),
	Page > 1, !,
	nb_getval(rdf_csv_bnodes, BNodes0),
        sparql_write_csv_result(
            current_output,
            select(VarTerm, Answers),
            [ http_header(false),
	      header_row(false),
	      bnode_state(BNodes0-BNodes)
	    ]),
	nb_setval(rdf_csv_bnodes, BNodes).
swish_csv:write_answers(Answers, VarTerm, _Options) :-
	swish_csv:write_answers(Answers, VarTerm).


                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- multifile
	pengines:prepare_module/3.

:- pengine_application(swish).
:- use_module(swish:lib/render).
:- use_module(swish:lib/trace).
:- use_module(swish:lib/projection).
:- use_module(swish:lib/jquery).
:- use_module(swish:lib/dashboard).
:- use_module(swish:lib/swish_debug).
:- use_module(swish:library(pengines_io)).
:- use_module(swish:library(semweb/rdf_db)).
:- use_module(swish:library(semweb/rdfs)).
% :- use_module(swish:library(semweb/rdf_optimise)).
:- use_module(swish:library(semweb/rdf_litindex)).
:- use_module(swish:library(solution_sequences)).
:- use_module(swish:library(aggregate)).
:- use_module(swish:lib/r_swish).
:- if(exists_source(library(tabling))).
:- use_module(swish:library(tabling)).
:- endif.

pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).
%:- set_setting(swish:time_limit, 3600).
% Additional sandboxing rules.
:- use_module(lib/flags).
:- use_module(lib/logging).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).
:- if(exists_source(library(semweb/rdf11))).
:- use_module(library(semweb/rdf11), []).
:- endif.
:- use_module(lib/swish_chr, []).

% load rendering modules

:- use_module(swish(lib/render/html),	  []).
:- use_module(swish(lib/render/sudoku),	  []).
:- use_module(swish(lib/render/chess),	  []).
:- use_module(swish(lib/render/table),	  []).
:- use_module(swish(lib/render/codes),	  []).
:- use_module(swish(lib/render/svgtree),  []).
:- use_module(swish(lib/render/graphviz), []).
:- use_module(swish(lib/render/c3),	  []).
:- use_module(swish(lib/render/url),	  []).
:- use_module(swish(lib/render/lpad),	  []).
:- use_module(swish(lib/render/prolog),	  []).
:- use_module(swish(lib/render/tiles),	  []).
:- use_module(swish(lib/render/sldnf),	  []).
:- use_module(library(r/r_sandbox)).

:- use_module(library(pita)).
:- use_module(library(mcintyre)).
:- use_module(library(slipcover)).
:- use_module(library(lemur),[]).
:- use_module(library(auc)).
:- use_module(library(matrix)).
:- use_module(library(clpr)).
:- use_module(library(cplint_r)).
:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(nf_r:{_}).


:- use_module(swish(lib/render/bdd),	  []).


:- if(exists_source(library(trill))).
% :- use_module(library(trill)).
:- endif.

:- if(exists_source(library(must_trace))).
%:- use_module(library(must_trace)).
:- endif.
:- if(exists_source(library(pfc))).
%:- use_module(library(pfc)).
:- endif.
:- if(exists_source(library(logicmoo_user))).
% :- use_module(library(logicmoo_user)).
:- endif.

% CLIOPATRIA rendering libraries
% :- use_module(swish_app:lib/render/cp_rdf,      []).
