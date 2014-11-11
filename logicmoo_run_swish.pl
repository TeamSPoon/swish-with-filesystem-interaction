:-module(user).




% setup paths to load relevant packages from development environment
/*
:- asserta(user:file_search_path(foreign, '../http')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(js, 'web/js')).
*/
% Hack: auto-loading this does not work.
:- [library(charsio)].
:- [charsio:library(memfile)].

:- debug(pengine(delay)).

:- use_module(library(plunit)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(process)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).

:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :- pengines_io:pengine_bind_io_to_html(Module).

:- debug(http(request)).

% :- use_module(library(pldoc)).
% :- use_module(library(pldoc/doc_http)).



:- use_module(library(settings)).

:- multifile http:location/3.
:- dynamic   http:location/3.
% doesn't descend from root because that's being moved for cliopatria
http:location(cliopatria, root(cliopatria), [priority(100)]).
:- use_module(library(memfile)).
:- use_module(server).

/*
swish_highlight:insert_memory_file(X,Y,Z):-dmsg(error(swish_highlight:insert_memory_file(X,Y,Z))).
swish_highlight:delete_memory_file(X,Y,Z):-dmsg(error(swish_highlight:delete_memory_file(X,Y,Z))).
swish_highlight:memory_file_line_position(X,Y,Z,A):-dmsg(error(swish_highlight:memory_file_line_position(X,Y,Z,A))).
swish_highlight:memory_file_substring(X,Y,Z,A,B):-dmsg(error(swish_highlight:memory_file_substring(X,Y,Z,A,B))).
swish_highlight:memory_file_to_string(X,Y):- memory_file_to_codes(X,C),string_codes(Y,C). %  dmsg(error(swish_highlight:memory_file_to_string(X,Y))).
*/

:-['../src/logicmoo_util/logicmoo_util_all'].

% :- listing((http:location(_, _, _))),retractall((http:location(cliopatria, root('.'), []))), retractall((http:location(_, root('.'), []))),!.

% :- asserta((http:location(pldoc, root('pldoc'), []))), asserta((http:location(pldoc_resource, root('pldoc'), []) :- pldoc_http:http_location_by_id(pldoc_resource, root('pldoc')))).

:-multifile(prolog:sandbox_allowed_clause/1).
prolog:sandbox_allowed_clause(Clause):-nonvar(Clause).
:-multifile(sandbox:safe_primitive/1).
sandbox:safe_primitive(X):-nonvar(X),!.
sandbox:safe_primitive(P):-var(P),!,current_predicate(F/A),functor(P,F,A).
sandbox:safe_primitive(M:P):-var(P),!,current_predicate(M:F/A),functor(P,F,A).

user:file_search_path(cliopatria, '/devel/ClioPatria'). %  current_prolog_flag(unix,true).
user:file_search_path(cliopatria, 't:/devel/ClioPatria'):- not( current_prolog_flag(unix,true)).

:- user:file_search_path(cliopatria,SP),
   exists_directory(SP),
   writeq(user:file_search_path(cliopatria,SP)),nl.
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

:- use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.

:- use_module(cliopatria(cliopatria)).

% :- retractall((http:location(_, root('.'), []))).
:- listing((http:location(_, _, _))).


:- use_module(cliopatria(cliopatria)).

% Load package manager

:- use_module(library(cpack/cpack)).

% Load the remainder of the  configuration. The directory config-enabled
% can also be used to  load   additional  (plugin)  functionality.

:- use_module(library(conf_d)).

:- load_conf_d([ 'config-enabled' ], []).

% :- http_server(http_dispatch, [port(3050)]).


:- use_module(library(pldoc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_hook)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(www_browser)).
:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_htmlsrc)).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_search)).
:- use_module(pldoc(doc_man)).
:- use_module(pldoc(doc_wiki)).
:- use_module(pldoc(doc_util)).
:- use_module(pldoc(doc_access)).
:- use_module(pldoc(doc_pack)).

:- use_module(library(doc_http)).
:- abolish(pldoc_http:src_skin,5).

pldoc_http:src_skin(Request, _Show, FormatComments, header, Out) :-  
  pldoc_http:((     
     member(request_uri(ReqURI), Request),!,
	prolog_xref:negate(FormatComments, AltFormatComments),
	replace_parameters(ReqURI, [show(raw)], RawLink),
        replace_parameters(ReqURI, [], EditLink0),
         logicmoo_util_strings:atom_subst(EditLink0,'help/source/doc','swish/filesystem/',EditLink),
	replace_parameters(ReqURI, [format_comments(AltFormatComments)], CmtLink),
	phrase(html(div(class(src_formats),
			[ 'View source with ',
			  a(href(CmtLink), \alt_view(AltFormatComments)),
                        ' or as ',
                        a(href(RawLink), raw),
                        ' or EDIT ',
                        a(href(EditLink), edit)
			])), Tokens),
	html_write:print_html(Out, Tokens))).

% called through source_to_html/3.
:- public(pldoc_http:src_skin/5).
:- listing(pldoc_http:src_skin/5).

edit_file_href(_Options,File0, HREF) :-
 pldoc_index:((  is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREFDOC))),
        logicmoo_util_strings:atom_subst(HREFDOC,'help/source/doc','swish/filesystem/',HREF),!.
edit_file_href(_Options,HREF, HREF).

doc_file_href(_Options,File0, HREF) :-
 pldoc_index:(( is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREF))).

doc_file_href(_Options,HREF, HREF).


%%	source_button(+File, +Options)// is det.
%
%	Add show-source button.
:- abolish(pldoc_html:source_button,4).
:- public(pldoc_html:source_button//2).
pldoc_html:source_button(_File, Options) -->
	{ pldoc_html:option(files(_Map), Options) }, !.	% generating files
pldoc_html:source_button(File, _Options) -->
	{show_call((doc_file_href(Options, File, HREF0),
         edit_file_href(Options, File, EDIT_HREF0)))},

	html_write:html([
         a(href(HREF0+[show(src)]),
	       img([ class(action),
		     alt('Show source cOdE'),
		     title('Show source CODE'),
		     src(location_by_id(pldoc_resource)+'source.png')
		   ])),
         a(href(EDIT_HREF0+[]),
	       img([ class(action),
		     alt('Edit source'),
		     title('Edit source'),
		     src(location_by_id(pldoc_resource)+'edit.png')
		   ]))]).


doug_debug(O):-format(user_error,'~nDOUG_DEBUG: ~q.~n',[O]),!.
:-debug(http_request(_)).
:-debug(cm(_)).
:-debug(swish(_)).
:-debug(storage).
:-debug(_).

testml([]):-!. testml([M|L]):-!,testml(M),testml(L). 
testml(M):-atomic(M),!,format('~w',[M]).
testml(nl(E)):-!,ignore((between(0,E,_),nl,fail)).
testml(ML):-phrase(ML,C,[]),testml(C).
