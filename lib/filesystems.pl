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

:- module(swish_filesystems, [fs_write_html/1,list_filesystems/1,
  get_fs_title/2,file_shorter_name/2,fs_write_file/4]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(storage).

% :- use_module(pack(plweb/pack_info),[pack_file_hierarchy//1]).


:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_search)).

:- use_module(library(http/mimetype)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_html),
	      [ doc_for_file/2			% other imports conflict
	      ]).				% with doc_wiki
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(prolog_xref)).

:- use_module(pack(plweb/pack_info)).


/** <module> Serve filesystem files

Locate and serve files for the _Filesystems_   menu. The filesystems come from
two sources:

  - Prolog files in the file search path `filesystems`
  - Gitty files marked as `filesystem`.
*/

:- multifile
	user:file_search_path/2,
	swish_config:config/2,
	swish_config:source_alias/2.

% make filesystem(File) find the filesystem data
user:file_search_path(project_files, pack(pfc/prolog)).
user:file_search_path(project_files, pack(pfc/t)).
%user:file_search_path(project_files, swish(examples)).
%user:file_search_path(project_files, swish(examples/inference)).
%user:file_search_path(project_files, swish(examples/learning)).
%user:file_search_path(project_files, swish(examples/lemur)).

user:file_search_path('swish/filesystem', '/').
user:file_search_path(filesystem, '/').
user:file_search_path(filesystem_files, '/').




		 /*******************************
		 *	    SWISH CONFIG	*
		 *******************************/

%%	swish_config:config(-Name, -Profiles) is det.
%
%	Provides the object `config.swish.filesystem_files`, a  JSON object that
%	provides the available filesystem.

swish_config:config(filesystem_files, _{html:"", json:[]}) :- !.

swish_config:config(filesystem_files, _{html:X, json:FileFilesystems}) :-
        reset_fs_names,
	with_output_to(string(X),filesystem_files(FileFilesystems)),!,
        reset_fs_names.


% make SWISH serve /filesystem/File as filesystem(File).
% swish_config:source_alias(filesystem, [access(read), search('*.{pl,swinb}')]).
%swish_config:source_alias(filesystem, [access(read), search('*.{pl,swinb,}')]).
swish_config:source_alias(filesystem, [access(both)]).
swish_config:source_alias('swish/filesystem', [access(both)]).


:- http_handler(swish(list_filesystems),
		list_filesystems, [id(swish_filesystems)]).

fs_write_html(HTML):- phrase(html(HTML), Tokens), current_output(Out), html_write:print_html(Out, Tokens).

list_pack_filesystem(Pack):-
 Pack = pfc,
 pack_info: (mirror_pack(Pack),
   %mirror_pack(Pack),
   pack_archive(Pack, _Hash, Archive),
   ensure_xref_pack(Archive),
   findall(File, pack_file(Pack, File, _Size, _XrefID), Files),
   files_to_tree(Files, Trees),
   fs_write_html( [
              div(class('pack-files'),
		 ul(class(tree),
		    \dir_nodes(Pack, Trees)))])),!.

:- thread_local(file_had_title/2).

reset_fs_names:- retractall(file_had_title(_,_)).

%%	list_filesystems(+Request)
%
%	Get a list of registered filesystem code. Filesystems are described in
%	a file swish_filesystems('index.json').

% list_filesystems(_Request) :- list_pack_filesystem(pfc),!.

list_filesystems(_Request) :- 
   swish_config:config(filesystem_files, JSON),
        reply_json(JSON).


:- system:import(list_filesystems/1).

list_filesystems2(_Request) :-
        storage_filesystems(StorageFilesystems),
	reply_json(StorageFilesystems).

%%	filesystem_files(JSON:list) is det.
%
%	JSON is a list of JSON dicts containing the keys below. The list
%	is composed from all *.pl files in the search path `filesystem`.
%
%	  - file:File
%	  - href:URL
%	  - title:String

filesystem_files(AllFilesystems) :-
	http_absolute_location(swish(filesystem), HREF, []),
	findall(Index,
		absolute_file_name(project_files(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs),
	maplist(fs_inx_json(HREF), ExDirs, JSON),
	append(JSON, AllFilesystems).

fs_inx_json(HREF, Dir, JSON) :-
	directory_file_path(Dir, 'index.json', File),
	fail, 
        access_file(File, read), !,
	fs_read_file_to_json(File, JSON0),
	maplist(fs_add_href(HREF), JSON0, JSON).
fs_inx_json(HREF, Dir, JSON) :-
	string_concat(Dir, "/*/*/*.{pl,swinb}", Pattern0),
        expand_file_name(Pattern0, Files0),
        string_concat(Dir, "/*/*.{pl,swinb}", Pattern1),
        expand_file_name(Pattern1, Files1),
        append(Files0, Files1,Files3),
        sort(Files3,Files),
	maplist(fsex_file_json(HREF), Files, JSON).

fs_read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON),
	    close(In)).

fs_add_href(HREF0, Dict, Dict2) :-
	is_dict(Dict),
	directory_file_path(HREF0, Dict.get(file), HREF), !,
	Dict2 = Dict.put(href, HREF).
fs_add_href(_, Dict, Dict).

%%	fsex_file_json(+FilesystemBase, +Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).

fsex_file_json(HREF0, Path, json{file:Path, href:HREF, title:UsedTitle}) :-
      must_det_l((  
      % File = Path,
      % file_base_name(Path, File),      
      % file_name_extension(Base, _, File),
      % Title = Path,
      get_fs_title(Path,Title),
      % directory_file_path(HREF0, Path, HREF),
      atom_concat(HREF0, Path, HREF),
      fs_write_file(Path, HREF, Title, UsedTitle))).

fs_write_file(_File, HREF, _Title, UsedTitle):- file_had_title(HREF,UsedTitle),!.
fs_write_file(File, _HREF, _Title, UsedTitle):- file_had_title(File,UsedTitle),!.
fs_write_file(File, HREF, Title, UsedTitle):- 
	file_had_title(_, Title), 
        file_shorter_name(File,Base),
	atomic_list_concat([Title,' - ',Base],NewTitle),
	fs_write_file(File, HREF, NewTitle, UsedTitle).
%fs_write_file(File, HREF, Title,UsedTitle):- fs_names(_, Title),!,fs_write_file(File, HREF, File,UsedTitle).
fs_write_file(File, HREF, Title, Title):- asserta(file_had_title(HREF,Title)),
      fs_write_file_in_list(File, HREF, Title).

:- thread_local(file_dir_written/1).

fs_write_file_in_list(File, HREF, Title):- file_dir_written(Was),atom_concat(Was,Shorter,File),!,
   fs_write_html([a(href(HREF), [Shorter]),&(nbsp),Title,br([],[])]),!.

fs_write_file_in_list(File, HREF, Title):- retractall(file_dir_written(_)),
      file_shorter_name2(File, Shorter),  
      atom_concat(Was,Shorter,File),
      asserta(file_dir_written(Was)),!,
      fs_write_html([Was,br([],[])]),
      fs_write_html([a(href(HREF), [Shorter]),&(nbsp),Title,br([],[])]),!.


un_helpfull_name('').
un_helpfull_name(prolog).
un_helpfull_name(pack).
un_helpfull_name(t).

file_shorter_name(Path, Shorter):-
     atomic_list_concat(O,'/',Path),
     exclude(un_helpfull_name,O,PathL),O \= PathL, 
     atomic_list_concat(PathL,'/',O2),
     file_shorter_name(O2, Shorter),!.
file_shorter_name(Path, Shorter):- 
     atom_concat(Next,'.pl',Path),!,
     file_shorter_name(Next, Shorter).
file_shorter_name(Path, Shorter):- file_shorter_name2(Path, Shorter).
file_shorter_name2(Path, Shorter):-
     directory_file_path(Dir, File, Path),
     file_base_name(Dir, Base),
     atomic_list_concat([Base,/,File],Shorter),!.
file_shorter_name2(File,File).



% a(href=HREF,Title)),pack_file_link(File


		 /*******************************
		 *	      STORAGE		*
		 *******************************/

%%	storage_filesystems(-List) is det.
%
%	Extract filesystems from the gitty store.

storage_filesystems(List) :-
       %  swish_config:config(community_filesystems, true),
	findall(Ex, gitty_filesystem(Ex), List1),
	List1 \== [], !,
	List = [\hr|List1].
storage_filesystems([]).
                                        
gitty_filesystem(json{title:Title, file:File, type:"store"}) :-
	storage_file(File),
	storage_meta_data(File, Meta),
	Meta.get(example) == true,
	(   Title = Meta.get(title), Title \== ""
	->  true
	;   Title = File
	).


old_title(Title) --> "%", whites, !, dcg_title(upper,Title).

get_fs_title(FilePath, Title) :- file_had_title(FilePath, Title),!.

get_fs_title(FilePath, Title) :-
	first_line_fst(FilePath, FirstLine),
	(   FirstLine == end_of_file
	->  fail
        	;   phrase(old_title(Title), FirstLine)
	).

get_fs_title(FilePath, Title) :- file_shorter_name(FilePath, Title),!.

get_fs_title(FilePath, Title) :-
	setup_call_cleanup(
	   open(FilePath, read, In),
           (repeat,
	     (at_end_of_stream(In)-> (!,fail) ;
              (read_line_to_codes(In, Line)->
                phrase(dcg_title(upper,Title), Line)))),
	   close(In)),!.

get_fs_title(FilePath, Title) :-
        setup_call_cleanup(
           open(FilePath, read, In),
           (repeat,
             (at_end_of_stream(In)-> (!,fail) ;
              (read_line_to_codes(In, Line)->
                phrase(dcg_title(alpha,Title), Line)))),
           close(In)),!.


get_fs_title(FilePath, Title) :-
	first_line_fst(FilePath, FirstLine),
	(   FirstLine == end_of_file
	->  Title = "Empty"
	;   phrase(dcg_title(graph,Title), FirstLine)
	).

first_line_fst(File, Line) :-
	setup_call_cleanup(
	    open(File, read, In),
	    read_line_to_codes(In, Line),
	    close(In)).

dcg_title(Type,Title) -->
	non_letters(Type), one_upper(Type,C),!, gfst_rest(Codes),
	{ string_codes(Title, [C|Codes]) }.


% dcg_title("No title") --> gfst_rest(_).

one_upper(Type,C) --> [C],{ char_type(C,Type) }.

non_letters(_Type) --> `:-`,!,{fail}.
non_letters(_Type) --> `(`,!,{fail}.
non_letters( Type) --> [C],{ \+ char_type(C,Type) },!,non_letters(Type).
non_letters(_Type) --> [].

gfst_rest(List, List, []).


% :- pack_info:update_pack_metadata.

% mirror_packs.
	

