:- module(jquery,
	  [ jquery_depends/2,		% +File, -DependsOn
	    use_jquery//1		% +Component
	  ]).
:- use_module(library(http/http_path), []).
:- use_module(library(http/html_head)).
:- use_module(library(http/dcg_basics)).
:- use_module(library(pure_input)).

/** <module> JQuery declarations and utilities
*/

user:file_search_path(web, web).
user:file_search_path(jquery, js('jquery-ui')).
http:location(jquery, js('jquery-ui'), []).

:- html_resource(jquery,
		 [ virtual(true),
		   requires(jquery('jquery-1.7.2.js'))
		 ]).

%%	use_jquery(+Component)//
%
%	Ensure we get the jQuery  component   Component  into the header
%	requirements, as well as its dependencies.

use_jquery(Component) -->
	{ declare_dependencies(Component) },
	html_requires(jquery(Component)).


:- dynamic
	dep_declared/1.

declare_dependencies(Component) :-
	dep_declared(Component), !.
declare_dependencies(Component) :-
	jquery_depends(jquery(Component), Files),
	maplist(wrap_jq, Files, Deps),
	(   Deps == []
	->  html_resource(jquery(Component), [requires(jquery)]),
	    assertz(dep_declared(Component))
	;   html_resource(jquery(Component), [requires(Deps)]),
	    assertz(dep_declared(Component)),
	    maplist(declare_dependencies, Files)
	).

wrap_jq(File, jquery(File)).


%%	jquery_depends(+File, -Files) is det.
%
%	Scan structured comment of a jquery file to find dependencies.

jquery_depends(File, Files) :-
	absolute_file_name(File, Path, [access(read)]),
	phrase_from_file(jq_depends(RelFiles), Path, []),
	maplist(jquery_component(Path), RelFiles, Files).

jquery_component(Path, File, Component) :-
	absolute_file_name(File, AbsFile, [relative_to(Path)]),
	absolute_file_name(jquery(.), JQueryDir),
	directory_file_path(JQueryDir, Component, AbsFile).

jq_depends(Files) -->
	"/*!", comment_lines(Files), !, rest.

comment_lines(Files) -->
	whites, "*", whites, "Depends:", blanks_to_nl, !,
	depend_lines(Files).
comment_lines([]) -->
	whites, "*/", !.
comment_lines(Files) -->
	string(_), "\n", !,
	comment_lines(Files).

depend_lines([H|T]) --> depend_line(H), !, depend_lines(T).
depend_lines([])    --> [].

depend_line(File) -->
	whites, "*", white, whites, string(Codes), blanks_to_nl, !,
	{ atom_codes(File, Codes) }.

rest --> [_], !, rest.
rest --> [].
