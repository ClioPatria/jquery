/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012 VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

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

user:file_search_path(jquery_js, js('jquery-ui')).
http:location(jquery_js, js('jquery-ui'), []).

:- html_resource(jquery,
		 [ virtual(true),
		   requires(jquery_js('jquery-1.7.2.js'))
		 ]).

%%	use_jquery(+Component)//
%
%	Ensure we get the jQuery  component   Component  into the header
%	requirements, as well as its dependencies.

use_jquery(Component) -->
	{ declare_dependencies(Component) },
	html_requires(jquery_js(Component)).


:- dynamic
	dep_declared/1.

declare_dependencies(Component) :-
	dep_declared(Component), !.
declare_dependencies(Component) :-
	jquery_depends(jquery_js(Component), Files),
	maplist(wrap_jq, Files, Deps),
	(   Deps == []
	->  html_resource(jquery_js(Component), [requires(jquery)]),
	    assertz(dep_declared(Component))
	;   html_resource(jquery_js(Component), [requires(Deps)]),
	    assertz(dep_declared(Component)),
	    maplist(declare_dependencies, Files)
	).

wrap_jq(File, jquery_js(File)).


%%	jquery_depends(+File, -Files) is det.
%
%	Scan structured comment of a jquery file to find dependencies.

jquery_depends(File, Files) :-
	absolute_file_name(File, Path, [access(read)]),
	phrase_from_file(jq_depends(RelFiles), Path, []),
	maplist(jquery_component(Path), RelFiles, Files).

jquery_component(Path, File, Component) :-
	absolute_file_name(File, AbsFile, [relative_to(Path)]),
	absolute_file_name(jquery_js(.), JQueryDir),
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
