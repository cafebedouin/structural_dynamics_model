/** codewalk_caller.pl — CALL-SITE walker for the bound-dispatch registry, module-resolved.

The second arm of the bound-caller instrument pair. The first arm is a single-line regex
(`audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py`), whose blind spots are
readable in its own source: one physical line at a time, no nested-term arguments (conceded
in its docstring at :44), bare lowercase atoms only, name/arity textual matching with NO
module resolution (:72-73). This arm uses `library(prolog_codewalk)` over the LOADED
program, so it sees module-resolved goals, multi-line clause bodies, nested-term arguments,
and goals reached through meta-predicates.

THE TWO ARMS ARE NOT NESTED. This one cannot see source in modules the load chain does not
reach, nor goal strings embedded in Python/shell — the regex can. Read a zero from this
file as "this arm declined", never as "no caller exists".

`evaluate` — WITNESSED 2026-08-18, AND IT CORRECTS THE CLAIM THIS FILE WAS BUILT TO MAKE.
The expectation going in was that a selector bound by unification before the call
(`T = rope, ..., p(C, T)`) is invisible to BOTH arms — a shared residue class. It is not.
`library(prolog_codewalk)` executes `A=B` goals as it walks (`evaluate/2`,
/usr/lib/swi-prolog/library/prolog_codewalk.pl:663-664, `unify_with_occurs_check/2`,
on by default), so the binding propagates into later goals in the same body and the call is
reported BOUND. SWI does not compile the unification away — `clause/2` on such a body still
shows `_G=alpha, q(a,_G)` — so this is the walker's abstract interpretation, not the
compiler. Consequence: that shape is a third CODEWALK-ONLY capability, not shared residue.

What is genuinely invisible to both arms is a selector bound by RUNTIME COMPUTATION —
`member(T, Types)`, a helper predicate's output, an arithmetic or findall result. Those
stay free under the walker because `evaluate/2` handles unification only.

`run_codewalk_caller/2` takes the flag, so the unification-bound stratum is MEASURABLE BY
DIFFERENCE: evaluate(true) minus evaluate(false) is exactly the set of sites whose selector
is bound by a unification rather than written literally at the call.

DECLARED ASSUMPTION, inherited from prolog/dispatch_head_check.pl:9-11: the OUTPUT argument
is the LAST argument, by engine convention. A predicate whose output is not last escapes
this walker exactly as it escapes the definition-site one.

DECLARED LIMITATION: sites are DEDUPED to (defining file, callee PI, bound/free, caller
file:line, caller PI, atom). Two calls to the same predicate in the same clause with the
same bound atom count as ONE site — the line number cannot tell them apart anyway (see
next paragraph), so a distinct count there would be a precision the output does not have.

DECLARED LIMITATION: site line numbers are the ENCLOSING CLAUSE's first line
(clause_property/2 line_count), not the goal's own line — prolog_codewalk reports a
term position inside the clause, and resolving it to a source line would need the clause
re-read. The clause is the unit a reader needs anyway.

DISCRIMINATION RECORD — ANCHORED TO CONTENT, not to a commit (operator, 2026-08-17).
Two-sided, both halves required in the same process before any zero from this file is
readable:
  FIRES   on dr_type/3  — the converted cascade entry point, called with the type bound at
          many sites. Expected output shape:
            CWC_PRED: drl_core.pl dr_type/3 module=drl_core sites=67 bound=19
          (67/19 measured 2026-08-18 on the [stack] load chain at HEAD 0300be24; the
          discriminating property is bound > 0, not the exact integers, which move with the
          engine.)
  DECLINES on constraint_signature/2 — converted 2026-08-17, no live bound callers.
            CWC_PRED: signature_detection.pl constraint_signature/2 module=signature_detection sites=18 bound=0
          The informative half: the arm LOOKED (sites > 0) and declined on the bound
          question (bound = 0). A declines-control reporting sites=0 means it did not look,
          and invalidates every zero in that run.
  To re-verify without trusting a SHA: bind the last argument of any live dr_type/3 call
  site and the count moves; the fixtures in python/codewalk_caller_check.py exercise the
  same path on planted code.

Output protocol (consumed by python/codewalk_caller_check.py):
  CWC_PRED: <deffile> <name>/<arity> module=<M> sites=<N> bound=<B>
  CWC_SITE: <deffile> <name>/<arity> <bound|free> <callerfile>:<line> caller=<PI> atom=<A|->
  CWC_UNRESOLVED: <deffile> <name>/<arity> reason=<file_not_loaded|predicate_not_owned_by_that_file>
  CWC_WALKED: <n_goals_traced>
  CWC_SCANNED: <n_specs>
  CWC_MODULES: <n_loaded_modules>
  CWC_EVALUATE: <true|false>   -- whether `A=B` bindings were propagated (see below)

Run:
  swipl -q -g "[stack]" -l prolog/codewalk_caller.pl \
        -g "run_codewalk_caller('specfile.txt'), halt" -t "halt(1)"

Spec file: one entry per line, `<deffile.pl> <name>/<arity>`; blank lines and % comments
skipped. The spec IS python/dispatch_head_check.py's DECLARED registry, written out by the
Python wrapper — this file carries no copy of it, so the two cannot fork (Pattern 2).
*/

:- module(codewalk_caller, [run_codewalk_caller/1, run_codewalk_caller/2]).

:- use_module(library(prolog_codewalk)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- dynamic cwc_spec/3.          % DefFile, Name, Arity
:- dynamic cwc_site/6.          % DefFile, Name/Arity, Kind, CallerFile, Line, Caller-Atom
:- dynamic cwc_walked/1.
:- dynamic cwc_resolved/4.   % DefFile, Name, Arity, Module

% ---------------------------------------------------------------------------

run_codewalk_caller(SpecFile) :-
    run_codewalk_caller(SpecFile, true).

run_codewalk_caller(SpecFile, Evaluate) :-
    retractall(cwc_spec(_,_,_)),
    retractall(cwc_site(_,_,_,_,_,_)),
    retractall(cwc_walked(_)),
    retractall(cwc_resolved(_,_,_,_)),
    assertz(cwc_walked(0)),
    load_specs(SpecFile),
    aggregate_all(count, cwc_spec(_,_,_), NSpecs),
    (   NSpecs =:= 0
    ->  format("CWC_ERROR: 0 specs read from ~w~n", [SpecFile]), fail   % Pattern 5: empty scan is broken, not clean
    ;   true
    ),
    aggregate_all(count, current_module(_), NMods),
    (   NMods < 2
    ->  format("CWC_ERROR: ~w loaded module(s) — the program was not loaded~n", [NMods]), fail
    ;   true
    ),
    resolve_specs,
    walk_all(Evaluate),
    report(NSpecs, NMods, Evaluate).

% Resolve each registry key (defining FILE + name/arity) to the module that actually owns
% that predicate. source_file/2, not module_property/2: four registry files (json_report,
% fpn_report, maxent_report, orbit_report) carry NO `:- module/2` header at all, so their
% predicates live in `user` and a file->module lookup can never resolve them. This is the
% module resolution the regex arm structurally cannot do, and it has to survive the
% headerless files or it silently scores them zero.
resolve_specs :-
    forall(cwc_spec(File, Name, Arity),
           ( functor(H, Name, Arity),
             (   source_file(M:H, Path),
                 file_base_name(Path, File)
             ->  assertz(cwc_resolved(File, Name, Arity, M))
             ;   true
             ))).

file_is_loaded(File) :-
    source_file(Path),
    file_base_name(Path, File),
    !.

load_specs(SpecFile) :-
    setup_call_cleanup(
        open(SpecFile, read, S),
        read_spec_lines(S),
        close(S)).

read_spec_lines(S) :-
    read_line_to_string(S, Line),
    (   Line == end_of_file
    ->  true
    ;   ( parse_spec(Line) -> true ; true ),
        read_spec_lines(S)
    ).

parse_spec(Line) :-
    split_string(Line, " \t", " \t", Parts0),
    exclude(==(""), Parts0, Parts),
    Parts = [FileS, PIS],
    \+ sub_string(FileS, 0, 1, _, "%"),
    split_string(PIS, "/", "", [NameS, ArityS]),
    number_string(Arity, ArityS),
    atom_string(File, FileS),
    atom_string(Name, NameS),
    assertz(cwc_spec(File, Name, Arity)).

% ---------------------------------------------------------------------------
% ONE walk over the whole loaded program; on_trace filters against the spec set.
% (58+ separate walks would re-traverse the program 58+ times for the same answer.)
% ---------------------------------------------------------------------------

walk_all(Evaluate) :-
    prolog_walk_code([ trace_reference(_),
                       on_trace(codewalk_caller:trace_goal),
                       source(true),
                       infer_meta_predicates(true),
                       autoload(false),
                       undefined(ignore),
                       evaluate(Evaluate),
                       trace_condition(codewalk_caller:in_spec)
                     ]).

in_spec(Callee, _Context) :-
    strip_module_qualified(Callee, _M, G),
    functor(G, Name, Arity),
    cwc_spec(_, Name, Arity).

trace_goal(Callee, Caller, Location) :-
    strip_module_qualified(Callee, M, Goal),
    functor(Goal, Name, Arity),
    % forall, NOT if-then-else: two registry entries can share a name/arity in
    % different files, and committing to the first drops the second silently.
    forall(cwc_spec(DefFile, Name, Arity),
           ( record_site(DefFile, Name, Arity, M, Goal, Caller, Location) -> true ; true )).

record_site(DefFile, Name, Arity, M, Goal, Caller, Location) :-
        bump_walked,
        module_matches(M, DefFile, Name, Arity),
        Goal =.. [_|Args],
        last(Args, Last),
        (   atom(Last)
        ->  Kind = bound, Atom = Last
        ;   Kind = free,  Atom = '-'
        ),
        location_file_line(Location, F, L),
        caller_pi(Caller, PI),
        atomic_list_concat([Name, /, Arity], PIA),
        (   cwc_site(DefFile, PIA, Kind, F, L, PI-Atom)
        ->  true                                   % dedupe: same site, same atom
        ;   assertz(cwc_site(DefFile, PIA, Kind, F, L, PI-Atom))
        ).

bump_walked :-
    retract(cwc_walked(N0)), N is N0 + 1, assertz(cwc_walked(N)).

strip_module_qualified(M:G, M, G) :- !.
strip_module_qualified(G, user, G).

% The callee module must be the one that OWNS the registry entry's predicate in the
% registry entry's file.
module_matches(M, DefFile, Name, Arity) :-
    cwc_resolved(DefFile, Name, Arity, M).

location_file_line(clause_term_position(Ref, _), F, L) :- !, clause_file_line(Ref, F, L).
location_file_line(clause(Ref), F, L)                  :- !, clause_file_line(Ref, F, L).
location_file_line(file_term_position(F0, _), F, 0)    :- !, rel(F0, F).
location_file_line(file(F0, L, _, _), F, L)            :- !, rel(F0, F).
location_file_line(_, '?', 0).

clause_file_line(Ref, F, L) :-
    (   catch(clause_property(Ref, file(F0)), _, fail) -> rel(F0, F) ; F = '?' ),
    (   catch(clause_property(Ref, line_count(L)), _, fail) -> true ; L = 0 ).

rel(Abs, Rel) :-
    (   atom(Abs),
        sub_atom(Abs, B, _, A, '/prolog/'),
        B > 0
    ->  sub_atom(Abs, _, A, 0, Tail), atom_concat('prolog/', Tail, Rel)
    ;   Rel = Abs
    ).

caller_pi(M:Head, PI) :- !,
    (   compound(Head) -> functor(Head, N, A) ; N = Head, A = 0 ),
    atomic_list_concat([M, :, N, /, A], PI).
caller_pi(Other, Other).

% ---------------------------------------------------------------------------

report(NSpecs, NMods, Evaluate) :-
    forall(cwc_spec(DefFile, Name, Arity), report_pred(DefFile, Name, Arity)),
    forall(( cwc_site(DefFile, PIA, Kind, F, L, PI-Atom) ),
           format("CWC_SITE: ~w ~w ~w ~w:~w caller=~w atom=~w~n",
                  [DefFile, PIA, Kind, F, L, PI, Atom])),
    cwc_walked(NW),
    format("CWC_WALKED: ~w~n", [NW]),
    format("CWC_SCANNED: ~w~n", [NSpecs]),
    format("CWC_MODULES: ~w~n", [NMods]),
    format("CWC_EVALUATE: ~w~n", [Evaluate]).

report_pred(DefFile, Name, Arity) :-
    atomic_list_concat([Name, /, Arity], PIA),
    (   cwc_resolved(DefFile, Name, Arity, M)
    ->  aggregate_all(count, cwc_site(DefFile, PIA, _, _, _, _), NS),
        aggregate_all(count, cwc_site(DefFile, PIA, bound, _, _, _), NB),
        format("CWC_PRED: ~w ~w module=~w sites=~w bound=~w~n", [DefFile, PIA, M, NS, NB])
    ;   file_is_loaded(DefFile)
    ->  format("CWC_UNRESOLVED: ~w ~w reason=predicate_not_owned_by_that_file~n",
               [DefFile, PIA])
    ;   format("CWC_UNRESOLVED: ~w ~w reason=file_not_loaded~n", [DefFile, PIA])
    ).
