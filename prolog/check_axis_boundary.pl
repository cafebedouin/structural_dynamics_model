% ============================================================================
% CHECK AXIS BOUNDARY — transitive committer→observer taint guard (OQ-15 / OQ-135)
% ============================================================================
% Implements v8 §8 item 1 (LOAD-BEARING): the one-seat invariant as a
% reachability / dataflow property over the *actual loaded call graph*, NOT a
% count or an import grep (v8 calls those a trap; W1 confirmed grep is blind —
% cs_drift_mismatch reaches observer machinery transitively, audit
% 2026-06-23_oq15_crossaxis_witnesses/).
%
% THE INVARIANT (v8 §3, directional):
%   No committer field reaches OBSERVER computation by ANY path except as the
%   entailment-typed payload on the single forward bridge
%   `influences` → detect_necessity_inheritance.
%
% HOW THIS REALIZES IT.  The committer axis is uniformly `cs_*`-prefixed (the
% facts cs_reading_relation/cs_kernel_id/cs_story_uid/cs_axiom*/cs_drift_state
% and every derived cs_* predicate). A "committer field reaching observer
% computation" is therefore a call edge  P → cs_X  in a loaded clause body where
% P is NOT itself committer-side (`cs_*`) and NOT in the mediator-output layer
% (json_report — the sanctioned both-axis reader that writes only JSON). Any
% transitive observer→committer path crosses the boundary at exactly one such
% last-hop edge (the last non-cs_ caller before the first cs_ callee), so the
% set of these DIRECT edges is COMPLETE for the guarded direction — transitive
% reachability collapses to edge-enumeration over the call graph. This reads the
% loaded program (clause/2 over every engine-defined predicate's bodies),
% descending into control constructs and meta-calls; it is not text matching.
%
% WHAT IT EMITS.  One `AXIS_EDGE: <SrcMod>:<Name>/<Ar> -> <Callee>/<Ar>` line per
% boundary-crossing edge, sorted+deduped. The Python harness
% (python/check_axis_boundary.py) diffs these against
% prolog/axis_boundary_allowlist.txt (load_warning_gate.py pattern): any edge not
% allowlisted → exit 1; an allowlisted edge that has disappeared → stale note.
%
% Run (engine only — corpus not needed; the call graph is static):
%   cd prolog && swipl -l check_axis_boundary.pl -g "run_axis_boundary, halt" -t "halt(1)"
% ============================================================================

:- [stack].

% Source files we count as ENGINE (own the call graph). File-based, so it is
% robust to module aliasing; excludes SWI library and this checker itself.
engine_file(F) :-
    sub_atom(F, _, _, _, '/prolog/'),
    \+ sub_atom(F, _, _, _, '/library/'),
    \+ sub_atom(F, _, _, _, 'check_axis_boundary').

% A predicate defined in the engine, with its DEFINING module + file. The
% imported_from/reexport guard is essential: drl_modal_logic `:- reexport(
% drl_composition)` etc. would otherwise surface detect_necessity_inheritance
% under every importing module — a module-attribution artifact, not real edges.
engine_predicate(M, Name, Arity, Head, F) :-
    current_module(M),
    current_predicate(Name, M:Head),
    functor(Head, Name, Arity),
    \+ predicate_property(M:Head, imported_from(_)),   % defining module only
    catch(predicate_property(M:Head, file(F)), _, fail),
    engine_file(F),
    % only predicates with walkable clauses (skip foreign/built-in)
    \+ predicate_property(M:Head, foreign),
    \+ predicate_property(M:Head, built_in).

% MEDIATOR-OUTPUT layer: reads both axes by design, writes only to JSON. A
% committer read from here is NOT a violation (it is the mediator's job). Keep
% this list MINIMAL and explicit — it is a sanctioned exemption, not a default.
mediator_module(json_report).

% body_calls(+Body, -CalleeName/-CalleeArity): every goal called in a clause
% body, descending through control constructs and the common meta-call wrappers.
body_calls(V, _) :- var(V), !, fail.
body_calls((A,B), G)        :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A;B), G)        :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A->B), G)       :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A*->B), G)      :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(\+(A), G)        :- !, body_calls(A,G).
body_calls(not(A), G)       :- !, body_calls(A,G).
body_calls(call(A), G)      :- !, body_calls(A,G).
body_calls(once(A), G)      :- !, body_calls(A,G).
body_calls(ignore(A), G)    :- !, body_calls(A,G).
body_calls(findall(_,A,_), G):- !, body_calls(A,G).
body_calls(findall(_,A,_,_), G):- !, body_calls(A,G).
body_calls(forall(A,B), G)  :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(aggregate_all(_,A,_), G) :- !, body_calls(A,G).
body_calls(bagof(_,A,_), G) :- !, body_calls(A,G).
body_calls(setof(_,A,_), G) :- !, body_calls(A,G).
body_calls(catch(A,_,B), G) :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(_^A, G)          :- !, body_calls(A,G).
body_calls(maplist(Goal,_), G)     :- !, meta_partial(Goal, G).
body_calls(maplist(Goal,_,_), G)   :- !, meta_partial(Goal, G).
body_calls(maplist(Goal,_,_,_), G) :- !, meta_partial(Goal, G).
% Peel module qualifiers RECURSIVELY: a clause consulted outside a module file
% can store its body double-qualified, e.g. user:(narrative_ontology:cs_X(..)).
% Taking functor/3 of the inner term then yields (:)/2 and the cs_ sink is missed
% (caught by positive control 1, 2026-06-23). Recurse to reach the real callee.
body_calls(M:Goal, G) :- !, atom(M), body_calls(Goal, G).
body_calls(Goal, N/A) :- callable(Goal), functor(Goal, N, A).

% maplist(Closure, ...): the closure names the called predicate.
meta_partial(Goal, N/A0) :-
    callable(Goal),
    ( Goal = M:G2, atom(M) -> true ; G2 = Goal ),
    functor(G2, N, A),
    A0 = A.   % under-counts added maplist args; name is what the cs_ test needs

% A committer taint sink: a predicate whose name is cs_-prefixed (covers the
% narrative_ontology committer facts AND every derived cs_* predicate).
committer_name(Name) :- atom(Name), sub_atom(Name, 0, 3, _, 'cs_').

% Source is committer-SIDE (not observer) if it is defined in a committer module
% (file basename cs_*) OR its own name is cs_-prefixed. Module-based catches the
% committer predicates that are NOT cs_-named (compare_kernel_readings in
% cs_kernel_registry, run_drift_mismatch_report in cs_drift_mismatch) — they read
% cs_ facts as their OWN axis, not across the boundary.
committer_side(_M, Name, _F) :- committer_name(Name), !.
committer_side(_M, _Name, F) :- file_base_name(F, B), sub_atom(B, 0, 3, _, 'cs_').

% boundary_edge(SrcMod, SrcName/SrcAr, CalleeName/CalleeAr): committer→observer
% crossing — an observer-side / tooling caller reaching a cs_ sink.
boundary_edge(M, Name/Arity, Callee) :-
    engine_predicate(M, Name, Arity, Head, F),
    \+ committer_side(M, Name, F),  % source is NOT committer-side
    \+ mediator_module(M),          % source is NOT the mediator-output layer
    catch(clause(M:Head, Body), _, fail),
    body_calls(Body, Callee),
    Callee = CName/_,
    committer_name(CName).          % sink IS committer-side

run_axis_boundary :-
    findall(edge(M, Src, Callee), boundary_edge(M, Src, Callee), Raw),
    sort(Raw, Edges),
    ( Edges == []
    ->  format("AXIS_EDGE_NONE~n", [])
    ;   forall(member(edge(M, Name/Ar, CName/CAr), Edges),
               format("AXIS_EDGE: ~w:~w/~w -> ~w/~w~n", [M, Name, Ar, CName, CAr]))
    ),
    length(Edges, N),
    format("AXIS_EDGE_COUNT: ~w~n", [N]).
