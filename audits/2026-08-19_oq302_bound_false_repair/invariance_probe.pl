% ============================================================================
% OQ-302 — invariance probe for boltzmann_invariant_mountain/2
% ============================================================================
% Measures what the bound-`false` repair at boltzmann_compliance.pl:577 changes.
% Preregistration: audits/2026-08-19_oq302_bound_false_repair/PREREGISTRATION.md
% (md5 c7a7345ce4664871df94e9adf1fc682d, frozen before this file was written).
%
% MECHANISM (PREREG §0a). probe_harness:with_overlay/3 CANNOT install the repair:
% it snapshots FACTS only (clause(M:T, true)), and boltzmann_invariant_mountain/2
% clause 1 is a RULE; the predicate is also static, so assertz throws. An overlay
% would have been a silent no-op and the two arms would have been identical for
% the wrong reason. Instead the repaired arm executes the ENGINE'S OWN compiled
% clause-2 body, fetched with clause/2 at run time — a composition, not a replica.
%
% ARM FLAGS NAME THE CLAUSE, NEVER ITS POSITION (PREREG §6.1). detect_source_arm/1
% READS the first clause's body and reports which arm the unmodified source
% implements. Before Commit 1 that is `defect`; after it, `repaired`. Every
% comparison keys on the arm-named columns, so the post-commit re-run cannot
% silently compare the wrong pair.
%
% Usage (from prolog/):
%   swipl -q -l stack.pl <this file> -g "probe_leg('testsets','/abs/out.tsv')" -t halt
% ============================================================================

:- use_module(library(lists)).

% ---------------------------------------------------------------------------
% Arm detection — by reading the clause, not by assuming
% ---------------------------------------------------------------------------

%% detect_source_arm(-Arm)  Arm in {defect, repaired, unknown}
detect_source_arm(Arm) :-
    (   clause(boltzmann_compliance:boltzmann_invariant_mountain(_, _), Body)
    ->  (   eac_arg2_in(Body, A2)
        ->  (   A2 == false -> Arm = defect
            ;   var(A2)     -> Arm = repaired
            ;   Arm = unknown )
        ;   Arm = unknown )
    ;   Arm = unknown ).

%% eac_arg2_in(+Body, -Arg2)  first epistemic_access_check/2 goal in a conjunction
eac_arg2_in(V, _) :- var(V), !, fail.
eac_arg2_in((A, _), X) :- eac_arg2_in(A, X), !.
eac_arg2_in((_, B), X) :- !, eac_arg2_in(B, X).
eac_arg2_in(once(G), X) :- !, eac_arg2_in(G, X).
eac_arg2_in(_:G, X) :- !, eac_arg2_in(G, X).
eac_arg2_in(epistemic_access_check(_, X), X).

%% body_clause(-HeadC, -HeadR, -Body)  the 4-test clause (head arg2 unbound)
body_clause(HC, HR, Body) :-
    clause(boltzmann_compliance:boltzmann_invariant_mountain(HC0, HR0), Body0),
    var(HR0), !,
    HC = HC0, HR = HR0, Body = Body0.

% ---------------------------------------------------------------------------
% Safe evaluation — a throw is a VALUE, and failure is a DISTINCT value
% ---------------------------------------------------------------------------

%% safe_eval(:Goal, ?Template, -Outcome)
%  Outcome = ok(Template) | failed | threw(Error). Never throws, never fails.
safe_eval(Goal, Templ, Outcome) :-
    catch( ( call(Goal) -> Outcome = ok(Templ) ; Outcome = failed ),
           E,
           Outcome = threw(E) ).

%% render(+Outcome, -Atom)
render(ok(X), A)     :- !, term_to_atom(X, A0), sanitize(A0, A).
render(failed, 'FAIL') :- !.
render(threw(E), A)  :- !, formal_of(E, F), term_to_atom(F, A0),
                        atom_concat('ERROR:', A0, A1), sanitize(A1, A).
render(_, 'BUG:unrendered').

formal_of(error(Formal, _), Formal) :- !.
formal_of(E, E).

%% sanitize(+Atom, -Atom)  TSV safety: no tabs, no newlines in a cell
sanitize(A0, A) :-
    atom_codes(A0, Cs0),
    maplist(nl_tab_to_space, Cs0, Cs),
    atom_codes(A, Cs).
nl_tab_to_space(9, 32) :- !.
nl_tab_to_space(10, 32) :- !.
nl_tab_to_space(13, 32) :- !.
nl_tab_to_space(C, C).

% ---------------------------------------------------------------------------
% The four tests — transcribed from boltzmann_compliance.pl:580-618.
% Each is its own column (PREREG §6.6); the transcription is cross-checked
% against the clause/2 result by agg_check (PREREG §0b).
% ---------------------------------------------------------------------------

t1(C, T1) :-
    boltzmann_compliance:boltzmann_compliant(C, CompResult),
    (   CompResult = compliant(_)
    ->  T1 = pass(factorization)
    ;   T1 = fail(factorization, CompResult) ).

t2(C, T2) :-
    boltzmann_compliance:scope_invariance_test(C, ScopeResult),
    (   ScopeResult = invariant
    ->  T2 = pass(scope_invariance)
    ;   T2 = fail(scope_invariance, ScopeResult) ).

t3(C, T3) :-
    (   boltzmann_compliance:excess_extraction(C, Excess)
    ->  (   Excess =< 0.01
        ->  T3 = pass(no_excess_extraction)
        ;   T3 = fail(excess_extraction, Excess) )
    ;   T3 = pass(no_extraction_data) ).

t4(C, T4) :-
    signature_detection:get_constraint_profile(C, Profile),
    (   signature_detection:natural_law_signature(Profile)
    ->  T4 = pass(natural_law_signature)
    ;   T4 = fail(natural_law_signature) ).

%% aggregate_tests(+T1,+T2,+T3,+T4, -Result)  mirrors :612-618
aggregate_tests(T1, T2, T3, T4, Result) :-
    Tests = [T1, T2, T3, T4],
    include(boltzmann_compliance:is_failure, Tests, Failures),
    (   Failures = []
    ->  Result = invariant(Tests)
    ;   Result = variant(Failures) ).

% ---------------------------------------------------------------------------
% The two arms
% ---------------------------------------------------------------------------

%% engine_result(+C, -R)  the predicate exactly as the source stands
engine_result(C, R) :-
    once(boltzmann_compliance:boltzmann_invariant_mountain(C, R)).

%% recon_repaired(+C, -R)  the repaired program, composed from the engine's own clause
recon_repaired(C, Result) :-
    once(boltzmann_compliance:epistemic_access_check(C, S)),
    (   S == false
    ->  Result = inconclusive(insufficient_data)
    ;   body_clause(HC, HR, Body),
        HC = C, HR = Result,
        once(boltzmann_compliance:Body) ).

% ---------------------------------------------------------------------------
% Row emission
% ---------------------------------------------------------------------------

header(S) :-
    format(S, "leg\tconstraint_id\tsource_arm\teac_value\t", []),
    format(S, "result__arm_defect\tresult__arm_repaired\t", []),
    format(S, "engine_result\trecon_repaired\t", []),
    format(S, "T1\tT2\tT3\tT4\tagg_check\n", []).

row(S, Leg, SourceArm, C) :-
    safe_eval(once(boltzmann_compliance:epistemic_access_check(C, Sv)), Sv, EacO),
    render(EacO, Eac),

    safe_eval(engine_result(C, ER), ER, EngO),   render(EngO, Eng),
    safe_eval(recon_repaired(C, RR), RR, RecO),  render(RecO, Rec),

    safe_eval(t1(C, A), A, O1), render(O1, C1),
    safe_eval(t2(C, B), B, O2), render(O2, C2),
    safe_eval(t3(C, D), D, O3), render(O3, C3),
    safe_eval(t4(C, E), E, O4), render(O4, C4),

    agg_check(EacO, O1, O2, O3, O4, RecO, Agg),

    (   SourceArm == defect
    ->  Defect = Eng, Repaired = Rec
    ;   SourceArm == repaired
    ->  Defect = 'NOT_MEASURED', Repaired = Eng
    ;   Defect = 'ARM_UNKNOWN', Repaired = 'ARM_UNKNOWN' ),

    format(S, "~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\n",
           [Leg, C, SourceArm, Eac, Defect, Repaired, Eng, Rec, C1, C2, C3, C4, Agg]).

%% agg_check(...)  PREREG 0b: the per-test transcription must reproduce the
%  clause/2-composed repaired Result on every row that actually reaches the body.
agg_check(ok(S), ok(T1), ok(T2), ok(T3), ok(T4), ok(Rec), Verdict) :-
    S \== false, !,
    (   aggregate_tests(T1, T2, T3, T4, Mine)
    ->  ( Mine == Rec -> Verdict = match ; Verdict = 'MISMATCH' )
    ;   Verdict = 'AGG_FAILED' ).
agg_check(ok(S), _, _, _, _, _, 'not_reached') :- S == false, !.
agg_check(_, _, _, _, _, _, 'na').

% ---------------------------------------------------------------------------
% Driver
% ---------------------------------------------------------------------------

probe_leg(Leg, OutFile) :-
    catch(retractall(config:param(corpus_path, _)), _, true),
    asserta(config:param(corpus_path, Leg)),
    corpus_loader:load_all_testsets,
    (   corpus_loader:corpus_loaded
    ->  true
    ;   throw(probe_corpus_not_loaded(Leg)) ),
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    length(Cs, N),
    detect_source_arm(Arm),
    format(user_error, "[probe] leg=~w corpus_constraint_count=~w source_arm=~w~n",
           [Leg, N, Arm]),
    (   Arm == unknown
    ->  throw(probe_source_arm_unknown(Leg))
    ;   true ),
    setup_call_cleanup(
        open(OutFile, write, S),
        (   header(S),
            forall(member(C, Cs), row(S, Leg, Arm, C))
        ),
        close(S)),
    format(user_error, "[probe] leg=~w wrote ~w rows to ~w~n", [Leg, N, OutFile]).
