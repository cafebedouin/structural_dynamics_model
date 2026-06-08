:- module(temporal_residual, [
    constraint_time_set/2,
    snapshot_seq/3,
    residual_for_context/3,
    residual_report/2
]).

/* ===========================================================================
   Type-A observer residual detector  (snapshot floor, 2026-06-08; OQ-83)

   A (B)-CATEGORY READ-ONLY SEAM DIAGNOSTIC. It reads ONLY observer-axis output
   (drl_composition:classify_at_time/5 + narrative_ontology:measurement/5),
   reads NO cs_* predicate, and feeds NO computation — it is consumed only by
   the JSON exporter. Hub separation (v7 Theorem 7): committer drift descriptors
   are emitted separately by json_report; reconciliation is an OFFLINE join.

   The observer-temporal object is NOT a per-seat "arc" (a seat has no endogenous
   dynamics) — it is the sequence of FLIP-EVENTS: where a fixed seat's computed
   type changes across the authored timeline. The flip-count is the number that
   later decides the D-fork (authored d-series vs time-indexed roles), so it
   CARRIES ITS COMPOSITION: real (backed→backed) flips are reported apart from
   fabrication_adjacent_transitions (type-changes touching a snapshot whose ε or
   suppression was FABRICATED rather than authored at that time — phantom motion,
   per catch #3). Each context also carries a RAN-WITNESS (times_examined,
   backed_times) so a 0-flip reads as "looked across N times, found none," never
   "didn't look." No catch/3 wrapping here: a classifier error must surface
   loudly, not read as empty.

   Expected on the current corpus: empty (flips=0 everywhere). That is the
   ε-floor reporting the signal is not ε-driven — informative, not a no-op.
   =========================================================================== */

:- use_module(library(lists)).

%% constraint_time_set(+C, -Times)
%  Sorted unique union of authored measurement/5 time-points for C. The time-set
%  is the real authored grid — never an arbitrary or interpolated one.
constraint_time_set(C, Times) :-
    findall(T, narrative_ontology:measurement(_, C, _, T, _), Ts),
    sort(Ts, Times).

%% snapshot_seq(+C, +Context, -Seq)
%  Seq = time-ordered [state(T, Type, D, Eps, Supp, Theater, Backed)], one per
%  authored time-point, from the d-surfacing classify_at_time/5 (d is read off
%  the classifier, NOT recomputed — cheap-given-revival).
snapshot_seq(C, Context, Seq) :-
    constraint_time_set(C, Times),
    findall(state(T, Type, D, Eps, Supp, Theater, Backed),
            ( member(T, Times),
              drl_composition:classify_at_time(C, T, Context, Type,
                                               snap(D, Backed, Eps, Supp, Theater)) ),
            Seq).

%% residual_for_context(+C, +Context, -Res)
%  Res = ctx_residual(TimesExamined, BackedTimes, Flips, FabAdjacent).
residual_for_context(C, Context, ctx_residual(NT, NB, Flips, FabAdj)) :-
    snapshot_seq(C, Context, Seq),
    length(Seq, NT),
    include(is_backed_state, Seq, BackedSeq),
    length(BackedSeq, NB),
    scan_transitions(Seq, Flips, FabAdj).

is_backed_state(state(_,_,_,_,_,_,true)).

%% scan_transitions(+Seq, -Flips, -FabAdjacentCount)
%  Adjacent type-changes only. Both endpoints backed -> real flip; otherwise the
%  transition rides a fabricated snapshot -> excluded from flips, counted as
%  fabrication-adjacent (a HYGIENE counter: it mixes ε-gaps and cross-metric
%  sparsity — do not read it as signal).
scan_transitions([], [], 0).
scan_transitions([_], [], 0).
scan_transitions([S1, S2 | Rest], Flips, FabAdj) :-
    scan_transitions([S2 | Rest], Flips0, FabAdj0),
    S1 = state(T1, Ty1, _, E1, Su1, Th1, B1),
    S2 = state(T2, Ty2, _, E2, Su2, Th2, B2),
    (   Ty1 == Ty2
    ->  Flips = Flips0, FabAdj = FabAdj0
    ;   ( B1 == true, B2 == true )
    ->  safe_delta(E2,  E1,  DEps),
        safe_delta(Su2, Su1, DSupp),
        safe_delta(Th2, Th1, DTheater),
        Flips = [flip(T1, T2, Ty1, Ty2, DEps, DSupp, DTheater) | Flips0],
        FabAdj = FabAdj0
    ;   Flips = Flips0, FabAdj is FabAdj0 + 1
    ).

%% safe_delta(+A, +B, -D)  — V2-V1 when both numeric, else `null`.
safe_delta(A, B, D) :- number(A), number(B), !, D is A - B.
safe_delta(_, _, null).

%% residual_report(+C, -Report)
%  Report = [ctx(Label, ctx_residual(...))] over the canonical-4 (+default).
%  Per-stakeholder seats become available additively once stakeholders[] are
%  authored (0 today) — the place the motivating re-partition flips will live.
residual_report(C, Report) :-
    residual_contexts(Contexts),
    findall(ctx(Label, Res),
            ( member(Context, Contexts),
              context_label(Context, Label),
              residual_for_context(C, Context, Res) ),
            Report).

residual_contexts(Contexts) :-
    constraint_indexing:site_contexts_canonical(C4),
    constraint_indexing:default_context(Def),
    ( memberchk(Def, C4) -> Contexts = C4 ; Contexts = [Def | C4] ).

context_label(context(agent_power(P), time_horizon(T),
                      exit_options(E), spatial_scope(S)), Label) :-
    format(atom(Label), "~w/~w/~w/~w", [P, T, E, S]).
