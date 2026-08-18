/* disagreement_probe.pl — Phase 1.3 read-only probe (bound is_X vs engine assignment).

For every corpus_constraint/1 story x canonical context: run each live bound-caller
alias is_X(C, Ctx, R) exactly as the hot path does, and separately compute the SAME
metrics and take the engine's own first solution via once(classify_from_metrics(...)).
A disagreement row = is_X succeeded with its atom while the engine's first solution is
a DIFFERENT atom: the bound call manufactured a classification the cascade would not
assign. Pre-stated asymmetry (plan): bound-agree only says the bodies happened to be
exclusive on this corpus; any disagreement is a manufactured classification in live
output.

Load chain: [stack] + corpus_loader:load_all_testsets — classify_from_metrics/6 is
pre-signature and metric-only (no MaxEnt fit needed; confirmed by reading its bodies:
config params + narrative_ontology facts + effective_theater_ratio only).

Output: DP_ROW lines (membership list), DP_POSCTL (positive control), DP_SUMMARY.
Run: cd prolog && swipl -q -l ../audits/2026-08-17_bound_dispatch_hardening/disagreement_probe.pl \
       -g "run_probe, halt" -t "halt(1)"
*/

:- [stack].

alias_type(mountain).
alias_type(snare).
alias_type(scaffold).
alias_type(rope).
alias_type(tangled_rope).
alias_type(piton).

% Planted control clause: for the synthetic id only, the alias layer "succeeds bound"
% while engine_type/3 has no solution (no authored facts) — probe_one must emit a row
% through its REAL emitter path. Scope: fires only on dp_ctl_synthetic, which is not a
% corpus member, so it cannot contaminate the live sweep.
is_alias(snare, dp_ctl_synthetic, ctl_ctx, snare) :- !.
is_alias(mountain, C, Ctx, R)     :- drl_core:is_mountain(C, Ctx, R).
is_alias(snare, C, Ctx, R)        :- drl_core:is_snare(C, Ctx, R).
is_alias(scaffold, C, Ctx, R)     :- drl_core:is_scaffold(C, Ctx, R).
is_alias(rope, C, Ctx, R)         :- drl_core:is_rope(C, Ctx, R).
is_alias(tangled_rope, C, Ctx, R) :- drl_core:is_tangled_rope(C, Ctx, R).
is_alias(piton, C, Ctx, R)        :- drl_core:is_piton(C, Ctx, R).

% Engine answer over the same metric computation the aliases perform.
engine_type(C, Ctx, T) :-
    drl_core:base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Ctx, Chi),
    drl_core:get_raw_suppression(C, Supp),
    once(drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, T)).

probe_one(C, Ctx) :-
    (   engine_type(C, Ctx, Engine) -> true ; Engine = no_engine_solution ),
    forall(
        alias_type(Type),
        (   (   is_alias(Type, C, Ctx, R), R == Type
            ->  BoundSaysYes = true
            ;   BoundSaysYes = false
            ),
            (   BoundSaysYes == true, Engine \== Type
            ->  format("DP_ROW: ~w ~w bound=~w engine=~w~n", [C, Ctx, Type, Engine])
            ;   true
            )
        )
    ).

% Positive control: a SYNTHETIC context probe proving the row-emitter fires when a
% disagreement exists. We assert a fabricated fact pair via the two-face trick:
% is_alias is our own wrapper, so plant a wrapper solution that must disagree.
% Same-path positive control: probe_one/2 on the planted synthetic id must emit a row
% through the REAL emitter (alias layer says snare; engine_type/3 has no solution).
% This demonstrates the row-emitter FIRES; it does not demonstrate that
% classify_from_metrics bodies overlap on real data — that is what the sweep measures,
% and a zero-row sweep is therefore "no witnessed disagreement on this corpus", not
% "overlap impossible" (prereg outcome semantics).
positive_control :-
    (   with_output_to(string(S), probe_one(dp_ctl_synthetic, ctl_ctx)),
        sub_string(S, _, _, _, "DP_ROW: dp_ctl_synthetic")
    ->  format("DP_POSCTL: same-path row emitter fired on planted case OK~n")
    ;   format("DP_POSCTL: FAILED — row emitter did not fire on planted case~n")
    ).

run_probe :-
    corpus_loader:load_all_testsets,
    positive_control,
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, NC),
    constraint_indexing:site_contexts_canonical(Ctxs),
    length(Ctxs, NCtx),
    forall(( member(C, Cs), member(Ctx, Ctxs) ), probe_one(C, Ctx)),
    format("DP_SUMMARY: ~w constraints x ~w contexts probed~n", [NC, NCtx]).
