% Probe: is δ (cognitive_displacement) load-bearing, or shadowed?
% Witness for OQ-162 (drained from OQ-69 ledger). Read-only, restores all overlays.
% Method: perturb δ via probe_harness:with_overlay/3 (caches cleared), diff χ
% against baseline across the canonical contexts for one corpus constraint.
%   - Negative control: no-op overlay (δ := 0.0, the default) must be byte-identical.
%   - Positive/experiment: δ := 0.3 must flip χ if δ flows through its sink
%     (resolve_displacement -> D_eff = clamp(D+δ) -> sigmoid -> χ).

:- initialization(main).

scores(Constraint, Contexts, Scores) :-
    findall(S,
            ( member(Ctx, Contexts),
              constraint_indexing:extractiveness_for_agent(Constraint, Ctx, S)
            ),
            Scores).

main :-
    [stack],
    use_module(probe_harness),
    corpus_loader:load_all_testsets,
    constraint_indexing:site_contexts_canonical(Contexts),
    % deterministically pick the first loaded corpus constraint that yields χ
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    once(( member(Constraint, Cs), scores(Constraint, Contexts, S0), S0 \== [] )),
    format("~n=== probe: δ load-bearing ===~n", []),
    format("constraint: ~w~n", [Constraint]),
    config:param(cognitive_displacement, DefDelta),
    config:param(cognitive_displacement_profile, Prof),
    format("default config: cognitive_displacement=~w profile=~w~n", [DefDelta, Prof]),

    % baseline (no overlay)
    scores(Constraint, Contexts, Base),
    format("~nBASELINE χ           : ~w~n", [Base]),

    % negative control: overlay δ := 0.0 (same as default) -> must equal baseline
    probe_harness:with_overlay(
        [config:param(cognitive_displacement, _)],
        [config:param(cognitive_displacement, 0.0)],
        scores(Constraint, Contexts, NegCtl)),
    format("NEG CTL  (δ:=0.0)    : ~w~n", [NegCtl]),
    ( Base == NegCtl
    -> format("  -> NEG CTL PASS: no-op overlay byte-identical (harness does not spuriously perturb)~n", [])
    ;  format("  -> NEG CTL FAIL: harness perturbs on a no-op overlay -- diff is UNTRUSTWORTHY~n", [])
    ),

    % experiment / positive control on δ's own sink: overlay δ := 0.3
    probe_harness:with_overlay(
        [config:param(cognitive_displacement, _)],
        [config:param(cognitive_displacement, 0.3)],
        scores(Constraint, Contexts, PosExp)),
    format("EXPERIMENT (δ:=0.3)  : ~w~n", [PosExp]),
    ( Base \== PosExp
    -> format("  -> δ LIVE: nonzero δ flips χ through its own sink => δ IS load-bearing when set~n", [])
    ;  format("  -> δ SHADOWED: nonzero δ leaves χ unchanged => δ not load-bearing (kill condition)~n", [])
    ),

    % restoration witness: baseline recomputes identical after overlays
    scores(Constraint, Contexts, Restored),
    ( Base == Restored
    -> format("RESTORE PASS         : ~w~n", [Restored])
    ;  format("RESTORE FAIL         : ~w (overlay leaked!)~n", [Restored])
    ),
    halt.
main :- format("PROBE FAILED TO RUN~n", []), halt(1).
