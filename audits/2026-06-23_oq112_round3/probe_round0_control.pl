% OQ-112 Round 3 — Round 0 POSITIVE CONTROL + Commit-2 mechanism (read-only/overlay).
% Round 0 part B found item-4 LATENT on 92 (all 86 claim-bearing constraints carry all
% three metrics). An empty result is a clean-grep until the probe is shown to DETECT a
% firing. Here we CONSTRUCT the firing by retracting a real constraint's theater and
% witness:
%   PC1  get_constraint_metrics else-branch FIRES -> fabricated theater=0.0 flows
%   PC2  theater-absence does NOT block dr_type (constraint stays classifiable) — so it
%        is collected into compute_type_profile(_,_,_,theater)'s findall metric-absent
%   PC3  CONTRAST: eps-absence (retract base_extractiveness) DOES block dr_type (is_X
%        calls it first) — confirming eps/chi drop at dr_type, theater survives = the
%        Commit-2 residual
%   MECH the sum_list-on-unknown behavior: is the Commit-2 case a LOUD throw (sum_list is
%        OUTSIDE the findall) or a SILENT drop? Settle it directly.

:- [stack].
:- use_module(probe_harness).
:- corpus_loader:ensure_corpus_loaded.

probe :-
    config:param(theater_metric_name, TheaterName),
    constraint_indexing:default_context(Ctx),

    % pick a real, classifiable, theater-bearing claim constraint
    once(( corpus_loader:corpus_constraint(C0),
           narrative_ontology:constraint_claim(C0, _),
           narrative_ontology:constraint_metric(C0, TheaterName, _),
           drl_core:dr_type(C0, Ctx, T0) )),
    format('control constraint C0 = ~w (baseline dr_type = ~w)~n~n', [C0, T0]),

    % baseline metrics (theater present)
    maxent_classifier:get_constraint_metrics(C0, E0, S0, Th0),
    format('baseline get_constraint_metrics(~w): eps=~w supp=~w theater=~w~n~n', [C0, E0, S0, Th0]),

    % ---- PC1 + PC2: retract theater, witness else-branch fires + dr_type survives ----
    probe_harness:with_retracted(
        [narrative_ontology:constraint_metric(C0, TheaterName, _)],
        ( maxent_classifier:get_constraint_metrics(C0, E1, S1, Th1),
          format('PC1: theater retracted -> get_constraint_metrics: eps=~w supp=~w theater=~w~n', [E1, S1, Th1]),
          format('PC1 verdict: theater else-branch ~w~n',
                 [(Th1 == 0.0 -> 'FIRED (fabricated 0.0 — item-4 firing reproduced)' ; 'did NOT fire')]),
          ( catch(drl_core:dr_type(C0, Ctx, T1), Edt, (T1 = error(Edt))) -> true ; T1 = failed ),
          format('PC2: dr_type(theater-absent ~w) -> ~w~n', [C0, T1]),
          format('PC2 verdict: theater-absence ~w dr_type (constraint ~w the theater findall)~n~n',
                 [(T1 == failed -> 'BLOCKS' ; 'does NOT block'),
                  (T1 == failed -> 'is dropped from' ; 'ENTERS')])
        )),

    % ---- PC3: contrast — eps-absence blocks dr_type ----
    % OQ-340 (2026-08-21) — DELIBERATELY NOT MIGRATED; this call THROWS at HEAD.
    % Both templates below are RULE-BEARING (drl_core:base_extractiveness/2 has 2
    % rule clauses, constraint_data:base_extractiveness/2 has 1). snapshot/2
    % collected FACTS only, so this arm retracted nothing of the rule-derived eps
    % path and ran against the UNMODIFIED program — meaning the PC3 verdict below
    % ("eps-absence BLOCKS dr_type") may have been reported off a diff that was
    % never a diff. The Goal only observes and prints; nothing here fails on a
    % no-op, so the run could not tell the two apart.
    % No retrofit wrapper: nothing in this artifact DECLARES a partial overlay, so
    % a wrapper would manufacture greenness after the fact. The fix is to retract
    % the FACT table the rules read (narrative_ontology:constraint_metric/3), as
    % a1_probe.pl:87 does, and re-run — see OQ-340.
    probe_harness:with_retracted(
        [drl_core:base_extractiveness(C0, _), constraint_data:base_extractiveness(C0, _)],
        ( maxent_classifier:get_constraint_metrics(C0, E2, S2, Th2),
          format('PC3: base_extractiveness retracted -> get_constraint_metrics: eps=~w supp=~w theater=~w~n', [E2, S2, Th2]),
          ( catch(drl_core:dr_type(C0, Ctx, T2), Edt2, (T2 = error(Edt2))) -> true ; T2 = failed ),
          format('PC3: dr_type(eps-absent ~w) -> ~w~n', [C0, T2]),
          format('PC3 verdict: eps-absence ~w dr_type (eps/chi drop at dr_type; only theater survives)~n~n',
                 [(T2 == failed -> 'BLOCKS' ; 'does NOT block')])
        )),

    % ---- MECH: sum_list on a list containing unknown (post-Commit-1 theater value) ----
    format('MECH: compute_type_profile structure — sum_list is OUTSIDE the findall.~n'),
    ( catch(sum_list([0.1, unknown, 0.2], Sum), Esum, (Sum = error(Esum))) -> true ; Sum = failed ),
    format('MECH: sum_list([0.1, unknown, 0.2]) -> ~w~n', [Sum]),
    format('MECH verdict: an unknown reaching the theater findall makes sum_list ~w~n',
           [(compound(Sum), Sum = error(_) -> 'THROW (LOUD, propagates out of compute_type_profile -> crashes maxent_precompute; NOT a silent drop)' ; 'succeed')]).

:- (catch(probe, E, (format('PROBE ERROR: ~w~n', [E]), fail)) -> true ; format('PROBE FAILED~n')), halt.
:- halt(1).
