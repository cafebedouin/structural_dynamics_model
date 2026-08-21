/* ===========================================================================
   A2 straitjacket probe — stakeholder-layer migration audit (2026-06-07)

   Claim under test: directionality keys on the power ATOM, not agent identity;
   two opposed powerful agents in one story collapse to one d/chi/type.

   Design: the context/4 term has no agent-identity slot, so "which powerful
   agent" cannot even be expressed. The probe shows the d-derivation consumes
   only EXISTENCE booleans (has-victims / has-beneficiaries) + the power atom:
     - identity mutations (remove ONE victim, either one): d UNCHANGED
     - existence mutation (remove ALL victims): d MOVES  <- same-probe positive
       control proving the unchanged results above are witnessed nulls
     - directionality_override(C, powerful, 0.9): the ONLY d-handle is the
       atom — every "powerful agent" in the story moves together.

   Predictions (named in advance, derived from the clause chain
   constraint_indexing.pl derive_directionality -> power_role_heuristic ->
   exit_modulation): baseline d=0.50 (powerful, HasVictims=true, mobile +0.00);
   one-victim-removed d=0.50; other-victim-removed d=0.50;
   all-victims-removed d=0.46 (HasVictims=false clause); override d=0.9.

   Run (from prolog/ cwd):
     swipl -g "consult('../audits/2026-06-07_stakeholder_layer_migration/a2_probe.pl'), a2_run, halt" -t "halt(1)"
   =========================================================================== */

:- [stack].
:- use_module(probe_harness).   % NOT loaded by [stack]
:- corpus_loader:ensure_corpus_loaded.

a2_ctx(context(agent_power(powerful), time_horizon(biographical),
               exit_options(mobile), spatial_scope(national))).

a2_report(Label) :-
    C = ai_governance_accountability,
    a2_ctx(Ctx),
    constraint_indexing:derive_directionality(C, Ctx, D),
    (   drl_core:dr_type(C, Ctx, T) -> true ; T = no_type ),
    constraint_indexing:extractiveness_for_agent(C, Ctx, Chi),
    findall(V, narrative_ontology:constraint_victim(C, V), Vs),
    format("A2 ~w: d=~q type=~q chi=~q victims=~q~n", [Label, D, T, Chi, Vs]).

a2_run :-
    C = ai_governance_accountability,
    a2_report(baseline),
    probe_harness:with_retracted(
        [narrative_ontology:constraint_victim(C, workers_displaced_without_recourse)],
        a2_report(one_victim_removed)),
    probe_harness:with_retracted(
        [narrative_ontology:constraint_victim(C, communities_facing_discriminatory_systems)],
        a2_report(other_victim_removed)),
    probe_harness:with_retracted(
        [narrative_ontology:constraint_victim(C, _)],
        a2_report(all_victims_removed)),
    probe_harness:with_asserted(
        [reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: no template, so no declared query shape (OQ-326 clause 4')"),
          constraint_indexing:directionality_override(C, powerful, 0.9))],
        a2_report(override_powerful_0_9)),
    a2_report(post_restore_control).
