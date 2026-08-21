/* ===========================================================================
   A1 keystone probe — stakeholder-layer migration audit (2026-06-07)

   Claim under test: the COMPUTED classification path (dr_type/3, chi, signature,
   H0/H1) never reads the AUTHORED perspective facts
   (constraint_indexing:constraint_classification/3).

   Registers captured per run (COMPUTED lines; AUTHORED lines are the
   mutation-visibility mid-control, gotchas doc section 3, and are excluded
   from the invariance diff):
     - per-context dr_type over canonical-4 AND product-156
     - per-context chi (extractiveness_for_agent/3)
     - story-level signature (UNBOUND query, gotchas section 6)
     - H0/H1 (cohomological_obstruction/3; memo cache cleared by probe_harness)

   Runs (from prolog/ cwd):
     swipl -g "consult('../audits/2026-06-07_stakeholder_layer_migration/a1_probe.pl'), a1_baseline, halt" -t "halt(1)"
     swipl -g "consult('../audits/2026-06-07_stakeholder_layer_migration/a1_probe.pl'), a1_mut_perspective, halt" -t "halt(1)"
     swipl -g "consult('../audits/2026-06-07_stakeholder_layer_migration/a1_probe.pl'), a1_mut_metric, halt" -t "halt(1)"

   Mutation (i)  a1_mut_perspective: flips authored P1 snare -> mountain at
                 (powerless,biographical,trapped,national). Invariance claim:
                 COMPUTED lines byte-identical to baseline.
   Mutation (ii) a1_mut_metric: epsilon 0.35 -> 0.75 (both constraint_metric and
                 the domain_priors mirror). Positive control: COMPUTED lines must
                 move on EVERY register (type rows, chi, signature, H0/H1) —
                 a chi-only movement licenses only the chi half of (i)'s claim;
                 escalate with a suppression/theater overlay if any register
                 fails to move (operator-reviewed plan, 2026-06-07).
   =========================================================================== */

:- [stack].
:- use_module(probe_harness).   % NOT loaded by [stack] — witnessed exit=2 without it
:- corpus_loader:ensure_corpus_loaded.

a1_constraint(ai_governance_accountability).

% ---- capture ----
a1_capture :-
    a1_constraint(C),
    % AUTHORED substrate visibility (mid dispatch control; proves the mutated
    % run is not a byte-identical re-read of the same substrate)
    forall(constraint_indexing:constraint_classification(C, T0, Ctx0),
           format("AUTHORED_PERSP ~q ~q~n", [T0, Ctx0])),
    forall(narrative_ontology:constraint_metric(C, M, V),
           format("AUTHORED_METRIC ~q ~q~n", [M, V])),
    % COMPUTED: canonical-4
    constraint_indexing:site_contexts_canonical(C4),
    forall(member(Ctx, C4), a1_row(C, canonical, Ctx)),
    % COMPUTED: product-156
    constraint_indexing:site_contexts_product(C156),
    forall(member(Ctx2, C156), a1_row(C, product, Ctx2)),
    % COMPUTED: story-level signature (Sig UNBOUND — clause-order faithful)
    (   signature_detection:constraint_signature(C, Sig) -> true ; Sig = none ),
    format("COMPUTED SIG ~q~n", [Sig]),
    % COMPUTED: H0/H1
    grothendieck_cohomology:cohomological_obstruction(C, H0, H1),
    format("COMPUTED H0 ~q H1 ~q~n", [H0, H1]).

a1_row(C, Site, Ctx) :-
    (   drl_core:dr_type(C, Ctx, T) -> true ; T = no_type ),
    (   constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0)
    ->  Chi = Chi0 ; Chi = no_chi ),
    format("COMPUTED ~q ~q TYPE ~q CHI ~q~n", [Site, Ctx, T, Chi]).

% ---- runs ----
a1_baseline :- a1_capture.

a1_mut_perspective :-
    a1_constraint(C),
    Old = constraint_indexing:constraint_classification(C, snare,
              context(agent_power(powerless), time_horizon(biographical),
                      exit_options(trapped), spatial_scope(national))),
    New = constraint_indexing:constraint_classification(C, mountain,
              context(agent_power(powerless), time_horizon(biographical),
                      exit_options(trapped), spatial_scope(national))),
    % OQ-326 (2026-08-21) — INSTALLED-THEN-DRIFTED. This call THROWS at HEAD, and
    % that is CORPUS DRIFT, not a defect in this probe. When it ran (2026-06-07) the
    % overlay demonstrably INSTALLED: AUDIT.md records the mid-control diff
    %     < AUTHORED_PERSP snare    context(powerless,biographical,trapped,national)
    %     > AUTHORED_PERSP mountain context(powerless,biographical,trapped,national)
    % produced by a1_capture's forall over constraint_classification/3 below.
    % Today that predicate has ZERO fact clauses corpus-wide — 258 live testsets
    % declare it :- multifile and none author it — so the snapshot is empty and the
    % strict harness raises probe_overlay_partial/probe_overlay_empty.
    % NO retrofit wrapper is written, deliberately: nothing here declares a zero
    % (the artifact declares the OPPOSITE — that it worked), so a wrapper would
    % encode a present-day corpus fact as a property of this probe. The finding
    % stands on its June witness. Re-running requires a corpus that authors
    % constraint_classification facts; the June corpus is gone.
    probe_harness:with_overlay([Old], [New], a1_capture).

% eps overlay targets narrative_ontology:constraint_metric ONLY. Witnessed
% 2026-06-07: the computed path reads eps via drl_core:base_extractiveness ->
% constraint_data:base_extractiveness -> narrative_ontology:constraint_metric
% (drl_core.pl:84, constraint_data.pl:11-13); the domain_priors mirror is
% static (retract -> permission_error) and is NOT on the eps read path.
a1_mut_metric :-
    a1_constraint(C),
    config:param(extractiveness_metric_name, ExtName),
    probe_harness:with_overlay(
        [ narrative_ontology:constraint_metric(C, ExtName, _) ],
        [ narrative_ontology:constraint_metric(C, ExtName, 0.75) ],
        a1_capture).
