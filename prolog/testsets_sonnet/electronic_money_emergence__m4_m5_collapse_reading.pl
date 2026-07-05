% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: The M4/M5 Aggregate Split as Retroactive Category-Creator for 'Electronic Money'
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   Central banks split broad money aggregates (M4 vs. M5, or local
 *   equivalents) for internal reporting convenience: certain instruments were
 *   reclassified as they crossed liquidity or dematerialization thresholds
 *   defined by statistical convention, not by any change in the instruments
 *   themselves. Historians and commentators later read the date of the
 *   aggregate split as 'the date electronic money emerged,' producing a clean
 *   origin story that the underlying technological and social history does
 *   not support — payment dematerialization was gradual, contested, and had
 *   no single moment. The claimed type here is piton: a classificatory
 *   boundary that has atrophied from a working statistical tool into an
 *   inertial artifact repeatedly mistaken for a historical event, maintained
 *   mostly because revising it would break time-series continuity, not
 *   because it tracks anything real about when digital money began.
 *
 * KEY AGENTS:
 *   - central_bank_statistics_departments: agenda_setter (institutional/arbitrage) — draws and maintains the aggregate boundary
 *   - monetary_aggregate_theorists: beneficiary (organized/constrained) — builds policy models on the boundary's apparent objectivity
 *   - monetary_historians: payer (moderate/constrained) — bears the cost of disentangling artifact from event
 *   - policy_analysts_relying_on_aggregate_continuity: payer (moderate/constrained) — inherits false precision in policy timing arguments
 *   - general_public_using_electronic_payment_instruments: excluded (powerless/trapped) — actually used the instruments, has no voice in the dating narrative
 *   - philosophers_of_measurement: observer (analytical/analytical) — names the reification mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "The M4/M5 Aggregate Split as Retroactive Category-Creator for 'Electronic Money'").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, 'be6641d2-64f9-4143-8299-f7e5ad143a97').
narrative_ontology:cs_kernel_codification('be6641d2-64f9-4143-8299-f7e5ad143a97', distributed).
narrative_ontology:cs_authority_grounding('be6641d2-64f9-4143-8299-f7e5ad143a97', extraction).
narrative_ontology:cs_interpretation_layer_present('be6641d2-64f9-4143-8299-f7e5ad143a97').
narrative_ontology:cs_reading_relation('be6641d2-64f9-4143-8299-f7e5ad143a97', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('be6641d2-64f9-4143-8299-f7e5ad143a97', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('be6641d2-64f9-4143-8299-f7e5ad143a97', foundational, measurement_convention_is_not_ontological_event).
narrative_ontology:cs_axiom_status(measurement_convention_is_not_ontological_event, holdable).
narrative_ontology:cs_axiom_grounding('be6641d2-64f9-4143-8299-f7e5ad143a97', measurement_convention_is_not_ontological_event, conventional).
narrative_ontology:cs_axiom('be6641d2-64f9-4143-8299-f7e5ad143a97', secondary, aggregate_boundary_revision_carries_no_historical_evidentiary_weight).
narrative_ontology:cs_axiom_status(aggregate_boundary_revision_carries_no_historical_evidentiary_weight, holdable).
narrative_ontology:cs_axiom_grounding('be6641d2-64f9-4143-8299-f7e5ad143a97', aggregate_boundary_revision_carries_no_historical_evidentiary_weight, empirically_contingent).
narrative_ontology:cs_reference_frame('be6641d2-64f9-4143-8299-f7e5ad143a97', aggregate_boundary_as_reporting_convention).
narrative_ontology:cs_drift_state('be6641d2-64f9-4143-8299-f7e5ad143a97', popular_historiographical_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be6641d2-64f9-4143-8299-f7e5ad143a97', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_theorists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregate_continuity).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_classification_is_a_natural_kind).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and maintains the M4/M5 (or equivalent broad-aggregate) reporting boundary, deciding which instruments count as 'money' for statistical purposes. Revising the boundary is costly (breaks time series, requires re-benchmarking historical data), so once a distinction is drawn it tends to persist and gets narrated as tracking a real economic event rather than as a bookkeeping choice. Faces essentially no penalty for treating the boundary as descriptive rather than constructed.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, beneficiary).

% Builds models and policy recommendations on the existence of a clean electronic-money category that the aggregate split appears to certify. Careers and forecasting frameworks are invested in the aggregate boundary being a discovery about the economy rather than an artifact of how a report was formatted. Benefits from the category's apparent objectivity without having created the boundary itself.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_theorists, beneficiary,
    organized, biographical, constrained, national).

% Attempts to date and explain 'the emergence of electronic money' as a historical event, only to find the dating collapses into the date a statistical convention changed. Bears the cost of untangling a measurement artifact from a substantive one, redoing analysis whenever the realization surfaces, and being told the effort is pedantic because the aggregate has already hardened into common usage.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, payer,
    moderate, generational, constrained, national).

% Uses the M4/M5 split as an input to policy timing arguments (e.g. 'electronic money changed monetary velocity starting in year X'), inheriting the artifact's false precision. Cannot easily substitute an alternative dataset because the aggregate is the only continuous series available, so the artifact's error propagates into their conclusions without their consent.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregate_continuity, payer,
    moderate, biographical, constrained, national).

% Actually held and used the instruments (debit balances, giro transfers, early card-linked accounts) the aggregate later reclassified. Their lived adoption of the technology preceded, and is unrelated to, the date the statistical category was drawn, but they have no voice in how the historical narrative of 'when electronic money began' gets constructed from the aggregate data.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, general_public_using_electronic_payment_instruments, excluded,
    powerless, biographical, trapped, national).

% Examines the general phenomenon of statistical categories retroactively constituting the objects they claim to measure — the reification of a reporting convention into a historical event. Takes no side in monetary policy but names the structural mechanism at work.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, philosophers_of_measurement, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The M4/M5 split solves a genuine and narrow coordination problem: central banks need a stable, auditable way to report broad money supply changes for policy communication and cross-period comparison. A fixed classification boundary lets analysts compare 'M4' across years without re-deriving the definition each time.
% TRANSFER_FUNCTION: The arrangement transfers narrative authority — the power to say WHEN a historical phenomenon began — from the people who actually built and used the instruments (payment technologists, bank customers) to the institution that drew the reporting line. It moves apparent objectivity (statistical certification) from a bookkeeping choice to a historiographical claim, without moving any money.
% ABSENT_VOICES: The engineers who built giro and card-clearing infrastructure, and the ordinary account-holders who used dematerialized balances years before any aggregate reclassification, never testify to when 'electronic money' began — the statistical departments speak for them retroactively through the aggregate.
% DISAPPEARANCE_RATIONALE: If the M4/M5 boundary were redrawn or abolished tomorrow, no payment instrument would change, no bank balance would move, and no technology would stop working — only the historiographical narrative that leans on the aggregate's date would lose its anchor. This is the diagnostic signature of a measurement artifact rather than a coordination structure with real stakes: removing it rearranges papers, not arrangements.
% FOUNDING_PROBLEM: Central banks needed a stable, comparable statistical boundary for reporting broad money supply for policy communication — a genuine bookkeeping problem, not a problem about dating technological history.
% FOUNDING_PROBLEM_CORROBORATION: Central bank statistics departments (the benefiting party) attest the boundary still serves live reporting needs. Independent corroboration comes from monetary historians and measurement philosophers outside the beneficiary set, who attest that the reporting function is legitimate and live but is a DIFFERENT founding problem than 'when did electronic money emerge' — the aggregate was never built to answer, and cannot honestly answer, the historical-emergence question it gets cited for.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 by 2020) and rises slowly — the cost this constraint imposes is not financial extraction but epistemic distortion: policy and historical arguments built on a false origin date. Suppression is low (0.22) because no one is coerced into believing the emergence narrative; the mechanism works through convenience and path dependency, not force. Theater ratio is high and rising (0.30 -> 0.71) because an increasing share of the aggregate's institutional maintenance is now about defending a historical narrative the boundary was never built to support, rather than about the reporting function it genuinely serves. Accessibility collapse is moderate (0.4): the artifact nature of the 'emergence' is recoverable by anyone who reads the original methodology notes, but in practice the compressed popular narrative displaces that recovery for most users of the aggregate.
 *
 * DIRECTIONALITY LOGIC:
 *   Statistics departments and aggregate theorists sit near the beneficiary end: they collect the apparent objectivity and predictive tidiness of a clean category without bearing the cost of its historiographical misuse. Historians and policy analysts sit nearer the target end: they inherit a false precision they did not create and must either accept it uncritically or spend real effort correcting it. The general public — who actually held and used the instruments in question — are structurally excluded from the dating conversation entirely; their exit option is 'trapped' not because they are coerced but because the narrative is constructed entirely without reference to their lived adoption timeline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a stable statistical boundary for policy reporting) remains live and legitimate — this is NOT mandatrophy of the reporting function itself. The mandatrophy is narrower and more specific: the aggregate's boundary has been asked to answer a question (when did electronic money emerge?) that it was never built to answer, and continues to be cited for that purpose because no one has formally revoked its authority to do so. Classifying this as piton rather than snare or mountain prevents two errors: treating the reporting function as pure extraction (it genuinely coordinates policy communication — that part is fine) and treating the emergence-dating use as natural law (it is not; it is a convention mistaken for a discovery, sustained by inertia rather than by anyone actively defending a false claim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_basis,
    'Given the same historical record, why adopt the collapse reading (no genuine emergence event) rather than the became_thinkable or first_held readings, which both posit a real dateable event?',
    'Examine whether any candidate ''emergence'' date proposed under the sibling readings survives contact with the underlying primary technological/social record independent of the aggregate reclassification date. If proposed dates cluster around aggregate-revision years rather than around independently documented technology-adoption or first-use events, that supports the collapse reading; if independently corroborated adoption dates diverge cleanly from aggregate-revision dates, that undercuts it.',
    'If independently corroborated dates exist and diverge from the statistical boundary''s revision dates, the collapse reading is falsified for at least one candidate emergence event, and the became_thinkable_reading or first_held_reading gains ground as the historically operative account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the choice among sibling readings of the kernel is itself empirically adjudicable or purely a matter of framing.').

omega_variable(
    boundary_revision_cost_vs_narrative_authority,
    'Is the persistence of the M4/M5-as-emergence-marker narrative driven by genuine measurement inertia (revising the series is technically costly) or by an interest in retaining the narrative authority the clean category confers on statistics departments and aggregate theorists?',
    'Compare episodes where the aggregate boundary WAS revised (e.g. due to new instrument types) against whether the revising institution issued any public correction to historiographical claims that had relied on the old boundary''s implied dating. Absence of correction despite technical capacity to revise would support an interest-driven persistence account over a pure-inertia account.',
    'If correction never occurs despite technical ease, the piton classification should be revisited toward a milder tangled_rope reading (genuine coordination function plus an uncorrected, low-grade extraction of narrative authority); if correction has occurred and simply failed to propagate, piton (pure inertia, no one benefiting enough to fix it) is the better fit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_revision_cost_vs_narrative_authority, empirical, 'Whether the artifact persists from inertia alone or from an uncorrected benefit to the beneficiary seats.').

omega_variable(
    measurement_artifact_naturalization_risk,
    'Could the collapse reading itself be over-claiming naturalness in the opposite direction — asserting too confidently that NO event occurred, when partial technological thresholds (e.g. first cross-bank real-time settlement) might constitute a genuine, if fuzzy, emergence event that the aggregate merely mis-dates rather than wholly fabricates?',
    'Independent technological history review, cross-referenced against multiple national aggregate-revision dates: if genuine emergence events cluster around similar underlying technology milestones across jurisdictions with different statistical conventions, a real event exists that the aggregate merely mis-times, rather than a pure artifact.',
    'Would soften the collapse reading''s ''no genuine emergence event'' claim toward ''a real but poorly-dated event,'' shifting this reading closer to the first_held_reading rather than remaining fully distinct from it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_artifact_naturalization_risk, conceptual, 'Whether the collapse reading''s core denial (no event, only artifact) is itself over-stated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.63).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.71).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.26).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.03).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% Three-story kernel decomposition of 'the emergence of electronic money.' became_thinkable_reading locates emergence at conceptual/technical possibility (a Mountain-leaning or Rope-leaning reading, since no institution profits from possibility per se). first_held_reading locates emergence at first institutional custody of dematerialized currency (likely Rope or Tangled Rope, since specific institutional actors can be named as first-movers and beneficiaries). This story (m4_m5_collapse_reading) denies a discrete emergence event altogether, reclassifying the entire question as an artifact of a statistical reporting boundary — Piton, since the boundary's real coordination function (policy reporting) is intact and legitimate while its historiographical use has atrophied into inertial, low-stakes misattribution. All three stories are ε-invariant readings of the same natural-language claim ('electronic money emerged') and must not be merged or averaged; they are linked here so contamination/coupling analysis can trace how a resolution of the empirical omega in one story would bear on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
