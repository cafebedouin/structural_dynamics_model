% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Displacement of Prior Practice (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state issues decrees abolishing a prior calendar system and
 *   mandating new dress norms, on the theory that legal authority alone is
 *   sufficient to displace entrenched practice: compliance is expected to
 *   follow from the mandate regardless of whether the population internalizes
 *   the new forms. This story instantiates the exogenous_override_reading of
 *   the legitimacy_of_imposed_practice kernel. Per the expected structural
 *   delta, the calendar component behaves as near-pure override — legally
 *   abolished, administratively imposed, but met with substantial rural
 *   non-compliance and durable practical workarounds (dual-dating, informal
 *   reversion) — while the dress component is a partial override: coercive
 *   enforcement (fines, inspections, exclusion from services) achieves real
 *   but incomplete displacement, concentrated in urban and state-facing
 *   contexts. The beneficiary is the state modernization agenda and its
 *   administrative apparatus, who capture legitimacy and legibility gains
 *   regardless of rural internalization; the victims are rural populations
 *   and traditional practice communities who bear adjustment costs,
 *   dual-system friction, and loss of the coordination function the old
 *   practice performed, without having been consulted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Displacement of Prior Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '59f18e2a-0f56-4217-affc-dd8ea967c086').
narrative_ontology:cs_kernel_codification('59f18e2a-0f56-4217-affc-dd8ea967c086', formalized).
narrative_ontology:cs_authority_grounding('59f18e2a-0f56-4217-affc-dd8ea967c086', extraction).
narrative_ontology:cs_interpretation_layer_present('59f18e2a-0f56-4217-affc-dd8ea967c086').
narrative_ontology:cs_reading_relation('59f18e2a-0f56-4217-affc-dd8ea967c086', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('59f18e2a-0f56-4217-affc-dd8ea967c086', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('59f18e2a-0f56-4217-affc-dd8ea967c086', foundational, legal_mandate_is_self_executing_displacement).
narrative_ontology:cs_axiom_status(legal_mandate_is_self_executing_displacement, holdable).
narrative_ontology:cs_axiom_grounding('59f18e2a-0f56-4217-affc-dd8ea967c086', legal_mandate_is_self_executing_displacement, conventional).
narrative_ontology:cs_axiom('59f18e2a-0f56-4217-affc-dd8ea967c086', secondary, internalization_is_immaterial_to_legitimate_compliance).
narrative_ontology:cs_axiom_status(internalization_is_immaterial_to_legitimate_compliance, holdable).
narrative_ontology:cs_axiom_grounding('59f18e2a-0f56-4217-affc-dd8ea967c086', internalization_is_immaterial_to_legitimate_compliance, instrumental).
narrative_ontology:cs_reference_frame('59f18e2a-0f56-4217-affc-dd8ea967c086', decree_authority_self_sufficiency).
narrative_ontology:cs_drift_state('59f18e2a-0f56-4217-affc-dd8ea967c086', post_rollout_enforcement_cycles, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59f18e2a-0f56-4217-affc-dd8ea967c086', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_administrative_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, local_enforcement_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree abolishing the prior calendar and mandating the new dress code, backed by legal penalties, police enforcement, and administrative record-keeping requirements that only recognize the new forms. Treats legal mandate as sufficient cause for compliance and does not build consultation or internalization mechanisms into the rollout.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Collects legitimacy and international recognition from visible markers of modernization — synchronized calendars for administration and trade, standardized dress read as civilizational alignment by foreign observers. Benefits regardless of whether rural populations actually internalize the changes, since compliance is measured by decree existence and urban visibility, not lived practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    institutional, civilizational, analytical, national).

% Adopts the new calendar and dress relatively quickly because career advancement, government employment, and urban social standing are now contingent on visible compliance. Bears some adjustment cost (new clothing expense, recalculating schedules) but the cost is offset by proximity to state institutions and access to the benefits of compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class, payer).

% Continue using the prior calendar for agricultural, religious, and market timing because the decree offers no substitute infrastructure for the functions the old calendar served locally; run a dual system where state paperwork uses the new dates and daily life uses the old. Face fines, harassment, or exclusion from official services for dress non-compliance but often revert once enforcement attention moves elsewhere. Bear the confusion and transaction cost of straddling two systems with no consultation on the transition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, regional).

% Hold the calendrical and dress knowledge that structured ritual, seasonal, and kinship obligations for generations. The decree does not engage this function at all — it substitutes a legal category for a lived one — leaving communities to either quietly maintain the old practice underground or lose the coordination the practice performed, with no state-provided replacement for either the ritual or the coordination function.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities, payer,
    powerless, generational, trapped, local).

% Charged with enforcing dress compliance and processing administrative business only in the new calendar, but operate inside communities that continue the old practice. Selectively enforce — cracking down during inspections or state visits, tolerating reversion otherwise — because full enforcement against the whole rural population is not sustainable with available manpower.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, local_enforcement_officials, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, local_enforcement_officials, payer).

% Read the decree and visible urban compliance as evidence of state capacity and civilizational alignment, which affects diplomatic recognition, loan terms, and trade relationships. Do not investigate rural non-compliance closely because the decree's existence, not its lived uptake, is what they are pricing.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, foreign_observers_and_creditors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes administrative, legal, and commercial time-reckoning and self-presentation with the international/urban systems the state wants to be legible to, replacing a patchwork of local calendars and dress codes with a single official standard.
% TRANSFER_FUNCTION: Moves legitimacy and legibility to the central state and its modernization narrative; moves adjustment costs — confusion, dual bookkeeping, fines, loss of ritual coordination function, cost of new dress — onto rural and traditional communities who were not consulted on the change and receive none of the international-recognition benefit.
% ABSENT_VOICES: Rural populations and traditional practice communities were not consulted before the decree; their objection — that the old calendar and dress performed real local coordination functions the decree does not replace — is not represented in the administrative record, which measures compliance by decree existence rather than lived adoption.
% DISAPPEARANCE_RATIONALE: If the decree were rescinded, urban administrative life would need to renegotiate its now-embedded new-calendar recordkeeping and international-facing practices (real rearrangement there), while rural daily life would barely change since the old calendar and dress never actually left — the world is already partly unchanged for the rural seat and would substantially rearrange for the urban/international seat. This asymmetry is the verdict.
% FOUNDING_PROBLEM: The state's founding problem, as decreed: fragmented pre-existing calendars and dress markers signaled backwardness to foreign powers and impeded uniform administration, taxation, and legal recordkeeping across regions.
% FOUNDING_PROBLEM_CORROBORATION: The central administrative apparatus and international observers attest the problem was real and is now resolved by decree. Rural populations and traditional practice communities — outside the beneficiary set — attest the underlying coordination problem the old calendar solved locally was never addressed, only overwritten on paper; ethnographic and administrative-noncompliance records from the affected regions corroborate persistent dual-system practice rather than internalized replacement.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly over the interval (0.55 to 0.68) as the administrative and international-legibility benefits of the decree compound while the underlying cost to rural populations of running two parallel systems persists. Suppression is high but declines slightly then partially rebounds (0.85 to 0.75 to 0.79), reflecting the enforcement-attention cycle local officials describe: intense at rollout, relaxed as full-population enforcement proves unsustainable, then partially reasserted during state visits or inspection waves. Theater ratio rises steadily (0.2 to 0.42) as administrative recordkeeping increasingly reports compliance by decree-existence and urban visibility rather than by measuring actual rural uptake — the state's own metrics substitute the proxy (decree issued, urban dress observed) for the target (practice actually displaced).
 *
 * PERSPECTIVAL GAP:
 *   From the central administrative apparatus's seat, this reads as successful coordination: a single legible standard replacing fragmentation, achieved efficiently through legal mandate. From the rural population's seat, the same decree is an imposed cost with no functional replacement for what the old calendar and dress did locally — coordination of planting, ritual, and kinship obligations — and the persistence of underground dual-system practice is the direct evidence that legal mandate alone did not displace the function, only the official record of it.
 *
 * DIRECTIONALITY LOGIC:
 *   The central administrative apparatus and the state modernization agenda sit at the beneficiary end: they collect legitimacy, administrative uniformity, and international recognition whether or not rural practice actually changes, because the metric they are judged by is decree existence, not lived adoption. Rural populations and traditional practice communities sit at the target end: trapped exit options (no legal alternative to the new calendar for official business, no consultation channel), bearing the transaction costs of a dual system imposed without their input. The urban administrative class occupies an intermediate position — real adjustment cost, but proximity to state benefits offsets it, which is why they comply faster and more durably than rural populations without needing the same enforcement intensity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, internationally illegible calendars and dress) is contested as live or dead: the state and foreign observers treat it as solved by the decree's existence; rural non-compliance evidence suggests the underlying local coordination problem the old system solved was never addressed, meaning the decree substituted a legal fiction for it. Classifying this as tangled_rope rather than a pure snare or pure mountain resists two mislabelings: treating decree-sufficiency as a natural fact about political authority (which would hide the extraction of costs onto non-consulted rural populations), and treating the whole arrangement as pure extraction with no coordination function (which would hide the genuine administrative-legibility problem the state apparatus does solve for itself and for international counterparties).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_sufficiency_vs_erosion,
    'Does exogenous decree authority genuinely displace prior practice on its own terms, or does the apparent compliance measured by the state (decree existence, urban visibility, official recordkeeping) mask a durable underlying non-compliance that decree authority never actually overcomes?',
    'Longitudinal ethnographic and administrative-record comparison between officially reported compliance rates and independently observed practice rates in rural regions over multiple enforcement cycles; persistence of dual-system practice after enforcement attention lapses would indicate the override was never structurally sufficient, only administratively declared sufficient.',
    'If override proves insufficient on independent observation, the exogenous_override_reading''s core premise is empirically weakened relative to the hybrid_scaffolding_reading, which predicts exactly this partial-displacement pattern; this would not reclassify THIS story (which is authored as the override premise regardless of its ultimate success) but would strengthen the case for treating the hybrid reading as the more descriptively accurate account of the same historical episode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_sufficiency_vs_erosion, empirical, 'Whether decree-sufficiency is real displacement or administrative fiction over persistent rural non-compliance.').

omega_variable(
    calendar_dress_asymmetry_origin,
    'Why does the calendar override fail almost completely in rural areas while the dress override achieves partial, durable displacement — is this because dress is more publicly visible and hence more enforceable, or because dress lacks the deep functional embedding (ritual and agricultural timing) that the calendar has?',
    'Compare enforcement mechanisms and functional substitutability for each artifact: if dress compliance tracks visibility/enforcement intensity and calendar non-compliance tracks loss of irreplaceable local function, the asymmetry is functional, not enforcement-based.',
    'If the asymmetry is functional (calendar performs an irreplaceable local coordination function dress does not), this predicts which future imposed-practice cases will show similar override failure, and argues the exogenous_override_reading systematically overstates displacement for functionally embedded practices while remaining roughly accurate for surface/visible markers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calendar_dress_asymmetry_origin, conceptual, 'Whether calendar/dress override asymmetry reflects enforceability or functional embeddedness.').

omega_variable(
    beneficiary_measurement_capture,
    'Is the state modernization agenda''s benefit real (genuine improvement in administrative legibility and international standing) or is it a measurement artifact where the agenda benefits purely from decree existence being counted as success regardless of underlying practice?',
    'Compare pre- and post-decree administrative efficiency and international-recognition outcomes against independently verified rural practice-uptake data; if legibility gains accrue even where rural uptake is near-zero, the beneficiary structure is measurement capture rather than genuine coordination gain.',
    'If measurement capture, the tangled_rope classification''s coordination-function claim weakens toward pure extraction (snare) for the calendar component specifically, since the coordination benefit the state claims would not depend on the underlying population''s actual behavior at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_measurement_capture, empirical, 'Whether the state''s legibility benefit is genuine or an artifact of measuring decree existence instead of practice uptake.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the legitimacy_of_imposed_practice kernel, decomposed per the ε-invariance principle because the three readings hold structurally distinct premises about what makes imposed practice displacement legitimate/effective (decree sufficiency vs. internalization necessity vs. scaffolded hybrid), each implying different beneficiary/victim structures and different predicted outcomes for the same historical calendar/dress reform episode. The exogenous_override_reading (this story) claims decree authority alone suffices and treats compliance as decree-following regardless of internalization; it computes as tangled_rope given the real administrative-legibility coordination function alongside the substantial uncompensated cost imposed on non-consulted rural populations. The endogenous_climb_reading and hybrid_scaffolding_reading are separate constraint files with their own ε, stakeholders, and classification, linked here because they are readings of the same contested kernel and because outcomes in this override attempt causally condition the resource availability and legitimacy conditions the other two readings' historical instantiations depend on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
