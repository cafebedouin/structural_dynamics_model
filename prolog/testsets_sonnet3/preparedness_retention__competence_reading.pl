% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Flood Preparedness as Live Exercised Competence (Competence Reading)
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_retention kernel: flood-defense drills and dike inspections
 *   in a mature coastal-management institution (modeled on the Dutch
 *   water-board tradition) are read as live, exercised skill maintenance
 *   rather than ceremonial performance. Under this reading the practice
 *   genuinely preserves the capacity it claims to preserve — inspection
 *   findings feed real engineering decisions, drill outcomes are measured
 *   against operational benchmarks, and near-misses update future scenarios.
 *   This is a distinct constraint from the husk_reading (same practices, read
 *   as memorial ritual with decayed live competence) and the hybrid_reading
 *   (competence concentrated in specialized institutions while broader
 *   societal memory goes ceremonial) — each of those is authored as its own
 *   constraint with its own epsilon, per the ε-invariance principle. The
 *   referent here is the standing arrangement (the actual drill/inspection
 *   regime) assessed by the competence reading's own lights: low extraction,
 *   low theater, functioning coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Flood Preparedness as Live Exercised Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '4b8ffd5b-7773-4a1e-a056-0a041a5a286e').
narrative_ontology:cs_kernel_codification('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', distributed).
narrative_ontology:cs_authority_grounding('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', expertise).
narrative_ontology:cs_interpretation_layer_present('4b8ffd5b-7773-4a1e-a056-0a041a5a286e').
narrative_ontology:cs_reading_relation('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', foundational, exercise_produces_verifiable_retained_capacity).
narrative_ontology:cs_axiom_status(exercise_produces_verifiable_retained_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', exercise_produces_verifiable_retained_capacity, empirically_contingent).
narrative_ontology:cs_axiom('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', secondary, resource_allocation_tracks_skill_retention_not_ceremony).
narrative_ontology:cs_axiom_status(resource_allocation_tracks_skill_retention_not_ceremony, holdable).
narrative_ontology:cs_axiom_grounding('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', resource_allocation_tracks_skill_retention_not_ceremony, instrumental).
narrative_ontology:cs_reference_frame('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', post_1953_flood_engineering_doctrine).
narrative_ontology:cs_drift_state('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', contemporary_climate_adaptation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4b8ffd5b-7773-4a1e-a056-0a041a5a286e', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, coastal_population).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, water_board_engineers).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, national_treasury).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, operational_readiness_requires_live_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, run, and evaluate flood-defense drills and dike inspections. They set inspection frequency and drill scenarios based on hydrological modeling and near-miss data, and their professional standing depends on the defenses actually holding when tested by real water. Exit from this role means abandoning the technical mandate that defines their institution.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, water_board_engineers, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, water_board_engineers, beneficiary).

% Live behind the dikes and storm barriers whose integrity depends on the drills and inspections being real rather than symbolic. They rarely observe the exercises directly but bear the full consequence if competence has quietly decayed into ritual; their only real check is trusting the outcome of a crisis they hope never to face.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, coastal_population, beneficiary,
    moderate, biographical, constrained, regional).

% Participate in joint drills with water boards and municipal authorities, training evacuation and coordination procedures. Their capacity to act fast in a real flood is a direct function of how realistically these exercises are run; if the exercises are honest they gain transferable operational skill.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_agencies, agenda_setter).

% Funds the inspection regime and drill cycles out of the national budget. Under the competence reading, this spending buys genuine risk reduction and is treated as an efficient allocation because it is calibrated to sustaining live skill rather than performing ritual; the only cost pressure is fiscal, not extractive.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, national_treasury, payer,
    institutional, generational, constrained, national).

% External technical reviewers who assess whether drill outcomes translate into measurable competence gains (response times, defense performance under stress tests) rather than mere completion of scheduled events. Their reports are the main outside check on whether the competence claim holds.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, independent_flood_auditors, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regularly exercising flood-defense infrastructure and personnel is the only way to detect degradation before a real flood does, and to keep institutional knowledge of failure modes current across generations of engineers and responders.
% TRANSFER_FUNCTION: Public funds and engineer/responder labor time are converted into maintained operational capacity — skill, calibrated equipment, tested procedures — that is drawn down only during real flood events; under this reading nothing is extracted beyond the ordinary cost of the coordination itself.
% ABSENT_VOICES: Taxpayers who bear the ongoing cost of drills and inspections without ever observing a flood are not routinely consulted on whether the frequency and intensity of exercises is calibrated correctly; their voice enters mainly through treasury budget review, at a remove from the technical decisions.
% DISAPPEARANCE_RATIONALE: If drills and inspections stopped, dike integrity and evacuation coordination would degrade silently and undetectably until tested by an actual flood, at which point failure would be sudden and catastrophic rather than gradual — the coastal population's safety margin depends materially on this practice continuing.
% FOUNDING_PROBLEM: Historical flood disasters (notably the 1953 North Sea flood) demonstrated that infrastructure alone is insufficient without continuously exercised human competence to operate, inspect, and respond to it under real conditions.
% FOUNDING_PROBLEM_CORROBORATION: Independent flood auditors and international engineering peer-review bodies (which are not beneficiaries of the water boards' budgets) corroborate that flood risk in the region remains physically live and that exercised competence measurably correlates with defense performance in stress tests; this is not solely self-attested by the water boards.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, under this reading, essentially all resource flow into drills and inspections converts into measurable operational capacity rather than rent or performance. Theater ratio is authored low (0.15) and only mildly rising, reflecting that some ceremonial residue (public demonstration exercises, anniversary commemorations) exists alongside the substantive work but does not dominate it. Suppression is low (0.12) because participation in the exercise regime is a professional and civic norm rather than a coerced one; resistance is correspondingly low (0.25) since no party is meaningfully extracted from under this reading. Accessibility collapse is moderate (0.35): alternative preparedness models exist in principle (e.g., pure infrastructure investment without exercise) but the competence-reading community has converged on live exercise as functionally necessary, not merely habitual.
 *
 * DIRECTIONALITY LOGIC:
 *   Water board engineers and emergency response agencies are declared beneficiaries because the practice directly builds and validates their professional competence; the coastal population is a beneficiary because the practice is what keeps the infrastructure that protects them functioning. No victim group is declared under this reading — the only cost is the ordinary fiscal cost borne by the national treasury, which is a payer in the coordination sense, not an extraction target. If resource allocation drifted toward over-investment relative to marginal safety gain, fiscal efficiency could become a secondary victim, but that is not the state described here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophic failure from untested infrastructure and unexercised personnel) is authored as still live, which blocks a mandatrophy verdict: this is not a mandate that outlived its function, because the flood risk it addresses is corroborated as physically ongoing by independent auditors, not merely asserted by the institutions that benefit from the practice continuing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_reading_verification_basis,
    'What observable evidence would distinguish this competence reading from the husk_reading or hybrid_reading of the same drill and inspection regime?',
    'Independent audit of drill outcomes against pre-registered operational benchmarks (response time targets, defense performance under simulated stress) over multiple drill cycles, cross-checked against actual flood event outcomes where they occur.',
    'If audited outcomes show drills producing measurable, retained skill gains and infrastructure performance matching or exceeding tested benchmarks, the competence reading is supported. If outcomes show drills completing on schedule without corresponding gains in real capacity, the husk_reading is the more accurate constraint for this same practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_verification_basis, empirical, 'Whether the drill/inspection regime empirically produces live competence or ceremonial completion, as distinguishing evidence between kernel readings.').

omega_variable(
    stratification_boundary_ambiguity,
    'Does competence retention hold uniformly across the whole preparedness system, or is it concentrated in specialized institutions (water boards, technical engineering corps) while broader civic/municipal participation is more ceremonial — which would make the hybrid_reading the more accurate account of the same underlying kernel?',
    'Disaggregate drill participation and outcome data by institutional tier (specialized technical agency vs. general municipal/civic emergency response) and compare competence retention metrics across tiers.',
    'If competence is uniform across tiers, this competence_reading stands as authored. If competence is concentrated in technical tiers with ceremonial decay at the civic tier, the hybrid_reading better describes the actual arrangement and this story''s beneficiary/extraction structure would need to be re-partitioned by tier rather than treated as a single population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_ambiguity, conceptual, 'Whether the kernel is better read as uniformly competent (this reading) or stratified (hybrid_reading) — a framing question located at the tier boundary.').

omega_variable(
    fiscal_overinvestment_victim_threshold,
    'At what point does drill/inspection frequency exceed the marginal safety benefit, converting the national treasury from a coordination payer into a genuine victim of over-provisioned ceremony?',
    'Cost-effectiveness analysis comparing marginal risk reduction per additional drill/inspection cycle against marginal fiscal cost, benchmarked against comparable coastal-defense systems internationally.',
    'If marginal risk reduction has flattened while spending continues rising, this reading''s ''no victim'' declaration becomes false and fiscal_efficiency should be added as a victim group, shifting the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_overinvestment_victim_threshold, empirical, 'Whether current resource allocation has crossed from efficient competence-preservation into fiscally extractive over-provision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__competence_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__competence_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__competence_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__competence_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__competence_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__competence_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story, preparedness_retention__husk_reading, and preparedness_retention__hybrid_reading are three readings of the same preparedness_retention kernel — the same physical practice (drills and dike inspections) read under three structurally distinct claims about whether live competence is actually preserved. Each carries its own epsilon (this reading: 0.18, low; husk_reading: expected high, ceremony masking decayed competence; hybrid_reading: expected mixed, tier-dependent). They are linked via affects_constraints rather than merged because per the ε-invariance principle, a single constraint cannot honestly carry three different extraction values for the same observable-selection question — the disagreement is which reading is true of the world, not a parameter internal to one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
