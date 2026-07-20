% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Preparedness Persistence (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The hybrid reading of the preparedness_persistence kernel holds that
 *   preparedness is stratified: engineering inspection remains a live,
 *   competent coordination function (approximating Mountain from the
 *   inspector seat), while evacuation drills and compliance reporting have
 *   atrophied into ritualized performance (Piton from the participant seat).
 *   The constraint as a whole persists through institutional inertia and
 *   bureaucratic budget logic rather than through concentrated beneficiary
 *   capture. Extraction is localized to specific subsystems â particularly
 *   drill programs that consume employee time and public funds without
 *   proportional operational return â while the inspection function
 *   continues to deliver genuine coordination value. This reading coexists
 *   with both the competence reading (which would deny the atrophy) and the
 *   husk reading (which would deny the residual competence).
 *
 * KEY AGENTS:
 *   - emergency_management_agency (agenda_setter/institutional/constrained): Administers the mixed system, cannot easily shed ritualized components without political cost
 *   - engineering_inspectors (observer/moderate/mobile): Maintain genuine competence, see the ritual/theater gap but lack budget authority
 *   - municipal_workforce (payer/moderate/constrained): Bear the time cost of ritualized drills without commensurate safety benefit
 *   - general_public (beneficiary/payer/organized/constrained): Benefit from inspection competence, pay for ritualized drill theater through taxes and false confidence
 *   - legislative_oversight (observer/institutional/analytical): Could restructure allocations but treats preparedness as a monolithic budget category
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.48).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.35).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Preparedness Persistence (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '1d1dff0d-4abe-4404-8a97-c4f4b4f12a69').
narrative_ontology:cs_kernel_codification('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', formalized).
narrative_ontology:cs_authority_grounding('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', practice).
narrative_ontology:cs_interpretation_layer_present('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69').
narrative_ontology:cs_reading_relation('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_axiom('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', foundational, preparedness_is_stratified_not_uniform).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_not_uniform, holdable).
narrative_ontology:cs_axiom_grounding('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', preparedness_is_stratified_not_uniform, empirically_contingent).
narrative_ontology:cs_reference_frame('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', operational_competence_priority).
narrative_ontology:cs_drift_state('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', contemporary_bureaucratic_steady_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d1dff0d-4abe-4404-8a97-c4f4b4f12a69', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, municipal_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the preparedness system, allocates funding between inspection and drill programs, and reports compliance metrics upward. Institutional survival depends on maintaining visible activity regardless of operational value. Could theoretically reallocate resources but faces bureaucratic inertia and political expectation of visible preparedness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agency, agenda_setter,
    institutional, generational, constrained, regional).

% Conduct genuine structural and engineering inspections, maintaining actual competence in hazard assessment. They observe that evacuation drills consume disproportionate resources relative to their operational value but lack authority to reallocate the preparedness budget.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspectors, observer,
    moderate, biographical, mobile, local).

% Required to participate in periodic evacuation drills that interrupt work. The drills follow scripted protocols with little variation for actual site conditions. They bear the time cost and operational disruption without evidence that drill participation improves their actual safety outcomes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, municipal_workforce, payer,
    moderate, biographical, constrained, local).

% Receives genuine benefit from building code enforcement and engineering inspection. Simultaneously funds the preparedness apparatus through taxation and is reassured by visible drill activity that may not correlate with actual response capability. Cannot selectively fund competent subsystems over ritualized ones.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, general_public, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, general_public, payer).

% Reviews preparedness budgets and incident outcomes. Has authority to mandate reform but rarely differentiates between engineering inspection outcomes and drill completion metrics when assessing preparedness. Tends to fund both categories through the same appropriation mechanism.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, legislative_oversight, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a standing capacity to identify structural hazards and coordinate emergency response across jurisdictions, so that societies do not enter each disaster entirely de novo.
% TRANSFER_FUNCTION: Moves public funds and employee time from productive operations into ritualized drill compliance and reporting, while genuine engineering inspection competes for the same budget pool.
% ABSENT_VOICES: Frontline workers who could attest that drills do not match actual operational conditions; taxpayers who would prefer itemized preparedness budgeting; engineering inspectors who could differentiate inspection value from drill theater but are not invited to budget hearings.
% DISAPPEARANCE_RATIONALE: If the preparedness persistence apparatus vanished, engineering inspections would continue through building codes and professional liability (rearranging to a narrower but competent safety regime), while the ritualized drill industry would collapse. Municipal budgets would reallocate, and the false confidence maintained by visible drill activity would dissolve.
% FOUNDING_PROBLEM: Catastrophic disasters revealed societies entering emergencies without pre-positioned knowledge, coordination protocols, or structural safeguards.
% FOUNDING_PROBLEM_CORROBORATION: Engineering inspectors and disaster sociologists attest the founding problem remains live for structural hazards. Municipal workers and public administration scholars attest the preparedness apparatus has partially atrophied into ritual; legislative auditors note budget allocations no longer correlate with demonstrated risk reduction.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.68) because a substantial fraction of preparedness activity â evacuation drills as commonly practiced â is performative maintenance of institutional legitimacy rather than operational rehearsal. Extractiveness is moderate (0.48) because the system still delivers genuine Mountain-like value through engineering inspection, pulling the overall extraction down from what a pure Piton or Snare would show. Suppression is moderate-low (0.35): the constraint persists more through inertia and budget continuity than through active suppression of alternatives. Resistance is low (0.20) because the diffuse costs are borne by taxpayers and workers who lack coordination to resist, while the genuine benefits mute opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the emergency management agency seat, the constraint is a legitimate institutional inheritance with both functional and ceremonial components; from the municipal workforce seat, it is a bureaucratic obligation whose safety value is invisible; from the engineering inspector seat, it is a bifurcated system where their own work is underfunded relative to theatrical programs. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   General_public is declared beneficiary for the competent inspection subsystem (low d for that component) but bears diffuse costs for the ritualized drill subsystem (higher d overall). Municipal_workforce is declared victim of the ritualized extraction (high d). Emergency_management_agency is not declared beneficiary in base_properties because their benefit is institutional survival rather than concentrated extraction â consistent with Piton characterization. The hybrid structure produces seat-dependent directionality: inspectors experience near-symmetric or beneficiary-like directionality, drill participants experience target-like directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the entire preparedness apparatus as pure extraction (Snare) because the engineering inspection function remains genuinely operational. It also prevents mislabeling it as pure coordination (Rope) because the ritualized drill subsystem extracts time and funds without proportional operational return. The hybrid reading captures the mandatrophy risk: if inspection competence continues to erode while drill ritual expands, the constraint will drift from mixed Mountain/Piton toward pure Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_erosion_rate,
    'Is the engineering inspection component genuinely stable, or is it being gradually hollowed out by the same budget logic that sustains ritualized drills?',
    'Longitudinal analysis of inspection staffing ratios versus drill program budgets across a representative sample of jurisdictions.',
    'If inspection is eroding, the constraint''s effective extractiveness is higher than current measures suggest and the Mountain-like component is transient; if stable, the hybrid reading is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_erosion_rate, empirical, 'Whether genuine competence is stable or eroding').

omega_variable(
    drill_substitution_effect,
    'Do ritualized drills crowd out genuine operational preparedness, or merely coexist with it?',
    'Comparative case studies of jurisdictions that eliminated mandatory drills versus those that retained them, measuring actual disaster response outcomes.',
    'If drills crowd out competence, the extraction is active substitution and the constraint drifts toward tangled_rope or snare; if they merely coexist, the extraction is additive waste consistent with piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_substitution_effect, empirical, 'Whether ritualized drills actively substitute for genuine preparedness').

omega_variable(
    institutional_suppression_mechanism,
    'Is the persistence of ritualized drills driven by internalized institutional belief in their efficacy, or by structural budget and mandate lock-in?',
    'Natural experiment from jurisdictions with deregulated preparedness standards: do drill programs persist when external mandates are relaxed?',
    'If persistence is internalized, effective suppression is higher than structural measures suggest; if purely structural, reform is cheaper than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_mechanism, conceptual, 'Internalized versus structural suppression in institutional preparedness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.6).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.65).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__hybrid_reading, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_persistence kernel, which decomposes into three structurally distinct claims: competence_reading (uniform operational readiness), husk_reading (uniform memorial performance), and hybrid_reading (stratified competence and ritual). Each reading instantiates a different constraint with distinct epsilon, stakeholders, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
