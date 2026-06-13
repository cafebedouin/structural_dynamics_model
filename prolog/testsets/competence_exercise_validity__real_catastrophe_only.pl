% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe Competence Exercise Validity
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the 'real_catastrophe_only' reading of the
 *   competence_exercise_validity kernel. The reading asserts that only real
 *   catastrophe truly exercises competence; simulation is an insufficient
 *   substitute. Organizations adopt the frame that simulation validates
 *   competence because admitting that validation requires actual crisis would
 *   expose them to liability and operational risk. This constraint models the
 *   extraction mechanism through which that frame persists despite growing
 *   evidence from post-incident analysis that simulation masked competence
 *   gaps. Field operators bear the untested-competence risk while
 *   institutional actors collect legitimacy.
 *
 * KEY AGENTS:
 *   - risk_denial_advocates (institutional beneficiary, identity-locked): benefit from treating simulation as sufficient; identity as competent stewards depends on the frame
 *   - simulation_investment_holders (trapped payer): invested careers in simulation systems; walking back that investment damages standing
 *   - training_program_operators (constrained payer): resource-constrained; shifting to live-fire training or admitting inadequacy both carry high cost
 *   - organization_leadership (powerful payer/beneficiary): benefits from competence narrative; constrained because alternatives carry higher institutional cost
 *   - field_operators (excluded, powerless): face actual competence gap risk; not in frame-setting conversation
 *   - external_auditors (observer, organized): position to assess competence-reality gap but power limited by organization's control of simulation data
 *   - past_incident_analysts (observer, analytical): review post-incident data showing simulation gaps; findings archived rather than operationalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.67).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.71).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.67).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, snare).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe Competence Exercise Validity").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, 'ba9ec4de-d316-4c6d-9b52-62603e29bc4b').
narrative_ontology:cs_kernel_codification('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', distributed).
narrative_ontology:cs_authority_grounding('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', extraction).
narrative_ontology:cs_interpretation_layer_present('ba9ec4de-d316-4c6d-9b52-62603e29bc4b').
narrative_ontology:cs_reading_relation('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', foundational, competence_exercise_requires_authentic_stress).
narrative_ontology:cs_axiom_status(competence_exercise_requires_authentic_stress, holdable).
narrative_ontology:cs_axiom_grounding('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', competence_exercise_requires_authentic_stress, empirically_contingent).
narrative_ontology:cs_axiom('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', foundational, simulation_masks_competence_decay).
narrative_ontology:cs_axiom_status(simulation_masks_competence_decay, holdable).
narrative_ontology:cs_axiom_grounding('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', simulation_masks_competence_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', competence_requires_real_stress).
narrative_ontology:cs_drift_state('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', contemporary_expanded_simulation_investment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ba9ec4de-d316-4c6d-9b52-62603e29bc4b', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, risk_denial_advocates).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_investment_holders).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, training_program_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, organization_leadership).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.48 → 0.67) because simulation programs accumulate, the institutional commitment to treating them as validation deepens, and evidence that they mask competence gaps must be increasingly suppressed. Theater rises (0.35 → 0.58) because as the evidence gap grows, more of the simulation activity becomes performative validation rather than genuine testing—drills pass reliably not because competence is exercised but because scenarios are designed to succeed. Suppression rises (0.55 → 0.71) because as external doubters (auditors, analysts, workers aware of past incidents) accumulate evidence, maintaining the frame requires more aggressive control of discourse: incident analysis is restricted, near-miss data is not fed back, alternative competence frameworks are delegitimized. The constraint is extractive because it transfers institutional legitimacy (claimed readiness) and risk (actual untested competence) asymmetrically—beneficiaries collect legitimacy, payers and excluded operators collect risk. Suppression is high because the frame's persistence depends on preventing field-level knowledge (where competence gaps are most visible) from bubbling up into frame-setting conversation.
 *
 * PERSPECTIVAL GAP:
 *   The risk_denial_advocates seat and the field_operators seat compute entirely differently. From the advocate's seat, the constraint is genuine coordination: 'We need a credible way to say we are prepared without running catastrophes.' From the field operator's seat (excluded, so not formally computing), the constraint is pure extraction: 'The organization has shifted all risk to us while collecting legitimacy from simulation they know is incomplete.' The engine computes the advocate seat's type from the beneficiary/victim structure and their high directionality (d near 0.0, full beneficiary); field operators are structurally excluded, so their type is not formally computed, but their situation establishes that the constraint operates as snare for those who would bear real consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Risk_denial_advocates are structural beneficiaries: they collect the institutional legitimacy the frame generates and bear none of the competence-gap risk (d ≈ 0.0–0.15, beneficiary end). Simulation_investment_holders are targets: their careers depend on the simulation system's sufficiency, but the rising theater_ratio shows that investment is increasingly indefensible; they are trapped because exit (publicly walking back competence claims) damages them worse than staying (d ≈ 0.75–0.85, target end, identity-locked exit). Organization_leadership sits asymmetrically: they benefit from the legitimacy narrative but also bear liability risk if untested competence fails catastrophically (d ≈ 0.4–0.5, near-symmetric but tilted toward beneficiary because they control the frame). Training_program_operators are targets with constrained exit: they cannot leave the organization without credential-specific employment cost; they cannot change the frame without institutional cost (d ≈ 0.70–0.80, target end). Field operators are maximally extracted from but structurally excluded, so no formal seat computes their type—this is a design feature that allows the constraint to persist by keeping the highest-risk population outside the frame-setting conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'real_catastrophe_only' reading creates a mandatrophy detection signal: the founding problem was 'how do we assess competence without running crises' and the founding solution was 'simulation as proxy.' The founding_problem_status is 'contested' because simulation operators claim the problem remains live (crises are rare, we need simulation) while post-incident analysts claim the problem has shifted (the real problem is now 'competence decay masked by theater,' not 'lack of assessment method'). The measurement series shows mandatrophy directly: as theater_ratio rises from 0.35 to 0.58, the constraint's capacity to solve the founding problem degrades—simulation becomes less about competence exercise and more about organizational legitimacy theater. At the interval end, the constraint persists not because it solves the original problem but because removing it would expose the organization to liability and field-level unease. This is a classic zombie constraint: the original coordination problem (assessment without catastrophe) has been solved by the advent of sophisticated simulation platforms, but the constraint persists because removing it would require admitting that competence validation is incomplete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_trajectory,
    'What is the actual decay rate of untested competence in high-stakes domains? Does simulation materially slow decay, or does it merely postpone observable failure?',
    'Controlled study of organizations forced to adopt live-fire training vs. simulation-only cohorts, tracking incident rates over 10+ years; or analysis of post-incident competence data from organizations that experienced genuine crises after years of simulation-only validation.',
    'If decay is rapid and simulation does not slow it, the constraint extracts heavily by shifting liability to field operators. If decay is slow even without simulation, the constraint may genuinely solve a coordination problem. If simulation materially slows decay but does not prevent competence gaps, the reading partially transfers from ''snare'' toward ''tangled_rope'' (mixing coordination with extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_trajectory, empirical, 'Whether untested competence decays faster than the constraint acknowledges.').

omega_variable(
    simulation_scenario_closure,
    'What proportion of real-catastrophe scenarios fall outside the design space of organizational simulation programs?',
    'Post-incident analysis: for incidents in organizations with active simulation programs, what fraction of actual cascades, decision points, or failure modes were not covered in drills? Comparative analysis across industries (aviation, nuclear, healthcare, emergency response).',
    'High closure (simulation covers most real scenarios) supports the ''continuous_refresh_hybrid'' reading and weakens pure ''real_catastrophe_only'' framing. Low closure (many real scenarios are novel to the organization) supports this reading''s core claim that simulation masks competence gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_scenario_closure, empirical, 'The degree to which simulation covers the actual scenario space organizations encounter.').

omega_variable(
    institutional_identity_lock_depth,
    'For simulation_investment_holders and organization_leadership, how much of the identity-lock and institutional commitment is based on genuine epistemic belief in simulation sufficiency vs. sunk-cost rationalization and reputational capture?',
    'Structural interview or revealed-preference experiment: would actors agree to independent competence validation (real scenarios, blind assessment) if they could preserve organizational legitimacy? How much friction appears when competence-assessment methods are decoupled from internal validation?',
    'If identity-lock is primarily epistemic (actors genuinely believe simulation works), the constraint might be addressed through new evidence. If identity-lock is primarily reputational (actors believe the opposite but cannot change course without damage), the constraint operates as a snare and remediation requires institutional redesign, not just evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock_depth, empirical, 'Whether institutional commitment to simulation is epistemic or reputation-driven.').

omega_variable(
    near_miss_feedback_suppression,
    'Is the rise in theater_ratio driven by genuine competence decay that simulation fails to catch, or by organizational suppression of near-miss and incident data that would otherwise feed back into simulation design?',
    'Comparison of simulation evolution in organizations with open incident-reporting culture vs. restricted reporting; analysis of whether near-miss data systematically disappears from competence-assessment conversation.',
    'If suppression is the driver, the extraction mechanism is primarily information control rather than competence decay. The constraint might be addressable through transparency without changing simulation methodology. If decay is the driver, structural change (live-fire training) is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_feedback_suppression, empirical, 'Whether theater rise reflects competence decay or information suppression.').

omega_variable(
    alternative_reading_lived_experience,
    'Do field operators and workers experienced with real crises actually endorse the ''real_catastrophe_only'' reading, or do they operate under a different epistemic frame about what counts as competence?',
    'Structured interview or ethnographic study of workers in organizations that have experienced genuine catastrophes, comparing their assessment of simulation-readiness to their actual-event performance and hindsight view of competence exercise.',
    'If field operators endorse ''real_catastrophe_only'' based on lived experience, this reading gains corroboration from excluded voices. If they report that simulation provided meaningful preparation despite gaps, the reading''s framing shifts toward acknowledging partial validity of simulation rather than categorical rejection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_lived_experience, empirical, 'Whether excluded field operators'' lived experience corroborates this reading''s claims about simulation inadequacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__real_catastrophe_only, theater_ratio, 5, 0.42).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.48).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__real_catastrophe_only, theater_ratio, 15, 0.53).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.56).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__real_catastrophe_only, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 25, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_validity kernel. The kernel asks: 'What counts as valid competence exercise in high-stakes domains?' This reading asserts that only real catastrophe truly exercises competence; simulation is an insufficient substitute. Sibling readings (simulation_as_proxy, continuous_refresh_hybrid) assert different definitions of validity. All three readings are linked via network.affects_constraints because they share the same kernel and compete for institutional adoption. The 'real_catastrophe_only' reading structurally forecloses 'simulation_as_proxy' (if only real catastrophe counts, simulation cannot count as adequate), but influences 'continuous_refresh_hybrid' (accepting real-catastrophe-only as foundational creates pressure toward approximation methods like continuous drill cycles). All three stories have different ε values and different beneficiary/victim structures because they instantiate different epistemic premises about competence exercise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
