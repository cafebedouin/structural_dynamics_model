% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation (Multi-Mechanism Exercise)
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint describes the requirement for continuous, multi-mechanism
 *   competence occupation in high-reliability organizations. It is a reading
 *   of the 'competence_occupation' kernel, emphasizing that no single
 *   training or assessment method is sufficient, leading to a hybrid
 *   approach. The lack of consensus on optimal configuration means the
 *   'solution' is a perpetual research problem, driving continuous demand for
 *   diverse training services. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates complex competence maintenance but also extracts
 *   significant resources due to its open-ended nature and the lack of clear
 *   'sufficiency' metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.65).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.7).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation (Multi-Mechanism Exercise)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '59e816d2-1006-44eb-b2a7-7ef500e0c243').
narrative_ontology:cs_kernel_codification('59e816d2-1006-44eb-b2a7-7ef500e0c243', formalized).
narrative_ontology:cs_authority_grounding('59e816d2-1006-44eb-b2a7-7ef500e0c243', expertise).
narrative_ontology:cs_interpretation_layer_present('59e816d2-1006-44eb-b2a7-7ef500e0c243').
narrative_ontology:cs_reading_relation('59e816d2-1006-44eb-b2a7-7ef500e0c243', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('59e816d2-1006-44eb-b2a7-7ef500e0c243', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('59e816d2-1006-44eb-b2a7-7ef500e0c243', foundational, competence_is_dynamic_and_multi_dimensional).
narrative_ontology:cs_axiom_status(competence_is_dynamic_and_multi_dimensional, holdable).
narrative_ontology:cs_axiom_grounding('59e816d2-1006-44eb-b2a7-7ef500e0c243', competence_is_dynamic_and_multi_dimensional, empirically_contingent).
narrative_ontology:cs_axiom('59e816d2-1006-44eb-b2a7-7ef500e0c243', foundational, no_single_mechanism_fully_occupies_competence).
narrative_ontology:cs_axiom_status(no_single_mechanism_fully_occupies_competence, holdable).
narrative_ontology:cs_axiom_grounding('59e816d2-1006-44eb-b2a7-7ef500e0c243', no_single_mechanism_fully_occupies_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('59e816d2-1006-44eb-b2a7-7ef500e0c243', adaptive_competence_ecosystem).
narrative_ontology:cs_drift_state('59e816d2-1006-44eb-b2a7-7ef500e0c243', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('59e816d2-1006-44eb-b2a7-7ef500e0c243', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_personnel).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizational_budgets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and certify multi-mechanism training programs. They benefit from the continuous demand for diverse training modalities and the perpetual need for optimization research. They set the standards for what constitutes 'sufficient' hybrid occupation.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers, agenda_setter,
    institutional, biographical, mobile, national).

% Mandate and oversee competence occupation requirements. They benefit from the perceived robustness of a multi-mechanism approach, which reduces their liability and public scrutiny. They enforce compliance with the hybrid model.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, beneficiary,
    institutional, generational, constrained, national).

% Undergo continuous training, simulations, refreshers, and audits, consuming significant time and cognitive load. Their professional identity is tied to maintaining competence, making exit from the training regime difficult despite its burdens. They bear the direct costs of compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_personnel, payer,
    moderate, immediate, identity_locked, local).

% Allocate substantial funds for diverse training programs, equipment, and personnel time. They face pressure to demonstrate competence without clear metrics for optimal return on investment, leading to a perpetual search for 'better' but not necessarily cheaper solutions.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, organizational_budgets, payer,
    powerful, biographical, constrained, national).

% Argue that high-fidelity simulation is sufficient for competence occupation and that other mechanisms are redundant or inefficient. Their proposals for streamlined, simulation-centric training are often dismissed by proponents of the hybrid model.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, simulation_only_advocates, excluded,
    moderate, biographical, constrained, global).

% Contend that only real-world catastrophic incidents truly test and occupy competence, rendering continuous, low-stakes training mechanisms largely performative. Their perspective is often seen as impractical or dangerous by the mainstream.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, real_incident_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous maintenance of high-stakes operational competence across diverse personnel and complex systems by integrating multiple training and assessment mechanisms, aiming to cover various skill dimensions and decay rates.
% TRANSFER_FUNCTION: Transfers resources (time, money, cognitive load) from operational personnel and organizational budgets to training providers and safety regulators, in exchange for certified competence and reduced liability.
% ABSENT_VOICES: Advocates for 'simulation sufficiency' and 'real incident necessity' are largely excluded from the core decision-making on training configuration, as their views challenge the premise of multi-mechanism hybridity. Their voices would push for a more focused, potentially less costly, or more 'authentic' approach.
% DISAPPEARANCE_RATIONALE: If the requirement for continuous, multi-mechanism competence occupation vanished, organizations would immediately reduce training budgets and personnel time dedicated to these activities. Safety regulators would lose a key enforcement lever, and training providers would see a collapse in demand. The entire high-reliability sector would have to fundamentally rethink how competence is assured, likely leading to a rapid, chaotic reorganization.
% FOUNDING_PROBLEM: The recognition that competence in high-stakes environments is dynamic, decays over time, and cannot be fully captured or maintained by a single training method, leading to a need for a comprehensive, adaptive approach.
% FOUNDING_PROBLEM_CORROBORATION: Academic research in human factors and organizational psychology, independent safety boards investigating incidents, and internal organizational reviews consistently corroborate the dynamic nature of competence and the limitations of single-mechanism training. This is attested by sources outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the high cost of implementing and maintaining multiple, diverse training mechanisms without a clear endpoint or consensus on optimal configuration. Suppression (0.7) is high because operational personnel are identity-locked into maintaining competence for their roles, and organizations are legally and reputationally constrained to comply with safety mandates. The theater ratio (0.4) reflects that while many components of the hybrid training are genuinely functional, some elements may be maintained more for regulatory compliance or to demonstrate 'due diligence' than for their proven marginal impact on competence, especially given the lack of consensus on optimal configuration. The rising trend in extractiveness, suppression, and theater ratio over time reflects the 'training ratchet' effect, where more mechanisms are added without clear evidence of their cost-effectiveness or without removing older, less effective ones.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of training providers and regulators, the hybrid approach is a necessary, robust coordination mechanism for complex competence. From the perspective of operational personnel and organizational budgets, it is an increasingly extractive and burdensome requirement, with unclear benefits for each additional layer of training. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Training providers and safety regulators are beneficiaries, as the constraint creates continuous demand for their services and reduces their liability. Operational personnel and organizational budgets are payers, bearing the direct costs in time, effort, and financial outlay. The 'excluded' stakeholders (simulation-only and real-incident advocates) highlight the contested nature of the 'optimal configuration' and the suppression of alternative, potentially less extractive, approaches.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_ambiguity,
    'What is the optimal configuration and balance of training mechanisms required to achieve and maintain competence, and how can this be measured objectively?',
    'Longitudinal, comparative studies across organizations with different training configurations, correlating specific training mixes with objective safety outcomes and skill decay curves, adjusted for confounding factors.',
    'Resolution would either validate the hybrid approach (supporting its coordination function) or reveal significant inefficiencies/redundancies (increasing its measured extractiveness and theater ratio), potentially leading to a reclassification towards a Snare or Piton if the extraction is found to be disproportionate to actual safety gains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimal_configuration_ambiguity, empirical, 'Lack of consensus on optimal training configuration drives perpetual demand and potential over-extraction.').

omega_variable(
    mandate_creep_vs_genuine_need,
    'To what extent does the expansion of training requirements reflect genuine, evolving safety needs versus institutional mandate creep by training providers and regulators?',
    'Independent audits of training program evolution, comparing new requirements against documented changes in operational risk profiles and incident data, with a focus on the marginal utility of each added mechanism.',
    'If mandate creep is dominant, the constraint''s extractiveness and theater ratio would be higher, and its coordination function weaker, pushing it towards a Snare. If genuine need is dominant, its Rope-like qualities would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_creep_vs_genuine_need, empirical, 'Distinguishing between genuine safety evolution and self-serving expansion of training mandates.').

omega_variable(
    reading_difference_on_sufficiency,
    'Is the ''hybrid_occupation'' reading genuinely superior to ''simulation_sufficiency'' or ''real_incident_necessity'', or does it merely represent a political compromise that maximizes resource flow?',
    'A meta-analysis of all available evidence for each reading, assessed by an independent, interdisciplinary panel with no vested interest in any specific training modality or regulatory framework.',
    'If ''hybrid_occupation'' is found to be a political compromise, its extractiveness would be re-evaluated upward, and its coordination function downward, potentially reclassifying it as a Snare. If its superiority is robustly demonstrated, its current classification as a Tangled Rope would be reinforced, or even shift towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_difference_on_sufficiency, conceptual, 'This omega captures the core contest between the kernel readings: whether the hybrid approach is structurally necessary or a negotiated outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.33).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.36).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
