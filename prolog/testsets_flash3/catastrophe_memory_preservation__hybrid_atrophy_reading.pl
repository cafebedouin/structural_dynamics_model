% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that originated to preserve
 *   survival-competence in the face of catastrophe. Over time, with the
 *   receding of direct threats and the rise of modernity, its operational
 *   function atrophied, but the practice persisted, transforming into a
 *   mechanism for collective mourning and identity preservation. This reading
 *   emphasizes the 'hybrid atrophy' – a state where the original, adaptive
 *   function has largely died, but the ritual continues to extract costs from
 *   practitioners while providing a different, less tangible benefit of
 *   in-group identity. It is a Piton because its primary function has
 *   atrophied, but it persists due to inertia and a shifted, diffuse benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'b2cc3c8d-38c7-4a9a-a484-67ddf005c838').
narrative_ontology:cs_kernel_codification('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', implicit).
narrative_ontology:cs_authority_grounding('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', practice).
narrative_ontology:cs_interpretation_layer_present('b2cc3c8d-38c7-4a9a-a484-67ddf005c838').
narrative_ontology:cs_reading_relation('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', foundational, ritual_function_evolves_with_context).
narrative_ontology:cs_axiom_status(ritual_function_evolves_with_context, holdable).
narrative_ontology:cs_axiom_grounding('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', ritual_function_evolves_with_context, empirically_contingent).
narrative_ontology:cs_axiom('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', secondary, identity_value_can_supersede_adaptive_value).
narrative_ontology:cs_axiom_status(identity_value_can_supersede_adaptive_value, holdable).
narrative_ontology:cs_axiom_grounding('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', identity_value_can_supersede_adaptive_value, conventional).
narrative_ontology:cs_reference_frame('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', ritual_as_adaptive_mechanism).
narrative_ontology:cs_drift_state('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b2cc3c8d-38c7-4a9a-a484-67ddf005c838', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and perform the ritual practices, bearing the time and resource costs. They experience the ritual as a burden without clear adaptive payoff, but are bound by cultural and identity ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, local).

% The original beneficiaries of the ritual, for whom it provided survival competence. This is an analytical construct representing the past function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community, beneficiary,
    analytical, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community).

% The current beneficiary of the ritual, which now primarily reinforces collective identity and belonging, even if its original function is lost. This is an analytical construct.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity, beneficiary,
    analytical, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).

% Administer and transmit the ritual, emphasizing its importance for continuity and identity. They are often unaware of the original survival function's atrophy, or actively resist acknowledging it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, cultural_elders, agenda_setter,
    organized, generational, constrained, local).

% Study the ritual's evolution, documenting its shift from practical competence to symbolic mourning. They analyze the discrepancy between claimed function and actual effect.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, coordinated collective action and knowledge transfer for survival in the face of recurring catastrophe. Currently, coordinates collective memory and in-group identity.
% TRANSFER_FUNCTION: Historically, transferred survival-critical knowledge and behavioral patterns across generations. Currently, transfers cultural heritage and a sense of shared past, but without operational competence.
% ABSENT_VOICES: The original catastrophe survivors, who would attest to the ritual's direct survival utility, are absent. Their perspective would highlight the current ritual's functional atrophy.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the present generation would lose a key marker of their collective identity and a connection to their past, leading to a significant cultural void and potential fragmentation of the group.
% FOUNDING_PROBLEM: To preserve critical survival knowledge and behavioral responses across generations in the face of recurring environmental or social catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical analyses from outside the practicing community corroborate that the original catastrophic threats have largely receded or are addressed by modern means, rendering the ritual's survival-competence function obsolete. Cultural elders within the community contest this, asserting the problem is still live in a symbolic sense.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and declining (0.45 at end) because the direct, tangible benefits of survival competence are gone, leaving only the costs of practice and the diffuse benefit of identity. Suppression is moderate (0.6) and also declining, as the enforcement shifts from strict adherence for survival to social pressure for cultural continuity. Theater ratio is high and rising (0.7 at end) because the performance of the ritual increasingly outweighs its functional utility, becoming more about symbolic display than practical outcome. The declining extractiveness and suppression, coupled with rising theater, are characteristic of a Piton where the original mandate has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   Cultural elders perceive the ritual as a vital link to the past and a source of identity, justifying its costs. Present-generation practitioners, while identity-locked, experience it as a burden. Anthropological observers see the functional atrophy and the shift to symbolic meaning. The engine's classification as Piton reflects the observer's analytical perspective on the constraint's structural state.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-generation practitioners are targets (payer role, identity_locked exit, bear costs without direct adaptive payoff). The historical community (an analytical construct) was a full beneficiary. The current in-group identity (also an analytical construct) is a diffuse beneficiary. Cultural elders act as agenda-setters, maintaining the practice due to tradition and a belief in its continued, albeit shifted, importance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a clear case of mandatrophy: its original mandate (survival competence) has largely expired, but the practice persists, having found a secondary, less adaptive function (identity preservation). The Piton classification prevents mislabeling it as a Rope (which would imply ongoing, net-positive coordination for its original purpose) or a Snare (which would imply active, concentrated extraction for a current beneficiary). The high theater ratio and declining extractiveness are key indicators of this atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_function_persistence,
    'To what extent does the ritual still, even implicitly, preserve any operational survival competence, or has this function completely atrophied?',
    'Empirical study of practitioners'' responses to novel threats that align with historical catastrophes: if the ritual provides measurable adaptive advantage, the atrophy is incomplete.',
    'If residual operational competence is found, the extractiveness might be slightly lower, and the theater_ratio might be lower, pushing it marginally closer to a Tangled Rope or even a degraded Rope, rather than a pure Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_function_persistence, empirical, 'Assesses the completeness of functional atrophy.').

omega_variable(
    identity_value_quantification,
    'How much ''value'' do present-generation practitioners derive from the identity-preserving function of the ritual, relative to the costs they bear?',
    'Sociological surveys and qualitative interviews with practitioners, attempting to quantify the subjective benefit of identity and belonging against the objective costs of practice.',
    'If the subjective identity value is high enough to outweigh costs, the constraint might be re-evaluated as a Rope for the ''identity coordination'' function, despite the atrophy of its original purpose. If the costs significantly outweigh the identity value, it reinforces the Piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_value_quantification, preference, 'Quantifies the subjective benefit of identity preservation.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''hybrid_atrophy_reading'' the most accurate description of the catastrophe_memory_preservation kernel, or do the ''survival_competence_reading'' or ''mourning_practice_reading'' better capture its current state?',
    'Longitudinal ethnographic studies combined with historical analysis, focusing on the actual adaptive outcomes versus the symbolic functions over time. Consensus among independent anthropological observers.',
    'If the ''survival_competence_reading'' were adopted, the constraint would likely be a Rope or Mountain (if truly natural law) with lower extractiveness. If the ''mourning_practice_reading'' were adopted, it would likely be a Piton or even a Snare (if identity is coercively enforced) with a different beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity in the primary function of the catastrophe memory preservation ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 75, 0.65).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 75, 0.47).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
