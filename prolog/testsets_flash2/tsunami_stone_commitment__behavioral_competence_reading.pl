% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes the tsunami stone inscriptions as retaining
 *   live behavioral force, actively enforced through intergenerational
 *   transmission. It is a 'behavioral competence' reading of the
 *   'tsunami_stone_commitment' kernel. The constraint functions as a piton: a
 *   stabilized, low-extraction mechanism that successfully coordinates
 *   long-term survival behavior. Its persistence is due to its continued
 *   efficacy and the active, non-coercive transmission of its norms, rather
 *   than inertia or theatricality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '90df8db0-3732-4ad0-a756-6caf3d2c14dc').
narrative_ontology:cs_kernel_codification('90df8db0-3732-4ad0-a756-6caf3d2c14dc', fixed_text).
narrative_ontology:cs_authority_grounding('90df8db0-3732-4ad0-a756-6caf3d2c14dc', lineage).
narrative_ontology:cs_interpretation_layer_present('90df8db0-3732-4ad0-a756-6caf3d2c14dc').
narrative_ontology:cs_reading_relation('90df8db0-3732-4ad0-a756-6caf3d2c14dc', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('90df8db0-3732-4ad0-a756-6caf3d2c14dc', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('90df8db0-3732-4ad0-a756-6caf3d2c14dc', foundational, intergenerational_transmission_of_survival_knowledge_is_effective).
narrative_ontology:cs_axiom_status(intergenerational_transmission_of_survival_knowledge_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('90df8db0-3732-4ad0-a756-6caf3d2c14dc', intergenerational_transmission_of_survival_knowledge_is_effective, empirically_contingent).
narrative_ontology:cs_axiom('90df8db0-3732-4ad0-a756-6caf3d2c14dc', foundational, stone_inscription_serves_as_active_behavioral_guide).
narrative_ontology:cs_axiom_status(stone_inscription_serves_as_active_behavioral_guide, holdable).
narrative_ontology:cs_axiom_grounding('90df8db0-3732-4ad0-a756-6caf3d2c14dc', stone_inscription_serves_as_active_behavioral_guide, conventional).
narrative_ontology:cs_reference_frame('90df8db0-3732-4ad0-a756-6caf3d2c14dc', active_intergenerational_survival_norm).
narrative_ontology:cs_drift_state('90df8db0-3732-4ad0-a756-6caf3d2c14dc', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('90df8db0-3732-4ad0-a756-6caf3d2c14dc', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, younger_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities, living in tsunami-prone areas, directly benefit from the behavioral norms encoded in the stone inscriptions. Their survival depends on adhering to the 'do not build below this point' rule, which is transmitted intergenerationally. Their identity is tied to the land and its history, making 'exit' from the constraint unthinkable without abandoning their ancestral homes.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities, beneficiary,
    organized, generational, identity_locked, local).

% The custodians of the oral tradition and the interpreters of the stone inscriptions. They actively transmit the behavioral norms, ensuring each generation understands the meaning and necessity of the 'do not build below this point' rule. Their authority is grounded in their knowledge and role in community survival.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, elders_and_storytellers, agenda_setter,
    powerful, generational, identity_locked, local).

% Bear the 'cost' of adhering to the building restrictions, which might limit economic development or land use in certain areas. However, this 'cost' is understood as a necessary investment in long-term safety. Their compliance is largely voluntary, driven by trust in elders and the historical record.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, younger_generations, payer,
    moderate, biographical, constrained, local).

% Researchers and anthropologists studying long-term disaster preparedness and institutional memory. They analyze the efficacy of such commitments and their transmission mechanisms, seeking to understand how ancient warnings retain contemporary behavioral force.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational behavior to ensure survival in high-risk coastal zones by establishing and maintaining a clear, actionable norm ('do not build below this point') based on historical disaster memory.
% TRANSFER_FUNCTION: Transfers critical survival knowledge and behavioral norms across generations, ensuring that the memory of past tsunamis translates into present-day land-use practices, effectively transferring safety from past experience to future generations.
% ABSENT_VOICES: Developers or economic actors prioritizing short-term gain over long-term safety might object to building restrictions, but their voices are largely absent from communities where the stone commitments are actively maintained, as the survival norm is deeply embedded.
% DISAPPEARANCE_RATIONALE: If the behavioral force of the stone inscriptions vanished, communities might gradually forget the danger, leading to construction in unsafe zones. This would expose future generations to catastrophic risk, fundamentally altering their survival prospects and land-use patterns.
% FOUNDING_PROBLEM: Repeated catastrophic tsunamis devastated coastal communities, leading to a need for a durable, intergenerational mechanism to encode and enforce safe land-use practices.
% FOUNDING_PROBLEM_CORROBORATION: The continued geological threat of tsunamis, attested by seismologists and oceanographers, corroborates that the founding problem remains live. The 2011 tsunami event, while validating the warnings, also underscored the ongoing nature of the threat, reinforcing the need for such commitments.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily provides a survival benefit, with minimal 'cost' beyond adherence to safe practices. Suppression is low (0.1) as compliance is driven by shared understanding and trust, not coercion. Theater ratio is also low (0.05) because the inscription's function is direct and effective, not performative. Accessibility collapse is high (0.8) because the alternative (building in unsafe zones) is understood to be catastrophic. Resistance is low (0.05) due to the clear, existential benefit of compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the coastal communities and elders, this constraint is a vital, low-cost survival mechanism. From an external, purely economic perspective, the building restrictions might appear as a 'cost' or 'lost opportunity,' but this reading emphasizes the long-term, existential benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities are direct beneficiaries (d=0.0) as the constraint ensures their survival. Elders and storytellers act as agenda-setters (d=0.1), actively transmitting the norms for the community's benefit. Younger generations are payers (d=0.2) in that they adhere to restrictions, but this is a 'cost' that directly translates to their own safety. Analytical observers are neutral (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a highly effective, low-extraction survival mechanism as a 'snare' or 'tangled rope.' The low extractiveness and suppression, coupled with active intergenerational transmission, indicate a functional, rather than atrophied, constraint. The 'piton' classification reflects its stabilized, effective operation without significant extractive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_fidelity_decay,
    'What is the rate of decay in transmission fidelity of the behavioral norms over generations, and at what point does it cease to have live behavioral force?',
    'Longitudinal ethnographic studies tracking intergenerational knowledge transfer and land-use patterns, combined with linguistic analysis of oral traditions for semantic drift.',
    'If transmission fidelity decays significantly, the constraint would shift towards a ''commemorative_husk_reading'' (higher theater_ratio, lower suppression, higher extractiveness if land is then developed unsafely), indicating a loss of its original function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_fidelity_decay, empirical, 'Rate at which intergenerational transmission of norms degrades.').

omega_variable(
    commemorative_vs_behavioral_framing,
    'Is the primary function of the tsunami stones commemorative (a historical marker) or behavioral (an active guide for land use)?',
    'Analysis of community discourse, land-use decisions, and responses to perceived threats. If land-use decisions consistently align with the stone''s warning, it supports the behavioral reading; if it''s primarily referenced for historical identity, it supports the commemorative reading.',
    'If primarily commemorative, the constraint''s extractiveness and suppression would be lower (closer to a true piton or even mountain of cultural memory), but its direct behavioral impact would be negligible. If behavioral, its low extractiveness and high accessibility collapse are justified by its survival function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commemorative_vs_behavioral_framing, conceptual, 'Framing of the stone''s function: historical commemoration vs. active behavioral guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t200, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement(tsun_tr_t400, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 400, 0.05).
narrative_ontology:measurement(tsun_tr_t600, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(tsun_tr_t800, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 800, 0.05).
narrative_ontology:measurement(tsun_tr_t1000, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t200, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 200, 0.05).
narrative_ontology:measurement(tsun_be_t400, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 400, 0.05).
narrative_ontology:measurement(tsun_be_t600, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 600, 0.05).
narrative_ontology:measurement(tsun_be_t800, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(tsun_be_t1000, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(tsun_su_t200, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 200, 0.1).
narrative_ontology:measurement(tsun_su_t400, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 400, 0.1).
narrative_ontology:measurement(tsun_su_t600, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(tsun_su_t800, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(tsun_su_t1000, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tsunami_stone_commitment' kernel. This 'behavioral_competence_reading' emphasizes the active, functional role of the stones in guiding behavior, contrasting with the 'commemorative_husk_reading' which sees them as inert symbols, and the 'catastrophe_validation_axis' which focuses on the 2011 tsunami as an empirical test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
