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
 *   human_readable: Tsunami Stone Inscription: Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'behavioral competence' reading of
 *   the tsunami stone commitment kernel. It describes a scenario where a
 *   stone inscription, placed after a devastating tsunami, successfully
 *   retained live behavioral force over centuries. This was achieved through
 *   active norm enforcement via intergenerational transmission, guiding
 *   communities to settle in safe zones and ensuring their survival during
 *   subsequent tsunami events. The constraint is characterized by very low
 *   extraction and high functional efficacy, appearing as a 'Piton' due to
 *   its deep internalization and minimal overt maintenance cost, rather than
 *   degradation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.2).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Inscription: Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '456ed1f4-b00e-4f08-bede-476d5e66689d').
narrative_ontology:cs_kernel_codification('456ed1f4-b00e-4f08-bede-476d5e66689d', fixed_text).
narrative_ontology:cs_authority_grounding('456ed1f4-b00e-4f08-bede-476d5e66689d', lineage).
narrative_ontology:cs_interpretation_layer_present('456ed1f4-b00e-4f08-bede-476d5e66689d').
narrative_ontology:cs_reading_relation('456ed1f4-b00e-4f08-bede-476d5e66689d', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('456ed1f4-b00e-4f08-bede-476d5e66689d', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('456ed1f4-b00e-4f08-bede-476d5e66689d', foundational, intergenerational_transmission_ensures_compliance).
narrative_ontology:cs_axiom_status(intergenerational_transmission_ensures_compliance, holdable).
narrative_ontology:cs_axiom_grounding('456ed1f4-b00e-4f08-bede-476d5e66689d', intergenerational_transmission_ensures_compliance, empirically_contingent).
narrative_ontology:cs_axiom('456ed1f4-b00e-4f08-bede-476d5e66689d', foundational, stone_inscription_is_active_behavioral_guide).
narrative_ontology:cs_axiom_status(stone_inscription_is_active_behavioral_guide, holdable).
narrative_ontology:cs_axiom_grounding('456ed1f4-b00e-4f08-bede-476d5e66689d', stone_inscription_is_active_behavioral_guide, empirically_contingent).
narrative_ontology:cs_reference_frame('456ed1f4-b00e-4f08-bede-476d5e66689d', ancestral_survival_paradigm).
narrative_ontology:cs_drift_state('456ed1f4-b00e-4f08-bede-476d5e66689d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('456ed1f4-b00e-4f08-bede-476d5e66689d', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, contemporary_coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original and ongoing transmitters of the stone's message and associated behavioral norms. They actively enforce the norm through storytelling, ritual, and direct instruction, ensuring its intergenerational continuity. Their identity is fused with the role of cultural preservation and community safety.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, ancestral_community_elders, agenda_setter,
    institutional, generational, identity_locked, local).

% Members of the community who adhere to the stone's warning by settling in higher, safer ground, foregoing the fertile but dangerous coastal lowlands. The 'cost' is the opportunity cost of less convenient or less productive land, but they are net beneficiaries of safety. Compliance is deeply internalized.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, contemporary_coastal_residents, payer,
    moderate, biographical, constrained, local).

% The primary beneficiaries of the stone's enduring behavioral force, as they inherit a culture of safety and settlement patterns that protect them from tsunamis. They are 'trapped' in the sense that their survival depends on the continued efficacy of this inherited knowledge.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Researchers who study the long-term efficacy of the stone inscription and its role in shaping community resilience. They analyze the mechanisms of intergenerational transmission and the behavioral outcomes.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated settlement patterns away from high-risk coastal areas, ensuring community survival during recurrent tsunami events by establishing safe zones.
% TRANSFER_FUNCTION: Transferred critical survival knowledge and behavioral norms regarding tsunami safety across generations, from ancestral elders to contemporary and future community members, ensuring collective memory and adaptive behavior.
% ABSENT_VOICES: Those who, in the past, chose to settle in low-lying areas despite the warning, or those who failed to receive or heed the intergenerational transmission. Their voices are absent due to their likely demise in subsequent tsunami events, or their marginalization from the community's core survival strategy.
% DISAPPEARANCE_RATIONALE: If the stone's behavioral force and intergenerational transmission vanished overnight, communities would likely re-settle in fertile but vulnerable coastal zones, leading to catastrophic loss of life in future tsunami events. The entire social and ecological relationship with the coastline would fundamentally reorganize to a more dangerous state.
% FOUNDING_PROBLEM: Recurrent devastating tsunamis that repeatedly wiped out coastal settlements, threatening the very existence of the community.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence of past tsunamis, historical records of disaster, and the continued existence of the tsunami hazard itself, corroborated by scientific studies and local oral histories from outside the immediate benefiting community, all attest to the ongoing nature of the founding problem.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The very low extractiveness (0.05) reflects that the constraint primarily serves a collective safety function with no identifiable party extracting rents from its operation. Suppression (0.20) is low because compliance is largely internalized through cultural transmission, rather than relying on overt coercion. The theater ratio (0.10) is also low, indicating that the inscription's function is genuinely live and effective, not merely performative. The 'Piton' classification, as instructed by the prompt, is interpreted here as a constraint so deeply integrated and effective that its maintenance costs are minimal, making it *appear* inert, rather than having an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   While this reading asserts the stone's live behavioral force, other readings (e.g., the 'commemorative husk' reading) would view it as a decayed symbolic artifact. The divergence lies in whether the inscription actively shapes behavior or merely stands as a historical marker, with profound implications for its functional status and classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The ancestral community elders act as agenda-setters, actively transmitting the norm. Contemporary coastal residents are payers, bearing the opportunity cost of safer land, but are net beneficiaries of the safety provided. Future generations are the ultimate beneficiaries, inheriting a survival strategy. There are no identifiable victims or extractive beneficiaries, as the constraint's primary function is collective survival.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    piton_functional_contradiction,
    'Is this constraint truly a Piton (degraded/inertial with atrophied function) or a highly effective Rope/Mountain that appears low-cost due to deep internalization and self-sustaining transmission?',
    'Detailed ethnographic and historical study of the costs of norm transmission versus the benefits, and the degree of active enforcement versus passive adherence. Re-evaluation of the ''Piton'' definition in cases of highly effective, deeply internalized constraints.',
    'If the function is truly live and self-sustaining with minimal cost, it might be reclassified as a Rope (collective coordination) or even a Mountain (if the natural hazard is the mountain and the stone is a guide to navigate it), rather than a degraded Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(piton_functional_contradiction, conceptual, 'Tension between ''Piton'' classification and ''live behavioral force'' description.').

omega_variable(
    behavioral_force_vs_symbolic_decay,
    'Does the stone inscription genuinely retain live behavioral force, or has it decayed to a symbolic artifact, with compliance being coincidental or weakly enforced?',
    'Empirical observation of contemporary settlement patterns, community response to tsunami warnings, and the explicit content of intergenerational transmission practices. Comparison with communities lacking such inscriptions.',
    'If the behavioral force is confirmed, this reading is strengthened. If it has decayed, the ''commemorative_husk_reading'' would be validated, leading to a reclassification (likely a Piton with higher theater ratio and lower functional efficacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_force_vs_symbolic_decay, empirical, 'Whether the stone''s message actively guides behavior or is merely symbolic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(tsun_tr_t200, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(tsun_tr_t300, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(tsun_tr_t400, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(tsun_tr_t500, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 500, 0.1).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(tsun_be_t200, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 200, 0.05).
narrative_ontology:measurement(tsun_be_t300, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 300, 0.05).
narrative_ontology:measurement(tsun_be_t400, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 400, 0.05).
narrative_ontology:measurement(tsun_be_t500, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 500, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tsun_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(tsun_su_t200, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 200, 0.2).
narrative_ontology:measurement(tsun_su_t300, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 300, 0.2).
narrative_ontology:measurement(tsun_su_t400, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 400, 0.2).
narrative_ontology:measurement(tsun_su_t500, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 500, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral competence' reading of the 'tsunami_stone_commitment' kernel, focusing on its active behavioral force and intergenerational transmission. It stands in contrast to the 'commemorative husk' reading, which views the inscription as a decayed symbolic artifact, and is influenced by the 'catastrophe validation' axis which provides empirical evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
