% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Competence Training
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the 'survival_competence_reading' of
 *   the 'catastrophe_memory_kernel'. It describes how ritual practices within
 *   a community facing persecution function as a mechanism to encode and
 *   transmit adaptive capacity, ensuring collective survival. The rituals
 *   serve as a form of 'survival training,' rehearsing responses to
 *   historical and anticipated threats. While providing vital coordination
 *   for resilience, the constraint also imposes significant costs on
 *   community members in terms of maintaining distinct boundaries and
 *   resisting assimilation, leading to its classification as a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Competence Training").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'b05aede8-1908-46ef-a3bc-2047441a34d8').
narrative_ontology:cs_kernel_codification('b05aede8-1908-46ef-a3bc-2047441a34d8', formalized).
narrative_ontology:cs_authority_grounding('b05aede8-1908-46ef-a3bc-2047441a34d8', lineage).
narrative_ontology:cs_interpretation_layer_present('b05aede8-1908-46ef-a3bc-2047441a34d8').
narrative_ontology:cs_reading_relation('b05aede8-1908-46ef-a3bc-2047441a34d8', catastrophe_memory_kernel__symbol_continuity_reading, influences).
narrative_ontology:cs_reading_relation('b05aede8-1908-46ef-a3bc-2047441a34d8', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('b05aede8-1908-46ef-a3bc-2047441a34d8', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('b05aede8-1908-46ef-a3bc-2047441a34d8', foundational, collective_memory_is_operational_knowledge).
narrative_ontology:cs_axiom_status(collective_memory_is_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('b05aede8-1908-46ef-a3bc-2047441a34d8', collective_memory_is_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('b05aede8-1908-46ef-a3bc-2047441a34d8', foundational, ritual_rehearsal_builds_adaptive_capacity).
narrative_ontology:cs_axiom_status(ritual_rehearsal_builds_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('b05aede8-1908-46ef-a3bc-2047441a34d8', ritual_rehearsal_builds_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('b05aede8-1908-46ef-a3bc-2047441a34d8', ancestral_survival_paradigm).
narrative_ontology:cs_drift_state('b05aede8-1908-46ef-a3bc-2047441a34d8', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b05aede8-1908-46ef-a3bc-2047441a34d8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_elders_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively participate in the rituals, transmitting them across generations. They are the primary beneficiaries of the adaptive capacity but also bear the costs of maintaining distinct boundaries and resisting assimilation, including social and economic penalties from the dominant culture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_members, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, community_members, payer).

% Guide the ritual practices, interpret their meaning, and ensure their faithful transmission. They benefit from the continuity and resilience of the community, and their authority is reinforced by their role in preserving survival knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_elders_leaders, agenda_setter,
    powerful, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, community_elders_leaders, beneficiary).

% Are the ultimate recipients of the adaptive capacity and survival strategies encoded in the rituals. They inherit the collective memory and the practical competence for navigating future persecutions, without direct participation in the constraint's enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).

% Are those who have chosen or been forced to abandon the community's rituals and assimilate into the dominant culture. They are excluded from the direct benefits of the survival competence and may bear the psychological and social costs of severed ties to their heritage.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilated_individuals, excluded,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, assimilated_individuals, payer).

% Represents the societal forces that historically or currently exert pressure for assimilation. While not directly participating in the ritual constraint, its presence and actions define the 'persecution' context that makes the survival competence necessary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_dominant_culture, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and behavioral patterns for survival in the face of external persecution, ensuring that critical adaptive capacity is transmitted across generations through ritual rehearsal.
% TRANSFER_FUNCTION: Transfers operational knowledge, resilience, and a shared framework for interpreting and responding to threats from past generations to present and future community members, at the cost of maintaining distinct cultural boundaries and resisting assimilation.
% ABSENT_VOICES: Assimilated individuals, who have opted out of the community's practices, would argue that the costs of maintaining these boundaries outweigh the benefits, or that the 'threat' is no longer as severe as the rituals imply. Their voices are absent from the internal discourse of the community.
% DISAPPEARANCE_RATIONALE: If the ritual practices and the memory kernel vanished overnight, the community would rapidly lose its collective adaptive capacity, making it highly vulnerable to assimilation or renewed persecution. Its distinct identity and survival strategies would erode, leading to a fundamental reorganization of its social and cultural structure.
% FOUNDING_PROBLEM: The existential threat of persecution and the need to transmit effective survival strategies and collective resilience across generations to ensure the community's continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of past persecutions, ongoing sociological studies of minority group resilience, and the continued existence of external pressures for assimilation corroborate that the founding problem remains live. Community narratives and scholarly analyses from outside the immediate community also attest to the efficacy of these rituals in maintaining group cohesion and survival capacity.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the 'cost' of ritual participation and boundary maintenance is substantial, but it is offset by the 'benefit' of collective survival and adaptive capacity. Suppression is high (0.70) due to the constant external pressure for assimilation, which the ritual actively resists, and the internal social pressure to conform to community norms. Theater ratio is low (0.10) because the rituals are highly functional, directly serving the purpose of survival and resilience, with minimal performative excess. Accessibility collapse is moderate (0.50) as assimilation is a clear alternative, but one with high costs for identity and community. Resistance is moderate (0.40) reflecting the ongoing struggle against external pressures and the internal effort required to maintain the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the rituals are a necessary, albeit costly, means of survival and identity preservation. From the perspective of assimilated individuals, the same rituals represent an unnecessary burden and a barrier to integration. The engine's classification as a Tangled Rope captures this dual nature, acknowledging both the vital coordination function and the asymmetric extraction of boundary-maintenance costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are both beneficiaries (receiving adaptive capacity) and payers (bearing the costs of boundary maintenance and ritual participation), placing them near the symmetric end, but with a slight tilt towards target due to the active enforcement required. Future generations are clear beneficiaries. Assimilated individuals are victims, paying the cost of lost community ties. Community elders and leaders are beneficiaries, as their authority and the community's continuity are preserved.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''catastrophe_memory_kernel'' or merely an aspect of a sibling reading?',
    'Comparative analysis of community narratives and ritual structures: if the primary emphasis and functional outcome consistently prioritize adaptive capacity over other functions (e.g., pure boundary maintenance), it is distinct.',
    'If not distinct, this reading might be subsumed under a sibling, altering its classification to reflect the dominant function of that sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinctness of the survival competence reading within the kernel.').

omega_variable(
    cost_benefit_balance_of_extraction,
    'Is the measured ''extraction'' (costs of boundary maintenance, resistance to assimilation) a necessary and proportionate cost for the ''coordination'' (survival competence), or does it represent an excessive burden?',
    'Longitudinal sociological studies comparing communities with varying levels of ritual adherence and persecution exposure, assessing survival rates and well-being. Economic analysis of the opportunity costs of non-assimilation.',
    'If costs are found to be excessive relative to benefits, the constraint leans more towards a Snare; if proportionate, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_balance_of_extraction, empirical, 'Assesses the proportionality of extraction to coordination benefits.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (external persecution, economic penalties for non-assimilation) or internalized (cognitive patterns, identity fusion that makes exit unthinkable)?',
    'Post-exit trajectory of individuals who leave the community: if suppression (e.g., self-imposed isolation, difficulty forming new social bonds) persists after the external extractive mechanisms are removed, it indicates internalized suppression.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making exit less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in resisting assimilation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
