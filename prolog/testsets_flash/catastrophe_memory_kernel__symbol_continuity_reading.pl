% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Preserves Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual functions to preserve symbolic
 *   continuity and collective identity across generations, particularly in
 *   communities that have experienced catastrophe. It is one reading of the
 *   'catastrophe_memory_kernel', focusing on the transmission of identity
 *   through shared symbols and practices. The constraint's primary function
 *   is coordination, but it carries a low level of extraction due to the
 *   rigidity it imposes on adaptive modification. The claimed type is 'rope'
 *   because it genuinely coordinates, but the metrics reflect a degree of
 *   performativity and cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Preserves Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a').
narrative_ontology:cs_kernel_codification('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', implicit).
narrative_ontology:cs_authority_grounding('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', practice).
narrative_ontology:cs_interpretation_layer_present('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a').
narrative_ontology:cs_reading_relation('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', foundational, symbolic_fidelity_preserves_identity).
narrative_ontology:cs_axiom_status(symbolic_fidelity_preserves_identity, holdable).
narrative_ontology:cs_axiom_grounding('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', symbolic_fidelity_preserves_identity, conventional).
narrative_ontology:cs_axiom('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', secondary, ritual_form_is_memory_container).
narrative_ontology:cs_axiom_status(ritual_form_is_memory_container, holdable).
narrative_ontology:cs_axiom_grounding('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', ritual_form_is_memory_container, conventional).
narrative_ontology:cs_reference_frame('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', contemporary_secular_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ffe2a4bf-78ce-4c05-a4dc-e68ebba3923a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that reinforce their shared history and identity, providing a sense of belonging and continuity. The cost is the rigidity of practice, which can feel anachronistic or burdensome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% Administer and interpret the rituals, ensuring their faithful transmission across generations. They benefit from the authority derived from preserving tradition but bear the burden of maintaining adherence in changing times.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, religious_leaders, agenda_setter,
    institutional, generational, constrained, local).

% The abstract concept of the tradition itself, which is sustained and given meaning by the ritual practice. It 'benefits' by persisting and remaining coherent over time.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).

% The potential for the ritual to evolve or adapt to new circumstances is constrained by the emphasis on strict symbolic continuity. This 'cost' is borne by the community's capacity for flexible response to change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).

% Study the ritual's function in preserving identity and memory, analyzing its social and psychological effects without direct participation or enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity by providing a shared symbolic framework and regular practices that link past, present, and future generations.
% TRANSFER_FUNCTION: Transfers symbolic meaning, historical narratives, and a sense of belonging from the past to current and future community members, at the cost of ritual rigidity.
% ABSENT_VOICES: Reformers or modernizers within the community who advocate for adapting rituals to contemporary needs, arguing that strict adherence to historical forms can hinder relevance and engagement. Their voices are often marginalized by the emphasis on tradition.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community's shared identity and collective memory would fragment, leading to a loss of cohesion and a re-evaluation of their historical narrative. The social fabric would need to reorganize to find new ways of transmitting meaning.
% FOUNDING_PROBLEM: To prevent the dissolution of collective identity and the loss of shared memory following a catastrophic event or period of persecution, ensuring the community's survival as a distinct entity.
% FOUNDING_PROBLEM_CORROBORATION: Community historians and sociologists, alongside religious leaders, corroborate that the threat of identity dissolution remains, particularly in diasporic or secularizing contexts. Independent studies of collective memory and cultural transmission support the ongoing need for such practices.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary 'cost' is the foregone opportunity for adaptive change, rather than direct material extraction. Suppression is moderate (0.4) as adherence is largely cultural and identity-driven, not coercively enforced. Theater ratio is high (0.6) because the 'performance' of the ritual is central to its function of symbolic transmission, even if its direct operational utility is low. Accessibility collapse is low (0.3) as alternatives for identity formation exist, but they come at the cost of breaking with tradition. Resistance is low (0.15) because the identity-locked nature of participation means most members do not actively resist the core practice, even if they may question its rigidity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital rope, providing essential identity and connection. From an analytical perspective focused on adaptive capacity, the same ritual imposes a cost by limiting flexibility. The engine's computation of per-seat types will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and the abstract 'tradition continuity' are beneficiaries, as the ritual directly serves their interests in identity and persistence. Religious leaders are agenda-setters, guiding the ritual's practice. 'Adaptive modification' is a victim, representing the cost of rigidity. The identity-locked exit option for community members means their directionality is shifted slightly towards the target end, even as beneficiaries, reflecting the internal cost of non-participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_adaptive_function,
    'Is the primary function of the ritual symbolic continuity, or does it also encode and transmit adaptive survival competence?',
    'Longitudinal ethnographic studies comparing ritual adherence with community resilience and survival rates in subsequent crises. If communities with stronger ritual adherence show higher adaptive capacity, the survival_competence_reading gains empirical support.',
    'If the adaptive function is significant, the extractiveness of ritual rigidity might be re-evaluated as a necessary cost for a more vital coordination function (shifting towards a stronger Rope or even Mountain for survival). If purely symbolic, the current low extractiveness and high theater ratio are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_adaptive_function, empirical, 'Distinguishes between symbolic and adaptive functions of ritual.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for community members a genuine internal commitment versus a form of internalized suppression?',
    'Studies of post-exit psychological trajectories: if individuals who leave the community experience lasting identity fragmentation and social isolation, it suggests a stronger internalized suppression component. If they successfully integrate new identities, it points to genuine commitment.',
    'If internalized suppression is a significant factor, the effective suppression for community members is higher than the base metric suggests, potentially shifting their seat classification towards a Snare. If it''s primarily genuine identity fusion, the current Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, conceptual, 'Clarifies the nature of identity-locked participation.').

omega_variable(
    ritual_rigidity_cost_vs_benefit,
    'Is the ''cost'' of adaptive modification truly a net negative, or is the rigidity itself a necessary component of symbolic stability and identity strength?',
    'Comparative studies of communities with varying degrees of ritual flexibility: if more flexible communities show weaker identity or faster dissolution, it suggests rigidity is a benefit. If they show greater resilience and adaptation, it''s a net cost.',
    'If rigidity is a necessary benefit, the ''adaptive_modification'' victim status might be re-evaluated, potentially lowering the overall extractiveness. If it''s a clear cost, the current metrics are appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_rigidity_cost_vs_benefit, preference, 'Evaluates the normative trade-off between ritual rigidity and adaptive capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.59).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.25).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_kernel', focusing on symbolic continuity. Its sibling readings address adaptive competence, trauma encoding, and boundary maintenance, each with distinct structural properties and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
