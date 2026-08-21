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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that originated as a critical
 *   mechanism for preserving survival-competence and social cohesion after
 *   catastrophic events. Over time, with the advent of modernity and the
 *   mitigation of original threats, its direct operational function has
 *   atrophied. It persists primarily as a mourning practice and a vehicle for
 *   collective identity, making it a Piton: its original mandate has largely
 *   expired, but it is maintained due to institutional inertia and its
 *   evolved symbolic role, extracting diffuse costs from inheritors without
 *   concentrated benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.4).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.5).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '65af0155-440a-4bf9-a12f-809261bab47a').
narrative_ontology:cs_kernel_codification('65af0155-440a-4bf9-a12f-809261bab47a', implicit).
narrative_ontology:cs_authority_grounding('65af0155-440a-4bf9-a12f-809261bab47a', practice).
narrative_ontology:cs_interpretation_layer_present('65af0155-440a-4bf9-a12f-809261bab47a').
narrative_ontology:cs_reading_relation('65af0155-440a-4bf9-a12f-809261bab47a', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('65af0155-440a-4bf9-a12f-809261bab47a', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('65af0155-440a-4bf9-a12f-809261bab47a', foundational, ritual_evolves_with_context).
narrative_ontology:cs_axiom_status(ritual_evolves_with_context, holdable).
narrative_ontology:cs_axiom_grounding('65af0155-440a-4bf9-a12f-809261bab47a', ritual_evolves_with_context, empirically_contingent).
narrative_ontology:cs_axiom('65af0155-440a-4bf9-a12f-809261bab47a', secondary, identity_from_shared_practice).
narrative_ontology:cs_axiom_status(identity_from_shared_practice, holdable).
narrative_ontology:cs_axiom_grounding('65af0155-440a-4bf9-a12f-809261bab47a', identity_from_shared_practice, conventional).
narrative_ontology:cs_reference_frame('65af0155-440a-4bf9-a12f-809261bab47a', survival_competence_ritual).
narrative_ontology:cs_drift_state('65af0155-440a-4bf9-a12f-809261bab47a', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65af0155-440a-4bf9-a12f-809261bab47a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_inheritors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community leaders and elders who actively preserve and transmit the ritual, believing it essential for collective identity and continuity. They benefit from the social cohesion and status derived from maintaining tradition, even if its original function has atrophied.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers, agenda_setter,
    organized, generational, identity_locked, local).

% Younger members of the community who inherit the obligation to perform the ritual. They bear the costs in time, effort, and resources, often without fully understanding or experiencing the original adaptive payoff, leading to a sense of burden or disconnect.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_inheritors, payer,
    moderate, biographical, constrained, local).

% The ancestral community that originally developed and practiced the ritual, for whom it served a vital survival-competence function in the face of catastrophe. This is a conceptual agent representing the past beneficiaries of the ritual's original efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community).

% External researchers who study the ritual's evolution, its social function, and its impact on the community. They analyze the gap between its historical purpose and its contemporary practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, coordinated the transmission of critical survival knowledge and collective action in response to recurring catastrophe. Now, it coordinates collective memory, shared identity, and social cohesion within the community.
% TRANSFER_FUNCTION: Historically, transferred practical skills, threat-recognition, and resilience strategies across generations. Presently, it transfers symbolic meaning, cultural heritage, and a sense of belonging, often without direct operational utility.
% ABSENT_VOICES: Those who have left the community due to the perceived anachronism or burden of the ritual, or those who advocate for purely secular or modernized forms of catastrophe memory and community building.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community's deep-seated sense of shared history, collective identity, and intergenerational connection would be profoundly disrupted. While practical survival might not be immediately impacted, the social and cultural fabric would require significant reorganization to fill the void.
% FOUNDING_PROBLEM: To preserve vital survival knowledge, maintain social cohesion, and process collective trauma in the aftermath of a recurring catastrophic event, ensuring the community's long-term resilience.
% FOUNDING_PROBLEM_CORROBORATION: Historical and anthropological records corroborate the ritual's original survival-competence function. Contemporary community leaders assert its ongoing necessity for identity and cultural preservation, while some younger members and external observers question its direct relevance to modern challenges.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` declines over time as the direct adaptive payoff diminishes, but it remains non-zero due to the costs borne by inheritors. `Theater_ratio` increases as the ritual becomes more performative and less functionally critical. `Suppression` decreases from high (when survival was at stake) to moderate (social pressure and identity-lock). `Accessibility_collapse` and `resistance` reflect the shift from a necessary practice to a burdensome tradition in a world with alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The `in_group_identity_maintainers` perceive the ritual as a vital, albeit evolved, coordination mechanism for cultural survival. The `present_generation_inheritors` may experience it as an extractive burden, a 'tax' on their identity with diminishing returns. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The `in_group_identity_maintainers` act as agenda-setters and beneficiaries, deriving social cohesion and status from the ritual's preservation. The `present_generation_inheritors` are payers, bearing the costs without the original adaptive benefits. The `historical_community` is a conceptual beneficiary of the ritual's past efficacy. `Anthropological_observers` provide an analytical perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_function_residue,
    'To what extent does the ritual still preserve any latent or indirect survival-competence, even if not explicitly acknowledged?',
    'Longitudinal ethnographic study tracking community resilience and adaptive capacity in the face of novel challenges, correlating with ritual adherence.',
    'If significant latent survival function is found, the constraint''s extractiveness would be re-evaluated downward, and its classification might shift closer to a Rope or Tangled Rope, as the ''payer'' seats receive more unacknowledged benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_function_residue, empirical, 'Assessing residual adaptive utility of the ritual.').

omega_variable(
    identity_lock_vs_social_pressure,
    'Is the persistence of the ritual primarily due to internalized identity-lock among inheritors, or to external social pressure from maintainers?',
    'Observational studies of individuals who attempt to exit or modify ritual practice, documenting internal conflict versus external sanctions.',
    'If internalized identity-lock is dominant, the effective suppression for ''present_generation_inheritors'' is higher than structural measures suggest, as the ''cost of exit'' is self-imposed. If external pressure dominates, the ''agenda_setter''s'' role in maintaining extraction is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_social_pressure, empirical, 'Distinguishing internalized vs. structural suppression mechanisms for ritual adherence.').

omega_variable(
    atrophy_vs_repurposing_framing,
    'Is the shift in the ritual''s function best understood as ''atrophy'' (loss of original purpose) or ''repurposing'' (adaptation to new social needs)?',
    'Conceptual analysis of the community''s own narrative frames and the degree to which new functions are explicitly articulated and valued, versus the implicit persistence of old forms.',
    'If ''repurposing'' is the dominant frame, the ''theater_ratio'' might be re-evaluated downward, and the ''extractiveness'' might be seen as a legitimate cost for a new, valued coordination function, potentially shifting the classification away from Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrophy_vs_repurposing_framing, conceptual, 'Framing the functional evolution of the ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.65).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.5).


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
