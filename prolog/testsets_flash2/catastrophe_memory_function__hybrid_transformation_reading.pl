% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid transformation' reading of
 *   catastrophe memory rituals, where the ritual simultaneously functions as
 *   a mourning practice (D1/D4) and a mechanism for transmitting survival
 *   competence (D5). The Passover Seder, for example, incorporates bitter
 *   herbs to recall suffering (mourning) alongside the structured performance
 *   of the meal and narrative, which reinforces collective identity and
 *   adaptive capacity (survival). This reading emphasizes the integrated
 *   nature of these functions, rather than prioritizing one over the other.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'f9461c08-ee91-4ca7-a555-f97cb0b45724').
narrative_ontology:cs_kernel_codification('f9461c08-ee91-4ca7-a555-f97cb0b45724', formalized).
narrative_ontology:cs_authority_grounding('f9461c08-ee91-4ca7-a555-f97cb0b45724', lineage).
narrative_ontology:cs_interpretation_layer_present('f9461c08-ee91-4ca7-a555-f97cb0b45724').
narrative_ontology:cs_reading_relation('f9461c08-ee91-4ca7-a555-f97cb0b45724', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9461c08-ee91-4ca7-a555-f97cb0b45724', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('f9461c08-ee91-4ca7-a555-f97cb0b45724', foundational, memory_and_adaptation_are_intertwined).
narrative_ontology:cs_axiom_status(memory_and_adaptation_are_intertwined, holdable).
narrative_ontology:cs_axiom_grounding('f9461c08-ee91-4ca7-a555-f97cb0b45724', memory_and_adaptation_are_intertwined, deontological).
narrative_ontology:cs_axiom('f9461c08-ee91-4ca7-a555-f97cb0b45724', foundational, ritual_as_transformative_process).
narrative_ontology:cs_axiom_status(ritual_as_transformative_process, holdable).
narrative_ontology:cs_axiom_grounding('f9461c08-ee91-4ca7-a555-f97cb0b45724', ritual_as_transformative_process, conventional).
narrative_ontology:cs_reference_frame('f9461c08-ee91-4ca7-a555-f97cb0b45724', integrated_commemorative_resilience).
narrative_ontology:cs_drift_state('f9461c08-ee91-4ca7-a555-f97cb0b45724', contemporary_secular_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f9461c08-ee91-4ca7-a555-f97cb0b45724', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, religious_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals like Passover, which provide a structured way to remember past catastrophes (e.g., the Exodus) while simultaneously rehearsing adaptive strategies for future challenges. The ritual reinforces group identity and resilience, but also requires emotional engagement with difficult memories.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% Administer and interpret the rituals, ensuring their continuity and relevance. They benefit from the cohesion and meaning these rituals provide to their communities, but are constrained by tradition and the need to balance commemorative and adaptive functions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, religious_leaders, agenda_setter,
    institutional, generational, constrained, regional).

% Analyze the structure and function of catastrophe memory rituals, observing how they simultaneously encode mourning and transmit survival competence. Their work helps to articulate the hybrid nature of these practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, historical_memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory by providing a shared framework for commemorating past catastrophes and transmitting adaptive cultural knowledge across generations, fostering group cohesion and resilience.
% TRANSFER_FUNCTION: Transfers historical memory, emotional processing of loss, and practical survival strategies from past generations to present and future community members, reinforcing group identity and continuity.
% ABSENT_VOICES: Those who reject the historical narrative or the ritual's efficacy might be absent, arguing that the focus on past trauma hinders present adaptation or that the 'survival competence' is a rationalization for continued adherence to tradition. Their voices are often marginalized by the strong communal identity reinforced by the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a central mechanism for collective memory, identity formation, and intergenerational transmission of adaptive strategies. This would lead to a significant reordering of social structures, potentially fragmenting the group or diminishing its resilience in the face of new challenges.
% FOUNDING_PROBLEM: How to preserve the memory of a catastrophic past (e.g., persecution, exile) and the associated grief, while simultaneously equipping the community with the cultural and practical tools to survive and thrive in an uncertain future.
% FOUNDING_PROBLEM_CORROBORATION: Community members and religious leaders attest to the ongoing need for both memory and adaptation. Historical memory scholars corroborate the dual function, noting that communities facing ongoing threats continue to rely on such hybrid rituals for resilience, even if the specific 'catastrophe' has shifted.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the ritual primarily serves the community's needs for memory and resilience, with minimal coercive overhead. Suppression is also low (0.20) as participation is largely voluntary, driven by cultural and identity-based motivations rather than overt coercion. The 'cost' is primarily emotional and temporal engagement. Theater ratio is low (0.10) because the ritual's functions are genuinely performed and experienced, not merely for show. The metrics reflect a robust, self-sustaining coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes the hybrid nature, other readings might prioritize either the mourning aspect (mourning_practice_reading) or the survival aspect (survival_competence_reading). This reading argues that the full function is only understood when both are considered as integrated. The engine's classification will reflect the low extraction and suppression inherent in this integrated view, which is distinct from a reading that might see the 'cost' of mourning as a form of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries, gaining identity, memory, and adaptive capacity. Religious leaders are agenda-setters, facilitating the ritual and benefiting from community cohesion. There are no identifiable 'victims' in this reading, as the costs (emotional engagement, time) are integral to the benefits and are voluntarily borne. The 'identity_locked' exit option for community members reflects the deep integration of the ritual into their self-concept and group belonging.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_mourning_and_adaptation,
    'What is the optimal balance between the mourning-practice and survival-competence functions within the ritual for long-term community resilience?',
    'Longitudinal ethnographic studies comparing communities with different ritual emphases, assessing their psychological well-being, social cohesion, and adaptive success over generations.',
    'If an imbalance is found to be detrimental, the ritual might be re-evaluated as having a subtle extractive component (e.g., excessive focus on mourning hindering adaptation, or vice versa), potentially shifting its classification towards a Tangled Rope for specific sub-groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_mourning_and_adaptation, empirical, 'Assesses the functional equilibrium between the dual purposes of the ritual.').

omega_variable(
    identity_lock_vs_genuine_adherence,
    'To what extent is participation in the ritual driven by genuine belief in its hybrid function versus an ''identity_locked'' adherence where exit is unthinkable due to social and self-concept costs?',
    'Sociological surveys and qualitative interviews with community members, including those who have considered or attempted to exit, to differentiate intrinsic motivation from social pressure and identity fusion.',
    'If identity-lock is the dominant mechanism, the ''suppression'' metric might be effectively higher for individuals, even if the overall constraint appears low-coercion. This could shift the individual''s per-seat classification towards a Snare, despite the overall Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_adherence, conceptual, 'Distinguishes between voluntary participation and identity-based entrapment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.19).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 75, 0.21).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
