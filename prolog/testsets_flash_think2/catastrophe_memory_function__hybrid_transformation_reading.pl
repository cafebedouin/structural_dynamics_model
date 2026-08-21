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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint describes a specific reading of how ritual functions in
 *   the aftermath of catastrophe: as a hybrid mechanism that simultaneously
 *   preserves the memory of loss (mourning practice) and transmits adaptive
 *   strategies for group survival (survival competence). The Passover Seder,
 *   with its bitter herbs (mourning) and structured performance (survival
 *   rehearsal), serves as a prime example. This reading emphasizes the
 *   integrated and mutually reinforcing nature of these two functions,
 *   positioning the ritual as a beneficial coordination mechanism for the
 *   community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '9acbb1fa-35c7-4c9e-920f-5266fc17039f').
narrative_ontology:cs_kernel_codification('9acbb1fa-35c7-4c9e-920f-5266fc17039f', formalized).
narrative_ontology:cs_authority_grounding('9acbb1fa-35c7-4c9e-920f-5266fc17039f', lineage).
narrative_ontology:cs_interpretation_layer_present('9acbb1fa-35c7-4c9e-920f-5266fc17039f').
narrative_ontology:cs_reading_relation('9acbb1fa-35c7-4c9e-920f-5266fc17039f', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('9acbb1fa-35c7-4c9e-920f-5266fc17039f', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('9acbb1fa-35c7-4c9e-920f-5266fc17039f', foundational, memory_and_adaptive_capacity_are_interdependent).
narrative_ontology:cs_axiom_status(memory_and_adaptive_capacity_are_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('9acbb1fa-35c7-4c9e-920f-5266fc17039f', memory_and_adaptive_capacity_are_interdependent, conventional).
narrative_ontology:cs_reference_frame('9acbb1fa-35c7-4c9e-920f-5266fc17039f', integrated_ritual_transmission).
narrative_ontology:cs_drift_state('9acbb1fa-35c7-4c9e-920f-5266fc17039f', contemporary_globalized_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('9acbb1fa-35c7-4c9e-920f-5266fc17039f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, gaining a sense of shared identity, historical continuity, and adaptive resilience. They bear the costs of time, emotional labor, and adherence to ritual norms, but are net beneficiaries of the collective memory and competence transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, community_members, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, community_members, payer).

% Inherit the collective memory of catastrophe and the adaptive mechanisms encoded in the ritual, which are crucial for their group's continuity and resilience. They are born into the framework established by the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Administer and interpret the ritual, ensuring its continuity and fidelity across generations. They benefit from the status and authority derived from their role, and bear the responsibility for the ritual's effective transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_leaders_keepers, agenda_setter,
    institutional, generational, constrained, local).

% Individuals who do not participate in or identify with the ritual. They are excluded from its benefits of collective memory and adaptive competence, but also from its demands and social pressures. Their exit is relatively unconstrained by the ritual itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, outsiders_dissenters, excluded,
    powerless, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective memory of a catastrophic event and simultaneously transmits adaptive mechanisms for group survival and transformation, ensuring both emotional processing of loss and practical resilience for future challenges.
% TRANSFER_FUNCTION: Transfers shared historical memory, emotional solidarity, and practical adaptive strategies across generations within a community, from past and present participants to future members.
% ABSENT_VOICES: Those who reject the ritual's dual function, perhaps emphasizing only mourning or only adaptation, or those who find the ritual's demands too burdensome. They would argue for a simpler, less demanding, or more focused approach to collective memory.
% DISAPPEARANCE_RATIONALE: If this ritual vanished, the community would lose a central mechanism for processing collective trauma and transmitting vital survival competence. Memory would fragment, identity would weaken, and adaptive capacity would diminish, leading to a significant reorganization of social and cultural life.
% FOUNDING_PROBLEM: The problem of how a community can collectively mourn a catastrophic loss while simultaneously ensuring its long-term survival and adaptation in the face of ongoing threats.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe communities, historical records of ritual development, and the lived experience of community elders all corroborate that the dual challenge of mourning and adaptation remains a live problem for many groups, and that such rituals are central to addressing it.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a 'rope' because it genuinely solves a collective action problem (how to collectively mourn and adapt) with net benefits for participants. Extractiveness is low (0.25) as the demands of the ritual are generally perceived as necessary for group well-being. Suppression is moderate (0.40), reflecting social pressure to conform to important group rituals, but not outright coercion. Theater ratio is 0.30, acknowledging the performative nature of ritual while affirming its live, functional role. Accessibility collapse is high (0.70) because few alternatives exist for such a deeply integrated, dual-purpose collective mechanism. Resistance is low (0.20) due to the perceived benefits and strong identity-locking mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes the integrated benefit, other readings might highlight the burden of mourning or the rigidity of adaptive mechanisms. However, from the perspective of this 'hybrid transformation' reading, the functions are synergistic, leading to a net positive outcome for the community.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, gaining identity, memory, and adaptive capacity. Ritual leaders act as agenda-setters, facilitating the process and deriving status. Outsiders are excluded, but the constraint's primary function is not to extract from them. The 'identity_locked' exit option for community members reflects the deep integration of the ritual with their self-concept and group belonging.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_emphasis_ambiguity,
    'Is the ritual''s primary function truly a hybrid of mourning and adaptation, or does one function implicitly dominate the other in practice?',
    'Longitudinal ethnographic studies observing how communities prioritize or de-emphasize aspects of the ritual during periods of stability versus crisis, or during generational shifts.',
    'If one function consistently dominates, the constraint might lean towards a ''mourning_practice_reading'' (potentially higher emotional extraction) or a ''survival_competence_reading'' (potentially higher instrumental demands), altering its classification from a pure ''rope'' to a ''tangled_rope'' if asymmetric costs emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_emphasis_ambiguity, empirical, 'Ambiguity in the practical emphasis of the ritual''s dual functions.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent does ''identity_locked'' exit for community members represent genuine belonging versus internalized social coercion?',
    'Studies of individuals who successfully exit the community or ritual tradition: if exit is followed by severe psychological distress or social ostracization, it suggests a higher degree of internalized coercion than currently measured.',
    'If internalized coercion is higher, the effective suppression for community members is higher, potentially shifting the constraint towards a ''tangled_rope'' or ''snare'' for that seat, as the ''rope'' classification assumes minimal coercive overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing genuine identity fusion from internalized social coercion in ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 75, 0.29).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.4).


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
