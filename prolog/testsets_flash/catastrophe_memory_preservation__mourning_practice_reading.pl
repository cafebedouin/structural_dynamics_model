% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Ritual as Mourning Practice for Catastrophe Memory Preservation
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes ritual practice as a mechanism for preserving
 *   the memory of a catastrophe and reinforcing collective identity, without
 *   direct operational transfer or coercive enforcement. It is a 'mourning
 *   practice' reading of the broader 'catastrophe_memory_preservation'
 *   kernel. Participation is voluntary, and the primary benefit is in-group
 *   cohesion and symbolic continuity. The constraint is claimed as a Rope due
 *   to its coordination function and low extractiveness/suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Ritual as Mourning Practice for Catastrophe Memory Preservation").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'fceb4b9c-081d-4daf-ad8f-f50fefd3541a').
narrative_ontology:cs_kernel_codification('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', implicit).
narrative_ontology:cs_authority_grounding('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', practice).
narrative_ontology:cs_interpretation_layer_present('fceb4b9c-081d-4daf-ad8f-f50fefd3541a').
narrative_ontology:cs_reading_relation('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', foundational, ritual_as_symbolic_transmission).
narrative_ontology:cs_axiom_status(ritual_as_symbolic_transmission, holdable).
narrative_ontology:cs_axiom_grounding('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', ritual_as_symbolic_transmission, conventional).
narrative_ontology:cs_axiom('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', foundational, collective_identity_through_shared_memory).
narrative_ontology:cs_axiom_status(collective_identity_through_shared_memory, holdable).
narrative_ontology:cs_axiom_grounding('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', collective_identity_through_shared_memory, deontological).
narrative_ontology:cs_reference_frame('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', ritual_as_identity_marker).
narrative_ontology:cs_drift_state('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', contemporary_secularization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fceb4b9c-081d-4daf-ad8f-f50fefd3541a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, collective_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate voluntarily in rituals to connect with shared history, reinforce group bonds, and process collective trauma. They gain a sense of belonging and continuity. Exit is possible but means losing access to this form of collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, community_members, beneficiary,
    moderate, biographical, mobile, local).

% Organize and guide the mourning rituals, ensuring their proper execution and transmission across generations. They derive authority and status from this role, but are also bound by the tradition's integrity. Their role is to facilitate, not coerce.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_leaders, agenda_setter,
    organized, generational, constrained, local).

% The abstract entity of the group's shared self-understanding and historical narrative. It is reinforced and sustained by the ritual practice, providing continuity and meaning to the community members.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, collective_identity, beneficiary,
    analytical, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__mourning_practice_reading, collective_identity).

% Study the ritual's function in preserving memory and identity, without direct participation or benefit. They analyze its structural role and impact on the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and emotional processing of past catastrophes, ensuring shared understanding and reinforcing group cohesion through symbolic action.
% TRANSFER_FUNCTION: Transfers symbolic meaning, emotional resonance, and a sense of shared history across generations, from the past event to contemporary community members.
% ABSENT_VOICES: Those who reject the community's historical narrative or the efficacy of ritual for processing trauma might object, but their absence is typically due to self-selection rather than active exclusion. They are not part of the community that defines itself through this ritual.
% DISAPPEARANCE_RATIONALE: If the mourning practice vanished, the community's collective memory of the catastrophe would fragment, shared identity would weaken, and the social bonds reinforced by the ritual would erode, leading to a gradual dissolution of the group's distinctiveness.
% FOUNDING_PROBLEM: The challenge of transmitting the memory and emotional impact of a catastrophic event across generations to maintain collective identity and prevent its recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of the community, historical records of the catastrophe, and direct testimony from community elders and members (outside of ritual leaders) corroborate that the problem of memory transmission and identity maintenance remains live, even if the direct threat has passed.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because participation is voluntary and the 'cost' is primarily time and emotional engagement, not material transfer. Suppression is negligible (0.05) as there are no active enforcement mechanisms; non-participation simply means opting out of the collective identity. Theater ratio is low (0.2) because the ritual's symbolic function is its primary purpose, not a cover for other activities. Accessibility collapse is low (0.2) as alternatives for individual memory or identity formation exist, but they do not offer the same collective benefits.
 *
 * PERSPECTIVAL GAP:
 *   All participants generally experience this as a beneficial coordination mechanism. The primary 'gap' is between participants who experience the direct emotional and social benefits, and external observers who analyze its structural function in collective memory. However, this is an analytical, not an extractive, gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries (d near 0.0) as they gain collective identity and emotional processing. Ritual leaders are agenda-setters (d near 0.5) as they facilitate the practice and gain status, but also bear the responsibility of maintaining the tradition. Collective identity is an abstract beneficiary. There are no identifiable victims, as participation is opt-in and costs are diffuse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_efficacy_ambiguity,
    'Does the ritual genuinely preserve the ''memory'' of the catastrophe in a way that impacts future behavior, or is it primarily a symbolic act of remembrance?',
    'Longitudinal studies tracking behavioral responses to new threats in communities with and without such rituals, or cognitive studies on the transmission fidelity of ritualized memory.',
    'If it primarily impacts behavior, the constraint might have a latent ''survival_competence'' function, shifting its classification towards a more functional Rope or even a Mountain (if the competence is truly irreducible). If purely symbolic, its current Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_ambiguity, empirical, 'Ambiguity regarding the operational vs. symbolic efficacy of the ritual.').

omega_variable(
    reading_distinction_clarity,
    'Is the distinction between ''mourning practice'' and ''survival competence'' truly separable, or do all rituals inherently contain elements of both?',
    'Conceptual analysis and cross-cultural comparison of ritual forms, seeking cases where one function is entirely absent without the other. If inseparable, the kernel itself might be a single, complex constraint.',
    'If the functions are inseparable, the ''mourning_practice_reading'' and ''survival_competence_reading'' might collapse into a single, more complex constraint, potentially a Tangled Rope if latent extraction is found.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Conceptual boundary between different functions of ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 75, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
