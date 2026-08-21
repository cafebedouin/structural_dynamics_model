% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function: Mourning Practice Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that preserves mourning and
 *   boundary norms, primarily functioning to maintain group identity through
 *   memorial obligation. It is a specific reading of the broader
 *   'catastrophe_memory_function' kernel, emphasizing the D1/D4
 *   (mourning/boundary) aspects without incorporating D5 (survival
 *   competence) transmission. The ritual coordinates collective memory and
 *   identity but extracts social costs from those who deviate from its
 *   prescribed forms, making it a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function: Mourning Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'f560a10e-9929-4631-a9eb-0b631d689203').
narrative_ontology:cs_kernel_codification('f560a10e-9929-4631-a9eb-0b631d689203', formalized).
narrative_ontology:cs_authority_grounding('f560a10e-9929-4631-a9eb-0b631d689203', lineage).
narrative_ontology:cs_interpretation_layer_present('f560a10e-9929-4631-a9eb-0b631d689203').
narrative_ontology:cs_reading_relation('f560a10e-9929-4631-a9eb-0b631d689203', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f560a10e-9929-4631-a9eb-0b631d689203', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('f560a10e-9929-4631-a9eb-0b631d689203', foundational, memorial_obligation_is_paramount).
narrative_ontology:cs_axiom_status(memorial_obligation_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f560a10e-9929-4631-a9eb-0b631d689203', memorial_obligation_is_paramount, deontological).
narrative_ontology:cs_axiom('f560a10e-9929-4631-a9eb-0b631d689203', foundational, group_identity_through_shared_grief).
narrative_ontology:cs_axiom_status(group_identity_through_shared_grief, holdable).
narrative_ontology:cs_axiom_grounding('f560a10e-9929-4631-a9eb-0b631d689203', group_identity_through_shared_grief, conventional).
narrative_ontology:cs_reference_frame('f560a10e-9929-4631-a9eb-0b631d689203', unbroken_commemorative_tradition).
narrative_ontology:cs_drift_state('f560a10e-9929-4631-a9eb-0b631d689203', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f560a10e-9929-4631-a9eb-0b631d689203', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_leaders).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, deviant_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, gaining a reinforced sense of group identity, belonging, and shared memory. They bear the costs of adherence, including time, emotional labor, and conformity to strict norms, which can be substantial.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_members, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, community_members, payer).

% Administer, interpret, and enforce the mourning practices and boundary norms. They benefit from the social capital and authority derived from their role in preserving the group's identity and memory, and bear the responsibility for its continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Are those who, while often still identifying with the group, do not fully adhere to the prescribed mourning practices or boundary norms. They pay a social cost through shaming, marginalization, or exclusion from core communal spaces, experiencing extraction through social pressure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, deviant_members, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, deviant_members, excluded).

% Study the ritual and its effects from an academic or detached perspective. They are not subject to its internal norms or benefits, but can analyze its structural properties and impact on participants.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective memory and group identity through shared mourning practices, ensuring cultural continuity and social cohesion across generations.
% TRANSFER_FUNCTION: Transfers emotional labor, time, and adherence to ritual norms from individual members to the collective, in exchange for a reinforced sense of belonging and shared identity. It also transfers social capital and authority to community leaders who administer the ritual.
% ABSENT_VOICES: Those who question the necessity or specific form of the mourning practice, or who advocate for alternative ways of remembering, are often marginalized or excluded from the core ritual space. Their voices are suppressed by the strong social pressure to conform to established norms.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the group's collective memory would fragment, its identity would weaken, and its social cohesion would erode. The community would struggle to transmit its cultural heritage across generations, leading to significant social and cultural reorganization.
% FOUNDING_PROBLEM: The need to collectively process catastrophic loss, preserve the memory of past suffering, and ensure the continuity of the group's identity and values in the face of existential threats.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of collective memory and ritual, sociological analyses of group cohesion, and historical accounts of communities enduring catastrophe all corroborate the enduring nature of these problems, independent of the specific religious community's claims.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the social and emotional costs of strict adherence to ritual norms, as well as the implicit extraction from 'deviant_members' through social pressure and exclusion. Suppression (0.7) is high due to the strong social enforcement of communal identity and boundary maintenance. The theater ratio (0.4) acknowledges the performative nature of ritual, which is essential for its function but can also mask underlying inertia. The claimed type is Tangled Rope because it genuinely coordinates group identity and memory (benefiting members and leaders) but simultaneously extracts from those who do not conform to its strictures.
 *
 * PERSPECTIVAL GAP:
 *   Community members and leaders perceive the ritual as essential for group survival and identity, viewing any 'extraction' as a necessary cost of belonging. Deviant members, however, experience the same structure as a source of pressure and exclusion. The engine's classification as Tangled Rope captures this divergence, showing that the coordination function comes with asymmetric costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and leaders are beneficiaries, gaining identity and authority, respectively. Deviant members are targets, bearing the social costs of non-conformity. External observers are analytical, outside the constraint's direct influence. The 'identity_locked' exit option for members and deviants highlights the deep connection between the ritual and their self-concept, making exit extremely costly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_survival_emphasis,
    'Is the ritual''s primary function truly limited to mourning and boundary maintenance, or does it implicitly transmit adaptive survival competence despite this reading''s explicit focus?',
    'Longitudinal ethnographic studies observing the actual behavioral outcomes and adaptive capacities of groups practicing the ritual, compared to groups without it.',
    'If significant adaptive competence is implicitly transmitted, this reading''s extractiveness might be re-evaluated as a more legitimate cost of a broader coordination function, potentially shifting its classification closer to a Rope or even a Scaffold (if the adaptive function is transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_survival_emphasis, empirical, 'Whether the ritual''s function is purely commemorative or also adaptive.').

omega_variable(
    exclusion_as_necessary_boundary,
    'To what extent is the social exclusion of ''deviant_members'' a necessary mechanism for maintaining group identity and boundary norms, versus an unnecessary or excessive form of social control?',
    'Comparative studies of similar groups with varying degrees of tolerance for deviation, assessing their long-term identity cohesion and resilience.',
    'If exclusion is found to be excessive or unnecessary, the ''suppression'' and ''extractiveness'' metrics would be re-evaluated upwards, strengthening the Snare-like aspects of the Tangled Rope classification. If found necessary, the extraction might be seen as a more integral, albeit still costly, part of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_as_necessary_boundary, conceptual, 'The necessity and proportionality of social exclusion for boundary maintenance.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is the ''mourning_practice_reading'' of the ''catastrophe_memory_function'' kernel. Sibling readings (''survival_competence_reading'', ''hybrid_transformation_reading'') emphasize adaptive mechanisms or a combination, whereas this reading focuses purely on commemorative obligation and boundary maintenance. The core disagreement is whether the ritual primarily transmits adaptive capacity or solely preserves memory and identity.',
    'Analysis of the explicit theological and philosophical texts, as well as the lived experiences and interpretations of practitioners, to determine the dominant intent and perceived function of the ritual within different interpretive communities.',
    'If this reading''s narrow focus on mourning and boundary maintenance is found to be a minority or historically superseded interpretation, its structural relationship to the kernel might be re-evaluated, potentially shifting its ''status'' or ''grounding_type'' in the CS structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing this reading''s focus from sibling interpretations of the catastrophe memory function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_function' kernel. This 'mourning_practice_reading' focuses on the ritual's role in preserving collective memory and maintaining group identity through memorial obligation and boundary norms (D1/D4), distinct from readings that emphasize adaptive survival competence (D5) or a hybrid of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
