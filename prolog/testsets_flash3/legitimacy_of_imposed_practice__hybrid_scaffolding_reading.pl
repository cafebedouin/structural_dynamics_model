% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Practices
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the process by which a top-down state mandate
 *   for cultural change (e.g., dress codes, calendar reforms) achieves
 *   partial success through a 'hybrid scaffolding' approach. This involves
 *   not just decree and enforcement, but also ideological messaging, elite
 *   modeling, and the creation of new institutions (like schools or media)
 *   that generate a 'quasi-endogenous pull' for the new practices. Pure
 *   decree (exogenous_override_reading) often fails, while pure bottom-up
 *   adoption (endogenous_climb_reading) is too slow for state-building
 *   projects. This reading focuses on the mixed results: partial displacement
 *   of old practices, often leading to hybrid forms, and differential
 *   adoption based on access to scaffolding infrastructure.
 *
 * KEY AGENTS:
 *   - state_modernization_project: Agenda-setter (institutional/constrained)
 *   - urban_elites: Beneficiary (powerful/mobile)
 *   - rural_traditionalists: Payer (powerless/trapped)
 *   - cultural_minorities: Payer (powerless/identity_locked)
 *   - ideological_propagandists: Agenda-setter (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Practices").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'afe0afa2-fd2d-45b5-9386-34df737f8c97').
narrative_ontology:cs_kernel_codification('afe0afa2-fd2d-45b5-9386-34df737f8c97', formalized).
narrative_ontology:cs_authority_grounding('afe0afa2-fd2d-45b5-9386-34df737f8c97', lineage).
narrative_ontology:cs_interpretation_layer_present('afe0afa2-fd2d-45b5-9386-34df737f8c97').
narrative_ontology:cs_reading_relation('afe0afa2-fd2d-45b5-9386-34df737f8c97', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('afe0afa2-fd2d-45b5-9386-34df737f8c97', legitimacy_of_imposed_practice__endogenous_climb_reading, influences).
narrative_ontology:cs_axiom('afe0afa2-fd2d-45b5-9386-34df737f8c97', foundational, cultural_change_requires_hybrid_pull).
narrative_ontology:cs_axiom_status(cultural_change_requires_hybrid_pull, holdable).
narrative_ontology:cs_axiom_grounding('afe0afa2-fd2d-45b5-9386-34df737f8c97', cultural_change_requires_hybrid_pull, empirically_contingent).
narrative_ontology:cs_axiom('afe0afa2-fd2d-45b5-9386-34df737f8c97', secondary, state_mandate_insufficient_alone).
narrative_ontology:cs_axiom_status(state_mandate_insufficient_alone, holdable).
narrative_ontology:cs_axiom_grounding('afe0afa2-fd2d-45b5-9386-34df737f8c97', state_mandate_insufficient_alone, empirically_contingent).
narrative_ontology:cs_reference_frame('afe0afa2-fd2d-45b5-9386-34df737f8c97', state_led_cultural_modernization).
narrative_ontology:cs_drift_state('afe0afa2-fd2d-45b5-9386-34df737f8c97', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('afe0afa2-fd2d-45b5-9386-34df737f8c97', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditionalists).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional apparatus driving the imposition of new practices, aiming for national modernization and integration into global norms. It designs and enforces mandates, often through legal and educational reforms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project, agenda_setter,
    institutional, generational, constrained, national).

% Adopt the new practices, often as markers of modernity, status, and alignment with the state's vision. They benefit from access to state resources, educational opportunities, and social mobility associated with the new cultural norms. They also serve as models for broader adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, mobile, local).

% Bear the costs of cultural displacement, often facing social pressure, economic penalties, or legal sanctions for adhering to traditional practices. They lack access to the scaffolding infrastructure (e.g., modern education, media) that reinforces the new norms, making compliance difficult and costly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditionalists, payer,
    powerless, generational, trapped, local).

% Experience the imposition as an assault on their distinct cultural identity. Their traditional practices are often explicitly targeted for suppression, leading to a loss of heritage and social cohesion. Exit is identity-locked, as abandoning practices means abandoning self-concept.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_minorities, payer,
    powerless, generational, identity_locked, regional).

% Develop and disseminate messaging that frames the new practices as desirable, progressive, or essential for national identity. They create the 'quasi-endogenous pull' that reinforces top-down mandates, making the imposition seem more natural or beneficial.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_propagandists, agenda_setter,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify a diverse population under a common set of modern practices and national identity, facilitating state administration, economic development, and international standing.
% TRANSFER_FUNCTION: Transfers cultural capital, social legitimacy, and economic opportunity from traditional practices and their adherents to new, state-sanctioned practices and their adopters. It also transfers compliance costs to those resisting the change.
% ABSENT_VOICES: Scholars of traditional cultures, religious leaders of suppressed faiths, and advocates for cultural pluralism are often excluded from the policy-making process. They would argue for the intrinsic value of diverse practices and the coercive nature of the imposition.
% DISAPPEARANCE_RATIONALE: If the state's mandate and ideological scaffolding vanished, the imposed practices would likely recede in many areas, especially rural and minority communities, leading to a resurgence of traditional norms and a re-fragmentation of cultural landscape. The 'quasi-endogenous pull' would dissipate without reinforcement.
% FOUNDING_PROBLEM: The perceived problem of national disunity, 'backwardness,' and vulnerability to external influence due to diverse traditional practices and lack of a singular modern national identity.
% FOUNDING_PROBLEM_CORROBORATION: The state and its aligned intellectuals continue to assert the problem is live, citing ongoing challenges to national cohesion and development. Independent historians and anthropologists, however, often frame the 'problem' as a construct of the modernizing state, designed to justify its centralizing power, rather than an objective pre-existing condition.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the imposition forces significant costs on those whose practices are displaced, even if some benefit from adopting new ones. Suppression (0.70) is substantial, as the state actively discourages or punishes non-compliance, but it's not absolute due to the 'hybrid' nature allowing some traditional practices to persist in modified forms. Theater ratio (0.40) reflects the performative aspect of ideological messaging and elite modeling, which aims to create an illusion of voluntary adoption while underlying coercion remains. The slight dip in extractiveness and suppression towards the end of the interval reflects the partial success and normalization of some hybrid practices, reducing the need for overt coercion.
 *
 * PERSPECTIVAL GAP:
 *   The state and urban elites perceive this as a successful, albeit challenging, modernization project, where the benefits of national unity and progress outweigh the costs. Rural traditionalists and cultural minorities experience it as a coercive imposition that erodes their heritage and identity, with limited benefits. The 'hybrid scaffolding' reading acknowledges both the state's coordination goals and the extractive costs borne by those outside the scaffolding's reach.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization project and urban elites are beneficiaries, as they gain power, legitimacy, and status from the new practices. Rural traditionalists and cultural minorities are clear targets, bearing the costs of displacement and suppression. Ideological propagandists are agenda-setters, actively shaping the narrative to reinforce the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine, albeit unevenly distributed, coordination function of state-building and modernization). The 'hybrid scaffolding' approach means the mandate is not purely extractive; it genuinely coordinates a new social order, but at significant cost to specific groups. The persistence is not purely inertial (Piton) because the state actively maintains the scaffolding and ideological pull, even if the founding problem's status is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_endogeneity,
    'To what extent is the ''quasi-endogenous pull'' genuinely internal to the population, versus a product of continuous state-sponsored ideological conditioning?',
    'Longitudinal studies of practice persistence after state enforcement and ideological messaging are withdrawn, or comparative analysis with similar mandates lacking such scaffolding.',
    'If the pull is largely external, the constraint is more extractive (closer to Snare); if genuinely internalized, it moves closer to a Rope, as the coordination becomes self-sustaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_endogeneity, empirical, 'Assesses the true source of adoption: internal conviction or external influence.').

omega_variable(
    scaffolding_access_equity,
    'Is the access to the ''scaffolding infrastructure'' (e.g., modern education, media, economic opportunities) equitably distributed, or does it systematically exclude certain groups?',
    'Geospatial analysis of infrastructure distribution correlated with adoption rates and socioeconomic indicators across different population segments.',
    'If access is highly inequitable, the constraint''s extraction from excluded groups is higher, reinforcing its Tangled Rope nature and potentially pushing it towards Snare for those groups. If equitable, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scaffolding_access_equity, empirical, 'Examines whether the benefits of the scaffolding are broadly accessible.').

omega_variable(
    hybrid_practice_legitimacy,
    'Are the resulting ''hybrid practices'' (mixtures of old and new) seen as legitimate adaptations or as signs of incomplete compliance by the state and its beneficiaries?',
    'Analysis of state discourse, legal rulings, and elite social norms regarding hybrid forms. Ethnographic studies of how hybrid practices are perceived within communities.',
    'If hybrid practices are delegitimized, the constraint''s suppression is higher, as even partial compliance is rejected. If accepted, it indicates a more flexible, less extractive form of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_practice_legitimacy, conceptual, 'Determines if partial adoption is seen as success or failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 1920, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1930, 0.35).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1960, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1960, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1960, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
