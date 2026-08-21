% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Reading of Divine Legitimacy Substrate
 *   domain: religious_studies/ancient_history/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'folk syncretistic' reading of divine
 *   legitimacy in ancient societies, where legitimacy flows from
 *   decentralized household and village ritual practices, pragmatically
 *   incorporating multiple deities. This reading emphasizes the bottom-up,
 *   resilient nature of local belief systems, often operating independently
 *   of, or in parallel to, official state religions. It is claimed as a
 *   Mountain due to its deep embedding in cultural identity and its
 *   resistance to top-down change, with low extractiveness and suppression
 *   reflecting its self-organizing nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.2).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, mountain).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Reading of Divine Legitimacy Substrate").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious_studies/ancient_history/political_economy").

domain_priors:emerges_naturally(divine_legitimacy_substrate__folk_syncretistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '5408c2a1-357a-4655-a11d-08b25ecff75e').
narrative_ontology:cs_kernel_codification('5408c2a1-357a-4655-a11d-08b25ecff75e', implicit).
narrative_ontology:cs_authority_grounding('5408c2a1-357a-4655-a11d-08b25ecff75e', practice).
narrative_ontology:cs_interpretation_layer_present('5408c2a1-357a-4655-a11d-08b25ecff75e').
narrative_ontology:cs_reading_relation('5408c2a1-357a-4655-a11d-08b25ecff75e', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('5408c2a1-357a-4655-a11d-08b25ecff75e', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('5408c2a1-357a-4655-a11d-08b25ecff75e', foundational, divine_presence_is_diffuse_and_local).
narrative_ontology:cs_axiom_status(divine_presence_is_diffuse_and_local, holdable).
narrative_ontology:cs_axiom_grounding('5408c2a1-357a-4655-a11d-08b25ecff75e', divine_presence_is_diffuse_and_local, conventional).
narrative_ontology:cs_axiom('5408c2a1-357a-4655-a11d-08b25ecff75e', foundational, ritual_efficacy_is_pragmatic_and_adaptive).
narrative_ontology:cs_axiom_status(ritual_efficacy_is_pragmatic_and_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('5408c2a1-357a-4655-a11d-08b25ecff75e', ritual_efficacy_is_pragmatic_and_adaptive, empirically_contingent).
narrative_ontology:cs_reference_frame('5408c2a1-357a-4655-a11d-08b25ecff75e', decentralized_communal_piety).
narrative_ontology:cs_drift_state('5408c2a1-357a-4655-a11d-08b25ecff75e', imperial_centralization_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('5408c2a1-357a-4655-a11d-08b25ecff75e', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_communities).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive a sense of cosmic order, social cohesion, and practical guidance from the rituals. The legitimacy of their social structures and daily practices is affirmed by these traditions. Exit means abandoning their cultural identity and social fabric.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_communities, beneficiary,
    organized, generational, identity_locked, local).

% Are the primary practitioners and interpreters of household rituals, ensuring the flow of divine favor and legitimacy for their families. Their authority within the household is tied to their role in maintaining these practices. Abandoning these practices would undermine their social standing and self-concept.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, agenda_setter,
    moderate, biographical, identity_locked, local).

% Benefits from the general stability and cosmic order affirmed by folk practices, even if not directly controlling them. The pharaoh's divine mandate is implicitly supported by a populace that believes in divine order, regardless of its specific manifestation. Direct intervention in folk practice is rare and risky.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, beneficiary,
    institutional, civilizational, constrained, national).

% Benefits from the broader belief in divine power and ritual efficacy, which underpins their own institutional authority. While their formal rituals are distinct, the folk practices reinforce the general religious substrate. Direct control over folk practice is limited.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, priesthood, beneficiary,
    institutional, generational, constrained, national).

% Study the historical development and social function of these belief systems, analyzing their impact on political and social structures without participating in the rituals themselves.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized framework for local communities and households to maintain social cohesion, moral order, and a sense of cosmic belonging through shared ritual practices, adapting to local needs and integrating diverse divine influences.
% TRANSFER_FUNCTION: Transfers a sense of legitimacy, social stability, and spiritual comfort to local communities and households, reinforcing their internal structures and practices, with minimal direct material transfer to central authorities.
% ABSENT_VOICES: Centralized religious authorities (like the Amun priesthood) or reformist movements (like Atenism) would object to the diffuse, syncretistic nature of folk practice, arguing for doctrinal purity or centralized control, but they are largely ignored or accommodated by the resilient local traditions.
% DISAPPEARANCE_RATIONALE: If this substrate of folk belief and ritual vanished, local communities would lose a fundamental source of social cohesion, moral guidance, and identity. The legitimacy of household structures and local leadership would be undermined, leading to widespread social and spiritual disarray, forcing a complete reorganization of local life.
% FOUNDING_PROBLEM: How to maintain social order, moral norms, and a sense of cosmic meaning in diverse local communities without centralized enforcement, allowing for adaptation to local conditions and integration of various spiritual influences.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of traditional societies and historical records of local resilience against imperial religious impositions corroborate that these decentralized systems effectively address ongoing needs for social cohesion and meaning, independent of elite religious narratives.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, ExtMetricName, E),
    domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(divine_legitimacy_substrate__folk_syncretistic_reading),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary 'benefit' is social cohesion and cosmic order, not material wealth extracted by a central authority. Suppression is low (0.2) as adherence is largely voluntary and identity-driven, rather than coercively enforced by external powers. Theater ratio is low (0.1) because the rituals are genuinely functional for local communities, not primarily performative for an external audience. The metrics show stability over time, reflecting the enduring nature of folk traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local communities, this system is a natural, self-sustaining order (Mountain). From the perspective of a centralizing pharaoh or priesthood, it might be seen as a 'distributed' or 'implicit' authority that needs to be managed or absorbed, but its deep cultural roots make direct confrontation difficult. The engine's classification will reflect the Mountain nature from the folk perspective, while other readings (e.g., Amun polytheistic) would likely compute as more extractive due to centralized priestly control.
 *
 * DIRECTIONALITY LOGIC:
 *   Local communities and household heads are the primary beneficiaries and agenda-setters, as they directly practice and derive meaning from these rituals. The pharaoh and priesthood are indirect beneficiaries; their legitimacy is broadly supported by a populace that believes in divine order, but they do not directly control or extract from these specific folk practices. Their directionality is thus closer to symmetric or slightly beneficiary, as the constraint subsidizes their broader authority without direct cost to them.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is resistant to mandatrophy because its 'mandate' is continuously renewed by the lived experience and identity of local communities. It solves an ongoing problem of social cohesion and meaning-making at the local level, which does not 'die' even if state-level religious structures change. Its persistence is not due to inertia but to its active, adaptive function for its practitioners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_legitimacy,
    'Is the diffuse divine legitimacy flowing through folk practice a ''natural'' emergent property of social life, or a ''constructed'' system that benefits local power structures (e.g., household heads)?',
    'Comparative anthropological studies of societies with varying degrees of centralized religious authority, examining the emergence and persistence of folk practices in different political-economic contexts.',
    'If more ''constructed'', the extractiveness and suppression metrics might be slightly higher, reflecting the subtle power dynamics within local communities, potentially shifting the classification towards a Rope or even a Tangled Rope at the local level. If truly ''natural'', the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_legitimacy, conceptual, 'Ambiguity between emergent social order and locally constructed power dynamics.').

omega_variable(
    resistance_to_centralization,
    'To what extent is the ''resistance'' to top-down religious revision an active, conscious choice by local communities, versus a passive, inertial persistence of tradition?',
    'Historical analysis of periods of attempted religious reform (e.g., Atenism), examining the specific mechanisms of local resistance (e.g., covert practice, reinterpretation, open defiance) versus mere non-compliance due to distance or inertia.',
    'If active resistance is higher than currently measured, the suppression metric from the perspective of central authorities would be higher, and the folk reading''s classification might shift from Mountain to a more ''active'' form of coordination (Rope) that actively defends its autonomy. If purely inertial, the Mountain classification is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_to_centralization, empirical, 'Distinguishing active resistance from passive inertia in folk religious persistence.').

omega_variable(
    pharaonic_legitimacy_dependence,
    'How dependent is the pharaoh''s broader divine legitimacy on the underlying, diffuse folk belief in divine order, even if not directly on specific folk practices?',
    'Historical analysis of periods of widespread social unrest or religious skepticism at the folk level, and their correlation with challenges to pharaonic authority, even in the absence of direct religious conflict.',
    'If the dependence is high, the pharaoh''s ''beneficiary'' role is more pronounced, and the folk reading''s contribution to the broader political system is more significant, potentially increasing its ''influence'' on the Amun polytheistic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_legitimacy_dependence, empirical, 'The degree to which pharaonic authority relies on diffuse folk belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(divi_tr_t75, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(divi_be_t75, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 25, 0.19).
narrative_ontology:measurement(divi_su_t50, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(divi_su_t75, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 75, 0.21).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_legitimacy_substrate' kernel. This 'folk_syncretistic_reading' emphasizes decentralized, pragmatic ritual practice, contrasting with the centralized 'amun_polytheistic_reading' and the monotheistic 'atenist_monotheistic_reading'. Each reading represents a distinct structural claim about the source and flow of divine legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
