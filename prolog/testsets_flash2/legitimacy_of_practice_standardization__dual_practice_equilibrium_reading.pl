% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium in Legitimacy Standardization
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes a 'dual practice equilibrium' reading of how
 *   legitimacy for practice standardization operates in many modernizing
 *   societies. It posits that state authority governs public and
 *   administrative domains (e.g., Gregorian calendar for taxes), while
 *   traditional authority governs private and ritual domains (e.g., lunar
 *   calendar for festivals). There is no expectation of convergence;
 *   compliance is strategic, not necessarily internalized. This reading
 *   emphasizes the stability and functional partitioning of authority, where
 *   both state and traditional institutions find a modus vivendi.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.2).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium in Legitimacy Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '5fc59404-70b8-46c7-848c-29c679d39332').
narrative_ontology:cs_kernel_codification('5fc59404-70b8-46c7-848c-29c679d39332', distributed).
narrative_ontology:cs_authority_grounding('5fc59404-70b8-46c7-848c-29c679d39332', practice).
narrative_ontology:cs_interpretation_layer_present('5fc59404-70b8-46c7-848c-29c679d39332').
narrative_ontology:cs_reading_relation('5fc59404-70b8-46c7-848c-29c679d39332', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fc59404-70b8-46c7-848c-29c679d39332', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5fc59404-70b8-46c7-848c-29c679d39332', foundational, domain_partitioning_is_stable).
narrative_ontology:cs_axiom_status(domain_partitioning_is_stable, holdable).
narrative_ontology:cs_axiom_grounding('5fc59404-70b8-46c7-848c-29c679d39332', domain_partitioning_is_stable, conventional).
narrative_ontology:cs_axiom('5fc59404-70b8-46c7-848c-29c679d39332', foundational, cultural_continuity_is_legitimate).
narrative_ontology:cs_axiom_status(cultural_continuity_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5fc59404-70b8-46c7-848c-29c679d39332', cultural_continuity_is_legitimate, deontological).
narrative_ontology:cs_reference_frame('5fc59404-70b8-46c7-848c-29c679d39332', functional_domain_bifurcation).
narrative_ontology:cs_drift_state('5fc59404-70b8-46c7-848c-29c679d39332', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5fc59404-70b8-46c7-848c-29c679d39332', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_and_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability of a partitioned legitimacy, where its authority is unchallenged in public administration. It enforces compliance with standardized practices in its domain but largely tolerates traditional practices in private spheres.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Maintains authority over traditional practices and rituals within their communities, benefiting from the state's non-interference in these domains. They navigate the dual system by ensuring their communities comply with state norms in public life while preserving traditional norms privately.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_community_leaders, beneficiary,
    organized, generational, constrained, local).

% Adhere to different sets of practices and norms depending on the domain (e.g., Gregorian calendar for work, lunar calendar for festivals). They bear the cognitive and practical costs of navigating this dual system but also benefit from cultural continuity and state stability. Compliance is often strategic rather than fully internalized.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_and_community_members, payer,
    moderate, biographical, constrained, local).

% Argue for a unified, rationalized system of practices across all domains, believing the dual system hinders progress and creates inefficiencies. Their calls for convergence are largely ignored by both state and traditional authorities who benefit from the equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social life by clearly partitioning domains of authority, allowing state and traditional systems to coexist without direct conflict, reducing friction from forced convergence.
% TRANSFER_FUNCTION: Transfers legitimacy and authority to the state in public/administrative domains and to traditional institutions in private/ritual domains, maintaining a stable social order.
% ABSENT_VOICES: Modernization advocates and universalist reformers are largely excluded from the policy discourse, as their proposals for unified practice challenge the established dual equilibrium that benefits both state and traditional authorities.
% DISAPPEARANCE_RATIONALE: If this dual equilibrium vanished, it would likely lead to significant social upheaval as either the state attempted to impose universal norms (leading to resistance) or traditional practices encroached on state domains (leading to administrative chaos). The current stability would collapse.
% FOUNDING_PROBLEM: The problem of integrating diverse traditional societies into modern nation-states without provoking widespread resistance or cultural loss, while establishing effective state governance.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists attest to the enduring nature of this problem in post-colonial and modernizing states. Both state officials (seeking stability) and traditional leaders (seeking cultural preservation) corroborate the ongoing relevance of managing this dualism, though their preferred solutions differ.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the system, while requiring adaptation, avoids direct conflict and allows for cultural continuity. Suppression is low (0.2) as the state generally refrains from actively suppressing traditional practices in their designated domains, and traditional authorities do not challenge state authority in its domain. Theater ratio is low (0.1) because the dual system is genuinely functional in maintaining social order and cultural identity, rather than being a mere performance. The stability of this equilibrium means metrics show little drift over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state and traditional leaders, this is a functional 'rope' that prevents conflict. From the perspective of modernization advocates, it's a 'tangled rope' or 'snare' that entrenches inefficiency and prevents progress. This story instantiates the 'rope' reading, reflecting the functional equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy benefits from its unchallenged authority in public life. Traditional community leaders benefit from the preservation of their authority in private/ritual life. Citizens bear the cost of navigating two sets of norms but also benefit from the stability and cultural continuity. Modernization advocates are excluded, as their vision of unified practice is incompatible with this equilibrium.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_vs_stagnation,
    'Is the observed dual practice equilibrium a stable, functional coordination mechanism, or a form of institutional stagnation that prevents necessary modernization?',
    'Longitudinal comparative studies of societies with dual vs. unified practice systems, measuring economic development, social cohesion, and adaptability to external shocks.',
    'If it''s stagnation, the ''rope'' classification is a misreading, and the constraint might be reclassified as a ''tangled rope'' or ''snare'' from the perspective of modernization. If it''s functional stability, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_stagnation, empirical, 'Assessing whether dual practice equilibrium is functional or dysfunctional over time.').

omega_variable(
    strategic_vs_internalized_compliance,
    'To what extent is compliance with the dual system strategic (avoiding penalties) versus internalized (believing in the legitimacy of both systems)?',
    'Sociological surveys and ethnographic studies exploring individual attitudes, beliefs, and motivations for adhering to different practice sets in different domains.',
    'If compliance is primarily strategic, the ''suppression'' metric might be understated, as the underlying tension is higher than observed. If internalized, the ''rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'Distinguishing between strategic and internalized compliance with dual practice norms.').

omega_variable(
    kernel_framing_choice,
    'Is ''legitimacy_of_practice_standardization'' best framed as a dual equilibrium, or as a process of endogenous displacement or exogenous override?',
    'Analysis of historical trajectories and policy outcomes in different national contexts, evaluating which reading best explains observed institutional change and stability.',
    'Adopting a different framing (e.g., endogenous_displacement_reading) would lead to a different constraint classification, as the core assumptions about change and legitimacy would shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'The choice of framing for the kernel ''legitimacy_of_practice_standardization'' significantly alters the interpretation of the constraint''s nature and dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(legi_tr_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(legi_tr_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(legi_be_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(legi_be_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(legi_su_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(legi_su_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
