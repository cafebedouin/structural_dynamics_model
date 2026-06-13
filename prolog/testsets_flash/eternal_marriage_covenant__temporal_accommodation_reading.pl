% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'temporal accommodation' reading of the
 *   eternal marriage covenant, specifically regarding the practice of plural
 *   marriage. It posits that the 1890 Manifesto, which suspended the
 *   practice, was a temporary measure to comply with federal law, not a
 *   renunciation of the eternal doctrine. The principle remains valid but
 *   dormant, awaiting a future restoration when political constraints lift.
 *   This reading allows the church to maintain doctrinal consistency while
 *   achieving social and legal integration. The constraint is claimed as a
 *   Piton because its original function (avoiding legal persecution) is
 *   largely 'dead,' but the doctrine is maintained through performative
 *   adherence and a narrative of future restoration, extracting costs from
 *   those who interpret the doctrine literally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.3).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.4).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, piton).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '9cf0d90c-390b-458e-86b2-75ef77048ea7').
narrative_ontology:cs_kernel_codification('9cf0d90c-390b-458e-86b2-75ef77048ea7', fixed_text).
narrative_ontology:cs_authority_grounding('9cf0d90c-390b-458e-86b2-75ef77048ea7', lineage).
narrative_ontology:cs_interpretation_layer_present('9cf0d90c-390b-458e-86b2-75ef77048ea7').
narrative_ontology:cs_reading_relation('9cf0d90c-390b-458e-86b2-75ef77048ea7', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cf0d90c-390b-458e-86b2-75ef77048ea7', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('9cf0d90c-390b-458e-86b2-75ef77048ea7', foundational, divine_law_temporarily_suspended).
narrative_ontology:cs_axiom_status(divine_law_temporarily_suspended, holdable).
narrative_ontology:cs_axiom_grounding('9cf0d90c-390b-458e-86b2-75ef77048ea7', divine_law_temporarily_suspended, theological).
narrative_ontology:cs_axiom('9cf0d90c-390b-458e-86b2-75ef77048ea7', secondary, obedience_to_law_of_land_takes_precedence).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_takes_precedence, holdable).
narrative_ontology:cs_axiom_grounding('9cf0d90c-390b-458e-86b2-75ef77048ea7', obedience_to_law_of_land_takes_precedence, conventional).
narrative_ontology:cs_reference_frame('9cf0d90c-390b-458e-86b2-75ef77048ea7', eternal_doctrine_unfolding_in_time).
narrative_ontology:cs_drift_state('9cf0d90c-390b-458e-86b2-75ef77048ea7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9cf0d90c-390b-458e-86b2-75ef77048ea7', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, historical_polygamist_families).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness has decreased over time as the direct legal pressure subsided, but it remains significant for splinter groups. Suppression has also decreased as the church's internal enforcement against plural marriage became less overt, but it still exists for those who defy the accommodation. The theater ratio has increased significantly: the 'suspension' is now largely performative, maintaining a doctrinal claim without active practice, while the actual function of avoiding legal persecution has atrophied. The high accessibility_collapse (0.7) reflects the difficulty of maintaining the practice outside the mainstream church, and the moderate resistance (0.2) comes primarily from marginalized groups.
 *
 * PERSPECTIVAL GAP:
 *   Church leadership and mainstream members experience this as a successful, divinely guided adaptation, preserving the church's mission. Fundamentalist splinter groups and historical polygamist families experience it as a betrayal and an ongoing source of extraction and suppression, forcing them to abandon core tenets or face severe consequences. The federal government, as an external enforcer, sees it as a successful application of legal supremacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and mainstream members are beneficiaries, gaining social acceptance and legal stability. Fundamentalist splinter groups and historical polygamist families are victims, bearing the costs of doctrinal non-compliance and social marginalization. The federal government is an agenda-setter whose pressure initiated the accommodation. Secular society is an observer, noting the church's adaptation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy. The original mandate (avoiding federal persecution for plural marriage) is largely 'dead' as a live threat. However, the constraint persists, not because it solves a current coordination problem for all, but due to institutional inertia, the desire to maintain doctrinal consistency, and the benefits of social acceptance for the mainstream church. The 'eternal principle remains valid' narrative is a theatrical element that allows the constraint to persist as a Piton, extracting costs from those who are identity-locked to the original doctrine, while the primary beneficiaries (church leadership, mainstream members) no longer face the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the ''eternal marriage covenant'' a genuine natural law or an institutionally constructed constraint that benefits identifiable agents?',
    'Analysis of theological texts and historical practice, cross-referenced with sociological studies of religious authority and institutional maintenance.',
    'If a genuine natural law, its classification as Piton is a misreading of its persistence. If constructed, the Piton classification is accurate, highlighting its performative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural law and institutional construct.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, legal penalties) or internalized (social pressure, identity fusion)?',
    'Post-exit suppression trajectory for former members of splinter groups: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    temporal_accommodation_vs_renunciation,
    'Is the Manifesto a temporary suspension of practice (accommodation) or a permanent renunciation of the doctrine of plural marriage?',
    'Future prophetic pronouncements or a shift in official church doctrine. If the practice is never restored despite political conditions allowing it, the ''accommodation'' reading weakens.',
    'If a permanent renunciation, the ''immutable commandment'' reading is foreclosed, and the ''prophetic override'' reading gains strength. This would fundamentally alter the doctrinal basis of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_accommodation_vs_renunciation, conceptual, 'Ambiguity over the permanence of the Manifesto''s suspension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(eter_tr_t1980, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(eter_tr_t2024, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(eter_be_t1980, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(eter_be_t2024, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(eter_su_t1980, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(eter_su_t2024, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel. It focuses on the 'temporal accommodation' interpretation, where practice is suspended but doctrine remains valid. It affects and is affected by sibling readings that emphasize immutability or prophetic override.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
