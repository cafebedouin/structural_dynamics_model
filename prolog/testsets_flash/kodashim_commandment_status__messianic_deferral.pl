% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'messianic_deferral' reading of the
 *   Kodashim (sacrifices) commandment status within Halakhic Judaism. It
 *   asserts that while the Temple and its sacrificial rites are currently
 *   suspended, the commandments themselves are not obsolete. Instead, their
 *   study and the maintenance of readiness for their future restoration (in a
 *   rebuilt Temple) constitute a present obligation. This reading justifies
 *   the allocation of significant intellectual and communal resources towards
 *   a future-contingent practice, creating an opportunity cost for
 *   present-generation needs.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Agenda setter (institutional/arbitrage) — interpret and enforce the deferral, benefiting from the intellectual and spiritual capital it generates.
 *   - messianic_aspirants: Beneficiary (organized/identity_locked) — find purpose and meaning in the deferral, their identity fused with the expectation of future restoration.
 *   - present_generation_needs: Payer (powerless/constrained) — bear the opportunity cost of resources diverted to future readiness, such as communal funds for study instead of social welfare.
 *   - community_resources: Payer (powerless/constrained) — financial and human capital allocated to maintaining readiness for a future event, rather than addressing immediate communal challenges.
 *   - halakhic_scholars: Beneficiary (organized/identity_locked) — their careers and intellectual pursuits are centered on the study of these laws, reinforcing the deferral.
 *   - secular_community_members: Excluded (moderate/mobile) — may question the allocation of resources but lack formal authority within the religious framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.6).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '99dd79c3-eb07-4cfc-8ee4-725088d89cce').
narrative_ontology:cs_kernel_codification('99dd79c3-eb07-4cfc-8ee4-725088d89cce', fixed_text).
narrative_ontology:cs_authority_grounding('99dd79c3-eb07-4cfc-8ee4-725088d89cce', lineage).
narrative_ontology:cs_interpretation_layer_present('99dd79c3-eb07-4cfc-8ee4-725088d89cce').
narrative_ontology:cs_reading_relation('99dd79c3-eb07-4cfc-8ee4-725088d89cce', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('99dd79c3-eb07-4cfc-8ee4-725088d89cce', kodashim_commandment_status__study_as_performance, influences).
narrative_ontology:cs_axiom('99dd79c3-eb07-4cfc-8ee4-725088d89cce', foundational, commandment_eternal_though_suspended).
narrative_ontology:cs_axiom_status(commandment_eternal_though_suspended, holdable).
narrative_ontology:cs_axiom_grounding('99dd79c3-eb07-4cfc-8ee4-725088d89cce', commandment_eternal_though_suspended, theological).
narrative_ontology:cs_axiom('99dd79c3-eb07-4cfc-8ee4-725088d89cce', secondary, messianic_era_restores_temple).
narrative_ontology:cs_axiom_status(messianic_era_restores_temple, holdable).
narrative_ontology:cs_axiom_grounding('99dd79c3-eb07-4cfc-8ee4-725088d89cce', messianic_era_restores_temple, theological).
narrative_ontology:cs_reference_frame('99dd79c3-eb07-4cfc-8ee4-725088d89cce', halakhic_continuity_through_deferral).
narrative_ontology:cs_drift_state('99dd79c3-eb07-4cfc-8ee4-725088d89cce', contemporary_secular_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('99dd79c3-eb07-4cfc-8ee4-725088d89cce', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_aspirants).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, community_resources).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the opportunity cost of diverting resources and attention from present needs to future readiness. Suppression (0.6) is significant, as questioning the messianic deferral can lead to social and religious ostracization within the community. Theater ratio (0.1) is low, as the study and readiness are genuinely believed to be functional for the future, not merely performative. Accessibility collapse (0.7) is high because within this framework, alternatives to maintaining readiness are largely foreclosed by theological commitment. Resistance (0.15) is low due to the strong social and theological pressures against questioning the deferral.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authorities and messianic aspirants experience this as a vital, coordinative constraint, ensuring the continuity of tradition and the fulfillment of divine will. For present-generation needs and community resources, it operates as an extractive mechanism, diverting tangible benefits to an uncertain future. Halakhic scholars benefit from the intellectual capital and status derived from this study, while secular community members may see it as an inefficient allocation of resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and messianic aspirants are beneficiaries (d near 0.0) as they gain spiritual authority, communal cohesion, and purpose from maintaining the deferral. Present-generation needs and community resources are victims (d near 1.0) as they bear the direct opportunity costs. Halakhic scholars are also beneficiaries, as their professional identity and livelihood are tied to the study of these laws. The constraint is actively enforced through social norms, religious education, and the authority of rabbinic leadership.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids being a Piton because the mandate (readiness for messianic restoration) is still considered live and actively pursued by beneficiaries, even if its direct fulfillment is deferred. It is not a pure Snare because there is a genuine coordination function (maintaining halakhic continuity and communal identity through shared expectation). It is a Tangled Rope because it coordinates the community around a shared future vision while simultaneously extracting resources and deferring present needs through the same structure, requiring active enforcement to maintain this balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine messianic deferral, or is it a reinterpretation that serves present institutional interests?',
    'Analysis of historical shifts in halakhic interpretation and resource allocation patterns over time, particularly during periods of messianic fervor versus institutional consolidation.',
    'If primarily serving institutional interests, the constraint''s extractiveness is higher, and its classification shifts closer to a Snare, as the ''deferral'' becomes a cover for ongoing resource capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''messianic_deferral'' reading of the ''kodashim_commandment_status'' kernel. Sibling readings (''performance_only'', ''study_as_performance'') would alter the victim set and the justification for present-day resource allocation.').

omega_variable(
    opportunity_cost_quantification,
    'What is the quantifiable opportunity cost of deferring present-generation needs and community resources to maintain readiness for future restoration?',
    'Economic and social impact studies comparing resource allocation under this reading versus alternative readings that prioritize present-day community welfare or reallocate resources from readiness maintenance.',
    'A high quantifiable opportunity cost would increase the effective extractiveness, particularly for ''present_generation_needs'' and ''community_resources'', potentially shifting the classification towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantifying the real-world impact of deferring present needs for future messianic readiness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t10, kodashim_commandment_status__messianic_deferral, theater_ratio, 10, 0.1).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.1).
narrative_ontology:measurement(koda_tr_t30, kodashim_commandment_status__messianic_deferral, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t10, kodashim_commandment_status__messianic_deferral, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(koda_be_t30, kodashim_commandment_status__messianic_deferral, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(koda_su_t10, kodashim_commandment_status__messianic_deferral, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(koda_su_t30, kodashim_commandment_status__messianic_deferral, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. This reading emphasizes future readiness and deferral, while 'performance_only' emphasizes present suspension, and 'study_as_performance' emphasizes intellectual engagement as fulfillment. Each reading has distinct beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
