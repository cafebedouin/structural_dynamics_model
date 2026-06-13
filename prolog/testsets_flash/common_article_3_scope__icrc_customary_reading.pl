% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope (ICRC Customary Law Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the International Committee of the Red Cross's
 *   (ICRC) reading of the scope of Common Article 3 (CA3) of the Geneva
 *   Conventions, which holds that CA3's application is determined by evolving
 *   state practice and opinio juris, as tracked through customary
 *   international law. This reading allows for a dynamic interpretation of
 *   CA3, enabling its application to new forms of armed violence without
 *   requiring formal treaty amendments. It acts as a procedural constraint on
 *   interpretation, guiding how the scope of IHL is understood and expanded.
 *
 * KEY AGENTS:
 *   - international_committee_of_the_red_cross: Agenda setter (institutional/analytical) — actively researches, publishes, and advocates for this reading.
 *   - states_seeking_legitimacy: Beneficiary (institutional/constrained) — benefit from a flexible, widely accepted framework for IHL application, especially when their actions might otherwise be ambiguous.
 *   - states_resisting_expansion: Payer (institutional/constrained) — bear the cost of potentially broader IHL obligations than they formally consented to, leading to resistance.
 *   - international_courts_and_tribunals: Observer (institutional/analytical) — interpret and apply IHL, often referencing customary law, thus influencing the practical scope of CA3.
 *   - non_state_armed_groups: Payer (powerless/trapped) — are increasingly subject to CA3 obligations under this reading, despite not being parties to the Geneva Conventions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.3).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.2).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope (ICRC Customary Law Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'd64ffd7e-de57-4838-b212-1534015f59f4').
narrative_ontology:cs_kernel_codification('d64ffd7e-de57-4838-b212-1534015f59f4', formalized).
narrative_ontology:cs_authority_grounding('d64ffd7e-de57-4838-b212-1534015f59f4', expertise).
narrative_ontology:cs_interpretation_layer_present('d64ffd7e-de57-4838-b212-1534015f59f4').
narrative_ontology:cs_reading_relation('d64ffd7e-de57-4838-b212-1534015f59f4', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64ffd7e-de57-4838-b212-1534015f59f4', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('d64ffd7e-de57-4838-b212-1534015f59f4', foundational, ihl_evolves_through_custom).
narrative_ontology:cs_axiom_status(ihl_evolves_through_custom, holdable).
narrative_ontology:cs_axiom_grounding('d64ffd7e-de57-4838-b212-1534015f59f4', ihl_evolves_through_custom, conventional).
narrative_ontology:cs_axiom('d64ffd7e-de57-4838-b212-1534015f59f4', secondary, icrc_as_custodian_of_custom).
narrative_ontology:cs_axiom_status(icrc_as_custodian_of_custom, holdable).
narrative_ontology:cs_axiom_grounding('d64ffd7e-de57-4838-b212-1534015f59f4', icrc_as_custodian_of_custom, conventional).
narrative_ontology:cs_reference_frame('d64ffd7e-de57-4838-b212-1534015f59f4', dynamic_ihl_interpretation).
narrative_ontology:cs_drift_state('d64ffd7e-de57-4838-b212-1534015f59f4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d64ffd7e-de57-4838-b212-1534015f59f4', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_committee_of_the_red_cross).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_seeking_legitimacy).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, evolving_standards_of_humanity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is relatively low because this reading primarily provides a framework for interpretation rather than direct, heavy-handed extraction. However, it does impose obligations on states and non-state actors that they might not have explicitly consented to, hence the non-zero value. Suppression (0.2) is also low, as compliance is largely driven by legitimacy and reputation rather than direct coercion, though states resisting expansion face diplomatic pressure. Theater ratio (0.1) is minimal, as the ICRC's work in this area is genuinely functional. Accessibility collapse (0.7) is high because once a norm is recognized as customary, it becomes binding on all states, leaving little room for opting out. Resistance (0.15) is present from states that prefer a more restrictive interpretation, but it's generally contained within diplomatic and legal discourse.
 *
 * PERSPECTIVAL GAP:
 *   The ICRC and states seeking legitimacy view this reading as a vital, flexible mechanism for humanitarian protection. States resisting expansion, however, perceive it as an overreach that expands their obligations without explicit consent. Non-state armed groups, often the targets of CA3, experience it as an externally imposed legal framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC is a primary beneficiary and agenda-setter, as this reading enhances its role and influence in IHL. States seeking legitimacy also benefit from a clear, evolving framework. States resisting expansion and non-state armed groups are payers, as they bear the costs of expanded obligations. International courts and tribunals are observers, interpreting the law without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by providing a mechanism for CA3's scope to evolve with changing conflict dynamics, ensuring the constraint remains relevant. Without this flexibility, CA3's mandate could atrophy as new forms of armed violence emerge that do not fit traditional definitions, leading to a gap in protection. The dynamic nature of customary law prevents the constraint from becoming a 'piton' of outdated treaty language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_treaty_law,
    'Is the ICRC''s customary law reading of CA3''s scope a genuine reflection of state practice and opinio juris, or an attempt to expand treaty obligations through interpretation?',
    'Systematic review of state declarations, military manuals, and judicial decisions over time, focusing on explicit acceptance or rejection of the ICRC''s methodology.',
    'If genuinely customary, it provides a flexible mechanism for IHL adaptation. If an overreach, it risks undermining state consent to IHL and could lead to pushback against the ICRC''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_law, empirical, 'Ambiguity in the source and authority of customary international law.').

omega_variable(
    icrc_reading_vs_state_centric_reading,
    'Does the ICRC''s customary reading of CA3''s scope genuinely coexist with the ''state-centric'' reading, or does it implicitly foreclose it by expanding the definition of armed conflict?',
    'Analysis of state responses to ICRC reports and specific instances where states explicitly reject the customary reading''s application to situations they deem internal law enforcement.',
    'If it forecloses the state-centric reading, it implies a broader application of IHL than many states accept, potentially increasing resistance to IHL compliance. If it coexists, it offers a complementary, more expansive interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_reading_vs_state_centric_reading, conceptual, 'Relationship between ICRC customary reading and state-centric reading of CA3 scope.').

omega_variable(
    icrc_reading_vs_expansive_human_rights_reading,
    'Does the ICRC''s customary reading of CA3''s scope sufficiently incorporate human rights principles, or is it too conservative compared to the ''expansive human rights'' reading?',
    'Comparative legal analysis of the protections offered by each reading in specific conflict scenarios, particularly those involving non-state actors and internal disturbances.',
    'If too conservative, it may leave gaps in protection for individuals in situations of violence. If it sufficiently incorporates human rights, it provides a pragmatic, widely accepted framework for protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_reading_vs_expansive_human_rights_reading, preference, 'Adequacy of human rights integration in ICRC customary reading of CA3 scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__icrc_customary_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(comm_tr_t20, common_article_3_scope__icrc_customary_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__icrc_customary_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(comm_be_t20, common_article_3_scope__icrc_customary_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__icrc_customary_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(comm_su_t20, common_article_3_scope__icrc_customary_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, ihl_applicability_to_non_state_actors).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Common Article 3 scope kernel, focusing on customary international law. It is linked to other readings that emphasize state consent or human rights principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
