% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via Welfare Coordination
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the EU's approach to free movement as operating
 *   through the coordination of national welfare systems, rather than full
 *   supranational harmonization. It focuses on the enforcement of
 *   anti-social-dumping rules while preserving member state welfare design
 *   autonomy. This is one reading of the 'federation_membership_kernel',
 *   emphasizing the practical coordination challenges and the resulting
 *   extraction from specific worker and labor market segments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via Welfare Coordination").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'a2a758f1-b956-4eb9-9298-d25d62489298').
narrative_ontology:cs_kernel_codification('a2a758f1-b956-4eb9-9298-d25d62489298', formalized).
narrative_ontology:cs_authority_grounding('a2a758f1-b956-4eb9-9298-d25d62489298', lineage).
narrative_ontology:cs_interpretation_layer_present('a2a758f1-b956-4eb9-9298-d25d62489298').
narrative_ontology:cs_reading_relation('a2a758f1-b956-4eb9-9298-d25d62489298', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2a758f1-b956-4eb9-9298-d25d62489298', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a2a758f1-b956-4eb9-9298-d25d62489298', foundational, national_welfare_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a2a758f1-b956-4eb9-9298-d25d62489298', national_welfare_autonomy_is_paramount, conventional).
narrative_ontology:cs_axiom('a2a758f1-b956-4eb9-9298-d25d62489298', foundational, anti_social_dumping_rules_are_necessary).
narrative_ontology:cs_axiom_status(anti_social_dumping_rules_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a2a758f1-b956-4eb9-9298-d25d62489298', anti_social_dumping_rules_are_necessary, instrumental).
narrative_ontology:cs_reference_frame('a2a758f1-b956-4eb9-9298-d25d62489298', coordinated_national_welfare_systems).
narrative_ontology:cs_drift_state('a2a758f1-b956-4eb9-9298-d25d62489298', contemporary_eu_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a2a758f1-b956-4eb9-9298-d25d62489298', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_remittances).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_brain_drain).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the structural delta: posted workers face wage undercutting, and receiving state labor markets experience competitive pressure. Suppression (0.70) is high due to the active enforcement of anti-social-dumping rules and the legal complexities that limit worker mobility and access to host state benefits. The theater ratio (0.20) is relatively low, indicating that the coordination function is still largely genuine, but there's a growing performative aspect in balancing national sovereignty claims with single market principles.
 *
 * PERSPECTIVAL GAP:
 *   EU institutions and sending member states (via remittances) perceive this as a necessary coordination mechanism for the single market. However, posted workers and receiving member state labor markets experience it as an extractive mechanism that creates unfair competition and social strain. The classification will likely diverge significantly between these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions are agenda-setters and beneficiaries, as they maintain the single market and political stability. Sending member states are mixed: beneficiaries through remittances, but victims through brain drain. Posted workers and receiving state labor markets are clear victims, bearing the costs of wage undercutting and labor market pressure. National welfare administrators are agenda-setters, balancing national interests with EU obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (coordinating welfare systems for free movement) is still live, but its operation has accumulated extractive elements. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring coordination). The rising extractiveness over time suggests a drift towards greater rent-seeking within the coordination framework, indicating a need for re-evaluation of the balance between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_coordination_vs_harmonization,
    'Is the current level of welfare coordination sufficient to prevent social dumping and ensure fair competition, or is greater supranational harmonization required?',
    'Empirical studies comparing labor market outcomes and social protection levels in different EU member states under the current coordination regime versus hypothetical harmonization scenarios.',
    'If coordination is deemed insufficient, the constraint''s extractiveness and suppression might be re-evaluated as higher, pushing it closer to a Snare. If sufficient, its Rope-like qualities would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_coordination_vs_harmonization, empirical, 'Ambiguity regarding the optimal level of welfare integration for free movement.').

omega_variable(
    natural_law_vs_constructed_coordination,
    'Is the preservation of national welfare design autonomy a ''natural'' and unchangeable feature of the EU, or a constructed political choice that could be altered?',
    'Analysis of legal precedents, treaty revisions, and political will within the EU. If member states consistently resist harmonization, it suggests a strong, almost ''mountain-like'' political constraint. If political will shifts, it''s a constructed choice.',
    'If it''s a constructed choice, the constraint''s extractiveness is more clearly attributable to policy decisions rather than inherent limits, potentially increasing the perceived agency of EU institutions to alter it. If ''natural'', the extraction might be seen as an unavoidable cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_coordination, conceptual, 'Whether national welfare autonomy is an irreducible limit or a policy choice.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''welfare_coordination_reading'' of the ''federation_membership_kernel'', or does it conflate elements of the ''integration_reading'' or ''member_sovereignty_reading''?',
    'Expert review by EU law and political economy scholars, comparing the structural elements of this constraint against the defined characteristics of the sibling readings. Focus on the specific mechanisms of enforcement and the declared beneficiaries/victims.',
    'Misidentification could lead to an inaccurate classification and misattribution of extraction. If it leans more towards ''integration'', the extractiveness might be seen as a cost of deeper integration; if towards ''member_sovereignty'', as a cost of national protectionism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the precise instantiation of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fede_be_t2010, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(fede_su_t2010, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel', focusing on the coordination of national welfare systems. It is linked to the 'integration_reading' and 'member_sovereignty_reading' as part of a constraint family that captures the contested nature of EU free movement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
