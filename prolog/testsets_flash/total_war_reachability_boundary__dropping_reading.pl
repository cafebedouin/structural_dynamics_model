% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Probability Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'dropping probability' reading of the
 *   total war reachability boundary. It posits that while the probability of
 *   total war has decreased since the Cold War, the possibility remains
 *   structurally reachable. Deterrence is understood as a 'tangled rope' – a
 *   coordination mechanism that prevents total war but extracts a persistent
 *   cost (existential risk, military spending) from the global population and
 *   non-nuclear states, while benefiting nuclear powers and strategic
 *   analysts. The constraint requires active enforcement through the
 *   maintenance of credible nuclear arsenals and doctrines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.3).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.4).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '4c99f2d1-eb93-4ecd-b415-26b76ff82eeb').
narrative_ontology:cs_kernel_codification('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', implicit).
narrative_ontology:cs_authority_grounding('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', extraction).
narrative_ontology:cs_interpretation_layer_present('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb').
narrative_ontology:cs_reading_relation('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', foundational, total_war_remains_feasible).
narrative_ontology:cs_axiom_status(total_war_remains_feasible, holdable).
narrative_ontology:cs_axiom_grounding('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', total_war_remains_feasible, empirically_contingent).
narrative_ontology:cs_axiom('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', foundational, deterrence_is_active_coordination).
narrative_ontology:cs_axiom_status(deterrence_is_active_coordination, holdable).
narrative_ontology:cs_axiom_grounding('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', deterrence_is_active_coordination, conventional).
narrative_ontology:cs_reference_frame('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', post_nuclear_strategic_stability).
narrative_ontology:cs_drift_state('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', contemporary_geopolitical_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4c99f2d1-eb93-4ecd-b415-26b76ff82eeb', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_analysts).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_population).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).
:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) reflects the ongoing cost of maintaining deterrence and the existential risk, which has dropped from Cold War highs but remains significant. Suppression (0.4) is moderate, as alternatives to deterrence (e.g., global disarmament) are actively suppressed by the perceived necessity of nuclear arsenals. Theater ratio (0.1) is low, indicating that the core function of deterrence is still very real, not merely performative, though some aspects of strategic posturing might be theatrical. The metrics reflect a post-Cold War 'new normal' where the threat is less acute but still present.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers perceive deterrence as a necessary, if costly, 'rope' that prevents a worse outcome. Non-nuclear states and the global population experience it as a 'snare' – a system that imposes existential risk and costs without their consent or direct benefit. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are agenda-setters and beneficiaries, as they control the deterrence mechanism and derive security from it, despite the costs. Non-nuclear states and the global population are victims, bearing the risk and costs without direct control or benefit. Strategic analysts benefit from the intellectual framework and continued relevance of deterrence theory. Peace activists are excluded, as their proposals for alternative security are not integrated into the dominant strategic discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_vs_risk,
    'Is the current level of deterrence stability genuinely robust, or does the persistent reachability of total war imply a higher, unacknowledged risk?',
    'Analysis of near-miss incidents, escalation dynamics in regional conflicts, and the impact of emerging technologies on strategic stability. If these reveal a higher frequency of unmanaged escalation pathways, the risk is higher.',
    'If the risk is higher, the constraint''s effective extractiveness (from the global population) is underestimated, pushing it closer to a Snare. If stability is robust, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_risk, empirical, 'Assessing the true stability of deterrence given persistent reachability.').

omega_variable(
    deterrence_as_rope_or_snare,
    'Is deterrence primarily a coordination mechanism (Rope) or an extractive one (Snare) from the perspective of non-nuclear states and the global population?',
    'Surveying non-nuclear states'' perceptions of security vs. imposed risk, and analyzing the opportunity costs of global military spending vs. alternative investments. If perceived costs significantly outweigh perceived benefits, it leans Snare.',
    'If primarily extractive for these groups, the constraint''s classification for those seats would shift from Tangled Rope to Snare, highlighting the asymmetric burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_as_rope_or_snare, preference, 'The normative framing of deterrence for non-nuclear actors.').

omega_variable(
    natural_law_vs_construct,
    'Is the ''dropping probability'' of total war a natural consequence of nuclear weapons (a Mountain-like feature), or a constructed outcome of active deterrence efforts (a Rope/Tangled Rope)?',
    'Historical counterfactual analysis: what would have happened without active deterrence policies? If total war would have been more likely, it''s a construct. If it would have been avoided anyway, it''s closer to a natural law.',
    'If a natural law, the extractiveness and suppression metrics would be re-evaluated as inherent features, not costs of a human-made system. If a construct, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_construct, conceptual, 'The ontological status of total war''s reduced probability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__dropping_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.2).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__dropping_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.7).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.8).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_non_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_reachability_boundary' kernel, focusing on the dropping probability of total war while maintaining its reachability. It is linked to the 'contraction_reading' and 'contingent_reachability_reading' which offer alternative interpretations of the same strategic reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
