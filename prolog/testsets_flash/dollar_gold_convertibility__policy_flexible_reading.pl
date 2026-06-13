% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar Gold Convertibility (Policy Flexible Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint describes the dollar's gold convertibility under the
 *   Bretton Woods system, interpreted through a 'policy-flexible' lens. In
 *   this reading, the U.S. obligation to convert dollars to gold is
 *   conditional and subordinate to its domestic economic stability, allowing
 *   for unilateral suspension or devaluation if domestic policy goals
 *   conflict with external convertibility demands. This perspective views the
 *   U.S. as having significant monetary autonomy, with foreign central banks
 *   and international investors bearing the risk of dollar devaluation.
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Primary beneficiary (institutional/arbitrage) — retains policy flexibility
 *   - us_domestic_economy: Primary beneficiary (global/arbitrage) — shielded from external constraints
 *   - foreign_central_banks: Primary target (institutional/constrained) — bears devaluation risk
 *   - international_investors: Primary target (powerful/constrained) — bears devaluation risk
 *   - international_monetary_fund: Agenda setter (institutional/analytical) — administers the system but lacks enforcement over U.S. policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.65).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.7).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar Gold Convertibility (Policy Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'eaaf171f-530b-4757-861b-415b41f7bcdc').
narrative_ontology:cs_kernel_codification('eaaf171f-530b-4757-861b-415b41f7bcdc', formalized).
narrative_ontology:cs_authority_grounding('eaaf171f-530b-4757-861b-415b41f7bcdc', lineage).
narrative_ontology:cs_interpretation_layer_present('eaaf171f-530b-4757-861b-415b41f7bcdc').
narrative_ontology:cs_reading_relation('eaaf171f-530b-4757-861b-415b41f7bcdc', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('eaaf171f-530b-4757-861b-415b41f7bcdc', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('eaaf171f-530b-4757-861b-415b41f7bcdc', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('eaaf171f-530b-4757-861b-415b41f7bcdc', domestic_stability_priority, conventional).
narrative_ontology:cs_axiom('eaaf171f-530b-4757-861b-415b41f7bcdc', foundational, unilateral_policy_flexibility).
narrative_ontology:cs_axiom_status(unilateral_policy_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('eaaf171f-530b-4757-861b-415b41f7bcdc', unilateral_policy_flexibility, conventional).
narrative_ontology:cs_reference_frame('eaaf171f-530b-4757-861b-415b41f7bcdc', post_war_us_hegemony).
narrative_ontology:cs_drift_state('eaaf171f-530b-4757-861b-415b41f7bcdc', pre_nixon_shock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eaaf171f-530b-4757-861b-415b41f7bcdc', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_investors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the U.S. can effectively transfer the costs of its domestic policy (e.g., inflation from Vietnam War spending) to foreign dollar holders through devaluation or suspension of convertibility. Suppression is high because foreign central banks have limited alternatives to holding dollars as a reserve currency, making them captive to U.S. policy. Theater ratio is moderate, as the 'obligation' of convertibility is maintained rhetorically while being undermined by policy flexibility. The metrics reflect the period leading up to the Nixon Shock in 1971.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of U.S. monetary authorities, this arrangement is a necessary flexibility to manage domestic economic stability, a form of coordination. From the perspective of foreign central banks, it is an extractive mechanism where they bear the costs of U.S. policy without recourse. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. monetary authorities and the U.S. domestic economy are beneficiaries (d near 0.0) as they gain policy autonomy and are shielded from external constraints. Foreign central banks and international investors are victims (d near 1.0) as they bear the risk of dollar devaluation and have constrained exit options from the dollar standard. The IMF is an agenda-setter, administering the system but with limited power to enforce strict convertibility against U.S. interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the convertibility as a pure coordination mechanism (Rope) by highlighting the asymmetric extraction. It also avoids the Piton classification by showing active enforcement and clear beneficiaries, rather than mere inertial persistence. The 'policy-flexible' interpretation itself is a mechanism to avoid mandatrophy by adapting the constraint's function to evolving U.S. interests, rather than letting it atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is dollar convertibility a strict legal obligation or a policy-flexible instrument?',
    'Historical analysis of U.S. policy decisions during periods of balance of payments pressure, and legal interpretations of Article IV of the Bretton Woods Agreement by international courts or arbitration panels.',
    'If convertibility is a strict obligation (strict_convertibility_reading), the U.S. monetary authorities would be a victim, not a beneficiary, and the constraint would be a Rope or Mountain for them. If it''s an inherent design flaw (triffin_structural_reading), the entire system is a Snare for all participants, requiring systemic re-design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''policy_flexible_reading'' of the ''dollar_gold_convertibility'' kernel. Sibling readings include ''strict_convertibility_reading'' and ''triffin_structural_reading''. This reading asserts that convertibility is subordinate to domestic stability, allowing for unilateral suspension or devaluation.').

omega_variable(
    devaluation_impact_distribution,
    'How are the costs of dollar devaluation distributed among foreign central banks and international investors?',
    'Empirical studies of capital flight, reserve asset rebalancing, and inflation rates in countries holding significant dollar reserves during periods of U.S. monetary policy shifts.',
    'If costs are concentrated on a few key actors, it strengthens the Snare-like aspects for those specific victims. If diffuse, it points to a more general systemic risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devaluation_impact_distribution, empirical, 'Uncertainty regarding the precise distribution of losses from dollar devaluation among external creditors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(doll_tr_t10, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(doll_tr_t20, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(doll_be_t10, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(doll_be_t20, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(doll_su_t10, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(doll_su_t20, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, international_reserve_currency_status).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'policy_flexible_reading' emphasizes U.S. monetary autonomy, contrasting with the 'strict_convertibility_reading' (binding obligation) and the 'triffin_structural_reading' (inherent unsustainability). Each reading has distinct beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
