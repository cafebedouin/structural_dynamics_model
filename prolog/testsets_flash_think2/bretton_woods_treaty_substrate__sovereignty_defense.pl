% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story analyzes the Bretton Woods system from the
 *   'sovereignty defense' reading, focusing on how the system, while
 *   ostensibly designed for global monetary stability, imposed external
 *   monetary discipline on non-reserve currency states while granting
 *   'exorbitant privilege' to the United States. The gold-dollar anchor,
 *   intended as a stabilizer, is interpreted here as a mechanism for
 *   asymmetric extraction. This reading highlights the power dynamics and the
 *   costs borne by nations seeking to maintain domestic policy autonomy under
 *   a dollar-centric fixed exchange rate regime.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.8).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '4f1ed9b4-c12a-4b13-b617-6e988f4d1627').
narrative_ontology:cs_kernel_codification('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', formalized).
narrative_ontology:cs_authority_grounding('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', extraction).
narrative_ontology:cs_interpretation_layer_present('4f1ed9b4-c12a-4b13-b617-6e988f4d1627').
narrative_ontology:cs_reading_relation('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', foundational, national_monetary_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(national_monetary_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', national_monetary_autonomy_is_paramount, conventional).
narrative_ontology:cs_axiom('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', foundational, reserve_currency_privilege_is_structural).
narrative_ontology:cs_axiom_status(reserve_currency_privilege_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', reserve_currency_privilege_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', post_war_national_reconstruction).
narrative_ontology:cs_drift_state('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', contemporary_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4f1ed9b4-c12a-4b13-b617-6e988f4d1627', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the primary reserve currency, the U.S. benefits from 'exorbitant privilege,' allowing it to run balance of payments deficits without immediate pressure to devalue, effectively exporting inflation and maintaining policy flexibility. It actively shaped and enforced the rules of the Bretton Woods system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These nations are subject to external monetary discipline, forced to adjust their domestic policies to maintain fixed exchange rates against the dollar, often at the cost of economic growth or social welfare. Their options are limited to devaluation (politically costly) or austerity.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    organized, biographical, constrained, global).

% Administers the rules of the Bretton Woods system, providing loans to countries facing balance of payments difficulties, often with strict conditionalities that reinforce external monetary discipline. Its structure gives disproportionate power to reserve currency states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Often lack the economic diversification and political leverage to resist the external discipline imposed by the system. They bear the brunt of adjustment costs and find their development paths constrained by the need to maintain currency stability against the dollar.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations, payer,
    powerless, generational, trapped, global).

% While constrained by capital controls, these markets still exert pressure on national currencies and influence the policy choices of non-reserve states. Their eventual growth and mobility contributed to the system's breakdown.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, global_capital_markets, observer,
    powerful, immediate, arbitrage, global).

% Analyze the structural asymmetries and power dynamics embedded in the Bretton Woods system, often highlighting the benefits accrued by the reserve currency issuer and the costs borne by others. Their analysis informs policy debates and historical interpretations.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, academic_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable international monetary system that allows national governments to pursue domestic economic policies without being subject to speculative attacks or external monetary discipline, while maintaining fixed exchange rates.
% TRANSFER_FUNCTION: Transfers the burden of adjustment for global imbalances to non-reserve currency states, and transfers seigniorage benefits and policy flexibility to the reserve currency issuer (U.S.).
% ABSENT_VOICES: Developing nations advocating for more flexible exchange rate regimes or greater access to international liquidity without conditionalities; alternative proposals for a truly multilateral reserve asset that would not confer 'exorbitant privilege' to any single nation.
% DISAPPEARANCE_RATIONALE: The collapse of Bretton Woods in the early 1970s led to a shift to floating exchange rates, increased capital mobility, and new forms of international financial governance, fundamentally altering the global monetary order and the distribution of monetary power.
% FOUNDING_PROBLEM: The interwar period's competitive devaluations, trade wars, and financial instability, which undermined global trade and contributed to economic depression.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars corroborate the interwar instability. Critics from developing nations and some economists attest that the founding problem was replaced by a new form of asymmetric power, while proponents argue the system successfully prevented a return to interwar chaos for decades.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.75) due to the asymmetric burden of adjustment and the seigniorage benefits enjoyed by the U.S. Suppression is also high (0.80) because the fixed exchange rate regime, enforced by the IMF, severely constrained the monetary policy options of non-reserve countries, effectively suppressing alternative approaches to economic management. Theater ratio is moderate (0.40) as the stated goal of universal stability increasingly served as a cover for the structural advantages of the reserve currency issuer. The metrics reflect the system's operation from the perspective of those subject to its discipline.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the U.S. and other reserve currency states, the system was a necessary coordination mechanism for global stability. From the perspective of non-reserve currency states, particularly developing nations, it was a system that extracted policy autonomy and imposed significant adjustment costs, making it a Tangled Rope or even a Snare. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States and, to a lesser extent, other reserve currency states are beneficiaries, gaining policy flexibility and seigniorage. Non-reserve currency states and developing nations are victims, bearing the costs of external discipline and limited policy options. The IMF acts as an agenda-setter, enforcing the rules that perpetuate these asymmetries. Global capital markets, while constrained, also influenced the system's dynamics and eventually contributed to its breakdown.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_quantification,
    'What is the precise economic value of the ''exorbitant privilege'' accrued by the United States under Bretton Woods, and how does it compare to the costs borne by non-reserve currency states?',
    'Detailed econometric modeling and historical accounting of seigniorage, balance of payments adjustment costs, and policy autonomy differentials across member states.',
    'A clear quantification would solidify the claim of asymmetric extraction, potentially reclassifying the constraint closer to a Snare for victim seats, or confirming its Tangled Rope nature with a high extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_quantification, empirical, 'Quantifying the asymmetric benefits and costs of the Bretton Woods system.').

omega_variable(
    gold_anchor_function_ambiguity,
    'Was the gold-dollar anchor primarily a genuine stabilizer for global trade, or a structural mechanism that enabled U.S. monetary policy flexibility at the expense of others?',
    'Comparative historical analysis of alternative monetary systems and counterfactual simulations of a truly multilateral reserve asset or more flexible exchange rates during the Bretton Woods era.',
    'If primarily a stabilizer, the coordination function is stronger, potentially lowering effective extraction for some seats. If primarily an enabling mechanism for U.S. privilege, it reinforces the Snare-like aspects for victim seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gold_anchor_function_ambiguity, conceptual, 'Ambiguity regarding the true function of the gold-dollar anchor.').

omega_variable(
    domestic_sovereignty_vs_external_discipline_tradeoff,
    'To what extent was the external monetary discipline imposed by Bretton Woods a necessary trade-off for global stability, versus an avoidable imposition that undermined national sovereignty?',
    'Analysis of the counterfactuals: what would have happened without Bretton Woods? What were the real alternatives for achieving stability without such asymmetric costs?',
    'If the trade-off was genuinely necessary, the coordination aspect of the Tangled Rope is emphasized. If avoidable, the extractive nature is amplified, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_sovereignty_vs_external_discipline_tradeoff, preference, 'The normative evaluation of the trade-off between global stability and national monetary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(bret_tr_t1957, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1957, 0.3).
narrative_ontology:measurement(bret_tr_t1964, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1964, 0.35).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(bret_be_t1957, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1957, 0.7).
narrative_ontology:measurement(bret_be_t1964, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1964, 0.73).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.73).
narrative_ontology:measurement(bret_su_t1957, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1957, 0.76).
narrative_ontology:measurement(bret_su_t1964, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1964, 0.78).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bretton_woods_treaty_substrate' kernel, focusing on the defense of national monetary sovereignty and the asymmetric distribution of benefits and costs within the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
