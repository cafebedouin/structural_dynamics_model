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
 *   human_readable: Bretton Woods Treaty Substrate: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story represents the 'sovereignty defense' reading of the
 *   Bretton Woods system, focusing on how the system, while ostensibly
 *   promoting stability, imposed significant external monetary discipline on
 *   non-reserve currency states, thereby limiting their national monetary
 *   sovereignty. The US, as the issuer of the reserve currency, benefited
 *   from an 'exorbitant privilege,' allowing it greater policy flexibility.
 *   The gold anchor, intended as a stabilizer, became a mechanism for this
 *   asymmetric extraction, effectively acting as a snare for many nations.
 *   This reading highlights the coercive aspects of the system's design and
 *   operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.65).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '73035e20-e8b2-4e3d-8003-9c475e8c3f42').
narrative_ontology:cs_kernel_codification('73035e20-e8b2-4e3d-8003-9c475e8c3f42', formalized).
narrative_ontology:cs_authority_grounding('73035e20-e8b2-4e3d-8003-9c475e8c3f42', extraction).
narrative_ontology:cs_interpretation_layer_present('73035e20-e8b2-4e3d-8003-9c475e8c3f42').
narrative_ontology:cs_reading_relation('73035e20-e8b2-4e3d-8003-9c475e8c3f42', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('73035e20-e8b2-4e3d-8003-9c475e8c3f42', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('73035e20-e8b2-4e3d-8003-9c475e8c3f42', foundational, national_monetary_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('73035e20-e8b2-4e3d-8003-9c475e8c3f42', national_monetary_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('73035e20-e8b2-4e3d-8003-9c475e8c3f42', foundational, reserve_currency_status_confers_asymmetric_privilege).
narrative_ontology:cs_axiom_status(reserve_currency_status_confers_asymmetric_privilege, holdable).
narrative_ontology:cs_axiom_grounding('73035e20-e8b2-4e3d-8003-9c475e8c3f42', reserve_currency_status_confers_asymmetric_privilege, empirically_contingent).
narrative_ontology:cs_reference_frame('73035e20-e8b2-4e3d-8003-9c475e8c3f42', post_war_sovereign_state_system).
narrative_ontology:cs_drift_state('73035e20-e8b2-4e3d-8003-9c475e8c3f42', post_bretton_woods_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('73035e20-e8b2-4e3d-8003-9c475e8c3f42', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the primary reserve currency, the US benefits from 'exorbitant privilege,' allowing it to run current account deficits without facing the same external monetary discipline as other nations. It sets the terms of the system and enforces its stability.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states are forced to maintain external monetary discipline, often at the cost of domestic policy autonomy, to preserve fixed exchange rates against the dollar. Their ability to devalue or manage capital flows is constrained by the system's rules, making them vulnerable to external shocks.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Often lacking diversified economies or strong financial institutions, these nations are particularly susceptible to the external monetary discipline imposed by Bretton Woods. They face severe constraints on their development policies due to the need to maintain currency convertibility and attract dollar reserves.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations, payer,
    powerless, generational, trapped, national).

% Beyond the US, other nations whose currencies gain reserve status (e.g., Germany, Japan in later periods) also benefit from increased demand for their currency, lower borrowing costs, and greater flexibility in monetary policy, though to a lesser extent than the US.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers, beneficiary,
    institutional, generational, mobile, global).

% The IMF enforces the rules of the Bretton Woods system, providing loans to countries facing balance-of-payments difficulties, but often with strict conditions that reinforce external monetary discipline on non-reserve currency states. It acts as a key mechanism for maintaining the system's structure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The system aimed to coordinate international monetary policy to prevent competitive devaluations and promote stable exchange rates, facilitating international trade and investment.
% TRANSFER_FUNCTION: Transfers the burden of external monetary adjustment from the reserve currency issuer (primarily the US) to non-reserve currency states, effectively transferring seigniorage and policy autonomy.
% ABSENT_VOICES: Many post-colonial and developing nations, whose economic structures made adherence to fixed exchange rates and capital mobility particularly costly, had limited voice in the design and early enforcement of the system. They would argue for greater policy space and less external discipline.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (as interpreted by this reading) had not been established, the international monetary system would likely have remained more fragmented, with greater capital controls and less dollar dominance, leading to a different global economic order.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, trade wars, and unstable exchange rates, which disrupted global commerce and contributed to economic crises.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate the problems of the interwar period. However, the effectiveness of Bretton Woods in solving these problems without creating new asymmetries is contested by critical international political economy scholars and developing nation advocates.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the system's design inherently transferred policy costs to non-reserve currency states. Suppression (0.75) is also high, reflecting the limited exit options for nations needing access to international trade and finance, forcing them to adhere to the dollar-gold standard. Theater ratio (0.20) is relatively low, as the system's stated goals of stability were genuinely pursued, but the underlying asymmetric power dynamics were often downplayed or reframed as universal benefits. The gold anchor, while presented as a neutral standard, became a tool for enforcing dollar hegemony.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the US, Bretton Woods was a successful coordination mechanism for global stability. From the perspective of non-reserve currency states, it was a system that extracted their monetary sovereignty and constrained their development options. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States and other reserve currency issuers are clear beneficiaries, enjoying policy flexibility and seigniorage. Non-reserve currency states and developing nations are the primary victims, bearing the costs of external discipline and constrained policy choices. The IMF acts as an agenda-setter, enforcing the rules that maintain this asymmetric structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_quantification,
    'What is the precise economic value of the ''exorbitant privilege'' accrued by the United States under Bretton Woods, and how does it compare to the costs borne by non-reserve currency states?',
    'Detailed econometric studies comparing counterfactual scenarios of a non-dollar-centric international monetary system, or historical analysis of capital flows and interest rate differentials.',
    'A clear quantification would strengthen the claim of asymmetric extraction and provide empirical grounding for the ''snare'' aspect of the gold anchor, potentially reclassifying the US seat as a pure beneficiary of extraction rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_quantification, empirical, 'Quantifying the asymmetric benefits of reserve currency status.').

omega_variable(
    gold_anchor_function_ambiguity,
    'Was the gold anchor primarily a neutral mechanism for exchange rate stability, or did its operationalization inherently serve to enforce dollar hegemony and external discipline on other nations?',
    'Historical analysis of IMF interventions and US monetary policy decisions, focusing on instances where the gold convertibility rule was applied selectively or used to pressure other states.',
    'If primarily a tool of hegemony, the gold anchor''s function shifts from a coordination mechanism to a pure extraction mechanism, increasing the overall extractiveness and suppression scores for non-reserve currency states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_function_ambiguity, conceptual, 'Ambiguity of the gold anchor''s primary function: stabilizer vs. enforcement tool.').

omega_variable(
    policy_autonomy_vs_stability_tradeoff,
    'To what extent was the sacrifice of national monetary sovereignty by non-reserve currency states a necessary tradeoff for global monetary stability, versus an avoidable cost imposed by the system''s design?',
    'Comparative analysis with alternative proposals for international monetary reform (e.g., Keynes''s Bancor plan) and their projected outcomes for stability and national autonomy.',
    'If the sacrifice was largely avoidable, it would underscore the extractive nature of the system; if largely necessary, it would lean towards a more ''tangled rope'' classification, acknowledging a genuine, albeit costly, coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_autonomy_vs_stability_tradeoff, preference, 'Assessing the necessity of sovereignty sacrifice for stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.2).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.5).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.6).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.6).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.7).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.73).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods Treaty Substrate kernel. It focuses on the system's role in enforcing external monetary discipline and preserving national monetary sovereignty, particularly for non-reserve currency states. Sibling readings ('keynesian_embedded_liberalism', 'neoliberal_convertibility') offer alternative interpretations of the system's primary function and beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
