% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Strict Dollar-Gold Convertibility Obligation (1944-1971)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'strict convertibility' reading of the
 *   dollar-gold convertibility kernel, emphasizing Article IV of the IMF
 *   Agreement as a binding legal obligation on the United States from 1944 to
 *   1971. Under this reading, the U.S. was legally bound to convert dollars
 *   held by foreign central banks into gold at a fixed price, significantly
 *   constraining its domestic monetary policy autonomy. Sibling readings
 *   include 'policy_flexible_reading' (convertibility as conditional
 *   obligation subordinate to domestic stability) and
 *   'triffin_structural_reading' (convertibility as an inherently
 *   unsustainable design flaw).
 *
 * KEY AGENTS:
 *   - us_monetary_policymakers: Primary target (institutional/constrained) — bore policy costs.
 *   - creditor_nations: Primary beneficiary (organized/mobile) — held enforceable claims.
 *   - international_monetary_fund: Agenda setter (institutional/analytical) — administered the system.
 *   - us_domestic_economy: Secondary target (powerless/trapped) — bore indirect macroeconomic costs.
 *   - gold_speculators: Secondary beneficiary (moderate/arbitrage) — profited from system strain.
 *   - policy_flexible_advocates: Excluded (organized/constrained) — voices for domestic autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.75).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.85).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Strict Dollar-Gold Convertibility Obligation (1944-1971)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '1d07b458-40ec-46b8-bb7b-eafcc1af3969').
narrative_ontology:cs_kernel_codification('1d07b458-40ec-46b8-bb7b-eafcc1af3969', formalized).
narrative_ontology:cs_authority_grounding('1d07b458-40ec-46b8-bb7b-eafcc1af3969', lineage).
narrative_ontology:cs_interpretation_layer_present('1d07b458-40ec-46b8-bb7b-eafcc1af3969').
narrative_ontology:cs_reading_relation('1d07b458-40ec-46b8-bb7b-eafcc1af3969', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d07b458-40ec-46b8-bb7b-eafcc1af3969', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('1d07b458-40ec-46b8-bb7b-eafcc1af3969', foundational, dollar_convertibility_is_binding_legal_obligation).
narrative_ontology:cs_axiom_status(dollar_convertibility_is_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1d07b458-40ec-46b8-bb7b-eafcc1af3969', dollar_convertibility_is_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('1d07b458-40ec-46b8-bb7b-eafcc1af3969', secondary, us_monetary_policy_subordinate_to_external_stability).
narrative_ontology:cs_axiom_status(us_monetary_policy_subordinate_to_external_stability, holdable).
narrative_ontology:cs_axiom_grounding('1d07b458-40ec-46b8-bb7b-eafcc1af3969', us_monetary_policy_subordinate_to_external_stability, conventional).
narrative_ontology:cs_reference_frame('1d07b458-40ec-46b8-bb7b-eafcc1af3969', post_bretton_woods_order).
narrative_ontology:cs_drift_state('1d07b458-40ec-46b8-bb7b-eafcc1af3969', pre_nixon_shock_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1d07b458-40ec-46b8-bb7b-eafcc1af3969', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_speculators).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policymakers).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for managing U.S. monetary policy while adhering to the gold convertibility obligation. Faced increasing pressure from gold outflows and the need to balance domestic economic goals with external stability. Exit meant abandoning the Bretton Woods system, a politically costly move.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policymakers, payer,
    institutional, biographical, constrained, national).

% Held dollar reserves and had the right to convert them to gold at a fixed price. Benefited from the stability of the system and could exert pressure on the U.S. by demanding gold, thereby constraining U.S. policy. Their exit option was to demand gold, which they increasingly did.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    organized, generational, mobile, global).

% Administered the Bretton Woods system, including the rules for convertibility. Benefited from the system's stability and its role as a central arbiter. Its 'exit' was to propose systemic reforms, which it eventually did.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Experienced the indirect costs of constrained U.S. monetary policy, such as higher interest rates or slower growth, as policymakers prioritized external convertibility over domestic needs. Had no direct exit from these macroeconomic effects.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerless, immediate, trapped, national).

% Profited from the growing imbalance between the official gold price and the market price, and from the increasing likelihood of a devaluation or suspension of convertibility. Could arbitrage the system by buying gold or shorting the dollar.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_speculators, beneficiary,
    moderate, immediate, arbitrage, global).

% Economists and policymakers within the U.S. who argued for greater domestic monetary policy autonomy, even at the cost of fixed exchange rates or convertibility. Their views were often sidelined by the perceived international obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable international monetary system with fixed exchange rates and confidence in the dollar as the global reserve currency, facilitating post-war global trade and investment.
% TRANSFER_FUNCTION: Transferred U.S. monetary policy autonomy to the international system, allowing creditor nations to constrain U.S. domestic policy via gold demands, in exchange for global financial stability.
% ABSENT_VOICES: Advocates for a more flexible, domestically-oriented U.S. monetary policy were often overridden by the perceived international obligation. Also, developing nations whose economic needs were secondary to the stability of the core currencies.
% DISAPPEARANCE_RATIONALE: The actual suspension of dollar-gold convertibility in 1971 led to the collapse of the Bretton Woods system, a shift to floating exchange rates, and a fundamental reorganization of international finance, demonstrating its foundational role.
% FOUNDING_PROBLEM: To prevent the competitive devaluations, currency instability, and protectionism that plagued the interwar period, and to establish a stable, predictable international monetary order for post-WWII reconstruction and growth.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international finance, economists, and contemporary diplomatic records from non-U.S. parties (e.g., European central bankers, IMF reports) corroborate the initial problem and the system's eventual unsustainability due to the Triffin Dilemma, indicating the founding problem was largely addressed but the mechanism itself became obsolete.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provided genuine international monetary coordination (fixed exchange rates, dollar as reserve currency) but did so through asymmetric extraction of U.S. policy autonomy. Extractiveness (0.75) was high, reflecting the significant policy space ceded by the U.S. to maintain convertibility. Suppression (0.85) was also high, as the U.S. had to actively defend the dollar's value and gold reserves against speculative attacks and foreign demands. Theater ratio (0.15) remained low, as the obligation was genuinely binding and actively enforced until its suspension. The metrics show a clear trend of increasing extractiveness and suppression as the system came under greater strain over its lifespan.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, the strict convertibility rule was a necessary anchor for global finance, ensuring the dollar's value. From the U.S. perspective, particularly as the Triffin Dilemma intensified, it became an increasingly burdensome constraint on domestic policy, forcing difficult trade-offs between internal and external balance. The engine's per-seat classification would reflect this divergence, with beneficiaries seeing a more 'Rope-like' function and victims experiencing 'Snare-like' extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. monetary policymakers and the domestic economy were the primary targets, bearing the costs of constrained policy. Creditor nations and the IMF were beneficiaries, gaining stability and influence. Gold speculators benefited from the system's growing instability. The strict legal interpretation meant that the U.S. was structurally positioned as the primary payer for global monetary stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope, rather than a pure Snare, acknowledges the genuine coordination function of the Bretton Woods system in its early years. However, the rising extractiveness and suppression over time, coupled with the 'dead' status of the founding problem (as the system became unsustainable), indicates a drift towards a more extractive, Snare-like operation by its end. The strict convertibility reading, by emphasizing the binding nature of the obligation, highlights the mechanism of this extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_obligation_vs_policy_choice,
    'To what extent was dollar-gold convertibility a binding legal obligation versus a policy choice that could have been unilaterally altered by the U.S. earlier?',
    'Analysis of international legal precedents, diplomatic archives, and internal U.S. government deliberations regarding the interpretation of Article IV and the political costs of unilateral action.',
    'If primarily a policy choice, the constraint''s suppression and extractiveness would be lower, reflecting greater U.S. agency. If a binding obligation, the current high values are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_obligation_vs_policy_choice, conceptual, 'Ambiguity of convertibility as legal vs. policy constraint.').

omega_variable(
    triffin_dilemma_impact,
    'Was the collapse of dollar-gold convertibility primarily due to the strict interpretation of the obligation, or to the inherent structural flaws of the Bretton Woods system (Triffin Dilemma)?',
    'Counterfactual historical analysis comparing outcomes under alternative interpretations or systemic designs, and econometric modeling of the system''s long-term sustainability.',
    'If structural flaws were dominant, the ''strict convertibility'' reading highlights the mechanism through which those flaws manifested. If the strict interpretation itself was the primary driver of unsustainability, it underscores the policy rigidity imposed by this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_impact, empirical, 'Role of strict interpretation vs. structural flaws in system collapse.').

omega_variable(
    us_policy_constraint_extent,
    'What was the true extent of U.S. domestic policy constraint imposed by the convertibility obligation, considering both explicit gold demands and implicit market pressures?',
    'Detailed econometric studies correlating U.S. monetary and fiscal policy decisions with gold flows, balance of payments data, and foreign central bank actions, controlling for other economic factors.',
    'If the constraint was less severe than perceived, the extractiveness and suppression metrics for the U.S. as a victim would be lower. If more severe, the current metrics are conservative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_policy_constraint_extent, empirical, 'Quantifying U.S. policy constraint under convertibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(doll_tr_t1956, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1956, 0.13).
narrative_ontology:measurement(doll_tr_t1962, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1962, 0.14).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.15).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.45).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(doll_be_t1956, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(doll_be_t1962, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1962, 0.67).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.72).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.6).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(doll_su_t1956, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1956, 0.7).
narrative_ontology:measurement(doll_su_t1962, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1962, 0.78).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.82).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
