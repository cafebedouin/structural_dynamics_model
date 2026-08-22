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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility (Policy-Flexible Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint represents the 'policy-flexible' reading of dollar-gold
 *   convertibility under the Bretton Woods system, where the U.S. implicitly
 *   reserved the right to prioritize domestic economic stability over strict
 *   convertibility at a fixed gold price. This reading contrasts with a
 *   'strict' interpretation that views convertibility as an absolute legal
 *   obligation. The flexibility allowed the U.S. to manage its economy, but
 *   at the cost of increasing devaluation risk for foreign dollar holders,
 *   leading to rising extractiveness over time as the system matured.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.65).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.4).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '3f3654bd-0bf3-4465-b798-7c46bea8cf1f').
narrative_ontology:cs_kernel_codification('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', formalized).
narrative_ontology:cs_authority_grounding('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', lineage).
narrative_ontology:cs_interpretation_layer_present('3f3654bd-0bf3-4465-b798-7c46bea8cf1f').
narrative_ontology:cs_reading_relation('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', domestic_stability_priority, conventional).
narrative_ontology:cs_axiom('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', foundational, reserve_currency_privilege).
narrative_ontology:cs_axiom_status(reserve_currency_privilege, holdable).
narrative_ontology:cs_axiom_grounding('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', reserve_currency_privilege, conventional).
narrative_ontology:cs_reference_frame('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', flexible_monetary_sovereignty).
narrative_ontology:cs_drift_state('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', post_nixon_shock_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3f3654bd-0bf3-4465-b798-7c46bea8cf1f', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, domestic_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains the flexibility to prioritize domestic economic goals (employment, growth) over the strict maintenance of dollar convertibility at a fixed gold price. Benefits from the ability to devalue the dollar if necessary, shifting costs to foreign holders.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_treasury, agenda_setter,
    institutional, generational, mobile, national).

% Exercises monetary policy independently, without being strictly bound by the need to defend the gold parity. Benefits from the freedom to use interest rates and quantitative easing to manage the domestic economy, even if it puts pressure on convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, federal_reserve, agenda_setter,
    institutional, generational, mobile, national).

% Hold significant dollar reserves, which are subject to devaluation risk if the U.S. prioritizes domestic policy over convertibility. They bear the cost of dollar depreciation and face limited options for divesting large dollar holdings without destabilizing the global financial system.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, biographical, constrained, global).

% Hold dollar-denominated assets and face currency risk. While they can move capital, large-scale divestment can be costly and disruptive. They bear the cost of dollar depreciation when it occurs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_investors, payer,
    powerful, immediate, mobile, global).

% Benefit from a more stable domestic economy, lower interest rates, and potentially more competitive exports if the dollar depreciates. They are shielded from the deflationary pressures that strict convertibility might impose.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, domestic_industries, beneficiary,
    organized, biographical, mobile, national).

% Monitors the international monetary system and provides policy advice. Its mandate includes promoting exchange rate stability, but it also recognizes the need for domestic policy autonomy. It observes the tension between these goals.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international trade and finance by establishing the dollar as a reserve currency, while allowing the U.S. to manage its domestic economy with greater flexibility than strict gold convertibility would permit.
% TRANSFER_FUNCTION: Transfers the risk of monetary adjustment (devaluation) from the U.S. domestic economy to foreign holders of dollar reserves, in exchange for the stability provided by the dollar's reserve currency status.
% ABSENT_VOICES: Advocates for a truly multilateral reserve asset or a more symmetrical adjustment mechanism would object, arguing that the current system unfairly burdens non-U.S. economies. Their proposals are sidelined by the existing dollar-centric framework.
% DISAPPEARANCE_RATIONALE: If the policy-flexible convertibility framework vanished, the global financial system would face immediate instability. Foreign central banks would scramble to diversify reserves, leading to massive currency fluctuations and a collapse of confidence in the dollar's role, forcing a rapid, chaotic reorganization of international monetary arrangements.
% FOUNDING_PROBLEM: The need to balance international monetary stability with domestic economic policy autonomy, particularly after World War II, avoiding the deflationary rigidity of the interwar gold standard.
% FOUNDING_PROBLEM_CORROBORATION: Economists and policymakers outside the U.S. Treasury and Federal Reserve acknowledge the persistent tension between international liquidity and domestic policy goals, particularly in the context of the Triffin Dilemma. Historical analyses of the Bretton Woods system's evolution also corroborate this problem.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the growing burden on foreign central banks and investors as the U.S. leveraged its reserve currency status for domestic policy flexibility, culminating in the 1971 Nixon Shock. Suppression (0.40) is moderate; while there was no overt coercion, the structural dependence on the dollar limited alternatives for foreign actors. Theater ratio (0.20) is low, as the U.S. genuinely sought to maintain the system, but its actions increasingly diverged from the strict convertibility ideal. The rising extractiveness and suppression over the interval reflect the increasing strain on the system as the U.S. exploited its flexibility.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, this was a necessary and beneficial flexibility to manage a global reserve currency. From the perspective of foreign central banks, it was an increasingly extractive arrangement where they bore the costs of U.S. domestic policy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and Federal Reserve are clear beneficiaries and agenda-setters, gaining monetary autonomy (low directionality). Foreign central banks and international investors are payers, bearing the risk and costs of dollar devaluation (high directionality). Domestic industries are indirect beneficiaries of U.S. policy flexibility. The IMF acts as an observer, monitoring the system's stability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convertibility_as_policy_choice_vs_legal_obligation,
    'Was dollar-gold convertibility primarily a policy choice, subject to U.S. domestic priorities, or a binding legal obligation under the Bretton Woods agreements?',
    'Analysis of declassified U.S. government documents and international legal interpretations from the period, particularly regarding the intent behind Article IV of the IMF Articles of Agreement.',
    'If primarily a policy choice, this ''policy-flexible'' reading is accurate. If a binding legal obligation, the U.S. actions were a breach, and the ''strict convertibility'' reading gains force, reclassifying the U.S. as an agenda-setter in violation of a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convertibility_as_policy_choice_vs_legal_obligation, conceptual, 'Ambiguity over the nature of convertibility: policy flexibility vs. strict legal duty.').

omega_variable(
    triffin_dilemma_inevitability,
    'To what extent was the breakdown of convertibility under this reading an inevitable outcome of the Triffin Dilemma (the conflict between providing international liquidity and maintaining confidence in the reserve currency), rather than a deliberate policy choice?',
    'Counterfactual historical analysis exploring alternative policy paths and their likely outcomes, alongside economic modeling of the system''s inherent contradictions.',
    'If inevitable, the ''Triffin structural'' reading gains strength, suggesting the system was a Snare by design, regardless of U.S. policy intent. If avoidable, the ''policy-flexible'' reading emphasizes U.S. agency and responsibility for the extractive outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, empirical, 'Whether the system''s collapse was structurally determined or a result of policy choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.55).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'policy_flexible_reading' emphasizes U.S. monetary autonomy and the resulting burden on foreign dollar holders, contrasting with a 'strict' legal interpretation and a 'structural' view of systemic unsustainability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
