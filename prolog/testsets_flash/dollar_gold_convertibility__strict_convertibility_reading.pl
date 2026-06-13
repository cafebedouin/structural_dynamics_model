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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Dollar Gold Convertibility (Strict Legal Obligation Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint models the strict interpretation of Article IV of the IMF
 *   Articles of Agreement, which mandated the U.S. dollar's convertibility to
 *   gold at a fixed price, as a binding legal obligation on U.S. monetary
 *   policy. This reading views the convertibility as a primary,
 *   non-negotiable constraint, placing the U.S. in a victim role due to the
 *   severe limitations on its domestic economic policy. Creditor nations, by
 *   contrast, are beneficiaries, holding enforceable claims on U.S. gold
 *   reserves. The constraint is claimed as a Snare because its coordination
 *   function (global monetary stability) increasingly served as a cover for
 *   extraction from U.S. policy space, enforced by the threat of gold
 *   outflows.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.75).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Dollar Gold Convertibility (Strict Legal Obligation Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '2c7765e0-f14e-464a-97d8-7293a74aee60').
narrative_ontology:cs_kernel_codification('2c7765e0-f14e-464a-97d8-7293a74aee60', formalized).
narrative_ontology:cs_authority_grounding('2c7765e0-f14e-464a-97d8-7293a74aee60', lineage).
narrative_ontology:cs_interpretation_layer_present('2c7765e0-f14e-464a-97d8-7293a74aee60').
narrative_ontology:cs_reading_relation('2c7765e0-f14e-464a-97d8-7293a74aee60', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('2c7765e0-f14e-464a-97d8-7293a74aee60', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('2c7765e0-f14e-464a-97d8-7293a74aee60', foundational, gold_convertibility_is_absolute_obligation).
narrative_ontology:cs_axiom_status(gold_convertibility_is_absolute_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2c7765e0-f14e-464a-97d8-7293a74aee60', gold_convertibility_is_absolute_obligation, deontological).
narrative_ontology:cs_axiom('2c7765e0-f14e-464a-97d8-7293a74aee60', foundational, domestic_policy_subordinate_to_external_stability).
narrative_ontology:cs_axiom_status(domestic_policy_subordinate_to_external_stability, holdable).
narrative_ontology:cs_axiom_grounding('2c7765e0-f14e-464a-97d8-7293a74aee60', domestic_policy_subordinate_to_external_stability, conventional).
narrative_ontology:cs_reference_frame('2c7765e0-f14e-464a-97d8-7293a74aee60', bretton_woods_original_intent).
narrative_ontology:cs_drift_state('2c7765e0-f14e-464a-97d8-7293a74aee60', late_1960s_triffin_pressure, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2c7765e0-f14e-464a-97d8-7293a74aee60', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_speculators).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states_monetary_policy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the obligation to convert dollars to gold at a fixed price, severely limiting its ability to conduct independent monetary policy for domestic economic stabilization. Any expansionary policy risks depleting gold reserves.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_monetary_policy, payer,
    institutional, generational, constrained, national).

% Hold dollar reserves with the enforceable right to convert them to gold, giving them leverage over U.S. policy and protecting their wealth from dollar devaluation. They benefit from the stability and discipline imposed on the U.S.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Profit from any perceived weakness in the U.S. commitment to convertibility, or from the eventual revaluation of gold if convertibility breaks. They hold a put option on the dollar.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_speculators, beneficiary,
    moderate, immediate, arbitrage, global).

% Suffers from the constraints on monetary policy, potentially leading to higher unemployment or slower growth when the U.S. cannot stimulate its economy due to convertibility concerns. Its fate is tied to the gold standard.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerless, biographical, trapped, national).

% Administers the international monetary system, including the rules of convertibility. While not directly enforcing gold claims, its institutional framework underpins the legal obligation and provides a forum for disputes.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Economists and policymakers who argue for greater flexibility in U.S. monetary policy to address domestic needs, viewing convertibility as an outdated and harmful constraint. Their arguments are often overridden by the perceived international obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable international monetary anchor by pegging the world's reserve currency (the dollar) to gold, facilitating international trade and investment by reducing exchange rate volatility and providing a credible store of value.
% TRANSFER_FUNCTION: Transfers control over domestic monetary policy from the United States to the international system (specifically, to creditor nations holding dollar claims), in exchange for maintaining the dollar's role as the global reserve currency.
% ABSENT_VOICES: Advocates for a more flexible, domestically-oriented U.S. monetary policy are structurally excluded from the decision-making process regarding convertibility, as the international obligation takes precedence. Their arguments for prioritizing domestic employment or growth are sidelined by the 'strict convertibility' interpretation.
% DISAPPEARANCE_RATIONALE: If the strict convertibility obligation vanished overnight, the U.S. would immediately gain full monetary policy independence, likely leading to a more expansionary stance. Creditor nations would lose their gold claim leverage, and the international monetary system would need a new anchor, potentially leading to significant currency volatility and a search for alternative reserve assets.
% FOUNDING_PROBLEM: The Bretton Woods system was established to prevent a return to the competitive devaluations and monetary instability of the interwar period, by creating a fixed exchange rate system anchored by the dollar's convertibility to gold.
% FOUNDING_PROBLEM_CORROBORATION: While the IMF and some central bankers still attest to the need for monetary stability, the specific problem of fixed gold convertibility became unsustainable by the late 1960s due to the Triffin dilemma. Independent economists and historical analyses widely corroborate that the founding problem, as strictly defined by gold convertibility, was no longer solvable under the original terms by the time of its collapse in 1971.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).

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
 *   Extractiveness is high and rising (0.85 by 1971) because the fixed convertibility rate became increasingly misaligned with economic realities, forcing the U.S. to sacrifice domestic policy goals (e.g., full employment) to defend the gold parity. Suppression (0.75) was high due to the legal and reputational costs of abandoning convertibility, and the active enforcement by creditor nations through gold demands. Theater ratio is low (0.1) because the obligation was genuinely binding and actively defended until its collapse; there was little performative maintenance without real function. The rising extractiveness and suppression over time reflect the increasing pressure of the Triffin dilemma.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, the constraint was a Rope or even a Mountain, providing essential stability and discipline. From the U.S. monetary policy seat, it was increasingly a Snare, extracting policy autonomy and imposing domestic costs. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary policy and domestic economy are clear targets (payers) as they bear the costs of constrained policy (d=1.0). Creditor nations are beneficiaries (d=0.0) as they gain leverage and stability. Gold speculators are also beneficiaries (d=0.0) as they profit from the system's inherent instability. The IMF, as agenda-setter, is more symmetric (d=0.5) as it maintains the system but also faces challenges from its inherent contradictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (global monetary stability) was initially genuine, but the 'strict convertibility' reading led to a situation where the mechanism itself became the problem. The founding problem (preventing interwar instability) was eventually 'dead' in its gold-convertibility form, but the constraint persisted due to institutional inertia and the concentrated benefits to creditor nations, making it a Snare rather than a Piton. The engine's detection of a 'dead' founding problem combined with 'world_rearranges' disappearance verdict will flag this as a capture/zombie constraint, consistent with the Snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_economic_priority,
    'Was the Article IV convertibility primarily a legal obligation or an economic policy choice, and how did this framing affect U.S. policy decisions?',
    'Analysis of internal U.S. government documents and international negotiations: did legal counsel or economic advisors hold sway in moments of crisis?',
    'If primarily a legal obligation, the constraint''s suppression is higher and less negotiable. If primarily an economic choice, the U.S. had more agency, and the extraction was more self-imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_vs_economic_priority, conceptual, 'Ambiguity between legal obligation and policy choice.').

omega_variable(
    triffin_dilemma_impact,
    'To what extent did the Triffin dilemma (the conflict between the dollar''s role as reserve currency and its convertibility to gold) render the ''strict convertibility'' reading unsustainable?',
    'Historical economic modeling of gold reserve depletion rates and dollar liabilities, counterfactual analysis of alternative policy paths.',
    'If the Triffin dilemma made strict convertibility inherently unsustainable, the constraint was a structural Snare from the outset, rather than merely becoming one due to policy choices. This would strengthen the ''triffin_structural_reading'' as a more accurate description of the underlying reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_impact, empirical, 'Impact of the Triffin dilemma on convertibility''s viability.').

omega_variable(
    creditor_nation_coercion,
    'How actively did creditor nations (e.g., France) use their gold claims to coerce U.S. monetary policy, and what were the specific mechanisms of this coercion?',
    'Archival research into diplomatic communications, central bank records, and declassified intelligence reports detailing gold demands and their stated motivations.',
    'Higher evidence of active coercion would increase the measured suppression and extractiveness, reinforcing the Snare classification. Lower evidence might suggest more passive acceptance of the constraint by the U.S.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_nation_coercion, empirical, 'Degree of active coercion by creditor nations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.75).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.65).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, us_balance_of_payments_policy).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, global_liquidity_provision).

% DUAL FORMULATION NOTE:
% This constraint is the 'strict_convertibility_reading' of the 'dollar_gold_convertibility' kernel. It emphasizes the binding legal obligation of the U.S. to convert dollars to gold, constraining its domestic monetary policy. See also 'policy_flexible_reading' and 'triffin_structural_reading' for alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
