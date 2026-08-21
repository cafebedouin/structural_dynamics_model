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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Dollar Gold Convertibility (Strict Legal Obligation Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint story analyzes dollar-gold convertibility, as defined by
 *   Article IV of the IMF Agreement, from the perspective that it constituted
 *   a binding legal obligation on the United States. This 'strict
 *   convertibility reading' views the U.S. as structurally constrained by the
 *   need to maintain convertibility, subordinating domestic monetary policy
 *   to external balance requirements. The constraint is claimed as a Snare,
 *   reflecting the high extraction from U.S. policy space and the active
 *   enforcement by creditor nations, despite the initial coordination
 *   benefits for the global system. The metrics reflect the increasing strain
 *   and cost borne by the U.S. over the Bretton Woods period.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.9).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Dollar Gold Convertibility (Strict Legal Obligation Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '838e54ba-aadc-4e0b-ba10-e704de02823a').
narrative_ontology:cs_kernel_codification('838e54ba-aadc-4e0b-ba10-e704de02823a', formalized).
narrative_ontology:cs_authority_grounding('838e54ba-aadc-4e0b-ba10-e704de02823a', lineage).
narrative_ontology:cs_interpretation_layer_present('838e54ba-aadc-4e0b-ba10-e704de02823a').
narrative_ontology:cs_reading_relation('838e54ba-aadc-4e0b-ba10-e704de02823a', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('838e54ba-aadc-4e0b-ba10-e704de02823a', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('838e54ba-aadc-4e0b-ba10-e704de02823a', foundational, gold_convertibility_as_legal_imperative).
narrative_ontology:cs_axiom_status(gold_convertibility_as_legal_imperative, holdable).
narrative_ontology:cs_axiom_grounding('838e54ba-aadc-4e0b-ba10-e704de02823a', gold_convertibility_as_legal_imperative, deontological).
narrative_ontology:cs_axiom('838e54ba-aadc-4e0b-ba10-e704de02823a', secondary, us_monetary_sovereignty_subordinate_to_external_balance).
narrative_ontology:cs_axiom_status(us_monetary_sovereignty_subordinate_to_external_balance, holdable).
narrative_ontology:cs_axiom_grounding('838e54ba-aadc-4e0b-ba10-e704de02823a', us_monetary_sovereignty_subordinate_to_external_balance, conventional).
narrative_ontology:cs_reference_frame('838e54ba-aadc-4e0b-ba10-e704de02823a', bretton_woods_original_intent).
narrative_ontology:cs_drift_state('838e54ba-aadc-4e0b-ba10-e704de02823a', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('838e54ba-aadc-4e0b-ba10-e704de02823a', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, global_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the legal obligation to convert dollars to gold at a fixed price, constraining domestic monetary policy. Bears the cost of gold outflows and the pressure to maintain external balance, often at the expense of domestic growth. Exit means abandoning the global reserve currency role.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed, payer).

% Hold dollar reserves with a credible gold backing, providing stability and an enforceable claim on U.S. assets. They benefit from a stable international monetary system and can exert pressure on the U.S. to maintain convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, agenda_setter).

% Administers the international monetary system and monitors compliance with Article IV. Benefits from the stability provided by convertibility, but also faces the systemic challenges of the Triffin dilemma.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Experiences the effects of monetary policy constrained by external convertibility demands, often leading to higher unemployment or slower growth when gold outflows necessitate tighter policy. Has no direct exit from these macroeconomic conditions.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerless, biographical, trapped, national).

% Relies on the stability and predictability of the gold-dollar standard for international transactions and investment. Benefits from the system's initial credibility but also reacts to any signs of strain or potential non-compliance.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, global_financial_markets, observer,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, global_financial_markets, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, credible anchor for the international monetary system, facilitating global trade and investment by fixing exchange rates and ensuring confidence in the dollar as a reserve currency.
% TRANSFER_FUNCTION: Transferred U.S. monetary policy autonomy and gold reserves to maintain global confidence in the dollar, effectively subsidizing international financial stability for creditor nations.
% ABSENT_VOICES: U.S. domestic industries and labor unions, who would advocate for expansionary monetary policies to support employment and growth, unconstrained by external balance requirements. Their concerns were often subordinated to international financial stability.
% DISAPPEARANCE_RATIONALE: The formal suspension of dollar-gold convertibility in 1971 (the 'Nixon Shock') led to a fundamental reorganization of the international monetary system, shifting to floating exchange rates and ending the Bretton Woods era. The world did not remain unchanged.
% FOUNDING_PROBLEM: The post-WWII international monetary system suffered from instability, competitive devaluations, and a lack of a credible reserve asset, hindering global economic recovery and trade.
% FOUNDING_PROBLEM_CORROBORATION: International economic historians, central bank archives, and IMF records from the period corroborate the initial problem of post-war monetary instability. However, independent economic analyses and later historical accounts from outside the U.S. Treasury or IMF often highlight the inherent unsustainability of the system, suggesting the 'founding problem' evolved or was superseded by new challenges.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the U.S. bore significant costs (gold outflows, constrained policy) to maintain a system that primarily benefited creditor nations and global stability. Suppression is very high, as the U.S. was legally and politically bound, facing severe consequences (loss of reserve currency status) for non-compliance. Theater ratio is low, as the obligation was genuinely binding and actively defended until its formal suspension. Accessibility collapse is high for the U.S. as there were few viable alternatives to maintain its global financial role without convertibility. Resistance grew over time from domestic political forces.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, the convertibility was a legitimate and beneficial coordination mechanism. From the U.S. perspective, especially as the Triffin dilemma intensified, it became an increasingly extractive burden. The engine's computation of per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading, the United States (Treasury/Fed, domestic economy) is the primary target/victim, bearing the costs of constrained policy and gold outflows. Creditor nations and the IMF are beneficiaries, gaining from system stability and enforceable claims. The IMF also acts as an agenda-setter, enforcing the rules. Global financial markets are observers who benefit from stability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_vs_flexible_obligation,
    'Was dollar-gold convertibility truly a binding, unconditional legal obligation, or was it understood by key actors (e.g., U.S. policymakers) as a conditional commitment subordinate to domestic economic stability?',
    'Analysis of declassified U.S. policy documents, diplomatic cables, and internal IMF deliberations from the 1960s, focusing on explicit statements regarding the hierarchy of policy objectives.',
    'If found to be conditional, the ''policy_flexible_reading'' gains strength, reducing the perceived extractiveness and suppression on the U.S. in this ''strict'' reading, potentially shifting its classification from Snare to Tangled Rope or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_vs_flexible_obligation, empirical, 'Ambiguity regarding the binding nature of convertibility for the U.S.').

omega_variable(
    triffin_dilemma_impact,
    'To what extent did the inherent structural contradiction of the Triffin dilemma (the need for dollars for global liquidity vs. the need for gold backing for dollar credibility) undermine the ''binding'' nature of convertibility, making its eventual collapse inevitable?',
    'Historical counterfactual analysis: could the system have been sustained with different U.S. policies, or was the structural flaw dominant? This is a conceptual debate among economic historians.',
    'If the Triffin dilemma is seen as overwhelmingly dominant, it supports the ''triffin_structural_reading'', suggesting that the ''strict convertibility'' was a temporary, unsustainable state rather than a stable, enforceable Snare. This would shift the focus from U.S. agency to systemic forces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_impact, conceptual, 'The role of the Triffin dilemma in the constraint''s persistence and eventual collapse.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''strict_convertibility_reading'' of the ''dollar_gold_convertibility'' kernel, or does it conflate elements of other readings?',
    'Review by international monetary historians and legal scholars to ensure the interpretation of ''binding legal obligation'' is distinct and consistent with the specified reading''s core tenets, without incorporating elements of ''policy_flexible_reading'' or ''triffin_structural_reading''.',
    'If conflated, the constraint would need to be decomposed further or re-authored to align with a single, ε-invariant reading, ensuring the integrity of the kernel classification system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the purity of this specific kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.09).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.75).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.82).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.8).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policy_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel, focusing on its nature as a strict legal obligation. It is linked to its sibling readings and to the constraint on U.S. monetary policy autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
