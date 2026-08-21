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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Dollar-Gold Convertibility (Strict Legal Obligation Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint story models the strict convertibility reading of Article
 *   IV of the IMF Articles of Agreement, which legally bound the U.S. dollar
 *   to gold at a fixed price from 1944 to 1971. Under this reading, the
 *   obligation was paramount, severely constraining U.S. monetary policy and
 *   prioritizing external balance over domestic economic needs. This
 *   perspective views the U.S. as a victim of its own commitment, with
 *   creditor nations as beneficiaries holding enforceable claims.
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
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Dollar-Gold Convertibility (Strict Legal Obligation Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '2a326e0b-2130-4dae-a7ea-e103148aa199').
narrative_ontology:cs_kernel_codification('2a326e0b-2130-4dae-a7ea-e103148aa199', formalized).
narrative_ontology:cs_authority_grounding('2a326e0b-2130-4dae-a7ea-e103148aa199', lineage).
narrative_ontology:cs_interpretation_layer_present('2a326e0b-2130-4dae-a7ea-e103148aa199').
narrative_ontology:cs_reading_relation('2a326e0b-2130-4dae-a7ea-e103148aa199', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('2a326e0b-2130-4dae-a7ea-e103148aa199', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('2a326e0b-2130-4dae-a7ea-e103148aa199', foundational, convertibility_as_absolute_legal_duty).
narrative_ontology:cs_axiom_status(convertibility_as_absolute_legal_duty, holdable).
narrative_ontology:cs_axiom_grounding('2a326e0b-2130-4dae-a7ea-e103148aa199', convertibility_as_absolute_legal_duty, deontological).
narrative_ontology:cs_axiom('2a326e0b-2130-4dae-a7ea-e103148aa199', secondary, external_balance_prioritizes_domestic_policy).
narrative_ontology:cs_axiom_status(external_balance_prioritizes_domestic_policy, holdable).
narrative_ontology:cs_axiom_grounding('2a326e0b-2130-4dae-a7ea-e103148aa199', external_balance_prioritizes_domestic_policy, conventional).
narrative_ontology:cs_reference_frame('2a326e0b-2130-4dae-a7ea-e103148aa199', bretton_woods_original_intent).
narrative_ontology:cs_drift_state('2a326e0b-2130-4dae-a7ea-e103148aa199', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('2a326e0b-2130-4dae-a7ea-e103148aa199', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_investors).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states_monetary_policy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Federal Reserve and Treasury are bound by the obligation to maintain the dollar's convertibility to gold at a fixed price, severely limiting their ability to conduct independent monetary policy for domestic economic stabilization. This creates a constant tension between external convertibility and internal policy goals.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_monetary_policy, payer,
    institutional, biographical, constrained, national).

% Hold dollar reserves with the assurance that they can convert them to gold at a fixed rate, providing a stable store of value and a check on U.S. monetary expansion. They benefit from the U.S. constraint, which underpins the value of their dollar holdings.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the perceived stability and reliability of the dollar as a reserve currency, backed by gold convertibility. This reduces exchange rate risk and provides a predictable environment for cross-border capital flows.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_investors, beneficiary,
    organized, immediate, mobile, global).

% Bears the costs of constrained monetary policy, which may lead to higher unemployment or slower growth when domestic needs conflict with the convertibility obligation. The economy is subject to external pressures without direct recourse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerless, biographical, trapped, national).

% Administers the Bretton Woods system, including Article IV, and monitors member compliance. While not directly enforcing convertibility, its institutional framework and surveillance mechanisms reinforce the legal obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable international monetary system by pegging major currencies to the U.S. dollar, which is in turn convertible to gold, providing a fixed exchange rate regime and confidence in the global financial order.
% TRANSFER_FUNCTION: Transfers policy autonomy from the U.S. monetary authorities to the international system, particularly to creditor nations, in exchange for maintaining the dollar's role as the global reserve currency. It also transfers real resources (gold) from the U.S. to other nations upon demand.
% ABSENT_VOICES: Advocates for a more flexible, domestically-oriented U.S. monetary policy, who would argue that the convertibility obligation unduly sacrifices internal stability for external balance. Also, developing nations who might argue the system disproportionately benefits developed creditor nations.
% DISAPPEARANCE_RATIONALE: If the strict convertibility obligation vanished overnight, the international monetary system would immediately lose its anchor. Exchange rates would float freely, dollar reserves would lose their gold backing, and the U.S. would gain significant monetary policy independence, leading to a fundamental reorganization of global finance.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, unstable exchange rates, and a lack of international monetary cooperation, leading to economic instability and trade wars.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international finance and economists generally agree on the problem of interwar monetary instability. While the specific solution (Bretton Woods) is debated, the need for international monetary coordination remains a live concern, corroborated by academic literature and international financial institutions.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.85) because the U.S. sacrificed significant monetary policy autonomy, leading to domestic economic costs (e.g., 'gold drain' crises). Suppression is also high (0.75) as the U.S. was actively compelled by international pressure and the threat of gold outflows to maintain convertibility, despite domestic policy desires. Theater ratio is low (0.1) because the obligation was genuinely binding and actively enforced, not merely performative. The accessibility collapse is high (0.8) because the U.S. had few viable alternatives to maintaining convertibility without undermining the entire Bretton Woods system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, the constraint was a vital mechanism for international monetary stability and a safeguard for their dollar holdings. From the U.S. monetary policy perspective, it was an increasingly burdensome obligation that extracted policy space and imposed domestic costs. The engine will compute these divergent classifications based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary policy and domestic economy are clear targets (payers) as they bear the direct costs of constrained policy. Creditor nations and international investors are beneficiaries, gaining stability and a reliable store of value. The IMF acts as an agenda-setter, reinforcing the legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights the inherent tension and eventual unsustainability of the strict convertibility obligation, which ultimately led to its collapse in 1971. It prevents mislabeling the constraint as a simple coordination mechanism by emphasizing the asymmetric extraction of policy space from the U.S. and the identifiable victims of this arrangement. The 'dead' status of the founding problem (interwar instability) for the U.S. domestic economy, combined with the 'world_rearranges' verdict, signals a Mandatrophy condition where the constraint's original purpose was no longer served for a key party, yet it persisted due to external enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_priority_ambiguity,
    'Was the U.S. truly legally bound to strict convertibility, or did it retain implicit flexibility to prioritize domestic policy, as argued by the ''policy_flexible_reading''?',
    'Analysis of declassified government documents and internal policy debates from the period, focusing on instances where domestic policy was explicitly sacrificed for convertibility, or vice-versa.',
    'If significant flexibility existed, the extractiveness from U.S. policy space would be lower, potentially reclassifying the constraint from a Snare to a Tangled Rope or even a Rope, depending on the degree of actual policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_priority_ambiguity, empirical, 'Ambiguity regarding the actual degree of U.S. policy autonomy under convertibility.').

omega_variable(
    triffin_dilemma_impact,
    'To what extent was the constraint''s eventual collapse due to the inherent ''Triffin Dilemma'' (the conflict between providing international liquidity and maintaining convertibility), rather than a failure of U.S. commitment?',
    'Economic modeling and historical counterfactuals exploring the system''s stability under different U.S. policy choices, and the role of structural factors versus policy decisions.',
    'If the Triffin Dilemma was the dominant factor, the constraint might be reclassified as a Mountain (an irreducible structural limit) from the perspective of the system as a whole, rather than a Snare imposed on the U.S. This would shift the locus of extraction from U.S. policy to the system''s design itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_impact, conceptual, 'Whether the constraint''s unsustainability was a structural feature of the system or a policy choice.').

omega_variable(
    reading_framing_impact,
    'This constraint is a ''strict_convertibility_reading'' of the ''dollar_gold_convertibility'' kernel. How would the classification change if a ''policy_flexible_reading'' or ''triffin_structural_reading'' were adopted?',
    'By generating separate constraint stories for each sibling reading and comparing their computed classifications and stakeholder directionalities.',
    'The ''policy_flexible_reading'' would likely show lower extractiveness from the U.S. and potentially reclassify as a Tangled Rope or Rope. The ''triffin_structural_reading'' might emphasize the systemic nature of the constraint, potentially leading to a Mountain classification for the system as a whole, with the U.S. as a victim of an inherent design flaw rather than a directly extracted party.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of alternative kernel readings on constraint classification.').


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
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.09).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.15).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.75).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.78).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.82).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.87).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.65).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.72).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_exchange_rate_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dollar_gold_convertibility' kernel. It emphasizes the binding legal obligation and the resulting extraction of U.S. policy space. Sibling readings (policy_flexible_reading, triffin_structural_reading) offer alternative interpretations of the same historical period and legal framework, with different implications for extractiveness and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
