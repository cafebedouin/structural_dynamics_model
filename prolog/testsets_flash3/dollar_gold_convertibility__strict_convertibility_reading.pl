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
 *   This constraint story models the dollar-gold convertibility under the
 *   Bretton Woods system (1944-1971) as a strict legal obligation,
 *   specifically from the perspective that Article IV of the IMF Articles of
 *   Agreement imposed a binding constraint on U.S. monetary policy. This
 *   reading emphasizes the U.S. as a constrained issuer, with creditor
 *   nations holding enforceable claims on its gold reserves. The metrics
 *   reflect a system that became increasingly extractive of U.S. policy
 *   autonomy over time, leading to the eventual collapse of the system.
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
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Dollar-Gold Convertibility (Strict Legal Obligation Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '1815659b-dc6b-4894-ab7f-f6d65e763541').
narrative_ontology:cs_kernel_codification('1815659b-dc6b-4894-ab7f-f6d65e763541', formalized).
narrative_ontology:cs_authority_grounding('1815659b-dc6b-4894-ab7f-f6d65e763541', lineage).
narrative_ontology:cs_interpretation_layer_present('1815659b-dc6b-4894-ab7f-f6d65e763541').
narrative_ontology:cs_reading_relation('1815659b-dc6b-4894-ab7f-f6d65e763541', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('1815659b-dc6b-4894-ab7f-f6d65e763541', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('1815659b-dc6b-4894-ab7f-f6d65e763541', foundational, article_iv_binding_legal_obligation).
narrative_ontology:cs_axiom_status(article_iv_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1815659b-dc6b-4894-ab7f-f6d65e763541', article_iv_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('1815659b-dc6b-4894-ab7f-f6d65e763541', foundational, gold_as_ultimate_monetary_anchor).
narrative_ontology:cs_axiom_status(gold_as_ultimate_monetary_anchor, holdable).
narrative_ontology:cs_axiom_grounding('1815659b-dc6b-4894-ab7f-f6d65e763541', gold_as_ultimate_monetary_anchor, conventional).
narrative_ontology:cs_reference_frame('1815659b-dc6b-4894-ab7f-f6d65e763541', bretton_woods_original_intent).
narrative_ontology:cs_drift_state('1815659b-dc6b-4894-ab7f-f6d65e763541', late_bretton_woods_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1815659b-dc6b-4894-ab7f-f6d65e763541', '').
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

% The Federal Reserve and Treasury are legally bound to maintain dollar convertibility to gold at a fixed price, severely limiting their ability to conduct independent monetary policy for domestic economic stabilization. This constraint forces them to prioritize external balance over internal stability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_monetary_policy, payer,
    institutional, biographical, constrained, national).

% Hold dollar reserves with the assurance that they can be converted to gold, giving them leverage over U.S. policy. They benefit from the stability of the international monetary system and the U.S. commitment to convertibility, which underpins their reserve assets.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the perceived stability and reliability of the dollar as a reserve currency, backed by gold. This reduces exchange rate risk and provides a stable store of value, facilitating international trade and investment.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_investors, beneficiary,
    powerful, immediate, mobile, global).

% Bears the costs of constrained monetary policy, which can lead to higher unemployment or slower growth when the U.S. is forced to defend the dollar's gold parity rather than stimulate the economy. The domestic economy has no direct exit from this policy constraint.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerless, biographical, trapped, national).

% Administers the international monetary system, including the rules of convertibility under Article IV. While not directly collecting rents, it enforces the framework that benefits creditor nations and constrains the U.S.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Economists and policymakers who argue that convertibility should be subordinate to domestic economic stability. Their arguments are marginalized by the strict legal interpretation, which prioritizes external obligations.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_reading_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable international monetary system by pegging the world's reserve currency (the dollar) to gold, providing a fixed anchor for exchange rates and fostering confidence in international transactions.
% TRANSFER_FUNCTION: Transfers policy autonomy from the United States (specifically its monetary policy) to the international system, particularly to creditor nations who hold dollar reserves and can demand gold conversion.
% ABSENT_VOICES: Advocates for a more flexible monetary policy, who prioritize domestic employment and growth, are excluded from the decision-making process that upholds strict convertibility. Their concerns are overridden by the perceived international legal obligation.
% DISAPPEARANCE_RATIONALE: If strict convertibility vanished overnight, the U.S. would gain significant monetary policy freedom, potentially leading to a more expansionary stance. Creditor nations would lose their gold claim, likely leading to a re-evaluation of the dollar's reserve status and a search for new international monetary anchors, fundamentally reorganizing global finance.
% FOUNDING_PROBLEM: The post-WWII international monetary system needed stability after the interwar chaos of competitive devaluations and currency blocs. The Bretton Woods system, with dollar-gold convertibility, was designed to provide this stability.
% FOUNDING_PROBLEM_CORROBORATION: While the IMF and creditor nations might argue for the continued relevance of stability, a broad consensus among economists and historians (outside the direct beneficiaries) holds that the original problem of interwar instability was solved, and the strict convertibility rule became an unsustainable constraint on the U.S. by the late 1960s, leading to its eventual collapse in 1971. The Triffin Dilemma, widely corroborated, demonstrated the inherent unsustainability of the system itself.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high and rising because the fixed gold price, combined with increasing global demand for dollars (Triffin Dilemma), forced the U.S. to choose between domestic policy goals and defending the gold parity. Suppression is high because the legal and institutional framework of Bretton Woods, backed by the IMF, actively enforced this convertibility, making exit for the U.S. costly and disruptive. Theater ratio is low as the obligation was genuinely binding until its suspension.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, this was a stable and fair system of international coordination. From the U.S. perspective, particularly as the system matured, it became an increasingly extractive constraint on its sovereignty. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary policy and domestic economy are the primary targets (high d) as they bear the costs of constrained policy. Creditor nations and international investors are beneficiaries (low d) as they gain from the stability and enforceability of the gold standard. The IMF acts as an agenda-setter, enforcing the rules that create this dynamic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_economic_obligation,
    'To what extent was convertibility a strict legal obligation versus an economic necessity for maintaining the dollar''s reserve status?',
    'Analysis of historical legal interpretations by the IMF and U.S. Treasury, compared with economic analyses of the costs of abandoning convertibility at various points in time.',
    'If primarily an economic necessity, the constraint''s ''suppression'' might be lower (more choice, less coercion); if strictly legal, ''suppression'' is higher, reinforcing the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_vs_economic_obligation, conceptual, 'Ambiguity between legal enforceability and market-driven necessity.').

omega_variable(
    triffin_dilemma_impact,
    'How much of the increasing extractiveness was due to the inherent structural flaw (Triffin Dilemma) versus the strict legal interpretation?',
    'Counterfactual modeling: what would the extractiveness trajectory have been if the legal interpretation had been more flexible, given the Triffin Dilemma''s dynamics?',
    'If the Triffin Dilemma was the dominant driver, the ''snare'' classification is robust regardless of legal interpretation; if the strict interpretation amplified the dilemma''s effects, a more flexible reading might have reduced extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_impact, empirical, 'Separating the impact of legal interpretation from systemic economic forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1955, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1955, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1955, 0.7).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(doll_su_t1955, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1955, 0.6).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, international_capital_flows).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, us_balance_of_payments).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'dollar_gold_convertibility' kernel. It focuses on the strict legal interpretation, distinct from 'policy_flexible_reading' (conditional obligation) and 'triffin_structural_reading' (systemic unsustainability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
