% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Triffin's Dilemma: Inherent Unsustainability of Dollar-Gold Convertibility
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint describes the dollar-gold convertibility as an inherently
 *   unsustainable design flaw, as articulated by Robert Triffin, leading to
 *   an inevitable collapse. It is one reading of the
 *   'dollar_gold_convertibility' kernel, contrasting with
 *   'strict_convertibility_reading' and 'policy_flexible_reading'. The
 *   system, while initially providing coordination, became increasingly
 *   extractive due to its internal contradictions, ultimately collapsing
 *   under the weight of its own design.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.75).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Triffin's Dilemma: Inherent Unsustainability of Dollar-Gold Convertibility").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '43d98a49-c19b-45b1-9ddf-0377d5ca5dbc').
narrative_ontology:cs_kernel_codification('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', formalized).
narrative_ontology:cs_authority_grounding('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', practice).
narrative_ontology:cs_interpretation_layer_present('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc').
narrative_ontology:cs_reading_relation('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', foundational, inherent_dilemma_of_reserve_currency).
narrative_ontology:cs_axiom_status(inherent_dilemma_of_reserve_currency, holdable).
narrative_ontology:cs_axiom_grounding('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', inherent_dilemma_of_reserve_currency, empirically_contingent).
narrative_ontology:cs_axiom('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', secondary, liquidity_confidence_tradeoff_inevitable).
narrative_ontology:cs_axiom_status(liquidity_confidence_tradeoff_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', liquidity_confidence_tradeoff_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('43d98a49-c19b-45b1-9ddf-0377d5ca5dbc', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, global_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining dollar convertibility to gold while simultaneously funding global liquidity through balance of payments deficits. This created an impossible dilemma, forcing difficult policy choices and eventually leading to the suspension of convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, agenda_setter,
    institutional, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, payer).

% Accumulated large dollar reserves, which were increasingly seen as overvalued and subject to inflation, while the gold backing dwindled. Demanding gold would destabilize the system, but not doing so meant accepting a depreciating asset. Their options were limited by the system's structure.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, excluded).

% Administered the Bretton Woods system and provided mechanisms for balance of payments adjustments, but lacked the structural power to resolve the fundamental dilemma of dollar-gold convertibility. Its role was to manage symptoms, not cure the underlying flaw.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, observer).

% Reacted to the growing unsustainability of the system through speculative attacks on the dollar and increased demands for gold, ultimately accelerating the crisis and forcing the suspension of convertibility. They bore the costs of uncertainty and instability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, global_financial_markets, observer,
    powerful, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, global_financial_markets, payer).

% The subsequent international monetary system of floating exchange rates that emerged after the collapse of dollar-gold convertibility. From Triffin's perspective, this regime 'benefited' by being the necessary structural successor to an inherently flawed system, resolving the dilemma by abandoning convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for international monetary stability, fixed exchange rates, and sufficient global liquidity for trade and investment in the post-WWII era.
% TRANSFER_FUNCTION: Transferred seigniorage benefits to the United States (by allowing it to run deficits and issue dollars as the reserve currency) and imposed the costs of an impossible dilemma (liquidity vs. confidence) on both the U.S. and creditor nations.
% ABSENT_VOICES: Advocates for a truly multilateral reserve asset (e.g., an expanded role for Special Drawing Rights) or a flexible exchange rate system were present but marginalized by the Bretton Woods architecture, which prioritized dollar-gold convertibility.
% DISAPPEARANCE_RATIONALE: The suspension of dollar-gold convertibility in 1971 (the 'Nixon Shock') led to the collapse of the Bretton Woods system, a shift to floating exchange rates, and a fundamental reorganization of international monetary relations, demonstrating its critical, albeit flawed, role.
% FOUNDING_PROBLEM: To establish a stable international monetary system after World War II, avoiding the competitive devaluations, trade wars, and monetary instability that characterized the interwar period, and to provide adequate liquidity for global economic growth.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international finance, economists (including Robert Triffin himself), and official records from the Bretton Woods conference and subsequent decades corroborate the initial problem and how the chosen solution created new, ultimately fatal, structural issues. The problem of global liquidity was solved, but at the cost of convertibility's sustainability.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the system forced the US to choose between providing global liquidity (requiring deficits) and maintaining gold convertibility (requiring surpluses), a dilemma that extracted costs from both the US and creditor nations. Suppression is high because the system required active management and political will to maintain the illusion of convertibility despite dwindling gold reserves and growing dollar overhang. Theater ratio increased as the convertibility claim became increasingly performative and less grounded in economic reality, culminating in its suspension. The claimed type is 'snare' because the coordination story (stable exchange rates) ultimately served as a cover for an unsustainable, extractive mechanism that trapped its participants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the US Treasury and creditor nations, the system became increasingly untenable and extractive, forcing difficult choices. From the analytical perspective of Triffin's structural reading, the system was inherently flawed from its inception, destined to collapse, making the eventual floating regime a structural 'beneficiary' of the prior system's unsustainability. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States Treasury, as the issuer of the reserve currency, was both an agenda-setter and a primary payer, trapped by the dilemma it administered. Creditor nations were payers, forced to hold depreciating dollars or risk destabilizing the system by demanding gold. The 'post_bretton_woods_floating_regime' is identified as a beneficiary not because it actively profited from the constraint's operation, but because it emerged as the necessary structural successor, resolving the inherent flaw by abandoning convertibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_policy_failure,
    'Was the unsustainability of dollar-gold convertibility truly inherent to its design (Triffin''s view), or could different policy choices (e.g., earlier revaluation of gold, stricter fiscal discipline by the US) have prolonged or saved the system?',
    'Counterfactual historical analysis, economic modeling of alternative policy paths, and re-examination of contemporary policy debates.',
    'If policy choices were decisive, the constraint''s extractiveness might be re-attributed more to specific actors'' decisions rather than structural inevitability, potentially shifting its classification towards a Tangled Rope or even a degraded Rope. If inherent, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_policy_failure, empirical, 'Distinguishing between structural design flaws and policy implementation failures in the collapse of convertibility.').

omega_variable(
    beneficiary_of_collapse_vs_operation,
    'Is the ''post_bretton_woods_floating_regime'' a true beneficiary of the *constraint''s operation*, or merely the beneficiary of its *collapse* and the subsequent systemic revision?',
    'Conceptual clarification of ''beneficiary'' in the context of systemic transitions: if the ''benefit'' is solely the resolution of a prior problem, it''s a beneficiary of the transition, not the constraint itself. This omega acknowledges the analytical distinction.',
    'If the floating regime is not considered a beneficiary of the constraint''s operation, the constraint''s classification as a Snare (pure extraction) is strengthened, as there would be no active beneficiary during its operational phase. If it is, the classification might lean towards a Tangled Rope (coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_of_collapse_vs_operation, conceptual, 'Clarifying the nature of benefit in a system designed for eventual collapse.').

omega_variable(
    triffin_dilemma_empirical_status,
    'To what extent does the Triffin Dilemma (the conflict between a reserve currency''s role in providing liquidity and maintaining confidence) remain empirically relevant for contemporary reserve currencies?',
    'Ongoing economic analysis of the US dollar''s role in the current international monetary system, including studies on global liquidity provision, balance of payments, and financial stability.',
    'If the dilemma is still highly relevant, it reinforces the ''holdable'' status of the foundational axiom. If structural changes (e.g., deep financial markets, central bank swap lines) have fundamentally altered the dilemma''s dynamics, it could challenge the axiom''s contemporary empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_empirical_status, empirical, 'Assessing the contemporary empirical validity of the Triffin Dilemma.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.45).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.55).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.6).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.75).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.65).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
