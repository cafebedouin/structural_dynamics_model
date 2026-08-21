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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin Dilemma Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint story analyzes dollar-gold convertibility through the
 *   lens of the Triffin Dilemma, which posits that a national currency cannot
 *   sustainably serve as an international reserve currency under a fixed
 *   exchange rate system. The constraint is viewed as an inherently
 *   unsustainable design flaw that extracted costs from both the U.S.
 *   (through gold drain and inflationary pressure) and creditor nations
 *   (through the risk of dollar devaluation), ultimately requiring systemic
 *   revision. The claimed type is 'snare' because the system, despite its
 *   initial coordination function, became a mechanism of unavoidable
 *   extraction due to its structural contradiction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.9).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin Dilemma Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '82b58658-18e8-49fd-9eba-1c947139b6ed').
narrative_ontology:cs_kernel_codification('82b58658-18e8-49fd-9eba-1c947139b6ed', formalized).
narrative_ontology:cs_authority_grounding('82b58658-18e8-49fd-9eba-1c947139b6ed', lineage).
narrative_ontology:cs_interpretation_layer_present('82b58658-18e8-49fd-9eba-1c947139b6ed').
narrative_ontology:cs_reading_relation('82b58658-18e8-49fd-9eba-1c947139b6ed', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('82b58658-18e8-49fd-9eba-1c947139b6ed', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_axiom('82b58658-18e8-49fd-9eba-1c947139b6ed', foundational, impossible_trilemma_inevitability).
narrative_ontology:cs_axiom_status(impossible_trilemma_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('82b58658-18e8-49fd-9eba-1c947139b6ed', impossible_trilemma_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('82b58658-18e8-49fd-9eba-1c947139b6ed', foundational, structural_unsustainability).
narrative_ontology:cs_axiom_status(structural_unsustainability, holdable).
narrative_ontology:cs_axiom_grounding('82b58658-18e8-49fd-9eba-1c947139b6ed', structural_unsustainability, empirically_contingent).
narrative_ontology:cs_reference_frame('82b58658-18e8-49fd-9eba-1c947139b6ed', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('82b58658-18e8-49fd-9eba-1c947139b6ed', pre_nixon_shock_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('82b58658-18e8-49fd-9eba-1c947139b6ed', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_treasury).
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

% As the issuer of the reserve currency, the U.S. Treasury managed the convertibility of dollars to gold. Under the Triffin dilemma, it faced the impossible choice between maintaining convertibility (risking gold drain) and providing global liquidity (risking inflation and loss of confidence). It was a victim of the system's inherent flaw.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_treasury, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, us_treasury, payer).

% Nations accumulating large dollar reserves. They benefited from stable exchange rates but faced the risk of dollar devaluation or the inability to convert their dollars to gold, effectively financing U.S. deficits. They were victims of the system's structural instability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    institutional, biographical, constrained, global).

% The institution tasked with overseeing the Bretton Woods system. It observed the growing imbalances and the unfolding Triffin dilemma, attempting to propose solutions within the existing framework, but ultimately unable to resolve the fundamental contradiction.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, agenda_setter).

% Participants in the international monetary system who faced increasing uncertainty and instability as the Triffin dilemma intensified, leading to speculative attacks on the dollar and eventual collapse of convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, global_financial_markets, payer,
    organized, immediate, constrained, global).

% Economists and policymakers who argued for a system of floating exchange rates as a more sustainable alternative to fixed convertibility, but whose proposals were largely outside the dominant Bretton Woods framework until its collapse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, advocates_for_flexible_exchange_rates, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a stable international monetary system after WWII, providing fixed exchange rates and a reliable reserve currency (the dollar convertible to gold) to facilitate global trade and investment.
% TRANSFER_FUNCTION: Initially transferred seigniorage benefits to the U.S. and exchange rate stability to creditor nations. Over time, it transferred the costs of the impossible trilemma (inflationary pressure in the U.S., gold drain, and the risk of dollar devaluation for creditors) to both primary parties.
% ABSENT_VOICES: Advocates for a truly international reserve asset (e.g., a global central bank currency) or for flexible exchange rates were largely excluded from the core decision-making that maintained the Bretton Woods system, despite their warnings about its inherent flaws.
% DISAPPEARANCE_RATIONALE: The constraint of dollar-gold convertibility disappeared in 1971, leading to the collapse of the Bretton Woods system and a fundamental reorganization of the global monetary order towards floating exchange rates. This was a systemic shift, not a minor adjustment.
% FOUNDING_PROBLEM: To prevent a return to the monetary chaos, competitive devaluations, and trade protectionism that characterized the interwar period, and to establish a stable foundation for post-WWII economic reconstruction and growth.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Bretton Woods conference, post-WWII economic analyses, and statements from key policymakers (e.g., Harry Dexter White, John Maynard Keynes) corroborate the initial problem. The subsequent economic history and the eventual collapse of the system, as analyzed by economists like Robert Triffin, confirm the founding problem was superseded by the system's own structural flaws.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high and rising because the structural contradiction of the Triffin Dilemma meant that the system was inherently extracting stability from its participants at the cost of its own long-term viability. Suppression is high because major economies were effectively trapped within the Bretton Woods framework, with no viable alternatives for international monetary coordination until the system's collapse. Theater ratio is low, as the system was genuinely functional for a period, but the underlying structural flaw meant its eventual failure was inevitable, not merely a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Triffin structural reading, the constraint was a snare for all major participants, regardless of their initial perceived benefits. Other readings might emphasize the coordination benefits or policy choices, but this reading focuses on the unavoidable structural extraction. The engine's classification will reflect this inherent unsustainability and extraction from all parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the U.S. Treasury and creditor nations are identified as victims because the Triffin Dilemma trapped them in a system where their short-term benefits were undermined by long-term structural costs. The U.S. faced the dilemma of maintaining convertibility while providing global liquidity, leading to gold drains and inflationary pressure. Creditor nations accumulated dollars, facing the risk of devaluation or inability to convert to gold. Neither party could escape the dilemma's extractive logic within the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_timing,
    'Was the Triffin Dilemma an inevitable consequence of the Bretton Woods design from its inception, or did specific U.S. policy choices (e.g., Vietnam War financing) accelerate its manifestation?',
    'Counterfactual historical analysis and economic modeling comparing outcomes under alternative U.S. fiscal and monetary policies during the 1960s.',
    'If inevitable, it strengthens the ''snare'' classification by emphasizing the structural trap. If accelerated by policy, it suggests a ''tangled_rope'' element where policy choices amplified an underlying flaw, rather than the flaw being purely deterministic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_timing, empirical, 'Whether the Triffin Dilemma''s manifestation was purely structural or partly policy-driven.').

omega_variable(
    political_feasibility_of_alternatives,
    'Were politically feasible alternatives to the Bretton Woods system, such as a global reserve currency or immediate flexible exchange rates, genuinely available and considered before the system''s collapse?',
    'Archival research into international negotiations and policy debates of the 1950s and 60s, assessing the political will and technical readiness for systemic reform.',
    'If viable alternatives were politically suppressed, it reinforces the ''snare'' classification by highlighting active coercion. If alternatives were genuinely unfeasible, it underscores the ''mountain-like'' aspect of the structural trap, where no easy policy exit existed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_alternatives, conceptual, 'The political feasibility of alternatives to the Bretton Woods system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.09).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.72).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.78).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.8).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel, each representing a distinct structural interpretation of the Bretton Woods system's core mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
