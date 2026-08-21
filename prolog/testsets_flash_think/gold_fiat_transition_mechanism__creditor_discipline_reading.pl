% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Loss of Gold-Backed Creditor Discipline (Post-Bretton Woods)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the 'gold-fiat transition mechanism'
 *   kernel from the perspective of 'creditor discipline.' It focuses on the
 *   structural shift that eliminated the power of creditor nations to enforce
 *   fiscal discipline on debtor nations through the threat of gold
 *   redemption, thereby enabling greater fiscal flexibility for debtor
 *   nations, especially the reserve currency issuer. This reading highlights
 *   the geopolitical power transfer from creditors to the reserve currency
 *   issuer, framing the post-Bretton Woods fiat system as a mechanism that
 *   coordinates global finance but with significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.9).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Loss of Gold-Backed Creditor Discipline (Post-Bretton Woods)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, 'eb2a28ce-e73f-4383-97ed-e44c54232a96').
narrative_ontology:cs_kernel_codification('eb2a28ce-e73f-4383-97ed-e44c54232a96', formalized).
narrative_ontology:cs_authority_grounding('eb2a28ce-e73f-4383-97ed-e44c54232a96', extraction).
narrative_ontology:cs_interpretation_layer_present('eb2a28ce-e73f-4383-97ed-e44c54232a96').
narrative_ontology:cs_reading_relation('eb2a28ce-e73f-4383-97ed-e44c54232a96', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb2a28ce-e73f-4383-97ed-e44c54232a96', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('eb2a28ce-e73f-4383-97ed-e44c54232a96', foundational, creditor_veto_is_extraction).
narrative_ontology:cs_axiom_status(creditor_veto_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('eb2a28ce-e73f-4383-97ed-e44c54232a96', creditor_veto_is_extraction, conventional).
narrative_ontology:cs_axiom('eb2a28ce-e73f-4383-97ed-e44c54232a96', foundational, fiscal_flexibility_is_sovereignty).
narrative_ontology:cs_axiom_status(fiscal_flexibility_is_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('eb2a28ce-e73f-4383-97ed-e44c54232a96', fiscal_flexibility_is_sovereignty, deontological).
narrative_ontology:cs_reference_frame('eb2a28ce-e73f-4383-97ed-e44c54232a96', post_bretton_woods_fiat_system).
narrative_ontology:cs_drift_state('eb2a28ce-e73f-4383-97ed-e44c54232a96', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eb2a28ce-e73f-4383-97ed-e44c54232a96', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, global_capital_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant fiscal and monetary policy flexibility, no longer constrained by gold redemption threats. Benefits from the ability to run larger deficits and manage its currency without external discipline, effectively shifting costs to others.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Gained increased fiscal flexibility, allowing them to pursue domestic policy goals without immediate balance-of-payments crises or the threat of gold redemption. However, they remain subject to market discipline and the reserve currency issuer's policies.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    organized, generational, constrained, global).

% Lost their primary leverage over debtor nations, which was the threat of gold redemption. Their ability to impose fiscal discipline on others was significantly diminished, leading to a geopolitical power shift away from them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    institutional, generational, constrained, global).

% Their role shifted from managing a fixed-exchange-rate system to overseeing a more flexible, fiat-based system. They gained new tools for macroeconomic management but also faced new challenges in maintaining global financial stability without the 'automatic' discipline of gold.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, observer).

% Benefited from increased liquidity and flexibility in international finance, allowing for greater capital flows and investment opportunities. However, they also faced new forms of risk related to floating exchange rates and sovereign debt.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, global_capital_markets, beneficiary,
    powerful, immediate, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated a more flexible global monetary system, allowing nations to manage domestic economies without immediate gold-reserve constraints, thereby coordinating national fiscal and monetary policies within a broader, less rigid international framework.
% TRANSFER_FUNCTION: Transferred fiscal and monetary policy autonomy and geopolitical leverage from creditor nations (who previously held the gold redemption threat) to debtor nations, particularly the reserve currency issuer, enabling greater domestic policy space.
% ABSENT_VOICES: Advocates for a return to a gold standard or other forms of hard monetary discipline, who would argue that the current system lacks accountability and promotes excessive debt and inflation. Their voices are largely marginalized in mainstream policy discourse.
% DISAPPEARANCE_RATIONALE: The global financial system is fundamentally built on fiat currency and flexible exchange rates, with central banks as primary actors. Reverting to a gold-backed system would cause massive economic disruption, re-introduce balance-of-payments crises, and fundamentally alter international power dynamics, requiring a complete reorganization of global finance.
% FOUNDING_PROBLEM: The Bretton Woods system's fixed exchange rates and gold convertibility imposed rigid balance-of-payments discipline, limiting national fiscal flexibility and leading to recurrent crises, particularly for debtor nations. The system also created an 'exorbitant privilege' for the US but constrained its own policy choices.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians generally agree on the problems of the Bretton Woods system's rigidity. However, whether the solution (full fiat) was optimal or merely shifted problems is contested. Proponents of fiat point to increased stability and growth; critics point to increased debt and inflation, with testimony from various economic schools and historical analyses supporting different views.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is high (0.85) because the mechanism fundamentally shifted power and resources (fiscal flexibility, seigniorage) from creditor nations to debtor nations. Suppression is very high (0.90) as the gold-backed system and its associated disciplinary mechanisms were actively dismantled and are continuously suppressed by the current fiat regime. Accessibility collapse is also high (0.90) because the alternative of gold-backed discipline is effectively foreclosed. Resistance is moderate (0.70) reflecting the ongoing, albeit often marginalized, calls for a return to hard money or alternative disciplinary mechanisms. Theater ratio is low (0.10) as the transition was a fundamental structural change, not a performative one.
 *
 * PERSPECTIVAL GAP:
 *   Creditor nations would perceive this transition as a loss of essential discipline and a move towards financial instability, experiencing it as a snare. Debtor nations, particularly the reserve currency issuer, would perceive it as a necessary evolution towards greater sovereignty and economic flexibility, experiencing it as a rope or even an eliminated constraint. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve currency issuer and other debtor nations are the primary beneficiaries, gaining fiscal flexibility and geopolitical leverage (low directionality). Creditor nations are the primary victims, losing their disciplinary power and seeing their leverage diminished (high directionality). International financial institutions play a dual role, adapting to and managing the new system, which benefits them by expanding their mandate, but also imposes new responsibilities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_creditor_discipline,
    'Is the primary structural change of the gold-fiat transition the elimination of creditor veto power, or is it better understood as the removal of an automatic physical constraint or a composite of multiple factors?',
    'Comparative historical analysis of policy outcomes and power dynamics in different monetary regimes, specifically isolating the impact of creditor leverage vs. physical reserve limits.',
    'If creditor discipline is the dominant factor, this reading''s high extractiveness and power shift analysis are validated. If other factors dominate, the classification might shift towards a more ''automatic'' or ''overdetermined'' type, reducing the perceived extraction from creditor nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_creditor_discipline, conceptual, 'Ambiguity in the primary structural change of the gold-fiat transition.').

omega_variable(
    fiscal_flexibility_vs_discipline_tradeoff,
    'To what extent has the increased fiscal flexibility for debtor nations, enabled by the loss of gold-backed discipline, led to sustainable economic growth versus increased sovereign debt and inflationary pressures?',
    'Longitudinal econometric studies comparing macroeconomic performance indicators (GDP growth, inflation, debt-to-GDP ratios) across nations and over time, controlling for other policy variables.',
    'If flexibility primarily led to unsustainable debt and inflation, the ''beneficiary'' aspect for debtor nations is undermined, potentially increasing their effective extraction. If it led to stable growth, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_flexibility_vs_discipline_tradeoff, empirical, 'The actual economic consequences of increased fiscal flexibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(gold_tr_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1981, 0.1).
narrative_ontology:measurement(gold_tr_t1991, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gold_tr_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(gold_tr_t2011, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2011, 0.1).
narrative_ontology:measurement(gold_tr_t2021, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2021, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.8).
narrative_ontology:measurement(gold_be_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1981, 0.82).
narrative_ontology:measurement(gold_be_t1991, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1991, 0.83).
narrative_ontology:measurement(gold_be_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2001, 0.84).
narrative_ontology:measurement(gold_be_t2011, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2011, 0.85).
narrative_ontology:measurement(gold_be_t2021, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2021, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(gold_su_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1981, 0.87).
narrative_ontology:measurement(gold_su_t1991, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1991, 0.88).
narrative_ontology:measurement(gold_su_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2001, 0.89).
narrative_ontology:measurement(gold_su_t2011, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2011, 0.9).
narrative_ontology:measurement(gold_su_t2021, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2021, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, global_debt_accumulation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_hegemony).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel, focusing on the shift in creditor-debtor power dynamics. It is linked to sibling readings that emphasize the removal of physical constraints or a multi-causal overdetermination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
