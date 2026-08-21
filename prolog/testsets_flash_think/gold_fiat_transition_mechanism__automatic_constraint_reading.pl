% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-Fiat Transition: Discretionary Central Bank Authority
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story instantiates the `automatic_constraint_reading` of
 *   the `gold_fiat_transition_mechanism` kernel. This reading emphasizes the
 *   direct replacement of a physical, automatic limit on money creation (gold
 *   reserves) with a discretionary, institutional authority (central banks).
 *   The transition shifted the nature of the constraint from an external,
 *   'natural' limit to an internal, policy-driven one. Sibling readings
 *   include the `creditor_discipline_reading` (focus on power shift from
 *   creditors) and the `composite_overdetermination_reading` (focus on
 *   multiple converging factors).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.75).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-Fiat Transition: Discretionary Central Bank Authority").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '72e1cab5-f22b-444f-b59a-2424a31d268a').
narrative_ontology:cs_kernel_codification('72e1cab5-f22b-444f-b59a-2424a31d268a', formalized).
narrative_ontology:cs_authority_grounding('72e1cab5-f22b-444f-b59a-2424a31d268a', lineage).
narrative_ontology:cs_interpretation_layer_present('72e1cab5-f22b-444f-b59a-2424a31d268a').
narrative_ontology:cs_reading_relation('72e1cab5-f22b-444f-b59a-2424a31d268a', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('72e1cab5-f22b-444f-b59a-2424a31d268a', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('72e1cab5-f22b-444f-b59a-2424a31d268a', foundational, money_supply_requires_discretion).
narrative_ontology:cs_axiom_status(money_supply_requires_discretion, holdable).
narrative_ontology:cs_axiom_grounding('72e1cab5-f22b-444f-b59a-2424a31d268a', money_supply_requires_discretion, instrumental).
narrative_ontology:cs_axiom('72e1cab5-f22b-444f-b59a-2424a31d268a', secondary, physical_limits_are_suboptimal).
narrative_ontology:cs_axiom_status(physical_limits_are_suboptimal, holdable).
narrative_ontology:cs_axiom_grounding('72e1cab5-f22b-444f-b59a-2424a31d268a', physical_limits_are_suboptimal, empirically_contingent).
narrative_ontology:cs_reference_frame('72e1cab5-f22b-444f-b59a-2424a31d268a', flexible_stabilizing_authority).
narrative_ontology:cs_drift_state('72e1cab5-f22b-444f-b59a-2424a31d268a', post_global_financial_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('72e1cab5-f22b-444f-b59a-2424a31d268a', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, government_treasuries).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, savers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained the discretion to manage money supply and interest rates, allowing for flexible responses to economic conditions. They administer the fiat system and benefit from the flexibility it provides to achieve policy goals.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from increased fiscal flexibility, no longer constrained by gold reserves to fund expenditures or manage national debt. They can coordinate with central banks on economic policy.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, government_treasuries, beneficiary,
    institutional, biographical, mobile, national).

% Lost the automatic protection against currency debasement that the gold standard offered. Their fixed-income assets are now vulnerable to inflation driven by discretionary monetary policy, leading to a transfer of real wealth.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Bear the risk of inflation eroding the value of their savings, as monetary policy is no longer automatically constrained by a physical commodity. Their options are limited to seeking inflation-hedging assets, which may not be accessible or risk-free.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, savers, payer,
    moderate, biographical, constrained, national).

% Argue for a return to commodity-backed money and are largely excluded from mainstream monetary policy debates. They would object to the discretionary nature of the current system and its potential for debasement.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, advocates_for_gold_standard, excluded,
    organized, generational, constrained, national).

% Analyze the structural shift from automatic to discretionary monetary constraints, its implications for power dynamics, and its long-term economic consequences. They provide critical analysis but do not directly participate in policy setting.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, political_economists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible money supply that can be adjusted by central banks to stabilize the economy, manage inflation, and respond to financial crises, thereby coordinating economic activity without the rigidity of a fixed commodity standard.
% TRANSFER_FUNCTION: Transfers the ultimate control over money creation and its value from an automatic, physical limit (gold reserves) to a discretionary, institutional authority (central banks), which can lead to a transfer of real wealth from holders of fixed-value assets to those who can create or access new money.
% ABSENT_VOICES: Advocates for a return to a gold standard or other forms of commodity-backed money are largely excluded from mainstream policy discourse. They would argue for automatic, non-discretionary monetary rules to protect against inflation and government overspending.
% DISAPPEARANCE_RATIONALE: If central bank discretionary authority vanished overnight, the global financial system would face immediate collapse due to a lack of liquidity management, lender-of-last-resort functions, and inflation control. A rapid re-establishment of some form of monetary authority or a chaotic return to commodity money would be inevitable.
% FOUNDING_PROBLEM: The gold standard imposed rigid limits on money supply, leading to deflationary pressures, economic instability, and an inability for governments to respond effectively to economic shocks or fund necessary public services.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and most mainstream economists corroborate that the gold standard's rigidity was a significant problem that fiat money resolved. However, gold standard advocates and some heterodox economists dispute this, arguing that the problems were misdiagnosed or replaced with new, more severe ones under fiat.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because discretionary monetary policy, while offering flexibility, inherently allows for wealth transfers through inflation, impacting creditors and savers. Suppression is also high, as the central bank's authority is legally mandated and enforced, with few viable alternatives to the national currency. The theater ratio is low, reflecting that central bank operations are genuinely functional, not merely performative, even if their outcomes are contested. Accessibility collapse is high because the automatic gold constraint is entirely removed, and the fiat system is pervasive. Resistance is moderate, reflecting ongoing academic and political debates about central bank independence and the merits of fiat money.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities, the shift to fiat money represents a necessary evolution to a more flexible and effective coordination mechanism for economic stability. From the perspective of creditors and savers, it represents a loss of automatic protection and an increase in extraction risk. The engine's computation of per-seat classifications will highlight this divergence, showing the same institutional structure as a benefit for some and a cost for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and government treasuries are beneficiaries, gaining significant discretion and fiscal flexibility. The creditor class and savers are victims, bearing the costs of potential inflation and losing automatic protection against currency debasement. Advocates for a gold standard are excluded, as their preferred system is no longer operative and their arguments are marginalized in policy circles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately captured as a single ''automatic constraint'' reading, or is it better understood through a composite lens?',
    'Comparative analysis with sibling readings: if the ''composite_overdetermination_reading'' provides a more complete causal account without losing explanatory power, the single-mechanism framing may be too narrow.',
    'If the composite reading is superior, this constraint''s classification might be re-evaluated as a component of a larger, multi-causal shift, potentially altering its network relationships and the perceived agency of its stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the gold_fiat_transition_mechanism kernel, emphasizing the replacement of an automatic constraint.').

omega_variable(
    creditor_power_shift_ambiguity,
    'To what extent did the gold-fiat transition primarily represent a shift in power from the creditor class to debtor nations/governments, as argued by the ''creditor_discipline_reading''?',
    'Historical analysis of balance-of-payments crises and sovereign debt defaults before and after the transition, focusing on the leverage of creditors versus national governments.',
    'If the creditor discipline argument is strongly corroborated, the ''creditor_class'' stakeholder''s ''payer'' role might be re-emphasized as a loss of structural power, and the ''government_treasuries'' ''beneficiary'' role as a gain of structural power, potentially increasing the perceived extractiveness from the creditor seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_power_shift_ambiguity, empirical, 'Ambiguity regarding the primary driver of the gold-fiat transition: automatic constraint replacement vs. creditor power shift.').

omega_variable(
    discretion_vs_rules_efficacy,
    'Is discretionary central bank authority inherently more effective at achieving economic stability than rule-based, automatic monetary systems?',
    'Long-term empirical studies comparing economic stability, inflation, and growth outcomes under discretionary fiat regimes versus historical or hypothetical rule-based systems.',
    'If rule-based systems are shown to be equally or more effective, the ''instrumental'' grounding of the ''money_supply_requires_discretion'' axiom would be challenged, potentially weakening the legitimacy claims of the current constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discretion_vs_rules_efficacy, empirical, 'Debate over the efficacy of discretionary vs. rule-based monetary policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(gold_tr_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1981, 0.09).
narrative_ontology:measurement(gold_tr_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gold_tr_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2001, 0.11).
narrative_ontology:measurement(gold_tr_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2011, 0.12).
narrative_ontology:measurement(gold_tr_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2021, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.75).
narrative_ontology:measurement(gold_be_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1981, 0.8).
narrative_ontology:measurement(gold_be_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1991, 0.78).
narrative_ontology:measurement(gold_be_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(gold_be_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2011, 0.88).
narrative_ontology:measurement(gold_be_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2021, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.7).
narrative_ontology:measurement(gold_su_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1981, 0.72).
narrative_ontology:measurement(gold_su_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1991, 0.73).
narrative_ontology:measurement(gold_su_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement(gold_su_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2011, 0.76).
narrative_ontology:measurement(gold_su_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2021, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel, focusing on the replacement of an automatic physical constraint with discretionary institutional authority. It is linked to 'gold_fiat_transition_mechanism__creditor_discipline_reading' and 'gold_fiat_transition_mechanism__composite_overdetermination_reading' as sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
