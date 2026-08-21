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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-Fiat Transition: Loss of Automatic Monetary Constraint
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story describes the transition from a gold-backed
 *   monetary system to a fiat system, specifically from the perspective that
 *   an automatic, physical constraint on money creation was replaced by a
 *   discretionary, institutional one. The claimed type is 'tangled_rope'
 *   because while the new system offers coordination benefits (monetary
 *   flexibility), it also involves significant asymmetric extraction
 *   (inflationary transfers from savers/creditors to governments/monetary
 *   authorities) and requires active enforcement (legal tender laws, central
 *   bank policy). The metrics reflect a system with high extractiveness and
 *   suppression, where the coordination function is increasingly intertwined
 *   with rent-seeking.
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
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-Fiat Transition: Loss of Automatic Monetary Constraint").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '8ba7a72b-3f3d-4fb9-a626-8c15a36304f4').
narrative_ontology:cs_kernel_codification('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', formalized).
narrative_ontology:cs_authority_grounding('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', extraction).
narrative_ontology:cs_interpretation_layer_present('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4').
narrative_ontology:cs_reading_relation('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', foundational, monetary_policy_requires_discretion).
narrative_ontology:cs_axiom_status(monetary_policy_requires_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', monetary_policy_requires_discretion, instrumental).
narrative_ontology:cs_axiom('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', foundational, physical_limits_are_automatic_constraints).
narrative_ontology:cs_axiom_status(physical_limits_are_automatic_constraints, holdable).
narrative_ontology:cs_axiom_grounding('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', physical_limits_are_automatic_constraints, empirically_contingent).
narrative_ontology:cs_reference_frame('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', gold_standard_automatic_constraint).
narrative_ontology:cs_drift_state('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', post_nixon_shock_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8ba7a72b-3f3d-4fb9-a626-8c15a36304f4', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, savers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretion over money supply and interest rates, no longer bound by physical gold reserves. This allows for counter-cyclical policy and financing government deficits, but also carries the risk of inflation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Benefited from the ability to finance spending through monetary expansion, reducing reliance on direct taxation or bond markets. This increased fiscal flexibility but also removed a natural check on spending.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_governments, beneficiary,
    institutional, biographical, mobile, national).

% Lost the automatic protection against currency debasement that gold convertibility provided. Their claims are now subject to the discretion of monetary authorities, leading to potential erosion of real value through inflation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Bear the risk of inflation eroding the purchasing power of their savings, as monetary policy is no longer automatically constrained by gold reserves. Their options are limited to seeking inflation-hedging assets, which may carry higher risk.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, savers, payer,
    moderate, biographical, constrained, national).

% Analyze the long-term consequences of the transition, debating the trade-offs between monetary flexibility and stability, and the implications for economic cycles and wealth distribution.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international trade and finance by providing a stable, universally accepted medium of exchange and a natural limit on monetary expansion. Its removal necessitated new institutional coordination mechanisms for global monetary policy.
% TRANSFER_FUNCTION: The transition transferred the power to create money and manage its value from an automatic, physically-backed system to a discretionary, institutionally-managed system. This shifted wealth from creditors/savers (who bore inflation risk) to governments/monetary authorities (who gained flexibility).
% ABSENT_VOICES: Advocates for a return to a gold standard or other commodity-backed money, who argue for automatic, non-discretionary monetary policy, are largely excluded from mainstream policy debates, which are dominated by proponents of fiat currency and central bank independence.
% DISAPPEARANCE_RATIONALE: If the fiat monetary system and central bank authority vanished overnight, global financial markets would collapse, trade would cease, and national economies would revert to barter or local, informal currencies, leading to a complete reorganization of economic activity.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on monetary policy, limiting governments' ability to respond to economic crises (e.g., recessions, wars) with fiscal and monetary expansion, and leading to deflationary pressures.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and most mainstream economists attest that the problem of needing flexible monetary policy to manage economic cycles is still live. Critics (e.g., Austrian school economists, gold bugs) argue that the 'problem' was a necessary discipline and its 'solution' created new, worse problems (inflation, moral hazard); their corroboration is from outside the benefiting parties.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high and rising because the discretionary nature of fiat money allows for continuous, albeit often subtle, transfers of wealth through inflation, which disproportionately affects those with fixed incomes or savings. Suppression is also high because the state's monopoly on legal tender and central bank authority effectively suppresses alternatives to fiat currency. Theater ratio is low, as the central bank's functions are largely genuine, even if their outcomes are contested. The shift from a physical constraint to an institutional one means that the 'naturalness' of the constraint is now entirely a matter of policy and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities, the fiat system is a necessary and beneficial coordination mechanism for managing modern economies. From the perspective of creditors and savers, it is a system of continuous, enforced extraction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and sovereign governments are clear beneficiaries, gaining immense flexibility and power over the economy (low directionality). The creditor class and savers are victims, as their wealth is subject to debasement by inflation (high directionality). Economic theorists act as observers, analyzing the system without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate shifted from maintaining currency convertibility to gold (dead) to managing economic stability and growth (live). However, the 'automatic constraint' reading highlights how the original problem (monetary rigidity) was 'solved' by replacing it with a system that introduced new forms of extraction. The persistence of the fiat system is not due to a lack of alternatives being suppressed, but rather the immense institutional power and coordination benefits it provides to its beneficiaries, despite its extractive nature for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_rules_tradeoff,
    'Is the increased monetary discretion afforded by fiat currency a net benefit for economic stability and growth, or does it lead to greater instability and wealth inequality in the long run?',
    'Longitudinal comparative studies of economic performance under gold vs. fiat standards, controlling for other variables; empirical analysis of central bank independence and its effects.',
    'If discretion is found to lead to greater instability, the extractiveness of the fiat system would be re-evaluated as a systemic cost rather than a policy choice, potentially reclassifying it towards a Snare. If it leads to greater stability, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_rules_tradeoff, empirical, 'The fundamental trade-off between discretionary monetary policy and rule-based systems.').

omega_variable(
    natural_vs_institutional_constraint,
    'To what extent was the gold standard a ''natural'' or ''physical'' constraint, versus an institutionally constructed one that merely leveraged a physical commodity?',
    'Historical analysis of the legal and institutional frameworks underpinning the gold standard, examining the role of state enforcement and international agreements in its maintenance.',
    'If the gold standard is re-framed as primarily institutional, the ''automatic constraint'' reading''s premise of a fundamental shift from ''natural'' to ''institutional'' would be weakened, suggesting a continuous institutional evolution rather than a categorical break.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_institutional_constraint, conceptual, 'The conceptual boundary between natural and institutional constraints in monetary history.').

omega_variable(
    inflation_as_tax_vs_policy_tool,
    'Is inflation under a fiat system primarily an implicit tax on savers and creditors, or a necessary and effective policy tool for managing aggregate demand and employment?',
    'Economic modeling of the distributional effects of inflation, analysis of central bank mandates and their actual outcomes, and public discourse analysis on the framing of inflation.',
    'If inflation is predominantly an implicit tax, the extractiveness of the fiat system is confirmed and potentially amplified. If it is primarily a policy tool, the coordination function of the central bank is strengthened, potentially lowering the perceived extractiveness from a systemic perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_as_tax_vs_policy_tool, preference, 'The normative framing of inflation''s role in a fiat economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.05).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.6).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel. It focuses on the replacement of an automatic physical constraint with discretionary central bank authority. Sibling readings ('creditor_discipline_reading', 'composite_overdetermination_reading') offer alternative causal accounts and emphasize different structural deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
