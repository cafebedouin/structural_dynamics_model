% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: IMF/Creditor Structural Adjustment Conditionalities as Coordination Mechanism
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This story instantiates the creditor-coordination reading of the
 *   structural_adjustment_conditionalities kernel: conditionality is authored
 *   here as a genuine solution to a sovereign commitment problem, not as a
 *   cover story. A finance ministry facing fiscal crisis cannot credibly
 *   promise reform to capital markets without an external monitor verifying
 *   tranche-by-tranche compliance; the IMF/creditor consortium provides that
 *   monitoring function, and the resulting signal restores market access at
 *   lower cost than the counterfactual of unmonitored borrowing or outright
 *   default. Under this reading, the constraint's victims are specifically
 *   inefficient SOEs and patronage networks whose contraction is the intended
 *   correction, not collateral damage to a legitimate social contract — that
 *   latter framing belongs to the sibling debtor_extraction_reading, a
 *   structurally distinct constraint with a different epsilon and different
 *   victim set (public-service users read there as the primary victims, not
 *   incidental ones). The hybrid_selectivity_reading is a third distinct
 *   constraint concerning differential enforcement across debtors of
 *   different geopolitical value; this story does not model selectivity and
 *   treats enforcement as uniform by design.
 *
 * KEY AGENTS:
 *   - imf_creditor_consortium: agenda_setter/beneficiary (institutional/arbitrage) — designs and monitors conditionality, preserves capital base
 *   - debtor_state_finance_ministry: agenda_setter/beneficiary (institutional/constrained) — uses conditionality as domestic leverage device
 *   - future_taxpayers_of_debtor_state: beneficiary (powerless/trapped) — inherits restored fiscal sustainability, generations later
 *   - inefficient_state_owned_enterprises: payer (moderate/constrained) — bears the intended contraction
 *   - public_sector_patronage_networks: payer (organized/constrained) — loses clientelist resource access
 *   - affected_public_service_users: excluded (powerless/trapped) — bears near-term transitional cost without a negotiating seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.38).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.42).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "IMF/Creditor Structural Adjustment Conditionalities as Coordination Mechanism").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '26b6db14-4720-431c-9113-04ee3d958b63').
narrative_ontology:cs_kernel_codification('26b6db14-4720-431c-9113-04ee3d958b63', formalized).
narrative_ontology:cs_authority_grounding('26b6db14-4720-431c-9113-04ee3d958b63', expertise).
narrative_ontology:cs_interpretation_layer_present('26b6db14-4720-431c-9113-04ee3d958b63').
narrative_ontology:cs_reading_relation('26b6db14-4720-431c-9113-04ee3d958b63', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('26b6db14-4720-431c-9113-04ee3d958b63', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('26b6db14-4720-431c-9113-04ee3d958b63', foundational, conditionality_solves_genuine_commitment_problem).
narrative_ontology:cs_axiom_status(conditionality_solves_genuine_commitment_problem, holdable).
narrative_ontology:cs_axiom_grounding('26b6db14-4720-431c-9113-04ee3d958b63', conditionality_solves_genuine_commitment_problem, instrumental).
narrative_ontology:cs_axiom('26b6db14-4720-431c-9113-04ee3d958b63', secondary, targeted_sector_contraction_is_intended_correction_not_harm).
narrative_ontology:cs_axiom_status(targeted_sector_contraction_is_intended_correction_not_harm, holdable).
narrative_ontology:cs_axiom_grounding('26b6db14-4720-431c-9113-04ee3d958b63', targeted_sector_contraction_is_intended_correction_not_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('26b6db14-4720-431c-9113-04ee3d958b63', bretton_woods_conditional_lending_framework).
narrative_ontology:cs_drift_state('26b6db14-4720-431c-9113-04ee3d958b63', post_2008_post_covid_debt_distress_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('26b6db14-4720-431c-9113-04ee3d958b63', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_debtor_state).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_consortium).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_patronage_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_state_finance_ministry).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_confidence_signaling_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and negotiates the conditionality package attached to lending programs, sets benchmarks (fiscal deficit targets, SOE privatization schedules, subsidy removal timetables), and disburses tranches contingent on compliance review. Collects repayment with reduced default risk and preserves its own capital base for future lending to other sovereigns.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium, beneficiary).

% Negotiates and co-signs the program, using external conditionality as leverage to force through domestic reforms (subsidy cuts, SOE restructuring) that domestic political coalitions would otherwise block. Gains access to bridge financing and a credible commitment device that lowers borrowing costs going forward.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_state_finance_ministry, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_state_finance_ministry, beneficiary).

% Inherit a fiscal position unencumbered by an unsustainable debt spiral and unproductive subsidy commitments; benefit from restored access to capital markets at reasonable rates and from public spending redirected away from patronage toward investment, once the adjustment period concludes. Have no seat at the negotiating table and experience the benefit only years later, if the reform holds.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_debtor_state, beneficiary,
    powerless, generational, trapped, national).

% Read IMF program approval as a credible signal that a sovereign's balance sheet is being brought under control, which repriced sovereign risk and permits continued bond issuance. Free to reallocate capital instantly if the signal proves false; the conditionality regime exists partly to preserve this signaling function.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, biographical, arbitrage, global).

% Lose subsidies, face privatization or closure mandates, and shed employment as conditionality-mandated restructuring is implemented. Some operate genuine strategic functions but many exist primarily as employment-patronage vehicles; the coordination reading treats their contraction as the removal of a genuine fiscal drag rather than as extraction from a legitimate function.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises, payer,
    moderate, biographical, constrained, national).

% Lose access to state resources distributed through clientelist channels — jobs, contracts, subsidized inputs — as conditionality forces retrenchment of the patronage apparatus. Resist through political mobilization but lack the international leverage the creditor consortium holds.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_patronage_networks, payer,
    organized, biographical, constrained, national).

% Experience near-term reductions in subsidized services (fuel, food, healthcare) during the adjustment window but were not party to conditionality negotiations, which occur between technocratic finance-ministry staff and IMF mission teams. Their transitional hardship is treated in this reading as a necessary short-run cost of restoring long-run fiscal sustainability.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, affected_public_service_users, excluded,
    powerless, immediate, trapped, local).

% Conduct ex-post program evaluations assessing whether conditionality-linked reforms achieved fiscal sustainability and growth outcomes relative to counterfactual non-program trajectories. Their assessments feed back into program design debates without directly controlling any single program.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists_program_reviewers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sovereign borrowers facing fiscal crisis have a time-inconsistency problem: without a credible external commitment device, domestic political economy will resist necessary but painful adjustment (subsidy removal, SOE restructuring, deficit reduction), producing repeated crises and closing off capital market access. Conditionality solves this by making tranche disbursement contingent on verified reform steps, giving the finance ministry external leverage against domestic veto players and giving capital markets a monitored, credible signal that fiscal trajectory is being corrected.
% TRANSFER_FUNCTION: Moves near-term subsidy and patronage flows away from state-owned enterprises and clientelist networks, and moves debt-service risk from the creditor consortium (via program financing) toward a restored fiscal position; the intended long-run transfer is a lower sovereign risk premium and preserved market access flowing to the state and its future taxpayers.
% ABSENT_VOICES: Public-service users experiencing immediate subsidy withdrawal and workers in restructured SOEs are not present in the finance-ministry/IMF negotiation room; their transitional costs are represented, if at all, through aggregate poverty and social-safety-net side letters rather than direct participation.
% DISAPPEARANCE_RATIONALE: From this reading's perspective, if conditionality vanished, sovereigns in fiscal crisis would lack a credible commitment device, domestic reform coalitions would face stronger resistance from patronage networks, and capital markets would lose their primary signal of program credibility — borrowing costs would likely rise and crisis recurrence would increase. Whether this counts as 'the world rearranging' or 'the world staying the same' is itself contested across readings of the same kernel: the debtor-extraction reading holds the opposite counterfactual.
% FOUNDING_PROBLEM: Sovereign debt crises in the 1980s-90s (Latin American debt crisis, later transition economies) demonstrated that ad hoc bailouts without attached reform commitments produced repeated defaults, moral hazard, and capital flight, because lenders had no mechanism to verify that borrowed funds would be used to restore solvency rather than to postpone adjustment.
% FOUNDING_PROBLEM_CORROBORATION: IMF program-evaluation staff and mainstream development economists (e.g., ex-post assessments by the IMF's own Independent Evaluation Office) attest the commitment-device problem remains live in recurring debt crises. Independent academic economists outside the IMF and creditor institutions (some sympathetic to this reading, others not) have found mixed evidence on whether conditionality actually improves post-program growth and fiscal outcomes relative to non-program adjustment paths — corroboration for the founding problem's continued relevance is genuine but not unanimous, and the debtor-extraction reading disputes it entirely.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, contested).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) and rising only slightly over the interval: under this reading the mechanism's costs fall on targeted, named inefficient sectors rather than diffusely across the population, and the coordination function (credible commitment, market signaling) is genuine and load-bearing, which keeps epsilon well below what a pure-extraction reading would author for the same nominal policy instrument. Suppression is moderate (0.42) because tranche-withholding is real enforcement leverage, but exit for the sovereign exists in principle (default, alternative financing, non-renewal) even if costly — this is not identity-locked trapped extraction. Theater ratio is low (0.22): compliance review is a genuinely functional verification mechanism in this reading, not predominantly performative, though some monitoring activity has calcified into box-checking over repeated program cycles, which the slow theater-ratio rise captures.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute the creditor consortium and finance-ministry seats (institutional power, arbitrage/constrained exit, agenda_setter role) closer to rope/tangled_rope territory, while the SOE and patronage-network payer seats, with moderate/organized power and constrained exit, may compute with higher effective extraction despite the story-level metrics being moderate — this divergence is expected and is not reconciled here; it is the seat-level measurement the framework exists to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Future taxpayers and international capital markets sit near the beneficiary end: they collect improved fiscal trajectory and preserved market access without bearing the near-term contraction. The finance ministry and IMF consortium are dual-positioned agenda-setters and beneficiaries — they administer the mechanism and each collects a version of its intended payoff (domestic leverage for reform; reduced default risk for the creditor). Inefficient SOEs and patronage networks are payers: their loss is structurally the intended output of the coordination function, not an externality. Affected public-service users are excluded rather than positioned as primary payers under this reading — their transitional hardship is real but secondary to the SOE/patronage contraction that is the mechanism's designed target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored contested rather than dead: recurring sovereign debt crises (most recently among low-income and middle-income borrowers post-2020) indicate the underlying commitment-credibility problem persists, which is what prevents this reading from collapsing into a piton. Where this reading would need revision is if ex-post program evaluations consistently showed conditionality-linked reforms failing to improve fiscal outcomes relative to non-program adjustment — that would corrode the coordination-function claim this reading rests on and shift weight toward the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_leverage_asymmetry,
    'Is the conditionality mechanism a genuine Pareto-improving commitment device (this reading), or does its bargaining structure systematically favor creditor interests such that the ''coordination'' framing is itself an artifact of who designs the benchmarks?',
    'Comparative institutional analysis of program design: were benchmark targets negotiated with meaningful debtor input and technical alternatives on the table, or dictated by creditor technical staff with the debtor''s only real choice being accept-or-default? Archival analysis of IMF mission internal documents and debtor negotiating records would help resolve this.',
    'If benchmark design is unilaterally creditor-driven with no genuine debtor counter-proposal power, the coordination-function claim this reading rests on weakens substantially and the constraint''s true structural position moves toward the debtor_extraction_reading or hybrid_selectivity_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_leverage_asymmetry, conceptual, 'Whether conditionality design reflects genuine bilateral coordination or creditor-dictated terms dressed as coordination.').

omega_variable(
    ex_post_outcome_evidence,
    'Do IMF-program countries show better fiscal sustainability and growth outcomes post-program relative to a credible non-program counterfactual, net of selection effects (countries that seek IMF programs may be systematically worse off to begin with)?',
    'Quasi-experimental or matched-comparison studies (e.g., regression discontinuity on IMF board approval thresholds, synthetic control methods) comparing program and non-program crisis countries with similar initial conditions.',
    'Consistent positive findings would corroborate the coordination reading''s founding-problem claim; consistently null or negative findings would support the sibling readings'' claim that the coordination story is a legitimating cover for extraction, shifting this constraint''s true classification toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ex_post_outcome_evidence, empirical, 'Whether program participation causally improves fiscal and growth outcomes relative to the credible non-program counterfactual.').

omega_variable(
    soe_inefficiency_versus_essential_function,
    'Are the state-owned enterprises and subsidy programs targeted for contraction genuinely inefficient rent-extraction vehicles, or do some perform essential redistributive or market-failure-correcting functions that this reading mischaracterizes as pure patronage?',
    'Sector-by-sector fiscal and social-impact audits distinguishing SOEs/subsidies with positive externalities or public-good characteristics from those functioning primarily as employment patronage.',
    'If a substantial share of targeted SOEs/subsidies serve genuine public functions, this reading''s victim characterization (payers = inefficient sectors only) is too narrow, and affected_public_service_users should be reclassified from excluded/bystander to primary payer — moving this constraint''s structure toward the debtor_extraction_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soe_inefficiency_versus_essential_function, empirical, 'Whether the targeted state sectors are genuinely inefficient or perform under-recognized essential functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stru_tr_t4, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(stru_tr_t12, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stru_be_t4, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(stru_be_t12, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(stru_su_t4, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(stru_su_t12, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the structural_adjustment_conditionalities kernel. creditor_coordination_reading (this story) authors conditionality as a low-to-moderate-epsilon rope solving a genuine sovereign commitment problem. debtor_extraction_reading authors the same nominal policy instrument as a high-epsilon extractive mechanism with public-service users as primary victims. hybrid_selectivity_reading authors the differential enforcement pattern across geopolitically strategic versus marginal debtors as the operative structural fact. All three share the kernel (conditional sovereign lending) but instantiate structurally distinct constraints with different epsilon, different beneficiary/victim sets, and different classifications, per the ε-invariance principle — they are not measurement-parameter variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
