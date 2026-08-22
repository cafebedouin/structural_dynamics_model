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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: IMF/World Bank Structural Adjustment Conditionalities (Creditor-Coordination Reading)
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This story instantiates the creditor-coordination reading of the
 *   structural-adjustment-conditionalities kernel: conditional lending is
 *   understood as a credible-commitment device solving a genuine
 *   sovereign-lending coordination problem — without it, capital markets
 *   cannot verify that emergency lending will be repaid rather than
 *   dissipated, so crisis lending either does not occur or occurs only at
 *   prohibitive risk. Under this reading, the near-term costs borne by
 *   protected state sectors and subsidy recipients are read as the removal of
 *   an unsustainable prior transfer (a correction), not as extraction from a
 *   legitimate entitlement. This is a distinct constraint from the
 *   debtor-extraction reading (which authors the SAME conditionality
 *   apparatus as a neo-colonial extraction mechanism with a much higher
 *   epsilon and a different beneficiary/victim structure) and from the
 *   hybrid-selectivity reading (which locates the constraint's defect in
 *   asymmetric enforcement rather than in the coordination logic itself). All
 *   three are siblings in one kernel and are NOT the same constraint — see
 *   network.affects_constraints and the omegas below for where the readings
 *   diverge.
 *
 * KEY AGENTS:
 *   - imf_creditor_consortium: agenda_setter/beneficiary (institutional/arbitrage) — administers conditions, protected by restored solvency
 *   - future_taxpayers_of_borrowing_states: beneficiary (powerless/trapped) — diffuse generational beneficiary of restored fiscal sustainability
 *   - international_bondholders: beneficiary (organized/arbitrage) — protected debt-service capacity
 *   - inefficient_state_owned_enterprises: payer (moderate/constrained) — lose subsidies read as unsustainable transfers
 *   - borrowing_state_finance_ministry: agenda_setter/payer (institutional/constrained) — signs and administers the program domestically
 *   - public_sector_subsidy_recipients: payer (powerless/trapped) — bear immediate consumption shock from subsidy phase-out
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.32).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "IMF/World Bank Structural Adjustment Conditionalities (Creditor-Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'a7b6a379-4da7-41e2-a720-5d932878d40e').
narrative_ontology:cs_kernel_codification('a7b6a379-4da7-41e2-a720-5d932878d40e', formalized).
narrative_ontology:cs_authority_grounding('a7b6a379-4da7-41e2-a720-5d932878d40e', extraction).
narrative_ontology:cs_interpretation_layer_present('a7b6a379-4da7-41e2-a720-5d932878d40e').
narrative_ontology:cs_reading_relation('a7b6a379-4da7-41e2-a720-5d932878d40e', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7b6a379-4da7-41e2-a720-5d932878d40e', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('a7b6a379-4da7-41e2-a720-5d932878d40e', foundational, conditionality_solves_genuine_commitment_problem).
narrative_ontology:cs_axiom_status(conditionality_solves_genuine_commitment_problem, holdable).
narrative_ontology:cs_axiom_grounding('a7b6a379-4da7-41e2-a720-5d932878d40e', conditionality_solves_genuine_commitment_problem, empirically_contingent).
narrative_ontology:cs_axiom('a7b6a379-4da7-41e2-a720-5d932878d40e', secondary, subsidy_withdrawal_is_fiscal_correction_not_taking).
narrative_ontology:cs_axiom_status(subsidy_withdrawal_is_fiscal_correction_not_taking, holdable).
narrative_ontology:cs_axiom_grounding('a7b6a379-4da7-41e2-a720-5d932878d40e', subsidy_withdrawal_is_fiscal_correction_not_taking, instrumental).
narrative_ontology:cs_reference_frame('a7b6a379-4da7-41e2-a720-5d932878d40e', credible_commitment_lending_framework).
narrative_ontology:cs_drift_state('a7b6a379-4da7-41e2-a720-5d932878d40e', post_2010s_serial_program_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a7b6a379-4da7-41e2-a720-5d932878d40e', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_borrowing_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_foreign_direct_investors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, protected_domestic_industries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_subsidy_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_state_finance_ministry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets loan conditions requiring fiscal consolidation, subsidy removal, and structural reform as a precondition for disbursement. Administers program reviews and can withhold tranches for noncompliance. Frames conditionality as the mechanism that makes lending to sovereign risk possible at all — without credible commitment devices, no capital would flow on any terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, imf_creditor_consortium, beneficiary).

% Inherit a fiscal position without runaway debt service crowding out future public spending, because the adjustment program restored sustainability during the crisis window. Do not participate in program negotiation but are the diffuse long-run recipients of restored solvency and lower future borrowing costs.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_borrowing_states, beneficiary,
    powerless, generational, trapped, national).

% Hold sovereign debt instruments whose value depends on the borrowing state maintaining debt service capacity. Conditionality programs function as a credible-commitment signal that reduces default risk premia, protecting bond value and unlocking future market access at lower cost for the sovereign itself.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders, beneficiary,
    organized, biographical, arbitrage, global).

% Lose subsidies, protected pricing, or face privatization mandates under program conditions. From this reading, their prior position reflected unsustainable fiscal transfers rather than genuine productive value; the adjustment removes a drain on public finances rather than a legitimate entitlement. Managers and workers experience real income loss but the loss is structurally a correction, not an extraction, under this reading.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises, payer,
    moderate, biographical, constrained, national).

% Face tariff reduction or subsidy withdrawal conditions that expose them to competition. Under this reading, protection had been misallocating capital away from comparative-advantage sectors; removal reallocates resources toward long-run productivity, though incumbent firms bear short-run adjustment costs.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, protected_domestic_industries, payer,
    moderate, biographical, constrained, national).

% Negotiates and signs the conditionality agreement, then administers domestic implementation (budget cuts, price liberalization, enterprise reform). Bears the political cost of enforcing austerity domestically while receiving the disbursement that keeps sovereign default at bay. Retains formal sovereignty over the decision to enter the program.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_state_finance_ministry, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_state_finance_ministry, payer).

% Households dependent on fuel, food, or utility subsidies scheduled for phase-out under the program. Bear immediate consumption shocks. Under this reading, subsidy removal is a necessary correction to unsustainable public finance, with the burden expected to be offset by targeted safety nets negotiated as part of the program design.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_subsidy_recipients, payer,
    powerless, immediate, trapped, local).

% Benefit from the macroeconomic stabilization and policy predictability the program is designed to produce, which lowers the risk premium on future investment in the reformed economy. Not party to program negotiation; their expected future entry is cited as part of the program's justification.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_foreign_direct_investors, beneficiary,
    organized, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine sovereign-lending commitment problem: without an external, monitored conditionality mechanism, a government facing a fiscal crisis cannot credibly promise future creditors it will not simply inflate away or default on new debt, so capital either does not flow or flows only at prohibitive risk premia. Conditionality substitutes for the missing domestic credible-commitment technology.
% TRANSFER_FUNCTION: In the near term, moves consumption and subsidy flows away from protected state-sector workers and subsidy recipients toward fiscal balance; in the medium term, restores debt-service capacity that protects existing bondholder claims and lowers the future cost of capital for the state, which accrues to future taxpayers and future investors.
% ABSENT_VOICES: Subsidy recipients and state-enterprise workers negotiate nothing directly — the finance ministry represents them in aggregate, but program conditions are set primarily in bilateral talks between the ministry and the creditor institution. Their objection, under this reading, is treated as a transition-cost concern to be mitigated by safety-net design, not as evidence against the coordination logic itself.
% DISAPPEARANCE_RATIONALE: If conditionality mechanisms disappeared, sovereign lending to fiscally stressed states would either cease or reprice sharply upward absent an alternative commitment device, precipitating faster, less-managed default cycles; the orderly workout function conditionality performs would need to be replaced by something else (unilateral default, ad hoc bailouts, or higher permanent risk premia).
% FOUNDING_PROBLEM: Sovereign borrowers in fiscal crisis lack a credible mechanism to commit to future repayment discipline, and creditors lack a mechanism to verify that emergency lending will be used to restore rather than postpone the underlying fiscal problem — without conditionality, crisis lending either does not happen or recurs without resolution.
% FOUNDING_PROBLEM_CORROBORATION: IMF program evaluations and some independent sovereign-debt economists (outside the Fund itself) attest the commitment-problem is real and recurring across serial-default histories, supporting a live-problem reading. Independent post-program audits and academic balance-of-payments research from outside both the IMF and borrowing-state governments are more divided — some find programs restore sustainability, others find repeated program failure suggests the mechanism addresses a narrower problem than claimed. No corroboration exists that is independent of stakes in either the lending relationship or the reform outcome.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-low (0.32-0.40, declining) because under this reading the transfers imposed by conditionality correct a prior fiscal misallocation rather than extract value from a legitimate baseline — the loss to state-sector workers and subsidy recipients is real but is structurally a withdrawal of a fiscally unsustainable benefit, not a taking. Suppression is moderate and declining (0.28-0.35) reflecting genuine enforcement machinery (tranche conditionality, program reviews) that nonetheless operates through a signed, formally voluntary agreement rather than coercive seizure. Theater ratio is low (0.18-0.22) because this reading holds the coordination function to be substantially real rather than performative. All three tracked metrics improve modestly over the interval reflecting the reading's own account: programs that succeed reduce the felt burden of conditionality as fiscal sustainability is restored and future borrowing costs fall.
 *
 * DIRECTIONALITY LOGIC:
 *   Future taxpayers and international capital sit at the beneficiary end because, under this reading, restored fiscal sustainability and reduced default risk are real coordination goods that accrue broadly and durably. Inefficient state-owned enterprises, protected domestic industries, and subsidy recipients sit toward the payer end because they lose specific, previously-received transfers — but their positioning as 'victims' in this reading is qualified: the loss is structurally the removal of a prior misallocation, and the beneficiaries of the correction (future taxpayers of the SAME state) substantially overlap with the population bearing the immediate cost, which is the central asymmetry this reading treats as intergenerational rather than extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination-vs-extraction classification question is precisely what distinguishes this reading from its siblings: this reading holds the founding problem (missing credible-commitment technology for sovereign crisis lending) to be substantially still live and the mechanism to be still functionally solving it, which is why founding_problem_status is authored as contested rather than dead — the debtor-extraction reading would author the same founding problem as either pretextual or long since resolved, with the persisting apparatus serving a different (extractive) function. The classification here as rope rather than tangled_rope follows from this reading's own account that the coordination function is not merely a cover story for asymmetric extraction, because the population bearing near-term cost and the population capturing long-term benefit substantially overlap (future taxpayers of the same state).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_extraction_kernel_ambiguity,
    'Is structural adjustment conditionality a genuine solution to a sovereign credible-commitment problem, or is the commitment-problem framing itself a legitimating narrative for creditor-favorable extraction that would occur under any framing?',
    'Compare post-program fiscal and growth trajectories against a plausible counterfactual (default-and-restructure without conditionality) across a large cross-country panel, controlling for selection into programs; also examine whether program design systematically favors bondholder recovery rates over domestic welfare metrics when the two conflict.',
    'If counterfactual analysis shows programs produce no better sustainability outcomes than unconditional restructuring, the coordination premise this reading depends on collapses and the constraint would more plausibly classify as tangled_rope or snare under a debtor-extraction reading. If programs demonstrably outperform the counterfactual, this reading''s rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_extraction_kernel_ambiguity, conceptual, 'The central kernel-level dispute: whether the coordination function is real or pretextual, which is exactly what the three sibling readings disagree about.').

omega_variable(
    future_taxpayer_beneficiary_identity_ambiguity,
    'Are ''future taxpayers of the borrowing state'' and ''current subsidy recipients / state-enterprise workers'' meaningfully the same population across the relevant time horizon, or does the beneficiary population differ systematically (e.g., by class, region, or formal/informal sector) from the population bearing the immediate cost?',
    'Incidence analysis tracing which income deciles and sectors bear near-term subsidy/wage losses versus which deciles and sectors capture the benefits of lower future borrowing costs and restored investment climate.',
    'If the near-term payers and long-term beneficiaries are substantially different populations (not merely different time-slices of the same population), the intergenerational-correction framing this reading relies on weakens, and the true structure looks more like cross-class transfer than cross-time smoothing — pushing the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_taxpayer_beneficiary_identity_ambiguity, empirical, 'Whether the reading''s core distributional claim (beneficiaries and payers substantially overlap across time) holds empirically.').

omega_variable(
    sovereignty_of_program_signing,
    'Is the finance ministry''s signature on a conditionality agreement a genuine exercise of sovereign choice among real alternatives, or a formally voluntary act made under fiscal duress that leaves no realistic alternative?',
    'Examine the counterfactual financing options available to the state at the time of signing — market access, bilateral alternatives, reserve buffers — to assess whether program entry was a selection among live options or the only available path to avoiding immediate default.',
    'If no realistic alternative existed, the ''formally voluntary agreement'' framing used to keep suppression moderate (rather than high) in this reading is weakened, and effective suppression is closer to coercive than contractual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_of_program_signing, empirical, 'Whether the formal voluntariness this reading relies on to keep suppression moderate reflects genuine choice or duress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stru_tr_t6, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(stru_tr_t12, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(stru_tr_t18, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(stru_tr_t36, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 36, 0.18).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stru_be_t6, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement(stru_be_t12, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(stru_be_t18, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 18, 0.34).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(stru_be_t36, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 36, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(stru_su_t6, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(stru_su_t12, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(stru_su_t18, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 18, 0.29).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(stru_su_t36, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 36, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the structural_adjustment_conditionalities kernel, decomposed per the ε-invariance principle because the same apparatus (IMF/World Bank conditional lending) produces structurally distinct epsilon values, beneficiary/victim sets, and classifications depending on whether the coordination premise is accepted, rejected, or held to apply selectively. This file (creditor_coordination_reading) authors low-moderate epsilon (0.32-0.40) with a genuine coordination function and overlapping payer/beneficiary populations across time. debtor_extraction_reading is expected to author substantially higher epsilon with the domestic population broadly as victim and international capital as the concentrated beneficiary, with no functioning coordination premise. hybrid_selectivity_reading is expected to locate the defect not in the coordination logic itself but in asymmetric enforcement across strategically important versus unimportant debtors. All three share the kernel (the conditionality apparatus itself) but are not the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
