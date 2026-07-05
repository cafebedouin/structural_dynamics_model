% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: IMF/World Bank Structural Adjustment Conditionalities as Creditor Coordination Mechanism
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This story instantiates the CREDITOR COORDINATION reading of the
 *   structural_adjustment_conditionalities kernel: conditionalities attached
 *   to IMF/World Bank lending as a genuine commitment-device solution to the
 *   sovereign-lending time-inconsistency problem. Under this reading, the
 *   arrangement is a Rope — a coordination mechanism benefiting future
 *   taxpayers (through restored fiscal capacity and market access) and
 *   international capital (through credible risk pricing), with costs
 *   concentrated on inefficient state sectors that were unsustainable
 *   independent of the program. This is a distinct constraint from the
 *   debtor_extraction_reading (which reads the same conditionality apparatus
 *   as neo-colonial extraction with the state itself as primary victim) and
 *   the hybrid_selectivity_reading (which reads it as selectively-enforced
 *   discipline correlated with geopolitical alignment rather than fiscal
 *   fundamentals). Each reading has a different beneficiary/victim structure
 *   and a different stable epsilon; they are linked as sibling constraints,
 *   not merged into one measurement.
 *
 * KEY AGENTS:
 *   - imf_program_negotiators: agenda_setter (institutional/analytical) — designs and monitors conditionality
 *   - future_taxpayers: beneficiary (powerless/trapped) — inherits restored fiscal capacity
 *   - international_capital_markets: beneficiary (organized/arbitrage) — prices sovereign risk on credible commitment signal
 *   - inefficient_state_owned_enterprises: payer (moderate/constrained) — loses subsidy and faces restructuring
 *   - borrowing_government_finance_ministry: agenda_setter/payer (institutional/constrained) — sovereign counterparty and domestic enforcer
 *   - domestic_civil_society_and_labor_unions: excluded (organized/trapped) — bears consequences without negotiating seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.32).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "IMF/World Bank Structural Adjustment Conditionalities as Creditor Coordination Mechanism").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'ab148b80-1ed7-4b04-b79c-6bc2c194a714').
narrative_ontology:cs_kernel_codification('ab148b80-1ed7-4b04-b79c-6bc2c194a714', formalized).
narrative_ontology:cs_authority_grounding('ab148b80-1ed7-4b04-b79c-6bc2c194a714', expertise).
narrative_ontology:cs_interpretation_layer_present('ab148b80-1ed7-4b04-b79c-6bc2c194a714').
narrative_ontology:cs_reading_relation('ab148b80-1ed7-4b04-b79c-6bc2c194a714', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab148b80-1ed7-4b04-b79c-6bc2c194a714', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('ab148b80-1ed7-4b04-b79c-6bc2c194a714', foundational, conditionality_solves_genuine_commitment_problem).
narrative_ontology:cs_axiom_status(conditionality_solves_genuine_commitment_problem, holdable).
narrative_ontology:cs_axiom_grounding('ab148b80-1ed7-4b04-b79c-6bc2c194a714', conditionality_solves_genuine_commitment_problem, empirically_contingent).
narrative_ontology:cs_axiom('ab148b80-1ed7-4b04-b79c-6bc2c194a714', foundational, adjustment_costs_reflect_prior_unsustainability_not_manufactured_transfer).
narrative_ontology:cs_axiom_status(adjustment_costs_reflect_prior_unsustainability_not_manufactured_transfer, holdable).
narrative_ontology:cs_axiom_grounding('ab148b80-1ed7-4b04-b79c-6bc2c194a714', adjustment_costs_reflect_prior_unsustainability_not_manufactured_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('ab148b80-1ed7-4b04-b79c-6bc2c194a714', credible_commitment_technocratic_framework).
narrative_ontology:cs_drift_state('ab148b80-1ed7-4b04-b79c-6bc2c194a714', post_2008_and_post_pandemic_program_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab148b80-1ed7-4b04-b79c-6bc2c194a714', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_bond_holders).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, prudently_managed_state_agencies).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, subsidized_but_unproductive_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_government_finance_ministry).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_precondition_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_confidence_signaling_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and attach conditionality packages (fiscal targets, subsidy removal, SOE reform, monetary discipline) to lending programs. Frame conditions as the technical price of restoring solvency and access to capital markets, not as punitive discipline. Monitor compliance through periodic reviews and can withhold tranches.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_program_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Inherit the debt burden and the fiscal capacity of the state. If the adjustment succeeds, they face a smaller debt-service load, a more solvent state, and continued access to affordable external financing rather than default and exclusion from capital markets. They are not present in negotiations but their long-run fiscal position is the primary stated justification for the conditions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Extend or hold sovereign debt contingent on credible fiscal commitments; conditionality functions as a public signal that reduces the risk premium demanded and restores market access. Can reallocate capital instantly to other sovereigns if a program fails, so their exposure to any single failure is limited.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    organized, biographical, arbitrage, global).

% Lose subsidies, face privatization or restructuring mandates, and often shed employment as conditionality targets unproductive spending. Management and unionized workers within these enterprises bear concentrated adjustment costs even where the enterprise's underlying inefficiency is real and pre-existing. Exit is limited to political resistance or informal-sector absorption.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_owned_enterprises, payer,
    moderate, biographical, constrained, national).

% Agricultural or industrial sectors kept afloat by price supports or trade protection lose that support under fiscal-consolidation conditions. Their output was not competitive at world prices; removal of support exposes this, though the transition period offers little cushioning.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, subsidized_but_unproductive_sectors, payer,
    powerless, biographical, constrained, national).

% Negotiates and formally accepts the conditionality package as a sovereign counterparty, then administers implementation domestically. Bears the domestic political cost of enforcing conditions but also gains external cover to make politically difficult reforms it may independently favor.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_government_finance_ministry, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_government_finance_ministry, payer).

% Are not party to program negotiations between the finance ministry and the lending institution, despite bearing direct employment and service-access consequences of fiscal consolidation. Voice concerns through protest and domestic political channels after terms are set, not during negotiation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_civil_society_and_labor_unions, excluded,
    organized, biographical, trapped, national).

% Conduct ex-post program evaluations (including the IMF's own Independent Evaluation Office) assessing whether conditionality achieved fiscal sustainability and growth outcomes across program countries.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists_and_evaluation_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine sovereign-lending coordination problem: without credible commitment mechanisms attached to disbursement, a government facing a fiscal crisis has weak incentive to sustain painful consolidation once financing arrives, and lenders have no basis to distinguish a state that will reform from one that will not. Conditionality ties tranche disbursement to verified fiscal steps, allowing lending to resume at lower risk premiums than would otherwise be available.
% TRANSFER_FUNCTION: Moves fiscal capacity away from unreformed, subsidized, or overstaffed state sectors toward debt service and macroeconomic stabilization, with the intended long-run transfer being reduced future debt-service burden (benefiting future taxpayers) in exchange for present-period reductions in specific sectoral transfers.
% ABSENT_VOICES: Domestic labor unions, civil society organizations, and the populations dependent on subsidized sectors are not present at the negotiating table between the finance ministry and the lending institution; they experience the terms only after ratification, through the domestic political process.
% DISAPPEARANCE_RATIONALE: If conditionality mechanisms disappeared, lending institutions would have no credible basis to price sovereign risk on distressed borrowers, external financing would either dry up or come at much higher cost, and governments facing fiscal crises would lose one channel of externally-anchored commitment to consolidation — capital markets would reprice sovereign risk sharply upward for crisis-prone borrowers.
% FOUNDING_PROBLEM: Sovereign borrowers in fiscal distress historically defaulted or inflated away debt after receiving emergency financing, because a government's promise to consolidate lacked any enforcement mechanism once the cash was disbursed — lenders needed a way to verify ongoing commitment before releasing successive tranches.
% FOUNDING_PROBLEM_CORROBORATION: IMF Independent Evaluation Office reports and academic sovereign-debt-crisis literature (outside the Fund's own program departments) corroborate that credible commitment problems are real and that disbursement-linked conditionality reduces default risk in some cases; the same evaluative literature also finds mixed and sometimes negative growth outcomes in program countries, so the founding problem's continued centrality to program design is not attested uniformly even by these outside evaluators.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate-low (0.32 at interval end) because, under this reading, the costs borne by SOE and subsidized-sector payers are read as the unwinding of a pre-existing inefficiency subsidy rather than a novel transfer extracted for creditor profit — the coordination story is substantive, not cover. Suppression is moderate (0.28): governments do retain the sovereign option to reject a program and seek alternative financing or default, so exit is constrained rather than trapped for the state as a whole, though it is closer to trapped for the specific sectors losing support. Theater ratio is low (0.15) reflecting that most conditionality activity under this reading is genuine fiscal monitoring rather than performative compliance theater. Resistance is moderate (0.45) — domestic political resistance is real but does not, under this reading, indicate that the coordination function is false.
 *
 * DIRECTIONALITY LOGIC:
 *   Future taxpayers and international capital sit near the beneficiary end: the former gain diffuse long-run fiscal-capacity benefit (low d), the latter gain direct pricing benefit with high mobility/arbitrage exit (very low d, since they can reallocate capital away from any single failing program). SOE and subsidized-sector payers sit toward the target end (higher d) because their losses are concentrated, immediate, and not offset by an equivalent direct gain within the same time horizon, even though the reading holds their prior position was unsustainable. The finance ministry occupies a dual seat: it is structurally an agenda_setter (co-designs and ratifies the program) but also a payer (bears domestic political cost of enforcement) — this dual role is why it carries a secondary_role rather than being forced into one box.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — lack of credible commitment mechanisms for distressed sovereign borrowers — could in principle become dead if alternative commitment technologies (state-contingent bonds, automatic fiscal rules, GDP-linked debt) supplanted conditionality's monitoring function. Under this reading that has not yet occurred: outside evaluators (IEO, academic sovereign-debt literature) find the commitment problem persists in most program countries, though growth outcomes are mixed. The classification as Rope rather than Tangled Rope hinges on this reading's claim that the SOE/subsidized-sector costs are not asymmetric extraction riding on the coordination function, but the coordination function's necessary unwinding of a prior, independently unsustainable arrangement. The sibling debtor_extraction_reading disputes exactly this premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_extraction_premise,
    'Are the fiscal consolidation costs borne by state-owned enterprises and subsidized sectors the necessary unwinding of a pre-existing, independently unsustainable arrangement (this reading''s premise), or are they a novel transfer manufactured by the conditionality apparatus itself for creditor benefit (the debtor_extraction_reading''s premise)?',
    'Comparative counterfactual analysis: did comparable state sectors in non-program countries facing similar fiscal pressure adjust on similar terms without external conditionality? Divergence would support the extraction reading; convergence would support this reading.',
    'If the extraction reading''s premise holds, this story''s beneficiary/victim structure is mischaracterized and the true classification shifts toward tangled_rope or snare at the level of the underlying kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_or_extraction_premise, conceptual, 'Core premise dispute between coordination and extraction readings of the same kernel.').

omega_variable(
    sovereign_exit_credibility,
    'Is the sovereign''s option to reject a program and seek alternative financing (or default) a genuine exit option, or is it foreclosed in practice by the absence of any comparable alternative financing source at the moment of crisis?',
    'Empirical survey of program countries that did decline IMF terms during acute crisis — did viable alternative financing exist, and on what terms?',
    'If sovereign exit is illusory in crisis conditions, the constrained exit_options authored for the finance ministry should be reclassified toward trapped, raising effective extraction under this reading''s own directionality logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_exit_credibility, empirical, 'Whether the sovereign borrower''s formal exit option is real or nominal.').

omega_variable(
    fsm_natural_law_framing_check,
    'Does this reading''s framing of fiscal consolidation as a natural, technocratic necessity (rather than a policy choice among several) function as a false-summit move that obscures identifiable beneficiaries?',
    'Not applicable directly — this story is claimed as rope, not mountain, so FSM does not gate here. Retained as a conceptual flag: check whether program documents rhetorically borrow mountain-framing (''there is no alternative'') while the structural claim remains rope-level with named beneficiaries.',
    'If program communications systematically use naturalizing language beyond what the rope classification supports, that is evidence for the hybrid_selectivity_reading''s account of selective, discretionary application dressed as technical inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_natural_law_framing_check, conceptual, 'Whether naturalizing rhetoric exceeds the structural claim this reading actually makes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stru_tr_t6, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(stru_tr_t12, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(stru_tr_t18, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(stru_tr_t36, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 36, 0.15).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(stru_be_t6, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(stru_be_t12, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(stru_be_t18, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(stru_be_t36, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 36, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(structural_adjustment_conditionalities__creditor_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% Three sibling stories decompose the natural-language concept 'structural adjustment conditionality': this story (creditor_coordination_reading, Rope, epsilon~0.32), debtor_extraction_reading (Snare/Tangled-Rope, substantially higher epsilon expected, state itself as primary victim), and hybrid_selectivity_reading (Tangled Rope with victim set contingent on borrower geopolitical alignment). Per the epsilon-invariance principle these are not one constraint measured three ways but three structurally distinct constraints sharing an institutional kernel (IMF/World Bank conditionality apparatus) and text (Articles of Agreement, program documents). Each carries its own stable epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
