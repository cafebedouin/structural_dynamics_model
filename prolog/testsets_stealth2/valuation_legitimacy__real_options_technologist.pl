% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__real_options_technologist, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: Real-Options Technologist Reading of Valuation Legitimacy
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — real_options_technologist — of the
 *   contested valuation_legitimacy kernel: the claim that a $1.75T private
 *   valuation for a vertically integrated space company is legitimate because
 *   it represents the present value of a portfolio of technological options
 *   (Starlink proven at scale, Starship high-variance and enabling all
 *   downstream options, orbital compute addressing a genuine power gap, lunar
 *   economy speculative with first-mover advantage, Mars as civilizational
 *   hedge), with vertical integration compounding optionality across
 *   segments. The epsilon referent is the standing arrangement under contest
 *   — the option-space pricing regime as it actually operates on this cap
 *   table — assessed by this reading's own lights: the reading endorses the
 *   method, so it perceives mostly legitimate coordination with material
 *   extraction concentrated at the margins (risk transfer to late entrants,
 *   information asymmetry, dilution-pressure suppression). Sibling readings
 *   (dcf_fundamentalist, musk_cult_believer, governance_skeptic) are separate
 *   constraint files linked via network.affects_constraints; per the
 *   epsilon-invariance principle each is its own constraint with its own
 *   epsilon over the same referent. The claimed type (tangled_rope) and the
 *   authored metrics are independent facts: the claim states what I believe
 *   is structurally true of this reading's constraint; the metrics state what
 *   I believe is descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - - spacex_controller: Primary beneficiary and agenda_setter (powerful/identity_locked) — sets the option narrative, controls disclosure, collects financing terms without dilution
 *   - - early_cap_table_investors: Secondary beneficiary (institutional/constrained) — appreciating marks, tender-only liquidity, lend credibility
 *   - - employee_equity_holders: Dual-positioned beneficiary-payer (moderate/constrained) — paper wealth up, illiquid and voiceless
 *   - - late_stage_valuation_entrants: Primary target (institutional/trapped) — buy near top marks, locked up, dependent on company-controlled data
 *   - - fund_limited_partners: Diffuse target (moderate/trapped) — indirect exposure through fund layers, fees on marked-up values
 *   - - dcf_analyst_community: Excluded critic (organized/constrained) — publishes rebuttals, no seat in the room
 *   - - securities_regulators: Analytical observer (institutional/analytical) — oversees disclosure from outside the private-market wall
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.36).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.28).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.36).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real-Options Technologist Reading of Valuation Legitimacy").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__real_options_technologist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'c480f5f8-ae81-4dbd-b11d-96456b4395e3').
narrative_ontology:cs_kernel_codification('c480f5f8-ae81-4dbd-b11d-96456b4395e3', formalized).
narrative_ontology:cs_authority_grounding('c480f5f8-ae81-4dbd-b11d-96456b4395e3', expertise).
narrative_ontology:cs_interpretation_layer_present('c480f5f8-ae81-4dbd-b11d-96456b4395e3').
narrative_ontology:cs_reading_relation('c480f5f8-ae81-4dbd-b11d-96456b4395e3', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c480f5f8-ae81-4dbd-b11d-96456b4395e3', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_reading_relation('c480f5f8-ae81-4dbd-b11d-96456b4395e3', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('c480f5f8-ae81-4dbd-b11d-96456b4395e3', foundational, option_space_present_value_is_legitimate_valuation_basis).
narrative_ontology:cs_axiom_status(option_space_present_value_is_legitimate_valuation_basis, holdable).
narrative_ontology:cs_axiom_grounding('c480f5f8-ae81-4dbd-b11d-96456b4395e3', option_space_present_value_is_legitimate_valuation_basis, instrumental).
narrative_ontology:cs_axiom('c480f5f8-ae81-4dbd-b11d-96456b4395e3', foundational, vertical_integration_compounds_option_value).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_option_value, holdable).
narrative_ontology:cs_axiom_grounding('c480f5f8-ae81-4dbd-b11d-96456b4395e3', vertical_integration_compounds_option_value, empirically_contingent).
narrative_ontology:cs_axiom('c480f5f8-ae81-4dbd-b11d-96456b4395e3', secondary, informed_voluntary_risk_transfer_is_not_extraction).
narrative_ontology:cs_axiom_status(informed_voluntary_risk_transfer_is_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c480f5f8-ae81-4dbd-b11d-96456b4395e3', informed_voluntary_risk_transfer_is_not_extraction, deontological).
narrative_ontology:cs_reference_frame('c480f5f8-ae81-4dbd-b11d-96456b4395e3', technological_option_space_present_value).
narrative_ontology:cs_drift_state('c480f5f8-ae81-4dbd-b11d-96456b4395e3', contemporary_private_markets_2025, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c480f5f8-ae81-4dbd-b11d-96456b4395e3', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_controller).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_cap_table_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, employee_equity_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_stage_valuation_entrants).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, fund_limited_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, employee_equity_holders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_theory).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_optionality_compounding).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, technological_option_space_pricing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds roughly 42% of equity and 82.4% of voting power. Sets the technology roadmap, decides what operational data leaves the company, and stages periodic share sales at rising marks. Pricing legitimacy by option space lets each capital raise bring in large sums while selling few new shares, so the roadmap is funded without diluting control. Leaving is not a live option: the mission and the person are fused, and the asset's central premise dissolves if the founder steps away.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_controller, agenda_setter,
    powerful, civilizational, identity_locked, global).

% Venture funds and strategic corporations that entered one to two orders of magnitude below current marks. Each tender round marks their positions up; they can sell portions into company-arranged tenders but there is no open market for the stock. Their continued presence lends the option narrative institutional credibility with later buyers.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_cap_table_investors, beneficiary,
    institutional, generational, constrained, global).

% Engineers and staff paid substantially in restricted stock. Paper compensation tracks the rising marks, but sales are window-bound and buyer-limited, and they hold no meaningful vote on company direction. Household balance sheets concentrate in the same option story their employer tells.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, employee_equity_holders, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, employee_equity_holders, payer).

% Sovereign wealth funds, mutual funds, and crossover funds buying into recent rounds near the top marks. They receive fractional claims sized by the option-space model, accept multi-year lockups with no liquid market, and depend entirely on company-controlled disclosures to update their beliefs about the portfolio's probabilities.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_stage_valuation_entrants, payer,
    institutional, biographical, trapped, global).

% Pension funds, endowments, and family offices whose exposure arrives indirectly through growth-fund allocations. They see the position through quarterly statements, cannot exit before fund term ends, and pay management and performance fees calculated on marked-up values.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, fund_limited_partners, payer,
    moderate, generational, trapped, global).

% Sell-side and academic analysts who price the company from cash-flow evidence and find the implied multiples unsupported. They publish critiques but have no seat in private tender negotiations and no access to internal unit economics beyond company releases; inside the room their framework is treated as a category error rather than a rival estimate.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_analyst_community, excluded,
    organized, biographical, constrained, global).

% Agencies overseeing private-market disclosure and systemic concentration. They observe tender marks and filing exemptions from outside, commission studies on private-market opacity, and could compel disclosure that would change how the option model's inputs are audited.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, spacex_controller).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared epistemic standard for pricing assets whose value is dominated by unrealized technological options, letting dispersed capital coordinate on high-variance frontier engineering before cash flows exist. Without a common option-pricing language, pre-cash-flow deep-tech either cannot raise or raises on pure founder charisma.
% TRANSFER_FUNCTION: Moves capital commitment and tail risk from late-stage entrants and fund LPs toward the controller and early cap table, in exchange for fractional claims on an option portfolio; simultaneously moves narrative authority from cash-flow discipline to technology-roadmap credibility.
% ABSENT_VOICES: DCF practitioners are structurally absent from private tender rooms — they would argue the implied multiples are unsupported but cannot access the data or the table. Future generations who inherit the opportunity costs of capital committed to Mars-scale bets are absent by construction. Minority-position holders who might demand governance concessions in exchange for capital are absent because the option framing makes their concession unnecessary.
% DISAPPEARANCE_RATIONALE: If option-space legitimacy vanished overnight, the next raise would reprice sharply downward or fail, capital formation for frontier deep-tech would reorganize around either strict cash-flow discipline (smaller, slower rounds) or explicit founder-track-record framings, and every imitator fundraising on option narratives across AI, space, and deeptech would lose its anchor pricing precedent.
% FOUNDING_PROBLEM: Assets whose value lies in unrealized technological options were unpriceable under discounted cash flow: the expected cash flows of speculative rockets and satellite constellations discounted at market rates approach zero, so capital markets systematically starved long-horizon, high-variance engineering programs. Real options theory offered a rigorous alternative that prices flexibility and variance instead of discounting it away.
% FOUNDING_PROBLEM_CORROBORATION: The academic finance literature on real options and investment under uncertainty (the Dixit-Pindyck tradition and subsequent venture-economics research) corroborates the pricing problem from outside the benefiting parties, as does the historical record of launch-industry capital starvation before reusable-rocket economics were demonstrated. These sources attest the founding problem is real and still unsolved by cash-flow methods; none of them attests that any particular current price is correct.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).
:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.36 at interval end) rather than low because although entrants are nominally informed risk-takers, the arrangement's operation transfers tail risk upward through the cap table: each raise at a higher mark converts earlier-paper into realized partial exits while new money absorbs the unexercised-option risk, and the framework's dominance suppresses the dilution pressure that would otherwise force governance concessions. Suppression is moderate-low (0.28) and purely structural: information control by a private issuer, social-professional delegitimation of cash-flow analysis inside deal rooms, and lockup design — there is no interpersonal or internalized component to weight. Theater ratio (0.28) reflects the share of valuation activity that is performative — TAM slide decks, Monte Carlo dress rehearsals over intuition — anchored against genuine engineering milestones (reusable boosters, subscriber-scale constellation revenue) that keep the ratio well below proxy-collapse levels. Accessibility collapse is low (0.25): DCF analysis, public-market comparables, and simply declining to invest all remain fully available; nothing collapses when the framework is understood. Resistance is substantial (0.58): an organized analyst community, governance activists, and skeptical press actively contest the pricing, which is why the framework requires active enforcement. The measurement series run on one shared six-point grid (2015-2025, biennial) so every tracked metric is authored at every examined time point; the series smooth the step-function reality of tender-round marks, which arrive discretely every 12-24 months — the underlying cycle is round-driven, not oscillatory in the intermittent-reinforcement sense.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the controller's seat the arrangement is a rope it built and maintains: a legitimate pricing language that funded what cash-flow markets refused to fund. From the late-entrant and LP seats the same structure operates as enforced risk transfer — they hold the variance, the controller holds the votes and the exits. From the excluded analyst seat the constraint is experienced as suppression of a competing discipline. The engine computes these per-seat classifications from power, exit, and directional position; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The controller sits nearest the beneficiary pole (d near 0.05): the framework subsidizes his financing terms directly, and identity-lock removes any disciplining exit threat. Early cap-table investors sit low (d near 0.2): they collect mark appreciation and supply credibility, bearing only illiquidity. Employee equity holders are genuinely dual-positioned — the derivation reads their beneficiary declaration, but their concentrated, non-voting, window-bound exposure pulls them toward symmetric (d near 0.45), which is why they carry a secondary payer role. Late-stage entrants sit near the target pole (d near 0.85): they pay the transfer and hold the tail risk with trapped exit. Fund LPs sit similarly high (d near 0.8) with the added friction of fee layers on marked values. The excluded analyst community and the regulator carry no directional position in the transfer itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pricing assets dominated by unrealized options — is still live: cash-flow methods genuinely cannot price this portfolio, and the academic literature corroborates the gap from outside the benefiting parties. The mandate has therefore NOT outlived its function, and mandatrophy_resolved is not declared. The drift risk to watch is theater accumulation: if engineering milestones stall while the narrative apparatus keeps expanding (rising theater_ratio on the authored series), the framework decays toward theatrical maintenance of a price its referents no longer support. Conversely, if Starlink-class proofs generalize across segments, the coordination function consolidates and the extraction share becomes the open question. Classification prevents mislabeling in both directions: calling this a pure rope ignores the identifiable payers absorbing mispriced variance; calling it a snare erases the genuine, externally corroborated coordination function that distinguishes it from narrative-only fundraising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the real_options_technologist reading of the valuation_legitimacy kernel; which structural elements would flip if a sibling reading (dcf_fundamentalist, musk_cult_believer, governance_skeptic) were instantiated instead?',
    'Cross-reading comparison across the four linked family stories: hold the referent (the standing option-priced arrangement) fixed and compare each reading''s authored epsilon, beneficiary/victim sets, and computed types.',
    'Under dcf_fundamentalist the same arrangement''s epsilon rises sharply (unproven options become uncompensated risk transfer); under governance_skeptic the victim set expands to minority holders and the 82.4% control block becomes the central fact; under musk_cult_believer epsilon falls toward zero. This file''s classification is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested valuation-legitimacy kernel; the disagreement is located in the evidentiary basis of price.').

omega_variable(
    tam_probability_calibration,
    'Is the roughly 6% probability weight on the $28.5T portfolio TAM well-calibrated, or does it embed optimism beyond base rates for multi-segment frontier-technology portfolios?',
    'Decadal outcome tracking against venture and portfolio base rates; independent replication of the option-tree inputs by analysts with access to Starlink unit economics and Starship test cadence data.',
    'If the calibrated probability is materially below 6%, late entrants are paying for mispriced variance and the authored extraction is understated; if above, the framework is conservative and extraction is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tam_probability_calibration, empirical, 'Calibration of the option-probability inputs underlying the headline price.').

omega_variable(
    integration_correlation_structure,
    'Does segment success actually raise sibling-segment probabilities (compounding optionality), or do segments share common failure modes — a single Starship architecture, key-person dependence — that correlate outcomes in both directions?',
    'Conditional-probability analysis of milestone interdependencies; stress tests that remove the common nodes (Starship, the controller) and recompute portfolio value.',
    'If correlations are common-mode, vertical integration concentrates rather than diversifies risk, the compounding multiplier collapses, and the arrangement shifts toward pure risk transfer with a coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_correlation_structure, empirical, 'Whether vertical integration compounds or concentrates option risk.').

omega_variable(
    intermediated_consent_victim_status,
    'Are late entrants and fund LPs genuinely informed principals accepting known risk/reward, or are they represented by agents — fund managers compensated on assets and markups — spending other people''s money with variance-seeking incentives?',
    'LP-side due-diligence records, fund-manager compensation structures, and post-write-down litigation patterns between LPs and sponsors.',
    'If the consent is agency-mediated, the effective victim set is far larger than the two declared groups, the reading''s informed-consent warrant fails, and both extraction and suppression revise upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediated_consent_victim_status, empirical, 'Whether the low-victim-set claim survives the principal-agent layer between LPs and the cap table.').

omega_variable(
    entrenchment_side_effect_allocation,
    'Does the option-space framework''s suppression of dilution pressure functionally entrench 82.4% voting control — a cost this reading books as neutral but the governance_skeptic reading books as the central extraction?',
    'Counterfactual cap-table analysis: reprice the last three raises under cash-flow-legitimate terms and compute the dilution and control distribution that would have resulted.',
    'If entrenchment is a systematic side effect, part of this reading''s low-extraction assessment is subsidized by a cost routed to a sibling ledger, and cross-reading reconciliation of the family''s epsilon values is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_side_effect_allocation, conceptual, 'Where the control-entrenchment cost sits across the kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2015, valuation_legitimacy__real_options_technologist, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(valu_tr_t2015, observed).
narrative_ontology:measurement(valu_tr_t2017, valuation_legitimacy__real_options_technologist, theater_ratio, 2017, 0.15).
narrative_ontology:measurement_basis(valu_tr_t2017, observed).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__real_options_technologist, theater_ratio, 2019, 0.19).
narrative_ontology:measurement_basis(valu_tr_t2019, observed).
narrative_ontology:measurement(valu_tr_t2021, valuation_legitimacy__real_options_technologist, theater_ratio, 2021, 0.23).
narrative_ontology:measurement_basis(valu_tr_t2021, observed).
narrative_ontology:measurement(valu_tr_t2023, valuation_legitimacy__real_options_technologist, theater_ratio, 2023, 0.26).
narrative_ontology:measurement_basis(valu_tr_t2023, observed).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__real_options_technologist, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(valu_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(valu_be_t2015, valuation_legitimacy__real_options_technologist, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement_basis(valu_be_t2015, observed).
narrative_ontology:measurement(valu_be_t2017, valuation_legitimacy__real_options_technologist, base_extractiveness, 2017, 0.2).
narrative_ontology:measurement_basis(valu_be_t2017, observed).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__real_options_technologist, base_extractiveness, 2019, 0.24).
narrative_ontology:measurement_basis(valu_be_t2019, observed).
narrative_ontology:measurement(valu_be_t2021, valuation_legitimacy__real_options_technologist, base_extractiveness, 2021, 0.29).
narrative_ontology:measurement_basis(valu_be_t2021, observed).
narrative_ontology:measurement(valu_be_t2023, valuation_legitimacy__real_options_technologist, base_extractiveness, 2023, 0.33).
narrative_ontology:measurement_basis(valu_be_t2023, observed).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__real_options_technologist, base_extractiveness, 2025, 0.36).
narrative_ontology:measurement_basis(valu_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2015, valuation_legitimacy__real_options_technologist, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement_basis(valu_su_t2015, observed).
narrative_ontology:measurement(valu_su_t2017, valuation_legitimacy__real_options_technologist, suppression_requirement, 2017, 0.22).
narrative_ontology:measurement_basis(valu_su_t2017, observed).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__real_options_technologist, suppression_requirement, 2019, 0.24).
narrative_ontology:measurement_basis(valu_su_t2019, observed).
narrative_ontology:measurement(valu_su_t2021, valuation_legitimacy__real_options_technologist, suppression_requirement, 2021, 0.26).
narrative_ontology:measurement_basis(valu_su_t2021, observed).
narrative_ontology:measurement(valu_su_t2023, valuation_legitimacy__real_options_technologist, suppression_requirement, 2023, 0.27).
narrative_ontology:measurement_basis(valu_su_t2023, observed).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__real_options_technologist, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(valu_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% The colloquial label 'valuation legitimacy' covers four structurally distinct claims about what grounds a price, decomposed per the epsilon-invariance principle into a four-story constraint family sharing one kernel (valuation_legitimacy) and one referent (the standing option-priced arrangement of the vertically integrated space company). Each member authors its own epsilon over that fixed referent — the values differ because the readings differ, not because any single reading measures with multiple observables. This story (real_options_technologist) is the mid-chain member: the dcf_fundamentalist story contests its evidentiary basis, the musk_cult_believer story supplies the track-record prior its probability inputs lean on, and the governance_skeptic story books the control-entrenchment cost this reading leaves off its own ledger. Contamination propagates across these edges: a demonstrated option exercise (Starlink-class proof) strengthens this reading and the cult reading simultaneously while weakening the DCF reading's grip on the referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
