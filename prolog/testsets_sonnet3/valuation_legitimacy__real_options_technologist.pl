% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Real-Options Valuation Legitimacy for Vertically Integrated Space Technology Portfolio
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story authors the real-options-technologist reading of a contested
 *   valuation kernel around a vertically integrated space technology
 *   enterprise. Under this reading, the $1.75T valuation is legitimate
 *   because it prices the present value of a portfolio of real options
 *   (proven Starlink cash flows, high-variance Starship, unproven orbital
 *   compute, speculative lunar economy, civilizational-hedge Mars) whose
 *   vertical integration creates genuine compounding optionality: progress in
 *   reusable launch lowers the marginal cost of pursuing every downstream
 *   segment. This is NOT a claim that the valuation is fully justified by
 *   current cash flows (that is the dcf_fundamentalist sibling reading), NOT
 *   a claim that legitimacy rests on founder track record independent of
 *   financial structure (musk_cult_believer), and NOT a claim centered on the
 *   governance harms of concentrated voting control (governance_skeptic).
 *   Each of those is a structurally distinct constraint with its own ε and
 *   stakeholder set, linked via network.affects_constraints. This reading's ε
 *   is authored as moderate (0.38): the coordination function (vertical
 *   integration genuinely compounding technical optionality) is real, but the
 *   specific probability-weighting (~6%) and TAM figure ($28.5T) used to
 *   justify the mark are self-asserted by parties who benefit from the mark
 *   holding, and the gap between later-stage buyers' information and
 *   founder/early-investor information constitutes a real, if not extreme,
 *   extraction channel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.38).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.22).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.38).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real-Options Valuation Legitimacy for Vertically Integrated Space Technology Portfolio").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__real_options_technologist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, '8532f99f-99fb-4eff-bec4-f8587d62674b').
narrative_ontology:cs_kernel_codification('8532f99f-99fb-4eff-bec4-f8587d62674b', distributed).
narrative_ontology:cs_authority_grounding('8532f99f-99fb-4eff-bec4-f8587d62674b', distributed).
narrative_ontology:cs_reading_relation('8532f99f-99fb-4eff-bec4-f8587d62674b', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('8532f99f-99fb-4eff-bec4-f8587d62674b', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_reading_relation('8532f99f-99fb-4eff-bec4-f8587d62674b', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('8532f99f-99fb-4eff-bec4-f8587d62674b', foundational, technological_optionality_has_present_value_before_cashflow).
narrative_ontology:cs_axiom_status(technological_optionality_has_present_value_before_cashflow, holdable).
narrative_ontology:cs_axiom_grounding('8532f99f-99fb-4eff-bec4-f8587d62674b', technological_optionality_has_present_value_before_cashflow, instrumental).
narrative_ontology:cs_axiom('8532f99f-99fb-4eff-bec4-f8587d62674b', secondary, vertical_integration_compounds_cross_segment_success_probability).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_cross_segment_success_probability, holdable).
narrative_ontology:cs_axiom_grounding('8532f99f-99fb-4eff-bec4-f8587d62674b', vertical_integration_compounds_cross_segment_success_probability, empirically_contingent).
narrative_ontology:cs_reference_frame('8532f99f-99fb-4eff-bec4-f8587d62674b', real_options_finance_theory_applied_to_frontier_technology).
narrative_ontology:cs_drift_state('8532f99f-99fb-4eff-bec4-f8587d62674b', post_1_75t_valuation_mark, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8532f99f-99fb-4eff-bec4-f8587d62674b', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, founder_controlling_shareholder).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_and_growth_stage_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, employees_holding_equity).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_multiplanetary_hedge_beneficiaries).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_stage_secondary_market_buyers).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, minority_shareholders_without_board_influence).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, employee_equity_holders_with_illiquid_stakes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, employees_holding_equity).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounds_optionality).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, technological_option_space_prices_present_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% voting control with 42% equity, sets the narrative of the option portfolio (Starlink, Starship, orbital compute, lunar economy, Mars), decides capital allocation across segments, and controls the timing and structure of any liquidity event. Can reprice the option story to justify further capital raises at escalating valuations, and personally captures upside disproportionate to equity share through control premium and follow-on issuance terms.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, founder_controlling_shareholder, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, founder_controlling_shareholder, beneficiary).

% Entered at valuations that priced the option portfolio conservatively relative to current $1.75T marks. Benefit from markups on paper and from structuring rights (board seats, information rights, pro-rata) that let them monitor and sometimes influence capital allocation. Exit is constrained to secondary sales or eventual liquidity event but they have negotiated protective terms unavailable to later entrants.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_and_growth_stage_investors, beneficiary,
    organized, generational, constrained, global).

% Compensated substantially in equity whose value depends entirely on the option-portfolio narrative holding. Cannot freely sell (transfer restrictions, tender-offer-only liquidity), cannot diversify away from concentration risk, and have limited visibility into the true probability weighting behind the $28.5T TAM claim. Benefit if the story holds through a liquidity event; bear illiquidity and concentration risk that outside investors with diversified portfolios do not.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, employees_holding_equity, beneficiary,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, employees_holding_equity, payer).

% A diffuse, non-organized beneficiary class: if the option portfolio pays off (functioning Starship, orbital compute addressing the power gap, lunar/Mars settlement), the resulting civilizational insurance and technology spillovers accrue broadly, not just to shareholders. Named for completeness of the coordination claim, not as an agent capable of monitoring or enforcing anything.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, humanity_multiplanetary_hedge_beneficiaries, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, humanity_multiplanetary_hedge_beneficiaries).

% Buy in at $1.75T valuations that already price in roughly 6% probability-weighted success across a five-segment option portfolio. If the probability-weighting is optimistic, or if any single high-variance segment (Starship reliability, orbital compute demand, lunar first-mover economics) underdelivers, they absorb markdown risk with far less information than the founder or early investors about true technical progress. Secondary market pricing depends on continued narrative credibility they cannot independently verify.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_stage_secondary_market_buyers, payer,
    powerful, biographical, constrained, global).

% Hold equity without governance leverage given the 82.4% voting concentration. Cannot influence capital allocation decisions across the option portfolio, cannot compel independent verification of technical milestones underlying valuation claims, and depend entirely on the controlling shareholder's continued alignment of interests. Their claims are subordinate in practice to whatever capital structure decisions serve the controlling shareholder's broader ventures.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_shareholders_without_board_influence, payer,
    powerless, biographical, trapped, national).

% Distinct from the broader employee beneficiary framing: this is the specific cohort whose equity comp is marked at option-portfolio valuations they cannot sell except through company-controlled tender windows, and who bear career and compensation risk if the option narrative deflates before any liquidity event, without recourse to adjust their exposure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, employee_equity_holders_with_illiquid_stakes, payer,
    powerless, biographical, trapped, national).

% Produce independent(ish) valuations of the option portfolio using real-options and probability-weighted TAM methodologies. Their credibility depends on being seen as neutral, but their access to management guidance and desire for continued deal flow creates pressure toward the framing that sustains the highest defensible valuation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, sell_side_analysts_and_valuation_firms, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, founder_controlling_shareholder).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vertical integration genuinely coordinates capital, technical talent, and infrastructure across launch, satellite constellation, and prospective compute/lunar/Mars segments so that engineering learning and cost curves in one segment (reusable launch) lower the cost of pursuing the next (orbital compute, lunar logistics), which no single-segment competitor can replicate — this is a real technological interdependency, not merely a narrative device.
% TRANSFER_FUNCTION: Moves capital from later-stage investors, secondary market buyers, and equity-compensated employees toward the controlling shareholder and early investors, denominated in valuation marks that price probability-weighted optionality years or decades before cash flows materialize; also moves control premium disproportionately to the founder relative to his 42% cash-flow equity share via the 82.4% voting structure.
% ABSENT_VOICES: Skeptical independent auditors of technical milestone claims (e.g., independent verification of Starship flight cadence reliability, actual demand signals for orbital compute against the claimed 62 GW gap) are largely absent from the valuation-setting process; their perspective would discount the probability-weighting used to justify the $1.75T mark, but they have no seat at the table that sets valuation.
% DISAPPEARANCE_RATIONALE: If the real-options framing disappeared and only proven-cash-flow (DCF) valuation applied, the marked value would likely collapse toward Starlink's demonstrated EBITDA multiple alone, wiping out a large share of paper value for later investors and equity-comp employees, while founder and early investors who already realized liquidity or hold protective terms would be comparatively insulated — whether this counts as 'the world rearranging' or 'correcting to what was already true' is exactly what the sibling readings dispute.
% FOUNDING_PROBLEM: Standard DCF valuation cannot price technologies whose payoff is contingent on binary, multi-year technical milestones (reusable heavy-lift, orbital data centers, lunar logistics) where the option to scale is valuable even under high failure probability — real-options methodology exists to avoid systematically undervaluing genuine technological optionality.
% FOUNDING_PROBLEM_CORROBORATION: Academic real-options finance literature (outside SpaceX and its investors) corroborates that option-pricing frameworks are the correct tool for valuing contingent technology payoffs in principle; however, no corroborating source outside the company's own investor base and sell-side analysts with deal-flow incentives has independently validated the specific ~6% probability weighting or the $28.5T TAM figure used to justify the current mark — that specific number is self-asserted by parties who benefit from it holding.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, contested).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.38 rather than low or high: the coordination story is not pure cover (real technological interdependencies exist and are independently corroborated by real-options finance literature as the correct general framework), but the specific numbers used to price the option portfolio are unverifiable by outside parties and asymmetrically benefit the controlling shareholder and early investors who set and control the narrative. Theater ratio rises over the measured interval (0.20 to 0.40) as the valuation narrative increasingly leans on speculative segments (orbital compute, lunar economy, Mars) relative to the proven Starlink base, consistent with a drift toward narrative-substitution-for-verification as the total mark grows faster than verified milestones. Suppression is low (0.22) because no party is coerced into holding the equity or accepting the valuation framing — participation is voluntary and informed investors can and do exit via secondary markets, though at prices set by the same asymmetric-information structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder/controlling shareholder sits at the strong-beneficiary end: sets the narrative, controls capital allocation, and captures disproportionate upside via voting control exceeding cash-flow equity share. Early investors are beneficiaries with negotiated protections. Employees holding equity are beneficiaries in principle but payers in practice given illiquidity and concentration risk without corresponding governance voice — hence the dual role. Late-stage secondary buyers and non-board minority shareholders sit at the target end: they bear markdown risk from a valuation methodology whose key parameters they cannot independently verify, and their exit options are constrained by illiquid secondary markets and subordinate governance position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification, rather than snare or rope, is deliberate: this reading holds that the coordination function (vertical integration compounding real technological optionality) is genuine and would be mislabeled as pure extraction — but it also holds that the specific valuation numbers used to monetize that optionality are asymmetrically controlled and benefit concentrated parties at the expense of less-informed capital, which would be mislabeled as pure coordination if left unexamined. The classification prevents both errors: it does not dismiss the real engineering interdependencies as mere narrative, and it does not treat the $1.75T mark as a neutral, fully-informed market outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_weighting_provenance,
    'Is the ~6% probability-weighting applied to the $28.5T TAM across the option portfolio a defensible independent estimate, or a number reverse-engineered to justify the $1.75T mark?',
    'Independent replication of the real-options pricing model using disclosed technical milestone data (Starship flight reliability trend, actual orbital compute demand signals, lunar economy first-mover contracts) by parties without deal-flow incentive from the company.',
    'If the weighting is defensible and independently reproducible, this reading''s low-moderate ε is well-grounded. If it is reverse-engineered, the extraction channel is substantially larger than authored here and the classification would drift toward snare for the late-stage-buyer seat specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(probability_weighting_provenance, empirical, 'Whether the core probability-weighting number is independently verifiable or self-serving.').

omega_variable(
    vertical_integration_optionality_genuineness,
    'Does success in one segment (e.g., reusable launch) actually raise the probability of success in structurally distinct segments (orbital compute, lunar economy, Mars), or is ''compounding optionality'' primarily a narrative device linking otherwise independent bets?',
    'Technical audit of shared infrastructure, cost curves, and engineering talent flows across segments by independent aerospace economists; compare against counterfactual single-segment competitors'' cost trajectories.',
    'If genuine, the coordination half of the tangled_rope classification is strongly supported. If the segments are more independent than claimed, the vertical-integration premium is largely narrative and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vertical_integration_optionality_genuineness, empirical, 'Whether cross-segment optionality compounding is a real technical phenomenon or primarily rhetorical.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the four sibling readings of valuation_legitimacy (real-options-technologist, dcf-fundamentalist, musk-cult-believer, governance-skeptic) genuinely incommensurable value-frameworks that different investor classes hold simultaneously, or does one reading represent the ''true'' underlying structure with the others as partial or motivated framings?',
    'Track which reading''s predictions (valuation trajectory, milestone-contingent repricing, governance dispute outcomes) best match realized outcomes over a multi-year window as segments mature or fail.',
    'If one reading proves systematically more predictive, the framework''s kernel-reading pluralism should be revisited for this case; if all readings persist as live positions held by different investor classes regardless of outcomes, the coexists_with relations are validated as structurally correct rather than provisional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel readings are genuinely coexisting frameworks or one is structurally privileged.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__real_options_technologist, theater_ratio, 4, 0.24).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__real_options_technologist, theater_ratio, 8, 0.28).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.32).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__real_options_technologist, theater_ratio, 16, 0.35).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.38).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__real_options_technologist, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__real_options_technologist, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__real_options_technologist, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__real_options_technologist, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__real_options_technologist, base_extractiveness, 24, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the single natural-language label 'SpaceX valuation legitimacy dispute,' per the ε-invariance principle: measuring legitimacy via technological option-space present-value yields a materially different ε (0.38, moderate) than measuring via proven-cashflow discounting (dcf_fundamentalist, expected higher ε — treats unproven segments as unpriced options rather than assets in the base, implying more of the current mark is unearned), via founder track record (musk_cult_believer, expected lower ε — near-mountain framing where financial metrics are dismissed as lagging), or via governance protection adequacy (governance_skeptic, expected higher ε and explicit snare-adjacent framing centered on the 82.4%/42% voting-equity gap as the extraction mechanism itself). Each sibling is authored as its own file with its own stakeholders and metrics; this file does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
