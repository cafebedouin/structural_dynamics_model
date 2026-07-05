% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__dcf_fundamentalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__dcf_fundamentalist, []).

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
 *   constraint_id: valuation_legitimacy__dcf_fundamentalist
 *   human_readable: DCF Fundamentalist Reading of Space-Tech Valuation Legitimacy
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the DCF-fundamentalist reading of the contested
 *   valuation-legitimacy kernel applied to a vertically integrated
 *   space-technology enterprise. Under this reading, legitimate valuation is
 *   anchored to discounted proven cash flows: Starlink's approximately $4.4B
 *   operating profit supports a valuation in the $44-88B range at
 *   conventional multiples, while orbital AI and Mars colonization programs
 *   are unproven R&D bets properly priced as options with near-zero present
 *   cash-flow value, not as revenue-generating assets. Against this standard,
 *   a $1.75T valuation on $18.7B revenue and a $4.9B net loss represents
 *   roughly 93x revenue with negative earnings — a multiple this reading
 *   treats as fundamentally unjustifiable by cash-flow analysis. This is ONE
 *   of four declared readings of the same kernel; the
 *   real-options-technologist reading treats the same speculative programs as
 *   compounding option value rather than valueless R&D, the
 *   musk-cult-believer reading treats financial metrics as lagging indicators
 *   subordinate to founder track record, and the governance-skeptic reading
 *   focuses on voting-control extraction rather than cash-flow analysis. Each
 *   is a separate constraint with its own epsilon; this file speaks only for
 *   the DCF-fundamentalist claim.
 *
 * KEY AGENTS:
 *   - controlling_founder_musk: primary beneficiary (institutional/arbitrage) — liquidates control premium sustained by narrative valuation
 *   - early_venture_investors and pre_ipo_insiders: secondary beneficiaries (organized-powerful/arbitrage-mobile) — exit at peak before cash-flow reality is priced in
 *   - public_retail_investors and late_stage_index_fund_holders: primary victims (powerless/trapped) — bear the correction when DCF discipline eventually reasserts
 *   - equity_research_analysts: analytical observer — applies DCF discipline but lacks price-moving power against dominant control structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.78).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.55).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.78).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, snare).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF Fundamentalist Reading of Space-Tech Valuation Legitimacy").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, 'e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb').
narrative_ontology:cs_kernel_codification('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', distributed).
narrative_ontology:cs_authority_grounding('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', distributed).
narrative_ontology:cs_reading_relation('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', valuation_legitimacy__real_options_technologist, forecloses).
narrative_ontology:cs_reading_relation('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', foundational, unproven_technology_has_zero_present_cash_flow_value).
narrative_ontology:cs_axiom_status(unproven_technology_has_zero_present_cash_flow_value, holdable).
narrative_ontology:cs_axiom_grounding('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', unproven_technology_has_zero_present_cash_flow_value, conventional).
narrative_ontology:cs_axiom('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', secondary, revenue_multiple_ceiling_bounds_legitimate_valuation).
narrative_ontology:cs_axiom_status(revenue_multiple_ceiling_bounds_legitimate_valuation, holdable).
narrative_ontology:cs_axiom_grounding('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', revenue_multiple_ceiling_bounds_legitimate_valuation, empirically_contingent).
narrative_ontology:cs_reference_frame('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', cash_flow_discounting_primacy).
narrative_ontology:cs_drift_state('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', post_vertical_integration_valuation_surge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e02fbc2a-9d97-4c13-b9ec-38d7c66e0fbb', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_venture_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, pre_ipo_insiders).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, public_retail_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, late_stage_index_fund_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, employee_stockholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, employee_stockholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, discounted_cash_flow_primacy_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, revenue_multiple_sanity_bound).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds concentrated equity and voting control; can time secondary sales, pledge shares as collateral, and use the narrative of technological destiny (orbital AI, Mars colonization) to sustain a valuation far above what Starlink's actual operating profit would independently support. Liquidity events at peak valuation crystallize gains regardless of whether the speculative businesses ever generate matching cash flow.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk, beneficiary,
    institutional, biographical, arbitrage, global).

% Entered at valuations a fraction of the current mark; secondary markets and tender offers let them exit at the inflated multiple before any reckoning with revenue reality. Their information advantage and negotiated liquidity rights mean they are structurally positioned to sell into strength.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_venture_investors, beneficiary,
    organized, biographical, arbitrage, global).

% Employees and early backers with vested equity can sell in periodic tender windows priced off the same inflated mark, converting paper value into cash before public markets or independent auditors test the multiple against Starlink's actual $4.4B operating profit.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, pre_ipo_insiders, beneficiary,
    powerful, biographical, mobile, global).

% Buy in through secondary funds, SPAC-adjacent vehicles, or eventual public listing at a valuation implying 93x revenue with negative net income. They lack access to the private financials that would let them independently verify Starlink's segment profitability versus the loss-making segments; by the time cash-flow reality surfaces in public filings, exit is only possible at a loss.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, public_retail_investors, payer,
    powerless, biographical, trapped, global).

% Passive capital that will be forced to hold the position once it enters major indices, regardless of the DCF-implied overvaluation; their exposure is structural and non-negotiable, driven by fund mandates rather than any assessment of the underlying cash flows.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, late_stage_index_fund_holders, payer,
    powerless, generational, trapped, global).

% Compensated substantially in equity marked at the inflated valuation; they benefit on paper but face lockups, blackout windows, and concentration risk that leave them exposed if the DCF-implied correction arrives before they can diversify. Some cash out early alongside insiders; most remain trapped in concentrated, overvalued equity.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, employee_stockholders, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, employee_stockholders, beneficiary).

% Publish valuation models; some apply strict DCF discounting on proven segments (Starlink) and treat orbital AI and Mars colonization as unpriced options with no cash-flow basis, producing target valuations dramatically below the traded mark. Their reports are read by institutional allocators but do not by themselves move the price against a dominant control structure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, equity_research_analysts, observer,
    moderate, immediate, analytical, national).

% Could compel disclosure standards that force segment-level cash-flow reporting, but have limited statutory basis to challenge a valuation multiple itself absent fraud; their absence from the pricing conversation lets the speculative narrative persist unchecked in the near term.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, securities_regulators, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Discounted cash flow analysis solves a genuine problem: it gives dispersed investors a common, auditable method to compare the present value of different assets' future cash generation, rather than pricing purely on narrative or sentiment.
% TRANSFER_FUNCTION: Under this reading, the DCF-implied gap between the traded price and cash-flow-justified value transfers wealth from investors who buy at the inflated mark to those who sell into it — founder, early investors, and insiders realize gains that later buyers' capital ultimately funds.
% ABSENT_VOICES: Independent forensic accountants and short-sellers with segment-level cost data would object loudly to treating unproven R&D as revenue-equivalent asset value, but their access to non-public financials is limited and their public reports are drowned out by the dominant narrative; securities regulators who could mandate segment disclosure are also structurally absent from the pricing conversation.
% DISAPPEARANCE_RATIONALE: If DCF discipline were actually applied and enforced as the valuation standard, the traded price would compress toward the Starlink-justified range (roughly $44-88B), erasing well over a trillion dollars of nominal market capitalization; insiders would lose the ability to liquidate at the inflated mark, and public holders currently underwater would see the loss crystallized rather than deferred.
% FOUNDING_PROBLEM: DCF methodology was built to solve the problem of pricing assets on verifiable, repeatable cash generation rather than on story, hype, or founder charisma — a discipline imposed after episodes of speculative mispricing (dot-com era, SPAC boom) where narrative valuation detached from fundamentals and retail investors absorbed the correction.
% FOUNDING_PROBLEM_CORROBORATION: Independent equity research desks and academic corporate-finance literature (outside the company's own investor relations function) continue to apply and defend cash-flow discounting as the operative standard; the problem DCF was built to solve — narrative-driven overvaluation harming dispersed shareholders — is attested as live by these outside analysts even as it goes unaddressed in this specific valuation.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__dcf_fundamentalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__dcf_fundamentalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising because the gap between the DCF-justified valuation band and the traded price widens as the speculative narrative compounds while cash-flow fundamentals (an $4.9B net loss) do not correspondingly improve. Theater ratio is authored substantial and rising (0.62) because an increasing share of investor communication emphasizes the option-value narrative (orbital AI, Mars) rather than the audited cash-flow segments that would actually justify the price under this reading's own standard. Suppression is moderate (0.55): there is no coercive barrier to applying DCF methodology, but concentrated voting control and information asymmetry about segment profitability make it hard for dispersed holders to act on a DCF-based judgment even once they form one. Accessibility collapse is authored lower (0.4) precisely because DCF alternatives to the prevailing narrative valuation are freely available and publicly argued by analysts — this is not a mountain-like collapse of alternatives, it is a contested claim resisted by a well-resourced counter-narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the founder/insider seat, the valuation reflects legitimate optionality and technological achievement, not extraction; from the DCF-fundamentalist seat instantiated here, the same structure is a wealth transfer from later, less-informed buyers to earlier, better-positioned sellers, mediated by a narrative that resists straightforward cash-flow discounting. The engine computes these as different seat-level classifications from the same structural data; this story authors only the DCF-fundamentalist claim and its supporting metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder and early/pre-IPO insiders sit near the full-beneficiary end: they can time liquidity events against the inflated mark and their exit options are arbitrage-grade. Public retail investors and passive index holders sit near the full-target end: trapped exit, no segment-level information advantage, and structurally required exposure (index mandates) regardless of their own DCF judgment. Employee stockholders are genuinely dual-positioned — compensated in the inflated equity (a benefit) but exposed to concentration and lockup risk if the DCF-implied correction arrives (a cost) — hence the secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem DCF discipline was built to solve — narrative-driven overvaluation harming dispersed shareholders — remains live by this reading's own account (founding_problem_status: live), which is what makes the high extractiveness reading of the current valuation coherent rather than a mislabeled coordination function. This is not a case of a once-useful constraint whose function has disappeared; DCF discipline's coordination function (comparable, auditable valuation) persists, and this reading holds the mandate against the current valuation as unmet, not obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_value_vs_zero_value_disagreement,
    'Is the DCF-fundamentalist premise — that unproven orbital AI and Mars colonization programs carry near-zero present cash-flow value — itself correct, or does the real-options-technologist sibling reading''s treatment of them as compounding option value better capture their actual contribution to enterprise value?',
    'Independent real-options pricing analysis using observable inputs (R&D burn rate, time-to-market estimates, volatility of underlying technology outcomes) compared against realized outcomes over a 10-20 year horizon; if the programs eventually generate cash flows justifying a material fraction of the current premium, the DCF-fundamentalist zero-value treatment was too conservative.',
    'If option-value framing is vindicated, the extraction reading here overstates the wealth transfer — some of the premium reflects genuine forward-looking value the strict DCF lens is structurally blind to. If the programs never monetize, the DCF-fundamentalist reading is vindicated and the extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_vs_zero_value_disagreement, conceptual, 'Whether speculative R&D deserves option-value pricing or zero-value DCF treatment — the core premise dividing this reading from its real_options_technologist sibling.').

omega_variable(
    control_premium_vs_extraction_distinction,
    'Is founder liquidation at the inflated mark better characterized as legitimate control-premium realization (a normal feature of concentrated-founder equity) or as extraction facilitated by information asymmetry and narrative control?',
    'Comparison of insider sale timing and disclosure against segment-level financial releases; if sales cluster immediately before negative cash-flow disclosures or narrative-sensitive announcements, extraction is better supported than routine liquidity management.',
    'Determines whether the beneficiary/victim structure authored here (founder and insiders as beneficiaries, public/index holders as victims) reflects genuine asymmetric extraction or ordinary market liquidity dynamics that would exist under any valuation standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_vs_extraction_distinction, empirical, 'Whether insider liquidation timing evidences extraction or ordinary control-premium realization.').

omega_variable(
    which_reading_the_market_actually_prices,
    'Which of the four kernel readings does the traded market price actually reflect at any given time — is the $1.75T mark a DCF-rejected anomaly sustained by narrative, or does it reveal that market participants are pricing under the real-options or musk-cult-believer readings instead?',
    'Decompose analyst price targets by stated methodology (DCF vs. real-options vs. track-record-based) and weight by assets under management to infer the revealed dominant pricing framework; track convergence or divergence as segment financials mature.',
    'If the market is genuinely pricing under a coherent alternative reading (not merely momentum/narrative), this story''s snare classification may overstate coercive extraction — the price reflects a contested but internally coherent legitimacy standard, not a suppressed one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_market_actually_prices, conceptual, 'Which kernel reading the observed market price actually instantiates, versus which this story claims as normatively correct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.35).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 4, 0.42).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 8, 0.48).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 12, 0.53).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 16, 0.57).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 20, 0.6).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.78).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__dcf_fundamentalist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language claim 'valuation legitimacy' for this enterprise, per the ε-invariance principle: each reading (dcf_fundamentalist, real_options_technologist, musk_cult_believer, governance_skeptic) grounds legitimacy in a structurally distinct claim with its own epsilon, beneficiary/victim structure, and classification. This file grounds legitimacy in discounted cash flow analysis and computes high extraction; the real_options_technologist sibling grounds legitimacy in option-space present value and would compute materially lower extraction from the same traded price; the governance_skeptic sibling focuses on voting-control asymmetry rather than the cash-flow multiple. All four are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
