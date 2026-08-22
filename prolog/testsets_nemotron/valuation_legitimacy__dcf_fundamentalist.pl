% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__dcf_fundamentalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: DCF Fundamentalist Valuation Legitimacy Constraint
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   The DCF fundamentalist reading of valuation legitimacy asserts that at
 *   $18.7B revenue and $4.9B net loss, a $1.75T valuation (93x revenue,
 *   negative earnings) is fundamentally unjustifiable by any discounted cash
 *   flow model. Starlink's $4.4B operating profit supports a $44-88B
 *   valuation (10-20x earnings); orbital AI and Mars colonization are
 *   speculative R&D, properly valued as options (Black-Scholes, decision
 *   trees), not as DCF terminal value. The constraint extracts from public
 *   equity holders (index funds, retail, institutions) who buy at
 *   narrative-inflated prices, transferring wealth to Musk (liquidating
 *   control premium) and early investors (exiting at peak). The theater ratio
 *   rises as 'master plan' narratives and non-GAAP metrics increasingly
 *   decorate what is fundamentally a control-premium extraction mechanism.
 *   Suppression operates through index inclusion (forcing passive holders to
 *   participate), narrative control (media/brand as enforcement), and
 *   regulatory capture (SEC reluctance to challenge 'innovation').
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.85).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.68).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.85).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, snare).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF Fundamentalist Valuation Legitimacy Constraint").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, 'af6f1ab2-3c91-40b5-817d-2820b1ea7007').
narrative_ontology:cs_kernel_codification('af6f1ab2-3c91-40b5-817d-2820b1ea7007', implicit).
narrative_ontology:cs_authority_grounding('af6f1ab2-3c91-40b5-817d-2820b1ea7007', distributed).
narrative_ontology:cs_reading_relation('af6f1ab2-3c91-40b5-817d-2820b1ea7007', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('af6f1ab2-3c91-40b5-817d-2820b1ea7007', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_reading_relation('af6f1ab2-3c91-40b5-817d-2820b1ea7007', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('af6f1ab2-3c91-40b5-817d-2820b1ea7007', foundational, proven_cash_flow_necessary_for_valuation_legitimacy).
narrative_ontology:cs_axiom_status(proven_cash_flow_necessary_for_valuation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('af6f1ab2-3c91-40b5-817d-2820b1ea7007', proven_cash_flow_necessary_for_valuation_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('af6f1ab2-3c91-40b5-817d-2820b1ea7007', foundational, speculative_technologies_valued_as_options_not_terminal_value).
narrative_ontology:cs_axiom_status(speculative_technologies_valued_as_options_not_terminal_value, holdable).
narrative_ontology:cs_axiom_grounding('af6f1ab2-3c91-40b5-817d-2820b1ea7007', speculative_technologies_valued_as_options_not_terminal_value, empirically_contingent).
narrative_ontology:cs_reference_frame('af6f1ab2-3c91-40b5-817d-2820b1ea7007', graham_dodd_dcf_orthodoxy).
narrative_ontology:cs_drift_state('af6f1ab2-3c91-40b5-817d-2820b1ea7007', post_2020_meme_stock_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('af6f1ab2-3c91-40b5-817d-2820b1ea7007', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidators).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_investors_exiting_at_peak).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, public_equity_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, index_fund_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, retail_investors_buying_overvalued_equity).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, dcf_valuation_primacy).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, proven_cash_flow_supremacy).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, speculative_assets_are_options_not_assets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Musk and his inner circle control 82.4% voting power with 42% equity, enabling strategic share sales at peak valuations detached from fundamentals. They convert control premium into liquidity while retaining governance dominance. Exit is trivial: they set the terms of any transaction.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidators, beneficiary,
    institutional, biographical, arbitrage, global).

% Pre-IPO and early institutional investors who entered at genuine value inflection points (Starlink deployment, Starship milestones) now exit at valuations 10-20x beyond any DCF justification. They capture the option-value markup as realized gains. Exit is straightforward: public markets provide deep liquidity at current prices.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_investors_exiting_at_peak, beneficiary,
    powerful, immediate, mobile, global).

% Institutional and retail buyers purchasing TSLA/SpaceX-equivalent exposure at 93x revenue with negative earnings. They bear the cost of the control premium and narrative markup. Exit is constrained: selling crystallizes losses if the narrative holds, but holding risks catastrophic re-rating when cash-flow reality asserts itself.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, public_equity_investors, payer,
    organized, biographical, constrained, global).

% Passive investors forced to hold via S&P 500 inclusion and index methodology. They cannot exit without abandoning the index strategy. They absorb the valuation premium with no voice in governance. Their situation is structural captivity to index rules that treat market cap as truth.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, index_fund_holders, payer,
    powerless, generational, trapped, global).

% Individual investors drawn by narrative, brand loyalty, or FOMO who buy at peaks and provide liquidity for insider sales. They face asymmetric information and behavioral traps. Exit is psychologically constrained: the same narrative that drew them in makes selling feel like betrayal of the mission.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, retail_investors_buying_overvalued_equity, payer,
    powerless, immediate, constrained, global).

% Analysts and academics who maintain that valuation must anchor to discounted proven cash flows. They publish models showing fundamental impossibility of current pricing, but their models are treated as 'missing the optionality' rather than falsifying the valuation. They observe the constraint but cannot enforce their framework.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, valuation_analysts_dcf_camp, observer,
    analytical, generational, analytical, universal).

% Regulators who could require segment-level disclosure of Starlink vs. automotive vs. speculative R&D cash flows, or challenge non-GAAP metrics that obscure the earnings reality. They have been captured by the 'innovation narrative' and face political pressure not to disrupt 'national champions.' Their exclusion is voluntary: they choose not to use existing authority.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, sec_and_accounting_standard_setters, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared valuation language (DCF) that allows capital allocation across time and risk profiles, enabling investors to price proven cash flows comparably and allocate capital to its highest verified return.
% TRANSFER_FUNCTION: Moves capital from public equity buyers (index funds, retail, institutions) to insiders and early investors liquidating at narrative-inflated valuations. The transfer vehicle is the control premium embedded in share price: 93x revenue with negative earnings vs. 10-20x earnings for the only profitable segment (Starlink).
% ABSENT_VOICES: Future capital allocators who will bear the cost when the valuation re-rates to cash-flow reality. SpaceX employees holding illiquid equity who cannot exit. Competitors in launch/satellite/internet markets whose cost of capital is inflated by the distorted benchmark. None of these parties are in the room when the valuation is set.
% DISAPPEARANCE_RATIONALE: If the DCF legitimacy constraint vanished overnight, TSLA would re-rate to a 10-20x earnings multiple on Starlink's $4.4B operating profit ($44-88B enterprise value) plus option value for FSD/Optimus/Starship priced as genuine options (Black-Scholes, not DCF terminal value). Musk's control premium would collapse. Index funds would shed ~$1.6T in market cap. Capital would reallocate to companies with proven cash flows. The entire Musk industrial complex's financing model would break.
% FOUNDING_PROBLEM: Early corporate finance needed a method to value going concerns with predictable cash flows, replacing speculative par-value and asset-based approaches. DCF provided a rigorous, arbitrage-free framework for capital budgeting and equity valuation.
% FOUNDING_PROBLEM_CORROBORATION: The DCF framework's founding problem (valuing proven cash flows) is corroborated by every finance textbook and CFA curriculum. The contention is whether the framework *exhausts* valuation legitimacy. Aswath Damodaran (NYU Stern, independent of Musk interests) argues DCF can incorporate optionality via decision trees and real options — but the current TSLA valuation exceeds even generous real-options bounds. No credible independent analyst corroborates 93x revenue with negative earnings as a DCF outcome.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.85) reflects the extreme gap between DCF-justifiable value (~$44-88B for Starlink + option value for speculation) and market price ($1.75T). The 93x revenue multiple with negative earnings has no DCF precedent. Suppression (0.68) is structural: index methodology forces passive participation, dual-class structure prevents governance challenges, and narrative dominance marginalizes fundamental critique. Theater ratio (0.52) captures the growing share of communication devoted to 'master plan' storytelling vs. actual cash-flow disclosure. Accessibility collapse (0.35) is moderate: alternatives (shorting, avoiding, indexing ex-TSLA) exist but are costly or imperfect. Resistance (0.42) reflects persistent short interest, critical analyst coverage, and growing regulatory scrutiny — but the constraint persists and grows.
 *
 * PERSPECTIVAL GAP:
 *   From the DCF fundamentalist seat, the constraint is a snare: pure extraction masquerading as valuation methodology. From the Musk cult believer seat, the same structure is a mountain: Musk's track record makes DCF irrelevant. From the real options seat, it's a tangled rope: genuine optionality exists but is mispriced. From the governance skeptic seat, it's a snare with different victims (minority shareholders) and beneficiaries (controller). The engine computes these per-seat classifications from the structural data — this reading instantiates the DCF fundamentalist seat's structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk/control circle and early investors are structural beneficiaries (d near 0.0): they capture the control premium and narrative markup as realized liquidity. Public equity holders (index funds, retail, institutions) are structural targets (d near 1.0): they provide the liquidity and bear the re-rating risk. Index fund holders are trapped (exit_options: trapped) by methodology; retail are constrained by psychology and information asymmetry. Valuation analysts (analytical seat) observe but cannot enforce DCF discipline. Regulators (excluded) have authority but choose not to exercise it — their exclusion is endogenous to the constraint's power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (valuing proven cash flows) is live for mature businesses but dead for pre-revenue speculative ventures — yet the DCF framework is applied universally as the *only* legitimate valuation language. This mandate atrophy enables the extraction: by insisting DCF is the sole legitimacy test, the constraint forces speculative ventures into a framework that cannot value them, then allows narrative to fill the vacuum. The mandatrophy is unresolved: the framework persists as a legitimacy gate while its domain of valid application has shrunk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dcf_vs_real_options_boundary,
    'Can a rigorous real-options valuation (decision trees, Black-Scholes on Starlink/FSD/Starship optionality) bridge the gap to $1.75T, or does the gap remain irreducibly extractive?',
    'Independent real-options analysis by analysts with no Musk financial interest, using conservative volatility and time-to-maturity assumptions for each option leg (Starlink deployment, FSD regulatory approval, Starship orbital refueling, Mars transport). Compare aggregate option value to DCF baseline.',
    'If real-options valuation reaches >$500B, the extraction claim weakens (tangled rope). If it remains <$200B, the $1.75T valuation is >80% extraction (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dcf_vs_real_options_boundary, empirical, 'Whether the valuation gap is explicable by genuine optionality or is pure extraction').

omega_variable(
    index_inclusion_as_suppression_mechanism,
    'Is S&P 500 inclusion a structural suppression mechanism (forcing passive capital to provide liquidity for insider sales) or a neutral consequence of market cap rules?',
    'Analyze post-inclusion flow data: did index fund buying provide the liquidity for accelerated insider sales? Compare insider selling velocity pre/post inclusion. Model counterfactual: without index inclusion, what would the float and price trajectory be?',
    'If inclusion is suppression, the constraint''s extraction is structurally amplified by passive investing plumbing — a systemic feature, not firm-specific. If neutral, suppression is firm-specific narrative control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_inclusion_as_suppression_mechanism, empirical, 'Whether index methodology functions as extraction infrastructure').

omega_variable(
    starlink_cash_flow_visibility,
    'What are Starlink''s actual standalone financials (revenue, operating profit, capex, free cash flow) separated from SpaceX launch and R&D?',
    'Regulatory compulsion (SEC segment reporting) or voluntary disclosure. Starlink is now a distinct business unit with separate P&L — the data exists but is not public.',
    'If Starlink FCF supports >$100B valuation, the extraction ratio drops. If Starlink FCF is <$2B (implying <$40B valuation at 20x), the extraction ratio rises toward 95%+. This is the single most consequential empirical variable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(starlink_cash_flow_visibility, empirical, 'The true cash-flow anchor for the only profitable segment').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''valuation_legitimacy'' kernel admit a single coherent framing, or do the sibling readings operate on fundamentally different ontologies of what ''valuation'' *is* (price discovery vs. capital allocation vs. narrative coordination)?',
    'Philosophical analysis of the term ''valuation'' across the four readings. Map each reading''s implicit definition: DCF = present value of verified cash flows; real options = PV of decision rights; cult = PV of founder credibility; governance = PV of protected cash flows. If definitions are incommensurate, the kernel is a category error.',
    'If the kernel is a category error, the four readings are not ''readings of one kernel'' but four distinct constraints mislabeled by a shared word. The engine should treat them as separate constraint families with no network edges. This would invalidate the committer frame for this kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a coherent structural object or a linguistic trap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.22).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t48, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 48, 0.35).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t72, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 72, 0.44).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t96, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 96, 0.49).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_tr_t120, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 120, 0.52).

% Extraction over time
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t48, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 48, 0.71).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t72, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 72, 0.79).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t96, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 96, 0.82).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_be_t120, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 120, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t0, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t24, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t48, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 48, 0.55).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t72, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 72, 0.61).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t96, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 96, 0.65).
narrative_ontology:measurement(valuation_legitimacy__dcf_fundamentalist_su_t120, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 120, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, spacex_starlink_valuation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, tesla_fsd_option_valuation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, public_market_index_methodology).

% DUAL FORMULATION NOTE:
% Valuation legitimacy kernel decomposes into four constraint stories with irreducibly different ε: DCF fundamentalist (ε=0.85, snare), real options technologist (ε≈0.35, tangled rope), Musk cult believer (ε≈0.15, rope from believer seat / snare from outsider seat), governance skeptic (ε≈0.75, snare). The DCF reading provides the cash-flow anchor that the real options reading builds on; the Musk cult reading provides the narrative that inflates both; the governance reading targets the control structure that enables the extraction. All four are linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, institutional, 0.12).
constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, powerful, 0.18).
constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, organized, 0.85).
constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
