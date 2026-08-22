% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__dcf_fundamentalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: valuation_legitimacy__dcf_fundamentalist
 *   human_readable: Speculative-Premium Valuation Regime (as assessed by the DCF-fundamentalist reading)
 *   domain: corporate finance / technology governance / space economics
 *
 * SUMMARY:
 *   A founder-controlled frontier-technology enterprise carries a headline
 *   valuation near 1.75 trillion against 18.7 billion of revenue and a 4.9
 *   billion net loss, roughly ninety-three times revenue with negative
 *   earnings. Beneath the headline sits a proven-cash-flow core (satellite
 *   broadband generating about 4.4 billion of operating profit, plus
 *   contracted launch services) that the reading's own discounting discipline
 *   values an order of magnitude lower; the remainder is carried at
 *   asset-level multiples on option-space programs (orbital AI compute, Mars
 *   logistics) that have produced no revenue. This story instantiates the
 *   dcf_fundamentalist reading of the valuation_legitimacy kernel: valuation
 *   legitimacy derives from discounting proven cash flows; unproven
 *   technologies are options, not assets. Consistent with the
 *   epsilon-referent rule for kernel readings, the referent of epsilon is the
 *   standing arrangement under contest, the speculative-premium valuation
 *   regime itself, assessed by this reading's own lights, and never the
 *   DCF-governed counterfactual this reading would install. The claimed_type
 *   states this reading's structural belief; the metrics are authored
 *   independently as descriptive facts about the standing arrangement's
 *   operation. KEY AGENTS (by structural relationship): -
 *   musk_control_premium_liquidator: Agenda-setter and primary recipient
 *   ([institutional]/[arbitrage]) — sets disclosure, times exits, collects
 *   the control premium - early_insider_investors: Secondary beneficiaries
 *   ([powerful]/[mobile]) — orderly secondaries at rising marks -
 *   investment_banking_underwriters: Fee-collecting beneficiaries
 *   ([institutional]/[arbitrage]) - retail_momentum_investors: Primary payers
 *   ([powerless]/[constrained]) — enter at peaks, absorb compression -
 *   passive_index_funds: Trapped payers ([institutional]/[trapped]) — carry
 *   the premium for retirement savers - dissenting_valuation_analysts:
 *   Excluded counter-framework holders ([moderate]/[constrained]) — outside
 *   the price-setting conversation - securities_regulators: Analytical
 *   observers ([institutional]/[analytical])
 *
 * KEY AGENTS:
 *   - musk_control_premium_liquidator — agenda-setter and primary recipient; institutional power, arbitrage-grade exit, generational horizon, global scope
 *   - early_insider_investors — beneficiary seat; powerful, mobile exit via negotiated secondaries, biographical horizon
 *   - investment_banking_underwriters — beneficiary seat; institutional, arbitrage, fee income scaled to headline size
 *   - retail_momentum_investors — payer seat; powerless, constrained exit, immediate horizon, global scope
 *   - passive_index_funds — payer seat; institutionally powerful but mandate-trapped, generational horizon
 *   - dissenting_valuation_analysts — excluded seat; moderate power, constrained exit through career and litigation exposure
 *   - securities_regulators — observer seat; institutional power, analytical stance, national scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.9).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.78).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.9).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "Speculative-Premium Valuation Regime (as assessed by the DCF-fundamentalist reading)").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate finance / technology governance / space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, '96155ced-ce89-43f6-bec2-17e3ba5d0c75').
narrative_ontology:cs_kernel_codification('96155ced-ce89-43f6-bec2-17e3ba5d0c75', formalized).
narrative_ontology:cs_authority_grounding('96155ced-ce89-43f6-bec2-17e3ba5d0c75', expertise).
narrative_ontology:cs_interpretation_layer_present('96155ced-ce89-43f6-bec2-17e3ba5d0c75').
narrative_ontology:cs_reading_relation('96155ced-ce89-43f6-bec2-17e3ba5d0c75', valuation_legitimacy__real_options_technologist, forecloses).
narrative_ontology:cs_reading_relation('96155ced-ce89-43f6-bec2-17e3ba5d0c75', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_reading_relation('96155ced-ce89-43f6-bec2-17e3ba5d0c75', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('96155ced-ce89-43f6-bec2-17e3ba5d0c75', foundational, valuation_legitimacy_requires_proven_cashflows).
narrative_ontology:cs_axiom_status(valuation_legitimacy_requires_proven_cashflows, holdable).
narrative_ontology:cs_axiom_grounding('96155ced-ce89-43f6-bec2-17e3ba5d0c75', valuation_legitimacy_requires_proven_cashflows, empirically_contingent).
narrative_ontology:cs_axiom('96155ced-ce89-43f6-bec2-17e3ba5d0c75', foundational, unproven_technology_is_option_not_asset).
narrative_ontology:cs_axiom_status(unproven_technology_is_option_not_asset, holdable).
narrative_ontology:cs_axiom_grounding('96155ced-ce89-43f6-bec2-17e3ba5d0c75', unproven_technology_is_option_not_asset, instrumental).
narrative_ontology:cs_reference_frame('96155ced-ce89-43f6-bec2-17e3ba5d0c75', discounted_proven_cashflow_standard).
narrative_ontology:cs_drift_state('96155ced-ce89-43f6-bec2-17e3ba5d0c75', contemporary_narrative_capital_markets, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96155ced-ce89-43f6-bec2-17e3ba5d0c75', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidator).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_insider_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, investment_banking_underwriters).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, retail_momentum_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, passive_index_funds).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, margin_of_safety_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, discounted_cash_flow_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founder-controller holding supervoting shares that convert a minority economic stake into commanding voting control. Sets the disclosure cadence and owns the social platform through which market-moving announcements reach investors, times secondary sales and capital raises against narrative peaks, pledges shares for personal liquidity, and directs capital among affiliated ventures. Exit is asymmetrically easy: shares can be sold into strength, borrowed against, and the personal brand travels across ventures regardless of any single company's price.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidator, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidator, beneficiary).

% Venture funds, sovereign vehicles, and angels from the earliest rounds. Receive staggered secondary-sale windows at successively higher marks; their realized returns are set by the narrative premium rather than by dividends or cash yield, none of which the company pays. Exit is negotiated and orderly: tender participations and structured secondaries arranged while sentiment is strong. Some recycle proceeds into follow-on rounds to preserve allocation access.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_insider_investors, beneficiary,
    powerful, biographical, mobile, global).

% Advisers and placement agents on capital raises, tender offers, and employee-share programs. Fees scale with headline transaction size, giving a standing incentive to validate elevated marks. They carry no lasting balance-sheet exposure after allocation and their downside is limited to episodic reputational cost.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, investment_banking_underwriters, beneficiary,
    institutional, biographical, arbitrage, global).

% Household investors who buy through brokerage apps at narrative peaks and hold concentrated, often margined positions. Their information diet is filtered through channels the controller owns or dominates, bear-case content is throttled or ridiculed in the communities they inhabit, and selling is socially coded as disloyalty. They can technically sell at any moment but do so at realized losses, frequently forced by margin calls at price bottoms. They hold no governance voice.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, retail_momentum_investors, payer,
    powerless, immediate, constrained, global).

% Benchmark-tracking managers obligated to hold index weights once the stock is included. They must mechanically buy into rises and sell into declines, cannot underweight without breaching mandate, and cast stewardship votes that are neutralized by the controller's supervoting block. The premium is carried on behalf of pension savers and retirement accounts who never chose the position.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, passive_index_funds, payer,
    institutional, generational, trapped, global).

% Analysts and short sellers who publish sum-of-the-parts models showing an order-of-magnitude gap between price and discounted proven cash flows. Their work circulates outside the price-setting conversation: platforms associated with the controller amplify mockery campaigns, litigation exposure attaches to public bear positions, and career incentives push coverage analysts toward hedged language or dropped coverage. They would reprice the stock within weeks if their framework governed legitimacy.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, dissenting_valuation_analysts, excluded,
    moderate, biographical, constrained, global).

% Disclosure and market-integrity authorities monitoring pledge disclosures, promotional statements, and manipulation patterns. Constrained by jurisdiction over offshore tender rounds, by the systemic optics of deflating a flagship listing held by pension money, and by the controller's litigation capacity and political footprint. They observe, probe, and occasionally fine, but do not set the valuation standard.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, musk_control_premium_liquidator).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools patient public capital at a scale private credit will not supply, financing decade-long, capital-intensive frontier engineering (reusable launch, satellite broadband manufacturing and deployment) through its loss-making years; provides continuous liquidity and price discovery through which early insiders can transition out and new capital cycles in.
% TRANSFER_FUNCTION: Moves purchasing power from late-cycle public entrants (app-era retail buyers and index-mandated funds) to the controller (share sales, borrowing capacity, and a control stake worth far more than its economic share), early insiders (staggered secondaries at rising marks), and underwriters (fees scaled to headline size), priced at roughly twenty times the value supportable by proven cash flows.
% ABSENT_VOICES: Dissenting valuation analysts and short sellers possess a fully articulated counter-account and are structurally outside the room: their framework is disqualified from price-setting legitimacy by platform control, ridicule dynamics, and litigation exposure, and minority shareholders lack any governance instrument that could force their models onto the record.
% DISAPPEARANCE_RATIONALE: If the speculative-premium arrangement vanished overnight (prices snapping to discounted proven cash flows), insider liquidity events would reprice immediately, recent retail cohorts would absorb the entire compression as realized wealth destruction, index funds would transmit losses to retirement systems, the cost of the next capital raise would multiply, and the pace of constellation expansion and Mars-program spending would drop to internally funded rates. Every named seat's position depends on the arrangement persisting.
% FOUNDING_PROBLEM: Frontier transportation and communications infrastructure requires a decade of losses before cash flows arrive, and no lender finances that shape; the arrangement was built to solve how to fund Mars-scale capital expenditure ahead of any provable revenue stream, by letting a narrative premium stand in for future cash flows during the buildout years.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and independent aerospace-industry analysts corroborate that the underlying problem was real and is the same one solved historically by transcontinental railroad and long-haul fiber manias: frontier infrastructure has repeatedly been financed by speculative premia that exceeded eventual cash flows. Those same outside sources dispute whether the problem persists in a form justifying current pricing, since the satellite-broadband segment has crossed into operating profitability and no longer needs narrative subsidy; no source outside the benefiting parties attests that 93x-revenue pricing is the necessary continuing solution.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.9, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.90 because the referent arrangement transfers roughly twenty times the value of its proven-cash-flow core from late entrants to insiders: even granting generous option value to unproven programs, the overwhelming share of headline capitalization is unsupported by any discountable stream. Suppression is authored at 0.78 as a raw structural property, deliberately unscaled: it comprises disclosure-channel ownership, throttling and ridicule of bear-case content, litigation exposure attached to public short positions, supervoting locks on governance voice, and index mechanics, and it is NOT amplified by power or scope the way extractiveness is. Theater_ratio sits at 0.60 because a growing majority of narrative output (program dates, capability demonstrations, roadmap revisions) functions to maintain the premium rather than to ship proven product, while genuine engineering continues underneath. Accessibility_collapse is moderate (0.40): an investor who accepts the reading retains real alternatives (abstain, diversify, short at cost), so alternatives do not fully vanish, unlike a natural law. Resistance is 0.55: an organized bear-case community, shareholder suits, and regulator interest meet the arrangement continuously and ineffectually. Claim and metrics are independent authored facts: I believe the standing arrangement is a tangled rope (a real financing coordination function carrying severe asymmetric extraction through the same structure, actively enforced); the engine's per-seat computations may diverge from that claim, and any such divergence is the datum the corpus exists to collect. Coalition note: the payer seats span powerless retail and trapped institutional money; retail coalition potential (class actions, collective refusal at tenders) is the main channel by which the weakest seat could acquire countervailing power, and it is presently dormant.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the agenda-setter's position the arrangement presents as earned coordination: he built capabilities competitors called impossible and reads the premium as the market finally pricing that achievement, so his computed type should look rope-flavored with near-zero directional extraction borne by himself. From the retail payer's seat the same structure computes as enforced overpayment with constrained exit and identity-fused holding. The passive index seat is institutionally powerful yet computes among the worst positions because mandate structure removes exit entirely, a same-side differentiation the derivation should surface from exit options alone. The excluded analyst seat, holding the counter-framework, computes the arrangement as a pure waiting game whose resolution is certain, which is the closest any seat comes to reading it as a natural-law outcome; the engine should register that certainty as a property of the seat's framework, not of the arrangement. The governance_skeptic sibling would relocate the entire analysis from price level to control structure, which is why that sibling is recorded as coexisting rather than foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   The controller declares as both agenda-setter and beneficiary with arbitrage-grade exit, placing him nearest the beneficiary pole (d near 0.05): the arrangement subsidizes him through cheap primary capital, borrowing capacity, and exit timing he controls. Early insiders (beneficiary, mobile) sit near d 0.10 and underwriters (beneficiary, arbitrage) near d 0.15; effective extraction is damped or inverted for all three. Retail payers (constrained exit) derive near d 0.85 and index funds (trapped) near the full-target pole, so effective extraction is amplified for them, further amplified by global scope raising verification difficulty. The suppressed analyst seat is neither beneficiary nor declared victim; its position enters through the exclusion structure rather than through a payment flow. Beneficiary and victim declarations map directly onto observable flows: secondaries and fees upward, purchase prices downward.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling the arrangement a pure snare erases the genuine coordination that built reusable launch and a profitable satellite-broadband segment during years when no lender would fund them; calling it a pure rope erases the late-cycle transfer in which the same capital-market machinery moves roughly twenty times fundamental value from entrants to insiders. The mandatrophy question is temporal: the founding problem (financing frontier capital expenditure ahead of cash flows) was live through the buildout years and is now contested, because the segment that justified patience has reached profitability while the premium's justification has migrated to permanently-unprovable programs. The measurement series records that migration directly: theater_ratio crosses 0.5 around 2024, marking the point where premium maintenance outweighs functional signaling. Because the founding problem is contested rather than dead, mandatrophy_resolved is not declared; the zombie-flag consumer should read the status-times-verdict combination (contested status, world_rearranges verdict) as indicating a live arrangement whose founding warrant is half-obsolete, with fixing_cost prohibitive because the parties who could correct it (regulators, exchanges, index committees) bear systemic-optics costs that exceed their stake in correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_location,
    'This story is one reading (dcf_fundamentalist) of the valuation_legitimacy kernel. Would instantiating a sibling reading relocate the victim and beneficiary sets so completely that no cross-reading comparison of epsilon is meaningful?',
    'Author each sibling as its own epsilon-invariant story and compare victim/beneficiary declarations: the musk_cult_believer reading dissolves the victim set entirely (late buyers are ''early'' by definition), the governance_skeptic reading relocates the harm from the price-versus-cash-flow gap to the control structure, and the real_options_technologist reading converts the unsupported premium into legitimately priced optionality.',
    'If sibling readings relocate rather than resize the victim set, the four stories form a genuine constraint family with incomparable epsilons; if they merely re-weight the same seats, a single meta-classification of the valuation regime becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_location, conceptual, 'Whether the four readings of the valuation-legitimacy kernel describe one arrangement or four distinct constraints.').

omega_variable(
    reflexive_premium_vs_convergence,
    'Is the gap between headline valuation and proven-cash-flow value a transient mispricing that must converge, or a reflexive loop in which the premium itself finances capability growth fast enough to close the gap?',
    'Track realized segment cash flows against the growth path implied by the premium over a full cycle; if implied growth is achieved, the premium partially self-validates and effective extraction falls toward coordination cost; if convergence occurs, the measured transfer from late entrants is confirmed as the arrangement''s dominant output.',
    'Resolution moves the classification boundary: self-validation pushes toward benign resource allocation; convergence confirms a tangled-rope-to-snare drift with public investors as the residual payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reflexive_premium_vs_convergence, empirical, 'Whether the speculative premium is self-validating or convergent.').

omega_variable(
    internalized_suppression_among_retail_holders,
    'How much of the retail holders'' inability to exit at narrative peaks is structural (information control, platform throttling of bear cases, margin mechanics) versus internalized (identity fusion with the founder community, social-media reinforcement making selling a betrayal of the group)?',
    'Post-exit trajectory study of former holders: if suppression measures persist after accounts are closed and information barriers bypassed, the internalized share is substantial; survey-based measurement of identity attachment versus information scarcity among holders.',
    'If internalized suppression dominates, the constraint''s hold survives removal of the structural enforcement machinery and decays only across a generational turnover of holders; classification severity rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_among_retail_holders, empirical, 'Structural versus internalized share of retail-holder suppression.').

omega_variable(
    proven_segment_value_share,
    'What fraction of the headline enterprise value is defensibly attributable to proven-cash-flow segments (satellite broadband, launch services) under the reading''s own discounting discipline, versus option space (orbital AI, Mars logistics) carried at asset-level multiples?',
    'Audited segment-level disclosure of revenue, operating profit, and capital intensity, combined with comparable-company discount-rate benchmarks; the reading''s own arithmetic places the proven core near 44-88 billion against a headline near 1.75 trillion.',
    'A verified order-of-magnitude gap anchors epsilon near its upper bound and supports the transfer diagnosis; a materially smaller gap would indicate the premium is concentrated in genuinely maturing segments and soften the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proven_segment_value_share, empirical, 'Share of headline value supported by proven cash flows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 2013, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vl_dcf_tr_t2013, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2013, 0.14).
narrative_ontology:measurement_basis(vl_dcf_tr_t2013, observed).
narrative_ontology:measurement(vl_dcf_tr_t2015, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2015, 0.17).
narrative_ontology:measurement_basis(vl_dcf_tr_t2015, observed).
narrative_ontology:measurement(vl_dcf_tr_t2018, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2018, 0.24).
narrative_ontology:measurement_basis(vl_dcf_tr_t2018, observed).
narrative_ontology:measurement(vl_dcf_tr_t2020, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2020, 0.34).
narrative_ontology:measurement_basis(vl_dcf_tr_t2020, observed).
narrative_ontology:measurement(vl_dcf_tr_t2022, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2022, 0.45).
narrative_ontology:measurement_basis(vl_dcf_tr_t2022, observed).
narrative_ontology:measurement(vl_dcf_tr_t2024, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2024, 0.53).
narrative_ontology:measurement_basis(vl_dcf_tr_t2024, observed).
narrative_ontology:measurement(vl_dcf_tr_t2026, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2026, 0.6).
narrative_ontology:measurement_basis(vl_dcf_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(vl_dcf_be_t2013, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2013, 0.22).
narrative_ontology:measurement_basis(vl_dcf_be_t2013, observed).
narrative_ontology:measurement(vl_dcf_be_t2015, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement_basis(vl_dcf_be_t2015, observed).
narrative_ontology:measurement(vl_dcf_be_t2018, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2018, 0.36).
narrative_ontology:measurement_basis(vl_dcf_be_t2018, observed).
narrative_ontology:measurement(vl_dcf_be_t2020, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(vl_dcf_be_t2020, observed).
narrative_ontology:measurement(vl_dcf_be_t2022, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement_basis(vl_dcf_be_t2022, observed).
narrative_ontology:measurement(vl_dcf_be_t2024, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2024, 0.79).
narrative_ontology:measurement_basis(vl_dcf_be_t2024, observed).
narrative_ontology:measurement(vl_dcf_be_t2026, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2026, 0.9).
narrative_ontology:measurement_basis(vl_dcf_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(vl_dcf_su_t2013, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2013, 0.15).
narrative_ontology:measurement_basis(vl_dcf_su_t2013, observed).
narrative_ontology:measurement(vl_dcf_su_t2015, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2015, 0.18).
narrative_ontology:measurement_basis(vl_dcf_su_t2015, observed).
narrative_ontology:measurement(vl_dcf_su_t2018, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2018, 0.26).
narrative_ontology:measurement_basis(vl_dcf_su_t2018, observed).
narrative_ontology:measurement(vl_dcf_su_t2020, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement_basis(vl_dcf_su_t2020, observed).
narrative_ontology:measurement(vl_dcf_su_t2022, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement_basis(vl_dcf_su_t2022, observed).
narrative_ontology:measurement(vl_dcf_su_t2024, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(vl_dcf_su_t2024, observed).
narrative_ontology:measurement(vl_dcf_su_t2026, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(vl_dcf_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial question 'is the trillion-plus valuation legitimate?' covers four structurally distinct claims and is decomposed into four stories sharing the valuation_legitimacy kernel. This story instantiates the dcf_fundamentalist reading and fixes its epsilon referent to the standing speculative-premium valuation arrangement as that reading assesses it. The real_options_technologist sibling assigns the same referent a radically lower epsilon by counting option space as value; the musk_cult_believer sibling dissolves the victim set; the governance_skeptic sibling relocates the harm from price level to control structure. Each file carries its own epsilon, beneficiaries, and victims; edges here enable contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
