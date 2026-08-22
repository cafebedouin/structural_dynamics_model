% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__dcf_fundamentalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: valuation_legitimacy__dcf_fundamentalist
 *   human_readable: Insider-Priced Narrative Valuation Regime (DCF-Fundamentalist Reading)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   A privately held space company is marked at $1.75T — roughly 93x its
 *   $18.7B revenue against a $4.9B net loss — by a price-setting process the
 *   controlling shareholder runs: it sets tender prices, controls disclosure,
 *   and sells into the rounds it prices. This story is authored from the
 *   reading that valuation legitimacy comes from discounting proven cash
 *   flows: Starlink's $4.4B operating profit supports roughly $44-88B, launch
 *   services add contracted revenue, and the unproven ventures (orbital AI,
 *   Mars colonization) are options to be priced, not assets to be marked at
 *   narrative scale. On that arithmetic, the standing arrangement transfers
 *   the gap between the mark and the discountable value from late-round
 *   buyers and equity-paid employees to the seat that sets the price. The ε
 *   referent is the standing arrangement — the current insider-priced mark —
 *   as this reading assesses it, never the DCF-priced alternative the reading
 *   endorses.
 *
 * KEY AGENTS:
 *   - musk_control_block: Primary beneficiary and agenda-setter (institutional/arbitrage) — sets tender prices, controls disclosure, sells into the rounds it prices
 *   - early_venture_investors: Secondary beneficiary (powerful/arbitrage) — exits positions at peak marks
 *   - late_round_public_investors: Primary payer (moderate/constrained) — buys the mark at 93x revenue without audited segment financials
 *   - equity_compensated_employees: Payer with partial benefit (powerless/constrained) — compensated in marked, illiquid equity
 *   - fundamental_value_short_sellers: Excluded critic (organized/constrained) — publishes the price discovery the tender process bypasses
 *   - securities_disclosure_regulators: Analytical observer (institutional/analytical) — oversight reaches the public vehicles, not the private mark
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.88).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.7).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "Insider-Priced Narrative Valuation Regime (DCF-Fundamentalist Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "economic/technological").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, '651adfcd-3ab5-4fe2-ae1a-636a718524b1').
narrative_ontology:cs_kernel_codification('651adfcd-3ab5-4fe2-ae1a-636a718524b1', distributed).
narrative_ontology:cs_authority_grounding('651adfcd-3ab5-4fe2-ae1a-636a718524b1', expertise).
narrative_ontology:cs_interpretation_layer_present('651adfcd-3ab5-4fe2-ae1a-636a718524b1').
narrative_ontology:cs_reading_relation('651adfcd-3ab5-4fe2-ae1a-636a718524b1', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('651adfcd-3ab5-4fe2-ae1a-636a718524b1', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('651adfcd-3ab5-4fe2-ae1a-636a718524b1', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('651adfcd-3ab5-4fe2-ae1a-636a718524b1', foundational, proven_cashflows_ground_legitimacy).
narrative_ontology:cs_axiom_status(proven_cashflows_ground_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('651adfcd-3ab5-4fe2-ae1a-636a718524b1', proven_cashflows_ground_legitimacy, instrumental).
narrative_ontology:cs_axiom('651adfcd-3ab5-4fe2-ae1a-636a718524b1', foundational, unproven_technologies_priced_as_options).
narrative_ontology:cs_axiom_status(unproven_technologies_priced_as_options, holdable).
narrative_ontology:cs_axiom_grounding('651adfcd-3ab5-4fe2-ae1a-636a718524b1', unproven_technologies_priced_as_options, empirically_contingent).
narrative_ontology:cs_reference_frame('651adfcd-3ab5-4fe2-ae1a-636a718524b1', proven_cashflow_discounting_standard).
narrative_ontology:cs_drift_state('651adfcd-3ab5-4fe2-ae1a-636a718524b1', contemporary_private_mark_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('651adfcd-3ab5-4fe2-ae1a-636a718524b1', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_venture_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, late_round_public_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, equity_compensated_employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, equity_compensated_employees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% of voting power on 42% of the equity, sets the price in every tender offer and secondary sale, decides what financial information is disclosed and when, and sells into the rounds it prices. The gap between the marked price and what proven cash flows support is realized here as liquidity: each secondary at an elevated mark converts narrative into cash. Exit is not a problem for this seat — it can mark, sell, and repeat, and no external party can force disclosure or repricing.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, musk_control_block, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, musk_control_block, beneficiary).

% Entered at valuations a fraction of the current mark across earlier funding rounds and funds, and realize gains by selling positions into later tenders at peak marks. Their returns depend on the mark holding long enough to exit, which aligns them with the price-setting seat without giving them control over it. Exit is clean: they are selling, not buying.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Buy exposure at or near the current mark — directly in tenders where admitted, or through listed and private vehicles that hold the equity — at roughly 93x revenue against negative net income. They have no vote on valuation marks, no audited segment financials to check the price against, and no fundamentals-priced alternative entry. Their realistic choices are paying the narrative premium, declining the asset class, or exiting at whatever the next mark is.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, late_round_public_investors, payer,
    moderate, biographical, constrained, global).

% Receive a substantial share of compensation as equity marked at the company's latest round, so their realized pay tracks a price they cannot influence and cannot sell on any public market. Vesting schedules and transfer restrictions hold their wealth in the marked equity for years; leaving forfeits or delays the unvested portion. They receive real wages and genuine upside if the mark is ever justified by results, and they carry the loss if it is not.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, equity_compensated_employees, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, equity_compensated_employees, beneficiary).

% Publish discounted-cash-flow critiques of the mark and take short exposure through proxies where available, but hold no seat in the tender process, receive no disclosure beyond what the control block releases, and cannot short the private equity itself. Their analysis is the price discovery the tender process bypasses; they bear real losses for as long as the mark holds.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, fundamental_value_short_sellers, excluded,
    organized, biographical, constrained, global).

% Oversee disclosure and investor protection for the public vehicles and tender processes that touch their jurisdiction, but the primary price-setting happens in private rounds where their reach is limited. They receive the critiques and the marks as filed, commission analysis, and can act on the public-facing vehicles — the private mark itself sits largely outside their authority.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, securities_disclosure_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates patient private capital at a scale conventional revenue-supported finance would not commit — orbital launch cadence, satellite manufacturing and broadband deployment — and provides a recurring liquidity mechanism (tender offers) through which insiders and employees can sell marked equity.
% TRANSFER_FUNCTION: Moves the premium between the $1.75T mark and the roughly $44-88B that discounting proven cash flows supports, from late-round public investors and equity-compensated employees to the control block selling into its own tenders and early investors exiting at peak marks.
% ABSENT_VOICES: Fundamental-value analysts and short sellers who would price the proven cash flows and option-price the rest have no seat in the price-setting room; late-round buyers have no vote on the marks they pay; the audited segment financials that would let outsiders audit the price are not produced. They object from outside — published critiques, refused tenders, short positions in proxies.
% DISAPPEARANCE_RATIONALE: The mark exists only while this price-setting arrangement maintains it: remove insider-controlled tenders and narrative-based legitimacy, and the valuation reverts toward what proven cash flows discount to, recent entrants' positions reprice sharply downward, the liquidity mechanism for insiders closes, and the operating businesses must restructure funding around debt, project finance, or a disclosing public listing.
% FOUNDING_PROBLEM: Orbital launch and satellite broadband required capital far beyond what early revenue could support; pricing the venture on mission scale rather than current cash flows solved the funding problem when no conventional basis existed.
% FOUNDING_PROBLEM_CORROBORATION: Launch customers and Starlink subscribers attest the operating businesses and their revenues are real; independent space-economy analysts attest the proven segments could now be financed conventionally. No source outside the control block attests that the remaining unproven ventures require the current premium to proceed — the continuation claim is carried by the benefiting seat alone.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.88, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.88 because the DCF-supportable value of proven cash flows (~$44-88B) is a small fraction of the $1.75T mark; the premium — on this reading's arithmetic, roughly 95% of the mark — is the transfer surface, and it is realized in cash by the price-setting seat. Suppression is 0.7 and structural rather than legal: no audited segment disclosure, price-setter and information-controller are the same seat, employee equity is locked by vesting and illiquidity, and there is no fundamentals-priced alternative entry. Theater is 0.58: investor communications and valuation-maintenance activity are majority narrative (mission scale, track-record montage, unproven-venture framing) while the operating businesses remain genuinely functional. Accessibility_collapse is 0.48 — alternatives partly survive (decline tenders, exit fund positions, short proxies) but no instrument lets a buyer purchase the proven-cash-flow core separately from the narrative premium. Resistance is 0.52 — sustained published critique and short campaigns, some tender refusals, yet capital keeps flowing and the mark holds. The measurement series run on one shared time grid (2012/2015/2017/2019/2021/2023/2025) with every tracked metric authored at every point; the enforcement series is included because the story specifically traces enforcement build-up — as the mark detached further from discountable value, more machinery (gated tender cadence, selective disclosure, employee trading restrictions, fund gatekeeping) was required to hold it. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the control-block seat the arrangement is the funding machine it built and prices — that seat computes near-full subsidy. From the late-round buyer seat the same structure is a price it must take from a seller who controls the information the price rests on. Employees compute a mixed seat: real wages, marked-up paper, no vote. The engine computes these per-seat classifications from the structural data; the divergence between the agenda-setter's experience and the payers' is the perspectival fact this story encodes, not something the authored claim adjudicates. Note on coalition potential: employees are individually powerless but are the only victims inside the information perimeter — collective action through employee channels is the one coalition path the structure does not price out, though vesting locks stagger their incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   The control block and early investors are declared beneficiaries with arbitrage-grade exit — the derivation places them at the beneficiary end (the control block at or near d=0 as the seat the gains demonstrably accrue to; early investors slightly off it, since their gain requires finding buyers at the next mark). Late-round public investors and equity-compensated employees are declared victims with constrained exit — high d, damped for employees by their secondary beneficiary position (real compensation flows to them even as the markup flows from them). Short sellers are excluded rather than coordinated: outside the transfer, negatively exposed to the mark's persistence. Regulators are analytical. Suppression (0.7) is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and spatial scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope call keeps two facts the other labels would each erase. The coordination function is real — capital formation for operating launch and broadband businesses; this reading itself values Starlink at $44-88B on proven operating profit. The premium above that is taken from identifiable payers through the same structure that raises the capital. Calling the whole arrangement pure extraction would erase the funded operations; calling it pure coordination would erase the victims. On mandatrophy: the founding problem (funding capital intensity beyond revenue support) was real and is corroborated by the operations it built, but its continuation claim is now contested — the proven segments could self-fund or raise conventionally, and this reading prices the unproven remainder as options rather than as justification for the premium. The status-by-verdict pairing (contested founding problem against a world that would rearrange) flags an arrangement whose original justification is partially obsolete while its transfer function persists at full scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_restructure,
    'This story instantiates the dcf_fundamentalist reading of the valuation_legitimacy kernel; how would the sibling readings restructure the same arrangement''s beneficiary, victim, and extraction profile?',
    'Author the sibling stories (real_options_technologist, musk_cult_believer, governance_skeptic) over the same referent and compare per-reading epsilon, victim sets, and claimed types; divergence across the family locates the disagreement in the legitimacy criterion itself.',
    'real_options_technologist would shrink the victim set by pricing option space as real value; musk_cult_believer would collapse measured extraction toward subsidy; governance_skeptic would relocate the transfer from the valuation mark to the 82.4% control structure. The high-epsilon profile here is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_restructure, conceptual, 'Committer structure: one kernel, four readings; this file is the DCF reading.').

omega_variable(
    proven_cashflow_boundary,
    'Which cash flows count as ''proven'' for the DCF-supportable valuation — Starlink''s $4.4B operating profit alone, or launch services margins and contracted backlog as well — and over what discount horizon and terminal assumption?',
    'Audited segment-level financial disclosure (currently not produced by the control block) plus independent replication of the discounting exercise under disclosed assumptions.',
    'The $44-88B supportable band and therefore the roughly 95% premium share of the $1.75T mark move with the boundary; a wider proven base narrows the premium, a narrower one widens it. The reading''s own estimate is provisional pending disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proven_cashflow_boundary, empirical, 'Boundary of the proven-cash-flow set that fixes the reading''s extraction arithmetic.').

omega_variable(
    residual_option_value,
    'What legitimate option value should the extraction share net out for the unproven ventures (orbital AI, Mars colonization) that this reading prices as options rather than assets?',
    'Option-pricing comparables: venture-stage technology bets with analogous payoff distributions, priced with standard real-option models; sensitivity of the mark to the option-value residual.',
    'A material option residual (above roughly 10% of the mark) would soften the reading''s extraction estimate and shift the structure toward a more balanced coordination reading; a negligible residual leaves the premium almost entirely extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_option_value, empirical, 'Size of the option-value residual the reading concedes inside the mark.').

omega_variable(
    suppression_structural_vs_belief,
    'Is the suppression holding the valuation mark structural (controlled disclosure, insider-gated tenders, employee illiquidity) or belief-based (would participants accept narrative pricing even under full disclosure)?',
    'Natural experiment: an audited segment disclosure or an exchange listing. If the mark survives full disclosure, suppression is belief-based; if the mark collapses toward discountable value, the enforcement machinery was load-bearing.',
    'Belief-based suppression would lower the effective coercion measure and shift explanatory weight from enforcement machinery to narrative conviction; structural suppression confirms the active-enforcement requirement the arrangement''s persistence depends on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_belief, empirical, 'Whether the mark''s persistence needs machinery or only belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 2012, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2012, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(valu_tr_t2015, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(valu_tr_t2017, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2017, 0.38).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2019, 0.45).
narrative_ontology:measurement(valu_tr_t2021, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2021, 0.55).
narrative_ontology:measurement(valu_tr_t2023, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2023, 0.58).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(valu_be_t2012, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement(valu_be_t2015, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(valu_be_t2017, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(valu_be_t2021, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(valu_be_t2023, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2023, 0.85).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2012, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(valu_su_t2015, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(valu_su_t2017, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2017, 0.5).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(valu_su_t2021, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(valu_su_t2023, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2023, 0.69).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% The colloquial question 'is the $1.75T valuation legitimate?' decomposes into four structurally distinct constraints, one per reading of the valuation_legitimacy kernel, each with its own epsilon, beneficiary/victim map, and type: this DCF reading (high extraction from late entrants via the cash-flow premium), the real-options reading (extraction shrinks to the option-space discount), the track-record reading (extraction collapses toward subsidy), and the governance reading (extraction relocates to the control structure). They are linked as a constraint family via affects_constraints; epsilon divergence across the family is the measurement the corpus exists to take, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
