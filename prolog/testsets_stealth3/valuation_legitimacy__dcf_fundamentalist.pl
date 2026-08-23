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
 *   human_readable: DCF-Fundamentalist Valuation Legitimacy Standard — Narrative-Marked Private Funding Arrangement
 *   domain: economic/technological/financial-governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the private-market valuation
 *   regime surrounding the vertically integrated launch-and-broadband
 *   operator: successive funding rounds have marked the company at roughly
 *   $1.75 trillion against $18.7 billion of revenue and a $4.9 billion net
 *   loss — about 93x revenue with negative earnings. This story instantiates
 *   the dcf_fundamentalist reading of the valuation_legitimacy kernel. From
 *   this seat, legitimate value is the discounted stream of proven cash
 *   flows, and the gap between that figure (roughly $44–88 billion for the
 *   profitable broadband segment at conventional earnings multiples, plus a
 *   conservative launch-franchise contribution) and the prevailing mark is
 *   not foresight — it is a transfer. The arrangement retains a genuine
 *   coordination function: decade-horizon private capital built reusable
 *   launch capacity and a self-funding communications business that public
 *   quarterly-earnings markets would not have financed. But the same
 *   machinery that aggregates that capital now prices the aggregate far above
 *   what its proven cash flows support, and the spread accrues, at each mark,
 *   disproportionately to the controlling shareholder and early investors who
 *   sell into rounds they help narrate. Late-round entrants and
 *   equity-compensated employees bear the corresponding exposure. Per the
 *   epsilon-referent rule, epsilon is authored for the standing
 *   narrative-marked arrangement as this reading sees it — never for the
 *   fundamentals-anchored alternative this reading would endorse. The claim
 *   and the metrics are independent authored facts: the tangled_rope claim
 *   states what this reading believes is structurally true, and the metric
 *   values state what it believes is descriptively true of the arrangement's
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computed type is
 *   the measurement the corpus exists to take. Sibling readings of the same
 *   kernel are separate constraints (see network.dual_formulation_note) and
 *   are linked, not averaged, here.
 *
 * KEY AGENTS:
 *   - - musk_control_holder: agenda-setter and principal beneficiary (institutional/arbitrage) — sets round cadence and public narrative; captures the largest share of each markup
 *   - - early_stage_venture_investors: beneficiaries (institutional/arbitrage) — entered at low marks, sell tranches into later rounds at successive peaks
 *   - - late_round_growth_investors: primary targets (powerful/trapped) — buy at narrative marks into illiquid positions with no resale market but the next round
 *   - - spacex_employee_option_holders: targets (moderate/identity_locked) — trade below-market cash compensation for equity priced by the employer's own storytelling
 *   - - commercial_launch_customers: incidental beneficiaries (institutional/mobile) — receive cheap launch and bandwidth from the capitalized buildout without equity exposure
 *   - - fundamental_value_analysts: analytical observers (organized/analytical) — publish the cash-flow appraisal the mark diverges from
 *   - - minority_governance_advocates: excluded voice (organized/trapped) — no seat exists for them in private-round pricing
 *   - - securities_regulators: observers (institutional/analytical) — jurisdiction currently stops short of private-mark transparency
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
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF-Fundamentalist Valuation Legitimacy Standard — Narrative-Marked Private Funding Arrangement").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "economic/technological/financial-governance").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, '6f12f7fe-242b-4f5c-a76e-fbe929aef9f8').
narrative_ontology:cs_kernel_codification('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', distributed).
narrative_ontology:cs_authority_grounding('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', lineage).
narrative_ontology:cs_interpretation_layer_present('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8').
narrative_ontology:cs_reading_relation('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', foundational, valuation_legitimacy_requires_proven_cashflows).
narrative_ontology:cs_axiom_status(valuation_legitimacy_requires_proven_cashflows, holdable).
narrative_ontology:cs_axiom_grounding('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', valuation_legitimacy_requires_proven_cashflows, empirically_contingent).
narrative_ontology:cs_axiom('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', foundational, unproven_technologies_are_options_not_assets).
narrative_ontology:cs_axiom_status(unproven_technologies_are_options_not_assets, holdable).
narrative_ontology:cs_axiom_grounding('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', unproven_technologies_are_options_not_assets, conventional).
narrative_ontology:cs_reference_frame('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', proven_cashflow_discounting_standard).
narrative_ontology:cs_drift_state('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', contemporary_private_markets, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6f12f7fe-242b-4f5c-a76e-fbe929aef9f8', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, musk_control_holder).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_stage_venture_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, commercial_launch_customers).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, late_round_growth_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, spacex_employee_option_holders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, patient_capital_enables_orbital_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a voting majority (about 82 percent of votes on roughly 42 percent of equity), sets the cadence of funding rounds, chooses what operational detail is disclosed, and supplies the public narrative — Mars timelines, orbital-computing ambitions — between raises. Sells small personal tranches into each round at the prevailing mark and pledges holdings for personal liquidity, so each upward mark converts directly into spendable resources for him. His exit is unilateral: he decides when and how much to monetize, and no external party can force his position onto a market.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, musk_control_holder, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, musk_control_holder, beneficiary).

% Entered at valuations two orders of magnitude below the current mark during the years when the outcome was genuinely uncertain. Their positions have appreciated enormously on paper; several have sold partial stakes into later rounds at successive marks, recycling gains while retaining upside exposure. They hold information comparable to anyone outside the company, can decline any round, and can distribute their selling across rounds — participation is optional for them at every step.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_stage_venture_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Buy into rounds priced at the prevailing narrative mark, often through allocations they competed for, and receive illiquid private shares with no resale market except the next round. Their documented downside case — the value supported by audited cash flows — sits far below their entry price. Exit consists of waiting for a subsequent buyer at a higher mark, a liquidity event the controlling shareholder schedules, or writing the position down, which their own fundraising narratives penalize.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, late_round_growth_investors, payer,
    powerful, biographical, trapped, global).

% Accept below-market cash salaries in exchange for equity grants struck at or near the narrative mark, with vesting cliffs and exercise windows that concentrate their wealth in the company's own pricing. Their working identity is bound to the mission — making life multiplanetary — so the mark is simultaneously their savings vehicle and their creed. Leaving forfeits unvested grants and, socially, the mission community; staying compounds their exposure to a price set by their employer's own storytelling.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, spacex_employee_option_holders, payer,
    moderate, biographical, identity_locked, national).

% Buy launch and bandwidth at prices an order of magnitude below the pre-2002 baseline, made possible by the capitalized buildout the funding rounds financed. They pay market rates for delivered services, hold no equity exposure, and can contract with competing providers for most mission classes — their benefit flows from the infrastructure existing, not from the mark.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, commercial_launch_customers, beneficiary,
    institutional, generational, mobile, global).

% Publish discounted-cash-flow and sum-of-the-parts appraisals of the company using disclosed segment results, and document the gap between those appraisals and the prevailing private mark. They hold no position-setting power in private rounds, cannot short the shares, and influence the arrangement only indirectly through the climate of skepticism their work sustains.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, fundamental_value_analysts, observer,
    organized, biographical, analytical, global).

% Would argue for board independence, disclosure standards, and one-share-one-vote protections before capital enters at each new mark, but no seat exists for them in private-round pricing: the parties at the table are the issuer and the buyers, both served by the current structure. Their levers — regulatory petition and public argument — operate outside the room where each mark is set.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, minority_governance_advocates, excluded,
    organized, generational, trapped, national).

% Oversee private-market conduct within jurisdictional limits that currently stop short of mandating mark transparency for late-stage private issuers. They observe the widening gap between private marks and any fundamentals anchor, collect complaints, and would act chiefly at a public listing, when the marks meet exchange-level disclosure obligations.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, musk_control_holder).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates decade-horizon private capital for capital-intensive orbital infrastructure — reusable launch, satellite mass production, a global broadband constellation — staged against technical milestones that public quarterly-earnings markets would not finance at this duration; recycles proceeds between rounds to keep the buildout continuously funded.
% TRANSFER_FUNCTION: Moves two things in opposite directions: real capital flows from investors into launch and manufacturing operations, while the valuation premium over cash-flow-supported value flows, at each successive mark, from new entrants and equity-compensated employees to the controlling shareholder and early investors who sell into the rounds.
% ABSENT_VOICES: Minority-shareholder governance advocates are absent from every round that sets a mark (no seat exists for them in private pricing); would-be short sellers have no mechanism to register dissent against a private issuer; future public-market buyers who would inherit the marks at a listing are absent from the negotiations that create their entry price.
% DISAPPEARANCE_RATIONALE: If the narrative-marked funding arrangement vanished overnight, rounds would reprice to disclosed cash flows, insider liquidity events would shrink by roughly an order of magnitude, employee compensation would shift back toward cash, and the pace of speculative programs (orbital computing, Mars logistics) would slow to what operating cash flows could fund directly — the private space-capital economy would reorganize around fundamentals-anchored pricing.
% FOUNDING_PROBLEM: In the early 2000s, orbital launch and satellite manufacturing were capital-intensive, failure-prone, decade-long undertakings that public equity markets — tuned to quarterly earnings — would not finance; the founding problem was constructing a funding instrument patient enough to survive repeated rocket failures and a ten-year horizon to revenue.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on both sides of the dispute: independent launch-cost studies and the competitive response of legacy providers attest that the original financing gap was real and that the patient-capital instrument solved it; fundamental-value research houses and financial-press analyses attest that the profitable segment's operating cash flows now cover core capital expenditure, making further narrative-marked raises unnecessary for the core business. Only insiders assert the Mars-scale continuation of the problem as requiring the current premium; no external auditor attests that claim.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.78 for the standing arrangement: the spread between the prevailing mark and the value supported by proven cash flows is on the order of 95 percent of the headline number, and the mechanism converts that spread into realized insider liquidity at each round while leaving late entrants holding illiquid claims priced on narrative. It is not authored higher because a minority of the mark does rest on audited operating profit and hard assets, and because the transfer requires the next buyer to take the mark at face rather than operating through legal coercion. Suppression is 0.55: participation is voluntary and legal alternatives abound, but the arrangement constrains exits (illiquidity, vesting cliffs), gates access (allocation of oversubscribed rounds), and controls information (selective disclosure by a private issuer) — coercive in effect without being coercive in form. Per the unscaled-suppression rule, this 0.55 is a raw structural property; only extractiveness is scaled by directionality and scope downstream. Theater ratio 0.62 is scoped to the valuation-maintenance activity (round presentations, timeline projections, orbital-computing and Mars revenue narratives, milestone spectacles timed to raises), not to launch operations, which are functional; within the activity that holds the mark aloft, the performative share is now the majority. Accessibility collapse 0.35: understanding the mechanics does not eliminate alternatives — refusing participation, other asset classes, and other issuers all persist — so what collapses is the illusion that the mark is information rather than positioning. Resistance 0.45: skeptical analysis is vocal and organized (this story is one artifact of that discourse) but lacks mechanical bite, since private shares cannot be shorted and dissent does not reprice anything. The three measurement series share one grid (t = 0,4,8,12,16,20,24 across the company's lifecycle from founding to the current standing arrangement) so every tracked metric is authored at every examined point. suppression_requirement is included because the story specifically tracks enforcement intensification: as skeptical coverage grew, holding the mark demanded progressively more active narrative defense, disclosure control, and allocation discipline. At t=24 the enforcement-intensity series (0.60) runs slightly above the structural suppression scalar (0.55) because defending the mark against a growing skeptic community takes more effort than the resulting coercion registers.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the controlling shareholder's seat the arrangement is a funding machine he built and personally validates — coordination he administers, with liquidity he times. From the late-round investor's seat the same structure operates as gated access to an asset whose price is set by the seller's narrative, with exit only through the next believer. From the employee's seat it is a fused bargain: mission identity, vesting cliffs, and below-market cash make the mark simultaneously paycheck and creed, so the exposure is nearly invisible from inside — this is ideological-plus-relational identity fusion, and if the mission frame broke (e.g., a credible public repricing), the employee seat's computed extraction would spike as the fusion dissolved. From the launch customer's seat the entire contest is invisible: they receive cheap, reliable launch and pay market rates for it. Same nominal market, four different lived constraints. Coalition note: the payer seats are not natural allies — late-round institutions defend their marks to protect fundraising track records, and employees defend the mission — which is precisely why the arrangement has faced organized skepticism but little organized repricing pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: musk_control_holder and early_stage_venture_investors sit near the beneficiary end (d low) — each upward mark subsidizes their holdings and their exits, and both hold arbitrage-grade exit (they choose the timing and size of their own liquidity). commercial_launch_customers benefit incidentally through the capitalized buildout without paying the premium, and their mobile exit damps their d further toward the subsidized end. Victim declarations: late_round_growth_investors bear the transfer with trapped exit (d high — no resale market but the next round, and the liquidity event is scheduled by the counterparty); spacex_employee_option_holders bear it with identity_locked exit (d nearest the full-target end — trapped economics compounded by self-concept fusion with the mission). Observers (fundamental_value_analysts, securities_regulators) take the analytical seat and feed no extraction arithmetic; the excluded seat (minority_governance_advocates) is commentary-grade and drives no correction. No directionality overrides are authored: the derivation chain from declared roles, power, and exit options reproduces the intended directionalities, and overrides are keyed by power atom rather than by agent, so authoring one would cross-contaminate distinct seats that share an atom (e.g., the organized-power beneficiary customer seat and the organized-power excluded advocacy seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite errors. Reading the arrangement as pure extraction would erase the real coordination achievement — the launch-cost revolution and a self-funding broadband business exist because this funding machinery worked, and a pure-extraction claim would misattribute that buildout to coercion that never happened. Reading it as pure coordination would erase the transfer — the spread between mark and proven cash flow is not a coordination cost; it is a positional rent collected at each round by whoever holds the narrative. The tangled_rope structure holds both: genuine coordination function, asymmetric extraction riding on it, and active enforcement required to keep the narrative mark aloft. On mandate obsolescence: the founding problem (financing decade-horizon orbital infrastructure that public markets would not touch) is genuinely disputed as resolved — the profitable segment's operating cash flows now cover core capital needs, which insiders dispute by pointing to Mars-scale requirements. The story therefore authors founding_problem_status as contested rather than resolving it unilaterally; the mismatch consumer reads status against the disappearance verdict, and this story deliberately declines to manufacture either a zombie flag or a clean bill from the same contested evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This story instantiates one reading (dcf_fundamentalist) of the valuation_legitimacy kernel; how would the constraint''s beneficiary/victim structure and classification change under each sibling reading?',
    'Generate the three sibling stories (valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer, valuation_legitimacy__governance_skeptic) with their own epsilon values over the same standing arrangement and compare computed per-seat types; the disagreement locus is the legitimacy criterion itself (proven cash flows vs. option space vs. founder track record vs. minority governance).',
    'Under real_options_technologist a bounded share of the premium becomes legitimate option value (epsilon falls toward the rope/tangled_rope boundary); under musk_cult_believer the seat structure inverts (the mark is validated prophecy and the skeptics become the arrangement''s targets); under governance_skeptic the referent shifts to the voting-control structure and the multiple becomes secondary evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Kernel-reading membership: sibling readings would restructure beneficiaries, victims, and epsilon over the same arrangement.').

omega_variable(
    option_value_attribution_boundary,
    'Even granting generous technological optimism, how much of the prevailing mark is legitimately attributable to unpriced optionality (orbital datacenters, point-to-point transport, Mars logistics) rather than narrative premium?',
    'Structured scenario valuation bounding each option''s revenue path, probability, and discount rate, published alongside the discounted-cash-flow baseline with sensitivities.',
    'A large defensible option-value share lowers epsilon and softens the reading toward rope; a small share confirms the premium as positional rent and pushes the arrangement toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_attribution_boundary, empirical, 'Boundary between defensible option value and narrative premium inside the headline mark.').

omega_variable(
    paper_mark_vs_realized_transfer,
    'Is the transfer realized (insiders converting narrative marks into cash via secondary sales and borrowings) or still largely paper (marks held, no conversion)?',
    'Track insider secondary-sale volumes and borrow-against-equity balances at each successive mark; compare cumulative realized proceeds to the notional markup.',
    'Realized conversion confirms an operative transfer mechanism and supports the tangled_rope-to-snare side of the reading; purely paper marks make the transfer contingent on a future liquidity event and soften current-severity assessments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paper_mark_vs_realized_transfer, empirical, 'Whether narrative marks have been converted into realized insider gains.').

omega_variable(
    employee_equity_trade_fairness,
    'Do equity-compensated employees rationally accept the cash-for-paper trade at prevailing marks, or are they systematically overpaying for illiquid, narrative-priced claims?',
    'Revealed-choice data: tender-offer participation rates, secondary-sale uptake when windows open, and post-departure mark expectations versus realized outcomes.',
    'If employees are rational participants, the employee seat is a consenting counterparty and the victim set narrows to late-round capital; if systematically mispriced, the victim set widens and the internalized share of suppression rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employee_equity_trade_fairness, empirical, 'Whether the employee seat is a consenting counterparty or a bearing party.').

omega_variable(
    terminal_liquidity_price_discovery,
    'Does the narrative premium survive transition to public price discovery (IPO or direct listing), or does it reprice onto the last buyers?',
    'Observe the listing event: opening valuation versus last private mark, lockup-expiry behavior, and subsequent trading range versus the cash-flow-supported value.',
    'Downward repricing realizes the transfer onto late-round and employee holders and confirms the victim structure ex post; survival of the premium would show the arrangement can reproduce itself in public markets and extends its persistence horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(terminal_liquidity_price_discovery, empirical, 'Terminal test of whether the narrative mark survives public price discovery.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression structural (disclosure control, allocation gating, vesting lockups, illiquidity) or internalized (mission belief, founder deference, fear of losing deal access)?',
    'Post-exit trajectory comparison: investors who declined participation show no lasting impairment (structural reading); employees and alumni who left and still defer to the founder-narrative in subsequent decisions show an internalized component that travels with them.',
    'An internalized share means effective suppression exceeds the structural measure and persists after formal exit options open; a purely structural reading means removing disclosure and gating barriers would dissolve most of the measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression between structural barriers and internalized allegiance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(valu_tr_t0, observed).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 4, 0.14).
narrative_ontology:measurement_basis(valu_tr_t4, observed).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(valu_tr_t8, observed).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(valu_tr_t12, observed).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(valu_tr_t16, observed).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(valu_tr_t20, observed).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.62).
narrative_ontology:measurement_basis(valu_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(valu_be_t0, observed).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 4, 0.22).
narrative_ontology:measurement_basis(valu_be_t4, observed).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(valu_be_t8, observed).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(valu_be_t12, observed).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(valu_be_t16, observed).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(valu_be_t20, observed).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.78).
narrative_ontology:measurement_basis(valu_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(valu_su_t0, observed).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 4, 0.12).
narrative_ontology:measurement_basis(valu_su_t4, observed).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(valu_su_t8, observed).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 12, 0.28).
narrative_ontology:measurement_basis(valu_su_t12, observed).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 16, 0.36).
narrative_ontology:measurement_basis(valu_su_t16, observed).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(valu_su_t20, observed).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(valu_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the SpaceX valuation debate.' The single public controversy covers four structurally distinct claims with different epsilon values over the same standing arrangement. This file authors the dcf_fundamentalist reading: referent is the narrative-marked private funding arrangement, epsilon high (0.78) — the mark-to-cash-flow spread transfers to insiders at each round. The real_options reading authors a different constraint (a bounded share of the same premium reclassified as legitimate option value, lowering its epsilon); the musk_cult reading authors another (the mark as validated prophecy, inverting the seat structure); the governance_skeptic reading authors a fourth (referent shifted to the voting-control structure, where the 82.4%/42% split is itself the extraction). Upstream/downstream: this reading is the analytic baseline the other three react against — each sibling cites or rebuts the cash-flow anchor — so this story influences all three without foreclosing any. Family members are linked via affects_constraints per the epsilon-invariance principle; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
