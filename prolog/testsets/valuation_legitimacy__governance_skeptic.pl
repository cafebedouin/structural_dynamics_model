% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__governance_skeptic, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Voting Control as Extraction Mechanism (Governance-Skeptic Reading)
 *   domain: corporate_finance/technology_governance
 *
 * SUMMARY:
 *   Elon Musk's control of Tesla via a 10:1 dual-class voting structure
 *   grants him 82.4% voting control while holding ~42% economic equity. Class
 *   A shareholders (57.8% economic stake) hold zero voting power on any
 *   material decision. This constraint story instantiates the
 *   GOVERNANCE-SKEPTIC reading of the valuation-legitimacy kernel: that
 *   Musk's supermajority voting is a pure extraction mechanism, enabled by
 *   charter terms that insulate him from accountability. The dual-class
 *   structure is presented (by Musk, Tesla IR, and ideological allies) as
 *   necessary to protect long-term innovation from quarterly earnings
 *   pressure. This reading contests that framing: the structure now primarily
 *   enables capital misallocation (Terafab), conflicts of interest (SpaceX
 *   benefits), and compensation extraction that would not survive independent
 *   board review. The measurement series tracks extraction (rising from 0.55
 *   to 0.81 over the interval, correlating with Musk's accumulated wealth and
 *   firmer control) and suppression (rising as shareholder objections are
 *   ignored and charter protections are formalized). Theater ratio rises
 *   initially (more proxy-fight-era posturing) then plateaus (suppression
 *   hardens, objections are mathematically powerless).
 *
 * KEY AGENTS:
 *   - Musk: institutional power, civilization time horizon, arbitrage exit — sets capital allocation and compensation without accountability; benefits directly from valuation capture.
 *   - Class A public shareholders: organized power, biographical horizon, mobile exit but coordination lock — own 57.8% economic stake, zero voting power; can exit but exiting means abandoning the Musk-option value they paid for.
 *   - Class B early holders (including VC): institutional power, generational horizon, trapped exit — VC investors and founders hold voting supermajority, benefit from private benefits of control without proportionate economic risk; cannot exit Class B without forfeiting control.
 *   - Tesla employees with equity: moderate power, biographical horizon, constrained exit — residual claimants on company performance but with no governance voice; golden handcuffs (vesting schedules) tie them to equity appreciation controlled by Musk.
 *   - Institutional investors (BlackRock, Vanguard, CalPERS): powerful but systematically disempowered by the vote ratio; formal objections to capital allocation are recorded and ignored.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.81).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.76).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.81).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, snare).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Control as Extraction Mechanism (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, '6339732b-e157-440c-97c4-dab3918915ff').
narrative_ontology:cs_kernel_codification('6339732b-e157-440c-97c4-dab3918915ff', formalized).
narrative_ontology:cs_authority_grounding('6339732b-e157-440c-97c4-dab3918915ff', extraction).
narrative_ontology:cs_reading_relation('6339732b-e157-440c-97c4-dab3918915ff', valuation_legitimacy__dcf_fundamentalist, influences).
narrative_ontology:cs_reading_relation('6339732b-e157-440c-97c4-dab3918915ff', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_reading_relation('6339732b-e157-440c-97c4-dab3918915ff', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_axiom('6339732b-e157-440c-97c4-dab3918915ff', foundational, minority_shareholder_protection_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(minority_shareholder_protection_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6339732b-e157-440c-97c4-dab3918915ff', minority_shareholder_protection_necessary_for_legitimacy, deontological).
narrative_ontology:cs_axiom('6339732b-e157-440c-97c4-dab3918915ff', foundational, supermajority_voting_incompatible_with_minority_protection).
narrative_ontology:cs_axiom_status(supermajority_voting_incompatible_with_minority_protection, holdable).
narrative_ontology:cs_axiom_grounding('6339732b-e157-440c-97c4-dab3918915ff', supermajority_voting_incompatible_with_minority_protection, deontological).
narrative_ontology:cs_axiom('6339732b-e157-440c-97c4-dab3918915ff', secondary, extraction_extracted_via_governance_structure).
narrative_ontology:cs_axiom_status(extraction_extracted_via_governance_structure, holdable).
narrative_ontology:cs_axiom_grounding('6339732b-e157-440c-97c4-dab3918915ff', extraction_extracted_via_governance_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('6339732b-e157-440c-97c4-dab3918915ff', independent_board_governance_with_prorate_voting).
narrative_ontology:cs_drift_state('6339732b-e157-440c-97c4-dab3918915ff', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6339732b-e157-440c-97c4-dab3918915ff', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, class_b_early_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, tesla_employees_with_equity_grants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, tesla_employees_with_equity_grants).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, venture_capital_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, institutional_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, minority_shareholder_protection_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, agency_cost_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Tesla via 82.4% voting power while holding ~42% economic equity. Sets capital allocation (Terafab, R&D budgets, no dividends), executive compensation (his own packages totaling $56B+ in 2018 grant, subsequent RSU grants), strategic direction (Full Self-Driving priority, Optimus development timeline, energy business expansion), and charter amendments (dual-class ratio restoration in 2024 shareholder vote). Operates simultaneously as CEO of SpaceX and founder/active participant in Neuralink, The Boring Company, and other ventures, creating ongoing conflicts between Tesla's capital and his attention and SpaceX's capital needs (Terafab benefits both). The dual-class voting structure legally shields these decisions from shareholder challenge; only technical performance failures or regulatory intervention can override his discretion.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Own ~57.8% economic equity but hold zero voting power due to 10:1 vote-ratio dual-class structure. Paid for exposure to Tesla's upside when they purchased shares, but that upside is now mediated entirely through Musk's capital allocation and strategic decisions, which they cannot influence. Receive no dividends (all cash retained at Musk's discretion). Cannot vote on charter amendments, M&A, executive compensation, or strategic pivots. Can vote on routine housekeeping matters (board-nominated directors, which are pre-selected by Musk or his allies). Exit is liquid (can sell shares any day) but psychologically costly — exiting means admitting the bet on Musk's leadership was mistaken. Institutional holders (BlackRock, Vanguard, CalPERS) have formally objected to capital allocation decisions and compensation, but objections are recorded and ignored due to mathematical powerlessness.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    organized, biographical, mobile, global).

% Includes Musk (largest Class B holder) and early investors (venture capital firms, founders, early employees who converted to Class B through secondary markets). Collect private benefits of control: influence over capital allocation, strategic direction, compensation structure, dividend policy, and charter amendments — all without accountability to Class A shareholders. Class B shares are legally non-transferable except to family members or in very limited circumstances, creating a permanent voting block that cannot be diluted by secondary market sales. Benefit because the dual-class structure enables long-term, high-risk bets (Full Self-Driving, Optimus, energy transition) without quarterly earnings pressure or activist interference. Benefit also from compensation extraction: Musk's compensation is set by a compensation committee he effectively controls (no independent directors), and it is typically far above peer benchmarks.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_b_early_holders, beneficiary,
    institutional, generational, trapped, global).

% Receive stock options and RSUs as compensation, representing a significant portion of total pay for engineering and senior staff. Equity appreciation depends entirely on company execution and valuation, both controlled by Musk. Have zero governance voice despite being residual claimants on company performance. If strategic decisions prove poor (capital misallocated to Terafab, overambitious FSD timelines, or SpaceX benefits that don't accrue to Tesla), equity value erodes but they cannot challenge or influence those decisions. Retention incentives are high: RSUs vest over 4 years, options vest over 4–5 years, creating golden handcuffs that lock them in while their wealth depends on Musk's discretion. Exit option exists (can leave for competitor or startup) but carries opportunity cost (losing unvested equity, resetting vesting clocks).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_employees_with_equity_grants, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, tesla_employees_with_equity_grants, beneficiary).

% Hold significant Class A blocks (BlackRock ~8%, Vanguard ~5%, CalPERS ~0.5%, others smaller). Technically can vote in shareholder meetings but votes are mathematically powerless: even unanimous institutional opposition to a measure is overridden by Musk's 82.4% Class B voting supermajority. Have formally filed shareholder proposals and dissented from: Terafab capital allocation (2024), Musk's compensation structure (2024), charter amendments, and related-party transactions (SpaceX synergy deals). Dissent is recorded in proxy statements and becomes a matter of public record, but has zero enforcement power. Exit is liquid and costless (can sell shares instantly in public markets) but carries reputational cost: institutional investors that divest Tesla face pressure from asset managers and industry peers that hold Tesla for its growth upside. Functionally disempowered but not powerless: they can pressure through proxy voting advisory firms (ISS, Glass Lewis) and shareholder activism campaigns, though these campaigns are performative rather than effective.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, institutional_investors, payer,
    powerful, biographical, mobile, global).

% Early-stage investors (Sequoia Capital, Khosla Ventures, others) who hold Class B shares from Series B–Series D rounds and received those shares pre-IPO. Benefit from private benefits of control without bearing proportionate economic risk: their economic stake in Tesla (percentage of equity) is typically smaller than their voting stake (percentage of Class B shares held). Captured massive upside from Series B ($4.9M, 2008) through IPO (2010) and subsequent appreciation without facing downside risk proportional to their voting power. Legally trapped: Class B shares cannot be sold in secondary markets without losing voting rights (forced conversion to Class A). Profit from all of Musk's strategic decisions even when those decisions harm Class A shareholders, because their Class B holdings maintain supermajority control and its private benefits. Income accrual is entirely from equity appreciation (no dividends); exit value is trapped unless liquidity events (secondary markets, M&A) occur and preserve Class B status.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, venture_capital_class_b_holders, beneficiary,
    institutional, generational, trapped, global).

% SEC (Securities and Exchange Commission) has formal oversight of proxy disclosures (Rule 14a-8, Schedule 14A), executive compensation disclosure (Item 402 of Regulation S-K), related-party transactions, and Rule 10b5 disclosure adequacy. Formally reviewed and challenged Musk's 2018 $56B compensation grant as excessive and inadequately disclosed; settled with a reduced grant ($2.6B annually thereafter). Authority to condition public-company trading privileges on disclosure compliance and procedural fairness (proxy access rules, shareholder proposal rights). Cannot directly dictate governance terms or compensation levels but can impose procedural requirements (e.g., independent compensation committee, disclosure of golden-parachute agreements). Has ceded controlled-company exemptions under NYSE rules that allow Musk to avoid independent board committees — this is a regulatory choice, not a structural necessity. Limited authority to challenge the dual-class voting structure itself (it is legal under Delaware corporate law and federal proxy rules).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, securities_regulators, observer,
    institutional, generational, analytical, national).

% Includes legacy automakers (GM, Ford, Volkswagen, BMW) and new EV entrants (Rivian, Lucid, Nio) and tech firms with autonomous-vehicle programs (Waymo/Alphabet, Aurora, others). Compete with Tesla in battery chemistry, autonomous driving, energy storage, and vehicle electrification. Musk's divided attention (CEO of Tesla and SpaceX, founder of Neuralink) potentially lowers Tesla's competitive intensity — his time is divided, his focus shifts based on personal priority rather than market dynamics. Their governance structures explicitly include independent boards, supermajority voting restrictions (one share = one vote or weighted but not 10:1), and explicit conflict-of-interest policies preventing CEO involvement in multiple major ventures. They contend that independent governance and accountability mechanisms strengthen rather than weaken long-term value creation and competitive position. Structurally excluded from Tesla's governance — cannot lodge shareholder proposals, cannot influence capital allocation, cannot access decision-making. Their objections to the dual-class structure and Musk's conflicts of interest cannot influence Tesla's strategic direction, though they can shape industry best-practice norms and regulatory pressure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, competing_tech_firms, excluded,
    institutional, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-class voting structure (claims to) solve a genuine coordination problem: protecting long-term, high-risk technical development (Full Self-Driving, Optimus humanoid robots, energy-storage systems) from quarterly earnings pressure and activist short-sellers who profit from volatility. The founding problem was real in 2010–2015 when Tesla's survival was uncertain, activist shorts were aggressive, and a sustained hostile takeover threat existed. Dual-class voting enabled Musk to commit to decade-long R&D cycles without forced dividend payouts or executive replacement during downturns.
% TRANSFER_FUNCTION: Transfers governance power from Class A shareholders (who hold 57.8% economic equity) to Class B holders (who hold 42.2% economic equity but 82.4% voting power via 10:1 vote ratio). Musk, as the largest Class B holder, captures: (1) capital allocation discretion (~$50B+ annual spend, including Terafab silicon fab, without shareholder approval), (2) compensation extraction ($56B 2018 grant, $2.6B+ annual RSU grants, no independent review), (3) strategic direction (FSD timeline, Optimus development stage, energy-business pivot), (4) dividend policy (no dividends ever; all cash retained), (5) related-party benefits (Terafab serves both Tesla and SpaceX; allocation is Musk's choice). Early-VC Class B holders capture proportionate voting power without proportionate economic risk (pre-IPO upside capture, post-IPO Class A dilution of their percentage ownership). Class A shareholders surrender governance power but are locked into the upside option (cannot exit without losing the exposure they bought).
% ABSENT_VOICES: Competing technology firms (excluded from governance but affected by Musk's divided attention) argue that dual-class structures create moral hazard and reduce discipline. They would propose independent director requirements and conflict-of-interest policies that are standard in their own governance. Displaced activist investors who would otherwise mount proxy contests are structurally silenced: even unanimous opposition cannot override the 10:1 ratio. Academic corporate governance scholars (from outside the benefiting parties) would contest the foundational premise that founder protection requires supermajority voting; empirical studies show independent governance in peer companies (Alphabet, Microsoft) correlates with similar or better long-term innovation.
% DISAPPEARANCE_RATIONALE: If the 10:1 dual-class structure dissolved overnight (charter reformed to give Class A shareholders pro-rata voting, or dissolution forced by regulatory action), Tesla would immediately face board-level pressure to: (1) divest or recuse Musk from decisions affecting SpaceX and other competing ventures (SpaceX capital needs would compete openly with Tesla capex), (2) constrain R&D budgets on unproven technologies (Optimus, FSD advanced features) through independent board approval, (3) establish an independent compensation committee that would reset Musk's compensation to peer-comparable levels (estimated $500M–$1.5B annually, down from $56B+), (4) implement dividend policy review or share buybacks to return capital (currently all retained at Musk's discretion), (5) impose independent nominating committee to diversify board composition (currently board is Musk-aligned). Valuation would reset downward by 15–30% (the market premium attributed to 'Musk's vision freedom' would compress to peer multiples). Capital allocation would shift from long-term innovation bets toward profitable near-term projects. Stock price trajectory would likely flatten (lower reinvestment rate) or decline (lower valuation multiple + earnings pressure from reduced R&D). Musk's wealth would decline by ~$100–300B due to the valuation reset and lower future stock appreciation.
% FOUNDING_PROBLEM: Tesla was founded in 2003 when no major automaker was seriously pursuing electric vehicles; the industry consensus was that EV technology was 20+ years away. Musk acquired Tesla in 2004 and took it public in 2010 while it was still pre-profitable and technically unproven. The founding problem was: how can a capital-intensive, long-development-cycle company with unproven technology and hostile activist short-sellers execute a visionary technical roadmap that requires 10–15 years of R&D before profitability, without being displaced, diluted, or forced to return capital? Supermajority voting for the founder was the answer: Musk could commit to the long-term bet without activist interference. This problem was real and structural.
% FOUNDING_PROBLEM_CORROBORATION: By 2024, the founding problem no longer exists. Tesla is: (1) massively profitable ($30B+ annual free cash flow, 2023–2024), (2) dominant in EVs (largest global EV manufacturer, >20% market share), (3) technically proven (FSD deployed in beta across hundreds of thousands of vehicles, energy-storage business profitable, Supercharger network dominant), (4) faced with no acquisition threat (Musk's wealth makes hostile takeover impossible, no strategic buyer has leverage), (5) faced with no activist short-seller threat (shorts are abundant but cannot influence governance). The activist threat that motivated dual-class voting in 2010–2015 is now absent — Tesla's market position is unassailable. Corroboration from outside the benefiting parties: CalPERS (major institutional investor) has formally stated that the founding problem is solved and the dual-class structure now primarily enables extraction rather than protection. Academic governance researchers (Lucian Bebchuk, others) have published empirical analyses showing that supermajority voting correlates with reduced shareholder returns and increased insider extraction in mature companies; the protective function diminishes over time. Competing automakers (GM, Volkswagen) operate with one-share-one-vote and comparable long-term R&D cycles, demonstrating that the coordination problem can be solved without supermajority voting. Industry governance advisors (ISS, Glass Lewis) recommend against dual-class voting for profitable, mature companies. Musk and Tesla IR defend the structure by citing 'ongoing activist threats' and 'necessity for long-term vision' — framing that is contradicted by the objective facts of Tesla's market position and cash generation.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__governance_skeptic_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__governance_skeptic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.81 at interval end) because Musk's voting control enables: (1) capital allocation without shareholder approval (Terafab, continued R&D on unproven technologies like Optimus at 10%+ annual spend), (2) compensation extraction ($56B grant in 2018, massive RSU packages, no independent review), (3) no dividend policy (all cash retained, at Musk's discretion for his pet projects), (4) no independent nominating or compensation committees (controlled-company exemption). Suppression is high (0.76) because the class-A voting exclusion is structural and permanent — shareholders cannot challenge it without charter amendment, which requires super-majority (Class B) approval. Voting in shareholder meetings is theatrical: dissent can be registered but is mathematically powerless. Theater ratio rises from 0.38 to 0.51 because proxy-fight posturing increases (ISS recommendations, shareholder activism advisory firms mobilize opposition) but the underlying suppression mechanisms (10:1 ratio, dual-class charter) remain unchanged. Measurement series reflect observable drift: 2018 grant (extraction spike), 2024 ratio restoration after prior dispute (suppression formalized), ongoing Terafab investments (capital misallocation amplified), Musk's divided attention across ventures (conflict-of-interest mechanism hardened). Accessibility collapse is 0.63 (moderate-high): shareholders can exit the stock, but exiting means losing exposure to Tesla's upside option, and alternative equity structures are unavailable — no proxy contest can change the voting rules, no tender offer can buy control (Musk already holds 82.4%), no regulatory lever exists to force governance reform (SEC allows controlled companies).
 *
 * PERSPECTIVAL GAP:
 *   From Musk's and early-VC seat, the dual-class structure is essential coordination: it enables long-term bets (Optimus, FSD) without activist interference, founder-alignment without dilution, and vertical integration (Terafab, SpaceX synergy) without quarterly earnings pressure. From the Class A shareholder seat, the same structure appears as pure extraction: governance power was surrendered in exchange for an option on Musk's leadership, but that option now generates private benefits (capital allocation, compensation, conflict monetization) that exceed what an independent board would allow. From the employee-equity seat, the structure creates golden handcuffs: upside is real but constrained by Musk's discretion, downside is asymmetric (equity vests gradually while decisions are made instantly). The engine computes this divergence from the structural data — different power atoms (institutional vs. organized), different exit options (arbitrage vs. mobile), different beneficiary/victim status yield different effective directionalities and types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk (power=institutional, exit=arbitrage): d approaches 0.0 (full beneficiary) because he collects the private benefits of control (capital allocation, compensation discretion, reputational halo), bears minimal downside (his massive wealth and control make exit costly but feasible if questioned), and has the highest time horizon (civilizational — he frames Tesla as a long-term mission). Class A shareholders (power=organized, exit=mobile): d approaches 0.85 (full target) because they pay through forfeited governance rights and diluted valuation (the extraction premium reduces the stock price below what it would be under independent governance), but retain the liquidity to exit if the relationship becomes untenable. Early Class B VC holders: d approaches 0.1 (beneficiary) because they hold the voting supermajority and reap private benefits, but their economic stake is typically smaller than their voting stake (they captured the upside from Series B–D rounds before IPO; post-IPO Class A dilutes their percentage ownership). Employees with equity: d approaches 0.65 (heavy target) because their equity appreciation depends entirely on Musk's decisions (which they cannot influence), and their employment relationship creates status quo bias (mobility cost is high if the alternative is job loss).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insulate long-term R&D from quarterly earnings pressure and activist interference) was real in 2010–2015 when Tesla's survival was uncertain and activist short-sellers threatened hostile action. By 2020, Tesla's market position was unassailable (largest EV manufacturer, profitable, dominant Supercharger network). By 2024, the founding problem is unambiguously dead: Tesla faces no acquisition threat, faces no existential earnings pressure, and operates with >$30B annual free cash flow — conditions where quarterly earnings pressure is a feature, not a bug. The governance structure persists not to solve the founding problem but to preserve Musk's private benefits (capital allocation discretion, compensation extraction, no accountability). This is the canonical mandatrophy: a constraint built to solve a real coordination problem that is now obsolete, persisting because those it benefits (Musk, early VC holders) control the charter and those it harms (Class A shareholders) lack the voting power to change it. The theater ratio rise (0.38 to 0.51) reflects the performative increase: more shareholder activism, more proxy votes, more governance advisory firm warnings — all structurally powerless because the 10:1 vote ratio is immutable without charter amendment. The constraint exhibits the mandatrophy profile: founding problem dead, coordination function atrophied, extraction mechanism hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_benefits_of_control_magnitude,
    'What portion of Tesla''s $1.75T valuation is attributable to Musk''s demonstrated execution ability (present in any governance structure) versus the private benefits Musk captures from supermajority voting (capital allocation discretion, no dividend pressure, no compensation restraint)?',
    'Comparable valuation analysis: peer automakers (Volkswagen, BYD, GM) with independent governance show 30–50% lower enterprise value per unit of technical capability; spin-off scenario modeling if Tesla were forced to distribute Optimus/FSD to independent boards as separate entities.',
    'If private benefits represent >15% of valuation, the structure is primarily extractive. If <5%, the voting concentration primarily enables coordination. The gap determines whether the snare classification holds or whether the constraint is better modeled as rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_benefits_of_control_magnitude, empirical, 'The quantified portion of valuation attributable to private benefits of control versus public-value execution.').

omega_variable(
    alternative_governance_counterfactual,
    'Would Tesla''s rate of innovation, capital efficiency, and long-term value creation improve, remain the same, or decline if Class A shareholders gained pro-rata voting and could constrain Musk through an independent nominating committee?',
    'Counterfactual scenario modeling comparing Tesla''s R&D output vs. peers; natural experiment if forced divestiture occurs (Musk exits or governance reform is imposed); historical analysis of Musk''s time allocation across ventures and correlation with Tesla execution speed.',
    'If innovation improves or plateaus with independent governance, the dual-class structure is revealed as extraction without coordination benefit. If innovation declines sharply, the structure provides genuine coordination value and is better classified as rope or tangled-rope with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_counterfactual, conceptual, 'Whether the governance structure''s private benefits enable better innovation or merely extract from already-viable operations.').

omega_variable(
    kernel_reading_contestation,
    'Is valuation legitimacy grounded in governance structures protecting minority shareholders (this reading''s core), or in Musk''s track record of achieving impossible goals (musk_cult_believer reading), or in DCF cash-flow discounting (dcf_fundamentalist reading), or in technological option value (real_options_technologist reading)?',
    'Empirical test: track Tesla''s actual delivered cash flows vs. promised timelines (Optimus profitability, FSD monetization) over the next 3–5 years. If delivered cash flows fall >20% short of projections, DCF legitimacy collapses and the musk_cult_believer reading is exposed as narrative cover. If cash flows hold, valuation is justified independent of governance. If cash flows hit but Musk''s conflicts of interest (Terafab allocation, SpaceX benefits) can be quantified as value-destroying, this reading''s core (governance protection prevents extraction) is vindicated.',
    'This reading (governance_skeptic) forecloses the musk_cult_believer reading only if empirical evidence shows Musk''s personal decrees are systematically value-destructive AND that independent governance would have corrected them. Coexistence is possible if Musk''s leadership produces value that exceeds the extraction cost; then both readings remain live (he is both valuable AND extractive). The readings compete at the kernel level: which frame legitimizes Tesla''s valuation? Competing legitimacy frames ground different readings; the empirical outcome determines which reading is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'The kernel contest: which valuation legitimacy frame is empirically true?').

omega_variable(
    controlled_company_exemption_abuse,
    'Does the controlled-company exemption (allowing Musk to avoid independent compensation/nominating committees under NYSE rules) enable efficient founder-aligned decision-making, or does it enable compensation extraction that would be rejected by an independent committee?',
    'Benchmarking: compare Musk''s total compensation (salary + options + stock gifts) as percentage of shareholder value vs. peer CEOs operating under independent governance; audit whether compensation committee decisions (e.g., 2018 $56B grant) would survive an independent review standard.',
    'If Musk''s compensation is 2–3x peer levels relative to delivered value, the exemption enables extraction. If it is within peer range and correlates with performance metrics, the exemption enables efficient alignment. This empirical distinction determines whether the compensation structure is a feature of the snare or incidental to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controlled_company_exemption_abuse, empirical, 'Whether the controlled-company exemption enables valuable alignment or pure extraction.').

omega_variable(
    terafab_allocation_conflict_of_interest,
    'Is Tesla''s capital investment in Terafab (Musk''s vertical-integration play benefiting both Tesla and SpaceX) a value-maximizing diversification, or a form of indirect Musk enrichment at Tesla shareholders'' expense?',
    'Return-on-capital analysis: measure Terafab''s contribution to Tesla''s core business (energy/vehicles) vs. its benefits to SpaceX (Musk''s higher-valuation pet project). If Terafab returns exceed Tesla''s cost of capital and competitive alternative uses, it is value-creating. If returns lag peer capex or show benefits flowing primarily to SpaceX, it is extraction disguised as diversification.',
    'Terafab represents the most concrete example of structural conflict under dual-class governance. Independent directors would likely challenge the capital allocation; Musk''s supermajority vote prevents the challenge. Quantifying the extraction here is a material component of the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terafab_allocation_conflict_of_interest, empirical, 'The magnitude of extraction via conflicted capital allocation (Terafab as test case).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(valu_tr_t0, observed).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.42).
narrative_ontology:measurement_basis(valu_tr_t4, observed).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.47).
narrative_ontology:measurement_basis(valu_tr_t8, observed).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.51).
narrative_ontology:measurement_basis(valu_tr_t12, observed).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.53).
narrative_ontology:measurement_basis(valu_tr_t16, projected).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(valu_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(valu_be_t0, observed).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.61).
narrative_ontology:measurement_basis(valu_be_t4, observed).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.68).
narrative_ontology:measurement_basis(valu_be_t8, observed).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(valu_be_t12, observed).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.79).
narrative_ontology:measurement_basis(valu_be_t16, projected).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(valu_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(valu_su_t0, observed).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.54).
narrative_ontology:measurement_basis(valu_su_t4, observed).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(valu_su_t8, observed).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(valu_su_t12, observed).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.74).
narrative_ontology:measurement_basis(valu_su_t16, projected).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(valu_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, spacex_vertical_integration_terafab).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, tesla_fsd_monetization_pathway).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested valuation_legitimacy kernel. The governance_skeptic reading decomposes from three sibling readings (dcf_fundamentalist, musk_cult_believer, real_options_technologist) that ground legitimacy in different mechanisms: cash-flow discounting, founder track record, and technological option value. Each reading instantiates a different constraint with a different epsilon because they measure different observables (governance structure vs. cash flows vs. historical success vs. option value). The network links record the family: each reading influences the empirical tests for its siblings (e.g., if cash flows miss DCF targets, the musk_cult_believer reading must explain why; if governance reform occurs, the real_options reading must model changed incentive structures). The readings coexist in contemporary discourse: different constituencies hold each one, no single reading logically forecloses another within its own epistemic framework, but empirical evidence may vindicate one over others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, organized, 0.85).
constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, institutional, 0.08).
constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
