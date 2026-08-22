% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Real Options Valuation Legitimacy via Technology Portfolio and Vertical Integration
 *   domain: corporate_finance/technology_governance
 *
 * SUMMARY:
 *   SpaceX's $1.75 trillion valuation in the real-options technologist
 *   reading derives not from discounted cash flows of proven assets but from
 *   the present value of a portfolio of technological options with
 *   compounding interdependencies. Starlink ($7.2B EBITDA, proven)
 *   underwrites Starship development (high-variance, enables all downstream
 *   options). Starship enables orbital compute (addresses genuine 62 GW U.S.
 *   power supply gap). Orbital compute reduces cost of lunar missions. Lunar
 *   missions demonstrate Mars-economy viability. Mars is a civilizational
 *   hedge. Vertical integration means success in any segment increases
 *   success probability across the whole portfolio. The real-options
 *   technologist reading claims this valuation is legitimate because it
 *   correctly prices option values; competing readings (DCF fundamentalist:
 *   cash flows only; Musk cult: Musk's track record; governance skeptic:
 *   control extraction) contest the frame. This story instantiates ONLY the
 *   real-options technologist reading as a clean, ε-invariant constraint. The
 *   other readings are separate constraints in the same kernel family.
 *
 * KEY AGENTS:
 *   - Equity investors: hold asymmetric exposure to portfolio optionality; benefit if management's option-interdependency judgment is sound
 *   - SpaceX management: controls portfolio construction and integration strategy; legitimacy rests on option-space framing
 *   - Minority shareholders: hold equity subject to majority control; benefit from portfolio optionality if management is sound; lack governance participation
 *   - Technology market consensus: pricing mechanism that validates or refutes option-value claims; reflected in equity price
 *   - Competing space firms: excluded from integrated portfolio due to lack of capital and technical depth for across-segment option compounding
 *   - Regulatory authorities: monitor whether option-space framing is predictive or a cover story; can degrade option values via licensing and export control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.41).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.28).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.41).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Valuation Legitimacy via Technology Portfolio and Vertical Integration").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, '833bfdc6-a935-454d-9500-74d5a230a934').
narrative_ontology:cs_kernel_codification('833bfdc6-a935-454d-9500-74d5a230a934', distributed).
narrative_ontology:cs_authority_grounding('833bfdc6-a935-454d-9500-74d5a230a934', expertise).
narrative_ontology:cs_interpretation_layer_present('833bfdc6-a935-454d-9500-74d5a230a934').
narrative_ontology:cs_reading_relation('833bfdc6-a935-454d-9500-74d5a230a934', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('833bfdc6-a935-454d-9500-74d5a230a934', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('833bfdc6-a935-454d-9500-74d5a230a934', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('833bfdc6-a935-454d-9500-74d5a230a934', foundational, option_pricing_theory_applies_to_technology_portfolios).
narrative_ontology:cs_axiom_status(option_pricing_theory_applies_to_technology_portfolios, holdable).
narrative_ontology:cs_axiom_grounding('833bfdc6-a935-454d-9500-74d5a230a934', option_pricing_theory_applies_to_technology_portfolios, empirically_contingent).
narrative_ontology:cs_axiom('833bfdc6-a935-454d-9500-74d5a230a934', foundational, vertical_integration_creates_compounding_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_creates_compounding_optionality, holdable).
narrative_ontology:cs_axiom_grounding('833bfdc6-a935-454d-9500-74d5a230a934', vertical_integration_creates_compounding_optionality, empirically_contingent).
narrative_ontology:cs_axiom('833bfdc6-a935-454d-9500-74d5a230a934', secondary, first_mover_advantage_in_space_economy_justifies_concentration).
narrative_ontology:cs_axiom_status(first_mover_advantage_in_space_economy_justifies_concentration, holdable).
narrative_ontology:cs_axiom_grounding('833bfdc6-a935-454d-9500-74d5a230a934', first_mover_advantage_in_space_economy_justifies_concentration, instrumental).
narrative_ontology:cs_reference_frame('833bfdc6-a935-454d-9500-74d5a230a934', technological_option_space_as_valuation_referent).
narrative_ontology:cs_drift_state('833bfdc6-a935-454d-9500-74d5a230a934', contemporary_2024_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('833bfdc6-a935-454d-9500-74d5a230a934', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, equity_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_multiplanetary_hedge).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, minority_shareholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, option_pricing_applied_to_technology_firms).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_creates_compounding_optionality).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, first_mover_advantage_in_space_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold equity claims on the portfolio of proven and unproven technological options. In the real-options frame, they benefit from exposure to asymmetric payoff curves: Starlink proven cash flow offsets Starship development risk; each success in one domain increases option value across the whole system. Exit available: public markets provide continuous liquidity; valuation reasonability is tested by capital availability.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, equity_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Controls the portfolio construction and integration strategy. Decides which options to develop, which to defer, which to combine. In the real-options frame, management's judgment about the interdependencies between segments (Starship enabling orbital compute, orbital compute reducing lunar-mission cost, lunar missions demonstrating Mars-economy viability) is the core value driver. Exit is constrained by career capital in space technology; management legitimacy rests on option-space framing.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_management, agenda_setter,
    institutional, generational, trapped, global).

% Own equity subject to majority control and strategic decisions they do not set. In the real-options frame, they benefit from portfolio optionality if management's judgment about option interdependencies is sound. They bear the cost of portfolio decisions that prove uninformed or capture-driven. Exit available: sell at market prices determined by the options-market consensus; governance participation is unavailable.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_shareholders, payer,
    powerless, biographical, mobile, global).

% Pricing mechanism that aggregates investor beliefs about option values. Market price of equity reflects the collective estimate of the portfolio's present value under the real-options model. The consensus validates or refutes the management claim that vertical integration creates compounding optionality; price behavior is the feedback signal.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, technology_market_consensus, observer,
    analytical, biographical, analytical, global).

% Are excluded from the integrated portfolio because SpaceX holds the first-mover optionality lock. Blue Origin, Axiom, Relativity pursue narrow segments (suborbital tourism, station modules, 3D printing) without the across-segment option compounding that vertical integration enables. Their exclusion from the portfolio is structural, not chosen; they lack the capital and technical depth to build parallel integration.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, competing_space_firms, excluded,
    powerful, biographical, constrained, global).

% Monitor whether the valuation's option-space framing is predictive of market efficiency or a cover story for extraction. They can impose constraints on launch licensing, orbital rights, spectrum allocation, and export control that would degrade option values. The real-options reading's legitimacy depends on whether option prices remain risk-adjusted over time.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, equity_investors).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the technology integration problem: a single firm must coordinate across launch infrastructure, payload development, ground network, and future applications to realize the value of first-mover advantage in the space economy. Vertical integration internalizes the knowledge spillovers and option-value flows that would be externalized in a fragmented market. Without coordination, each segment's value is isolated; with it, success in Starlink proves Starship feasibility, which enables orbital compute, which reduces lunar-mission cost, etc.
% TRANSFER_FUNCTION: Transfers option-value exposure from specialized, powerless investors to a concentrated holding in SpaceX equity. The arrangement moves the right to set portfolio development strategy from public capital markets to centralized management, in exchange for exposure to asymmetric upside if management's option-space judgment proves sound.
% ABSENT_VOICES: Competing space firms (Blue Origin, Relativity, Axiom, others) would argue that option values are speculative, that vertical integration is concentration risk rather than optionality-compounding, and that fragmented competition produces faster learning. They are excluded because capital markets reward the integrated firm's option portfolio more than the specialist firms' individual bets. Lunar and Mars economy stakeholders (future colonists, off-world resource actors) would argue that the option-space frame correctly prices civilizational optionality but underprices governance — the concentrated control of multiplanetary infrastructure raises tail-risk concerns they would articulate if present.
% DISAPPEARANCE_RATIONALE: If SpaceX and its option-space valuation framework disappeared, the space economy would decompose into fragmented segments: launch, communications, manufacturing, habitation, energy would be separated again. Option-value interdependencies would be lost; first-mover advantage would atomize. The $28.5T TAM that the real-options model prices in would remain latent, pursued piecemeal by competing firms at higher total cost. Investors would reprrice equity from the option-space frame into a sum-of-parts or discounted-cash-flow frame, and valuations would compress unless alternative integrators emerged.
% FOUNDING_PROBLEM: The space economy in the 2010s was fragmented and under-capitalized. Launch was expensive and unreliable; communications satellites were separate from launch providers; manufacturing and energy applications were theoretical. No single firm had capital and technical depth to build an integrated value chain. The founding problem was: how to unlock the $28.5T TAM by aligning incentives across segments through vertical integration and pricing the portfolio's option value correctly in equity markets.
% FOUNDING_PROBLEM_CORROBORATION: SpaceX management and option-pricing theorists attest the founding problem is live and the arrangement solves it. DCF-fundamentalist valuators and governance skeptics attest the founding problem was partly solved (Starlink proven) but the remaining option values are speculative and the governance structure extracts rather than creates. Regulatory authorities and competing space firms attest the founding problem exists but are split on whether centralized vertical integration is the best solution. No external corroborator with stake-neutrality has settled the status; the contest remains.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.41) because the constraint's operation depends on genuine technological option-space interdependencies, not pure asymmetric extraction. Investors understand they are pricing options, not guaranteed cash flows. The arrangement does extract value from minority shareholders relative to majority control, but the extraction is transparent in valuation logic, not hidden. Suppression is low (0.28) because the constraint does not require active coercion; market pricing is continuous and reflexive. Theater is low (0.22) because the option-space framing is directly predictive of observable outcomes (Starlink EBITDA validates the first stage; Starship development validates the next). The measurement series shows a gentle upward drift in extractiveness (as unproven options gradually prove or fail, the portfolio's realized option-value mix becomes clearer and management's judgment is tested), a modest rise in theater (as more capital flows to speculative segments and their near-term commercial justification weakens), and stable suppression (no active enforcement machinery is required because capital markets continuously reprrice). The real-options reading is 'rope' because the coordination problem (aligning across-segment development through vertical integration) is genuine, the beneficiaries (investors, humanity) are substantial, and the constraint persists by participant preference, not coercion.
 *
 * PERSPECTIVAL GAP:
 *   The equity investor seat and the management seat should compute differently from the minority shareholder and governance-skeptic seats. Investors and management see genuine option-value compounding and legitimate asymmetric returns to integration; governance skeptics see extraction veiled in option-space language and concentrated control of multiplanetary infrastructure. The engine computes this divergence from the structural data: investors have arbitrage exit (liquid markets) and understand option pricing; minority shareholders have mobile exit but no governance participation; management is trapped by career capital in space technology. The perspective gap is structural, not merely verbal.
 *
 * DIRECTIONALITY LOGIC:
 *   Equity investors approach d=0.2 (strong beneficiaries: they hold asymmetric option exposure and can exit via public markets). SpaceX management approaches d=0.5-0.6 (partially trapped: career capital in space technology, but significant optionality in strategic choices). Minority shareholders approach d=0.6 (partial targets: hold equity subject to concentrated control, benefit from optionality if management is sound, but lack governance participation). The technology market consensus is analytical (d=0.5 by default). Competing space firms are excluded from the coordination, not coordinated with it. Regulatory authorities are analytical observers. The directionality profile reflects the real-options frame's genuine coordination benefit paired with its concentrated control of portfolio strategy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unlock $28.5T TAM via integrated vertical coordination) is CONTESTED in status, not obviously live or dead. This matters for mandatrophy detection. If the founding problem is dead (the TAM is smaller, coordination is less necessary, competing strategies prove superior), then the arrangement persists by extraction and theater rather than genuine coordination, and the type should shift toward snare. If the founding problem is live and well-understood (TAM is real, coordination is necessary, management judgment is sound), then the type holds as rope. The measurement series does not yet show the mandatrophy pattern (low extraction, rising theater, trapped victims). Instead, we see moderate extraction with low theater and stable suppression — consistent with genuine coordination. The contest over founding-problem status is an omega variable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_interdependency_empirical_truth,
    'Are the claimed option-value interdependencies (Starship enables orbital compute, orbital compute enables lunar missions, lunar missions enable Mars) structurally real, or are they narrative claims that overstate correlation?',
    'Empirical tracking of development costs and timelines: does success in Starlink reduce Starship marginal development cost relative to standalone development? Does Starship success reduce lunar-mission cost? Are these reductions explained by technical spillover or by shared capital pools and management attention?',
    'If interdependencies are real and substantial, the real-options frame correctly captures value and the constraint is rope. If interdependencies are overstated, the arrangement extracts by pricing option-value that does not materialize, shifting type toward snare. The measurement series would show rising theater as options fail to correlate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_interdependency_empirical_truth, empirical, 'Whether option-space interdependencies are structural or narrative.').

omega_variable(
    founding_problem_status_contest,
    'Is the founding problem (unlock $28.5T TAM via integrated vertical coordination) live, dead, or something between?',
    'Track whether Starship successfully enables orbital compute within claimed cost/timeline, whether orbital compute proves commercially viable at the claimed addressable market size, whether these successes occur because of vertical integration or despite management distraction and capital misallocation.',
    'If founding problem is dead (the TAM shrinks, competing strategies prove superior, integration becomes a liability), then the arrangement persists by extraction and the type should shift toward snare or piton. If it is live and management judgment proves sound, the rope type holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_contest, empirical, 'Whether the constraint''s founding problem persists or has been superseded.').

omega_variable(
    governance_extraction_vs_coordination_tradeoff,
    'Does concentrated control (82.4% voting power, 42% equity) enable faster option-portfolio decision-making (genuine coordination value), or does it primarily extract through minority-shareholder asymmetry (minority pay for optionality they do not control)?',
    'Comparative analysis of SpaceX''s portfolio development speed vs. public competitors with distributed governance; quantification of minority-shareholder cost vs. public firm peers; measurement of whether Musk''s strategic decisions (e.g., Starship-first, orbital compute investment) were correct in hindsight and whether they would have been approved by distributed governance.',
    'If concentrated control materially accelerates sound decision-making, extraction is a coordination cost and the constraint is rope. If concentrated control primarily enables rent extraction from minorities, the constraint is closer to snare and governance critique applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_extraction_vs_coordination_tradeoff, empirical, 'Whether concentrated control''s governance benefit outweighs its extraction cost.').

omega_variable(
    real_options_vs_dcf_frame_boundary,
    'This reading competes with the DCF-fundamentalist reading in the same kernel. Is the boundary between ''option'' (unproven but valuable) and ''speculative asset'' (overcounted) structurally determinate, or does it depend on observer confidence in management?',
    'Test option prices against realized outcomes: do market prices track implied option values, or are prices driven primarily by management narrative and capital availability? If prices diverge from option-theory predictions as outcomes accumulate, the frame is unstable.',
    'If the option-pricing frame is structurally sound, the real-options technologist reading is well-grounded and the DCF critique applies only to underestimation. If the frame is observer-dependent, both readings remain live and the contest depends on confidence in management — shifting the kernel toward the musk_cult_believer reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_options_vs_dcf_frame_boundary, conceptual, 'Whether option-space framing is frame-invariant or observer-dependent.').

omega_variable(
    minority_shareholder_victim_status,
    'Are minority shareholders in a real-options technologist arrangement beneficiaries (gaining from optionality exposure) or victims (paying for concentrated control without governance)?',
    'Track minority-shareholder returns vs. public market benchmarks and distributed-governance peers. If returns are high and consistent with option-pricing theory, they are beneficiaries. If returns are low despite high option-value claims and management narrative drives equity prices, they are victims.',
    'If victims, the constraint carries a snare component (asymmetric extraction from minorities who do not understand option pricing). If beneficiaries, the rope type holds and the arrangement is coordination with symmetric risk-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_shareholder_victim_status, empirical, 'Whether minority shareholders benefit or suffer from the option-space valuation frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(valu_tr_t0, observed).
narrative_ontology:measurement(valu_tr_t3, valuation_legitimacy__real_options_technologist, theater_ratio, 3, 0.14).
narrative_ontology:measurement_basis(valu_tr_t3, observed).
narrative_ontology:measurement(valu_tr_t6, valuation_legitimacy__real_options_technologist, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(valu_tr_t6, observed).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(valu_tr_t12, observed).
narrative_ontology:measurement(valu_tr_t18, valuation_legitimacy__real_options_technologist, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(valu_tr_t18, observed).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__real_options_technologist, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(valu_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(valu_be_t0, observed).
narrative_ontology:measurement(valu_be_t3, valuation_legitimacy__real_options_technologist, base_extractiveness, 3, 0.32).
narrative_ontology:measurement_basis(valu_be_t3, observed).
narrative_ontology:measurement(valu_be_t6, valuation_legitimacy__real_options_technologist, base_extractiveness, 6, 0.36).
narrative_ontology:measurement_basis(valu_be_t6, observed).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.39).
narrative_ontology:measurement_basis(valu_be_t12, observed).
narrative_ontology:measurement(valu_be_t18, valuation_legitimacy__real_options_technologist, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(valu_be_t18, observed).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__real_options_technologist, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(valu_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__real_options_technologist, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(valu_su_t0, observed).
narrative_ontology:measurement(valu_su_t3, valuation_legitimacy__real_options_technologist, suppression_requirement, 3, 0.2).
narrative_ontology:measurement_basis(valu_su_t3, observed).
narrative_ontology:measurement(valu_su_t6, valuation_legitimacy__real_options_technologist, suppression_requirement, 6, 0.22).
narrative_ontology:measurement_basis(valu_su_t6, observed).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__real_options_technologist, suppression_requirement, 12, 0.25).
narrative_ontology:measurement_basis(valu_su_t12, observed).
narrative_ontology:measurement(valu_su_t18, valuation_legitimacy__real_options_technologist, suppression_requirement, 18, 0.27).
narrative_ontology:measurement_basis(valu_su_t18, observed).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__real_options_technologist, suppression_requirement, 24, 0.28).
narrative_ontology:measurement_basis(valu_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the valuation_legitimacy kernel. The real_options_technologist reading derives legitimacy from option-space pricing theory and technological interdependencies. The dcf_fundamentalist reading contests this by arguing that only proven cash flows justify valuation. The governance_skeptic reading contests the control concentration that enables option-portfolio strategy. The musk_cult_believer reading contests the technical logic by grounding legitimacy in leadership track record. All four are constraints in the same family, linked via affects_constraints; they are alternative readings of the SAME kernel (valuation legitimacy), not separate kernels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
