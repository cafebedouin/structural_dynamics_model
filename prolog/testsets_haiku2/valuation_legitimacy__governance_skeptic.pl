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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Voting Control as Valuation Legitimacy Mechanism (Governance-Skeptic Reading)
 *   domain: corporate_finance/technology_governance
 *
 * SUMMARY:
 *   Tesla's dual-class share structure grants Musk 82.4% voting control with
 *   only 42% equity ownership through 10:1 Class B vote ratio. This reading
 *   (governance-skeptic) interprets the structure as a mechanism for
 *   extracting private benefits of control from public shareholders who lack
 *   governance voice. The founding problem — execution risk in an uncertain
 *   EV market requiring unified vision — was real and acute in 2008–2012. By
 *   2018–2020, Tesla had achieved operational maturity, profitability, and
 *   market leadership. Yet the dual-class structure persists, now functioning
 *   primarily to enable unilateral strategic decisions (cross-company
 *   resource allocation, related-party transactions), unaccountable
 *   compensation levels, and a valuation narrative that treats Musk's vision
 *   as insurable against governance risk. The valuation legitimacy story —
 *   that unproven optionality and Musk's track record justify $1.75T
 *   valuation despite governance exclusion of 58% of equity holders — is the
 *   constraint. This reading asserts that valuation legitimacy structurally
 *   depends on independent governance; therefore the constraint is
 *   extractive.
 *
 * KEY AGENTS:
 *   - Musk (through personal wealth stream): control setter, extracts private benefits via charter renunciation, opportunity allocation, and control premium
 *   - Class A shareholders (58% equity, 17.6% votes): victims, absorb governance risk and dilution without voice
 *   - Early Class B holders: beneficiaries, wealth locked into Musk's control premium
 *   - Tesla minority stakeholders (employees, suppliers, creditors): bear opportunity cost of strategic drift
 *   - Public market: prices the optionality narrative rather than governance-adjusted fundamentals
 *   - SEC, institutional investors: observers with limited leverage due to controlled-company exemptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.78).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.71).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.78).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, snare).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Control as Valuation Legitimacy Mechanism (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'c57b6ffd-2713-4904-a00d-196ef089ad2d').
narrative_ontology:cs_kernel_codification('c57b6ffd-2713-4904-a00d-196ef089ad2d', fixed_text).
narrative_ontology:cs_authority_grounding('c57b6ffd-2713-4904-a00d-196ef089ad2d', extraction).
narrative_ontology:cs_interpretation_layer_present('c57b6ffd-2713-4904-a00d-196ef089ad2d').
narrative_ontology:cs_reading_relation('c57b6ffd-2713-4904-a00d-196ef089ad2d', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c57b6ffd-2713-4904-a00d-196ef089ad2d', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('c57b6ffd-2713-4904-a00d-196ef089ad2d', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('c57b6ffd-2713-4904-a00d-196ef089ad2d', foundational, minority_shareholders_require_governance_protection).
narrative_ontology:cs_axiom_status(minority_shareholders_require_governance_protection, holdable).
narrative_ontology:cs_axiom_grounding('c57b6ffd-2713-4904-a00d-196ef089ad2d', minority_shareholders_require_governance_protection, conventional).
narrative_ontology:cs_axiom('c57b6ffd-2713-4904-a00d-196ef089ad2d', foundational, unilateral_control_creates_private_benefit_extraction).
narrative_ontology:cs_axiom_status(unilateral_control_creates_private_benefit_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c57b6ffd-2713-4904-a00d-196ef089ad2d', unilateral_control_creates_private_benefit_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('c57b6ffd-2713-4904-a00d-196ef089ad2d', minority_shareholder_governance_accountability).
narrative_ontology:cs_drift_state('c57b6ffd-2713-4904-a00d-196ef089ad2d', contemporary_2024_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c57b6ffd-2713-4904-a00d-196ef089ad2d', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk_early_class_b_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk_personal_wealth_stream).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, tesla_minority_stakeholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, tesla_employees).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, tesla_employees).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, tesla_suppliers_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets Tesla's board composition, executive compensation, capital allocation to Musk's other companies (Terafab, NeuraLink, The Boring Company), and charter policy through 82.4% voting control. Collects the governance premium (estimated $300–800B of the $1.75T valuation) as private benefit of control. Extracts via related-party transactions, opportunity allocation across portfolio companies, and unilateral strategic decisions. Controls the narrative: frames the dual-class structure as enabling visionary long-term thinking, not as governance suppression.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_personal_wealth_stream, agenda_setter,
    institutional, generational, arbitrage, global).

% Own 58% of equity but hold only 17.6% of voting power. Cannot appoint board members, cannot vote on compensation, cannot propose governance reform. Bear dilution from stock-based compensation packages (Musk receives ≈$50M/year in options) they cannot refuse. Absorb the governance risk discount implicitly (they pay for it by accepting lower governance rights at high valuation). Can exit by selling shares at market price, but cannot change the structure. Subjected to a valuation narrative that treats governance risk as acceptable (Musk's vision insures against it).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_shareholders, payer,
    powerless, biographical, mobile, global).

% Hold Class B shares (10:1 voting) with Musk or early insiders. Wealth is explicitly tied to Musk's continued control and the governance premium it generates. Vote in Musk's alignment or exit cleanly. Benefit from the $300–800B control-premium spread in the valuation without bearing operational responsibility. The constraint's persistence is wealth-protecting for them.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_early_class_b_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Operate Tesla and execute strategy, but strategic direction is set unilaterally by Musk. Compensation skews toward Musk and executives in his network (related-party dynamics). Benefit from Tesla's profitability and growth narrative; bear opportunity cost when Musk's time and resources are divided across SpaceX, NeuraLink, X, etc. Musk's charter renunciation of corporate opportunities means they cannot claim those opportunities for Tesla, even when they would benefit Tesla more than his other companies.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_employees, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, tesla_employees, beneficiary).

% Suppliers and creditors have contracts with Tesla but no governance voice. Musk's unilateral capital allocation (e.g., prioritizing SpaceX battery supply over Tesla supply, or Terafab investment over Tesla capex) creates execution risk for them. Cannot object or renegotiate terms based on strategic shift; constrained to honor existing contracts while Tesla's priorities shift.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_suppliers_creditors, payer,
    moderate, biographical, constrained, global).

% Pension funds, asset managers, index holders own substantial Tesla positions but hold only Class A shares (powerless). Submit governance reform proposals annually; all fail due to Class B majority. Cannot appoint directors, cannot nominate compensation committee members, cannot enforce independent review of related-party transactions. Excluded from governance leverage despite holding significant equity.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, institutional_investors, excluded,
    powerful, biographical, constrained, global).

% Can investigate Tesla's disclosure of related-party transactions and compensation but lacks authority to mandate governance reform. Controlled-company exemptions limit SEC reach. Can document the structure and issue comment, but cannot change it without legislative mandate or Musk's voluntary reform.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, sec_governance_oversight, observer,
    institutional, generational, analytical, national).

% Prices Tesla at $1.75T using the optionality/vision narrative Musk's control story enables. Markets price governance risk as a discount, but the discount is small relative to the optionality premium attributed to Musk. If governance were reformed, market pricing would immediately recalibrate downward (estimated 40–50% valuation drop based on governance-adjusted DCF models).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, public_equity_market, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk_personal_wealth_stream).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading denies there is a coordination function. The declared function (enabling unified vision, long-term optionality, decisive leadership) is treated as post-hoc justification for singular control. No genuine coordination problem is solved by dual-class voting; instead, Musk's control solves a private problem (his wealth maximization through control premium) that Class A shareholders absorb the cost of.
% TRANSFER_FUNCTION: Transfers control authority (votes), valuation legitimacy (the narrative that justifies high equity price despite governance risk), and strategic optionality (unilateral resource allocation across Musk's portfolio companies) from diffuse Class A shareholders to Musk and early Class B holders. Quantitatively: moves an estimated $300–800B governance premium in the valuation from public shareholders to Musk's personal wealth stream; moves $50M/year compensation and option grants to Musk without Class A input; moves capital allocation authority (Terafab, NeuraLink) to Musk's unilateral control.
% ABSENT_VOICES: Independent board members who are not Musk-aligned; governance-reform shareholders (institutional investors, Class A holders) who are excluded by Class B voting majority; regulatory bodies (SEC) who could mandate independent committees but lack the charter-override authority; proxy voting advisors whose recommendations carry no binding weight; academic governance researchers who study dual-class decay but are not in the capital-allocation conversation.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and Musk's 82.4% control vanished overnight, Tesla's governance would immediately shift to independent board composition, majority-independent nominating/compensation committees, constrained related-party transactions, and potentially different strategic focus (less resource allocation to Musk's other companies). Valuation would reprice from $1.75T toward governance-adjusted fundamentals ($850B–1.1T based on DCF and peer multiples). Musk's wealth from Tesla would drop by $200–500B. Capital allocation would no longer be unilateral; institutional investors would gain voting power; strategic decisions would face board-level scrutiny. The world does not automatically rearrange (Tesla would continue operating), but the institutional and financial arrangement would fundamentally shift.
% FOUNDING_PROBLEM: In 2008–2010, Tesla faced existential execution risk: capital constraints, skeptical markets, deep technical uncertainty about EVs. Founders (Musk and early investors) needed unified decision authority to move capital quickly and navigate strategic pivots without consensus drag. The dual-class super-voting structure was a mechanism to retain control despite outside capital requirements.
% FOUNDING_PROBLEM_CORROBORATION: Tesla is now profitable ($15–25B annual net income, 2023–2025), operationally mature with established manufacturing, supply chains, and product lines. Market leadership is established (≈1.8M vehicles annually, 60%+ global EV market share). These facts are attested by audited financial statements, independent analyst reports, and regulatory filings. The survival-critical execution phase ended by 2018. Musk himself and early investors (in interviews and shareholder meetings) acknowledge Tesla is no longer in existential crisis mode. Independent observers (SEC, institutional investors, corporate governance researchers, journalists) attest that the survival problem was real in 2008–2012 but is clearly resolved by 2025. None of these parties — benefiting or not — credibly attest that the dual-class structure remains necessary for Tesla's continued operation or profitability. The constraint persists without its founding problem, a mandatrophy state.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.15 (2008, survival phase) to 0.78 (2025, mature profitable company with unilateral control) as the founding problem died and the constraint's function shifted from necessary execution authority to governance suppression. Theater ratio rises from 0.05 to 0.42 as the justification narrative (visionary optionality, long-term thinking) becomes increasingly performative relative to the extraction it enables — by 2025, proxy governance proposals are routinely submitted and rejected, board-independence arguments are litigated, and the survival story has been replaced with optionality/vision language. Suppression requirement rises from 0.35 to 0.71 as the constraint must now actively suppress governance-reform proposals, exclude institutional investors from nominating authority, and defend related-party transactions — where 2008 suppression was incidental to legitimate unified decision-making, 2025 suppression is structural. Accessibility collapse is moderate (0.68) because exit is mobile at market price, but only to sell the asset, not to change it; alternatives (independent board, governance reform) are formally closed by Class B voting. Resistance is moderate-high (0.59) because institutional investors and governance advocates mount sustained pressure (shareholder proposals, litigation, regulatory comment) despite lacking voting power.
 *
 * PERSPECTIVAL GAP:
 *   Musk and Class B holders experience the constraint as enabling optionality and long-term vision — the beneficiary reading. Class A shareholders experience it as governance suppression that inflates valuation risk they cannot manage — the payer reading. The engine should compute snare for the payer seats (Musk extracts, no genuine coordination function, suppression is structural) and rope-like for Musk's seat (maintains optionality that he genuinely exercises). The perspectival gap is structural: the same constraint (dual-class + charter renunciation) is governance liberator for the controller and governance prison for the powerless shareholders. This is the definitional snare structure — asymmetric extraction sustained by asymmetric power.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk's directionality is near 0.0 (full beneficiary): controls the rules, sets compensation, allocates capital unilaterally, exits at no cost. Class A shareholders' directionality is near 1.0 (full target): zero governance seats, dilution from compensation they cannot vote against, cannot exit without selling the asset, bear the governance risk discount. Early Class B holders' directionality is near 0.1 (beneficiary): wealth locked into Musk's control premium, no operational burden. The asymmetry is the engine's classification lever: institutional and powerful actors (Musk, early Class B) are beneficiaries with low d; powerless and constrained actors (Class A) are victims with high d. This produces a snare classification for the payer seats as designed.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a textbook mandatrophy case (founding problem dead, constraint persists). Mandatrophy status: RESOLVED (the problem it solved is gone; the structure remains). In 2008–2012, unified decision authority was necessary — the founding problem was real and acute. By 2020, Tesla was profitable with established product lines, mature supply chain, $100B+ annual revenue. The execution-speed justification evaporated. The constraint now persists because Musk's control premium is worth $300–800B; removing it would require surrendering that wealth, so the constraint remains defended, now via optionality/vision narrative rather than survival necessity. The theater ratio rise (0.05 → 0.42) documents the shift: 2008 theater is low because the justification (speed) is real; 2025 theater is higher because the justification (vision) is increasingly decoupled from operational necessity. This reading asserts that mandatrophy resolution requires either (a) Musk's voluntary governance reform (near-zero probability), or (b) shareholder vote / regulatory mandate to change the structure (class-A powerlessness prevents this). The constraint persists by institutional inertia and wealth protection, not because it coordinates value creation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optionality_vs_private_benefits_entanglement,
    'Is the measured extractiveness ($300–800B governance premium in the $1.75T valuation) compensation for the optionality value Musk uniquely can exercise, or is it private-benefits extraction disguised by optionality framing?',
    'Counterfactual valuation: model what Tesla would be worth with independent governance but identical optionality set (Terafab tech, Gigafactory roadmap, product pipeline). If valuation drops less than the governance premium, the gap is private benefits. If it drops by the full premium, optionality and governance are inseparable.',
    'If entangled: the constraint is genuinely coordination (optionality requires singular vision). If separable: the constraint is pure extraction (optionality could be exercised under independent governance, but Musk''s control premium is the rent he extracts for the option of singular vision).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optionality_vs_private_benefits_entanglement, empirical, 'Whether the control premium is payment for unique optionality or extractive rent.').

omega_variable(
    suppression_internalization_boundary,
    'Class A shareholders'' inability to governance-vote is structural (Class B majority rule, charter terms). But is their willingness to hold the stock at $1.75T valuation despite governance risk also structural suppression, or a independent belief in the optionality narrative?',
    'Post-governance-reform cohort study: if governance reform occurs (independently elected board, majority-independent committees) and valuation reprices, do Class A shareholders exhibit different trading behavior? If they sell in reaction to governance loss (not valuation drop), suppression was internalized. If they hold despite lower valuation, belief in the narrative is independent.',
    'If internalized: the constraint''s suppression is higher than the raw structural measure suggests — Class A shareholders carry the governance acceptance with them even if control changed. If independent: suppression is purely structural, not internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Extent of internalized suppression beyond structural exclusion.').

omega_variable(
    control_premium_attribution,
    'Of the $300–800B estimated governance premium in Tesla''s $1.75T valuation, how much is attributable to Musk''s unique track record (optionality he alone can execute), and how much to the control authority itself (the power to allocate capital and strategy unilaterally)?',
    'Regression analysis across dual-class and single-class firms: control premium as a function of founder track record, company stage, optionality density, and governance structure. Isolate the control-structure coefficient from the founder-quality coefficient.',
    'If control-structure premium is large relative to founder-quality premium, Musk is extracting governance rent. If founder-quality dominates, the constraint is coordination around his unique talent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_attribution, empirical, 'Decompose control premium into track-record vs. structure components.').

omega_variable(
    kernel_contest_framing_underdetermination,
    'The governance-skeptic reading asserts Musk''s control is extractive because it lacks independent governance; the real_options_technologist reading asserts it is coordination because optionality requires unified vision. These readings make different empirical claims about whether optionality and governance can coexist. Could the disagreement be resolved by data, or is it a fundamental framing choice about what ''governance legitimacy'' means?',
    'Thought experiment: Tesla with independent governance but identical Musk involvement (Musk as CTO/Chief Scientist, non-voting visionary role, majority-independent board sets strategy in consultation with him). Would this ''solve'' the governance-skeptic reading''s concern (independent accountability) while preserving the real_options_technologist''s optionality (Musk''s vision remains operative)? If yes, the constraint is about governance structure, not optionality. If no, the disagreement is about what governance legitimacy means (accountability vs. alignment) and cannot be empirically resolved.',
    'If the thought experiment satisfies both readings: the constraint can be reformed to independent governance + Musk-in-visionary-role, and the gridlock is structural choice, not empirical discovery. If it satisfies only one: the disagreement is empirical (either optionality requires singular control, or it doesn''t) and the issue is foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing_underdetermination, conceptual, 'Whether the kernel contest is ultimately about governance structure or about the meaning of legitimacy itself.').

omega_variable(
    fcf_vs_control_premium_measurement,
    'Tesla''s free cash flow (≈$28B annually, 2023–2025) implies a governance-adjusted DCF valuation of $800–1.1T. The $1.75T valuation implies either: (a) unproven optionality worth $650–950B, (b) governance premium (Musk''s control value) worth $650–950B, or (c) both. How is this difference measured and attributed?',
    'Market pricing of comparable-governance Tesla (hypothetical: spin-off class-A into independent company, keep Musk as visionary advisor). What would the market value each? Difference from $1.75T split is the governance premium vs. optionality split.',
    'This determines whether extractiveness is truly 0.78 or whether optionality genuinely justifies the gap. If extractiveness is 0.78, the constraint is snare. If optionality closes the gap, the constraint is tangled_rope (coordination around optionality + extraction via control premium).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fcf_vs_control_premium_measurement, empirical, 'Decompose the valuation gap between FCF fundamentals and current price into optionality vs. control-premium components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 2008, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2008, valuation_legitimacy__governance_skeptic, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(valu_tr_t2012, valuation_legitimacy__governance_skeptic, theater_ratio, 2012, 0.12).
narrative_ontology:measurement(valu_tr_t2016, valuation_legitimacy__governance_skeptic, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__governance_skeptic, theater_ratio, 2019, 0.31).
narrative_ontology:measurement(valu_tr_t2022, valuation_legitimacy__governance_skeptic, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__governance_skeptic, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(valu_be_t2008, valuation_legitimacy__governance_skeptic, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(valu_be_t2012, valuation_legitimacy__governance_skeptic, base_extractiveness, 2012, 0.28).
narrative_ontology:measurement(valu_be_t2016, valuation_legitimacy__governance_skeptic, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__governance_skeptic, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(valu_be_t2022, valuation_legitimacy__governance_skeptic, base_extractiveness, 2022, 0.71).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__governance_skeptic, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2008, valuation_legitimacy__governance_skeptic, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(valu_su_t2012, valuation_legitimacy__governance_skeptic, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(valu_su_t2016, valuation_legitimacy__governance_skeptic, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__governance_skeptic, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement(valu_su_t2022, valuation_legitimacy__governance_skeptic, suppression_requirement, 2022, 0.67).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__governance_skeptic, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the valuation_legitimacy kernel. The kernel is a contested claim about what makes a $1.75T Tesla valuation legitimate. The governance_skeptic reading (this story) asserts legitimacy requires independent governance and that Musk's 82.4% voting control is extractive. The dcf_fundamentalist reading asserts legitimacy derives from discounted cash flows, not governance structure or optionality narrative. The real_options_technologist reading asserts legitimacy derives from unproven optionality and that vertical integration requires unified control. The musk_cult_believer reading asserts legitimacy derives from Musk's track record, making governance a secondary concern. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and classification. They are linked via network.affects_constraints to enable cross-reading analysis and foreclosure detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
