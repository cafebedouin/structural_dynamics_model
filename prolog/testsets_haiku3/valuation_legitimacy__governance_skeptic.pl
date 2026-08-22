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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Voting Control as Extraction Mechanism (Governance Skeptic Reading)
 *   domain: corporate_finance/governance/space_economics
 *
 * SUMMARY:
 *   Tesla's dual-class share structure grants Musk 82.4% voting control while
 *   he holds only 42% of equity. This reading (the governance-skeptic reading
 *   of the valuation-legitimacy kernel) interprets the structure as a control
 *   extraction mechanism, not a coordination device. Early (2010–2014) the
 *   super-voting shares served a real function: Tesla was existentially
 *   capital-constrained and needed undistracted visionary leadership to
 *   survive the financial crisis and scale production. By 2018–2025, Tesla
 *   achieved sustained profitability ($16B+ annual net income), established
 *   supply chains, proven production scale, and a dominant market position.
 *   The reading argues the governance structure now persists as extraction:
 *   Musk uses 82.4% voting control to (1) set his own compensation without
 *   independent committee oversight ($56B 2018 package, later $55B tranches);
 *   (2) allocate Tesla's capital and his personal time across five competing
 *   companies (Tesla, SpaceX, X, Neuralink, Boring Company) without
 *   shareholder veto; (3) renounce corporate opportunities so his personal
 *   ventures capture upside without conflict review; (4) collect private
 *   benefits of control (veto power, optionality, reputation leverage) while
 *   public shareholders capture only the measurable profit stream. The $1.75T
 *   valuation (2021 peak) prices in Musk's private benefits of control and
 *   the market's confidence in his vision unconstrained by governance
 *   friction—a premium that evaporates if governance constraints are added.
 *   Class A shareholders (58% of shares by count, 17.6% voting control) are
 *   structurally locked in: they cannot remove directors, veto related-party
 *   transactions, or influence capital allocation, yet the valuation premium
 *   they own is contingent on Musk's unconstrained authority. The extraction
 *   is sustained by suppression: controlled-company exemptions allow Musk to
 *   avoid independent compensation/nominating committees; the charter
 *   renounces corporate opportunities; the dual-class structure makes
 *   shareholder votes on governance non-binding; regulatory and institutional
 *   pressure (proxy advisors, SEC questioning) has not breached the control
 *   structure because Musk's visionary narrative and Tesla's operational
 *   success mute the critique.
 *
 * KEY AGENTS:
 *   - Musk (Class B holder): Institutional-level power, arbitrage-class exit (can sell equity while retaining control), collects compensation and private benefits of control
 *   - Class A public shareholders: Organized power, mobile exit (can sell but not control governance), bear the governance-concentration risk while owning the valuation premium
 *   - Employee shareholders: Powerless, constrained exit (employment-dependent), indexed to Musk's strategic decisions without governance voice
 *   - Independent board candidates: Moderate power, trapped exit (need Musk approval for any board seat), structurally blocked from independent authority
 *   - SEC regulators: Institutional analytical power, can recommend rule changes but cannot unilaterally override charter/voting structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.81).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.72).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.81).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, snare).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Control as Extraction Mechanism (Governance Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'c12f9c92-795a-4620-a44e-01cfd0376942').
narrative_ontology:cs_kernel_codification('c12f9c92-795a-4620-a44e-01cfd0376942', fixed_text).
narrative_ontology:cs_authority_grounding('c12f9c92-795a-4620-a44e-01cfd0376942', extraction).
narrative_ontology:cs_interpretation_layer_present('c12f9c92-795a-4620-a44e-01cfd0376942').
narrative_ontology:cs_reading_relation('c12f9c92-795a-4620-a44e-01cfd0376942', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c12f9c92-795a-4620-a44e-01cfd0376942', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('c12f9c92-795a-4620-a44e-01cfd0376942', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('c12f9c92-795a-4620-a44e-01cfd0376942', foundational, governance_protection_legitimacy).
narrative_ontology:cs_axiom_status(governance_protection_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c12f9c92-795a-4620-a44e-01cfd0376942', governance_protection_legitimacy, deontological).
narrative_ontology:cs_axiom('c12f9c92-795a-4620-a44e-01cfd0376942', foundational, voting_control_alignment_requirement).
narrative_ontology:cs_axiom_status(voting_control_alignment_requirement, holdable).
narrative_ontology:cs_axiom_grounding('c12f9c92-795a-4620-a44e-01cfd0376942', voting_control_alignment_requirement, conventional).
narrative_ontology:cs_reference_frame('c12f9c92-795a-4620-a44e-01cfd0376942', shareholder_protection_governance_standard).
narrative_ontology:cs_drift_state('c12f9c92-795a-4620-a44e-01cfd0376942', contemporary_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c12f9c92-795a-4620-a44e-01cfd0376942', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk_class_b_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_musk_aligned_insiders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, employee_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, employee_shareholders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, debt_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Musk holds 82.4% voting control through 10:1 super-voting Class B shares while owning only 42% of equity. Controls Tesla board, sets CEO compensation (including $56B 2018 pay package later challenged), decides major strategic allocations across Tesla, SpaceX, X, Neuralink, and The Boring Company without shareholder vote thresholds. Exit option: can liquidate equity at public market prices while retaining control indefinitely through voting structure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_class_b_holders, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold majority of Tesla equity (by count: 58% of shares) but have minimal governance participation (17.6% voting control). Cannot remove directors, veto CEO pay, block related-party transactions, or influence strategic direction (Musk's allocation of time across five companies, charter-renounced corporate opportunities, vertical integration decisions that may benefit SpaceX over Tesla shareholders). Exit: can sell shares but cannot control governance; valuation incorporates Musk's private-benefit-of-control premium, which evaporates if they exit.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    organized, biographical, mobile, global).

% Receive equity grants and stock purchase plans at company-set prices as compensation. Structurally dependent on Musk for employment, compensation, and the future value of their shares—have no governance input on the decisions that most affect share value (strategic allocation, related-party transactions, capital structure). Exit requires giving up deferred compensation and realizing tax consequences; professional identity is fused with Tesla employment, making exit psychologically costly.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, employee_shareholders, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, employee_shareholders, beneficiary).

% Would-be independent directors with compensation/nominating committee authority are structurally blocked by Musk's 82.4% voting control and his use of controlled-company exemptions (NYSE rules allow controlled companies to opt out of independent committee requirements). Any candidate would need Musk's approval, which makes independence nominal rather than structural.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, independent_board_candidates, excluded,
    moderate, biographical, trapped, global).

% Tesla carries significant debt ($14B+ in recent years). The governance structure concentrates strategic risk in Musk's decision-making without independent oversight; Musk's divided attention across five companies creates operational and reputational risk that debt-holders bear but cannot veto. They benefit from the extraction because Musk-as-visionary narrative supports valuations and operational execution, but the benefit is contingent and asymmetric.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, debt_holders, beneficiary,
    powerful, biographical, constrained, global).

% Monitor disclosure adequacy, board independence rules, and related-party transaction approvals. Have challenged Musk's compensation and questioned whether controlled-company exemptions adequately protect shareholders in high-risk situations (space/AI development). Can recommend rule changes or enforcement actions, but cannot unilaterally override charter provisions or voting structures once authorized.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, sec_regulators, observer,
    institutional, generational, analytical, national).

% ISS, Glass Lewis recommend shareholder votes on board/pay matters; have recommended against Musk's compensation packages and noted governance risks. Their influence is advisory; ultimate votes are cast by shareholders who hold governance-fragmented equity and have limited alternatives.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, proxy_advisors, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk_class_b_holders).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates long-term strategic decision-making for capital-intensive, moonshot technology programs (EVs, batteries, autonomous driving, space launch, neural interfaces) under a single visionary operator with direct accountability (Musk directly controls execution, not committee bureaucracy).
% TRANSFER_FUNCTION: Moves control of strategic capital allocation and executive compensation decisions from public shareholders to Musk, enabling him to: (1) set his own pay (~$56B package, later ~$55B tranches); (2) allocate Tesla's capital and his personal time across five competing companies (Tesla, SpaceX, X, Neuralink, Boring) without shareholder approval; (3) renounce corporate opportunities so personal ventures (X, Neuralink, Boring) compete for capital and management attention without conflict-of-interest review; (4) collect private benefits of control (reputation, optionality, veto power) while equity-holders capture only the measurable profit stream.
% ABSENT_VOICES: Independent compensation committees, independent nominating committees, institutional investors with governance leverage (CalPERS, Vanguard governance teams recommend against the structure but cannot control outcome), employee share ownership advocates (who would argue for governance participation or clawback clauses), debt-holders who bear operational risk from Musk's divided attention.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and Musk's control were removed overnight (e.g., via charter amendment forced by shareholder vote or regulatory mandate), Tesla's governance would shift to standard Sarbanes-Oxley independent committees, strategic allocation would route through board fiduciary duty and independent director review, related-party transactions would face committee scrutiny, and CEO compensation would be set by independent compensation committees. The $1.75T valuation would undergo fundamental re-pricing because a material portion of it capitalizes Musk's private benefits of control and the market's confidence in his vision unconstrained by governance friction. The company would survive but the organizational and financial structure would rearrange substantially.
% FOUNDING_PROBLEM: Early Tesla (2003-2010) needed undistracted visionary leadership and capital discipline to survive the financial crisis and scale EV production before legacy automakers could enter; governance friction would have slowed decision-making in a capital-constrained, existential-threat period. Super-voting shares gave Musk the authority to sustain long-term strategy without quarterly earnings pressure.
% FOUNDING_PROBLEM_CORROBORATION: Tesla is now the world's most valuable automaker with consistent profitability ($16B+ annual net income in recent years), established supply chains, and demonstrated production scale. Independent analysts (Morgan Stanley, Goldman Sachs) acknowledge Tesla's strategic success but question whether the founding existential condition still justifies governance concentration. Securities analysts and institutional investor surveys show majority view that the founding problem (viability risk, capital discipline) is solved and the governance structure now serves extraction, not coordination.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness runs 0.35 (2010) → 0.81 (2025), a 2.3x increase over the interval. In 2010, the founding problem was live (capital discipline, survival risk) and the governance structure served it; by 2018, the problem was solved (Model 3 scaling, profitability demonstrated) but Musk's compensation (~$56B 2018 package) decoupled sharply from measurable performance, signaling the structure's function had shifted. By 2023–2025, extractiveness stabilizes at 0.78–0.81 because the constraint has matured: Tesla's operational success and market dominance are now Musk's defense against governance critique (the narrative 'he earned it' deflates scrutiny), which means the extraction can persist without rising further—a plateau at high extraction. Theater ratio (0.12 → 0.44) captures the growing share of Musk's publicly-stated rationale that is uncoupled from governance function: early statements emphasized capital discipline and long-term vision (functional), but post-2018 the narrative shifted to 'no CEO should be constrained by committees' and 'Musk's genius requires autonomy' (performative, decorating the extraction). Suppression (0.42 → 0.72) measures the enforcement machinery: controlled-company exemptions, charter renouncements, non-binding shareholder votes on governance, and institutional investor inability to translate criticism into outcomes. The measurement series use a single shared time grid (2010, 2014, 2018, 2021, 2023, 2025) for all three metrics, so the temporal dynamics are aligned and comparable. The claim (snare) and the authored metrics are independent facts: the governance-skeptic reading CLAIMS this is pure extraction (snare), while the metrics descriptively show the extraction level, suppression requirement, and performative overlay. The engine will compute each seat's type from the metrics; divergence between the claim and the computed type is exactly the falsifiability test the corpus exists to capture.
 *
 * PERSPECTIVAL GAP:
 *   From Musk's seat: the dual-class structure and personal benefits are justified as necessary conditions for visionary long-term thinking unconstrained by quarterly earnings pressure and committee consensus-building. He controls the agenda (82.4% voting), has arbitrage-class exit (can sell equity while retaining voting control), and a civilizational time horizon (plans measured in decades, not quarters). The governance structure is coordination—it solves the problem of aligning incentives for moonshot technologies over decades. From Class A shareholders' seats: the same structure operates as extraction. They hold 58% of equity (by share count) but 17.6% of voting control, cannot veto related-party transactions, cannot remove Musk or independent directors, and cannot influence his allocation of time across five competing companies. The valuation they own incorporates a Musk-control premium (~15–25% of value by some analyst estimates), which means their return is contingent on Musk's unconstrained authority. If governance constraints were added, the valuation would reset and their percentage ownership would reflect a different market capitalization. The engine will compute directionality from the power atoms and exit options: Musk (institutional power, arbitrage exit) derives d near 0.0 (full beneficiary); Class A shareholders (organized power, mobile exit, structurally constrained by voting control) derive d near 0.8–0.9 (full target). The company employees (powerless, identity-locked through equity compensation and career dependence) derive d near 1.0 (trapped target). This seat divergence is the signature of snare classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk (agenda_setter, institutional power): d ≈ 0.05–0.10. Beneficiary by structure—controls compensation, capital allocation, strategic direction. Arbitrage-class exit (can liquidate equity while retaining voting control via charter structure). Civilizational time horizon aligns with moonshot objectives. The directionality is near-zero beneficiary because the extraction accrues to him directly and he controls the exit rules. Class A shareholders (payer, organized power): d ≈ 0.85–0.95. Victims by structure—cannot veto governance, cannot remove directors, structurally locked into the valuation premium. Mobile exit (can sell shares) partially mitigates but does not resolve the lock-in, because selling means exiting the premium valuation, which itself is a cost of exit. Geographic scope is global (Tesla trades internationally), spatial_scope=global. Employee shareholders (payer, powerless): d ≈ 0.95–1.0. Trapped by employment dependence and deferred compensation cliffs; cannot influence the strategic decisions that most affect share value; identity-locked through career at Tesla; full target of the extraction mechanism. Debt holders (beneficiary, powerful): d ≈ 0.30–0.40. They benefit from the extraction because Musk-as-visionary narrative supports valuations and operational credibility, which reduces borrowing costs. But the benefit is contingent on continued operational success and Musk's attention—a risk they bear but cannot veto. Modest beneficiary directionality. Independent board candidates (excluded, moderate): d ≈ 0.50 (symmetric). They would be indifferent if admitted, but they are structurally trapped by the fact that meaningful board independence requires Musk's willingness to cede control, which he has no incentive to do. The exclusion is the key structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (viability risk, capital discipline) is coded as 'dead' with corroboration from outside the beneficiary set (securities analysts, institutional investor surveys, SEC testimony). The disappearance verdict is 'world_rearranges' because the governance structure's removal would trigger significant reorganization (independent committee governance, strategic allocation by board oversight, compensation committee-set pay, related-party transaction reviews). Under the snare classification, mandatrophy is PRESENT: the arrangement persists despite the founding problem being solved and outside parties (regulators, proxy advisors, institutional investors) attesting it is now extraction, not coordination. The engine should flag this constraint as mandatrophy=resolved (founding problem dead + disappearance verdict world_rearranges + high extraction + suppression holding the constraint in place). This is exactly the pattern that mandatrophy resolution is designed to detect: the governance structure no longer solves the problem it was built for, yet persists because the beneficiary has institutional power and exit control. The snare classification ensures mandatrophy appears in the output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_vs_current_necessity,
    'Is the governance structure still necessary to solve the founding problem (capital discipline, long-term visionary leadership), or has Tesla''s maturity and market position made governance concentration optional for continued innovation and competitiveness?',
    'Counterfactual: impose independent compensation and nominating committees while keeping Musk as CEO and major shareholder. Measure whether operational performance, innovation velocity, or strategic decision-making deteriorates. Historical comparisons with other mature high-growth tech firms (Apple post-Jobs, Amazon under Bezos with increasing board independence) offer weak but suggestive data.',
    'If governance concentration is no longer necessary, the founding-problem justification for the constraint becomes purely historical retrospective, and the snare classification is strengthened. If performance deteriorates measurably, the coordination framing (visionary leadership requires autonomy) has support and the classification edges toward tangled_rope (coordination function + extraction residue).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_vs_current_necessity, empirical, 'Whether the founding problem condition still holds or is retrospectively justified.').

omega_variable(
    valuation_premium_attribution,
    'What portion of Tesla''s $1.75T valuation (or current valuation) is attributable to Musk''s personal leadership and private benefits of control, vs. the company''s proven operational and market position (supply chain, production scale, profit, brand)?',
    'Valuation model decomposition: vary assumptions about governance independence, CEO succession, board composition, and measure the delta. Market studies comparing Tesla to comparable automakers (Toyota, BYD, traditional EV makers) on earnings multiples, with adjustment for governance premium. Analyst surveys directly asking for Musk-control-premium estimates.',
    'High Musk-control premium (25–40% of valuation) supports the extraction reading: shareholders are paying for Musk''s autonomy, which is a private benefit, not a public value. Low premium (<10%) supports the talent/vision reading: the valuation reflects genuine competitive advantage, and Musk''s control is incidental. The measurement would establish whether the constraint is extracting material wealth from the public shareholder base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valuation_premium_attribution, empirical, 'Quantification of the private-benefit-of-control embedded in valuation.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of governance reform (institutional investor defeat, proxy advisor recommendations ignored, SEC questioning unanswered) structural (regulatory/charter barriers) or internalized (shareholders have accepted the narrative that Musk''s autonomy is justified and governance independence is unnecessary)?',
    'Shareholder sentiment post-major event: if governance reform propositions appear on ballots following operational incidents (SpaceX resource drain, X acquisition controversy, Neuralink animal testing, Boring Company opportunity cost), measure the vote intensity. If >60% of Class A shareholders support governance reform but still lack voting power to implement it, suppression is structural. If <40% support, suppression is internalized (the narrative that Musk''s control is justified has been accepted).',
    'If internalized, the constraint persists with lower pressure because the target population has accepted the extracted terms as legitimate. The effective suppression (what the target population accepts) is lower than the measured suppression (what the control structure enforces). This would require revising the theater_ratio upward (more of the constraint''s persistence is narrative/performance than active enforcement). If structural, the suppression is brittle: an external shock (regulatory change, major incident, succession crisis) could fracture it rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether governance suppression is structural barriers or internalized acceptance.').

omega_variable(
    kernel_reading_alternative_forecloses,
    'Does the governance_skeptic reading foreclose the real_options_technologist reading, or do both readings coexist as live positions?',
    'Examine whether a real-options valuation (which prices in the value of option space created by vertical integration and technological diversity) logically requires unconstrained governance authority for Musk, or whether real-option value could be captured under governance-constrained (board-reviewed) capital allocation. If option value is separable from control authority, the readings coexist. If control authority is a necessary condition for realizing option value (e.g., Musk''s solo veto power over cross-company capital flows is essential to the optionality), the governance_skeptic reading forecloses real_options_technologist because governance constraints would reduce the realized option value.',
    'Coexistence: both readings remain live positions for different parties (governance skeptics and option value enthusiasts can both be right about different aspects). Foreclosure: the governance_skeptic reading''s critique of extraction would extend to the real_options reading''s valuation (the optionality is priced in but cannot be realized without extraction). This is a kernel-reading ambiguity that affects the coherence of the DCF and real-options readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_forecloses, conceptual, 'Whether governance constraints are compatible with real-options valuation logic or are foreclosed by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2010, valuation_legitimacy__governance_skeptic, theater_ratio, 2010, 0.12).
narrative_ontology:measurement_basis(valu_tr_t2010, observed).
narrative_ontology:measurement(valu_tr_t2014, valuation_legitimacy__governance_skeptic, theater_ratio, 2014, 0.18).
narrative_ontology:measurement_basis(valu_tr_t2014, observed).
narrative_ontology:measurement(valu_tr_t2018, valuation_legitimacy__governance_skeptic, theater_ratio, 2018, 0.28).
narrative_ontology:measurement_basis(valu_tr_t2018, observed).
narrative_ontology:measurement(valu_tr_t2021, valuation_legitimacy__governance_skeptic, theater_ratio, 2021, 0.38).
narrative_ontology:measurement_basis(valu_tr_t2021, observed).
narrative_ontology:measurement(valu_tr_t2023, valuation_legitimacy__governance_skeptic, theater_ratio, 2023, 0.41).
narrative_ontology:measurement_basis(valu_tr_t2023, observed).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__governance_skeptic, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(valu_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(valu_be_t2010, valuation_legitimacy__governance_skeptic, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement_basis(valu_be_t2010, observed).
narrative_ontology:measurement(valu_be_t2014, valuation_legitimacy__governance_skeptic, base_extractiveness, 2014, 0.48).
narrative_ontology:measurement_basis(valu_be_t2014, observed).
narrative_ontology:measurement(valu_be_t2018, valuation_legitimacy__governance_skeptic, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement_basis(valu_be_t2018, observed).
narrative_ontology:measurement(valu_be_t2021, valuation_legitimacy__governance_skeptic, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement_basis(valu_be_t2021, observed).
narrative_ontology:measurement(valu_be_t2023, valuation_legitimacy__governance_skeptic, base_extractiveness, 2023, 0.78).
narrative_ontology:measurement_basis(valu_be_t2023, observed).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__governance_skeptic, base_extractiveness, 2025, 0.81).
narrative_ontology:measurement_basis(valu_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2010, valuation_legitimacy__governance_skeptic, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement_basis(valu_su_t2010, observed).
narrative_ontology:measurement(valu_su_t2014, valuation_legitimacy__governance_skeptic, suppression_requirement, 2014, 0.51).
narrative_ontology:measurement_basis(valu_su_t2014, observed).
narrative_ontology:measurement(valu_su_t2018, valuation_legitimacy__governance_skeptic, suppression_requirement, 2018, 0.63).
narrative_ontology:measurement_basis(valu_su_t2018, observed).
narrative_ontology:measurement(valu_su_t2021, valuation_legitimacy__governance_skeptic, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement_basis(valu_su_t2021, observed).
narrative_ontology:measurement(valu_su_t2023, valuation_legitimacy__governance_skeptic, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement_basis(valu_su_t2023, observed).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__governance_skeptic, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(valu_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested valuation_legitimacy kernel. The kernel is a stabilized commitment (Tesla's charter, voting structure, equity distribution) that four different readings evaluate under four different legitimacy criteria (governance protection, discounted cash flows, visionary track record, technological optionality). This story instantiates the governance_skeptic reading and evaluates the standing arrangement by whether governance structures protect minority shareholders. The sibling readings (dcf_fundamentalist, real_options_technologist, musk_cult_believer) are separate constraint stories with different ε values, different beneficiary/victim structures, and different classifications, all referencing the same charter arrangement but deriving legitimacy claims from different standards. The governance_skeptic reading produces the highest ε (0.81 at 2025) because it measures extraction under a legitimacy standard (governance protection) that is NOT satisfied. The decomposition avoids forcing one story to handle multiple observables or forcing the reading-dependent ε values to average or hedge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
