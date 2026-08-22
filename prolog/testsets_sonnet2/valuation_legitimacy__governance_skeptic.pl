% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Voting Control as Extraction Mechanism (Governance-Skeptic Reading)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the governance-skeptic reading of the
 *   valuation-legitimacy kernel applied to a founder-controlled technology
 *   company: the claim that legitimate valuation requires governance
 *   structures protecting minority shareholders, and that an 82.4%
 *   voting-control stake resting on only 42% equity constitutes extraction
 *   rather than value creation. The referent for extractiveness is the
 *   standing dual-class governance arrangement as this reading assesses it,
 *   not any counterfactual governance-reformed arrangement. The reading
 *   treats the charter's renunciation of corporate opportunities, the absence
 *   of independent compensation and nominating committees under
 *   controlled-company exemptions, and the founder's divided attention across
 *   five-plus companies as structural mechanisms of extraction layered onto a
 *   genuine — but time-bounded — coordination function.
 *
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
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Voting Control as Extraction Mechanism (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'c43e19a5-44bb-42d8-b203-27e2a61bc235').
narrative_ontology:cs_kernel_codification('c43e19a5-44bb-42d8-b203-27e2a61bc235', distributed).
narrative_ontology:cs_authority_grounding('c43e19a5-44bb-42d8-b203-27e2a61bc235', distributed).
narrative_ontology:cs_reading_relation('c43e19a5-44bb-42d8-b203-27e2a61bc235', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c43e19a5-44bb-42d8-b203-27e2a61bc235', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('c43e19a5-44bb-42d8-b203-27e2a61bc235', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('c43e19a5-44bb-42d8-b203-27e2a61bc235', foundational, minority_shareholder_protection_is_valuation_precondition).
narrative_ontology:cs_axiom_status(minority_shareholder_protection_is_valuation_precondition, holdable).
narrative_ontology:cs_axiom_grounding('c43e19a5-44bb-42d8-b203-27e2a61bc235', minority_shareholder_protection_is_valuation_precondition, conventional).
narrative_ontology:cs_axiom('c43e19a5-44bb-42d8-b203-27e2a61bc235', foundational, founder_track_record_cannot_substitute_for_accountability_structure).
narrative_ontology:cs_axiom_status(founder_track_record_cannot_substitute_for_accountability_structure, holdable).
narrative_ontology:cs_axiom_grounding('c43e19a5-44bb-42d8-b203-27e2a61bc235', founder_track_record_cannot_substitute_for_accountability_structure, deontological).
narrative_ontology:cs_reference_frame('c43e19a5-44bb-42d8-b203-27e2a61bc235', single_class_shareholder_primacy_norm).
narrative_ontology:cs_drift_state('c43e19a5-44bb-42d8-b203-27e2a61bc235', post_ipo_dual_class_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c43e19a5-44bb-42d8-b203-27e2a61bc235', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, minority_institutional_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, controlled_company_exemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% of voting control via a 10:1 dual-class share structure while owning only 42% of equity. Sets executive compensation terms, board composition, and strategic direction without independent compensation or nominating committee oversight (controlled-company exemptions apply). Simultaneously runs 5+ other companies (Tesla, SpaceX, X, Neuralink, The Boring Company) and allocates his own attention and the company's opportunities across them under a charter clause that renounces corporate-opportunity claims against him. Faces essentially no exit cost — he can extract compensation, redirect opportunities, or divest at will.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, musk, beneficiary).

% Hold the high-vote share class alongside Musk, giving them outsized influence relative to their capital contribution. Benefit from the control premium embedded in valuation and from governance terms that insulate management decisions from public shareholder challenge. Can exit at favorable terms because their shares carry both economic and control value.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    organized, generational, arbitrage, global).

% Hold the low-vote share class and supply the majority of invested capital while controlling essentially none of the voting power. Cannot elect independent directors, cannot contest executive compensation through normal channels, and cannot force divestment of corporate opportunities to affiliated Musk companies. Exit means selling shares at a price that itself reflects the control structure being contested — there is no way to remain invested and gain a governance voice.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    moderate, biographical, constrained, global).

% Large asset managers and pension funds hold significant Class A positions and have sophisticated governance expertise, but their aggregate voting power is structurally capped below any threshold that could compel change given the 10:1 ratio. Can file shareholder proposals and vote against say-on-pay resolutions, but these are advisory only. Their fiduciary duty to beneficiaries conflicts with continued holding, yet index mandates and diversification requirements limit their ability to exit.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, minority_institutional_investors, payer,
    powerful, biographical, constrained, global).

% Shareholders of Musk's other companies have no seat in this company's governance decisions but are directly affected by how Musk allocates opportunities, attention, and resources across the corporate group. When a valuable opportunity is captured by this company rather than Tesla or SpaceX (or vice versa), these shareholders bear the opportunity cost with no mechanism to object within this constraint's governance structure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_and_spacex_shareholders, excluded,
    powerful, biographical, constrained, global).

% Academic and institutional governance researchers evaluate the dual-class structure, compensation design, and inter-company allocation questions using disclosed filings, proxy statements, and comparative corporate law analysis. They take no capital position but publish assessments that inform investor decisions and regulatory attention.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, corporate_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A controlling founder can execute long-horizon, capital-intensive technological bets (reusable rockets, autonomous vehicles, AI infrastructure) without being deposed by activist investors or short-term earnings pressure — dual-class structures genuinely solve a real coordination problem for capital-intensive, long-payback ventures.
% TRANSFER_FUNCTION: Moves control premium and private benefits of control (compensation packages, allocation of corporate opportunities, insulation from accountability mechanisms) from Class A public shareholders' claim on firm value to Musk and Class B holders, while public shareholders supply the bulk of invested capital.
% ABSENT_VOICES: Tesla and SpaceX shareholders have a direct stake in how opportunities and executive attention are allocated across the Musk corporate group but have no standing within this company's governance structure to contest allocation decisions. Class A shareholders nominally have proxy voting rights but lack the votes to make them consequential.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and controlled-company exemptions disappeared overnight, independent compensation and nominating committees would form, executive pay would be renegotiated under arm's-length review, corporate-opportunity allocation across Musk's companies would require formal board-level conflict processes, and the valuation premium currently attributed to unchecked founder control would likely compress as governance risk repriced into the stock.
% FOUNDING_PROBLEM: Early-stage technology ventures with capital-intensive, decades-long payback horizons (reusable orbital rockets, full self-driving, humanoid robotics) are structurally vulnerable to short-term investor pressure, hostile takeover, or premature strategic pivots that would destroy long-run value before it materializes.
% FOUNDING_PROBLEM_CORROBORATION: Musk and Class B holders attest the founding problem is live — that dispersed public shareholder control would force short-termist decisions incompatible with the company's technological roadmap. Governance researchers, minority institutional investors' proxy voting records, and Delaware Chancery Court litigation over comparable compensation and conflict-of-interest structures at Musk's other companies attest from outside the beneficiary set that the founding problem has been substantially resolved by demonstrated market dominance and access to capital, and that the governance exemptions now function primarily to entrench control rather than to protect the venture from premature disruption.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.52 to 0.78) as the valuation premium attributable to unchecked control compounds and as compensation packages and cross-company opportunity allocations accumulate without independent review. Suppression is substantial (0.71 at endpoint) because Class A shareholders structurally cannot assemble enough votes to contest governance terms regardless of organization — this is a design feature of the 10:1 ratio, not a temporary information or coordination failure. Theater ratio is moderate (0.42): board processes, disclosure filings, and say-on-pay votes exist and are formally conducted, but their advisory-only character and the controlled-company exemptions mean an increasing share of this activity is procedural rather than binding. Accessibility collapse (0.62) reflects that alternative governance arrangements (single-class structure, independent committees) are legally available in principle but practically foreclosed once the IPO structure locks in the vote ratio. Resistance (0.58) captures active proxy contests, shareholder derivative suits, and academic governance critique — this is not a quiescent arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement is a rational solution to a real problem — protecting long-horizon technological bets from short-term investor pressure. From the payer seats, the identical structure computes as an enforced extraction mechanism with no realistic exit or voice. The engine computes this divergence from the structural data (power, exit options, beneficiary/victim declarations); the claimed_type of tangled_rope is authored precisely because both a genuine coordination function (patient capital for capital-intensive ventures) and asymmetric extraction (control premium flowing to a concentrated minority) are present simultaneously, and enforcement (proxy structure, charter provisions, controlled-company exemptions) is active and required to sustain it.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk sits at the extraction end: agenda-setter and primary beneficiary simultaneously, with arbitrage-grade exit (he can divest, renegotiate, or redirect opportunities at will). Early Class B holders share the beneficiary position through the same voting structure. Class A public shareholders and minority institutional investors are targets: they supply the bulk of invested capital, cannot assemble a controlling vote bloc under any realistic coalition given the 10:1 ratio, and their only real exit is divestment at a price that already reflects the contested control structure — this is constrained exit, not mobile exit. Tesla/SpaceX shareholders are excluded rather than positioned on the beneficiary/victim axis at all within this constraint; they are affected but have no standing here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting a nascent, capital-intensive technological venture from short-termist investor pressure — was genuinely live at the company's founding and IPO. Whether it remains live at a $1.75T valuation with demonstrated market dominance and unconstrained capital access is exactly the founding_problem_status='contested' finding: the coordination rationale that justified the governance concession has plausibly been substantially achieved, while the governance concession itself persists and has hardened into a durable extraction channel. Classifying this as tangled_rope rather than snare preserves the genuine coordination function's residual validity while flagging the asymmetric extraction as requiring active enforcement (charter terms, exemptions, vote structure) to sustain — exactly the profile mandatrophy analysis is designed to surface rather than collapsing into either 'pure coordination, nothing to see' or 'pure extraction, no coordination ever existed.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_premium_vs_coordination_value,
    'Does the $1.75T valuation reflect the present value of coordination benefits from insulated founder control (patient capital enabling long-horizon bets), or does it price in private benefits of control that come at public shareholders'' expense with no offsetting value creation?',
    'Event-study analysis of valuation changes around governance-relevant disclosures (compensation package votes, corporate-opportunity allocation decisions, Delaware litigation outcomes) and comparison to single-class peer valuations adjusted for growth and risk profile.',
    'If the premium tracks demonstrated execution capability rather than control-structure announcements, the coordination story strengthens relative to the extraction story; if the premium is insensitive to execution but sensitive to control-entrenchment events, the extraction reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_vs_coordination_value, empirical, 'Whether the valuation premium is attributable to coordination benefit or extraction of private control benefits.').

omega_variable(
    cross_company_allocation_governance,
    'Who actually decides how Musk''s attention, capital, and corporate opportunities are allocated across Tesla, SpaceX, X, Neuralink, and this company, and is that allocation process itself governed or purely discretionary?',
    'Discovery in shareholder derivative litigation, board minutes disclosure, or regulatory investigation into inter-company related-party transactions and opportunity allocation.',
    'If allocation follows a documented, arm''s-length process, the conflict-of-interest concern is mitigated; if allocation is ad hoc and unilateral, it corroborates the extraction reading''s claim that the charter''s corporate-opportunity renunciation is functioning as designed to enable value transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_company_allocation_governance, empirical, 'Whether inter-company opportunity allocation is governed or purely discretionary.').

omega_variable(
    kernel_framing_dependence,
    'Is the correct unit of analysis this single company''s governance structure in isolation, or the entire Musk corporate group''s governance structure as a portfolio — and does that framing choice change whether this reading or the real_options_technologist reading is the more structurally accurate description?',
    'Compare classification outcomes under a single-entity framing (this story) versus a group-level framing that would treat vertical integration and cross-company optionality as the primary value driver; document which framing the SEC, Delaware courts, and institutional investors actually apply in practice.',
    'A group-level framing might shift weight toward the real_options_technologist reading''s optionality account and away from this reading''s extraction account for the same underlying facts; a single-entity framing (adopted here) treats the governance concession as the primary structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_dependence, conceptual, 'Whether entity-level versus group-level framing changes which kernel reading best fits the facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.25).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.29).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.33).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.36).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.39).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.41).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__governance_skeptic, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__governance_skeptic, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__governance_skeptic, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.1).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, musk_cult_believer).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the valuation_legitimacy kernel applied to the same founder-controlled technology company. dcf_fundamentalist assesses legitimacy against discounted proven cash flows (low ε, mountain-adjacent framing); real_options_technologist assesses legitimacy against technological option value from vertical integration (low-to-moderate ε, rope-adjacent framing); musk_cult_believer assesses legitimacy against founder track record (very low ε, near-mountain framing from inside that reading); this governance_skeptic reading assesses legitimacy against minority-shareholder protection and finds substantial, rising extraction (tangled_rope). Each reading shares the same underlying facts about the company but authors a structurally distinct ε because each is a different constraint, not a different observable of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
