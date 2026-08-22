% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Governance Extraction from Minority Shareholders
 *   domain: economic/organizational
 *
 * SUMMARY:
 *   Dual-class share structures create a voting asymmetry where
 *   founder/insider-held Class B shares (typically 10:1 or 20:1 voting power)
 *   dominate governance while public Class A shareholders bear economic risk.
 *   Controlled-company exemptions from exchange listing standards
 *   (independent board majority, compensation committee independence) strip
 *   mandatory protections. This reading — minority_extraction — frames the
 *   arrangement as a continuous transfer of governance value from
 *   capital-providing minorities to control-holding founders. The constraint
 *   is claimed as tangled_rope because it retains a genuine coordination
 *   function (founder long-horizon latitude) while exhibiting asymmetric
 *   extraction (governance without proportional risk). The measurement series
 *   (2004–2024) tracks the secular rise of dual-class IPOs (Google 2004,
 *   Facebook 2012, Snap 2017, Uber/Lyft/Pinterest 2019, etc.) and the
 *   corresponding increase in extraction magnitude as firms mature without
 *   converting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.68).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.62).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Governance Extraction from Minority Shareholders").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "economic/organizational").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'f368d0e2-63df-4869-acda-4a6b7d1f7d28').
narrative_ontology:cs_kernel_codification('f368d0e2-63df-4869-acda-4a6b7d1f7d28', formalized).
narrative_ontology:cs_authority_grounding('f368d0e2-63df-4869-acda-4a6b7d1f7d28', extraction).
narrative_ontology:cs_interpretation_layer_present('f368d0e2-63df-4869-acda-4a6b7d1f7d28').
narrative_ontology:cs_reading_relation('f368d0e2-63df-4869-acda-4a6b7d1f7d28', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('f368d0e2-63df-4869-acda-4a6b7d1f7d28', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('f368d0e2-63df-4869-acda-4a6b7d1f7d28', foundational, proportional_governance_entitlement).
narrative_ontology:cs_axiom_status(proportional_governance_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('f368d0e2-63df-4869-acda-4a6b7d1f7d28', proportional_governance_entitlement, deontological).
narrative_ontology:cs_axiom('f368d0e2-63df-4869-acda-4a6b7d1f7d28', foundational, capital_risk_governance_alignment).
narrative_ontology:cs_axiom_status(capital_risk_governance_alignment, holdable).
narrative_ontology:cs_axiom_grounding('f368d0e2-63df-4869-acda-4a6b7d1f7d28', capital_risk_governance_alignment, deontological).
narrative_ontology:cs_reference_frame('f368d0e2-63df-4869-acda-4a6b7d1f7d28', proportional_governance_norm).
narrative_ontology:cs_drift_state('f368d0e2-63df-4869-acda-4a6b7d1f7d28', contemporary_dual_class_proliferation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f368d0e2-63df-4869-acda-4a6b7d1f7d28', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_family_trusts).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, early_insider_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_class_a_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_minority_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, employee_stock_plan_participants).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, proportional_governance_entitlement).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, capital_risk_governance_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting Class B shares (typically 10:1 or 20:1 voting power) that confer board control and veto authority over all major corporate actions. Set capital allocation, M&A, and succession strategy unilaterally. Justify control as necessary for long-horizon mission execution. Collect control premium through voting power without proportional capital at risk.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controllers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold concentrated super-voting shares through dynasty trusts and family offices. Benefit from control durability across generations without ongoing operational involvement. Can monetize control premium through selective sales while retaining governance dominance. Structural position is hereditary control without proportional risk.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_family_trusts, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founding_family_trusts, agenda_setter).

% Received super-voting shares pre-IPO at nominal cost. Benefit from founder alignment and control premium. Exit options include secondary sales to strategic buyers who value control adjacency, or conversion to Class A upon transfer restrictions lapsing. Their position is contingent on founder coalition stability.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, early_insider_investors, beneficiary,
    powerful, biographical, mobile, global).

% Purchase single-vote Class A shares at public market prices bearing full economic risk. Hold zero effective governance influence on charter amendments, director elections, or change-of-control transactions. Controlled-company exemptions strip mandatory exchange-listing protections (independent directors, compensation committees). Exit is possible but crystallizes loss of any governance claim.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_class_a_shareholders, payer,
    organized, biographical, constrained, global).

% Deploy large capital pools into Class A shares as fiduciaries. Systematically excluded from governance despite capital at risk. Engage in stewardship campaigns and proxy fights that are structurally unwinnable due to voting asymmetry. Constrained exit: index mandate forces holding; active exit signals governance failure and triggers reputational cost.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_minority_investors, payer,
    institutional, biographical, constrained, global).

% Receive equity compensation exclusively in Class A shares (or RSUs converting to Class A). Bear concentrated idiosyncratic risk — human capital and financial capital tied to same firm. Zero governance voice on decisions affecting their wealth (acquisitions, restructuring, option repricing). Exit requires leaving employment and liquidating, often at unfavorable terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, employee_stock_plan_participants, payer,
    powerless, biographical, trapped, national).

% Advocate for mandatory sunset provisions, voting parity, or controlled-company reform. Analyze structural misalignment and document extraction magnitude. No direct stake in any specific dual-class company but institutional interest in market integrity. Their analysis informs SEC rulemaking and exchange listing standards.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, corporate_governance_reformers, observer,
    institutional, generational, analytical, global).

% Set listing standards that currently permit dual-class structures and controlled-company exemptions. Face competitive pressure to attract listings (revenue) versus governance credibility. Their rulemaking authority is the primary structural lever that could alter the constraint; they currently optimize for listing volume.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_exchanges, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, securities_exchanges, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founder-led firms to access public capital markets without surrendering operational control, solving the coordination problem of aligning dispersed public capital with a concentrated entrepreneurial vision that may require long-horizon, unconventional bets.
% TRANSFER_FUNCTION: Transfers governance authority (voting control, agenda-setting, veto power) from Class A capital providers to Class B holders, while economic risk (downside exposure, liquidity need) remains disproportionately with Class A holders. The control premium is extracted continuously through the voting asymmetry.
% ABSENT_VOICES: Future public shareholders who have not yet purchased but will inherit the governance asymmetry; retail investors who lack sophistication to price the control discount; employees who join post-IPO and receive only Class A equity. They are absent because the structure is fixed at IPO and cannot be renegotiated by subsequent participants.
% DISAPPEARANCE_RATIONALE: If dual-class voting asymmetry and controlled-company exemptions vanished overnight, founder-controlled firms would face immediate governance contests: board composition would shift, capital allocation would be subject to shareholder vote, M&A vetoes would lapse. The control premium would collapse, re-pricing Class B shares downward and Class A upward. Founder-led firms would either convert to single-class or go private.
% FOUNDING_PROBLEM: Founders of high-growth technology and media firms needed to raise public capital while preserving the ability to make long-horizon, unconventional strategic bets that public markets would punish in the short term (e.g., Amazon's infrastructure reinvestment, Meta's pivot investments, Google's moonshots). The dual-class structure was the mechanism to access public capital without surrendering the decision latitude that created the value.
% FOUNDING_PROBLEM_CORROBORATION: Founder-controlled companies attest the problem remains live, citing ongoing long-horizon bets (e.g., Meta's Reality Labs, Tesla's vertical integration) that would be curtailed under quarterly governance. Institutional investors (Council of Institutional Investors, ICGN) and academic governance scholars (Bebchuk, Hirst, Coates) attest the founding problem is substantially solved for mature firms — the long-horizon bets have either succeeded or failed, and the control structure now primarily serves entrenchment. Delaware courts have acknowledged the tension but deferred to contract (informed consent at IPO).
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the governance transfer is large and continuous: Class A holders provide ~90%+ of equity capital but hold <10% of voting power in mature dual-class firms. Suppression (0.62) is substantial because the constraint persists through active legal architecture (charter provisions, controlled-company exemptions, poison pills) that make exit the only realistic dissent channel — and exit crystallizes the governance discount. Theater (0.38) is moderate: the long-horizon mission narrative is real for some firms but increasingly performative for mature companies where the founding bets have resolved. The claim/metric gap is structural: the coordination function is genuine at founding but atrophies over time while extraction persists.
 *
 * PERSPECTIVAL GAP:
 *   From the founder_controller seat, the constraint is a rope (coordination enabling long-horizon value creation). From the public_class_a_shareholder seat, it is a snare (extraction with no exit). From the institutional_minority_investor seat, it is a tangled_rope (they coordinate capital deployment but are systematically extracted from). The engine computes this per-seat divergence from the structural data — the single claimed_type does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_controllers and founding_family_trusts are structural beneficiaries (d ≈ 0.15): they collect control premium without proportional capital at risk, with arbitrage-grade exit (can sell control blocks at premium). Early_insider_investors are partial beneficiaries (d ≈ 0.3): they received super-voting shares at nominal cost but face conversion risk. Public_class_a_shareholders, institutional_minority_investors, and employee_stock_plan_participants are targets (d ≈ 0.85, 0.8, 0.9 respectively): they bear full economic risk with zero governance voice, constrained or trapped exit. Corporate_governance_reformers and securities_exchanges are analytical observers (d = 0.5). The engine derives these from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (long-horizon latitude for unconventional bets) is contested: genuine for firms still executing founder-original visions (Tesla, Meta, Google/Alphabet), but attenuated or dead for mature firms where the founding bets have resolved (e.g., Ford-family control, NYT Sulzberger control). The arrangement persists in the latter cases through institutional inertia and the control premium's self-reinforcing value — classic mandatrophy. The theater_ratio rise from 0.18 to 0.38 over the interval tracks this transition from functional coordination to performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'For any given mature dual-class firm, is the founding problem (need for long-horizon latitude) still live, or has the control structure outlived its coordination function?',
    'Case-by-case analysis of whether current strategic bets require insulation from quarterly governance pressure, versus whether the firm operates as a conventional mature business where control serves entrenchment. Track sunset conversion events (e.g., News Corp 2023, some SPAC conversions).',
    'If the founding problem is dead for a firm, its dual-class structure reclassifies from tangled_rope toward snare/piton — extraction without coordination justification. If live, tangled_rope holds. This drives the temporal drift in extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the coordination function persists at maturity or has atrophied').

omega_variable(
    consent_vs_entitlement_boundary,
    'Does informed consent at IPO (disclosure of voting asymmetry) legitimize the ongoing extraction, or does the proportional governance entitlement persist regardless of initial consent?',
    'Legal-theoretical analysis: whether securities law consent framework can waive a continuing structural entitlement. Empirical: whether Class A purchasers price the control discount efficiently (they do not — control discount persists, suggesting consent is not fully informed or priced).',
    'If consent legitimizes, the constraint leans toward rope (coordination by contract). If entitlement persists regardless of consent, the constraint is structurally extractive (tangled_rope/snare) because the transfer continues without ongoing agreement. This is the core disagreement with the disclosure_consent sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_vs_entitlement_boundary, conceptual, 'Whether initial disclosure consent extinguishes the proportional governance claim').

omega_variable(
    control_premium_measurement,
    'What is the quantitative magnitude of the control premium extracted from Class A holders via voting asymmetry, and how does it vary across firm maturity and performance?',
    'Event-study analysis of dual-class unification proposals, controlled-company premium in takeovers, Class A vs Class B price spreads where both trade, and counterfactual valuation under single-class governance.',
    'A large, persistent control premium (empirically 5–15% of enterprise value) confirms substantial extraction. If the premium is near zero, the coordination function dominates. If the premium grows with firm maturity, extraction accumulates (T17 signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_measurement, empirical, 'Magnitude of governance value transfer from minority to controller').

omega_variable(
    reading_foreclosure_structure,
    'Does the minority_extraction reading''s core premise (proportional governance entitlement) logically foreclose the founder_stewardship reading within a single governance framework, or do they coexist as competing legitimate positions?',
    'Analyze whether a single corporate charter could simultaneously enshrine proportional governance as a shareholder right AND grant founder control as a stewardship mechanism. Delaware law currently treats them as coexisting (contractual freedom), but a mandatory proportional-governance rule would foreclose founder_stewardship.',
    'If forecloses: the kernel has a genuine logical contradiction between readings. If coexists_with: the kernel sustains multiple legitimate frameworks simultaneously. Determines the reading_relations declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between minority_extraction and founder_stewardship readings').

omega_variable(
    employee_equity_identity_lock,
    'Are employee_stock_plan_participants identity_locked to the constraint (career identity fused with firm) or merely constrained (financial exit possible but costly)?',
    'Survey and longitudinal data on employee mobility post-vesting, equity concentration in net worth, and self-reported governance voice expectations. Compare turnover rates at dual-class vs single-class firms.',
    'If identity_locked, their directionality d → 1.0 (full target) and effective extraction χ is amplified — the constraint extracts from agents who cannot exit even conceptually. If merely constrained, d ≈ 0.8. This distinction matters for per-seat classification and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employee_equity_identity_lock, empirical, 'Whether employee equity holders are identity-locked or merely constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2004, dual_class_legitimacy__minority_extraction, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2008, dual_class_legitimacy__minority_extraction, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2012, dual_class_legitimacy__minority_extraction, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2016, dual_class_legitimacy__minority_extraction, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2020, dual_class_legitimacy__minority_extraction, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_tr_t2024, dual_class_legitimacy__minority_extraction, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2004, dual_class_legitimacy__minority_extraction, base_extractiveness, 2004, 0.32).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2008, dual_class_legitimacy__minority_extraction, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2012, dual_class_legitimacy__minority_extraction, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2016, dual_class_legitimacy__minority_extraction, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2020, dual_class_legitimacy__minority_extraction, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_be_t2024, dual_class_legitimacy__minority_extraction, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2004, dual_class_legitimacy__minority_extraction, suppression_requirement, 2004, 0.35).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2008, dual_class_legitimacy__minority_extraction, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2012, dual_class_legitimacy__minority_extraction, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2016, dual_class_legitimacy__minority_extraction, suppression_requirement, 2016, 0.56).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2020, dual_class_legitimacy__minority_extraction, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(dual_class_legitimacy__minority_extraction_su_t2024, dual_class_legitimacy__minority_extraction, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, controlled_company_exemptions).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, exchange_listing_standards).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, proxy_access_rules).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dual_class_legitimacy kernel. The kernel decomposes into: (1) minority_extraction (this story) — governance proportionality entitlement; (2) founder_stewardship — concentrated control as long-horizon coordination mechanism; (3) disclosure_consent — Securities Act disclosure as legitimacy ground. The ε values differ substantially: this reading authors ε=0.68 (extraction dominant); founder_stewardship would author ε≈0.25 (coordination dominant); disclosure_consent would author ε≈0.15 (contractual consent dominant). They have different victim/beneficiary structures and different temporal profiles. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, institutional, 0.2).
constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
