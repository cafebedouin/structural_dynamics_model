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
 *   human_readable: Tesla Dual-Class Governance with Musk Control
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   Tesla's dual-class share structure (10:1 voting ratio) gives Elon Musk
 *   82.4% voting control with 42% equity ownership. The charter renounces
 *   corporate opportunities for Musk, enabling cross-company resource flows
 *   among Tesla, SpaceX, X, Neuralink, The Boring Company, and xAI without
 *   independent review. No independent compensation or nominating committees
 *   exist due to controlled company exemptions. The $1.75T peak valuation
 *   embeds a substantial control premium reflecting Musk's private benefits
 *   of control. Class A shareholders (public) bear the extraction with no
 *   governance voice. The constraint is claimed as tangled_rope — genuine
 *   coordination function (founder-led execution across capital-intensive
 *   domains) coexisting with asymmetric extraction (control premium,
 *   corporate opportunity waiver, absent independent oversight).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.72).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.68).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Tesla Dual-Class Governance with Musk Control").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, '56c0e54d-59e7-4aab-84f0-f9652e3df590').
narrative_ontology:cs_kernel_codification('56c0e54d-59e7-4aab-84f0-f9652e3df590', formalized).
narrative_ontology:cs_authority_grounding('56c0e54d-59e7-4aab-84f0-f9652e3df590', extraction).
narrative_ontology:cs_interpretation_layer_present('56c0e54d-59e7-4aab-84f0-f9652e3df590').
narrative_ontology:cs_reading_relation('56c0e54d-59e7-4aab-84f0-f9652e3df590', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('56c0e54d-59e7-4aab-84f0-f9652e3df590', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('56c0e54d-59e7-4aab-84f0-f9652e3df590', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('56c0e54d-59e7-4aab-84f0-f9652e3df590', foundational, minority_shareholder_protection_required_for_legitimate_valuation).
narrative_ontology:cs_axiom_status(minority_shareholder_protection_required_for_legitimate_valuation, holdable).
narrative_ontology:cs_axiom_grounding('56c0e54d-59e7-4aab-84f0-f9652e3df590', minority_shareholder_protection_required_for_legitimate_valuation, deontological).
narrative_ontology:cs_axiom('56c0e54d-59e7-4aab-84f0-f9652e3df590', foundational, dual_class_structure_enables_private_benefits_extraction).
narrative_ontology:cs_axiom_status(dual_class_structure_enables_private_benefits_extraction, holdable).
narrative_ontology:cs_axiom_grounding('56c0e54d-59e7-4aab-84f0-f9652e3df590', dual_class_structure_enables_private_benefits_extraction, empirically_contingent).
narrative_ontology:cs_axiom('56c0e54d-59e7-4aab-84f0-f9652e3df590', secondary, controlled_company_exemption_abused_when_controller_has_conflicts).
narrative_ontology:cs_axiom_status(controlled_company_exemption_abused_when_controller_has_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('56c0e54d-59e7-4aab-84f0-f9652e3df590', controlled_company_exemption_abused_when_controller_has_conflicts, conventional).
narrative_ontology:cs_reference_frame('56c0e54d-59e7-4aab-84f0-f9652e3df590', governance_protected_valuation).
narrative_ontology:cs_drift_state('56c0e54d-59e7-4aab-84f0-f9652e3df590', musk_era_tesla_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56c0e54d-59e7-4aab-84f0-f9652e3df590', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, elon_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, tesla_board).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, institutional_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, minority_shareholder_protection_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, governance_premium_valuation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 42% equity but 82.4% voting control via Class B shares (10:1 ratio). Serves as CEO of Tesla while leading SpaceX, X, Neuralink, The Boring Company, and xAI. Charter renounces corporate opportunities for him, allowing cross-company resource flows (e.g., Terafab benefiting Tesla/SpaceX) without independent review. Sets compensation without independent committee oversight (controlled company exemption). Extracts private benefits of control priced into $1.75T valuation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, elon_musk, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, elon_musk, beneficiary).

% Early investors and insiders holding Class B shares with 10:1 voting rights. Benefit from Musk's control maintaining strategic direction they backed. Their voting power is disproportionate to economic stake. Can exit by selling shares but lose governance influence. Aligned with Musk's long-horizon bets.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    powerful, generational, mobile, global).

% Public shareholders holding Class A shares with 1 vote per share. No governance rights — cannot elect directors, approve compensation, or block related-party transactions. Valuation prices in Musk's private benefits of control (estimated 15-25% control premium). Exit options limited: sell at market price reflecting control premium, or hold with no voice. Collective action problems prevent coordinated pressure.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_shareholders, payer,
    moderate, biographical, constrained, global).

% Board dominated by Musk allies and family members (Kimbal Musk, Ira Ehrenpreis, etc.). No independent compensation or nominating committees due to controlled company exemption. Approves Musk's compensation packages and cross-company arrangements. Directors' professional identity and network access tied to Musk ecosystem — exit means losing privileged position.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_board, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, tesla_board, beneficiary).

% Large index funds and active managers holding significant Class A positions. Publicly advocate for governance reforms but structurally constrained: index funds cannot exit; active managers face tracking error and client pressure if they underweight Tesla. Vote against proposals routinely defeated by Class B block. Their stewardship rhetoric exceeds structural leverage.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, institutional_investors, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, institutional_investors, observer).

% Regulatory and judicial oversight bodies. SEC has investigated Musk's tweets and compensation but settlements preserve dual-class structure. Delaware courts (e.g., Tornetta v. Musk) have upheld controlled company exemptions. Their authority is real but bounded by precedent deferring to shareholder-approved charter provisions.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, sec_and_delaware_courts, observer,
    institutional, generational, analytical, national).

% Qualified candidates who would bring independent oversight but are structurally excluded by controlled company governance. Nominating committee controlled by Musk/board allies ensures only aligned directors are slated. Would object to charter provisions renouncing corporate opportunities and absence of independent committees if seated.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, potential_independent_directors, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified strategic direction for capital-intensive, long-horizon ventures (EVs, space, energy, AI) by concentrating decision authority in a founder with proven execution track record. Solves coordination among diverse stakeholders (employees, suppliers, governments) by eliminating governance friction.
% TRANSFER_FUNCTION: Moves governance rights and control premium from Class A shareholders (public) to Musk and Class B holders. Transfers corporate opportunities across Musk's portfolio (Terafab, AI talent, engineering resources) without arm's-length pricing. Moves valuation upside from public shareholders to insiders via control premium embedded in share price.
% ABSENT_VOICES: Potential independent directors and governance experts who would challenge charter provisions renouncing corporate opportunities for Musk and the absence of independent committees. They are excluded by the nominating process controlled by Musk-aligned board. Retail shareholders lack organizational capacity to coordinate.
% DISAPPEARANCE_RATIONALE: If dual-class structure and controlled company exemptions vanished overnight, Tesla would need independent compensation/nominating committees, Musk's corporate opportunity waiver would lapse, and cross-company resource allocations would require arm's-length terms. Musk's voting control would drop to ~42%, enabling shareholder votes on compensation and related-party transactions. Valuation would reprice without control premium.
% FOUNDING_PROBLEM: Early Tesla faced existential funding risk and needed founder-led rapid execution across manufacturing, software, and energy. Dual-class structure was adopted to protect long-term vision from short-term market pressure and activist intervention during capital-intensive scaling phase.
% FOUNDING_PROBLEM_CORROBORATION: Musk and early investors attest the founding problem remains live — competition in EVs/AI/space requires founder speed. Governance scholars (Bebchuk, Hirst, Coates) and institutional investor coalitions (CII, CALPERS) attest the capital-intensive scaling phase is complete; Tesla is now a mature S&P 500 company where the governance structure extracts rather than protects. Delaware Chancery Court rulings (Tornetta) acknowledge the structure's persistence beyond its protective rationale.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the control premium priced into valuation and the corporate opportunity waiver transfer value from public shareholders to Musk/Class B holders. Suppression (0.68) reflects active maintenance: controlled company exemptions block independent committees; nominating process excludes independent directors; charter provisions require supermajority Class B vote to amend. Theater ratio (0.45) is moderate — the coordination function (unified long-horizon execution) is real but increasingly performative as Tesla matures; governance theater (board independence rituals, compensation consultant opinions) masks extraction. Accessibility collapse (0.62) — alternatives (independent board, single-class recap) are structurally blocked. Resistance (0.55) — institutional investors file proposals, litigation occurs (Tornetta), but structural barriers prevent meaningful change.
 *
 * PERSPECTIVAL GAP:
 *   From Musk/Class B seat: the structure enables the coordination that created $1.75T value; extraction metrics reflect market pricing of founder optionality, not theft. From Class A seat: the same structure is a snare — coordination function has atrophied, extraction persists. From institutional investor seat: trapped in a tangled_rope — genuine coordination value exists but extraction is excessive and unchecked. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk and early Class B holders are structural beneficiaries (d near 0.0) — they collect control premium and private benefits. Class A shareholders are structural targets (d near 1.0) — they pay via depressed governance rights and valuation embedding extraction. Board members are identity_locked (d ~0.7) — professional identity fused to Musk ecosystem. Institutional investors are constrained payers (d ~0.65) — economic exposure forces participation despite governance objections. SEC/courts are analytical observers (d ~0.5). Potential independent directors are trapped excluded (d ~0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting founder-led execution during existential scaling) was live in 2010-2018. By 2020-2024, Tesla achieved scale, profitability, S&P 500 inclusion — the protective rationale is contested/dead. Yet the structure persists and extraction intensifies (rising ε, theater, suppression). This is mandatrophy: the mandate (protect long-term execution) has outlived its function, but the constraint remains because Musk/Class B benefit and Class A cannot coordinate exit. The classification prevents mislabeling: not pure snare (coordination was real and partly persists), not pure rope (extraction is asymmetric and enforced), not mountain (constructed, not natural). Tangled_rope captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_vs_execution_causality,
    'Is Tesla''s execution speed and capital allocation efficiency causally dependent on Musk''s unilateral control, or would independent governance achieve similar outcomes with less extraction?',
    'Counterfactual analysis: compare Tesla''s capital efficiency and execution speed against mature peers with independent boards (e.g., Toyota, Microsoft) controlling for industry and growth stage. Natural experiment if Delaware court mandates governance changes.',
    'If unilateral control is causally necessary for coordination benefits, the tangled_rope classification holds with high coordination value. If independent governance could achieve similar outcomes, the coordination function is overstated and the constraint trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_vs_execution_causality, empirical, 'Causal attribution of execution outcomes to governance structure vs. founder capability.').

omega_variable(
    control_premium_quantification,
    'What portion of Tesla''s valuation premium over peers is attributable to Musk''s private benefits of control vs. genuine optionality from vertical integration?',
    'Event studies around governance proposals (2018 compensation package vote, 2022 shareholder proposals), cross-sectional regression of valuation multiples on governance scores controlling for growth/optionality metrics, analysis of SpaceX/X/Neuralink valuation marks implying cross-subsidy.',
    'A large control premium (15%+) supports high extractiveness and snare/tangled_rope classification. A small premium (<5%) suggests market prices governance neutrally, reducing extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_quantification, empirical, 'Decomposition of valuation premium into governance extraction vs. technological optionality.').

omega_variable(
    kernel_reading_boundary,
    'Does the governance_skeptic reading foreclose the dcf_fundamentalist, real_options_technologist, or musk_cult_believer readings within a single valuation framework, or do they coexist as competing frameworks?',
    'Analyze whether a single valuation model can simultaneously incorporate governance risk adjustment (this reading), DCF of proven cash flows (dcf_fundamentalist), real options on technology tree (real_options_technologist), and founder track record premium (musk_cult_believer) without internal contradiction.',
    'If they coexist, the kernel has multiple live readings — classification depends on which reading''s structural premises are adopted. If this reading forecloses others, it claims exclusive structural validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between governance_skeptic and sibling valuation_legitimacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2010, valuation_legitimacy__governance_skeptic, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2013, valuation_legitimacy__governance_skeptic, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2016, valuation_legitimacy__governance_skeptic, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2018, valuation_legitimacy__governance_skeptic, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2020, valuation_legitimacy__governance_skeptic, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2022, valuation_legitimacy__governance_skeptic, theater_ratio, 2022, 0.42).
narrative_ontology:measurement(val_leg_gov_skeptic_tr_t2024, valuation_legitimacy__governance_skeptic, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2010, valuation_legitimacy__governance_skeptic, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2013, valuation_legitimacy__governance_skeptic, base_extractiveness, 2013, 0.22).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2016, valuation_legitimacy__governance_skeptic, base_extractiveness, 2016, 0.35).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2018, valuation_legitimacy__governance_skeptic, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2020, valuation_legitimacy__governance_skeptic, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2022, valuation_legitimacy__governance_skeptic, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(val_leg_gov_skeptic_be_t2024, valuation_legitimacy__governance_skeptic, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2010, valuation_legitimacy__governance_skeptic, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2013, valuation_legitimacy__governance_skeptic, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2016, valuation_legitimacy__governance_skeptic, suppression_requirement, 2016, 0.45).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2018, valuation_legitimacy__governance_skeptic, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2020, valuation_legitimacy__governance_skeptic, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2022, valuation_legitimacy__governance_skeptic, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement(val_leg_gov_skeptic_su_t2024, valuation_legitimacy__governance_skeptic, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, tesla_compensation_package_2018).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, tesla_corporate_opportunity_waiver).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, spacex_tesla_resource_sharing).

% DUAL FORMULATION NOTE:
% This constraint is the governance_skeptic reading of the valuation_legitimacy kernel. It decomposes the single colloquial claim 'Tesla valuation' into structurally distinct constraints: governance_skeptic (this file, tangled_rope, ε=0.72) vs. dcf_fundamentalist (mountain/rope, low ε) vs. real_options_technologist (tangled_rope, moderate ε) vs. musk_cult_believer (piton/rope, variable ε). The ε values differ because each reading's referent arrangement differs: this reading's referent is the dual-class governance structure; dcf_fundamentalist's referent is the cash flow stream; real_options_technologist's referent is the technology option portfolio; musk_cult_believer's referent is the founder's personal agency. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, organized, 0.7).
constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
