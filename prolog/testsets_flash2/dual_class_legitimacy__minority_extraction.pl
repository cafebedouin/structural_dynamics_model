% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Minority Shareholder Governance Rights (Extraction Reading)
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story analyzes the dual-class share structure from the
 *   perspective of 'minority extraction,' where the arrangement is seen as
 *   transferring governance value from public shareholders (Class A) to
 *   founding shareholders (Class B) disproportionate to capital and risk. The
 *   constraint is framed as a Snare, as its persistence relies on suppressing
 *   the voice and exit options of minority shareholders, despite claims of
 *   long-term stewardship. This is one reading of the 'dual_class_legitimacy'
 *   kernel.
 *
 * KEY AGENTS:
 *   - founding_shareholders_class_b: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - public_shareholders_class_a: Primary target/payer (powerless/constrained)
 *   - institutional_investors: Secondary target/payer (organized/constrained)
 *   - securities_regulators: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.85).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.75).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.85).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, snare).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Minority Shareholder Governance Rights (Extraction Reading)").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '027f151b-88dc-410b-9952-0f9fc5edcf1a').
narrative_ontology:cs_kernel_codification('027f151b-88dc-410b-9952-0f9fc5edcf1a', formalized).
narrative_ontology:cs_authority_grounding('027f151b-88dc-410b-9952-0f9fc5edcf1a', extraction).
narrative_ontology:cs_interpretation_layer_present('027f151b-88dc-410b-9952-0f9fc5edcf1a').
narrative_ontology:cs_reading_relation('027f151b-88dc-410b-9952-0f9fc5edcf1a', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('027f151b-88dc-410b-9952-0f9fc5edcf1a', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('027f151b-88dc-410b-9952-0f9fc5edcf1a', foundational, governance_proportional_to_risk_and_capital).
narrative_ontology:cs_axiom_status(governance_proportional_to_risk_and_capital, holdable).
narrative_ontology:cs_axiom_grounding('027f151b-88dc-410b-9952-0f9fc5edcf1a', governance_proportional_to_risk_and_capital, deontological).
narrative_ontology:cs_axiom('027f151b-88dc-410b-9952-0f9fc5edcf1a', foundational, control_without_ownership_is_extraction).
narrative_ontology:cs_axiom_status(control_without_ownership_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('027f151b-88dc-410b-9952-0f9fc5edcf1a', control_without_ownership_is_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('027f151b-88dc-410b-9952-0f9fc5edcf1a', one_share_one_vote_parity).
narrative_ontology:cs_drift_state('027f151b-88dc-410b-9952-0f9fc5edcf1a', contemporary_capital_markets, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('027f151b-88dc-410b-9952-0f9fc5edcf1a', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_shareholders_class_b).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_shareholders_class_a).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds super-voting shares, maintaining control disproportionate to economic ownership. Benefits from the ability to direct corporate strategy, appoint management, and resist hostile takeovers, often at the expense of Class A shareholder value. Can extract private benefits of control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_shareholders_class_b, agenda_setter,
    institutional, generational, arbitrage, global).

% Owns shares with limited or no voting rights, bearing full economic risk without proportional governance influence. Their only recourse is to sell their shares, often at a discount due to the control premium held by Class B, or to engage in costly, often futile, proxy battles.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_shareholders_class_a, payer,
    powerless, immediate, constrained, global).

% Holds significant blocks of Class A shares but remains subject to the control of Class B. Advocates for 'one share, one vote' principles but is often forced to accept dual-class structures to gain access to high-growth companies. Their exit is costly due to large position sizes.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_investors, payer,
    organized, biographical, constrained, global).

% Oversees capital markets and corporate disclosures. While acknowledging the existence of dual-class structures, they are often constrained by legal precedents and lobbying efforts from imposing strict governance parity. Their role is to ensure transparency, not necessarily equity of control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-class structure coordinates capital formation by allowing founders to raise public capital without ceding control, theoretically enabling long-term strategic vision free from short-term market pressures.
% TRANSFER_FUNCTION: Transfers governance value (control rights, strategic autonomy, ability to extract private benefits) from public Class A shareholders to founding Class B shareholders, in exchange for capital investment.
% ABSENT_VOICES: Advocates for 'one share, one vote' and stronger minority shareholder protections are often marginalized in the initial IPO structuring process, where the terms are set by founders and underwriters. Their arguments are heard in academic and policy debates but rarely influence the initial terms of issuance.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight, many companies would either not go public or would restructure their governance, leading to a significant shift in capital markets, corporate control dynamics, and potentially a re-evaluation of founder incentives and long-term strategy vs. shareholder democracy.
% FOUNDING_PROBLEM: Founders of high-growth companies sought to raise significant public capital while retaining control to pursue long-term visions, fearing short-term market pressures would derail innovation and strategic goals.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their allies attest the problem is live, citing the need for insulated leadership. Institutional investors and corporate governance experts, from outside the benefiting parties, argue the problem is largely a pretext for entrenchment and extraction, with evidence of underperformance in controlled firms over the long run.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because Class B shareholders gain control rights far exceeding their economic stake, enabling them to make decisions that may not maximize Class A shareholder value. Suppression is also high (0.75) due to the structural inability of Class A shareholders to influence governance or easily exit without incurring losses. The theater ratio is low (0.20) because while some 'stewardship' rhetoric exists, the primary function is control entrenchment and value transfer, not performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of founding shareholders, the dual-class structure is a legitimate mechanism for long-term value creation and mission protection (a 'founder_stewardship' reading). From the minority extraction reading, it is a mechanism for control entrenchment and value transfer. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders (Class B) are clear beneficiaries, as they retain control with disproportionate voting power (low d). Public shareholders (Class A) and institutional investors are targets, bearing risk without commensurate governance rights and having constrained exit options (high d). Securities regulators are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The 'minority extraction' reading suggests that the original mandate of enabling long-term vision has atrophied into a mechanism for private benefits of control. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism (Rope) or a temporary support (Scaffold), highlighting the persistent, coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_vs_extraction,
    'Is the concentrated control of dual-class structures primarily a mechanism for long-term founder stewardship, or for private benefits of control and extraction from minority shareholders?',
    'Empirical studies correlating dual-class structures with long-term shareholder returns, innovation, and instances of related-party transactions or governance abuses. Analysis of company performance after founder departure or conversion to single-class shares.',
    'If stewardship is dominant, the constraint might be reclassified closer to a Rope or even a Scaffold (if temporary). If extraction is dominant, the Snare classification is reinforced, potentially leading to calls for regulatory intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_stewardship_vs_extraction, empirical, 'Ambiguity between the stated purpose (stewardship) and observed outcome (extraction).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of minority shareholder voice structural (legal/contractual barriers) or internalized (investors accepting the terms as ''the cost of entry'' to high-growth firms)?',
    'Analysis of investor behavior in markets with varying regulatory protections for minority shareholders. Surveys of institutional investor decision-making regarding dual-class IPOs. If suppression persists even with stronger legal protections, internalized factors are more significant.',
    'If internalized, the effective suppression is higher than structural measures suggest, as investors self-limit their resistance. If purely structural, legal reforms could more directly address the issue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for minority shareholders.').

omega_variable(
    kernel_reading_minority_extraction,
    'This constraint is the ''minority_extraction'' reading of the ''dual_class_legitimacy'' kernel. How would the classification change under the ''founder_stewardship'' or ''disclosure_consent'' readings?',
    'Generating separate constraint stories for each sibling reading and comparing their computed classifications and metric profiles. The divergence in extractiveness and suppression would quantify the perspectival gap.',
    'The ''founder_stewardship'' reading would likely yield a lower extractiveness and higher coordination function, potentially classifying as a Rope or Tangled Rope. The ''disclosure_consent'' reading would focus on transparency, potentially shifting the emphasis from extraction to information asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_minority_extraction, conceptual, 'This omega documents that the current constraint is one specific reading of a contested kernel, and its classification is conditional on this interpretive frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t1980, dual_class_legitimacy__minority_extraction, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dual_tr_t1990, dual_class_legitimacy__minority_extraction, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(dual_tr_t2000, dual_class_legitimacy__minority_extraction, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(dual_tr_t2010, dual_class_legitimacy__minority_extraction, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(dual_tr_t2020, dual_class_legitimacy__minority_extraction, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(dual_tr_t2024, dual_class_legitimacy__minority_extraction, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dual_be_t1980, dual_class_legitimacy__minority_extraction, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dual_be_t1990, dual_class_legitimacy__minority_extraction, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(dual_be_t2000, dual_class_legitimacy__minority_extraction, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(dual_be_t2010, dual_class_legitimacy__minority_extraction, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(dual_be_t2020, dual_class_legitimacy__minority_extraction, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(dual_be_t2024, dual_class_legitimacy__minority_extraction, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t1980, dual_class_legitimacy__minority_extraction, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(dual_su_t1990, dual_class_legitimacy__minority_extraction, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(dual_su_t2000, dual_class_legitimacy__minority_extraction, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(dual_su_t2010, dual_class_legitimacy__minority_extraction, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(dual_su_t2020, dual_class_legitimacy__minority_extraction, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(dual_su_t2024, dual_class_legitimacy__minority_extraction, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.15).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, securities_disclosure_requirements).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, corporate_governance_best_practices).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on the extractive aspects for minority shareholders. Sibling readings include 'founder_stewardship' and 'disclosure_consent', which emphasize different aspects of the dual-class structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
