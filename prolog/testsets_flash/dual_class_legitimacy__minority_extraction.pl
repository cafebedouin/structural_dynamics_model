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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Minority Shareholder Extraction in Dual-Class Structures
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint represents the 'minority extraction' reading of
 *   dual-class share structures, where the disproportionate control held by
 *   founding shareholders is seen as a mechanism for extracting value from
 *   public minority shareholders. It is a Snare because it relies on active
 *   enforcement (legal frameworks, listing rules) to suppress the voice and
 *   influence of those who bear economic risk without governance power. The
 *   coordination story (long-term vision) is viewed as a cover for asymmetric
 *   extraction.
 *
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
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Minority Shareholder Extraction in Dual-Class Structures").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'cb69e5a2-5538-499a-8192-a7f330915d8d').
narrative_ontology:cs_kernel_codification('cb69e5a2-5538-499a-8192-a7f330915d8d', formalized).
narrative_ontology:cs_authority_grounding('cb69e5a2-5538-499a-8192-a7f330915d8d', extraction).
narrative_ontology:cs_interpretation_layer_present('cb69e5a2-5538-499a-8192-a7f330915d8d').
narrative_ontology:cs_reading_relation('cb69e5a2-5538-499a-8192-a7f330915d8d', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('cb69e5a2-5538-499a-8192-a7f330915d8d', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('cb69e5a2-5538-499a-8192-a7f330915d8d', foundational, governance_proportional_to_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('cb69e5a2-5538-499a-8192-a7f330915d8d', governance_proportional_to_risk, deontological).
narrative_ontology:cs_axiom('cb69e5a2-5538-499a-8192-a7f330915d8d', foundational, control_is_economic_value).
narrative_ontology:cs_axiom_status(control_is_economic_value, holdable).
narrative_ontology:cs_axiom_grounding('cb69e5a2-5538-499a-8192-a7f330915d8d', control_is_economic_value, empirically_contingent).
narrative_ontology:cs_reference_frame('cb69e5a2-5538-499a-8192-a7f330915d8d', one_share_one_vote_principle).
narrative_ontology:cs_drift_state('cb69e5a2-5538-499a-8192-a7f330915d8d', contemporary_capital_markets, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb69e5a2-5538-499a-8192-a7f330915d8d', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, controlled_company_board).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares, maintaining control disproportionate to their economic stake. They benefit from the ability to direct corporate strategy, appoint the board, and resist hostile takeovers, often at the expense of minority shareholder value. Their control is legally entrenched.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Own Class A shares with limited or no voting rights, bearing full economic risk without proportional governance influence. They are subject to decisions made by founding shareholders, which may not align with their financial interests. Their primary recourse is to sell their shares, often at a discount due to the control premium.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholders, payer,
    powerless, biographical, constrained, global).

% Manage large pools of capital invested in dual-class companies. While they have more leverage than individual minority shareholders, their ability to influence governance is severely limited by the super-voting structure. They often engage in public advocacy or shareholder proposals, but these are rarely binding.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_investors, payer,
    organized, biographical, constrained, global).

% Oversee capital markets and enforce disclosure requirements. They are tasked with protecting investors but often operate within legal frameworks that permit dual-class structures, focusing on transparency rather than governance parity. They can propose rule changes but face political and industry resistance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Appointed by the founding shareholders, the board's primary loyalty is often to the controlling block rather than all shareholders. They benefit from stability and insulation from shareholder activism, which can lead to entrenchment and reduced accountability.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, controlled_company_board, beneficiary,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows companies to raise public capital while retaining founder control, theoretically enabling long-term strategic vision free from short-term market pressures. It coordinates capital formation with founder autonomy.
% TRANSFER_FUNCTION: Transfers governance value (control rights, decision-making power, resistance to takeovers) from public minority shareholders to founding shareholders, disproportionate to economic ownership.
% ABSENT_VOICES: Advocates for 'one share, one vote' principles, who argue that governance should always be proportional to economic risk, are systematically excluded from the decision-making process that establishes or maintains dual-class structures. Their arguments are often dismissed as short-termism.
% DISAPPEARANCE_RATIONALE: If dual-class structures vanished overnight, many companies would face immediate pressure to equalize voting rights, potentially leading to changes in board composition, strategic direction, and increased susceptibility to takeovers. Capital markets would likely adjust pricing for control premiums, and investor protections would be re-evaluated.
% FOUNDING_PROBLEM: Founders of innovative companies sought to raise significant capital from public markets without ceding control, fearing that short-term investor demands would compromise long-term vision and mission.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their allies attest the problem is still live, citing examples of activist investors pushing for short-sighted gains. Institutional investors and corporate governance experts, from outside the benefiting parties, argue that the problem is largely a pretext for entrenchment and extraction, with little evidence of superior long-term performance for dual-class firms.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).

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
 *   Extractiveness is high (0.85) because minority shareholders bear full economic risk but have minimal governance influence, leading to a transfer of control premium to founders. Suppression is also high (0.75) due to legal and structural barriers that prevent minority shareholders from exercising proportional power or exiting without penalty. The theater ratio is low (0.2) because the mechanism is functional in its extractive purpose, with little performative maintenance; the 'long-term vision' narrative is a justification, not a theatrical facade for a defunct function.
 *
 * PERSPECTIVAL GAP:
 *   Founding shareholders perceive dual-class structures as a legitimate mechanism for long-term stewardship and value creation, aligning with the 'founder_stewardship' reading. Minority shareholders, however, experience it as a structural disadvantage where their capital is used to fund ventures over which they have no meaningful control, aligning with this 'minority_extraction' reading. Securities regulators often attempt to bridge this gap by focusing on disclosure, which is the core of the 'disclosure_consent' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are clear beneficiaries (d=0.0) as they retain control disproportionate to their economic stake. Minority shareholders and institutional investors are targets (d=1.0) as they bear economic risk without governance power. Securities regulators are observers (d=0.5) tasked with oversight but often constrained by existing legal frameworks. The controlled company board is a beneficiary (d=0.1) due to its alignment with the controlling block.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (enabling long-term vision) is contested. From this reading, the original problem of short-termism has either been solved or was never as severe as claimed, and the dual-class structure now primarily serves to entrench control and facilitate extraction. The persistence of the structure, despite growing evidence of its costs to minority shareholders, indicates a potential mandatrophy where the mechanism outlives its stated function and becomes a pure extraction vehicle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_premium_valuation,
    'What is the true economic value of the control premium embedded in super-voting shares, and how much of this value is transferred from minority shareholders?',
    'Empirical studies comparing valuations of dual-class vs. single-class companies, and analysis of share price movements following changes in governance structures or control blocks.',
    'A high, quantifiable control premium would strengthen the ''minority extraction'' reading by demonstrating a clear transfer of value. A low or negligible premium would weaken it, suggesting other factors are at play.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(control_premium_valuation, empirical, 'Quantifying the value transfer from minority to controlling shareholders.').

omega_variable(
    long_term_performance_correlation,
    'Is there a statistically significant correlation between dual-class structures and superior long-term company performance, or is the ''long-term vision'' justification a narrative cover?',
    'Longitudinal studies comparing the financial performance (e.g., innovation, profitability, market capitalization growth) of dual-class companies against single-class peers, controlling for industry and size.',
    'Strong evidence of superior long-term performance would lend credence to the ''founder stewardship'' reading and challenge the ''minority extraction'' claim. Lack of such evidence would support the extraction narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_performance_correlation, empirical, 'Empirical basis for the ''long-term vision'' justification.').

omega_variable(
    governance_vs_disclosure_framing,
    'Is the core problem of dual-class structures one of inadequate governance rights (minority extraction) or insufficient disclosure and informed consent (disclosure consent)?',
    'Analysis of regulatory effectiveness: if enhanced disclosure alone fails to mitigate the negative impacts on minority shareholders, it suggests the problem is structural governance, not just information asymmetry.',
    'If disclosure is insufficient, the ''minority extraction'' reading is strengthened. If disclosure is sufficient and investors still choose to invest, it strengthens the ''disclosure consent'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_vs_disclosure_framing, conceptual, 'Framing the problem as governance rights vs. disclosure adequacy.').


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
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('minority_extraction') of the 'dual_class_legitimacy' kernel. It focuses on the transfer of governance value from public to founder, contrasting with 'founder_stewardship' (which emphasizes long-term vision) and 'disclosure_consent' (which emphasizes informed investor choice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
