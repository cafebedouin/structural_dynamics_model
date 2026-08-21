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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Governance as Minority Extraction
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint models dual-class share structures from the perspective
 *   of minority shareholder extraction. It asserts that the disproportionate
 *   control held by founding shareholders, enabled by super-voting shares,
 *   constitutes a transfer of governance value from public investors. The
 *   constraint is actively enforced through corporate charters, securities
 *   law exemptions for 'controlled companies,' and board appointments. The
 *   claimed type is 'snare' because the coordination story (founder
 *   stewardship) is seen as cover for asymmetric extraction, with
 *   identifiable victims (minority shareholders) whose exit options are
 *   suppressed.
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
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Governance as Minority Extraction").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '45778ca5-c00b-40c8-8209-2f1b991ffe2d').
narrative_ontology:cs_kernel_codification('45778ca5-c00b-40c8-8209-2f1b991ffe2d', formalized).
narrative_ontology:cs_authority_grounding('45778ca5-c00b-40c8-8209-2f1b991ffe2d', extraction).
narrative_ontology:cs_interpretation_layer_present('45778ca5-c00b-40c8-8209-2f1b991ffe2d').
narrative_ontology:cs_reading_relation('45778ca5-c00b-40c8-8209-2f1b991ffe2d', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('45778ca5-c00b-40c8-8209-2f1b991ffe2d', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('45778ca5-c00b-40c8-8209-2f1b991ffe2d', foundational, governance_proportional_to_risk_and_capital).
narrative_ontology:cs_axiom_status(governance_proportional_to_risk_and_capital, holdable).
narrative_ontology:cs_axiom_grounding('45778ca5-c00b-40c8-8209-2f1b991ffe2d', governance_proportional_to_risk_and_capital, deontological).
narrative_ontology:cs_axiom('45778ca5-c00b-40c8-8209-2f1b991ffe2d', foundational, control_disproportionate_to_economic_interest_is_extraction).
narrative_ontology:cs_axiom_status(control_disproportionate_to_economic_interest_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('45778ca5-c00b-40c8-8209-2f1b991ffe2d', control_disproportionate_to_economic_interest_is_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('45778ca5-c00b-40c8-8209-2f1b991ffe2d', one_share_one_vote_principle).
narrative_ontology:cs_drift_state('45778ca5-c00b-40c8-8209-2f1b991ffe2d', contemporary_tech_ipo_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('45778ca5-c00b-40c8-8209-2f1b991ffe2d', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, controlled_company_boards).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares, maintaining control disproportionate to their economic interest. They set the company's strategic direction, appoint board members, and are insulated from public shareholder pressure. They benefit from the ability to extract private benefits of control and pursue long-term visions without fear of hostile takeover or short-term market demands.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Own Class A shares with limited or no voting rights, bearing full economic risk without proportional governance influence. They are exposed to potential expropriation of value by controlling shareholders and have limited recourse against management decisions that may not align with their interests. Their exit options are selling shares at a discount or engaging in costly, often futile, litigation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, minority_shareholders, payer,
    powerless, biographical, constrained, global).

% A broad category of institutional and retail investors who hold non-voting or low-voting shares. They provide capital but lack the power to influence governance, relying on market mechanisms and regulatory oversight for protection. They face the risk of value transfer to controlling shareholders and are often forced to accept lower valuations for their shares due to the governance discount.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_investors, payer,
    moderate, biographical, constrained, global).

% Composed largely of appointees loyal to the controlling shareholders, they often prioritize the interests of the super-voting class over minority shareholders. They benefit from stable tenure and the ability to implement the controlling shareholders' vision, even if it conflicts with broader shareholder value maximization.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, controlled_company_boards, beneficiary,
    institutional, biographical, constrained, national).

% Tasked with protecting investors and ensuring fair markets. They observe dual-class structures for potential abuses but are often constrained by existing legal frameworks that permit such arrangements. Their actions are typically reactive, focusing on disclosure and anti-fraud, rather than proactive governance reform.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows companies to raise public capital while retaining founder control, theoretically enabling long-term strategic planning free from short-term market pressures.
% TRANSFER_FUNCTION: Transfers governance value (control rights, decision-making power, ability to extract private benefits) from public and minority shareholders to founding/controlling shareholders, in exchange for capital.
% ABSENT_VOICES: Advocates for 'one share, one vote' principles, institutional investors with strong governance mandates, and academic researchers highlighting the governance discount in dual-class firms are present in public discourse but lack direct power to alter the constraint's structure within existing legal frameworks.
% DISAPPEARANCE_RATIONALE: If dual-class structures were abolished overnight, companies would either have to cede control to public markets, seek alternative private funding, or face significant governance restructuring. Share prices of existing dual-class firms would likely re-rate, and the balance of power in corporate decision-making would fundamentally shift.
% FOUNDING_PROBLEM: Founders of innovative companies needed to raise significant capital from public markets without losing control of their long-term vision to short-term investor demands or hostile takeovers.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their advocates consistently attest that the problem of short-termism and hostile takeovers remains live, especially for companies with long development cycles or mission-driven objectives. Critics (minority shareholders, governance advocates) acknowledge the historical problem but argue that the current structure has become a mechanism for entrenchment and extraction, rather than pure stewardship.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because minority shareholders bear full economic risk without proportional governance rights, leading to a 'governance discount' on their shares and potential expropriation of value. Suppression (0.75) is significant due to legal frameworks that permit dual-class structures, limited shareholder remedies, and the difficulty of organizing dispersed minority shareholders. Theater ratio (0.20) is low because the mechanism is highly functional in achieving its extractive goal; the 'stewardship' narrative is a justification, not a performance masking atrophy. The increasing extractiveness and suppression over time reflect the hardening of these structures and the growing awareness of their costs to minority shareholders.
 *
 * PERSPECTIVAL GAP:
 *   The founding shareholders perceive this as a 'rope' or even a 'mountain' (natural right to control one's creation), enabling long-term value creation. Minority shareholders, however, experience it as a 'snare' due to the asymmetric power and value transfer. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders and controlled company boards are clear beneficiaries (d near 0.0) as they retain control and can extract private benefits. Minority shareholders and public investors are targets (d near 1.0) as they provide capital but lack voice and bear the costs of disproportionate control. Securities regulators are observers (d near 0.5) with an analytical perspective, attempting to balance market efficiency with investor protection.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_discount_quantification,
    'What is the precise, empirically verifiable ''governance discount'' applied to non-voting or low-voting shares in dual-class companies, and how much of this discount is attributable to control extraction versus other factors?',
    'Large-scale econometric studies comparing valuations of dual-class and single-class firms, controlling for industry, size, and performance metrics, coupled with event studies around governance changes.',
    'A robust quantification of the governance discount directly attributable to control extraction would strengthen the ''snare'' classification and provide a basis for regulatory intervention or shareholder activism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_discount_quantification, empirical, 'Empirical measurement of value transfer due to governance asymmetry.').

omega_variable(
    founder_stewardship_vs_extraction,
    'To what extent does concentrated founder control genuinely enable long-term value creation and mission preservation (stewardship), versus facilitating private benefits of control and entrenchment (extraction)?',
    'Longitudinal studies comparing the performance and governance outcomes of dual-class firms with single-class peers over extended periods, analyzing founder tenure, strategic shifts, and related-party transactions.',
    'If stewardship benefits are demonstrably minimal or short-lived, the ''snare'' classification is reinforced. If significant, sustained stewardship benefits are proven, it would challenge the high extractiveness score and push towards a ''tangled_rope'' or even ''rope'' classification from a broader societal perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_stewardship_vs_extraction, empirical, 'Distinguishing genuine stewardship from self-serving entrenchment.').

omega_variable(
    informed_consent_sufficiency,
    'Is the disclosure regime under which minority shareholders acquire their shares truly sufficient to constitute ''informed consent'' to the governance asymmetry, given the complexity of corporate structures and behavioral biases?',
    'Behavioral economics studies on investor comprehension of dual-class risks, legal analysis of disclosure effectiveness, and surveys of investor expectations regarding governance rights.',
    'If consent is found to be systematically uninformed or coerced, the ''disclosure_consent'' reading''s legitimacy is undermined, reinforcing the ''minority_extraction'' reading''s claim of victimhood. If consent is robust, it would challenge the ''snare'' classification by shifting responsibility to the investor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_sufficiency, conceptual, 'The effectiveness of disclosure in mitigating governance asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__minority_extraction, theater_ratio, 5, 0.22).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__minority_extraction, theater_ratio, 10, 0.2).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.18).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__minority_extraction, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__minority_extraction, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__minority_extraction, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__minority_extraction, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, corporate_governance_standards).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, securities_law_exemptions).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on the extractive aspects for minority shareholders. It is linked to sibling readings that emphasize founder stewardship and disclosure-based consent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
