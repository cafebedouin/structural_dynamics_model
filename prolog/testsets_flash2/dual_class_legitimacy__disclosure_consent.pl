% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy: Disclosure and Informed Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the legitimacy of
 *   dual-class share structures, where legitimacy is derived from robust
 *   disclosure under securities law and the informed consent of investors. It
 *   posits that if investors are fully aware of the governance disparity when
 *   they purchase shares, the structure is a legitimate contractual
 *   arrangement, not an extractive one. This reading emphasizes contractual
 *   freedom and transparency over governance parity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.15).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.2).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy: Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'bfa9f086-7e52-4e55-947a-a40949cbad92').
narrative_ontology:cs_kernel_codification('bfa9f086-7e52-4e55-947a-a40949cbad92', formalized).
narrative_ontology:cs_authority_grounding('bfa9f086-7e52-4e55-947a-a40949cbad92', lineage).
narrative_ontology:cs_interpretation_layer_present('bfa9f086-7e52-4e55-947a-a40949cbad92').
narrative_ontology:cs_reading_relation('bfa9f086-7e52-4e55-947a-a40949cbad92', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('bfa9f086-7e52-4e55-947a-a40949cbad92', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('bfa9f086-7e52-4e55-947a-a40949cbad92', foundational, investor_informed_consent_is_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(investor_informed_consent_is_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bfa9f086-7e52-4e55-947a-a40949cbad92', investor_informed_consent_is_sufficient_for_legitimacy, deontological).
narrative_ontology:cs_axiom('bfa9f086-7e52-4e55-947a-a40949cbad92', secondary, securities_disclosure_ensures_informed_consent).
narrative_ontology:cs_axiom_status(securities_disclosure_ensures_informed_consent, holdable).
narrative_ontology:cs_axiom_grounding('bfa9f086-7e52-4e55-947a-a40949cbad92', securities_disclosure_ensures_informed_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('bfa9f086-7e52-4e55-947a-a40949cbad92', contractual_freedom_and_disclosure).
narrative_ontology:cs_drift_state('bfa9f086-7e52-4e55-947a-a40949cbad92', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bfa9f086-7e52-4e55-947a-a40949cbad92', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, dual_class_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies that structure their equity with different voting rights, typically giving founders/insiders super-voting shares (Class B) and public investors subordinate shares (Class A). They benefit from maintaining control while raising public capital, asserting that full disclosure makes this a legitimate contractual choice.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, dual_class_companies, agenda_setter,
    institutional, generational, mobile, national).

% Investors who purchase Class A shares in dual-class companies. This reading asserts they are beneficiaries because they enter the contract with full knowledge of the governance structure, and the lower voting rights are priced into the share valuation. Their exit is selling shares on the open market.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, beneficiary,
    moderate, biographical, mobile, global).

% Government bodies (e.g., SEC) responsible for enforcing securities laws, including disclosure requirements. This reading holds that their role is to ensure transparency, not to dictate corporate governance structures, and that adequate disclosure fulfills their mandate.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Groups that argue for one-share, one-vote principles and view dual-class structures as inherently problematic due to governance disparity. This reading excludes their concerns by prioritizing contractual freedom and informed consent over governance parity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, corporate_governance_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates capital formation for companies by allowing founders to retain control while accessing public markets, and for investors to participate in high-growth companies with transparent governance terms.
% TRANSFER_FUNCTION: Enables the transfer of capital from public investors to companies, in exchange for equity with clearly defined (though unequal) voting rights, with the implicit transfer of governance control to founders.
% ABSENT_VOICES: Corporate governance advocates and minority shareholder rights groups are structurally excluded from dictating governance terms, as this reading prioritizes the contractual agreement and disclosure over their concerns about control parity.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures based on disclosure and consent vanished, companies would face significant challenges in raising capital without ceding control, and investors would demand equal voting rights, fundamentally altering capital markets and corporate structures.
% FOUNDING_PROBLEM: Companies needed to raise significant capital to grow, but founders often wished to retain strategic control to execute long-term visions without short-term market pressures. Investors sought access to high-growth opportunities.
% FOUNDING_PROBLEM_CORROBORATION: Dual-class companies and their legal counsel attest that the problem of balancing capital needs with founder control remains live. Securities regulators corroborate the need for clear disclosure in such arrangements, even if they don't endorse the structure itself. Academic legal scholars also document the ongoing tension.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because, from this reading's perspective, any 'cost' to Class A investors (e.g., lower voting rights) is fully priced into the share value and willingly accepted. Suppression is low (0.20) as investors are free to choose whether to buy these shares, and regulators enforce disclosure, not a specific governance model. Theater ratio is minimal (0.05) as the disclosure process is considered genuinely functional. Accessibility collapse is moderate (0.70) because while investors can choose not to invest, the prevalence of dual-class structures in certain high-growth sectors can limit access to those specific opportunities without accepting the terms. Resistance is low (0.10) because, within this framework, the terms are transparent and accepted.
 *
 * PERSPECTIVAL GAP:
 *   This reading inherently creates a perspectival gap with those who prioritize governance parity. From the perspective of corporate governance advocates, the structure is extractive regardless of disclosure. However, this constraint story strictly adheres to the 'disclosure and consent' reading, where such structures are legitimate contractual choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual-class companies are beneficiaries as they gain capital without losing control. Class A investors are also considered beneficiaries because they knowingly enter the contract, and the terms are priced in. Securities regulators are agenda-setters, ensuring the disclosure framework. Corporate governance advocates are excluded, as their concerns about parity are not prioritized by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_depth,
    'How ''informed'' is investor consent, given the complexity of governance structures and potential behavioral biases?',
    'Empirical studies on investor comprehension of complex governance terms and the impact of behavioral economics on investment decisions in dual-class companies.',
    'If consent is found to be systematically less informed than assumed, the extractiveness of the constraint would be re-evaluated upward, potentially shifting classification towards a Tangled Rope or Snare, as the ''contractual choice'' justification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_depth, empirical, 'Assesses the actual efficacy of disclosure in ensuring truly informed consent.').

omega_variable(
    pricing_efficiency_of_governance_disparity,
    'Is the governance disparity in dual-class shares fully and efficiently priced into their valuation by the market?',
    'Event studies and econometric analysis comparing the performance and valuation of dual-class vs. single-class companies, controlling for other factors, especially around governance-related events.',
    'If the market systematically underprices the governance disparity, Class A investors would be victims of extraction, shifting the constraint towards a Snare. If efficiently priced, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_efficiency_of_governance_disparity, empirical, 'Evaluates whether market mechanisms fully account for the control imbalance.').

omega_variable(
    legitimacy_framing_choice,
    'Is the legitimacy of dual-class structures fundamentally a matter of contractual freedom and disclosure, or of governance parity and shareholder rights?',
    'This is a conceptual omega. Resolution depends on which normative framework (contractualism vs. shareholder democracy) is prioritized by legal and economic discourse. No empirical resolution.',
    'Prioritizing contractual freedom supports the current ''rope'' classification. Prioritizing governance parity would fundamentally reframe the constraint as extractive, regardless of disclosure, pushing it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_framing_choice, conceptual, 'The core conceptual disagreement over the basis of legitimacy for dual-class structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.05).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.05).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.05).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__disclosure_consent, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__disclosure_consent, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__disclosure_consent, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__disclosure_consent, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__disclosure_consent, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on disclosure and informed consent. It is distinct from the 'founder_stewardship' and 'minority_extraction' readings, which offer alternative justifications or critiques of dual-class structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
