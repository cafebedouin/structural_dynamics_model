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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy: Disclosure and Informed Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint represents the 'disclosure and informed consent' reading
 *   of dual-class share structures. Under this reading, the governance
 *   disparity inherent in dual-class structures is legitimate because
 *   investors (Class A) are fully informed of the terms via regulatory
 *   disclosures (e.g., S-1 filings) and voluntarily choose to invest. The
 *   structure is viewed as a contractual choice, not an extractive mechanism,
 *   as the market prices in the governance disparity. The constraint's
 *   function is to facilitate capital formation under specific governance
 *   terms, with minimal extraction beyond the inherent costs of market
 *   operation and regulatory compliance.
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
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'e3ea9d65-e7dd-44b5-9a89-62924fe4f696').
narrative_ontology:cs_kernel_codification('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', formalized).
narrative_ontology:cs_authority_grounding('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', lineage).
narrative_ontology:cs_interpretation_layer_present('e3ea9d65-e7dd-44b5-9a89-62924fe4f696').
narrative_ontology:cs_reading_relation('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', foundational, investor_informed_consent_legitimizes_governance_disparity).
narrative_ontology:cs_axiom_status(investor_informed_consent_legitimizes_governance_disparity, holdable).
narrative_ontology:cs_axiom_grounding('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', investor_informed_consent_legitimizes_governance_disparity, conventional).
narrative_ontology:cs_axiom('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', secondary, securities_disclosure_ensures_informed_consent).
narrative_ontology:cs_axiom_status(securities_disclosure_ensures_informed_consent, holdable).
narrative_ontology:cs_axiom_grounding('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', securities_disclosure_ensures_informed_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', contractual_freedom_and_disclosure).
narrative_ontology:cs_drift_state('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', contemporary_governance_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e3ea9d65-e7dd-44b5-9a89-62924fe4f696', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, securities_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Investors who purchase Class A shares with full knowledge of the dual-class structure, accepting the governance disparity in exchange for potential upside. Their consent is central to the legitimacy claim of this reading.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, beneficiary,
    moderate, biographical, mobile, global).

% Holders of Class B shares with super-voting rights, typically founders or early investors. They retain control over the company's strategic direction, justified by their initial vision and ongoing commitment. Their control is legitimate because Class A investors consented to it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_b_founders, agenda_setter,
    powerful, generational, arbitrage, global).

% Enforce disclosure requirements under securities law. Their role is to ensure that all material information, including governance structure, is transparently presented to investors. Compliance with their rules validates the consent mechanism.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, securities_regulators, beneficiary).

% Groups that argue for one-share-one-vote principles and proportional governance. From this reading's perspective, their objections are moot if investors have given informed consent.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_shareholder_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital formation with founder control by establishing a clear contractual framework where investors knowingly accept differential voting rights in exchange for equity ownership.
% TRANSFER_FUNCTION: Transfers capital from Class A investors to the company, and voting control from Class A investors to Class B founders, in exchange for equity and the expectation of future returns.
% ABSENT_VOICES: Minority shareholder advocates and proponents of 'one-share-one-vote' principles are effectively absent from the 'legitimacy by consent' conversation, as their arguments are dismissed by the premise of informed investor choice.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures based on disclosure and consent vanished, it would fundamentally alter how companies raise capital and how corporate governance is structured, likely leading to a shift towards more egalitarian voting rights or new contractual forms.
% FOUNDING_PROBLEM: The need to balance founder control and long-term vision with the demands of public capital markets, allowing companies to access funding without ceding immediate control.
% FOUNDING_PROBLEM_CORROBORATION: Securities regulators corroborate the ongoing need for clear disclosure. Founders and venture capitalists attest to the problem's live status, emphasizing the importance of control for innovation. Critics, however, argue that the problem has evolved into one of entrenchment rather than genuine long-term vision.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low because the governance disparity is considered a known and priced-in feature of the investment, not a hidden cost. Suppression is low because investors are presumed to have exit options (selling shares) if they disagree with the governance, and the primary enforcement is ensuring disclosure, not coercing investment. Theater ratio is low as the disclosure process is genuinely functional in informing investors. Accessibility collapse is moderate, as while the specific dual-class structure is fixed, alternative investment opportunities exist. Resistance is low because, from this reading's perspective, those who consent have no grounds for resistance.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (e.g., 'minority extraction') would classify this as a Snare, highlighting the power imbalance and lack of proportional governance. This reading, however, emphasizes the contractual nature and informed choice, leading to a Rope classification. The divergence arises from whether the initial consent is considered sufficient to legitimize ongoing governance disparity.
 *
 * DIRECTIONALITY LOGIC:
 *   Class A investors are beneficiaries because they gain access to potentially high-growth companies, accepting the governance terms as part of the deal. Class B founders are agenda-setters, benefiting from retained control while accessing public capital. Securities regulators are also beneficiaries, as their mandate for market transparency is fulfilled. There are no 'victims' in this reading, as all parties are presumed to have entered the arrangement with informed consent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as extraction by focusing on the ex-ante contractual agreement. If the founding problem (balancing control and capital) is still live and disclosure is effective, the constraint functions as a legitimate coordination mechanism. Mandatrophy would only occur if disclosure became meaningless or consent was demonstrably coerced, which this reading denies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_disclosure,
    'To what extent does S-1 disclosure truly lead to informed consent among all Class A investors, particularly retail investors?',
    'Empirical studies on investor comprehension of complex governance structures, and analysis of market pricing efficiency in reflecting governance disparities.',
    'If disclosure is found to be consistently ineffective, the ''informed consent'' premise weakens, shifting the constraint towards a more extractive classification (e.g., Tangled Rope or Snare) by undermining the legitimacy of the contractual choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_disclosure, empirical, 'Assesses whether the mechanism of disclosure genuinely achieves its intended effect of informed consent.').

omega_variable(
    market_efficiency_in_pricing_governance,
    'Does the market efficiently and accurately price in the governance disparity of dual-class shares, such that Class A investors are fully compensated for their lack of control?',
    'Event studies on dual-class IPOs and secondary offerings, comparing valuations of single-class vs. dual-class companies with similar fundamentals.',
    'If the market systematically underprices the control premium, or if Class A shares trade at a discount not fully explained by other factors, the ''contractual choice'' argument weakens, suggesting an uncompensated transfer of value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_in_pricing_governance, empirical, 'Examines whether the market mechanism effectively translates governance structure into fair valuation.').

omega_variable(
    scope_of_consent,
    'Does initial consent to a dual-class structure at IPO extend indefinitely to all future actions by Class B holders, or is there a point at which ''informed consent'' must be re-established or re-evaluated?',
    'Legal precedent and evolving corporate governance norms regarding ''sunset clauses'' or periodic shareholder votes on governance structures.',
    'If consent is not considered perpetual, the constraint''s legitimacy could be challenged over time, requiring new mechanisms for re-validation or leading to reclassification as a Snare if control is maintained without renewed consent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_consent, conceptual, 'Defines the temporal and functional boundaries of investor consent in dual-class structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.03).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.04).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.04).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.05).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__disclosure_consent, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__disclosure_consent, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__disclosure_consent, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__disclosure_consent, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.19).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__disclosure_consent, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on disclosure and informed consent. It contrasts with 'founder_stewardship' (legitimacy from founder vision) and 'minority_extraction' (dual-class as inherently extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
