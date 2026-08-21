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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy: Disclosure and Informed Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'disclosure_consent' reading of
 *   the 'dual_class_legitimacy' kernel. From this perspective, the legitimacy
 *   of dual-class stock structures rests on the principle of informed
 *   consent, facilitated by comprehensive disclosure under securities law.
 *   Investors are presumed to be rational actors who, having been fully
 *   informed of the governance disparity through S-1 filings and other
 *   regulatory disclosures, choose to invest. Therefore, any perceived 'cost'
 *   or 'disadvantage' to Class A investors is considered a consensually
 *   accepted term of the investment, priced into the valuation. The structure
 *   is seen as a contractual choice, not an extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.15).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.2).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy: Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '5ca0ad0c-f8fe-4096-92a6-39110c711ed3').
narrative_ontology:cs_kernel_codification('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', formalized).
narrative_ontology:cs_authority_grounding('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', expertise).
narrative_ontology:cs_interpretation_layer_present('5ca0ad0c-f8fe-4096-92a6-39110c711ed3').
narrative_ontology:cs_reading_relation('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', dual_class_legitimacy__minority_extraction, forecloses).
narrative_ontology:cs_axiom('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', foundational, investor_rationality_and_information_efficiency).
narrative_ontology:cs_axiom_status(investor_rationality_and_information_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', investor_rationality_and_information_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', foundational, contractual_freedom_in_capital_markets).
narrative_ontology:cs_axiom_status(contractual_freedom_in_capital_markets, holdable).
narrative_ontology:cs_axiom_grounding('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', contractual_freedom_in_capital_markets, conventional).
narrative_ontology:cs_reference_frame('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', efficient_market_contractualism).
narrative_ontology:cs_drift_state('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5ca0ad0c-f8fe-4096-92a6-39110c711ed3', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, controlling_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, investment_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founders or early investors who hold high-vote shares, retaining control disproportionate to their economic stake. They benefit from the ability to raise public capital without ceding governance, enabled by the disclosure regime.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, controlling_shareholders, agenda_setter,
    institutional, generational, arbitrage, national).

% Public investors who purchase low-vote shares, accepting the governance disparity in exchange for access to the company's growth potential. From this reading's perspective, they are making an informed, consensual choice based on disclosed terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, payer,
    moderate, biographical, mobile, global).

% Government bodies responsible for enforcing securities laws, ensuring that companies provide full and accurate disclosure (e.g., S-1 filings) to protect investors. They define the parameters of 'informed consent'.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Facilitate the IPOs and secondary offerings of dual-class companies, earning fees. They benefit from the market's acceptance of dual-class structures under the disclosure framework, as it expands their deal flow.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, investment_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Groups that argue for 'one share, one vote' principles and greater governance parity. From the 'disclosure_consent' reading, their concerns are addressed by the transparency of the market, and their arguments are not seen as invalidating the contractual basis of dual-class structures.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_shareholder_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, controlling_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital formation by allowing companies to raise public funds while founders retain control, and provides investment opportunities for public investors who accept the terms.
% TRANSFER_FUNCTION: Transfers capital from public investors to the company (and indirectly, to founders) in exchange for equity, with the understanding that governance control is concentrated in high-vote shareholders.
% ABSENT_VOICES: Minority shareholder advocates who argue that disclosure alone is insufficient to ensure fair treatment, and that structural governance disparities inherently lead to extraction, regardless of consent. They are excluded from the framing that 'disclosure equals legitimacy'.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures based on disclosure and consent vanished, companies would be forced to adopt 'one share, one vote' structures to access public markets, fundamentally altering capital formation strategies and investor expectations.
% FOUNDING_PROBLEM: How to enable founders of high-growth companies to raise significant public capital without losing strategic control, while simultaneously protecting public investors from undisclosed risks.
% FOUNDING_PROBLEM_CORROBORATION: Securities lawyers and corporate finance academics often corroborate that the problem of balancing founder control and public investment remains relevant, and that disclosure is a primary mechanism for addressing it. Regulatory bodies also attest to the ongoing need for robust disclosure frameworks.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.15) and suppression (0.20) reflect this reading's core premise: if consent is truly informed, then the arrangement is a fair contractual exchange, not extraction. The 'requires_active_enforcement: true' refers to the ongoing regulatory oversight of disclosure requirements, which is seen as upholding the integrity of the consent mechanism. The low theater ratio (0.10) indicates that the disclosure process is considered genuinely functional in informing investors.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in the interpretation of 'informed consent' and 'market efficiency'. While this reading assumes investors are fully capable of assessing and pricing governance disparities, other readings (and many real-world observers) dispute the sufficiency of disclosure or the rationality of investor behavior, leading to very different classifications of the same dual-class structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Controlling shareholders are clear beneficiaries, gaining control without full equity dilution. Investment banks also benefit from facilitating these transactions. Class A investors are 'payers' in the sense that they accept the terms, but are not 'victims' from this reading's perspective, as their choice is deemed informed and consensual. Securities regulators are agenda-setters, defining the rules of disclosure. Minority shareholder advocates are 'excluded' from this framing, as their arguments against dual-class structures are considered addressed by the disclosure regime.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_disclosure,
    'Is S-1 disclosure, as currently practiced, truly sufficient to ensure ''informed consent'' from all classes of investors regarding dual-class governance structures?',
    'Empirical studies on investor comprehension of complex governance structures, behavioral economics research on decision-making under uncertainty, and analysis of post-IPO performance of dual-class firms relative to single-class firms.',
    'If disclosure is found insufficient, the ''informed consent'' premise weakens, increasing the perceived extractiveness and suppression, potentially reclassifying the constraint towards a Tangled Rope or Snare. If sufficient, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_disclosure, empirical, 'Whether current disclosure practices genuinely enable informed investor consent.').

omega_variable(
    market_efficiency_in_pricing_governance,
    'Does the market efficiently price the governance disparity inherent in dual-class structures into the valuation of Class A shares, as implied by the ''disclosure_consent'' reading?',
    'Event studies analyzing stock price reactions to changes in governance rights, cross-sectional regressions of valuation multiples on governance structures, and analysis of the long-term performance of dual-class vs. single-class companies.',
    'If the market is found to systematically underprice the governance disparity, the ''contractual choice'' premise is undermined, suggesting a hidden cost to investors and increasing extractiveness. If efficiently priced, it strengthens the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_in_pricing_governance, empirical, 'Whether market prices accurately reflect dual-class governance risks.').

omega_variable(
    alternative_readings_validity,
    'Are the alternative readings of dual-class legitimacy (founder stewardship, minority extraction) conceptually coherent and empirically supported, challenging the ''disclosure_consent'' framing?',
    'Comparative analysis of legal scholarship, economic studies, and corporate governance literature supporting each reading. This is a conceptual omega, resolved by assessing the strength of arguments and evidence for each framing.',
    'If alternative readings gain stronger conceptual or empirical support, the ''disclosure_consent'' reading''s claim to sole legitimacy is weakened, highlighting the contestability of the kernel and potentially shifting the overall classification of the dual-class structure from other perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_validity, conceptual, 'Assessing the validity of competing interpretations of dual-class legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.09).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.1).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__disclosure_consent, base_extractiveness, 5, 0.13).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__disclosure_consent, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__disclosure_consent, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__disclosure_consent, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__disclosure_consent, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
