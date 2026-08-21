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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Dual-Class Stock Legitimacy via Disclosure and Investor Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'disclosure_consent' reading of
 *   the 'dual_class_legitimacy' kernel. Under this reading, the legitimacy of
 *   dual-class stock structures, where founders retain disproportionate
 *   voting control, rests entirely on the principle of informed investor
 *   consent. As long as the governance structure is fully disclosed in
 *   accordance with Securities Act requirements, and investors choose to
 *   purchase shares, the arrangement is considered a valid contractual
 *   choice, not a mechanism for extraction. The structure is viewed as a
 *   coordination mechanism for capital formation, with minimal inherent
 *   extraction beyond standard transaction costs.
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
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Stock Legitimacy via Disclosure and Investor Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb').
narrative_ontology:cs_kernel_codification('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', formalized).
narrative_ontology:cs_authority_grounding('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', lineage).
narrative_ontology:cs_interpretation_layer_present('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb').
narrative_ontology:cs_reading_relation('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', dual_class_legitimacy__minority_extraction, forecloses).
narrative_ontology:cs_axiom('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', foundational, investor_informed_consent_is_legitimacy).
narrative_ontology:cs_axiom_status(investor_informed_consent_is_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', investor_informed_consent_is_legitimacy, conventional).
narrative_ontology:cs_axiom('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', secondary, governance_disparity_is_priced).
narrative_ontology:cs_axiom_status(governance_disparity_is_priced, holdable).
narrative_ontology:cs_axiom_grounding('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', governance_disparity_is_priced, empirically_contingent).
narrative_ontology:cs_reference_frame('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', securities_act_disclosure_framework).
narrative_ontology:cs_drift_state('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cbf829e7-7a1a-49e3-a6e3-2b69191eb7bb', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_b_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, securities_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, investment_banks).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, efficient_market_hypothesis).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founders and insiders who hold super-voting shares, allowing them to retain control disproportionate to their economic ownership. They benefit from the ability to pursue long-term strategies without short-term market pressure, assuming investors have consented to this structure via disclosure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_b_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Public shareholders who purchase non-voting or subordinate-voting shares. Under this reading, they are assumed to have made an informed choice, with the governance disparity priced into the share valuation. Their 'payment' is the acceptance of reduced control for economic participation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, payer,
    powerful, biographical, mobile, global).

% Government bodies responsible for enforcing securities laws, particularly disclosure requirements. Their role is to ensure that investors receive all material information, thereby validating the consent-based legitimacy of dual-class structures.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Facilitate the initial public offerings (IPOs) of dual-class companies, collecting significant fees. They benefit from the market's acceptance of these structures, which expands their deal flow.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, investment_banks, beneficiary,
    institutional, immediate, arbitrage, global).

% Academics and legal experts who analyze corporate governance structures. From this reading's perspective, they observe the legal framework and its application, often debating the sufficiency of disclosure for true investor consent.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, class_b_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital formation for companies seeking public investment while allowing founders to retain strategic control, by standardizing disclosure requirements for investors.
% TRANSFER_FUNCTION: Transfers investment capital from public shareholders to companies, in exchange for equity, with the understanding that governance control remains concentrated with Class B shareholders.
% ABSENT_VOICES: Minority shareholder advocates and some corporate governance experts, who argue that disclosure alone is insufficient to legitimize control disparities, are present in public discourse but their arguments are not structurally accommodated by this consent-based framework.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures based on disclosure and investor consent vanished, companies would face significant challenges in raising public capital under such structures. This would force a fundamental reorganization of capital markets, potentially leading to a shift towards single-class structures or alternative private financing models.
% FOUNDING_PROBLEM: How to enable high-growth companies, often founder-led, to access public capital markets without forcing them to relinquish strategic control and long-term vision to short-term market pressures.
% FOUNDING_PROBLEM_CORROBORATION: Securities regulators corroborate that robust disclosure is a cornerstone of market integrity. Founders and investment banks attest to the ongoing need for control retention to foster innovation and long-term value creation, supporting the continued relevance of the founding problem.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects the view that any 'cost' to Class A investors is a known and accepted part of the investment, priced into the valuation. Suppression (0.20) is low because investors are assumed to have exit options (selling shares) and alternatives (investing in other companies). Theater ratio (0.10) is low as the disclosure process is considered genuinely functional in informing investors. The claimed type is 'rope' because it's seen as a coordination mechanism that benefits all parties by facilitating capital markets, with participants being net beneficiaries due to informed choice.
 *
 * PERSPECTIVAL GAP:
 *   This reading explicitly foregrounds investor consent and disclosure as the sole basis for legitimacy, thereby downplaying or rejecting alternative perspectives that emphasize inherent power imbalances or the need for proportional governance. The engine will compute different classifications for other readings of the 'dual_class_legitimacy' kernel, which will highlight this perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Class B shareholders (founders/insiders) are primary beneficiaries as they retain control while accessing public capital. Securities regulators also benefit as their framework for market integrity (disclosure) is validated. Class A investors are 'payers' in the sense that they accept reduced control, but are considered beneficiaries of the overall capital market function. No victims are declared under this reading, as consent is assumed to negate victimhood.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investor_rationality_and_consent,
    'Does ''informed consent'' under Securities Act disclosure truly reflect rational investor choice, or is it undermined by behavioral biases, information asymmetry, or market power dynamics?',
    'Empirical studies on investor behavior in dual-class IPOs, analysis of long-term returns of dual-class vs. single-class firms, and regulatory reviews of disclosure effectiveness.',
    'If consent is found to be systematically impaired, the constraint''s effective extractiveness and suppression would be higher, potentially reclassifying it towards a ''tangled_rope'' or ''snare'' from the investor''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_rationality_and_consent, empirical, 'The empirical validity of investor informed consent.').

omega_variable(
    disclosure_sufficiency_for_legitimacy,
    'Is disclosure alone sufficient to legitimize a fundamental disparity in corporate governance control, or does corporate legitimacy inherently require some degree of control proportionality to capital contribution?',
    'Conceptual analysis within corporate law and governance theory, comparative legal studies of jurisdictions with different approaches to dual-class structures, and policy debates on shareholder rights.',
    'If control proportionality is deemed an inherent requirement for legitimacy, this reading''s claim of ''rope'' would be conceptually challenged, potentially shifting its classification towards ''tangled_rope'' even with full disclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disclosure_sufficiency_for_legitimacy, conceptual, 'The conceptual sufficiency of disclosure for governance legitimacy.').

omega_variable(
    reading_identity_and_structural_delta,
    'This constraint is the ''disclosure_consent'' reading of the ''dual_class_legitimacy'' kernel. How would its structural properties (extractiveness, suppression, claimed_type) change if interpreted through the ''founder_stewardship'' or ''minority_extraction'' sibling readings?',
    'Comparison with separate constraint stories generated for the ''founder_stewardship'' and ''minority_extraction'' readings, focusing on their declared base properties and stakeholder analyses.',
    'The ''founder_stewardship'' reading would likely maintain a ''rope'' or ''scaffold'' classification with low extraction, while the ''minority_extraction'' reading would likely classify as a ''snare'' or ''tangled_rope'' with high extraction and suppression, demonstrating the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_and_structural_delta, conceptual, 'Structural differences across readings of the dual-class legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.1).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.1).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.1).

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

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, which also includes 'founder_stewardship' and 'minority_extraction' readings. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
