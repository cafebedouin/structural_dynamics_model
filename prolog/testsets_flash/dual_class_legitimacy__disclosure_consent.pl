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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Stock Legitimacy via Disclosure and Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'disclosure and consent' reading
 *   of dual-class stock legitimacy. Under this reading, the legitimacy of
 *   dual-class share structures rests on the principle that investors, having
 *   been fully informed of the governance disparity through mandated
 *   disclosures (e.g., S-1 filings), freely choose to invest. Therefore, the
 *   structure is seen as a contractual choice, not an inherent extraction or
 *   coordination failure. The governance disparity is presumed to be priced
 *   into the valuation of the Class A shares, making the arrangement fair by
 *   market mechanisms. This reading emphasizes investor autonomy and the
 *   efficiency of capital markets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.25).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.3).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.25).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Stock Legitimacy via Disclosure and Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'd33cb721-d3da-472d-a855-ae23f34e45fd').
narrative_ontology:cs_kernel_codification('d33cb721-d3da-472d-a855-ae23f34e45fd', formalized).
narrative_ontology:cs_authority_grounding('d33cb721-d3da-472d-a855-ae23f34e45fd', lineage).
narrative_ontology:cs_interpretation_layer_present('d33cb721-d3da-472d-a855-ae23f34e45fd').
narrative_ontology:cs_reading_relation('d33cb721-d3da-472d-a855-ae23f34e45fd', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('d33cb721-d3da-472d-a855-ae23f34e45fd', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('d33cb721-d3da-472d-a855-ae23f34e45fd', foundational, investor_autonomy_paramount).
narrative_ontology:cs_axiom_status(investor_autonomy_paramount, holdable).
narrative_ontology:cs_axiom_grounding('d33cb721-d3da-472d-a855-ae23f34e45fd', investor_autonomy_paramount, deontological).
narrative_ontology:cs_axiom('d33cb721-d3da-472d-a855-ae23f34e45fd', foundational, full_disclosure_enables_efficient_markets).
narrative_ontology:cs_axiom_status(full_disclosure_enables_efficient_markets, holdable).
narrative_ontology:cs_axiom_grounding('d33cb721-d3da-472d-a855-ae23f34e45fd', full_disclosure_enables_efficient_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('d33cb721-d3da-472d-a855-ae23f34e45fd', securities_act_disclosure_regime).
narrative_ontology:cs_drift_state('d33cb721-d3da-472d-a855-ae23f34e45fd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d33cb721-d3da-472d-a855-ae23f34e45fd', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, efficient_market_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares (Class B) that grant disproportionate control over the company, allowing them to pursue long-term visions without immediate market pressure. They benefit from the stability and strategic autonomy this structure provides, which they argue is disclosed and consented to by other investors.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Purchase non-voting or subordinate-voting shares (Class A) in publicly traded dual-class companies. They are presumed to have read and understood the S-1 disclosure, accepting the governance structure in exchange for potential financial returns. Their consent to the structure is the basis of its legitimacy.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, beneficiary,
    powerful, biographical, mobile, global).

% Enforce disclosure requirements under the Securities Act, ensuring that investors are fully informed about the governance structure of dual-class companies. Their role is to ensure transparency, not to dictate corporate governance models, thus validating the consent-based legitimacy.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that dual-class structures inherently disenfranchise minority shareholders, regardless of disclosure. They are excluded from the 'consent' framework of this reading, as their objections are based on principles of proportional governance rather than informed contractual choice.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, corporate_governance_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital formation with founder control, allowing companies to access public markets while retaining strategic autonomy, provided all terms are transparently disclosed.
% TRANSFER_FUNCTION: Transfers capital from Class A investors to the company, in exchange for equity that carries financial rights but limited governance rights, with the understanding that control remains with founding shareholders.
% ABSENT_VOICES: Corporate governance advocates and some institutional investors, who believe that even with full disclosure, the lack of proportional voting rights constitutes an unfair arrangement. They would argue for 'one share, one vote' as a fundamental principle, which this reading dismisses as a matter of contractual freedom.
% DISAPPEARANCE_RATIONALE: If the legitimacy of dual-class structures based on disclosure and consent vanished, many publicly traded companies would face immediate governance crises, requiring restructuring or delisting. Capital markets would need to re-evaluate how founder control is valued and permitted, leading to significant re-organization of corporate finance and governance norms.
% FOUNDING_PROBLEM: Companies seeking public capital often faced pressure from short-term-focused investors, hindering long-term strategic execution and founder vision. Dual-class structures were adopted to solve this by separating economic ownership from voting control.
% FOUNDING_PROBLEM_CORROBORATION: Founding shareholders and many venture capitalists attest that the problem of short-term market pressure remains live, citing examples of companies that lost their strategic direction after losing founder control. Securities regulators corroborate the need for mechanisms that balance capital access with founder autonomy, provided disclosure is robust. Corporate governance advocates contest the 'problem' itself, viewing it as a justification for entrenchment rather than a genuine market failure.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).

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
 *   Extractiveness is low (0.25) because, from this perspective, any 'cost' to Class A investors is a consented-to term of the investment, priced into the shares. It's not 'extraction' if you knowingly agree to it. Suppression is also low (0.30) because investors are free to choose other investments; the 'suppression' is merely the consequence of a contractual agreement. Theater ratio is low (0.10) as the disclosure process is genuinely functional in informing investors. The claimed type is 'rope' because it's viewed as a coordination mechanism for capital formation, with minimal coercive overhead, where participants (Class A investors) are net beneficiaries of the investment opportunity, and alternatives are not suppressed (other investment options exist).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of founding shareholders and securities regulators, this is a legitimate, transparent, and efficient mechanism. From the perspective of corporate governance advocates, the 'consent' is often theoretical, given information asymmetries and market pressures, and the structure still enables extraction. The engine's classification will reflect the structural data, which this reading interprets as a fair exchange.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are clear beneficiaries (d=0.0) as they retain control. Class A investors are also considered beneficiaries (d=0.15) because they willingly participate in the investment, accepting the terms for financial gain. Securities regulators are agenda-setters (d=0.05) ensuring the transparency that underpins this legitimacy. Corporate governance advocates are 'excluded' (d=0.9) as their fundamental objections to the structure are outside the 'consent' framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a consensual contractual arrangement as extraction. If the disclosure mechanism were to become purely theatrical, or if investors were demonstrably not informed, then the constraint would shift from a Rope to a Snare, as the 'consent' basis for legitimacy would collapse. However, as long as disclosure is robust and investors have exit options, this reading maintains it as a legitimate coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_validity,
    'To what extent is investor ''informed consent'' truly robust, given potential information asymmetries, behavioral biases, and market pressures?',
    'Empirical studies on investor comprehension of complex S-1 disclosures, analysis of market pricing efficiency for dual-class shares, and surveys of investor sentiment regarding governance rights.',
    'If consent is found to be systematically flawed, the ''disclosure_consent'' reading''s legitimacy would weaken, pushing the constraint towards a ''tangled_rope'' or ''snare'' classification, as the ''contractual choice'' argument would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_validity, empirical, 'Assesses the real-world efficacy of informed consent in validating dual-class structures.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''rope'' based on informed consent, or is it a ''snare'' whose extractive nature is obscured by the ''disclosure_consent'' framing?',
    'Comparison with the ''minority_extraction'' sibling reading: if empirical evidence strongly supports systematic underpricing of Class A shares or persistent governance abuses despite disclosure, it would favor the ''minority_extraction'' reading.',
    'If the ''minority_extraction'' reading gains stronger empirical support, this constraint would be reclassified from ''rope'' to ''snare'', reflecting a shift in the dominant interpretation of dual-class legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishes between the ''disclosure_consent'' and ''minority_extraction'' readings of dual-class legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t1980, dual_class_legitimacy__disclosure_consent, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(dual_tr_t1990, dual_class_legitimacy__disclosure_consent, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(dual_tr_t2000, dual_class_legitimacy__disclosure_consent, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(dual_tr_t2010, dual_class_legitimacy__disclosure_consent, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(dual_tr_t2024, dual_class_legitimacy__disclosure_consent, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t1980, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(dual_be_t1990, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(dual_be_t2000, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(dual_be_t2010, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(dual_be_t2024, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t1980, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(dual_su_t1990, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1990, 0.27).
narrative_ontology:measurement(dual_su_t2000, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(dual_su_t2010, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(dual_su_t2024, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on disclosure and investor consent. It is structurally distinct from the 'founder_stewardship' and 'minority_extraction' readings, which emphasize different legitimating principles and structural outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
