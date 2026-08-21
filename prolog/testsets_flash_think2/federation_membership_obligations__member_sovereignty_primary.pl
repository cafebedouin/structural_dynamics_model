% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: National Welfare State Closure Authority (Member Sovereignty Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the 'member sovereignty primary' reading of
 *   federation membership obligations, where national welfare states retain
 *   significant closure authority. Free movement is conditional on protecting
 *   national labor markets and ensuring the sustainability of welfare
 *   systems. This reading emphasizes the right of member states to impose
 *   conditions on mobile workers' access to social benefits and labor
 *   markets, often through national legislation and administrative rules. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual function of
 *   coordinating national welfare state integrity while extracting from
 *   mobile workers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "National Welfare State Closure Authority (Member Sovereignty Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '7c44f507-a308-465c-855b-447866d9ad8e').
narrative_ontology:cs_kernel_codification('7c44f507-a308-465c-855b-447866d9ad8e', formalized).
narrative_ontology:cs_authority_grounding('7c44f507-a308-465c-855b-447866d9ad8e', lineage).
narrative_ontology:cs_interpretation_layer_present('7c44f507-a308-465c-855b-447866d9ad8e').
narrative_ontology:cs_reading_relation('7c44f507-a308-465c-855b-447866d9ad8e', federation_membership_obligations__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('7c44f507-a308-465c-855b-447866d9ad8e', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('7c44f507-a308-465c-855b-447866d9ad8e', foundational, national_sovereignty_over_welfare).
narrative_ontology:cs_axiom_status(national_sovereignty_over_welfare, holdable).
narrative_ontology:cs_axiom_grounding('7c44f507-a308-465c-855b-447866d9ad8e', national_sovereignty_over_welfare, conventional).
narrative_ontology:cs_axiom('7c44f507-a308-465c-855b-447866d9ad8e', foundational, labor_market_protection_priority).
narrative_ontology:cs_axiom_status(labor_market_protection_priority, holdable).
narrative_ontology:cs_axiom_grounding('7c44f507-a308-465c-855b-447866d9ad8e', labor_market_protection_priority, instrumental).
narrative_ontology:cs_reference_frame('7c44f507-a308-465c-855b-447866d9ad8e', westphalian_welfare_state_model).
narrative_ontology:cs_drift_state('7c44f507-a308-465c-855b-447866d9ad8e', contemporary_federal_integration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c44f507-a308-465c-855b-447866d9ad8e', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_recipients).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, pro_integration_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the authority to set conditions for welfare access and labor market participation for non-nationals, prioritizing national interests and the sustainability of their welfare systems. They enforce these conditions through legislation and administrative measures.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from policies that protect their wages and employment conditions from potential downward pressure due to unrestricted labor mobility. They support national governments in maintaining closure authority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Benefit from the perceived sustainability of national welfare systems, which this reading argues is protected by limiting access for mobile workers. They are often a key constituency for policies that prioritize national closure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_recipients, beneficiary,
    moderate, biographical, mobile, national).

% Bear the costs of conditional free movement, facing restrictions on welfare access, longer waiting periods, or specific labor market requirements that limit their full integration and benefit from the welfare state of the receiving country. Their options are to comply, seek work elsewhere, or return to their home country.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, continental).

% Advocate for broader free movement rights and less conditional welfare access, viewing national closure as undermining the principles of federal integration. They face an uphill battle against national sovereignty claims and public opinion in many member states.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, pro_integration_advocates, payer,
    organized, generational, constrained, continental).

% While formally tasked with upholding free movement, their authority to override national welfare state closure is limited by this reading. They are often in a position of negotiating with member states rather than unilaterally enforcing full integration, effectively excluded from primary decision-making on these specific conditions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_institutions, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, member_states).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to coordinate the protection of their national labor markets and the financial sustainability of their welfare systems, ensuring that the benefits of free movement do not disproportionately burden individual states.
% TRANSFER_FUNCTION: Transfers the burden of welfare system sustainability and labor market protection from national taxpayers and existing labor forces to mobile workers, who face conditional access and reduced benefits.
% ABSENT_VOICES: Migrant advocacy groups and some international legal scholars, who would argue for universal human rights and non-discriminatory access to social protection regardless of nationality or contribution history, are often marginalized in national policy debates.
% DISAPPEARANCE_RATIONALE: If national welfare states lost their closure authority overnight, there would be immediate and significant shifts in migration patterns, welfare expenditure, and labor market dynamics across the federation. Member states would face immense pressure to either harmonize welfare systems or risk 'welfare tourism,' leading to a fundamental reorganization of federal relations and national social contracts.
% FOUNDING_PROBLEM: The challenge of reconciling national welfare state models and labor market protections with the principle of free movement within a federal or quasi-federal structure, particularly concerning the fiscal and social sustainability of national systems.
% FOUNDING_PROBLEM_CORROBORATION: National governments, conservative political parties, and some economists consistently attest to the ongoing challenges of welfare state sustainability and labor market protection in the face of free movement. Public opinion polls in many member states also corroborate these concerns, indicating a live problem from the perspective of national populations.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because mobile workers are required to contribute to national economies without immediate or full access to the welfare benefits their contributions support, effectively subsidizing national systems. Suppression is high (0.75) due to the active enforcement of national laws and administrative hurdles that limit mobile workers' rights and options. Theater ratio is low (0.15) as the stated purpose of protecting welfare sustainability and labor markets is genuinely pursued through these measures, even if the outcome is extractive. Accessibility collapse is moderate-high (0.65) as alternatives for mobile workers to access full welfare benefits are significantly curtailed. Resistance is moderate (0.55) from pro-integration groups and mobile workers, but often insufficient to overcome national political will.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member states, this constraint is a necessary coordination mechanism to preserve national sovereignty and welfare state integrity. From the perspective of mobile workers and pro-integration advocates, it is an extractive barrier that undermines the spirit of free movement and creates a tiered system of citizenship. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national labor forces, and national welfare recipients are beneficiaries, as the constraint protects their interests and resources. Mobile workers and pro-integration advocates are payers/targets, bearing the costs of conditional access and facing structural barriers. EU institutions are excluded from directly overriding this national authority, reflecting their limited power in this specific reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_federation_membership_obligations,
    'Is this constraint accurately identified as the ''member_sovereignty_primary'' reading of the ''federation_membership_obligations'' kernel?',
    'Analysis of legal texts, policy documents, and political discourse to confirm the consistent articulation of national closure authority as primary.',
    'If misidentified, the entire analysis of this constraint''s relationship to its kernel and siblings would be invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_federation_membership_obligations, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    impact_of_integration_primary_reading,
    'How would the structural properties of this constraint change if the ''integration_primary'' reading of the kernel were adopted?',
    'Counterfactual analysis: if free movement were constitutive and welfare boundaries yielded, extractiveness from mobile workers would decrease, and suppression of their rights would diminish significantly.',
    'The constraint would likely shift towards a Rope or even a Mountain (for free movement itself), with mobile workers becoming beneficiaries rather than payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_integration_primary_reading, conceptual, 'Examines the structural delta if the ''integration_primary'' reading prevailed.').

omega_variable(
    impact_of_selective_solidarity_reading,
    'How would the structural properties of this constraint change if the ''selective_solidarity'' reading of the kernel were adopted?',
    'Counterfactual analysis: if rights were tiered by contribution, the basis of extraction would shift from national origin to economic activity/contribution history, potentially creating new victim groups among low-contribution mobile workers.',
    'The constraint would likely remain a Tangled Rope or Snare, but the specific beneficiaries and victims, and the mechanisms of extraction, would be reconfigured based on contribution rather than national closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_selective_solidarity_reading, conceptual, 'Examines the structural delta if the ''selective_solidarity'' reading prevailed.').

omega_variable(
    sustainability_vs_protectionism_ambiguity,
    'To what extent are claims of ''welfare system sustainability'' and ''labor market protection'' genuine concerns, versus rhetorical cover for national protectionism?',
    'Empirical economic studies comparing the actual fiscal impact of mobile workers on welfare systems with the stated policy rationales, and analysis of labor market outcomes with and without restrictions.',
    'If primarily protectionist, the extractiveness and suppression metrics would be more firmly attributed to rent-seeking rather than legitimate coordination costs, strengthening the Snare-like aspects of the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_vs_protectionism_ambiguity, empirical, 'Distinguishes genuine policy concerns from protectionist motives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t1995, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fede_tr_t2005, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(fede_tr_t2025, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(fede_be_t1995, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(fede_be_t2005, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(fede_be_t2015, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(fede_be_t2025, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(fede_su_t1995, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement(fede_su_t2005, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(fede_su_t2015, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(fede_su_t2025, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_obligations' kernel, alongside 'integration_primary' and 'selective_solidarity'. Each reading instantiates a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
