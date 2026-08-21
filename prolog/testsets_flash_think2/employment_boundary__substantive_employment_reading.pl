% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'substantive employment reading' of the
 *   broader 'employment_boundary' kernel. It posits that employment status
 *   should be determined by the economic reality of dependence and
 *   algorithmic control, rather than formal contractual terms. If adopted,
 *   this reading would reclassify many platform workers as employees,
 *   obligating platform companies to provide full labor protections and
 *   social benefits. The claimed type is 'tangled_rope' because it aims to
 *   coordinate labor relations for worker security while simultaneously
 *   extracting costs from platform companies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.75).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '17e53075-0910-4dc9-8685-038bb16e9523').
narrative_ontology:cs_kernel_codification('17e53075-0910-4dc9-8685-038bb16e9523', formalized).
narrative_ontology:cs_authority_grounding('17e53075-0910-4dc9-8685-038bb16e9523', lineage).
narrative_ontology:cs_interpretation_layer_present('17e53075-0910-4dc9-8685-038bb16e9523').
narrative_ontology:cs_reading_relation('17e53075-0910-4dc9-8685-038bb16e9523', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('17e53075-0910-4dc9-8685-038bb16e9523', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('17e53075-0910-4dc9-8685-038bb16e9523', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('17e53075-0910-4dc9-8685-038bb16e9523', economic_dependence_defines_employment, empirically_contingent).
narrative_ontology:cs_axiom('17e53075-0910-4dc9-8685-038bb16e9523', foundational, algorithmic_control_is_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_is_supervision, holdable).
narrative_ontology:cs_axiom_grounding('17e53075-0910-4dc9-8685-038bb16e9523', algorithmic_control_is_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('17e53075-0910-4dc9-8685-038bb16e9523', universal_labor_protections).
narrative_ontology:cs_drift_state('17e53075-0910-4dc9-8685-038bb16e9523', contemporary_platform_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17e53075-0910-4dc9-8685-038bb16e9523', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_welfare_systems).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently experience precarity and lack of benefits under independent contractor status. This reading, if adopted, would grant them employee rights, social insurance, and collective bargaining power, significantly improving their working conditions and security.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    organized, biographical, constrained, global).

% Currently benefit from classifying workers as independent contractors, avoiding costs associated with employment (social security, benefits, minimum wage). This reading would obligate them to provide these benefits, increasing their operational costs and potentially requiring business model adjustments. Their market position constrains exit.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, payer,
    institutional, biographical, constrained, global).

% Actively champion this substantive definition of employment, seeking to expand labor protections to platform workers. They would benefit from increased membership and influence if workers are reclassified as employees.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions_advocates, agenda_setter,
    organized, generational, analytical, national).

% Are responsible for defining and enforcing labor laws. This reading would require them to update regulations and actively enforce the reclassification, potentially leading to increased administrative burden but also ensuring broader social welfare coverage and tax revenue.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, governments_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Currently bear the social costs of platform worker precarity (e.g., unemployment benefits, healthcare gaps). This reading would shift these costs back to platform companies, stabilizing social safety nets and reducing public expenditure on worker support.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_welfare_systems, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(employment_boundary__substantive_employment_reading, social_welfare_systems).

% Adhere to a strict interpretation of employment based on formal contract terms and direct supervision. Their arguments are structurally excluded by the substantive premise of this reading, which prioritizes economic reality over legal form.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, formalist_legal_scholars, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize labor protections and social insurance for workers in the platform economy, ensuring a baseline of security and rights across diverse work arrangements, and to ensure fair competition among businesses by preventing the externalization of labor costs.
% TRANSFER_FUNCTION: Transfers social insurance contributions, job security obligations, and collective bargaining rights from platform companies to platform workers. It also transfers the burden of social welfare from public systems back to the employers responsible for the labor. Potentially transfers increased costs to consumers through higher prices for platform services.
% ABSENT_VOICES: Formalist legal scholars and platform lobbyists who argue for strict contractual definitions of independent contracting are structurally excluded from the premise of this reading. Some highly autonomous platform workers who genuinely prefer the flexibility of independent contractor status might also object, fearing loss of autonomy.
% DISAPPEARANCE_RATIONALE: If this substantive employment definition vanished, platform companies would continue to classify workers as independent contractors, avoiding social security contributions and labor protections. This would lead to increased precarity for a growing segment of the workforce, shifting social costs onto public welfare systems and exacerbating income inequality. The mobile labor market would reorganize around a lower-cost, lower-protection model.
% FOUNDING_PROBLEM: The rise of the platform economy created a large class of workers performing employee-like tasks (subject to algorithmic control, economically dependent) without employee protections, leading to precarity, lack of benefits, and social dumping, challenging existing labor law frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists, social policy researchers, and international labor organizations widely corroborate the existence and persistence of worker precarity in the platform economy, citing numerous studies and reports from outside the direct beneficiaries of reclassification.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.65) is moderate because while it extracts significant costs from platforms, it also provides substantial benefits to workers, balancing the overall extraction. Suppression (0.75) is high due to the active resistance from platform companies and the need for robust legal and regulatory enforcement to overcome existing contractual norms. Theater ratio (0.40) reflects the ongoing legal and political maneuvering by platforms to resist reclassification, often through lobbying and strategic litigation, which diverts resources from genuine compliance. Accessibility collapse (0.80) is high because this reading, if enforced, would largely collapse the alternative of classifying economically dependent platform workers as independent contractors. Resistance (0.70) is high, reflecting the strong opposition from platform companies and their allies.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies would experience this constraint as a snare or tangled rope, imposing significant new costs and restricting their business model flexibility. Platform workers, conversely, would experience it as a rope or scaffold, providing essential protections and a pathway to greater security. Governments and labor advocates would see it as a necessary regulatory intervention to correct market failures and ensure social equity.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers and social welfare systems are the primary beneficiaries, gaining security, benefits, and reduced social costs. Platform companies are the primary victims, bearing increased labor costs and regulatory burdens. Labor unions and governments act as agenda-setters, driving the adoption and enforcement of this definition. Formalist legal scholars are excluded, as their foundational premises are directly challenged by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a tangled_rope prevents mislabeling the redefinition as pure extraction (snare) from the platforms' perspective, acknowledging the genuine coordination function of standardizing labor relations and providing social security. Conversely, it prevents mislabeling it as a pure rope, recognizing the significant costs imposed on platforms and the active enforcement required against their resistance. The 'live' status of the founding problem (worker precarity) indicates that the mandate is still highly relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''substantive_employment_reading'' of the ''employment_boundary'' kernel?',
    'Comparison with legal scholarship and policy proposals advocating for this specific definition, ensuring fidelity to its core tenets.',
    'If misaligned, the analysis of the kernel contest would be flawed, potentially misrepresenting the structural relationships between sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies the fidelity of this story to its declared kernel reading.').

omega_variable(
    impact_of_formalist_reading,
    'If the ''formalist_employment_reading'' were to prevail, how would the structural relationships and classifications change?',
    'Simulating a scenario where formal contract terms are strictly upheld, and analyzing the resulting beneficiary/victim sets and extraction levels.',
    'A prevailing formalist reading would solidify platform workers as independent contractors, shifting them from beneficiaries to victims of precarity, and platform companies from victims to beneficiaries of cost avoidance. This constraint would be foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_formalist_reading, conceptual, 'Examines the counterfactual impact of a sibling reading.').

omega_variable(
    impact_of_hybrid_reading,
    'If the ''hybrid_security_reading'' were to prevail, how would the structural relationships and classifications change?',
    'Analyzing policy proposals for a ''third category'' of worker, assessing the specific protections offered and the costs imposed on platforms, and comparing to full employment status.',
    'A hybrid reading would create a new, intermediate category, potentially reducing extraction from platforms compared to full employment, but also offering fewer protections to workers than this substantive reading. This constraint would be influenced but not foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_hybrid_reading, conceptual, 'Examines the counterfactual impact of a sibling reading.').

omega_variable(
    platform_business_model_viability,
    'Can platform companies sustain their current business models if forced to reclassify workers as employees, or would it lead to widespread market disruption?',
    'Economic modeling and empirical studies of jurisdictions that have implemented similar reclassifications, analyzing impacts on profitability, pricing, and service availability.',
    'If business models are unsustainable, the political and economic resistance to this constraint would intensify, potentially leading to legislative rollback or significant job losses. If sustainable, it strengthens the case for reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_business_model_viability, empirical, 'Assesses the economic feasibility of reclassification for platform companies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__substantive_employment_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(empl_tr_t18, employment_boundary__substantive_employment_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__substantive_employment_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(empl_tr_t30, employment_boundary__substantive_employment_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(empl_be_t6, employment_boundary__substantive_employment_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(empl_be_t18, employment_boundary__substantive_employment_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(empl_be_t24, employment_boundary__substantive_employment_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(empl_be_t30, employment_boundary__substantive_employment_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(empl_su_t6, employment_boundary__substantive_employment_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(empl_su_t18, employment_boundary__substantive_employment_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(empl_su_t24, employment_boundary__substantive_employment_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(empl_su_t30, employment_boundary__substantive_employment_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, social_welfare_eligibility).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, platform_business_models).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'employment_boundary' kernel, each representing a distinct structural claim about the nature of employment in the platform economy. They are linked to show their contested relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
