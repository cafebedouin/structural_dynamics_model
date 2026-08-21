% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements: Graduated Access Filter Reading
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint describes statutory credential requirements as a
 *   'graduated access filter,' a reading that emphasizes how these
 *   requirements create tiered market access, disproportionately benefiting
 *   those with prior resources and disadvantaging marginalized workers. It is
 *   one reading of the 'licensing_statute_mandate' kernel, alongside
 *   'public_safety_coordination' and 'rent_seeking_suppression'. This reading
 *   focuses on the structural exclusion mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.85).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.9).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements: Graduated Access Filter Reading").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '7bae1286-fe33-409f-bb70-ff54dc5fc001').
narrative_ontology:cs_kernel_codification('7bae1286-fe33-409f-bb70-ff54dc5fc001', formalized).
narrative_ontology:cs_authority_grounding('7bae1286-fe33-409f-bb70-ff54dc5fc001', extraction).
narrative_ontology:cs_interpretation_layer_present('7bae1286-fe33-409f-bb70-ff54dc5fc001').
narrative_ontology:cs_reading_relation('7bae1286-fe33-409f-bb70-ff54dc5fc001', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('7bae1286-fe33-409f-bb70-ff54dc5fc001', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('7bae1286-fe33-409f-bb70-ff54dc5fc001', foundational, market_access_is_stratified_by_credentialing).
narrative_ontology:cs_axiom_status(market_access_is_stratified_by_credentialing, holdable).
narrative_ontology:cs_axiom_grounding('7bae1286-fe33-409f-bb70-ff54dc5fc001', market_access_is_stratified_by_credentialing, empirically_contingent).
narrative_ontology:cs_axiom('7bae1286-fe33-409f-bb70-ff54dc5fc001', foundational, prior_resource_access_determines_credential_acquisition).
narrative_ontology:cs_axiom_status(prior_resource_access_determines_credential_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('7bae1286-fe33-409f-bb70-ff54dc5fc001', prior_resource_access_determines_credential_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('7bae1286-fe33-409f-bb70-ff54dc5fc001', unfettered_labor_market).
narrative_ontology:cs_drift_state('7bae1286-fe33-409f-bb70-ff54dc5fc001', contemporary_regulatory_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7bae1286-fe33-409f-bb70-ff54dc5fc001', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced competition and increased wages due to restricted market entry. They advocate for maintaining or increasing credentialing requirements, framing them as essential for quality and public safety.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_professionals, beneficiary,
    organized, biographical, mobile, national).

% Administer and enforce the credentialing statutes, collecting fees for examinations and renewals. Their mandate is to uphold standards, but their actions also serve to maintain the scarcity that benefits credentialed members. They are often composed of incumbent professionals.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Are excluded from higher-paying, credentialed occupations due to prohibitive costs (time, money, prior education) of acquiring licenses. They are forced into lower-wage, uncredentialed work or informal economies, perpetuating economic disadvantage.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, immediate, trapped, local).

% Aspire to enter credentialed professions but face significant barriers. They invest heavily in education and training, often incurring debt, with no guarantee of licensure, making them vulnerable to changes in requirements or examination failures.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_aspirants, payer,
    moderate, biographical, constrained, regional).

% Are told that credentialing protects them from incompetent practitioners. They pay higher prices for services due to reduced competition but may not perceive the direct link between licensing and cost, or the alternative of lower-cost, regulated but uncredentialed services.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, public_consumers, observer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates market access by signaling competence, reducing information asymmetry for consumers, and standardizing professional practice.
% TRANSFER_FUNCTION: Transfers economic opportunity and higher wages to credentialed professionals by restricting labor supply, and transfers fees to licensing boards. It transfers the burden of credential acquisition (time, money, debt) to aspiring workers, particularly those with fewer prior resources.
% ABSENT_VOICES: Workers in informal economies, those who have been denied licensure, and advocates for alternative, less restrictive forms of quality assurance (e.g., performance-based assessments, tiered licensing) are largely excluded from the policy-making process.
% DISAPPEARANCE_RATIONALE: If statutory credentialing vanished overnight, the labor market for many professions would immediately open, leading to a surge in new entrants, downward pressure on wages for incumbents, and a rapid reorganization of training and quality assurance mechanisms. Consumers would face a more diverse, potentially lower-cost, but also potentially more variable service landscape.
% FOUNDING_PROBLEM: The founding problem was to protect the public from harm caused by unqualified practitioners and to ensure a minimum standard of competence in critical professions.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent professionals assert the problem is still live, citing ongoing risks. Marginalized workers and economic researchers, from outside the benefiting parties, argue that the problem is largely solved by other means (e.g., tort law, market reputation) and that the statutes now primarily serve to restrict access and extract rents.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the primary effect is to restrict labor supply, driving up wages for incumbents and creating a barrier to entry that extracts time, money, and opportunity from aspiring workers. Suppression is also high (0.90) as the legal framework actively prohibits uncredentialed practice, with severe penalties, effectively trapping marginalized workers in lower-tier jobs. Theater ratio is low (0.20) because while some public safety function remains, the primary enforcement effort is directed at maintaining market exclusivity rather than genuine competence assurance. Accessibility collapse is substantial (0.75) as legal barriers make alternative entry paths nearly impossible. Resistance is moderate (0.70) from excluded groups and advocates, but often diffuse and outmatched by organized professional bodies.
 *
 * PERSPECTIVAL GAP:
 *   The credentialed class perceives the constraint as a legitimate quality assurance mechanism (closer to a Rope or even Mountain), while marginalized workers experience it as an insurmountable barrier and a Snare. The engine's classification will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed professionals and licensing boards are clear beneficiaries (low directionality), gaining from reduced competition and administrative fees, respectively. Marginalized workers and uncredentialed aspirants are targets (high directionality), bearing the costs of exclusion or the burden of credential acquisition. Public consumers are observers, experiencing both the purported benefits of quality assurance and the hidden costs of reduced competition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (public safety) is contested. This reading suggests that while a public safety function may exist, the constraint has drifted into a mechanism for market control and rent extraction. The high extractiveness and suppression, coupled with the contested founding problem status, indicate a potential mandatrophy where the original coordination function has been overshadowed by extractive dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_vs_market_access,
    'What is the true marginal contribution of statutory credentialing to public safety, compared to alternative, less restrictive quality assurance mechanisms?',
    'Empirical studies comparing public harm rates in jurisdictions with varying credentialing stringency or alternative regulatory models (e.g., performance-based licensing, tiered certification).',
    'If public safety benefits are marginal or achievable by less restrictive means, the ''public_safety_coordination'' reading is weakened, strengthening the ''graduated_access_filter'' and ''rent_seeking_suppression'' readings. This would shift the constraint''s effective type towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_market_access, empirical, 'Quantifying the public safety benefit of credentialing versus its market access costs.').

omega_variable(
    internalized_suppression_of_aspirants,
    'To what extent is the suppression experienced by uncredentialed aspirants internalized (e.g., belief in the legitimacy of the system, self-blame for lack of credentials) versus purely structural (legal barriers, financial costs)?',
    'Sociological studies and qualitative interviews with aspiring workers, particularly those who have failed to obtain credentials, to assess their perceptions of the system''s fairness and their own agency.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as individuals carry the barrier within them even if external conditions slightly ease. This would amplify the Snare-like qualities of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_aspirants, empirical, 'Structural vs. internalized suppression mechanism for aspiring workers.').

omega_variable(
    framing_of_professional_autonomy,
    'Is the concept of ''professional autonomy'' (often invoked by credentialed groups) a genuine coordination mechanism for complex work, or a conceptual cover for rent-seeking and market control?',
    'Conceptual analysis of professional ethics literature, historical studies of professionalization, and comparative analysis of ''autonomous'' professions versus those with external oversight.',
    'If ''professional autonomy'' is primarily a cover, the ''public_safety_coordination'' reading is conceptually undermined, reinforcing the ''graduated_access_filter'' as a more accurate description of the constraint''s function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_professional_autonomy, conceptual, 'Conceptual ambiguity of ''professional autonomy'' in credentialing debates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.12).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.15).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.17).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.19).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'graduated_access_filter' reading of the 'licensing_statute_mandate' kernel. It focuses on the differential market access created by credentialing requirements, contrasting with 'public_safety_coordination' (emphasizing consumer protection) and 'rent_seeking_suppression' (emphasizing incumbent benefits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
