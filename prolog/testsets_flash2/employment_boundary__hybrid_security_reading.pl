% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Hybrid Security Framework
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid security' reading of the employment
 *   boundary kernel, where platform workers are classified into a third
 *   category distinct from traditional employment and independent
 *   contracting. This reading aims to provide tailored protections (e.g.,
 *   injury insurance, some medical benefits) while preserving flexibility for
 *   platforms and workers. The metrics reflect a system that, while offering
 *   some benefits, still institutionalizes a degree of precarity and
 *   extraction compared to full employment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.45).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.6).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Hybrid Security Framework").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '4d15b1cd-9480-4a9e-8810-eb3d7b6e427e').
narrative_ontology:cs_kernel_codification('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', formalized).
narrative_ontology:cs_authority_grounding('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', lineage).
narrative_ontology:cs_interpretation_layer_present('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e').
narrative_ontology:cs_reading_relation('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', foundational, platform_work_is_sui_generis).
narrative_ontology:cs_axiom_status(platform_work_is_sui_generis, holdable).
narrative_ontology:cs_axiom_grounding('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', platform_work_is_sui_generis, conventional).
narrative_ontology:cs_axiom('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', foundational, basic_protections_without_full_employment).
narrative_ontology:cs_axiom_status(basic_protections_without_full_employment, holdable).
narrative_ontology:cs_axiom_grounding('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', basic_protections_without_full_employment, instrumental).
narrative_ontology:cs_reference_frame('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', balanced_flexibility_and_security).
narrative_ontology:cs_drift_state('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4d15b1cd-9480-4a9e-8810-eb3d7b6e427e', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, some_platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, most_platform_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate digital platforms connecting workers with tasks. They benefit from avoiding full employment obligations while gaining some regulatory certainty. They are obligated to provide basic protections like injury insurance but not full benefits or career development.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, mobile, global).

% Provide labor through platforms, receiving some basic protections (e.g., medical, injury insurance) but lacking traditional employment benefits like retirement, unemployment insurance, and career development. Their flexibility is traded for precarity.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, most_platform_workers, payer,
    powerless, immediate, constrained, local).

% Value the flexibility and autonomy of platform work, and find the basic protections offered by the hybrid model sufficient for their needs, often supplementing with other income or benefits. They are net beneficiaries of the flexibility and partial security.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, some_platform_workers, beneficiary,
    moderate, biographical, mobile, local).

% Advocate for full employment rights for platform workers, viewing the hybrid model as a compromise that institutionalizes precarity. They are largely excluded from shaping the terms of this hybrid framework, which often bypasses collective bargaining.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Design and implement the legal frameworks for platform worker classification. They seek to balance innovation, worker protection, and fiscal sustainability, often creating the 'third category' to address perceived gaps in existing law.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, social_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the provision of basic social safety net protections (e.g., injury insurance, some medical benefits) for platform workers, addressing a gap left by traditional employment law without imposing full employment obligations on platforms.
% TRANSFER_FUNCTION: Transfers a portion of platform company revenue (or worker earnings) to fund basic worker protections, while transferring the risk of full employment benefits away from platforms and onto workers or public systems.
% ABSENT_VOICES: Labor unions and advocates for full employment rights are often marginalized in the creation of hybrid models, which they view as undermining the broader struggle for worker protections. Their arguments for full employee status are not fully incorporated.
% DISAPPEARANCE_RATIONALE: If this hybrid framework vanished, platform workers would revert to either full independent contractor status (losing basic protections) or be reclassified as employees (imposing full obligations on platforms), leading to significant legal and economic restructuring of the platform economy.
% FOUNDING_PROBLEM: The rise of the platform economy created a large class of workers who did not fit traditional employment definitions, leading to a lack of basic protections and legal uncertainty for both workers and companies.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers, international labor organizations, and some platform worker advocacy groups corroborate that the problem of precarious work in the platform economy remains live, even with hybrid models. Platform companies also attest to the need for legal clarity.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).
:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while some protections are provided, the framework still allows platforms to avoid the full costs of employment, shifting significant risks to workers. Suppression (0.60) is present as the framework actively prevents platform workers from being classified as full employees, limiting their ability to demand more comprehensive benefits. The theater ratio (0.20) is low, indicating that the protections offered are genuinely functional, though incomplete.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies and some workers view this as a beneficial coordination mechanism, balancing flexibility with essential security. Most platform workers and labor unions, however, experience it as a form of extraction, where the 'hybrid' label serves to legitimize a lower standard of protection than full employment. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are beneficiaries, gaining regulatory certainty and avoiding full employment costs. Most platform workers are payers, receiving partial benefits but bearing the costs of precarity. Some platform workers, who prioritize flexibility and find the basic protections sufficient, are also beneficiaries. Social policy makers act as agenda-setters, shaping the framework. Labor unions are excluded, as their preferred outcome (full employment) is foreclosed by this hybrid approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_protections_sufficiency,
    'Are the ''tailored protections'' offered by the hybrid model genuinely sufficient to address the precarity faced by platform workers, or do they merely institutionalize a lower standard of labor rights?',
    'Longitudinal studies comparing the economic and social outcomes of platform workers under hybrid models versus full employment, focusing on income stability, health outcomes, and retirement security.',
    'If protections are found insufficient, the extractiveness of the constraint would be re-evaluated upward, potentially reclassifying it closer to a Snare. If sufficient, the coordination function would be strengthened, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_protections_sufficiency, empirical, 'Assesses the actual impact and adequacy of hybrid protections for platform workers.').

omega_variable(
    regulatory_arbitrage_potential,
    'Does the creation of a ''third category'' for platform workers create new opportunities for regulatory arbitrage, allowing companies to shift costs or responsibilities across jurisdictions or worker classifications?',
    'Comparative legal analysis across jurisdictions with different classification models, tracking company behavior and worker outcomes in response to regulatory changes.',
    'If arbitrage is significant, the suppression and extractiveness metrics would be re-evaluated upward, as the framework''s complexity would be exploited to avoid obligations. If minimal, the framework''s stability as a coordination mechanism would be affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_potential, empirical, 'Examines whether the hybrid model inadvertently creates new avenues for regulatory evasion.').

omega_variable(
    framing_of_flexibility_vs_precarity,
    'Is the emphasis on ''flexibility'' in the hybrid model a genuine benefit for workers, or a rhetorical cover for transferring risk and precarity from platforms to workers?',
    'Worker surveys and qualitative studies exploring the lived experience of platform workers, distinguishing between desired autonomy and imposed precarity, and assessing the actual value of ''flexibility'' in their economic lives.',
    'If ''flexibility'' is largely a cover, the extractiveness and suppression would be seen as higher, as the coordination narrative masks a coercive structure. If genuinely valued, the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_flexibility_vs_precarity, conceptual, 'Distinguishes between genuine worker preference for flexibility and its use as a justification for reduced protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__hybrid_security_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(empl_be_t5, employment_boundary__hybrid_security_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(empl_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(empl_su_t5, employment_boundary__hybrid_security_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(empl_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
