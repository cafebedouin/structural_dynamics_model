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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory credential requirements, while ostensibly designed to ensure
 *   public safety and professional competence, are interpreted in this
 *   reading as a 'graduated access filter.' This constraint creates tiered
 *   market access where differential barriers (e.g., cost of education, time
 *   commitment, specific program requirements) disproportionately sort
 *   individuals by class and prior resource access. The system effectively
 *   excludes marginalized workers and uncredentialed aspirants from
 *   higher-paying professions, channeling economic rents to credentialed
 *   incumbents and the educational institutions that provide credentialing
 *   pathways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.9).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.9).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.9).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'd98f03d9-91a7-4916-ae15-3b1065b64e4b').
narrative_ontology:cs_kernel_codification('d98f03d9-91a7-4916-ae15-3b1065b64e4b', formalized).
narrative_ontology:cs_authority_grounding('d98f03d9-91a7-4916-ae15-3b1065b64e4b', extraction).
narrative_ontology:cs_interpretation_layer_present('d98f03d9-91a7-4916-ae15-3b1065b64e4b').
narrative_ontology:cs_reading_relation('d98f03d9-91a7-4916-ae15-3b1065b64e4b', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('d98f03d9-91a7-4916-ae15-3b1065b64e4b', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('d98f03d9-91a7-4916-ae15-3b1065b64e4b', foundational, credentialing_as_meritocratic_filter).
narrative_ontology:cs_axiom_status(credentialing_as_meritocratic_filter, holdable).
narrative_ontology:cs_axiom_grounding('d98f03d9-91a7-4916-ae15-3b1065b64e4b', credentialing_as_meritocratic_filter, conventional).
narrative_ontology:cs_axiom('d98f03d9-91a7-4916-ae15-3b1065b64e4b', secondary, market_access_is_a_privilege_not_a_right).
narrative_ontology:cs_axiom_status(market_access_is_a_privilege_not_a_right, holdable).
narrative_ontology:cs_axiom_grounding('d98f03d9-91a7-4916-ae15-3b1065b64e4b', market_access_is_a_privilege_not_a_right, conventional).
narrative_ontology:cs_reference_frame('d98f03d9-91a7-4916-ae15-3b1065b64e4b', meritocratic_gatekeeping_ideal).
narrative_ontology:cs_drift_state('d98f03d9-91a7-4916-ae15-3b1065b64e4b', contemporary_labor_market_realities, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d98f03d9-91a7-4916-ae15-3b1065b64e4b', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, educational_institutions).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government-mandated bodies that define, administer, and enforce credentialing standards. They claim to uphold public safety and professional integrity, but their processes often create significant barriers to entry, benefiting existing practitioners and the institutions that provide credentialing pathways.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Incumbent practitioners who have met the statutory requirements. They benefit from reduced competition, higher wages, and enhanced professional status due to the restricted labor supply. They often advocate for maintaining or increasing credentialing barriers.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_professionals, beneficiary,
    powerful, biographical, mobile, national).

% Individuals from low-income backgrounds, minority groups, or with non-traditional educational paths who are unable to meet the financial, time, or educational requirements for credentials. They are effectively excluded from higher-paying professions, forcing them into lower-wage, uncredentialed work or unemployment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, immediate, trapped, local).

% Individuals seeking to enter credentialed professions but facing significant hurdles in acquiring the necessary education, training, or experience due to cost, time commitment, or lack of access to approved programs. Their career mobility is severely constrained by these requirements.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_aspirants, payer,
    powerless, biographical, constrained, local).

% Organizations advocating for labor market reform, reduced occupational licensing barriers, and increased access to professions for marginalized communities. They analyze the economic and social impact of credentialing and lobby for policy changes.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, public_advocacy_groups, observer,
    organized, biographical, analytical, national).

% Universities, colleges, and vocational schools that offer programs leading to statutory credentials. They benefit financially from the demand for credentialing pathways, often aligning their curricula and admissions with licensing board requirements.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, educational_institutions, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates labor quality and consumer protection by setting minimum standards for professional practice.
% TRANSFER_FUNCTION: Transfers market access and economic rents from uncredentialed or marginalized workers to credentialed professionals and the institutions that provide credentialing, by restricting labor supply.
% ABSENT_VOICES: Workers from low-income backgrounds, immigrants, and those with non-traditional education paths are often excluded from the policy-making process, and their experiences of the barriers are not adequately represented.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished overnight, there would be a rapid influx of new entrants into professions, leading to wage compression, increased competition, and potentially a re-evaluation of quality assurance mechanisms. The labor market structure would fundamentally reorganize.
% FOUNDING_PROBLEM: To protect the public from incompetent or unethical practitioners by ensuring a minimum standard of professional competence.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and professional associations assert the problem is live, citing ongoing risks to public safety. Critics, including public advocacy groups and some economists, argue that the founding problem is substantially solved by other means (e.g., tort law, reputation) and that the requirements now primarily serve to restrict competition; independent economic analyses often support this shifted-function reading.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is very high (0.90) because the system systematically transfers economic opportunity from those excluded to those included, with costs far exceeding any marginal service provision. Suppression is also very high (0.90) due to the legal and institutional barriers that make entry into regulated professions extremely difficult without meeting specific, often costly, requirements. The theater ratio is moderate (0.45) as there is a genuine, albeit often overstated, public safety function, but a significant portion of the enforcement and justification serves to maintain market exclusion. Accessibility collapse is high (0.80) as viable alternative paths to professional practice are severely limited. Resistance is moderate (0.50) reflecting ongoing advocacy and debate, but the entrenched nature of the system limits its immediate impact. The measurement series show a clear trend of increasing extractiveness and suppression over time, indicating the hardening of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of licensing boards and credentialed professionals, the system is a necessary 'Rope' or 'Scaffold' for public protection and professional integrity. However, from the perspective of marginalized workers and uncredentialed aspirants, the same structure operates as a 'Snare,' actively extracting opportunity and suppressing mobility. The engine's classification will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed professionals and educational institutions are clear beneficiaries, gaining from reduced competition and increased demand for their services/programs. Licensing boards, as agenda-setters, also benefit from their institutional authority and control over the profession. Marginalized workers and uncredentialed aspirants are the primary victims, bearing the costs of exclusion and limited economic mobility. Public advocacy groups act as observers, analyzing and challenging the system's effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_vs_exclusion_primary_function,
    'Is the primary function of statutory credentialing to ensure public safety and competence, or to create market exclusion and extract rents?',
    'Comparative analysis of public harm rates in regulated vs. unregulated jurisdictions (where deregulation has occurred), and economic studies on the impact of licensing on wages and labor supply.',
    'If public safety is demonstrably the primary function, the constraint leans towards a Tangled Rope or Rope; if market exclusion dominates, it is firmly a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_exclusion_primary_function, empirical, 'Ambiguity over the true purpose of credentialing requirements.').

omega_variable(
    disproportionate_impact_causality,
    'Are the disproportionate impacts on marginalized groups an unintended side effect of legitimate standards, or a structural outcome of design choices that favor existing resource holders?',
    'Policy analysis tracing the legislative history and intent of specific credentialing requirements, combined with sociological studies on access barriers and outcomes for different demographic groups.',
    'If unintended, policy adjustments might mitigate harm without dismantling the system; if structural, the system itself is designed to filter access based on prior resources, reinforcing its Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disproportionate_impact_causality, conceptual, 'Causality of disproportionate impact on marginalized groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.33).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.36).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.39).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.42).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, professional_association_lobbying).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, vocational_training_funding).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, social_mobility_barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
