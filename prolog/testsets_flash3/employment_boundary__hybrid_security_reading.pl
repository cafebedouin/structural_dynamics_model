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
 *   human_readable: Hybrid Security for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid security' reading of the employment
 *   boundary, where platform workers are recognized as a distinct category
 *   requiring tailored protections, distinct from both traditional employment
 *   and independent contracting. This reading aims to provide some social
 *   safety net while preserving the flexibility desired by platform
 *   companies. The metrics reflect a moderately extractive and suppressive
 *   system, as it institutionalizes a degree of precarity while claiming to
 *   offer protection.
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
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Security for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'bbe6e758-54ca-45bf-838b-191be702b880').
narrative_ontology:cs_kernel_codification('bbe6e758-54ca-45bf-838b-191be702b880', formalized).
narrative_ontology:cs_authority_grounding('bbe6e758-54ca-45bf-838b-191be702b880', lineage).
narrative_ontology:cs_interpretation_layer_present('bbe6e758-54ca-45bf-838b-191be702b880').
narrative_ontology:cs_reading_relation('bbe6e758-54ca-45bf-838b-191be702b880', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbe6e758-54ca-45bf-838b-191be702b880', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('bbe6e758-54ca-45bf-838b-191be702b880', foundational, platform_work_is_distinct).
narrative_ontology:cs_axiom_status(platform_work_is_distinct, holdable).
narrative_ontology:cs_axiom_grounding('bbe6e758-54ca-45bf-838b-191be702b880', platform_work_is_distinct, conventional).
narrative_ontology:cs_axiom('bbe6e758-54ca-45bf-838b-191be702b880', foundational, basic_protections_are_necessary).
narrative_ontology:cs_axiom_status(basic_protections_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('bbe6e758-54ca-45bf-838b-191be702b880', basic_protections_are_necessary, instrumental).
narrative_ontology:cs_reference_frame('bbe6e758-54ca-45bf-838b-191be702b880', flexible_labor_with_social_floor).
narrative_ontology:cs_drift_state('bbe6e758-54ca-45bf-838b-191be702b880', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bbe6e758-54ca-45bf-838b-191be702b880', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced labor costs and flexibility compared to traditional employment, while accepting some obligations for basic worker protections (e.g., injury insurance). They actively lobby for this 'third category' to avoid full employment liabilities.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive some basic protections (e.g., medical, injury insurance) but lack comprehensive benefits like career development, retirement security, and collective bargaining rights. Their economic dependence on platforms is high, but they are not classified as full employees.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, local).

% Benefit from the convenience and often lower cost of platform services, enabled by the flexible labor model. They are generally insulated from the direct costs of worker precarity.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, consumers, beneficiary,
    moderate, immediate, mobile, local).

% Advocate for full employment rights for platform workers, viewing the 'third category' as a way to institutionalize precarity. They are largely excluded from shaping the current policy framework for this hybrid status.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Develop and implement regulations for platform work, seeking a balance between innovation, worker protection, and economic flexibility. They are the primary architects of the 'hybrid security' model.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, social_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the provision of basic social protections (e.g., injury insurance, some medical benefits) for platform workers, addressing some of the precarity inherent in independent contracting without imposing full employment obligations on platforms.
% TRANSFER_FUNCTION: Transfers some social security costs from the state or individual workers to platform companies, while transferring the burden of full employment benefits (e.g., retirement, career development) from platforms to workers or the state.
% ABSENT_VOICES: Labor unions and advocates for full employment rights are largely absent from the direct negotiation of this 'third category' framework; they would argue for a reclassification of platform workers as employees.
% DISAPPEARANCE_RATIONALE: If the hybrid security framework vanished, platform companies would either face pressure to fully employ workers (increasing costs) or revert to a pure independent contractor model (reducing worker protections), leading to significant legal and economic reorganization.
% FOUNDING_PROBLEM: The rise of the platform economy created a large class of workers who did not fit traditional employment definitions but lacked the full autonomy and security of independent contractors, leading to significant precarity and social welfare gaps.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers and international labor organizations corroborate that the problem of platform worker precarity remains live, even with hybrid models, as these models often do not fully address long-term security and career development. Platform companies acknowledge the problem but argue their model is the appropriate solution.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while some protections are offered, the core economic precarity and lack of comprehensive benefits remain. Suppression (0.6) is significant as this framework actively prevents platform workers from being classified as full employees, thereby suppressing demands for full employment rights. The theater ratio (0.2) is low, indicating that the protections offered are genuinely functional, but the 'hybrid' framing also serves to legitimize a less-than-full-employment model.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies view this as a necessary and fair compromise, balancing innovation with worker welfare. Platform workers, while benefiting from some protections, may still experience it as a form of extraction due to the lack of full employment rights. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are beneficiaries (lower labor costs, flexibility) and agenda-setters (lobbying for this model). Platform workers are payers (bear remaining precarity, lack full benefits). Consumers are beneficiaries (convenient, often cheaper services). Labor unions are excluded, as their preferred outcome (full employment) is foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_protections,
    'What is the actual coverage and adequacy of the ''tailored protections'' provided under this hybrid model, particularly regarding long-term financial security and career development?',
    'Longitudinal studies tracking platform worker outcomes (income stability, retirement savings, skill development) compared to traditional employees and truly independent contractors.',
    'If protections are found to be significantly inadequate, the effective extractiveness of this model would be higher, pushing it closer to a Snare. If they prove robust, extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_protections, empirical, 'Assesses the real-world impact and sufficiency of hybrid worker protections.').

omega_variable(
    institutional_legitimacy_of_third_category,
    'Is the ''third category'' a stable, legitimate institutional innovation, or a temporary compromise driven by platform lobbying that will eventually collapse into either full employment or pure independent contracting?',
    'Analysis of legislative trends, judicial rulings, and public discourse over the next decade. Persistence and expansion of the model would suggest stability; increasing legal challenges or legislative shifts would suggest instability.',
    'If unstable, the constraint''s long-term persistence is questionable, and its current classification as Tangled Rope might be a transitional state towards Snare (if protections erode) or Rope (if full employment is achieved).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_legitimacy_of_third_category, conceptual, 'Examines the long-term viability and legitimacy of the hybrid worker classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to reclassification) or internalized (platform workers'' acceptance of precarity due to lack of alternatives or perceived benefits of flexibility)?',
    'Post-exit suppression trajectory: if platform workers continue to accept similar precarity after leaving a specific platform, reclassify as partially internalized. Policy experiments with opt-out models for full employment benefits.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — workers carry the suppression with them after exit, making collective action harder. If purely structural, policy changes are more direct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for platform workers.').


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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
