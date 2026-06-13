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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Security for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint defines platform workers as a 'third category' of labor,
 *   distinct from traditional employees and independent contractors, and
 *   mandates tailored protections. It is a reading of the broader
 *   'employment_boundary' kernel, which is contested by formalist and
 *   substantive employment readings. This hybrid_security_reading aims to
 *   institutionalize a middle ground, providing some benefits while
 *   preserving platform flexibility. The structural delta is that platform
 *   workers are partially in the victim set (lack career development,
 *   retirement security) but receive basic protections (medical 91.5%, injury
 *   86.2%); platforms are partially obligated (injury insurance, not full
 *   employment); moderate ε as hybrid status institutionalizes precarity
 *   while claiming protection.
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
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'c8e1bc16-d081-4346-a20d-eedeb6030f51').
narrative_ontology:cs_kernel_codification('c8e1bc16-d081-4346-a20d-eedeb6030f51', formalized).
narrative_ontology:cs_authority_grounding('c8e1bc16-d081-4346-a20d-eedeb6030f51', lineage).
narrative_ontology:cs_interpretation_layer_present('c8e1bc16-d081-4346-a20d-eedeb6030f51').
narrative_ontology:cs_reading_relation('c8e1bc16-d081-4346-a20d-eedeb6030f51', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8e1bc16-d081-4346-a20d-eedeb6030f51', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('c8e1bc16-d081-4346-a20d-eedeb6030f51', foundational, platform_work_is_distinct).
narrative_ontology:cs_axiom_status(platform_work_is_distinct, holdable).
narrative_ontology:cs_axiom_grounding('c8e1bc16-d081-4346-a20d-eedeb6030f51', platform_work_is_distinct, conventional).
narrative_ontology:cs_axiom('c8e1bc16-d081-4346-a20d-eedeb6030f51', foundational, basic_protections_are_necessary).
narrative_ontology:cs_axiom_status(basic_protections_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c8e1bc16-d081-4346-a20d-eedeb6030f51', basic_protections_are_necessary, deontological).
narrative_ontology:cs_reference_frame('c8e1bc16-d081-4346-a20d-eedeb6030f51', balanced_flexibility_and_security).
narrative_ontology:cs_drift_state('c8e1bc16-d081-4346-a20d-eedeb6030f51', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8e1bc16-d081-4346-a20d-eedeb6030f51', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced labor costs and flexibility compared to traditional employment, while accepting some obligations for basic worker protections (e.g., injury insurance). They actively lobby for this hybrid classification to avoid full employment responsibilities.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive some basic protections (medical, injury insurance) but lack comprehensive employment benefits like career development, retirement security, and collective bargaining rights. Their income and working conditions remain precarious, but they gain some formal recognition.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, local).

% Benefit from the flexibility and lower costs of platform services, which are enabled by the hybrid labor model. They are generally unaware of the full extent of worker precarity.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, consumers, beneficiary,
    moderate, immediate, mobile, local).

% Advocate for full employment rights for platform workers, viewing the hybrid model as an erosion of labor standards. They are largely excluded from shaping the terms of this hybrid classification, which undermines their traditional bargaining power.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Bear higher labor costs due to full employment obligations, creating an uneven playing field with platform companies. They may lobby for either full employment classification for platform workers or for similar deregulation for themselves.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, payer,
    institutional, generational, constrained, national).

% Seek to balance innovation and worker protection, often seeing the hybrid model as a pragmatic compromise. They are responsible for designing and implementing the specific protections afforded to this third category of workers.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, social_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and policy framework for platform work that acknowledges some worker vulnerabilities without imposing the full regulatory burden of traditional employment, allowing the platform economy to continue operating with some social legitimacy.
% TRANSFER_FUNCTION: Transfers some basic social protections (e.g., injury insurance, medical benefits) to platform workers, while transferring the bulk of traditional employment costs (e.g., retirement, career development) away from platform companies.
% ABSENT_VOICES: Labor unions and advocates for full employment rights are largely absent from the design of this hybrid model, as their core demand for full employment is foreclosed by the premise of a 'third category.'
% DISAPPEARANCE_RATIONALE: If the hybrid security framework vanished, platform companies would face immediate pressure to either classify workers as full employees (increasing costs) or revert to pure independent contractor status (increasing worker precarity and social unrest). The platform economy's current operating model would be unsustainable.
% FOUNDING_PROBLEM: The rise of the platform economy created a large class of workers who did not fit traditional employment definitions but lacked the autonomy and security of true independent contractors, leading to widespread precarity and social welfare gaps.
% FOUNDING_PROBLEM_CORROBORATION: Academics, labor organizations, and some government agencies corroborate that the problem of worker precarity in the platform economy remains live, even with hybrid protections. Platform companies acknowledge the problem but argue their solution is appropriate; independent economic analysis supports the continued existence of significant precarity.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a new labor category (providing some protections) while simultaneously enabling asymmetric extraction (platform companies avoid full employment costs, workers remain precarious). Extractiveness is moderate (0.45) because while some benefits are provided, significant costs are still borne by workers. Suppression (0.6) is present as this classification actively suppresses attempts to reclassify workers as full employees. Theater ratio is low (0.2) as the protections are real, but the 'third category' framing can be performative in deflecting demands for full employment.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies perceive this as a necessary and fair coordination mechanism for a new economic model. Platform workers, while benefiting from some protections, often experience it as a form of extraction that institutionalizes their precarity. Policy makers see it as a pragmatic solution to a complex problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and consumers are beneficiaries, gaining flexibility and lower costs. Platform workers are victims, bearing the costs of precarity despite some protections. Labor unions are excluded, as their core demands are not met by this framework. Social policy makers and traditional employers occupy more complex positions, balancing competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_category_legitimacy,
    'Is the ''third category'' a genuine structural innovation in labor law, or a legal fiction designed to avoid full employment responsibilities?',
    'Longitudinal studies on worker outcomes (income stability, career progression, social mobility) under hybrid vs. full employment models, and analysis of legislative intent vs. actual impact.',
    'If a legal fiction, the constraint''s effective extractiveness is higher, and its classification shifts closer to a Snare. If a genuine innovation, it remains a Tangled Rope with a legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_category_legitimacy, conceptual, 'Ambiguity of the ''third category'' as a structural innovation vs. legal loophole.').

omega_variable(
    scope_of_protections,
    'Are the ''tailored protections'' sufficient to address the core vulnerabilities of platform work, or do they leave significant gaps in social security and worker rights?',
    'Comparative analysis of social safety nets and labor protections for traditional employees vs. hybrid platform workers across different jurisdictions, including worker surveys on perceived security.',
    'If protections are insufficient, the constraint''s extractiveness is higher for platform workers, and the ''coordination'' aspect is diminished, pushing it closer to a Snare. If sufficient, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_protections, empirical, 'Sufficiency of tailored protections for platform workers.').

omega_variable(
    founding_problem_mandatrophy,
    'Has the ''founding problem'' of worker precarity been genuinely addressed by the hybrid model, or has the model merely institutionalized a new form of precarity?',
    'Independent assessment of the ''founding_problem_status'' by non-beneficiary stakeholders (e.g., labor economists, worker advocacy groups) over time, comparing initial conditions to current outcomes.',
    'If the founding problem is ''dead'' but the constraint persists, it indicates mandatrophy, potentially shifting the classification towards a Piton or a more extractive Snare, as the original justification no longer holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_mandatrophy, empirical, 'Whether the hybrid model resolves or perpetuates platform worker precarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__hybrid_security_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__hybrid_security_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__hybrid_security_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(empl_tr_t2024, employment_boundary__hybrid_security_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(empl_be_t2015, employment_boundary__hybrid_security_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__hybrid_security_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__hybrid_security_reading, base_extractiveness, 2021, 0.43).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__hybrid_security_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2015, employment_boundary__hybrid_security_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__hybrid_security_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__hybrid_security_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__hybrid_security_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'employment_boundary' kernel. It defines a 'third category' for platform workers, distinct from traditional employment and independent contracting. It is linked to the 'formalist_employment_reading' and 'substantive_employment_reading' which represent alternative classifications of platform work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
