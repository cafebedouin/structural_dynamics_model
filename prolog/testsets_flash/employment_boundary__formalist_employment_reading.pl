% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Platform Economy Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.85).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.75).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Platform Economy Reading)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '04269a57-94a5-4cdd-98b5-1cfb44ad8156').
narrative_ontology:cs_kernel_codification('04269a57-94a5-4cdd-98b5-1cfb44ad8156', formalized).
narrative_ontology:cs_authority_grounding('04269a57-94a5-4cdd-98b5-1cfb44ad8156', lineage).
narrative_ontology:cs_interpretation_layer_present('04269a57-94a5-4cdd-98b5-1cfb44ad8156').
narrative_ontology:cs_reading_relation('04269a57-94a5-4cdd-98b5-1cfb44ad8156', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('04269a57-94a5-4cdd-98b5-1cfb44ad8156', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('04269a57-94a5-4cdd-98b5-1cfb44ad8156', foundational, contractual_form_determines_status).
narrative_ontology:cs_axiom_status(contractual_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('04269a57-94a5-4cdd-98b5-1cfb44ad8156', contractual_form_determines_status, conventional).
narrative_ontology:cs_axiom('04269a57-94a5-4cdd-98b5-1cfb44ad8156', foundational, direct_supervision_is_control).
narrative_ontology:cs_axiom_status(direct_supervision_is_control, holdable).
narrative_ontology:cs_axiom_grounding('04269a57-94a5-4cdd-98b5-1cfb44ad8156', direct_supervision_is_control, conventional).
narrative_ontology:cs_reference_frame('04269a57-94a5-4cdd-98b5-1cfb44ad8156', traditional_master_servant_doctrine).
narrative_ontology:cs_drift_state('04269a57-94a5-4cdd-98b5-1cfb44ad8156', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04269a57-94a5-4cdd-98b5-1cfb44ad8156', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, gig_economy_investors).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of engagement for platform workers, classifying them as independent contractors. This allows them to avoid payroll taxes, benefits, and minimum wage obligations. They actively lobby against reclassification efforts.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of self-employment, including health insurance, retirement savings, and unemployment risk, while often lacking true autonomy or control over their work. Their 'flexibility' comes at the cost of basic labor protections.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, local).

% Subsidize the platform economy by covering costs (e.g., healthcare, unemployment benefits) that would otherwise be borne by employers. This externalization of costs shifts the burden to the public.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, taxpayers_social_safety_net, payer,
    organized, generational, constrained, national).

% Operate under traditional employment laws, bearing higher labor costs and regulatory burdens. They are disadvantaged by the cost arbitrage enjoyed by platform companies, but cannot easily reclassify their own workforce without legal risk.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, excluded,
    powerful, biographical, constrained, national).

% Are structurally excluded from organizing platform workers due to their classification as independent contractors, which falls outside traditional collective bargaining frameworks. They advocate for reclassification but face significant legal and political barriers.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_unions, excluded,
    organized, generational, trapped, national).

% Profit from the low labor costs and high flexibility afforded by the independent contractor model, which boosts company valuations and returns on investment. They actively support the formalist interpretation of employment law.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, gig_economy_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for classifying workers, enabling businesses to structure their operations and manage labor costs by distinguishing between employees and independent contractors.
% TRANSFER_FUNCTION: Transfers the costs and risks associated with employment (e.g., benefits, payroll taxes, minimum wage) from platform companies to individual workers and the public social safety net.
% ABSENT_VOICES: Platform workers, if truly empowered, would demand reclassification as employees or a hybrid status with corresponding protections. Labor unions, excluded from organizing, would advocate for collective bargaining rights for these workers. Traditional employers would demand a level playing field.
% DISAPPEARANCE_RATIONALE: If the formalist employment boundary vanished, platform companies would face massive reclassification liabilities, forcing a fundamental restructuring of their business models, labor practices, and pricing. The social safety net would see reduced strain as costs are internalized by employers, and labor markets would rebalance.
% FOUNDING_PROBLEM: The need for clear legal distinctions between different types of work relationships to facilitate economic activity and assign responsibilities, particularly in the context of new forms of work.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and their investors assert the problem is live, arguing that a strict formalist boundary is essential for innovation and flexibility. Labor advocates, platform workers, and some policymakers argue that the original problem of classification has been distorted to enable exploitation, and that the current status is a 'dead' solution to a 'live' problem of worker precarity, as evidenced by numerous legal challenges and academic studies.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_vs_substantive_definition,
    'Is employment fundamentally defined by formal contract and direct supervision (formalist), or by economic dependence and algorithmic control (substantive)?',
    'Judicial rulings or legislative action that explicitly adopt a substantive test for employment, or a shift in economic conditions that makes formal control irrelevant.',
    'If a substantive definition is adopted, platform workers would be reclassified as employees, significantly altering the constraint''s beneficiary/victim structure and reducing its extractiveness. This would shift it from a Snare towards a Tangled Rope or even a Rope, depending on the new regulatory framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalist_vs_substantive_definition, conceptual, 'Ambiguity in the core definition of ''employment''.').

omega_variable(
    hybrid_category_viability,
    'Is a ''hybrid'' worker category, distinct from both employee and independent contractor, a viable and effective solution for platform workers?',
    'Empirical evaluation of hybrid models implemented in various jurisdictions (e.g., Spain''s ''rider law'', California''s Prop 22) to assess their impact on worker protections, platform viability, and economic efficiency.',
    'If a hybrid category proves viable and widely adopted, this formalist reading of the employment boundary would be superseded, leading to a new constraint (the ''hybrid_security_reading'') with potentially lower extraction and suppression for platform workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_category_viability, empirical, 'Feasibility and impact of a third worker classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, economic dependence) or internalized (workers'' belief in ''flexibility'' over rights)?',
    'Post-exit suppression trajectory: if workers continue to resist reclassification or unionization even after legal barriers are removed, it suggests internalized suppression. Surveys on worker preferences and understanding of their rights.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as workers carry the suppression with them. This would make the Snare more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for platform workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2008, employment_boundary__formalist_employment_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(empl_tr_t2012, employment_boundary__formalist_employment_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(empl_tr_t2016, employment_boundary__formalist_employment_reading, theater_ratio, 2016, 0.16).
narrative_ontology:measurement(empl_tr_t2020, employment_boundary__formalist_employment_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(empl_tr_t2024, employment_boundary__formalist_employment_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(empl_be_t2008, employment_boundary__formalist_employment_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(empl_be_t2012, employment_boundary__formalist_employment_reading, base_extractiveness, 2012, 0.7).
narrative_ontology:measurement(empl_be_t2016, employment_boundary__formalist_employment_reading, base_extractiveness, 2016, 0.78).
narrative_ontology:measurement(empl_be_t2020, employment_boundary__formalist_employment_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__formalist_employment_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2008, employment_boundary__formalist_employment_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(empl_su_t2012, employment_boundary__formalist_employment_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(empl_su_t2016, employment_boundary__formalist_employment_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(empl_su_t2020, employment_boundary__formalist_employment_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__formalist_employment_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, social_safety_net_funding).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, labor_union_organizing_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'employment_boundary' kernel. This 'formalist' reading emphasizes contractual form and direct supervision, leading to the exclusion of platform workers from employment protections. The 'substantive' reading emphasizes economic dependence, and the 'hybrid_security' reading proposes a third category. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
