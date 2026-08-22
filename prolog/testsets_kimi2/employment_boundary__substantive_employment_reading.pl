% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Reading of Platform Work Boundary
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the substantive employment reading of the
 *   contested employment_boundary kernel. It asserts that employment should
 *   be defined by economic dependence and algorithmic control rather than by
 *   contractual form, thereby reclassifying platform workers as employees
 *   entitled to full social protections. The constraint is assessed from this
 *   reading's own lights: the standing arrangement under contest is the
 *   platform economy's prevailing contractor-classification regime, which the
 *   substantive reading identifies as extractive. Platform workers are the
 *   structural beneficiaries of reclassification, while gig platforms bear
 *   the compliance costs as payers. The arrangement requires active
 *   enforcement because platforms resist reclassification through legal
 *   challenge, lobbying, and restructuring. Moderate extractiveness (0.55)
 *   reflects the real but contested transfer of labor costs from workers and
 *   public safety nets back to platforms. The claimed type is tangled_rope
 *   because the constraint simultaneously coordinates social protection and
 *   extracts compliance value from platforms.
 *
 * KEY AGENTS:
 *   - platform_workers: Primary beneficiary (powerless/constrained) â gain employment protections and social insurance under substantive reclassification.
 *   - gig_platforms: Primary payer (institutional/arbitrage) â bear compliance costs and resist reclassification through legal, political, and structural channels.
 *   - labor_regulators: Agenda setter (institutional/analytical) â define and enforce the substantive employment test across jurisdictions.
 *   - competition_authorities: Analytical observer (institutional/analytical) â assess competitive asymmetries created by divergent classification regimes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.55).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.6).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Reading of Platform Work Boundary").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '783aaa08-a5c0-46b5-9349-84dfe5ff70be').
narrative_ontology:cs_kernel_codification('783aaa08-a5c0-46b5-9349-84dfe5ff70be', formalized).
narrative_ontology:cs_authority_grounding('783aaa08-a5c0-46b5-9349-84dfe5ff70be', lineage).
narrative_ontology:cs_interpretation_layer_present('783aaa08-a5c0-46b5-9349-84dfe5ff70be').
narrative_ontology:cs_reading_relation('783aaa08-a5c0-46b5-9349-84dfe5ff70be', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('783aaa08-a5c0-46b5-9349-84dfe5ff70be', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('783aaa08-a5c0-46b5-9349-84dfe5ff70be', foundational, substantive_protection_over_contract_form).
narrative_ontology:cs_axiom_status(substantive_protection_over_contract_form, holdable).
narrative_ontology:cs_axiom_grounding('783aaa08-a5c0-46b5-9349-84dfe5ff70be', substantive_protection_over_contract_form, deontological).
narrative_ontology:cs_axiom('783aaa08-a5c0-46b5-9349-84dfe5ff70be', foundational, algorithmic_direction_is_substantive_control).
narrative_ontology:cs_axiom_status(algorithmic_direction_is_substantive_control, holdable).
narrative_ontology:cs_axiom_grounding('783aaa08-a5c0-46b5-9349-84dfe5ff70be', algorithmic_direction_is_substantive_control, empirically_contingent).
narrative_ontology:cs_reference_frame('783aaa08-a5c0-46b5-9349-84dfe5ff70be', protective_labor_tradition).
narrative_ontology:cs_drift_state('783aaa08-a5c0-46b5-9349-84dfe5ff70be', platform_economy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('783aaa08-a5c0-46b5-9349-84dfe5ff70be', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, gig_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform task-based labor managed by algorithmic dispatch, performance ratings, and dynamic pricing; depend on platform income for basic subsistence without social insurance, minimum wage floors, or collective bargaining rights. Under substantive reclassification they would gain employment protections, but face platform threats of market withdrawal and reduced scheduling flexibility.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, biographical, constrained, national).

% Operate digital labor markets that match workers with tasks and customers; classify workers as independent contractors to avoid social insurance contributions, minimum wage obligations, and severance costs. Resist substantive reclassification through litigation, lobbying, public relations campaigns, and algorithmic restructuring aimed at evading dependence thresholds.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_platforms, payer,
    institutional, generational, arbitrage, global).

% Administrative agencies and courts responsible for defining and enforcing the employment boundary; some jurisdictions have adopted or are considering substantive tests based on economic dependence and algorithmic control, while others retain formalist contract-based standards. Their enforcement posture determines whether the substantive standard bites.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Assess whether platform business models that rely on contractor classification confer unfair competitive advantages over firms that employ workers directly and bear full social insurance costs. They produce market studies and recommendations but do not directly set employment status.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, competition_authorities, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends social insurance, minimum wage guarantees, job security, and collective bargaining rights to economically dependent workers who are directed by algorithmic management, solving the coordination problem of protecting workers in digitally mediated labor markets without a traditional employer workplace.
% TRANSFER_FUNCTION: Moves the cost of social insurance contributions, minimum wage compliance, severance obligations, and employment protections from workers and public safety nets to the platform firms that exercise algorithmic control over the labor process.
% ABSENT_VOICES: Platform workers in jurisdictions without labor enforcement capacity, consumers who would face higher service prices under full cost internalization, and small platform competitors that could not absorb compliance costs are largely absent from policy negotiations dominated by large platforms and organized labor.
% DISAPPEARANCE_RATIONALE: If the substantive employment standard disappeared overnight, platforms would revert to unchallenged contractor classification, social insurance gaps for dependent workers would reopen, labor costs would shift back to workers and public systems, and the competitive landscape would tilt toward firms exploiting classification arbitrage.
% FOUNDING_PROBLEM: The rapid expansion of platform-mediated work created a growing population of workers who were economically dependent on a single platform and subject to real-time algorithmic direction but were classified as independent contractors, leaving them without social insurance, minimum wage protection, or collective bargaining rights.
% FOUNDING_PROBLEM_CORROBORATION: International Labour Organization reports, peer-reviewed labor economics research, and consumer protection advocates outside the platform industry attest to the growth of dependent self-employment in platform markets and the corresponding erosion of social protection coverage.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the cost transfer to platforms is real and substantial but bounded by enforcement limitations, jurisdictional variance, and platform evasion tactics. Suppression (0.60) is structural: platforms deploy significant legal and political resources to block enforcement, but suppression is not total because regulatory momentum and court rulings in some jurisdictions are advancing. Theater ratio (0.25) is modest: some platform responses are performative (adding nominal autonomy features to evade the test), but the core conflict is substantive rather than theatrical. Resistance (0.75) is high because platforms have mobilized aggressively against reclassification in multiple jurisdictions. Accessibility collapse (0.45) is moderate because the hybrid third-category alternative and formalist contract-based approaches remain live options in public discourse and policy debate.
 *
 * PERSPECTIVAL GAP:
 *   Platform workers and gig platforms compute diametrically opposed seat types: workers experience the constraint as protective coordination (low directionality, subsidy), while platforms experience it as cost imposition (high directionality, extraction). Labor regulators and competition authorities sit at an intermediate analytical distance with low personal extraction. The engine will detect this divergence from the identical structural data viewed from different power and exit positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_workers are declared beneficiaries with constrained exit options (low power, limited labor market alternatives), which drives their directionality toward the beneficiary pole. Gig_platforms are declared victims with arbitrage-grade exit options (institutional power, able to restructure, relocate, or litigate), but their victim status pulls directionality toward the target pole; the arbitrage option dampens but does not eliminate the extraction signal. Labor_regulators and competition_authorities are observers with analytical exit and institutional power, placing them near neutral directionality. The spread between worker and platform seats is wide and structurally stable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â dependent workers lacking social protection in the platform economy â remains live and substantiated by external corroboration, so mandatrophy (persistence past function) does not apply. The constraint is not a piton because its coordination function is genuine and ongoing, and it is not a snare because the worker protection rationale is not a cover story for extraction. The Tangled Rope classification captures the dual nature: genuine coordination of social protection for workers plus asymmetric extraction of compliance costs from platforms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evasion_threshold,
    'Can platforms restructure algorithms, contract terms, and task allocation to fall below economic dependence or algorithmic control thresholds, rendering the substantive employment test ineffective?',
    'Comparative jurisdictional analysis tracking platform restructuring responses after substantive employment rulings; measure changes in worker dependency ratios and algorithmic direction intensity post-decision.',
    'If evasion is widespread and successful, the constraint''s effective extractiveness is lower than measured and the constraint trends toward theater; if thresholds are robust, extraction holds and platforms remain payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evasion_threshold, empirical, 'Whether platforms can structurally evade substantive employment tests.').

omega_variable(
    jurisdiction_fragmentation,
    'Will substantive employment readings converge across jurisdictions or fragment into incompatible national standards, creating regulatory arbitrage opportunities?',
    'Track EU platform work directives, US state-level ABC test adoptions, and Global South classification jurisprudence for convergence or divergence patterns over the next decade.',
    'Fragmentation reduces the constraint''s effective spatial scope and raises extraction variance across seats; convergence strengthens enforcement and raises systemic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdiction_fragmentation, conceptual, 'Convergence versus fragmentation of substantive employment standards.').

omega_variable(
    founding_problem_persistence,
    'Is the growth of platform-mediated dependent work a temporary transition or a permanent structural shift in labor markets?',
    'Longitudinal labor market data on platform work duration, primary versus supplementary income dependence, and algorithmic management diffusion across non-platform sectors.',
    'If the founding problem is temporary, the constraint may function as a scaffold toward broader labor market transformation; if permanent, it is a sustained tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether platform-dependent work is a transient or permanent labor market feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substantive_emp_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(substantive_emp_tr_t4, employment_boundary__substantive_employment_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(substantive_emp_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(substantive_emp_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(substantive_emp_tr_t16, employment_boundary__substantive_employment_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(substantive_emp_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(substantive_emp_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(substantive_emp_be_t4, employment_boundary__substantive_employment_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(substantive_emp_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(substantive_emp_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(substantive_emp_be_t16, employment_boundary__substantive_employment_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(substantive_emp_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(substantive_emp_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(substantive_emp_su_t4, employment_boundary__substantive_employment_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(substantive_emp_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(substantive_emp_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(substantive_emp_su_t16, employment_boundary__substantive_employment_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(substantive_emp_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the employment_boundary kernel, which decomposes into three structurally distinct claims about where the employment relationship begins. The substantive reading (this file) defines employment by economic dependence and algorithmic control; the formalist reading defines it by contract and direct supervision; the hybrid reading invents a third category. Each reading emits a different constraint with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
