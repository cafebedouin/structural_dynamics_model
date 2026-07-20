% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary Reading
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the formalist_employment_reading of
 *   the employment_boundary kernel. Under this reading, employment status is
 *   determined exclusively by the presence of a formal contract of service
 *   and direct personal supervision. Platform workers, who typically sign
 *   independent-contractor agreements and are managed by algorithm rather
 *   than human supervisor, are placed outside the employment boundary. This
 *   reading generates high extraction by externalizing social insurance costs
 *   and income risks to workers and the public fisc, while platform companies
 *   capture the avoided obligations as retained surplus.
 *
 * KEY AGENTS:
 *   - platform_companies: Primary beneficiary (institutional/arbitrage) â captures avoided social insurance and wage obligations
 *   - platform_workers: Primary target (powerless/constrained) â bears extraction via misclassification
 *   - public_insurance_system: Secondary target (institutional/constrained) â absorbs externalized social costs
 *   - labor_courts_and_agencies: Agenda-setter (institutional/analytical) â administers the formalist test
 *   - worker_advocates: Excluded voice (organized/constrained) â argues for substantive tests but kept out of the formalist framework
 *   - labor_economists: Analytical observer (analytical/analytical) â documents the fiscal and welfare divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.82).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary Reading").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'a3e17ee7-e276-4529-8ada-112c9960b619').
narrative_ontology:cs_kernel_codification('a3e17ee7-e276-4529-8ada-112c9960b619', fixed_text).
narrative_ontology:cs_authority_grounding('a3e17ee7-e276-4529-8ada-112c9960b619', lineage).
narrative_ontology:cs_interpretation_layer_present('a3e17ee7-e276-4529-8ada-112c9960b619').
narrative_ontology:cs_reading_relation('a3e17ee7-e276-4529-8ada-112c9960b619', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('a3e17ee7-e276-4529-8ada-112c9960b619', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('a3e17ee7-e276-4529-8ada-112c9960b619', foundational, contract_form_determines_status).
narrative_ontology:cs_axiom_status(contract_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('a3e17ee7-e276-4529-8ada-112c9960b619', contract_form_determines_status, conventional).
narrative_ontology:cs_axiom('a3e17ee7-e276-4529-8ada-112c9960b619', foundational, algorithmic_absence_equals_autonomy).
narrative_ontology:cs_axiom_status(algorithmic_absence_equals_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a3e17ee7-e276-4529-8ada-112c9960b619', algorithmic_absence_equals_autonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('a3e17ee7-e276-4529-8ada-112c9960b619', formalist_legal_boundary).
narrative_ontology:cs_drift_state('a3e17ee7-e276-4529-8ada-112c9960b619', platform_economy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3e17ee7-e276-4529-8ada-112c9960b619', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, public_insurance_system).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, regulatory_forbearance_for_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify workers as independent contractors under the formalist test, avoiding payroll taxes, benefits, minimum wage obligations, and liability. They lobby legislatures and fund litigation to preserve the contract-form boundary, capturing avoided obligations as retained surplus.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, beneficiary,
    institutional, generational, arbitrage, national).

% Perform algorithmically managed labor but are legally classified as independent contractors. They pay self-employment taxes, receive no unemployment insurance or workers compensation, absorb income volatility, and are excluded from collective bargaining protections. Exit means leaving the platform economy entirely, which is costly in monopsonistic local labor markets.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, national).

% Absorbs costs externalized by platform companies when platform workers claim publicly funded healthcare subsidies, food assistance, or emergency unemployment-like support that would otherwise be employer-provided or employment-tied. The fund cannot exit its obligation to provide baseline social support.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, public_insurance_system, payer,
    institutional, generational, constrained, national).

% Administer the formalist employment test, examining contract language and direct supervision evidence to classify workers. Their rulings enforce the boundary and determine who falls inside or outside employment protections.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_courts_and_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Argue that economic dependence and algorithmic control should determine employment status, but their substantive framing is structurally excluded from the formalist test's operation; their arguments are treated as policy preferences rather than legal criteria.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, worker_advocates, excluded,
    organized, biographical, constrained, national).

% Document the divergence between formal classification and economic reality, measuring the fiscal and welfare costs of the formalist boundary. They observe without direct power to alter the test.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line legal test for classifying labor relationships, reducing administrative adjudication costs and preserving employer flexibility in structuring work arrangements.
% TRANSFER_FUNCTION: Moves the costs of social insurance, income risk absorption, and regulatory compliance from platform companies to platform workers and the public insurance system.
% ABSENT_VOICES: Platform workers experiencing economic dependence despite contract form, and advocates for substantive economic-realities tests, are structurally excluded from the formalist framework; their claims are treated as policy preferences outside the legal test.
% DISAPPEARANCE_RATIONALE: If the formalist boundary vanished, platform workers would be reclassified as employees, platform business models would face restructuring, public insurance costs would shift back to employers, and the gig economy's current cost structure would collapse.
% FOUNDING_PROBLEM: How to determine which labor relationships trigger employer obligations in a mixed economy, providing legal certainty and administrable rules for businesses and courts.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians attest the formalist test originated in early industrial factory settings with clear supervision hierarchies. Platform companies attest the problem remains live, citing need for clarity. Independent legal scholars and economists document the mismatch between the test and platform-era economic reality; no corroboration from outside the benefiting parties supports the claim that the formalist test is still fit for purpose in the platform context.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.82, 'kimi-k2.6', 'none', direct).

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

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint systematically transfers social insurance, risk, and wage-floor costs from firms to workers and the state. Suppression (0.78) is high because the boundary requires active legal enforcement (contractual arbitration clauses, anti-union doctrines for contractors, judicial adherence to the formalist test) to prevent reclassification. Theater_ratio (0.45) is moderate: public debate performs a narrative about entrepreneurial flexibility and innovation that obscures the static transfer. Accessibility_collapse (0.60) reflects that substantive tests exist in some jurisdictions but are suppressed in the dominant formalist regime. Resistance (0.55) captures ongoing worker litigation, regulatory challenges, and legislative proposals. The metrics and claimed_type are authored independently: the constraint is claimed as snare because the coordination story (legal clarity) serves primarily to obscure extraction.
 *
 * PERSPECTIVAL GAP:
 *   The platform-company seat experiences the constraint as necessary legal clarity that preserves flexible work arrangements. The worker seat experiences it as exclusion from basic protections despite economically dependent work. The public-insurance seat experiences it as an unfunded mandate. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_companies are declared beneficiaries: the constraint subsidizes them by relieving employment obligations, pushing their directionality toward the beneficiary pole and damping effective extraction. Platform_workers and public_insurance_system are declared victims: they bear the constraint's costs, pushing their directionality toward the target pole and amplifying effective extraction. Labor_courts_and_agencies administer the constraint with analytical exit; their directionality is symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was administrable labor classification in an industrial economy. The formalist reading's persistence is contested: platform companies claim the problem is still live, while labor economists and legal historians argue the economic reality has drifted and the formalist solution now functions primarily as a cost-externalization device. The R5 genealogy interview captures this contested status. Mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_substantive_exclusivity,
    'Is the formalist contract-and-supervision test logically exclusive of the substantive economic-dependence test, or can a single legal framework incorporate both as factors?',
    'Comparative legal analysis of jurisdictions with multi-factor tests versus jurisdictions where contract form is dispositive.',
    'If exclusive, the kernel produces a zero-sum political contest; if reconcilable, hybrid readings may be unnecessary and the classification of this constraint as foreclosing may require revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_substantive_exclusivity, conceptual, 'Whether the formalist and substantive readings are mutually exclusive or partially reconcilable.').

omega_variable(
    autonomy_narrative_validity,
    'Is the autonomy claimed by the formalist reading (platform workers chose flexibility) empirically accurate for the majority, or is it a post-hoc rationalization of structural necessity?',
    'Large-N surveys of worker preferences and exit behavior, controlling for local labor market monopsony conditions.',
    'If false, the formalist reading''s foundational axiom is empirically contested and the suppression metric reflects partially internalized coercion rather than purely structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_narrative_validity, empirical, 'Empirical validity of the worker-autonomy justification.').

omega_variable(
    public_cost_quantification,
    'What is the aggregate value of social insurance costs externalized from platform companies to public funds under the formalist boundary?',
    'Government accountability office audits and fiscal incidence studies measuring subsidy flows to platform workers.',
    'Quantifies the extraction magnitude and confirms whether the public insurance system functions as a victim seat or merely a pass-through.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_cost_quantification, empirical, 'Fiscal magnitude of cost externalization to public insurance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__formalist_employment_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__formalist_employment_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__formalist_employment_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(empl_tr_t32, employment_boundary__formalist_employment_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(empl_tr_t40, employment_boundary__formalist_employment_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t8, employment_boundary__formalist_employment_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(empl_be_t16, employment_boundary__formalist_employment_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(empl_be_t24, employment_boundary__formalist_employment_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(empl_be_t32, employment_boundary__formalist_employment_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(empl_be_t40, employment_boundary__formalist_employment_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(empl_su_t8, employment_boundary__formalist_employment_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(empl_su_t16, employment_boundary__formalist_employment_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(empl_su_t24, employment_boundary__formalist_employment_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(empl_su_t32, employment_boundary__formalist_employment_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(empl_su_t40, employment_boundary__formalist_employment_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'employment boundary' conflates three structurally distinct readings: the formalist reading (contract + supervision), the substantive reading (economic dependence + algorithmic control), and the hybrid reading (third category). Each has a distinct epsilon, beneficiary/victim structure, and classification. This file instantiates the formalist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
