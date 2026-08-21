% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Carve-Out for Critical Systems
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint is the 'severity carve-out' reading of the 'beta
 *   designation doctrine' kernel. It asserts that beta designations are
 *   categorically unavailable for life-safety, financial, or other critical
 *   systems, regardless of testing status or disclosure. This reading
 *   contrasts with the 'expansive shield' reading (beta is a comprehensive
 *   waiver for all contexts) and the 'narrow warning' reading (beta is a
 *   time-bounded testing disclosure with base product liability preserved).
 *   This reading functions as a domain-specific physical constraint, where
 *   safety requirements and harm severity override contractual liability
 *   allocation, making beta unavailable as a mechanism in high-stakes
 *   domains.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.78).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.85).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Carve-Out for Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).
domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '910132ca-0f7e-4671-9dca-8c5315da964f').
narrative_ontology:cs_kernel_codification('910132ca-0f7e-4671-9dca-8c5315da964f', formalized).
narrative_ontology:cs_authority_grounding('910132ca-0f7e-4671-9dca-8c5315da964f', lineage).
narrative_ontology:cs_interpretation_layer_present('910132ca-0f7e-4671-9dca-8c5315da964f').
narrative_ontology:cs_reading_relation('910132ca-0f7e-4671-9dca-8c5315da964f', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('910132ca-0f7e-4671-9dca-8c5315da964f', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('910132ca-0f7e-4671-9dca-8c5315da964f', foundational, harm_severity_overrides_contractual_freedom).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_freedom, holdable).
narrative_ontology:cs_axiom_grounding('910132ca-0f7e-4671-9dca-8c5315da964f', harm_severity_overrides_contractual_freedom, deontological).
narrative_ontology:cs_axiom('910132ca-0f7e-4671-9dca-8c5315da964f', foundational, public_safety_is_non_negotiable).
narrative_ontology:cs_axiom_status(public_safety_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('910132ca-0f7e-4671-9dca-8c5315da964f', public_safety_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('910132ca-0f7e-4671-9dca-8c5315da964f', precautionary_principle_in_critical_domains).
narrative_ontology:cs_drift_state('910132ca-0f7e-4671-9dca-8c5315da964f', contemporary_software_liability_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('910132ca-0f7e-4671-9dca-8c5315da964f', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, consumers_of_critical_software).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_developers_of_critical_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, insurers_of_critical_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are protected from the risks of inadequately tested software in life-safety, financial, and other critical systems, as developers cannot use 'beta' labels to disclaim liability. Their safety and financial well-being are prioritized.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, consumers_of_critical_software, beneficiary,
    powerless, immediate, trapped, global).

% Cannot use beta designations to limit liability for software deployed in life-safety, financial, or other critical applications. This increases their development, testing, and compliance costs, and shifts full liability onto them regardless of disclosure.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_developers_of_critical_systems, payer,
    organized, biographical, constrained, global).

% Enforce the categorical unavailability of beta designations for critical systems, ensuring public safety and financial stability. They interpret and apply the doctrine through regulations and legal actions.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Bear higher underwriting risk for critical software due to the inability of developers to limit liability via beta designations. This can lead to higher premiums or more stringent policy requirements for developers.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, insurers_of_critical_systems, payer,
    organized, biographical, constrained, global).

% Analyze the legal, ethical, and economic implications of software liability doctrines, particularly concerning the balance between innovation and public safety in critical domains.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, non-negotiable standard for software liability in critical domains, coordinating expectations around safety, accountability, and the limits of contractual disclaimers.
% TRANSFER_FUNCTION: Transfers the full burden of liability risk from consumers and the public to software developers and their insurers for critical application areas, regardless of a 'beta' label.
% ABSENT_VOICES: Software developers who advocate for greater flexibility in liability allocation for early-stage critical systems, arguing that robust disclosure should always suffice to manage risk.
% DISAPPEARANCE_RATIONALE: If this carve-out vanished, developers would immediately attempt to use beta designations for critical systems, shifting liability and potentially leading to catastrophic failures, requiring new regulatory responses and a complete reorganization of risk allocation in these sectors.
% FOUNDING_PROBLEM: Catastrophic failures, significant financial losses, and public harm caused by inadequately tested software in life-safety and financial systems, where contractual disclaimers proved insufficient to protect the public.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies, consumer advocacy groups, and historical case law consistently attest to the ongoing need for such protections, citing past incidents and the inherent risks of complex software. Independent safety organizations also corroborate the persistent threat.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, ExtMetricName, E),
    domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because the principle that severe harm overrides contractual freedom in critical systems is treated as an irreducible limit, even if its legal codification is human-made. Extractiveness is high (0.78) because it imposes significant costs on developers by preventing liability limitation. Suppression is very high (0.85) as it's legally enforced and overrides contractual agreements, effectively collapsing the option to use beta designations in these contexts. Theater ratio is low (0.10) as the constraint is highly functional, directly preventing liability evasion in critical domains. Accessibility collapse is high (0.90) because the option to use beta for liability limitation is almost completely removed for critical systems. Resistance is moderate (0.45) as developers may lobby, but the ethical and legal weight of public safety limits direct opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of consumers and regulators, this constraint is a necessary, almost natural, safeguard. From the perspective of developers and insurers, it is a significant, costly imposition that limits their contractual freedom and risk management options. The engine's classification will highlight this divergence between the claimed 'mountain' status and the high extraction experienced by payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers of critical software and regulatory bodies are beneficiaries, as they gain protection and enforcement of safety standards. Software developers and their insurers are payers, bearing increased liability and costs. The constraint's directionality is strongly towards protecting the public from inherent risks in critical systems.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the categorical unavailability of beta designation for critical systems a genuine natural law (emerging from inherent risks and ethical imperatives) or a constructed legal constraint that benefits identifiable agents?',
    'Comparative legal analysis across jurisdictions with different legal traditions regarding product liability and risk allocation; philosophical inquiry into the nature of ethical imperatives in technology.',
    'If primarily a natural law, its ''mountain'' classification is robust. If primarily constructed, it functions as a ''snare'' or ''tangled_rope'' for developers, despite its public safety benefits, highlighting a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural ethical limit and human-made legal construct.').

omega_variable(
    definition_of_critical_systems_ambiguity,
    'How precisely and consistently are ''life-safety, financial, or other critical systems'' defined and applied across different regulatory bodies and legal jurisdictions?',
    'Analysis of case law and regulatory guidance; empirical study of classification disputes and their outcomes.',
    'If definitions are vague or inconsistently applied, the constraint''s scope and extractiveness become uncertain, potentially leading to arbitrary enforcement or loopholes. If clear and consistent, the constraint''s application is more predictable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_critical_systems_ambiguity, empirical, 'Ambiguity in the scope and definition of ''critical systems''.').

omega_variable(
    innovation_vs_safety_tradeoff,
    'Does the categorical carve-out for beta designations unduly stifle innovation in critical systems by imposing too high a liability burden on early-stage development?',
    'Economic impact studies on innovation rates in critical software sectors compared to less regulated sectors; analysis of alternative risk-sharing models (e.g., government-backed insurance for early-stage critical tech).',
    'If innovation is significantly stifled without commensurate safety gains, the constraint might be re-evaluated for a ''tangled_rope'' classification, suggesting a need for policy adjustments to balance safety and progress. If not, the current ''mountain'' classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_vs_safety_tradeoff, empirical, 'Trade-off between safety and innovation in critical software development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
