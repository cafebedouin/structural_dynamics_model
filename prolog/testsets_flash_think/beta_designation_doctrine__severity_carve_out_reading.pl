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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint is the `severity_carve_out_reading` of the
 *   `beta_designation_doctrine` kernel. It asserts that beta designation is
 *   categorically unavailable for life-safety, financial, or other critical
 *   systems, regardless of testing status or disclosure. This reading posits
 *   an inherent limit on contractual liability disclaimers in high-stakes
 *   domains. Sibling readings include `expansive_shield_reading` (beta as
 *   comprehensive waiver) and `narrow_warning_reading` (beta as time-bounded
 *   testing disclosure).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.15).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.8).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Carve-Out for Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).
domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'ad492ea1-b389-404f-9205-d103bb4593f5').
narrative_ontology:cs_kernel_codification('ad492ea1-b389-404f-9205-d103bb4593f5', formalized).
narrative_ontology:cs_authority_grounding('ad492ea1-b389-404f-9205-d103bb4593f5', lineage).
narrative_ontology:cs_interpretation_layer_present('ad492ea1-b389-404f-9205-d103bb4593f5').
narrative_ontology:cs_reading_relation('ad492ea1-b389-404f-9205-d103bb4593f5', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('ad492ea1-b389-404f-9205-d103bb4593f5', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('ad492ea1-b389-404f-9205-d103bb4593f5', foundational, inherent_risk_cannot_be_contractually_waived).
narrative_ontology:cs_axiom_status(inherent_risk_cannot_be_contractually_waived, holdable).
narrative_ontology:cs_axiom_grounding('ad492ea1-b389-404f-9205-d103bb4593f5', inherent_risk_cannot_be_contractually_waived, deontological).
narrative_ontology:cs_axiom('ad492ea1-b389-404f-9205-d103bb4593f5', foundational, public_safety_trumps_commercial_disclaimer).
narrative_ontology:cs_axiom_status(public_safety_trumps_commercial_disclaimer, holdable).
narrative_ontology:cs_axiom_grounding('ad492ea1-b389-404f-9205-d103bb4593f5', public_safety_trumps_commercial_disclaimer, deontological).
narrative_ontology:cs_reference_frame('ad492ea1-b389-404f-9205-d103bb4593f5', inherent_risk_liability_principle).
narrative_ontology:cs_drift_state('ad492ea1-b389-404f-9205-d103bb4593f5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ad492ea1-b389-404f-9205-d103bb4593f5', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, consumers_of_critical_software).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_developers_of_critical_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop software for life-safety, financial, or other critical infrastructure. They bear the cost of increased liability and cannot use 'beta' designations to disclaim responsibility, forcing higher development and testing standards. Their exit options are to comply with the higher standards or exit these high-stakes markets.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_developers_of_critical_systems, payer,
    powerful, biographical, constrained, global).

% Rely on critical software for essential functions (e.g., medical devices, financial transactions, infrastructure control). They benefit from enhanced safety and accountability, as developers cannot externalize risk through beta labels. Their options are limited by the available software and its inherent risks.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, consumers_of_critical_software, beneficiary,
    powerless, generational, constrained, global).

% Interpret and enforce laws and regulations governing software liability, particularly in critical sectors. They benefit from a clear legal framework that prioritizes public safety over contractual disclaimers, enabling them to hold developers accountable. Their role is to ensure compliance and protect the public.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the evolution and application of software liability doctrines, including the 'beta' designation. They provide commentary and critique, influencing future legal interpretations and policy debates, but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, non-negotiable standard for liability in critical software domains, coordinating expectations between developers, users, and regulators regarding safety and accountability.
% TRANSFER_FUNCTION: Transfers the full burden of liability risk from consumers to developers in critical software systems, preventing developers from externalizing safety costs or disclaiming responsibility through 'beta' labels.
% ABSENT_VOICES: Software developers of non-critical systems might argue for a more flexible application of beta designation, fearing that the carve-out could expand to less critical contexts, increasing their liability burden unnecessarily.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, developers would immediately begin applying 'beta' designations to critical systems, shifting liability and increasing unmitigated risk for users. This would lead to a breakdown of trust, potential catastrophic failures in infrastructure, and a fundamental reorganization of the software liability landscape.
% FOUNDING_PROBLEM: Catastrophic failures, unmitigated risks, and a lack of accountability in critical software systems due to developers disclaiming liability through 'beta' labels, leaving users vulnerable to harm.
% FOUNDING_PROBLEM_CORROBORATION: Consumer advocacy groups, public safety regulators, and legal precedents from outside the software industry consistently corroborate the ongoing need for robust liability and accountability in critical infrastructure and life-safety domains, supporting the view that the founding problem remains highly relevant.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

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
 *   The constraint is claimed as a Mountain because it represents a fundamental, almost natural, limit on liability in domains where inherent risk to life or critical functions overrides contractual agreements. Its extractiveness is low (0.15) because the doctrine itself prevents developers from extracting by externalizing risk; instead, it imposes costs on them. Suppression is high (0.80) as it categorically removes the option of using beta designations in these contexts. Accessibility collapse is high for developers seeking to use beta in these specific domains. Resistance is moderate from developers who prefer more flexibility. Theater ratio is low (0.10) as the doctrine's function is direct and legal, not performative. The measurement series reflect a stable, consistently applied doctrine over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of consumers and regulators, this doctrine is a necessary safeguard, a 'natural law' of accountability in critical systems. From the perspective of developers, it is a burdensome legal imposition that limits their flexibility and increases costs. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a protective Rope or Mountain, and developers experiencing it as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers of critical software and regulatory bodies are beneficiaries, as the doctrine protects them from harm and ensures accountability (low directionality). Software developers of critical systems are targets, as they bear the increased liability and cannot use beta designations to mitigate risk (high directionality). Legal scholars act as observers, analyzing the doctrine's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the `severity_carve_out_reading` of the `beta_designation_doctrine` kernel?',
    'Analysis of legal precedents and regulatory guidance to confirm the specific interpretation and application of beta designation in critical systems, distinguishing it from broader or narrower interpretations.',
    'Misidentification would lead to incorrect classification of the constraint''s structural properties and its relationship to other readings of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific kernel reading being instantiated.').

omega_variable(
    natural_limit_vs_legal_construct,
    'Is the categorical unavailability of beta designation for critical systems a genuine natural limit arising from the inherent nature of risk, or a constructed legal doctrine that benefits identifiable agents?',
    'Philosophical and legal analysis of the concept of ''inherent risk'' and its relationship to legal enforceability, alongside an examination of the historical development of liability law in critical sectors.',
    'If primarily a legal construct, the ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'' from the developer''s seat, highlighting the constructed nature of the liability transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_legal_construct, conceptual, 'Ambiguity between natural law and constructed legal doctrine for liability in critical systems.').

omega_variable(
    scope_creep_risk,
    'Is there a risk that the ''severity carve-out'' principle will expand beyond life-safety and financial systems to other domains, increasing liability for developers of less critical software?',
    'Monitoring of legislative proposals, regulatory guidance, and judicial decisions over time to detect any broadening of the carve-out''s application.',
    'If scope creep occurs, the constraint''s impact on developers would increase, potentially shifting its classification towards a more extractive type for a wider range of software developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_risk, empirical, 'Potential for the carve-out to expand to less critical software domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(beta_tr_t2015, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(beta_tr_t2030, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2005, 0.13).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(beta_be_t2015, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2025, 0.15).
narrative_ontology:measurement(beta_be_t2030, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2030, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2005, 0.77).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(beta_su_t2015, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement(beta_su_t2030, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
