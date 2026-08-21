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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Doctrine: Severity Carve-Out Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents the 'severity carve-out' reading of the beta
 *   designation doctrine, asserting that software for life-safety, financial,
 *   or other critical systems cannot use a 'beta' label to limit liability,
 *   regardless of testing status or disclosure. This reading prioritizes
 *   public safety over developer flexibility, effectively imposing a higher
 *   standard of care in high-stakes domains. It is a specific interpretation
 *   of a broader legal kernel concerning software liability.
 *
 * KEY AGENTS:
 *   - software_developers_critical_systems: Payer (powerful/constrained)
 *   - critical_system_users: Beneficiary (powerless/trapped)
 *   - regulatory_bodies: Agenda Setter (institutional/analytical)
 *   - insurers_critical_systems: Payer (organized/constrained)
 *   - legal_scholars_consumer_protection: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.65).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.78).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Doctrine: Severity Carve-Out Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '8a561bc9-f721-476c-bc1e-19af93c80fe4').
narrative_ontology:cs_kernel_codification('8a561bc9-f721-476c-bc1e-19af93c80fe4', formalized).
narrative_ontology:cs_authority_grounding('8a561bc9-f721-476c-bc1e-19af93c80fe4', lineage).
narrative_ontology:cs_interpretation_layer_present('8a561bc9-f721-476c-bc1e-19af93c80fe4').
narrative_ontology:cs_reading_relation('8a561bc9-f721-476c-bc1e-19af93c80fe4', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a561bc9-f721-476c-bc1e-19af93c80fe4', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('8a561bc9-f721-476c-bc1e-19af93c80fe4', foundational, harm_severity_overrides_contractual_waiver).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_waiver, holdable).
narrative_ontology:cs_axiom_grounding('8a561bc9-f721-476c-bc1e-19af93c80fe4', harm_severity_overrides_contractual_waiver, deontological).
narrative_ontology:cs_axiom('8a561bc9-f721-476c-bc1e-19af93c80fe4', foundational, public_safety_is_non_negotiable).
narrative_ontology:cs_axiom_status(public_safety_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('8a561bc9-f721-476c-bc1e-19af93c80fe4', public_safety_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('8a561bc9-f721-476c-bc1e-19af93c80fe4', strict_product_liability_for_critical_systems).
narrative_ontology:cs_drift_state('8a561bc9-f721-476c-bc1e-19af93c80fe4', contemporary_software_development_practices, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8a561bc9-f721-476c-bc1e-19af93c80fe4', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_developers_critical_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop software for life-safety, financial, and other critical infrastructure. This reading prevents them from using 'beta' status to limit liability, forcing them to meet higher safety and reliability standards from the outset, increasing development and testing costs. Exit means abandoning critical system markets.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_developers_critical_systems, payer,
    powerful, biographical, constrained, global).

% Rely on life-safety, financial, and other critical systems. This reading protects them from the risks of unproven software in high-stakes contexts by ensuring developers bear full liability, even if they label their software 'beta'. Their safety is prioritized over developer flexibility.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_users, beneficiary,
    powerless, immediate, trapped, national).

% Enforce safety and liability standards for critical systems. This reading aligns with their mandate to protect public welfare, allowing them to reject 'beta' as a liability shield in high-risk domains. They actively interpret and apply this doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Provide liability coverage for critical system software. This reading increases their exposure by removing a potential liability limitation for developers, leading to higher premiums or more stringent underwriting requirements. They bear increased risk without a corresponding increase in control.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems, payer,
    organized, biographical, constrained, global).

% Analyze the implications of software liability doctrines for consumer protection. They view this reading as a necessary evolution of product liability law to address the unique risks of software in critical applications, advocating for its broader adoption.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_scholars_consumer_protection, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of liability and risk in critical software development, ensuring that the highest standards of safety and reliability are met by preventing developers from externalizing risk onto users via 'beta' labels.
% TRANSFER_FUNCTION: Transfers the full burden of liability for critical system software from developers (and potentially users) to developers, regardless of 'beta' designation, thereby increasing development costs and insurance premiums for developers.
% ABSENT_VOICES: Advocates for rapid innovation and reduced regulatory burden in software development, who would argue that this carve-out stifles progress even in critical domains, are largely excluded from the legal and regulatory discourse that shapes this doctrine.
% DISAPPEARANCE_RATIONALE: If this carve-out vanished, developers of critical systems would immediately begin labeling software 'beta' to limit liability, shifting risk to users and insurers. This would lead to a rapid degradation of safety standards in critical infrastructure, forcing regulatory bodies to re-intervene or users to bear catastrophic risks.
% FOUNDING_PROBLEM: The potential for catastrophic harm from software failures in life-safety, financial, and other critical systems, coupled with developers attempting to use 'beta' designations to evade responsibility for defects.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and consumer protection advocates attest that the problem of critical system software risk is live and ongoing, citing numerous incidents of software-related failures with severe consequences. Independent legal analysis supports the necessity of such carve-outs to maintain public safety.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the increased development costs and liability burden placed on developers of critical systems. Suppression (0.78) is high because the legal and regulatory framework actively prevents developers from using 'beta' as a shield, with severe penalties for non-compliance. The theater ratio (0.20) is relatively low, indicating that the enforcement of this carve-out is largely functional, focused on genuine risk mitigation rather than performative compliance. Accessibility collapse (0.70) is high as the option to release 'beta' software with limited liability is largely removed in these domains. Resistance (0.45) is moderate, as developers lobby against such restrictions but face strong public and regulatory pressure.
 *
 * PERSPECTIVAL GAP:
 *   Developers of critical systems experience this as a highly extractive and suppressive constraint, limiting their flexibility and increasing costs. Users and regulatory bodies, however, perceive it as a necessary and beneficial coordination mechanism that ensures safety and accountability in high-risk environments. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers in critical systems are direct targets (high d) as they bear the increased liability. Critical system users are beneficiaries (low d) as their safety is enhanced. Regulatory bodies are agenda setters and beneficiaries (low d) as the constraint aligns with their public protection mandate. Insurers are payers (high d) due to increased risk exposure. Legal scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying problem of critical system software risk remains live. Instead, the analysis focuses on whether the 'beta' designation itself has become a theatrical cover for extraction, which this reading actively prevents in high-stakes domains. The constraint's function is to prevent the mislabeling of extraction (developer risk externalization) as coordination (beta testing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_critical_systems,
    'What constitutes a ''critical system'' for the purpose of this carve-out, and how is this boundary adjudicated?',
    'Case law development and regulatory guidance that provides clear definitions and precedents for ''life-safety,'' ''financial,'' and other ''critical'' designations.',
    'A narrow definition would reduce the scope of the carve-out, increasing developer flexibility but potentially exposing more users to risk. A broad definition would expand protection but increase developer burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_critical_systems, conceptual, 'Ambiguity in defining the scope of ''critical systems'' to which the carve-out applies.').

omega_variable(
    effectiveness_of_liability_as_incentive,
    'Does increased developer liability genuinely lead to higher software quality and safety in critical systems, or does it primarily stifle innovation?',
    'Empirical studies comparing software defect rates and innovation metrics in jurisdictions with and without such liability carve-outs for critical systems.',
    'If liability proves ineffective or counterproductive, the justification for this constraint weakens, potentially leading to its re-evaluation. If effective, it reinforces the constraint''s protective function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_liability_as_incentive, empirical, 'Whether increased liability effectively incentivizes safety or merely impedes development.').

omega_variable(
    reading_relationship_to_expansive_shield,
    'Does this ''severity carve-out'' reading logically foreclose the ''expansive shield'' reading, or do they merely coexist as competing interpretations?',
    'Analysis of legal precedent: if a court explicitly rules that harm severity overrides contractual beta waivers, it forecloses the expansive shield. If courts merely apply the carve-out without invalidating the general principle of beta as a shield, they coexist.',
    'If foreclosed, the expansive shield reading loses its structural validity within this legal framework. If coexisting, the conflict remains a live legal contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relationship_to_expansive_shield, conceptual, 'The logical relationship between the severity carve-out and the expansive shield readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, software_product_liability_standards).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_cybersecurity_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'beta_designation_doctrine' kernel. It focuses on the carve-out for critical systems, while sibling readings address the general scope of beta liability waivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
