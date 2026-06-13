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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Unavailable for Critical Systems (Severity Carve-Out Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint asserts that software designated as 'beta' cannot be used
 *   to disclaim liability for systems where failure poses a risk to life,
 *   financial stability, or other critical functions. This is the 'severity
 *   carve-out' reading of the broader 'beta designation doctrine' kernel. It
 *   posits that the inherent risks of certain applications override any
 *   contractual attempt to label them as experimental for liability purposes.
 *   The constraint is treated as a Mountain because the severity of potential
 *   harm creates an irreducible limit on liability waivers, regardless of
 *   explicit enforcement.
 *
 * KEY AGENTS:
 *   - critical_system_users: Primary beneficiary (powerless/immediate) — protected from harm
 *   - software_developers: Primary target (powerful/biographical) — cannot disclaim liability
 *   - public_safety_regulators: Agenda setter/Beneficiary (institutional/generational) — enforce and benefit from safety standards
 *   - legal_scholars: Observer (analytical/generational) — analyze the doctrine's scope and implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.15).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.05).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Unavailable for Critical Systems (Severity Carve-Out Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '63e265da-fcd5-47b8-be13-e7c987d1d40b').
narrative_ontology:cs_kernel_codification('63e265da-fcd5-47b8-be13-e7c987d1d40b', formalized).
narrative_ontology:cs_authority_grounding('63e265da-fcd5-47b8-be13-e7c987d1d40b', lineage).
narrative_ontology:cs_interpretation_layer_present('63e265da-fcd5-47b8-be13-e7c987d1d40b').
narrative_ontology:cs_reading_relation('63e265da-fcd5-47b8-be13-e7c987d1d40b', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('63e265da-fcd5-47b8-be13-e7c987d1d40b', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('63e265da-fcd5-47b8-be13-e7c987d1d40b', foundational, harm_severity_overrides_contractual_intent).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_intent, holdable).
narrative_ontology:cs_axiom_grounding('63e265da-fcd5-47b8-be13-e7c987d1d40b', harm_severity_overrides_contractual_intent, deontological).
narrative_ontology:cs_axiom('63e265da-fcd5-47b8-be13-e7c987d1d40b', foundational, duty_of_care_in_critical_domains).
narrative_ontology:cs_axiom_status(duty_of_care_in_critical_domains, holdable).
narrative_ontology:cs_axiom_grounding('63e265da-fcd5-47b8-be13-e7c987d1d40b', duty_of_care_in_critical_domains, deontological).
narrative_ontology:cs_reference_frame('63e265da-fcd5-47b8-be13-e7c987d1d40b', inherent_safety_imperative).
narrative_ontology:cs_drift_state('63e265da-fcd5-47b8-be13-e7c987d1d40b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('63e265da-fcd5-47b8-be13-e7c987d1d40b', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, public_safety_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_developers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, safety_first_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, duty_of_care_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or organizations whose lives, health, or financial stability depend on the reliable operation of critical software systems. They are protected from developers attempting to disclaim liability for failures in these systems.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_users, beneficiary,
    powerless, immediate, trapped, global).

% Develop software for critical systems (e.g., medical devices, financial trading platforms, industrial control systems). They cannot use 'beta' designations to avoid full liability for defects, requiring more rigorous testing and compliance processes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_developers, payer,
    powerful, biographical, constrained, global).

% Government agencies responsible for setting and enforcing safety standards for critical infrastructure and consumer protection. They uphold the principle that certain systems cannot be treated as experimental for liability purposes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, public_safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Academics and legal experts who analyze the evolution of software liability law, including the interpretation and application of beta designation doctrines. They provide commentary and influence judicial and legislative thinking.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, non-negotiable baseline for safety and reliability in critical software domains, preventing a race to the bottom in liability standards that would undermine public trust and safety.
% TRANSFER_FUNCTION: Transfers the burden of ensuring safety and reliability from potential victims (users) to the developers of critical systems, by preventing liability disclaimers.
% ABSENT_VOICES: Developers who wish to apply 'beta' disclaimers to critical systems to reduce their liability or accelerate deployment would object. Their voices are largely marginalized by the societal consensus on safety in these domains.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, developers could label critical systems as 'beta' to avoid liability, leading to a rapid degradation of safety standards, increased incidents, and a collapse of public trust in essential software infrastructure. The legal and regulatory landscape would have to fundamentally reorganize to address the resulting chaos.
% FOUNDING_PROBLEM: The potential for catastrophic harm from software failures in life-safety, financial, and critical infrastructure systems, coupled with developers' desire to limit liability for experimental software.
% FOUNDING_PROBLEM_CORROBORATION: Public safety regulators, consumer advocacy groups, and independent engineering bodies consistently attest that the problem of ensuring safety in critical systems is ongoing and that attempts to circumvent liability remain a threat. This corroboration comes from outside the direct beneficiaries (users) and payers (developers).
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

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
 *   Extractiveness is low (0.15) because the constraint primarily prevents harm rather than extracting rents; it imposes a cost on developers but for a clear public good. Suppression is low (0.05) as the constraint is largely self-enforcing due to the catastrophic consequences of failure, rather than requiring active coercion. Theater ratio is zero as there's no performative aspect; the constraint is a direct reflection of the underlying risk. Accessibility collapse is high (0.9) because for critical systems, there are no legitimate alternatives to robust liability and safety standards. Resistance is low (0.05) because the principle of protecting life and critical infrastructure is widely accepted, even by developers who bear the cost.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of critical system users and public safety regulators, this constraint is a fundamental protection (Mountain). From the perspective of some software developers, it might be seen as an onerous burden (Tangled Rope), limiting their ability to innovate or test in production environments. However, the inherent severity of potential harm means that this 'burden' is an irreducible feature of the domain, not an arbitrary extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical system users are full beneficiaries (d=0.0) as they are directly protected from harm. Public safety regulators are also beneficiaries (d=0.1) as the constraint aligns with their mandate. Software developers are targets (d=0.9) as they bear the cost of increased liability and development rigor. The constraint subsidizes safety for users by extracting higher development costs and liability from developers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_policy_choice,
    'Is the unavailability of beta designation for critical systems a natural consequence of harm severity (a Mountain), or a policy choice enforced by legal precedent (a Snare/Tangled Rope)?',
    'Analysis of legal systems across jurisdictions: if the carve-out is universally recognized regardless of specific legal frameworks, it leans towards natural law; if it varies significantly, it suggests policy choice.',
    'If a policy choice, the constraint''s extractiveness and suppression would be higher, reflecting the active enforcement and potential for rent-seeking by those who benefit from the carve-out (e.g., established vendors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Ambiguity between inherent safety requirements and legal policy.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''beta_designation_doctrine'' kernel. How would the classification change if the ''expansive_shield_reading'' or ''narrow_warning_reading'' were adopted?',
    'Adoption of a different reading by a court or regulatory body.',
    'If ''expansive_shield_reading'' were adopted, beta designation would become a Snare for users, allowing developers to disclaim liability for critical systems. If ''narrow_warning_reading'' were adopted, it would be a Rope, providing clear disclosure without absolving core liability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(beta_tr_t30, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 30, 0.0).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(beta_be_t30, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(beta_su_t30, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, software_product_liability_standards).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_cybersecurity_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'beta_designation_doctrine' kernel. The other readings are 'expansive_shield_reading' and 'narrow_warning_reading', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
