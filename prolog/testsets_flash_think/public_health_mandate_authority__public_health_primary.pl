% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate as Protection of Vulnerable Commons
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primary' reading of
 *   the 'public_health_mandate_authority' kernel. It frames public health
 *   mandates as a necessary obligation to protect the vulnerable commons
 *   (immunocompromised individuals, healthcare infrastructure) through
 *   collective action. From this perspective, individual resistance to
 *   mandates is seen as imposing an externality on the collective, justifying
 *   coercive measures to ensure compliance. The metrics reflect a highly
 *   extractive and suppressive constraint, consistent with a Tangled Rope, as
 *   it coordinates collective action but extracts significantly from those
 *   who resist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.75).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.8).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate as Protection of Vulnerable Commons").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'd4f81d17-599f-4c70-8dda-fd0ef233dadb').
narrative_ontology:cs_kernel_codification('d4f81d17-599f-4c70-8dda-fd0ef233dadb', formalized).
narrative_ontology:cs_authority_grounding('d4f81d17-599f-4c70-8dda-fd0ef233dadb', lineage).
narrative_ontology:cs_interpretation_layer_present('d4f81d17-599f-4c70-8dda-fd0ef233dadb').
narrative_ontology:cs_reading_relation('d4f81d17-599f-4c70-8dda-fd0ef233dadb', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d4f81d17-599f-4c70-8dda-fd0ef233dadb', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('d4f81d17-599f-4c70-8dda-fd0ef233dadb', foundational, collective_wellbeing_priority).
narrative_ontology:cs_axiom_status(collective_wellbeing_priority, holdable).
narrative_ontology:cs_axiom_grounding('d4f81d17-599f-4c70-8dda-fd0ef233dadb', collective_wellbeing_priority, deontological).
narrative_ontology:cs_axiom('d4f81d17-599f-4c70-8dda-fd0ef233dadb', foundational, vulnerable_protection_duty).
narrative_ontology:cs_axiom_status(vulnerable_protection_duty, holdable).
narrative_ontology:cs_axiom_grounding('d4f81d17-599f-4c70-8dda-fd0ef233dadb', vulnerable_protection_duty, deontological).
narrative_ontology:cs_reference_frame('d4f81d17-599f-4c70-8dda-fd0ef233dadb', police_power_doctrine).
narrative_ontology:cs_drift_state('d4f81d17-599f-4c70-8dda-fd0ef233dadb', contemporary_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d4f81d17-599f-4c70-8dda-fd0ef233dadb', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employers_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for setting and enforcing public health mandates (e.g., vaccination, masking) to protect the population. They justify these actions based on scientific consensus and legal precedent, facing political and legal challenges.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from reduced disease transmission due to mandates, as their health is severely threatened by common pathogens. They have limited personal options for protection and rely on collective action.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Benefits from mandates by preventing overwhelming surges in patient numbers, preserving capacity for other medical needs, and protecting healthcare workers. Bears the cost of implementing and enforcing some mandates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system, beneficiary,
    institutional, generational, constrained, national).

% Benefits from overall reduced disease burden, greater social stability, and protection of public services. May experience minor inconveniences or costs from compliance, but generally supports measures that protect collective health.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Bear the direct costs of mandates, such as job loss for non-compliance, exclusion from services, or social stigma. Their resistance is often rooted in strong beliefs about bodily autonomy or distrust of authority, making exit from their position difficult.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    powerless, immediate, identity_locked, local).

% Are often tasked with enforcing mandates (e.g., requiring vaccination for employees, masking for customers). They bear administrative costs and potential legal challenges, but also benefit from a healthier workforce/customer base and reduced liability.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employers_service_providers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, employers_service_providers, agenda_setter).

% Analyze mandates through the lens of individual liberties and constitutional protections. They often represent mandate-resistant individuals in legal challenges, seeking to limit the scope of public health authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, constitutional_rights_advocates, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve collective immunity and reduce disease transmission, thereby protecting vulnerable populations and preventing the overwhelming of healthcare infrastructure, through coordinated public action.
% TRANSFER_FUNCTION: Transfers individual autonomy (e.g., choice over medical interventions, personal conduct) from mandate-resistant individuals to the collective, in exchange for reduced public health risks and preserved healthcare capacity. It also transfers the burden of disease risk from vulnerable populations to those who resist mandates (via compliance costs).
% ABSENT_VOICES: Those who hold an absolute view of bodily autonomy, seeing any non-consensual medical intervention or restriction as a categorical violation, are structurally excluded from the core framing of this constraint. Their concerns are framed as externalities rather than legitimate counter-claims within the public health primary framework.
% DISAPPEARANCE_RATIONALE: If public health mandate authority vanished, vulnerable populations would face significantly higher risks during outbreaks, healthcare systems would be more prone to collapse, and the societal expectation of collective protection against communicable diseases would be fundamentally altered, leading to a reorganization of social norms and individual risk calculations.
% FOUNDING_PROBLEM: Preventing widespread disease, protecting vulnerable populations from communicable diseases, and ensuring the functional capacity of healthcare systems during epidemics and pandemics.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by medical and epidemiological consensus, historical public health outcomes, and international health organizations. While the specific *solutions* are contested, the underlying problem of communicable disease and its societal impact remains.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because mandates impose significant costs on non-compliant individuals, including loss of employment or access to services. Suppression is also high (0.80) due to the active enforcement mechanisms (legal, institutional, social) used to ensure compliance and limit alternatives for those who resist. Theater ratio is low (0.10) because the mandates are understood as direct, functional interventions with clear public health goals, not primarily performative. Accessibility collapse is moderate-high (0.70) as alternatives to compliance are severely limited for many, especially in employment or essential services. Resistance is moderate-high (0.60) reflecting the significant public and legal challenges mandates face.
 *
 * PERSPECTIVAL GAP:
 *   The public health primary reading emphasizes collective benefit and duty, leading to a classification that sees mandates as a necessary, albeit extractive, coordination mechanism. Sibling readings, such as 'bodily_autonomy_primary', would experience this same constraint as a pure Snare, focusing solely on the extraction of individual rights. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, the healthcare system, and the general public are beneficiaries, as they gain from reduced disease burden and preserved system capacity. Mandate-resistant individuals and employers/service providers (who enforce mandates) are payers, bearing the costs of compliance or enforcement. The directionality for mandate-resistant individuals is near the full-target end due to the high costs and limited exit options, often compounded by identity-locked beliefs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_efficacy_ambiguity,
    'To what extent do public health mandates actually achieve their stated goals of reducing transmission and protecting healthcare capacity, especially given varying levels of compliance and pathogen evolution?',
    'Longitudinal epidemiological studies, comparative analysis of mandate effectiveness across jurisdictions, and modeling of counterfactual scenarios.',
    'If mandates are found to be less effective than claimed, the justification for their high extractiveness and suppression weakens, potentially reclassifying the constraint closer to a Snare or Piton. If highly effective, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_efficacy_ambiguity, empirical, 'Uncertainty regarding the empirical effectiveness of public health mandates.').

omega_variable(
    autonomy_collective_good_tradeoff,
    'What is the ethically justifiable balance point between individual bodily autonomy and the collective good of public health, particularly when individual choices impose externalities on vulnerable populations?',
    'Ongoing bioethical debate, legal precedent setting, and societal consensus formation. This is a conceptual and preference-based question without a purely empirical resolution.',
    'A shift in societal consensus towards greater individual autonomy would reduce the perceived legitimacy of mandates, increasing their effective extractiveness and potentially reclassifying them. A shift towards greater collective responsibility would reduce perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_collective_good_tradeoff, conceptual, 'Conceptual ambiguity in balancing individual rights against collective health imperatives.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by mandate-resistant individuals primarily structural (e.g., job loss, exclusion from services) or internalized (e.g., social pressure, identity-based resistance)?',
    'Sociological studies on post-mandate behavior, qualitative interviews with mandate-resistant individuals, and analysis of long-term social cohesion. If resistance persists after structural barriers are removed, internalized suppression is higher.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This could amplify the perceived extractiveness for those individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mandate-resistant individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t2020, public_health_mandate_authority__public_health_primary, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(publ_tr_t2021, public_health_mandate_authority__public_health_primary, theater_ratio, 2021, 0.08).
narrative_ontology:measurement(publ_tr_t2022, public_health_mandate_authority__public_health_primary, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(publ_tr_t2023, public_health_mandate_authority__public_health_primary, theater_ratio, 2023, 0.09).
narrative_ontology:measurement(publ_tr_t2024, public_health_mandate_authority__public_health_primary, theater_ratio, 2024, 0.1).
narrative_ontology:measurement(publ_tr_t2025, public_health_mandate_authority__public_health_primary, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t2020, public_health_mandate_authority__public_health_primary, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(publ_be_t2021, public_health_mandate_authority__public_health_primary, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(publ_be_t2022, public_health_mandate_authority__public_health_primary, base_extractiveness, 2022, 0.75).
narrative_ontology:measurement(publ_be_t2023, public_health_mandate_authority__public_health_primary, base_extractiveness, 2023, 0.72).
narrative_ontology:measurement(publ_be_t2024, public_health_mandate_authority__public_health_primary, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement(publ_be_t2025, public_health_mandate_authority__public_health_primary, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t2020, public_health_mandate_authority__public_health_primary, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(publ_su_t2021, public_health_mandate_authority__public_health_primary, suppression_requirement, 2021, 0.75).
narrative_ontology:measurement(publ_su_t2022, public_health_mandate_authority__public_health_primary, suppression_requirement, 2022, 0.8).
narrative_ontology:measurement(publ_su_t2023, public_health_mandate_authority__public_health_primary, suppression_requirement, 2023, 0.78).
narrative_ontology:measurement(publ_su_t2024, public_health_mandate_authority__public_health_primary, suppression_requirement, 2024, 0.79).
narrative_ontology:measurement(publ_su_t2025, public_health_mandate_authority__public_health_primary, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, healthcare_resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, individual_bodily_autonomy_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
