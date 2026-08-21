% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'regulatory takings' reading of the Fifth
 *   Amendment's Takings Clause, primarily established by the Supreme Court's
 *   decision in Penn Central Transportation Co. v. City of New York (1978).
 *   It holds that regulations that diminish economic value 'too far' can
 *   constitute a taking requiring compensation, even without physical
 *   appropriation. This reading introduces an ad hoc, fact-intensive
 *   balancing test, creating a mechanism for property owners to challenge
 *   regulations and expanding the scope of compensable takings beyond direct
 *   physical seizures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.75).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '8c6beaa7-bd8d-49ea-938f-0c02ccd640e0').
narrative_ontology:cs_kernel_codification('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', fixed_text).
narrative_ontology:cs_authority_grounding('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', lineage).
narrative_ontology:cs_interpretation_layer_present('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0').
narrative_ontology:cs_reading_relation('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', foundational, economic_value_is_property_interest).
narrative_ontology:cs_axiom_status(economic_value_is_property_interest, holdable).
narrative_ontology:cs_axiom_grounding('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', economic_value_is_property_interest, conventional).
narrative_ontology:cs_axiom('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', foundational, excessive_regulation_is_taking).
narrative_ontology:cs_axiom_status(excessive_regulation_is_taking, holdable).
narrative_ontology:cs_axiom_grounding('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', excessive_regulation_is_taking, conventional).
narrative_ontology:cs_reference_frame('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', penn_central_balancing_test).
narrative_ontology:cs_drift_state('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8c6beaa7-bd8d-49ea-938f-0c02ccd640e0', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, developers_and_investors).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_bodies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek compensation when government regulations significantly diminish the economic value of their property, even without physical appropriation. They bear the initial cost of regulation but can recover through litigation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, national).

% Implement regulations for public welfare (e.g., environmental protection, zoning). They face the risk of takings claims, which can result in costly compensation payments or regulatory chill, limiting their ability to act.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_bodies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, regulatory_bodies, agenda_setter).

% Promote regulations for environmental protection, historic preservation, and other public goods. They bear the cost of regulatory chill and the diversion of public funds to takings compensation, which could otherwise fund public projects.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates, payer,
    moderate, generational, constrained, national).

% Interpret and apply the regulatory takings doctrine, balancing public interest with private property rights. Their decisions shape the 'too far' boundary, influencing regulatory behavior and property owner expectations.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the protection against excessive regulation, which reduces risk for their projects. They can leverage takings claims to challenge regulations that impede development or reduce property values.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, developers_and_investors, beneficiary,
    powerful, biographical, mobile, national).

% Indirectly bears the cost of takings compensation through taxes and reduced public services, and suffers from the chilling effect on regulations that protect public goods like clean air and water.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, general_public, payer,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the government's power to regulate for public welfare with the constitutional protection of private property rights, ensuring that individuals are not forced to bear public burdens alone.
% TRANSFER_FUNCTION: Transfers financial compensation from public treasuries (funded by taxpayers) to private property owners when regulations are deemed to diminish property value 'too far', or transfers regulatory capacity from public bodies to private interests by chilling regulation.
% ABSENT_VOICES: Future generations, whose environmental and social interests may be compromised by regulatory chill or the diversion of public funds to takings compensation, are not directly represented in the ad hoc balancing test.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, governments would have significantly more freedom to regulate property without fear of compensation claims, potentially leading to more robust environmental, zoning, and land-use regulations. Property values would adjust, and the balance of power between public and private interests would fundamentally shift.
% FOUNDING_PROBLEM: To prevent government from effectively confiscating private property through regulation without providing just compensation, thereby ensuring that the costs of public benefits are broadly shared rather than concentrated on a few property owners.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, property rights organizations, and historical case law (e.g., Pennsylvania Coal Co. v. Mahon, Penn Central Transportation Co. v. City of New York) corroborate the ongoing concern about government overreach and the need for property protection. Public interest groups, however, contest the extent to which the problem remains 'live' in its original form, arguing the doctrine now primarily serves to chill beneficial regulation.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate public welfare regulation with private property rights (a coordination function), but it also involves significant asymmetric extraction. Property owners benefit from the potential for compensation and regulatory chill, while regulatory bodies and the public bear the costs of compensation and reduced regulatory capacity. Active enforcement is required through litigation and judicial review. Extractiveness is high due to the transfer of public funds/regulatory capacity to private hands. Suppression is also high, as the threat of takings claims can suppress beneficial public regulations. Theater ratio is low, as the legal process is genuinely functional, though its outcomes are contested.
 *
 * PERSPECTIVAL GAP:
 *   Property owners view this doctrine as essential protection against government overreach, ensuring fairness. Regulatory bodies and public interest advocates often view it as an impediment to necessary public welfare regulations, leading to 'regulatory chill' and diverting public resources. The courts, as agenda-setters, navigate this tension, but their decisions are often seen differently by the affected parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are beneficiaries (low d) as they gain protection and potential compensation. Regulatory bodies and public interest advocates are targets (high d) as they bear the costs and face constraints on their actions. The general public is also a target, indirectly paying for compensation and suffering from regulatory chill.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    too_far_ambiguity,
    'What specific criteria define ''too far'' in diminishing economic value, and how consistently are they applied across jurisdictions and cases?',
    'Empirical analysis of judicial decisions, identifying patterns in the application of the Penn Central factors, or legislative efforts to codify clearer standards for regulatory takings.',
    'Clearer criteria would reduce uncertainty for both regulators and property owners, potentially lowering litigation costs and making the constraint more predictable. Ambiguity amplifies extraction by favoring those with resources to litigate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(too_far_ambiguity, empirical, 'Ambiguity of the ''too far'' threshold in regulatory takings.').

omega_variable(
    regulatory_chill_quantification,
    'To what extent does the threat of regulatory takings claims actually deter or weaken beneficial public welfare regulations?',
    'Comparative studies of regulatory outcomes in jurisdictions with different takings jurisprudence, or surveys of regulatory bodies regarding their decision-making processes.',
    'If regulatory chill is substantial, the doctrine''s effective suppression is higher than directly measured, leading to under-regulation of public goods. If minimal, the doctrine''s costs are primarily financial compensation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_quantification, empirical, 'Quantifying the chilling effect of regulatory takings claims on public welfare regulations.').

omega_variable(
    value_diminution_vs_physical_taking,
    'Is the conceptual distinction between physical appropriation and severe economic value diminution sufficiently robust to justify different legal treatment, or are they functionally equivalent in their impact on property rights?',
    'Philosophical and legal conceptual analysis, or a shift in judicial doctrine that explicitly equates or further differentiates the two forms of taking.',
    'If functionally equivalent, the ''physical_appropriation_reading'' is conceptually foreclosed, and the ''regulatory_takings_reading'' becomes the dominant interpretation. If distinct, the current framework''s differentiation is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(value_diminution_vs_physical_taking, conceptual, 'Conceptual robustness of value diminution vs. physical taking distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.6).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, zoning_laws).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, historic_preservation_ordinances).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, land_use_planning).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'takings_clause_boundary' kernel, focusing on regulations that diminish economic value. It expands the scope beyond physical appropriation and introduces an ad hoc balancing test, distinguishing it from the 'physical_appropriation_reading' and 'categorical_takings_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
