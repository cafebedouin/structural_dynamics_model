% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers (Functionalist Reading)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'functionalist' reading of the separation
 *   of powers doctrine in constitutional law, which views the framework as
 *   flexible, permitting overlapping authority and intelligible delegation of
 *   principles to administrative agencies. This reading is crucial for
 *   legitimizing the modern regulatory state and is often contrasted with
 *   more rigid 'formalist' interpretations. The claimed type is 'rope'
 *   because it genuinely coordinates complex governance, with relatively low
 *   extraction and suppression, primarily through judicial deference
 *   doctrines. The metrics reflect a slight increase in extractiveness and
 *   suppression over time as the administrative state has grown and faced
 *   more challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.25).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.15).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers (Functionalist Reading)").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '516500d0-217d-4f70-bef3-214596df7187').
narrative_ontology:cs_kernel_codification('516500d0-217d-4f70-bef3-214596df7187', fixed_text).
narrative_ontology:cs_authority_grounding('516500d0-217d-4f70-bef3-214596df7187', lineage).
narrative_ontology:cs_interpretation_layer_present('516500d0-217d-4f70-bef3-214596df7187').
narrative_ontology:cs_reading_relation('516500d0-217d-4f70-bef3-214596df7187', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('516500d0-217d-4f70-bef3-214596df7187', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('516500d0-217d-4f70-bef3-214596df7187', foundational, flexible_governance_necessity).
narrative_ontology:cs_axiom_status(flexible_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('516500d0-217d-4f70-bef3-214596df7187', flexible_governance_necessity, instrumental).
narrative_ontology:cs_axiom('516500d0-217d-4f70-bef3-214596df7187', foundational, intelligible_principle_delegation_legitimacy).
narrative_ontology:cs_axiom_status(intelligible_principle_delegation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('516500d0-217d-4f70-bef3-214596df7187', intelligible_principle_delegation_legitimacy, conventional).
narrative_ontology:cs_reference_frame('516500d0-217d-4f70-bef3-214596df7187', modern_administrative_state_legitimacy).
narrative_ontology:cs_drift_state('516500d0-217d-4f70-bef3-214596df7187', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('516500d0-217d-4f70-bef3-214596df7187', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, regulatory_state_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, administrative_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to exercise delegated authority, implement complex policy, and adapt regulations without constant legislative intervention. Their existence and operational flexibility are legitimized by this reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Benefits from delegating complex policy details to expert agencies, allowing it to focus on broader legislative priorities and avoid micromanagement. This reading permits its current legislative practice.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the ability to oversee and direct the executive branch, including administrative agencies, to implement policy effectively. This reading supports a robust executive role in governance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, beneficiary,
    institutional, generational, constrained, national).

% Interprets and applies the separation of powers doctrine, often through deference doctrines (e.g., Chevron deference) that uphold agency authority. Their rulings shape the practical application of this reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Would argue that this reading undermines the constitutional structure by permitting excessive delegation and blurring distinct powers. Their arguments are often marginalized in current administrative law discourse.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, excluded,
    moderate, generational, identity_locked, national).

% Would contend that this reading allows for independent agencies that infringe on the President's sole executive authority. Their perspective is often at odds with the practical realities of the modern administrative state.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_advocates, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the complex governance needs of a modern state by allowing legislative and executive branches to delegate detailed policy implementation to specialized administrative bodies, ensuring effective and adaptable governance.
% TRANSFER_FUNCTION: Transfers specific policy-making and enforcement authority from Congress and the President to administrative agencies, enabling efficient and expert-driven regulation.
% ABSENT_VOICES: Formalist legal scholars and unitary executive advocates are largely excluded from the mainstream legal and political discourse that upholds this functionalist reading. They would argue for stricter adherence to distinct powers and a more constrained administrative state.
% DISAPPEARANCE_RATIONALE: If this functionalist reading of separation of powers vanished, the entire modern regulatory state would be delegitimized. Agencies would lose their authority, countless regulations would be invalidated, and Congress would be overwhelmed, leading to a profound crisis in governance and a complete reorganization of federal power.
% FOUNDING_PROBLEM: The original constitutional framework, while robust, did not explicitly anticipate the need for a complex administrative state capable of addressing highly technical and evolving policy challenges in areas like environmental protection, financial regulation, and public health.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in administrative law, political scientists studying governance, and practitioners within federal agencies corroborate that the problem of governing a complex society with a static constitutional text remains live. They point to the necessity of administrative flexibility for effective modern governance, a view often contested by those outside the administrative state's direct beneficiaries.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is low because this reading primarily facilitates coordination and efficient governance, with any 'extraction' being the necessary overhead of a complex administrative state, rather than rent-seeking. Suppression (0.15) is also low, as this reading is widely accepted in practice, though it faces ongoing academic and judicial challenges. Theater ratio (0.05) is minimal, indicating that the functions performed are largely genuine. The slight increases in these metrics over the interval reflect the growing size and complexity of the administrative state and the increasing legal challenges it faces.
 *
 * PERSPECTIVAL GAP:
 *   While the functionalist reading is largely accepted by the institutional actors it benefits, those advocating for formalist or unitary executive interpretations experience the constraint as a suppression of their preferred constitutional order. The engine's classification for the 'excluded' seats would reflect this higher perceived suppression and extractiveness, even as the overall constraint computes as a rope for the system's beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies, Congress, and the President are all beneficiaries, as this reading enables their effective functioning in modern governance. The federal judiciary acts as the agenda-setter, interpreting and applying this reading through its rulings. Formalist legal scholars and unitary executive advocates are excluded, as their alternative readings are not currently dominant in practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_vs_formalist_legitimacy,
    'Is the functionalist reading of separation of powers a legitimate evolution of constitutional principles, or a pragmatic deviation from the original intent that has accumulated unconstitutional power?',
    'A definitive Supreme Court ruling explicitly overturning or reaffirming the non-delegation doctrine and the scope of agency power, or a constitutional amendment clarifying the roles of administrative agencies.',
    'If deemed an illegitimate deviation, the entire administrative state would face a constitutional crisis, potentially leading to its dismantling or radical restructuring. If reaffirmed, it would solidify the functionalist approach against formalist challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_vs_formalist_legitimacy, conceptual, 'Ambiguity regarding the constitutional legitimacy of the functionalist approach versus originalist/formalist interpretations.').

omega_variable(
    delegation_intelligible_principle_test,
    'Is the ''intelligible principle'' test for congressional delegation of authority to agencies genuinely constraining, or has it become a rubber stamp for broad delegations?',
    'Empirical analysis of judicial review of agency delegations: a rise in successful challenges to broad delegations would indicate a more constraining test; continued judicial deference would suggest it''s a rubber stamp.',
    'If the test is a rubber stamp, the functionalist reading''s coordination function is weaker, and its potential for extraction (through unchecked agency power) is higher. If genuinely constraining, it reinforces the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_intelligible_principle_test, empirical, 'Whether the ''intelligible principle'' test effectively limits congressional delegation.').

omega_variable(
    functionalist_vs_unitary_executive_tension,
    'To what extent does the functionalist reading''s acceptance of independent agencies conflict with the unitary executive principle, and is this conflict resolvable within the current constitutional framework?',
    'Supreme Court rulings on the appointment and removal powers of the President over independent agency heads. A ruling favoring presidential control would shift the balance towards the unitary executive reading.',
    'If the conflict is deemed irreconcilable and the unitary executive principle prevails, the functionalist reading''s scope for independent agency action would be severely curtailed, potentially reclassifying parts of the administrative state as a snare for agencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_vs_unitary_executive_tension, conceptual, 'Tension between functionalist acceptance of independent agencies and the unitary executive principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__functionalist_reading, theater_ratio, 1930, 0.02).
narrative_ontology:measurement(sepa_tr_t1960, separation_of_powers_text__functionalist_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__functionalist_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__functionalist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__functionalist_reading, base_extractiveness, 1930, 0.15).
narrative_ontology:measurement(sepa_be_t1960, separation_of_powers_text__functionalist_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__functionalist_reading, base_extractiveness, 1990, 0.23).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__functionalist_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__functionalist_reading, suppression_requirement, 1930, 0.1).
narrative_ontology:measurement(sepa_su_t1960, separation_of_powers_text__functionalist_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__functionalist_reading, suppression_requirement, 1990, 0.14).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__functionalist_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_procedure_act).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'separation_of_powers_text' kernel. It is linked to other readings (formalist_reading, unitary_executive_reading) that offer alternative interpretations of the same constitutional text, each with distinct structural implications for governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
