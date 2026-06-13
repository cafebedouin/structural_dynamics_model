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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Reading of Separation of Powers
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'functionalist' reading of the separation
 *   of powers doctrine in constitutional law, which views the framework as
 *   flexible, permitting overlapping authority and intelligible delegation of
 *   principles to administrative agencies. This reading is central to the
 *   legitimacy of the modern regulatory state. It is one reading of the
 *   broader 'separation_of_powers_text' kernel, alongside the
 *   'formalist_reading' and 'unitary_executive_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.25).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.3).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Reading of Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '5216dbcc-6684-42b1-97c0-a53204f127c9').
narrative_ontology:cs_kernel_codification('5216dbcc-6684-42b1-97c0-a53204f127c9', fixed_text).
narrative_ontology:cs_authority_grounding('5216dbcc-6684-42b1-97c0-a53204f127c9', lineage).
narrative_ontology:cs_interpretation_layer_present('5216dbcc-6684-42b1-97c0-a53204f127c9').
narrative_ontology:cs_reading_relation('5216dbcc-6684-42b1-97c0-a53204f127c9', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5216dbcc-6684-42b1-97c0-a53204f127c9', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('5216dbcc-6684-42b1-97c0-a53204f127c9', foundational, flexible_governance_necessity).
narrative_ontology:cs_axiom_status(flexible_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5216dbcc-6684-42b1-97c0-a53204f127c9', flexible_governance_necessity, instrumental).
narrative_ontology:cs_axiom('5216dbcc-6684-42b1-97c0-a53204f127c9', foundational, intelligible_principle_delegation_legitimate).
narrative_ontology:cs_axiom_status(intelligible_principle_delegation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5216dbcc-6684-42b1-97c0-a53204f127c9', intelligible_principle_delegation_legitimate, conventional).
narrative_ontology:cs_reference_frame('5216dbcc-6684-42b1-97c0-a53204f127c9', modern_administrative_state_legitimacy).
narrative_ontology:cs_drift_state('5216dbcc-6684-42b1-97c0-a53204f127c9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5216dbcc-6684-42b1-97c0-a53204f127c9', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legitimized by this reading, agencies can exercise delegated authority to implement complex policy, providing expertise and flexibility. Their existence and operational scope depend on this interpretation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the ability to delegate complex legislative tasks to agencies, allowing it to focus on broader policy goals and avoid micromanagement. This reading preserves its flexibility in governance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, mobile, national).

% Benefits from a flexible executive branch structure, allowing for effective administration and policy implementation through agencies, even those with some independence. This reading supports a robust executive capacity.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, beneficiary,
    institutional, generational, mobile, national).

% Interprets and enforces the separation of powers doctrine, often deferring to agency expertise under this reading (e.g., Chevron deference). Its rulings shape the boundaries of delegated authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocate for a strict separation of powers, arguing against broad delegation to agencies. Their views are often marginalized in mainstream administrative law, which largely operates under a functionalist framework.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, excluded,
    moderate, generational, constrained, national).

% Argue that all executive power must vest in the President, challenging the legitimacy of independent agencies. This functionalist reading directly contradicts their core principle.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the complex governance of a modern state by allowing legislative and executive functions to be shared and delegated to specialized administrative bodies, ensuring effective policy implementation and adaptation.
% TRANSFER_FUNCTION: Transfers specific policy-making and enforcement authority from Congress and the President to administrative agencies, enabling efficient governance in areas requiring technical expertise and continuous adaptation.
% ABSENT_VOICES: Formalist legal scholars and unitary executive advocates are largely excluded from the operational consensus, as their strict interpretations would dismantle the current administrative state. They would argue for a return to rigid departmentalization and direct presidential control.
% DISAPPEARANCE_RATIONALE: If this functionalist reading vanished, the entire administrative state would be delegitimized. Agencies would lose their authority to regulate, implement, and enforce, leading to a collapse of modern governance and a constitutional crisis as legislative and executive powers would be forced into rigid, unworkable silos.
% FOUNDING_PROBLEM: The original constitutional text provided a framework for separated powers, but the increasing complexity of governance in a modern industrial society required a more flexible approach to allow for effective administration and expert policy implementation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and practitioners widely corroborate that the functionalist reading addresses the ongoing challenge of governing a complex society, enabling the regulatory state to function effectively. This view is supported by decades of judicial precedent and administrative practice, not just by the beneficiaries of the system.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).

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
 *   The functionalist reading is classified as a Rope because it genuinely coordinates complex governance, allowing for effective administration. Its extractiveness (0.25) is relatively low, representing the necessary overhead of a complex administrative state rather than pure rent-seeking. Suppression (0.3) is moderate, reflecting the need to actively defend this interpretation against formalist challenges, but it doesn't suppress alternatives in a coercive way. Theater ratio (0.1) is low, as the administrative state's functions are largely genuine and not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of administrative agencies, Congress, and the President, this reading is a highly effective coordination mechanism. From the perspective of formalist or unitary executive advocates, it represents a deviation from constitutional principles and an overreach of governmental power. The engine's classification reflects the operational reality under the dominant functionalist interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies, Congress, and the President are all beneficiaries, as this reading legitimizes their operational flexibility and capacity to govern. The federal judiciary acts as the agenda-setter, interpreting and upholding this reading. Formalist legal scholars and unitary executive advocates are 'excluded' as their alternative readings are not currently dominant in practice, though they continue to voice resistance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_legitimacy_ambiguity,
    'Is the functionalist reading a genuine interpretation of constitutional principles, or a pragmatic adaptation that has accumulated legitimacy through necessity and precedent, effectively overriding original intent?',
    'Historical-legal analysis tracing the evolution of judicial deference doctrines and their alignment with evolving societal needs versus original constitutional debates.',
    'If primarily a pragmatic adaptation, its ''naturalness'' as a constitutional interpretation is weaker, potentially increasing its effective extractiveness by revealing a gap between claimed and actual grounding. If genuinely rooted, its Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the foundational legitimacy of the functionalist reading.').

omega_variable(
    delegation_doctrine_drift,
    'Has the ''intelligible principle'' doctrine, which limits congressional delegation to agencies, become so broad as to be effectively meaningless, allowing for unchecked agency power?',
    'Empirical analysis of judicial review outcomes for agency actions, specifically examining how often the ''intelligible principle'' is genuinely applied to strike down delegations.',
    'If the ''intelligible principle'' is effectively defunct, the functionalist reading''s coordination function might mask a higher degree of unchecked power, pushing its classification closer to a Tangled Rope or Snare for those subject to agency rules without clear legislative guidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_doctrine_drift, empirical, 'Whether the limits on delegation within the functionalist framework are still meaningful.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__functionalist_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement_basis(sepa_tr_t1930, observed).
narrative_ontology:measurement(sepa_tr_t1960, separation_of_powers_text__functionalist_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement_basis(sepa_tr_t1960, observed).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__functionalist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement_basis(sepa_tr_t1990, observed).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__functionalist_reading, theater_ratio, 2024, 0.1).
narrative_ontology:measurement_basis(sepa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__functionalist_reading, base_extractiveness, 1930, 0.15).
narrative_ontology:measurement_basis(sepa_be_t1930, observed).
narrative_ontology:measurement(sepa_be_t1960, separation_of_powers_text__functionalist_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement_basis(sepa_be_t1960, observed).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__functionalist_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement_basis(sepa_be_t1990, observed).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__functionalist_reading, base_extractiveness, 2024, 0.25).
narrative_ontology:measurement_basis(sepa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__functionalist_reading, suppression_requirement, 1930, 0.2).
narrative_ontology:measurement_basis(sepa_su_t1930, observed).
narrative_ontology:measurement(sepa_su_t1960, separation_of_powers_text__functionalist_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement_basis(sepa_su_t1960, observed).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__functionalist_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement_basis(sepa_su_t1990, observed).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__functionalist_reading, suppression_requirement, 2024, 0.3).
narrative_ontology:measurement_basis(sepa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_procedure_act).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is the 'functionalist_reading' of the 'separation_of_powers_text' kernel. It coexists with 'formalist_reading' and 'unitary_executive_reading', which offer alternative interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
