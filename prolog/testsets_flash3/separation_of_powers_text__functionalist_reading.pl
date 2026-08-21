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
 *   human_readable: Separation of Powers (Functionalist Reading): Flexible Delegation
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'functionalist reading' of the
 *   constitutional separation of powers, which views the framework as
 *   flexible, permitting overlapping authority and intelligible delegation of
 *   principles to administrative agencies. This reading is crucial for the
 *   legitimacy and operation of the modern regulatory state, allowing
 *   Congress and the President to share legislative and executive functions
 *   effectively. It is one reading of the 'separation_of_powers_text' kernel,
 *   distinct from formalist or unitary executive interpretations.
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
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers (Functionalist Reading): Flexible Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'aa963a77-a466-4c62-99c5-25e55c461cab').
narrative_ontology:cs_kernel_codification('aa963a77-a466-4c62-99c5-25e55c461cab', fixed_text).
narrative_ontology:cs_authority_grounding('aa963a77-a466-4c62-99c5-25e55c461cab', lineage).
narrative_ontology:cs_interpretation_layer_present('aa963a77-a466-4c62-99c5-25e55c461cab').
narrative_ontology:cs_reading_relation('aa963a77-a466-4c62-99c5-25e55c461cab', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa963a77-a466-4c62-99c5-25e55c461cab', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('aa963a77-a466-4c62-99c5-25e55c461cab', foundational, flexible_governance_necessity).
narrative_ontology:cs_axiom_status(flexible_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('aa963a77-a466-4c62-99c5-25e55c461cab', flexible_governance_necessity, instrumental).
narrative_ontology:cs_axiom('aa963a77-a466-4c62-99c5-25e55c461cab', foundational, intelligible_principle_delegation).
narrative_ontology:cs_axiom_status(intelligible_principle_delegation, holdable).
narrative_ontology:cs_axiom_grounding('aa963a77-a466-4c62-99c5-25e55c461cab', intelligible_principle_delegation, conventional).
narrative_ontology:cs_reference_frame('aa963a77-a466-4c62-99c5-25e55c461cab', modern_administrative_state_efficacy).
narrative_ontology:cs_drift_state('aa963a77-a466-4c62-99c5-25e55c461cab', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aa963a77-a466-4c62-99c5-25e55c461cab', '').
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

% Benefit from the ability to receive broad delegations of authority from Congress, allowing them to implement complex policy. Their legitimacy and operational scope depend on this flexible interpretation of separation of powers.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the ability to delegate complex legislative tasks to agencies, allowing it to focus on broader policy goals and avoid micromanagement. This reading preserves its capacity to govern effectively in a complex modern state.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the flexibility to oversee and direct administrative agencies, which are part of the executive branch, ensuring policy implementation aligns with executive priorities. This reading supports a strong, unified executive function.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the constitutional framework, often deferring to agency expertise under doctrines like Chevron. This reading allows the judiciary to maintain its role without becoming a super-legislature or super-executive.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Would argue that this functionalist reading undermines the constitutional design by permitting excessive delegation and blurring the lines between branches. Their arguments are often heard in academic discourse and dissenting judicial opinions but do not currently dominate the practical application of the constraint.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the effective governance of a complex modern state by allowing Congress to delegate detailed policy implementation to expert administrative agencies, while maintaining oversight by the President and judicial review.
% TRANSFER_FUNCTION: Transfers legislative detail and implementation authority from Congress to administrative agencies, and oversight responsibility from Congress to the President, enabling a more efficient and adaptable regulatory state.
% ABSENT_VOICES: Formalist legal scholars and advocates for a strict separation of powers are present in academic and judicial dissent, but their arguments for impermeable boundaries are largely excluded from the practical operationalization of the constraint by the political branches and the prevailing judicial doctrines.
% DISAPPEARANCE_RATIONALE: If this functionalist reading vanished overnight, the entire administrative state would be rendered unconstitutional, leading to a collapse of regulatory capacity across numerous domains (environmental protection, financial regulation, public health). Congress would be overwhelmed, and the modern state's ability to govern would fundamentally reorganize.
% FOUNDING_PROBLEM: The original constitutional framework, designed for a simpler agrarian society, struggled to adapt to the complexities of industrialization and the modern administrative state, requiring a flexible interpretation to enable effective governance.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and practitioners widely corroborate that the functionalist reading emerged to address the practical challenges of modern governance, allowing the constitutional structure to remain relevant and effective. This is attested by historical legal precedent and contemporary administrative practice.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The functionalist reading is characterized by relatively low extractiveness (0.25) and suppression (0.30) because it is largely accepted as a necessary adaptation for effective governance, benefiting multiple institutional actors. The 'costs' are primarily the perceived blurring of constitutional lines, which is a conceptual rather than direct material extraction. Theater ratio is low (0.10) as the functions performed by agencies under this reading are genuinely productive and not merely performative. Accessibility collapse is moderate (0.60) because while alternatives (strict formalism) exist conceptually, they are practically difficult to implement without dismantling the modern state. Resistance is low (0.20) as this reading is the dominant operational paradigm.
 *
 * PERSPECTIVAL GAP:
 *   While the functionalist reading is widely adopted by the political branches and much of the judiciary, formalist scholars perceive it as a deviation from the original constitutional design, leading to a significant conceptual gap. However, this gap does not translate into high material extraction or suppression within the functionalist framework itself, as the framework is designed to coordinate these complex inter-branch relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies, Congress, and the President are all beneficiaries of this reading, as it enables their effective functioning in a complex world. The federal judiciary acts as an observer, interpreting and applying the framework. Formalist legal scholars are 'excluded' in the sense that their preferred strict interpretation is not the operational norm, though their arguments persist in academic and dissenting legal discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (enabling effective modern governance) is still live. The functionalist reading actively prevents the mislabeling of necessary coordination (delegation to agencies) as pure extraction, by providing a legitimate constitutional basis for such arrangements. If the founding problem were 'dead', the constraint would likely compute as a Piton or Snare, but its ongoing utility as a coordinating mechanism keeps it in the Rope category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_vs_formalist_legitimacy,
    'Is the functionalist reading''s legitimacy derived from its practical efficacy in modern governance, or is it a deviation from the original constitutional intent that requires re-evaluation?',
    'A constitutional amendment explicitly codifying or rejecting the administrative state''s structure, or a definitive Supreme Court ruling overturning decades of deference doctrines.',
    'If re-evaluated and rejected, the constraint would shift dramatically towards a Snare or Tangled Rope, as the administrative state would be deemed illegitimate and highly extractive. If explicitly codified, its Rope classification would be further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_vs_formalist_legitimacy, conceptual, 'Ambiguity regarding the ultimate constitutional legitimacy of the functionalist reading versus a strict formalist interpretation.').

omega_variable(
    delegation_intelligibility_standard,
    'Is the ''intelligible principle'' standard for congressional delegation sufficiently robust to prevent arbitrary agency action, or has it become a mere formality that permits excessive, unchecked power?',
    'Empirical analysis of agency rulemaking outcomes and judicial review patterns, or a Supreme Court decision establishing a stricter, more demanding intelligible principle test.',
    'If the standard is found to be a formality, the constraint''s extractiveness and suppression would be higher, as agencies would operate with less accountability, potentially shifting it towards a Tangled Rope. If robust, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_intelligibility_standard, empirical, 'Whether the ''intelligible principle'' doctrine effectively constrains delegated power or serves as a cover for unchecked agency discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__functionalist_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__functionalist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__functionalist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sepa_tr_t50, separation_of_powers_text__functionalist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__functionalist_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__functionalist_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__functionalist_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(sepa_be_t50, separation_of_powers_text__functionalist_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__functionalist_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__functionalist_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__functionalist_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(sepa_su_t50, separation_of_powers_text__functionalist_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_deference_doctrines).

% DUAL FORMULATION NOTE:
% This constraint is the 'functionalist reading' of the 'separation_of_powers_text' kernel. It coexists with and influences the 'formalist_reading' and 'unitary_executive_reading' by providing an alternative, dominant operational framework for constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
