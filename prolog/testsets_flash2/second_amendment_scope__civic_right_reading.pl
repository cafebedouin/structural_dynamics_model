% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment: Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents a reading of the Second Amendment that
 *   interprets the right to bear arms as an individual right, but one that is
 *   explicitly conditioned on participation in a civic militia. This reading
 *   emphasizes the 'well regulated Militia' clause as central to the
 *   amendment's purpose, distinguishing it from both a purely individual
 *   right and a purely collective right. It implies a civic duty alongside
 *   the right, allowing for state regulation to ensure the militia's
 *   effectiveness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.3).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment: Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '405a2e31-685f-4042-8600-623a5f24bc64').
narrative_ontology:cs_kernel_codification('405a2e31-685f-4042-8600-623a5f24bc64', fixed_text).
narrative_ontology:cs_authority_grounding('405a2e31-685f-4042-8600-623a5f24bc64', lineage).
narrative_ontology:cs_interpretation_layer_present('405a2e31-685f-4042-8600-623a5f24bc64').
narrative_ontology:cs_reading_relation('405a2e31-685f-4042-8600-623a5f24bc64', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('405a2e31-685f-4042-8600-623a5f24bc64', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('405a2e31-685f-4042-8600-623a5f24bc64', foundational, individual_right_tied_to_civic_purpose).
narrative_ontology:cs_axiom_status(individual_right_tied_to_civic_purpose, holdable).
narrative_ontology:cs_axiom_grounding('405a2e31-685f-4042-8600-623a5f24bc64', individual_right_tied_to_civic_purpose, deontological).
narrative_ontology:cs_axiom('405a2e31-685f-4042-8600-623a5f24bc64', foundational, well_regulated_militia_is_essential_for_free_state).
narrative_ontology:cs_axiom_status(well_regulated_militia_is_essential_for_free_state, holdable).
narrative_ontology:cs_axiom_grounding('405a2e31-685f-4042-8600-623a5f24bc64', well_regulated_militia_is_essential_for_free_state, conventional).
narrative_ontology:cs_reference_frame('405a2e31-685f-4042-8600-623a5f24bc64', civic_republican_originalism).
narrative_ontology:cs_drift_state('405a2e31-685f-4042-8600-623a5f24bc64', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('405a2e31-685f-4042-8600-623a5f24bc64', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve_in_militia).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens benefit from the right to bear arms for militia service, which implies a civic duty and a corresponding right. Their right is conditional, meaning they must meet certain criteria related to militia participation, which may involve training or registration. Exit means foregoing the right or refusing civic duty.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).

% State governments benefit from having a well-regulated militia for security, which this reading supports. They have the authority to define and regulate militia participation, balancing individual rights with collective defense needs. Their power is moderated by the individual right component.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% These citizens bear the cost of not having an unconditional right to bear arms if they are unwilling or unable to participate in a militia. They may face restrictions on firearm ownership that others do not, effectively paying for the civic-mindedness of others. Exit means either conforming to militia requirements or living without certain firearm rights.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve_in_militia, payer,
    powerless, immediate, constrained, national).

% The federal judiciary interprets the Second Amendment, shaping the balance between individual rights and militia service. They observe the practical implications of this reading and adjudicate disputes, influencing its application over time.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right of individuals to possess firearms with the civic duty of participating in a well-regulated militia, ensuring a balance between individual liberty and collective security.
% TRANSFER_FUNCTION: Transfers the responsibility for collective defense, in part, to armed citizens organized as a militia, while granting those citizens the right to bear arms. It also transfers some regulatory authority to the state.
% ABSENT_VOICES: Advocates for an unconditional individual right to bear arms would object, arguing that militia service should not be a prerequisite for firearm ownership. They are often excluded from the framing of this reading, which prioritizes civic duty.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape around firearm ownership would fundamentally shift. States would lose a clear constitutional basis for regulating militias and conditioning firearm rights, leading to a scramble for new legal frameworks and potentially a more absolute individual right or a purely collective one.
% FOUNDING_PROBLEM: The framers sought to ensure the security of a free state by providing for a well-regulated militia, while also acknowledging the importance of an armed populace.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside of direct beneficiary groups corroborate that the founding problem involved balancing individual arms-bearing with the necessity of a civic militia for state security, reflecting the republican ideals of the era. Legal scholars also attest to the ongoing debate regarding this balance.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it grants a right, it conditions it, imposing a cost on those unwilling to meet the condition. Suppression (0.3) is low to moderate, as it involves state regulation rather than outright prohibition, but still restricts unconditional access. Theater ratio (0.1) is low, as the civic militia concept, while debated, is not purely performative within this reading. The constraint is claimed as a 'rope' because it genuinely attempts to coordinate individual rights with collective security, with identifiable beneficiaries and payers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of militia-eligible citizens, this is a beneficial coordination mechanism that grants a right while ensuring public safety. From the perspective of those unwilling to serve, it is an extractive constraint that denies an unconditional right. State governments see it as a necessary framework for public order. The engine will compute these different experiences based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are beneficiaries (damped extraction) as they gain a conditional right. State governments are also beneficiaries (or agenda-setters) as they gain a constitutional basis for a regulated militia. Citizens unwilling to serve are payers (amplified extraction) as they bear the cost of the conditionality. The federal judiciary acts as an observer, interpreting and applying the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_definition_ambiguity,
    'What constitutes a ''well regulated Militia'' in contemporary society, and how does individual participation manifest?',
    'Legislative action defining militia structure and participation requirements, or Supreme Court rulings clarifying the scope of ''well regulated''.',
    'A clear definition would solidify the conditions for the individual right, potentially increasing or decreasing extractiveness depending on the burden of participation. Ambiguity allows for varied state-level interpretations, leading to inconsistent application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''militia'' and ''participation''.').

omega_variable(
    civic_duty_enforceability,
    'To what extent is the ''civic duty'' aspect of militia participation genuinely enforceable, and what are the practical implications for individual rights?',
    'Empirical study of state-level militia laws and their enforcement, including cases where individuals are denied firearm rights due to non-participation.',
    'If civic duty is largely unenforced, the ''conditioned'' aspect of the right becomes theatrical, shifting the constraint closer to an unconditional individual right. If strictly enforced, it reinforces the civic-republican framing but increases suppression for non-participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_enforceability, empirical, 'The practical enforceability of the civic duty component.').

omega_variable(
    reading_foreclosure_potential,
    'Does this ''civic_right_reading'' logically foreclose the ''individual_right_reading'' within a single coherent constitutional framework, or do they merely coexist as competing interpretations?',
    'Detailed legal-philosophical analysis of the internal consistency of each reading and their mutual exclusivity, particularly in light of historical context and contemporary jurisprudence.',
    'If this reading forecloses the individual_right_reading, it implies a deeper structural conflict that cannot be resolved by mere policy choice. If they coexist, the contest is one of political preference and judicial interpretation, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_potential, conceptual, 'Whether the civic right interpretation logically excludes the unconditional individual right interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
