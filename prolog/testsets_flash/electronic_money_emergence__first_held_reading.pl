% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence (First Held Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint defines the emergence of electronic money as a discrete
 *   institutional event, specifically when the first institutional bearer
 *   held dematerialized currency in a form distinguishable from physical
 *   notes. This reading emphasizes legal and regulatory recognition as the
 *   key threshold, rather than conceptual possibility or statistical
 *   aggregation. It is presented as a Mountain because, from this
 *   perspective, the historical event of institutional recognition is a
 *   fixed, unchangeable fact, even if its interpretation is contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.1).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.05).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First Held Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '4cc7ecf6-7d06-41ca-94b4-4c969382aabf').
narrative_ontology:cs_kernel_codification('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', implicit).
narrative_ontology:cs_authority_grounding('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', expertise).
narrative_ontology:cs_reading_relation('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', foundational, emergence_is_discrete_institutional_event).
narrative_ontology:cs_axiom_status(emergence_is_discrete_institutional_event, holdable).
narrative_ontology:cs_axiom_grounding('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', emergence_is_discrete_institutional_event, conventional).
narrative_ontology:cs_axiom('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', secondary, distinguishable_form_is_ontological_marker).
narrative_ontology:cs_axiom_status(distinguishable_form_is_ontological_marker, holdable).
narrative_ontology:cs_axiom_grounding('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', distinguishable_form_is_ontological_marker, conventional).
narrative_ontology:cs_reference_frame('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', clear_institutional_definition).
narrative_ontology:cs_drift_state('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4cc7ecf6-7d06-41ca-94b4-4c969382aabf', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, institutional_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, legally recognized definition of electronic money, which allows for effective monetary policy and financial stability regulation. The emergence of this form of money provides a new object for their oversight.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).

% Gain clarity and a defined scope for their regulatory activities once electronic money is institutionally recognized and held. This allows them to establish frameworks for consumer protection, anti-money laundering, and systemic risk management.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, financial_regulators, beneficiary,
    institutional, generational, analytical, national).

% Are the first entities to hold dematerialized currency in a distinct form, bearing the initial costs and risks of developing and implementing the necessary technological and legal infrastructure. They must comply with emerging regulatory definitions.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, institutional_bearers, payer,
    organized, biographical, constrained, global).

% Analyze the historical moment and conditions under which electronic money became institutionally recognized. Their work involves identifying the specific events and legal precedents that mark this emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, institutionally recognized definition of electronic money, enabling coordinated legal, regulatory, and economic frameworks for its management and use.
% TRANSFER_FUNCTION: Defines the ontological boundary of 'electronic money,' transferring conceptual clarity from an ambiguous state to a discrete, measurable institutional fact.
% ABSENT_VOICES: The 'became_thinkable_reading' proponents would argue that the constraint misses the crucial pre-institutional conceptual shift, while the 'm4_m5_collapse_reading' proponents would argue that the emergence is a statistical artifact, not a discrete event. Both are excluded from this reading's focus on institutional recognition.
% DISAPPEARANCE_RATIONALE: The historical fact of electronic money's institutional emergence, as defined by this reading, is a past event. Its disappearance would not alter the current reality of digital finance, only our understanding of its origin point.
% FOUNDING_PROBLEM: The ambiguity surrounding the precise moment and nature of electronic money's emergence, leading to difficulties in historical analysis and regulatory definition.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and monetary theorists outside the direct beneficiaries continue to debate the precise definition and timing of electronic money's emergence, corroborating that the problem of definition remains live, even if this reading offers a specific resolution.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because the constraint itself is a definition of an historical event, not an active mechanism of extraction. Suppression is also low (0.05) as it's a conceptual boundary, not enforced coercion. Theater ratio is 0.0 as there's no performative maintenance of a past event. Accessibility collapse is high (0.9) because once the institutional recognition occurs, the 'alternative' definitions of emergence are conceptually foreclosed within this specific framework. Resistance is 0.0 because the 'event' itself cannot be resisted, only its interpretation.
 *
 * PERSPECTIVAL GAP:
 *   This reading's focus on institutional recognition creates a gap with other readings that prioritize conceptual shifts or statistical artifacts. From the perspective of this reading, the emergence is a clear, observable historical fact, while other perspectives might see it as a gradual process or a measurement effect.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and financial regulators are beneficiaries because a clear, institutionally recognized definition of electronic money aids their functions. Institutional bearers are 'payers' in the sense that they are the agents whose actions define the emergence, bearing the initial costs of innovation and compliance. Economic historians are observers, analyzing the event without directly benefiting or paying from its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_emergence,
    'Is the emergence of electronic money primarily an institutional event (first held), a conceptual shift (became thinkable), or a statistical artifact (M4/M5 collapse)?',
    'Further historical and economic research, potentially leading to a consensus on the most salient definitional criteria for ''emergence.''',
    'If resolved towards ''became_thinkable_reading'', the constraint would shift from a discrete event to a more gradual, less precisely datable process, potentially altering its classification from Mountain to a more fluid type. If resolved towards ''m4_m5_collapse_reading'', the emergence would be seen as a measurement artifact, making the ''constraint'' itself a conceptual tool rather than an emergent property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_emergence, conceptual, 'Ambiguity in the definition of ''emergence'' for electronic money.').

omega_variable(
    legal_recognition_threshold,
    'What specific legal or regulatory act constitutes the ''first institutional holding'' that defines this emergence?',
    'Detailed legal and historical analysis of early electronic payment systems and their regulatory treatment across different jurisdictions.',
    'A clear, universally accepted legal threshold would strengthen this reading''s claim as a Mountain. Ambiguity or multiple competing thresholds would weaken it, suggesting a more distributed or contested emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recognition_threshold, empirical, 'Precision of the legal/regulatory threshold for institutional recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__first_held_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__first_held_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__first_held_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
