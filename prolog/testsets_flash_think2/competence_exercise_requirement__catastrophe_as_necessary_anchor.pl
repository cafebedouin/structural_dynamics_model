% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Competence Requires Catastrophe Anchor
 *   domain: Safety Engineering/Organizational Learning/High-Reliability Organizations
 *
 * SUMMARY:
 *   This constraint represents the deeply ingrained, often implicit, belief
 *   within certain high-stakes domains that only real catastrophic events or
 *   near-misses provide the 'irreducible exercise' necessary to maintain
 *   operational competence. It posits that competence atrophies during
 *   catastrophe-free periods, and that simulations, no matter how advanced,
 *   cannot fully substitute for the learning derived from actual jeopardy.
 *   This reading claims this as an unchangeable truth, a 'mountain' of
 *   organizational learning.
 *
 * KEY AGENTS:
 *   - Organizational Leaders: Agenda setter, perpetuate the belief, benefit from perceived cost savings.
 *   - Safety Engineers: Payer, bear the cost of under-resourced proactive safety.
 *   - High-Reliability Organizations: Payer, suffer competence decay.
 *   - Public Safety Advocates: Payer, bear the ultimate cost of harm.
 *   - Catastrophe Response Industry: Beneficiary, gains relevance and funding post-event.
 *   - Analytical Observers: Observer, study the dynamics of this belief.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.85).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.9).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.85).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, mountain).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Competence Requires Catastrophe Anchor").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Safety Engineering/Organizational Learning/High-Reliability Organizations").

domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '588f9147-35d8-433b-baa9-aefc7bf4cd5d').
narrative_ontology:cs_kernel_codification('588f9147-35d8-433b-baa9-aefc7bf4cd5d', implicit).
narrative_ontology:cs_authority_grounding('588f9147-35d8-433b-baa9-aefc7bf4cd5d', practice).
narrative_ontology:cs_interpretation_layer_present('588f9147-35d8-433b-baa9-aefc7bf4cd5d').
narrative_ontology:cs_reading_relation('588f9147-35d8-433b-baa9-aefc7bf4cd5d', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('588f9147-35d8-433b-baa9-aefc7bf4cd5d', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('588f9147-35d8-433b-baa9-aefc7bf4cd5d', foundational, real_world_jeopardy_is_unique_teacher).
narrative_ontology:cs_axiom_status(real_world_jeopardy_is_unique_teacher, holdable).
narrative_ontology:cs_axiom_grounding('588f9147-35d8-433b-baa9-aefc7bf4cd5d', real_world_jeopardy_is_unique_teacher, empirically_contingent).
narrative_ontology:cs_axiom('588f9147-35d8-433b-baa9-aefc7bf4cd5d', secondary, simulation_creates_false_confidence).
narrative_ontology:cs_axiom_status(simulation_creates_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('588f9147-35d8-433b-baa9-aefc7bf4cd5d', simulation_creates_false_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('588f9147-35d8-433b-baa9-aefc7bf4cd5d', historical_catastrophe_learning_cycle).
narrative_ontology:cs_drift_state('588f9147-35d8-433b-baa9-aefc7bf4cd5d', contemporary_safety_science_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('588f9147-35d8-433b-baa9-aefc7bf4cd5d', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_response_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizational_leaders).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_safety_advocates).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_engineers).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, learning_from_failure_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, experience_is_the_best_teacher).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Often perpetuate this belief, consciously or unconsciously, as it justifies lower investment in costly, high-fidelity simulation and training. They benefit from perceived cost savings until a catastrophe forces a reactive response.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizational_leaders, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of this belief through under-resourced proactive safety programs and the professional burden of managing systems with decaying competence. They advocate for more robust simulation but face resistance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_engineers, payer,
    moderate, biographical, constrained, national).

% Are the primary entities whose competence is at risk. They may internally hold this belief, leading to a cycle of competence decay followed by reactive learning from actual events, rather than proactive maintenance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organizations, payer,
    institutional, generational, identity_locked, national).

% Bear the ultimate cost of competence decay in terms of public harm and loss of trust. They resist this belief by pushing for stronger regulatory oversight and proactive safety measures, but often lack direct influence on organizational culture.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Benefits from the reactive cycle of competence decay and catastrophic events, as their services (investigation, recovery, remediation, and post-event training) become indispensable after a major incident.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_response_industry, beneficiary,
    organized, biographical, mobile, national).

% Study organizational learning and safety culture, identifying the structural dynamics of this belief. They can articulate the costs and benefits but are external to the operational decision-making.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_response_industry).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared, often implicit, understanding within an organization or industry about the ultimate source of true operational competence and the limits of alternative training methods.
% TRANSFER_FUNCTION: Transfers resources and attention away from proactive, high-fidelity simulation and training, and towards reactive learning and response mechanisms that activate only after real-world failures. It also transfers the cost of competence decay to the public and frontline operators.
% ABSENT_VOICES: Advocates for advanced simulation, proactive safety researchers, and those who have suffered from preventable incidents are often marginalized or unheard in the face of this deeply ingrained belief. Their arguments for alternative competence maintenance are suppressed by the perceived 'naturalness' of learning from real events.
% DISAPPEARANCE_RATIONALE: If this belief vanished overnight, organizations would fundamentally rethink their investment in simulation, training, and proactive risk management. There would be a significant shift towards preventing catastrophes through continuous, high-fidelity competence exercise, rather than relying on real events as anchors. This would reorganize safety budgets, training methodologies, and organizational learning cultures.
% FOUNDING_PROBLEM: The problem of how to effectively maintain high-stakes operational competence in complex, dynamic environments, especially when real catastrophic events are rare but devastating.
% FOUNDING_PROBLEM_CORROBORATION: The problem of competence maintenance is universally acknowledged as live by all stakeholders. However, the *solution* (catastrophe as necessary anchor) is contested. Historical accident reports and post-mortem analyses from independent safety boards often highlight competence gaps that developed during 'quiet' periods, lending empirical weight to the 'catastrophe as anchor' perspective, even if it's a tragic one. Conversely, simulation advocates point to successful simulation-based training programs as counter-evidence.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, ExtMetricName, E),
    domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.85) because this belief extracts safety and preparedness by leading to underinvestment in proactive competence maintenance, effectively 'taxing' the system with the risk of actual failure. `Suppression` is very high (0.90) as it suppresses the legitimacy of alternative competence-building methods (like high-fidelity simulation) by framing them as inherently insufficient. `Theater_ratio` is moderate-high (0.60) because many organizations perform simulations, but if the underlying belief is that 'real' learning only comes from catastrophe, these simulations become performative rather than truly functional for deep competence exercise. `Accessibility_collapse` is high (0.95) because if only real catastrophe provides the 'irreducible exercise,' then all other alternatives for *that specific kind* of exercise are deemed ineffective. `Resistance` is moderate (0.40) from safety professionals and advocates, but the belief is deeply entrenched.
 *
 * PERSPECTIVAL GAP:
 *   Organizational leaders may perceive this as a pragmatic truth, justifying resource allocation away from expensive simulations, while safety engineers and public safety advocates experience it as a dangerous, extractive force that compromises safety. The catastrophe response industry, while not actively promoting the belief, benefits from its consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational leaders benefit from the perceived cost savings of not investing heavily in 'insufficient' simulations, and the catastrophe response industry benefits from the reactive cycle. High-reliability organizations, safety engineers, and public safety advocates bear the costs of competence decay and actual incidents. The belief itself, while not an active enforcer, shapes resource flows and risk exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a strong candidate for false summit detection. It is claimed as a 'mountain' (an irreducible truth about how competence is maintained), but it has clear beneficiaries and victims, and its operation leads to significant extraction (of safety). The engine's classification will likely diverge from the claimed type, highlighting it as a constructed constraint masquerading as natural law. The 'mandate' (maintaining competence) is still live, but the 'method' (catastrophe as anchor) is highly problematic and extractive, suggesting a deep structural flaw rather than simple mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_belief,
    'Is the necessity of catastrophe for competence maintenance a genuine natural law of organizational learning, or a constructed belief perpetuated by organizational inertia and resource allocation patterns?',
    'Longitudinal studies of organizations that successfully maintain competence through high-fidelity simulation and proactive measures without major incidents, demonstrating that the ''catastrophe anchor'' is not universally required.',
    'If constructed, the constraint''s ''mountain'' claim is false, and it would reclassify as a Snare or Tangled Rope, highlighting its extractive nature and the suppression of alternatives. If a genuine natural law, the high extractiveness is an unavoidable cost of reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_belief, empirical, 'Ambiguity between a natural law and a constructed organizational belief.').

omega_variable(
    simulation_adequacy_ambiguity,
    'To what extent can high-fidelity simulation and advanced training truly replicate the ''irreducible exercise'' provided by real catastrophic events, thereby making them adequate alternatives?',
    'Empirical validation of simulation-trained teams'' performance in real-world high-stakes scenarios, compared to teams with ''catastrophe-anchored'' experience.',
    'If simulations are proven adequate, the ''catastrophe_as_necessary_anchor'' reading''s core premise is undermined, leading to a re-evaluation of its suppression and extractiveness. This would support the ''simulation_as_adequate_exercise'' sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_adequacy_ambiguity, empirical, 'The degree to which simulation can substitute for real-world jeopardy.').

omega_variable(
    disagreement_location_of_exercise,
    'Where is the fundamental disagreement located regarding competence exercise: in the nature of ''jeopardy'' (real vs. simulated), the ''fidelity'' of the training environment, or the ''transferability'' of learning?',
    'Conceptual analysis and expert consensus on the specific mechanisms of learning and skill retention in high-stakes environments.',
    'Clarifying the locus of disagreement would enable more targeted interventions and potentially bridge the gap between this reading and its siblings, or solidify their irreconcilable differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_of_exercise, conceptual, 'Pinpointing the core conceptual difference between readings on competence exercise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 1990, 0.48).
narrative_ontology:measurement(comp_tr_t2000, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(comp_tr_t2020, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2020, 0.59).
narrative_ontology:measurement(comp_tr_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(comp_be_t2000, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(comp_be_t2020, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(comp_be_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(comp_su_t2000, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(comp_su_t2020, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(comp_su_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'competence_exercise_requirement' kernel. This reading, 'catastrophe_as_necessary_anchor', asserts that only real catastrophic events provide the irreducible exercise for competence. It stands in direct opposition to 'simulation_as_adequate_exercise' and 'hybrid_dependency'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
