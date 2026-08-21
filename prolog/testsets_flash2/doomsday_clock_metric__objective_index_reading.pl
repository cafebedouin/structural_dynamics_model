% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock: Objective Index Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint represents the 'objective index' reading of the Doomsday
 *   Clock, where its setting is understood as a direct, empirically-driven
 *   measure of existential risk. This reading emphasizes expert synthesis of
 *   measurable indicators, aiming to provide an authoritative, non-normative
 *   assessment. It is one reading of the 'doomsday_clock_metric' kernel,
 *   distinct from readings that emphasize its performative or hybrid nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.78).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock: Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '8495bfea-82c1-486e-937f-3f386f1f4952').
narrative_ontology:cs_kernel_codification('8495bfea-82c1-486e-937f-3f386f1f4952', formalized).
narrative_ontology:cs_authority_grounding('8495bfea-82c1-486e-937f-3f386f1f4952', expertise).
narrative_ontology:cs_interpretation_layer_present('8495bfea-82c1-486e-937f-3f386f1f4952').
narrative_ontology:cs_reading_relation('8495bfea-82c1-486e-937f-3f386f1f4952', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('8495bfea-82c1-486e-937f-3f386f1f4952', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('8495bfea-82c1-486e-937f-3f386f1f4952', foundational, existential_risk_is_objectively_measurable).
narrative_ontology:cs_axiom_status(existential_risk_is_objectively_measurable, holdable).
narrative_ontology:cs_axiom_grounding('8495bfea-82c1-486e-937f-3f386f1f4952', existential_risk_is_objectively_measurable, empirically_contingent).
narrative_ontology:cs_axiom('8495bfea-82c1-486e-937f-3f386f1f4952', foundational, expert_synthesis_yields_unbiased_index).
narrative_ontology:cs_axiom_status(expert_synthesis_yields_unbiased_index, holdable).
narrative_ontology:cs_axiom_grounding('8495bfea-82c1-486e-937f-3f386f1f4952', expert_synthesis_yields_unbiased_index, empirically_contingent).
narrative_ontology:cs_reference_frame('8495bfea-82c1-486e-937f-3f386f1f4952', pure_scientific_assessment).
narrative_ontology:cs_drift_state('8495bfea-82c1-486e-937f-3f386f1f4952', contemporary_risk_governance_discourse, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8495bfea-82c1-486e-937f-3f386f1f4952', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_institutions).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perception that the Clock is a purely objective, empirically-driven assessment, reinforcing its epistemic authority in risk governance. This reading positions scientific bodies as the sole legitimate interpreters of existential risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority, beneficiary,
    institutional, generational, constrained, global).

% The institutions (e.g., Bulletin of the Atomic Scientists) that convene experts and publish the Clock. They assert the Clock's objectivity and empirical grounding, thereby maintaining their role as authoritative arbiters of global risk. They control the methodology and communication.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Bears the cost of an expert monopoly on risk interpretation, where the normative and policy implications of the Clock's setting are presented as direct consequences of objective facts, rather than subject to democratic deliberation. This suppresses public input on the 'what should be done' question.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability, payer,
    powerless, biographical, trapped, national).

% Suffers from the narrowing of debate around existential risks, as the 'objective index' framing discourages questioning the underlying assumptions or the normative choices embedded in the risk assessment. It limits the space for alternative framings or policy approaches.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_discourse, payer,
    moderate, immediate, constrained, global).

% Can leverage the 'objective' Clock reading to justify policy decisions, presenting them as responses to empirically validated threats rather than politically contested choices. This provides a veneer of scientific legitimacy to their agendas.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, beneficiary,
    powerful, biographical, mobile, national).

% Analyze the epistemic claims of the Clock, questioning the possibility of a purely objective index for complex, value-laden risks. They highlight the implicit normative choices and the performative aspects of the Clock's communication.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_epistemologists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expert consensus on the severity of global existential risks, providing a single, authoritative metric for public and policy attention. It aims to synthesize diverse scientific inputs into a coherent signal.
% TRANSFER_FUNCTION: Transfers epistemic authority over existential risk assessment from broad public and political discourse to a specialized expert body, along with the associated influence on policy framing and public perception.
% ABSENT_VOICES: Advocates for democratic deliberation on risk priorities and the normative dimensions of existential threats are structurally excluded. Their arguments for a more participatory approach to risk assessment are suppressed by the assertion of objective, expert-driven indexing.
% DISAPPEARANCE_RATIONALE: If the Clock, as an objective index, vanished, the primary mechanism for expert-driven, 'objective' synthesis of existential risk would disappear. This would force a re-evaluation of how such risks are communicated and governed, likely leading to a more fragmented or overtly political discourse around these issues.
% FOUNDING_PROBLEM: The problem of communicating complex, long-term existential threats (like nuclear war) to a broad public and policymakers in a way that is both authoritative and actionable, without being alarmist or dismissive.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists and many scientific bodies attest that the problem of communicating complex global risks remains live. Critical epistemologists and social scientists, while questioning the 'objective index' approach, corroborate the underlying challenge of risk communication, but not the efficacy or legitimacy of this specific solution.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the concentration of interpretive authority in expert hands, which limits broader public and democratic engagement with the normative dimensions of risk. Suppression (0.78) is high because this reading actively suppresses alternative framings (e.g., the Clock as a performative tool or a hybrid of science and values) to maintain its claim of objectivity. The theater ratio (0.20) is low because, from this reading's perspective, the Clock's primary function is genuinely to index risk, not merely to perform. The rising extractiveness and suppression over time reflect the increasing institutionalization of this 'objective' framing and the corresponding marginalization of dissenting interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expert institutions, the Clock is a necessary coordination mechanism for global risk assessment. From the perspective of democratic accountability, it is an extractive mechanism that centralizes power and suppresses public deliberation on inherently normative issues. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Scientific authority and expert institutions are beneficiaries (d near 0.0) as this reading reinforces their epistemic power and control over the risk narrative. Democratic accountability and public discourse are victims (d near 1.0) as their role in shaping risk priorities is diminished by the assertion of objective, expert-driven metrics. Policymakers are also beneficiaries, as they can use this 'objective' framing to legitimize their actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_normative_boundary,
    'Is it possible to construct a purely objective, empirical index for existential risks, or do such indices inherently embed normative judgments?',
    'Philosophical and sociological analysis of risk assessment methodologies, examining the points at which values and assumptions enter the ''objective'' calculation.',
    'If purely objective indexing is impossible, this reading''s foundational axiom is challenged, potentially reclassifying it as a Snare (if the ''objectivity'' is cover for extraction) or a Tangled Rope (if a genuine coordination function remains but is entangled with unacknowledged normative choices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_normative_boundary, conceptual, 'Ambiguity of the empirical-normative boundary in existential risk assessment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (e.g., institutional power of expert bodies) or internalized (e.g., public deference to scientific authority)?',
    'Post-exit suppression trajectory: if alternative framings gain traction after expert institutions lose their monopoly on the Clock''s interpretation, it suggests structural suppression. If deference persists, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them after exit, making democratic accountability harder to achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative risk framings.').

omega_variable(
    mandate_drift_from_founding,
    'Has the Clock''s mandate drifted from its founding purpose of warning about nuclear war to a broader, more ambiguous ''existential risk'' index, and if so, does the ''objective index'' reading obscure this drift?',
    'Historical analysis of the Bulletin''s stated mission and the evolution of the Clock''s criteria over time, compared with contemporary expert and public perceptions.',
    'If significant drift is confirmed and obscured, it would strengthen the ''snare'' elements of this reading, as the ''objective index'' framing would serve to legitimize an expanded, potentially unmandated, scope of authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_from_founding, empirical, 'Drift in the Clock''s mandate and its concealment by the objective index reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__objective_index_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__objective_index_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__objective_index_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__objective_index_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__objective_index_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__objective_index_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__objective_index_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__objective_index_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__objective_index_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__objective_index_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__objective_index_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__objective_index_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'doomsday_clock_metric' kernel. This 'objective_index_reading' influences (and is influenced by) the 'performative_tool_reading' and 'hybrid_legitimacy_reading' by shaping the overall discourse around the Clock's legitimacy and function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
