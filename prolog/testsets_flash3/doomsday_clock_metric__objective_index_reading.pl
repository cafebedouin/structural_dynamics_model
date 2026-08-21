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
 *   human_readable: Doomsday Clock as Objective Existential Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint models the Doomsday Clock as a purely objective index,
 *   tracking measurable existential risk levels through expert synthesis of
 *   empirical indicators. This reading emphasizes the scientific and
 *   empirical basis of the Clock's setting, downplaying or suppressing its
 *   normative and performative dimensions. It is one reading of the
 *   'doomsday_clock_metric' kernel, distinct from 'performative_tool_reading'
 *   and 'hybrid_legitimacy_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.75).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Existential Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'd41e4c93-dc09-463c-b3e1-4cfa5dfe56d0').
narrative_ontology:cs_kernel_codification('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', formalized).
narrative_ontology:cs_authority_grounding('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', expertise).
narrative_ontology:cs_interpretation_layer_present('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0').
narrative_ontology:cs_reading_relation('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_reading_relation('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', foundational, existential_risk_is_objectively_measurable).
narrative_ontology:cs_axiom_status(existential_risk_is_objectively_measurable, holdable).
narrative_ontology:cs_axiom_grounding('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', existential_risk_is_objectively_measurable, empirically_contingent).
narrative_ontology:cs_axiom('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', foundational, expert_synthesis_yields_value_neutral_assessment).
narrative_ontology:cs_axiom_status(expert_synthesis_yields_value_neutral_assessment, holdable).
narrative_ontology:cs_axiom_grounding('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', expert_synthesis_yields_value_neutral_assessment, empirically_contingent).
narrative_ontology:cs_reference_frame('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', scientific_objectivity_paradigm).
narrative_ontology:cs_drift_state('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', contemporary_science_studies_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d41e4c93-dc09-463c-b3e1-4cfa5dfe56d0', '').
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

% Benefits from the perception that the Doomsday Clock is a purely objective, empirically-driven measure, reinforcing its epistemic authority in risk assessment. This framing allows it to present its judgments as facts rather than interpretations.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority, beneficiary,
    institutional, generational, mobile, global).

% The institutions (e.g., Bulletin of the Atomic Scientists) that convene the experts and publish the Clock. They administer the process, synthesize expert input, and present the final 'objective' reading, thereby maintaining their central role in global risk discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Suffers from the expert monopoly on interpretation, as the 'objective' framing of the Clock can bypass broader public deliberation and democratic processes on risk priorities and normative trade-offs. The public is presented with a verdict, not a debate.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability, payer,
    powerless, generational, trapped, global).

% Bears the cost of a narrowed scope for public debate on existential risks. When the Clock is presented as an objective index, the normative and political dimensions of risk assessment are suppressed, limiting the space for diverse perspectives and values to shape policy.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_discourse, payer,
    moderate, biographical, constrained, global).

% Can leverage the 'objective' authority of the Clock to justify policy decisions, presenting them as responses to empirically validated threats rather than politically contested choices. This reduces their need to engage in complex public persuasion.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, beneficiary,
    powerful, immediate, mobile, national).

% Analyze the underlying assumptions and epistemic claims of the Doomsday Clock, questioning the possibility of a purely objective index for complex, normatively laden risks. They highlight the implicit values and framings embedded in expert judgments.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_epistemologists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative metric for global existential risk, allowing diverse actors (governments, NGOs, public) to coordinate their attention and resources around a shared understanding of threat levels.
% TRANSFER_FUNCTION: Transfers epistemic authority and interpretive control over existential risk assessment from broad public and democratic processes to a specialized expert body, in exchange for a seemingly objective and unified risk signal.
% ABSENT_VOICES: Normative ethicists, political philosophers, and representatives of marginalized communities whose values might differ on risk prioritization are excluded from the 'objective' index-setting process. They would argue for a more explicit and inclusive deliberation on the normative dimensions of existential risk.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock, as an objective index, vanished, the global discourse on existential risk would lose a central, widely recognized metric. This would force a rearrangement of how risks are communicated, prioritized, and acted upon, likely leading to more fragmented or explicitly contested framings.
% FOUNDING_PROBLEM: The problem of communicating the urgency and scale of existential threats (especially nuclear war) to a global audience in a way that commands attention and prompts action, without being dismissed as mere alarmism.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists and many scientific bodies attest that the problem of communicating complex, long-term risks remains live and critical. Critical epistemologists, while questioning the 'objective' solution, generally agree on the persistence of the underlying communication challenge.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the transfer of interpretive authority from public discourse to experts, effectively 'extracting' the right to define and prioritize risks. Suppression (0.75) is high because this reading actively suppresses alternative, more normative or politically engaged framings of the Clock's function, maintaining an expert monopoly. Theater ratio is low (0.1) because, within this reading, the Clock is genuinely intended as a functional index, not primarily a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scientific authority, this is a legitimate coordination mechanism for global risk. From the perspective of democratic accountability, it is an extractive mechanism that centralizes power over critical public discourse. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Scientific authority and expert institutions are beneficiaries, as this reading reinforces their epistemic power and central role. Democratic accountability and public discourse are victims, as their role in defining and deliberating on risks is diminished. Policy makers benefit from the simplified, 'objective' justification for their actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_empirical_boundary,
    'Is it possible to construct a purely objective, empirical index for existential risks, or do all such assessments inherently involve normative judgments?',
    'Philosophical analysis of the epistemology of risk, and empirical studies of expert judgment processes to identify implicit normative framings.',
    'If purely objective indexing is impossible, this reading''s claim to be a ''tangled_rope'' (with a coordination function) would be weakened, pushing it closer to a ''snare'' (pure extraction of interpretive authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_empirical_boundary, conceptual, 'The conceptual possibility of a purely objective risk index.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of normative framing structural (due to the institutional design of expert panels) or internalized (due to experts'' self-perception of their role)?',
    'Analysis of expert panel mandates and composition, alongside qualitative interviews with experts to understand their epistemic self-conception and how it shapes their output.',
    'If internalized, the suppression is more deeply embedded and harder to address through institutional reform alone, potentially increasing the effective suppression beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism of normative framing.').

omega_variable(
    democratic_deficit_severity,
    'How severe is the democratic deficit caused by the ''objective index'' framing, and what are its long-term impacts on public engagement with risk?',
    'Longitudinal studies of public understanding and engagement with the Doomsday Clock, comparing outcomes in contexts where its ''objectivity'' is emphasized versus those where its normative dimensions are openly discussed.',
    'A high severity would strengthen the ''snare'' classification for democratic accountability, indicating a more profound and damaging extraction of public agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deficit_severity, empirical, 'Severity of democratic deficit due to objective framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doom_tr_t5, doomsday_clock_metric__objective_index_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__objective_index_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__objective_index_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__objective_index_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(doom_be_t5, doomsday_clock_metric__objective_index_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__objective_index_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__objective_index_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__objective_index_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(doom_su_t5, doomsday_clock_metric__objective_index_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__objective_index_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__objective_index_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__objective_index_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'doomsday_clock_metric' kernel. It focuses on the claim that the Clock is an objective, empirically-driven index of existential risk. Its ε value reflects the extraction of interpretive authority inherent in this claim, which differs significantly from readings that emphasize its performative or hybrid nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
