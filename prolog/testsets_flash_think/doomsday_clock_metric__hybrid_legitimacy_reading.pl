% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock: Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint represents the Doomsday Clock's operation as embodying an
 *   irreducible entanglement of scientific judgment and normative stakes. Its
 *   legitimacy derives from a deliberate ambiguity, blending expert
 *   assessment with a call to action, which allows it to function as a
 *   powerful communication tool but also creates an accountability void. This
 *   reading focuses on how this hybrid nature is maintained and what
 *   structural effects it has.
 *
 * KEY AGENTS:
 *   - bulletin_of_atomic_scientists: Primary agenda_setter (institutional/constrained) — maintains the clock's authority.
 *   - global_risk_discourse: Primary beneficiary (institutional/mobile) — gains a focal point for discussion.
 *   - pure_scientific_community: Primary payer (organized/constrained) — seeks clarity but finds entanglement.
 *   - clear_accountability_demands: Excluded (organized/constrained) — marginalized by the clock's ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.45).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.65).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock: Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'c11b8261-46e5-4a0a-b32d-a29c17b45b00').
narrative_ontology:cs_kernel_codification('c11b8261-46e5-4a0a-b32d-a29c17b45b00', formalized).
narrative_ontology:cs_authority_grounding('c11b8261-46e5-4a0a-b32d-a29c17b45b00', lineage).
narrative_ontology:cs_interpretation_layer_present('c11b8261-46e5-4a0a-b32d-a29c17b45b00').
narrative_ontology:cs_reading_relation('c11b8261-46e5-4a0a-b32d-a29c17b45b00', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('c11b8261-46e5-4a0a-b32d-a29c17b45b00', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('c11b8261-46e5-4a0a-b32d-a29c17b45b00', foundational, risk_communication_requires_synthesis).
narrative_ontology:cs_axiom_status(risk_communication_requires_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('c11b8261-46e5-4a0a-b32d-a29c17b45b00', risk_communication_requires_synthesis, conventional).
narrative_ontology:cs_axiom('c11b8261-46e5-4a0a-b32d-a29c17b45b00', secondary, ambiguity_as_strategic_resource).
narrative_ontology:cs_axiom_status(ambiguity_as_strategic_resource, holdable).
narrative_ontology:cs_axiom_grounding('c11b8261-46e5-4a0a-b32d-a29c17b45b00', ambiguity_as_strategic_resource, instrumental).
narrative_ontology:cs_reference_frame('c11b8261-46e5-4a0a-b32d-a29c17b45b00', cold_war_mobilization_framework).
narrative_ontology:cs_drift_state('c11b8261-46e5-4a0a-b32d-a29c17b45b00', contemporary_poly_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c11b8261-46e5-4a0a-b32d-a29c17b45b00', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, global_risk_discourse).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, clear_accountability_demands).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, pure_scientific_framings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, pure_scientific_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, public_opinion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organization that maintains and sets the Doomsday Clock. It benefits from the clock's unique authority, which stems from its blend of scientific input and normative judgment, allowing it to shape global discourse on existential risks without being subject to purely scientific or political accountability.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from having a globally recognized, impactful symbol that focuses attention on existential threats. However, it also inherits the ambiguity of the clock's legitimacy, making it harder to establish clear, actionable consensus based on a single epistemic foundation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, global_risk_discourse, beneficiary,
    institutional, generational, mobile, global).

% Seeks clear, empirical risk assessments and transparent methodologies. It 'pays' by having its efforts to establish purely scientific consensus entangled with the clock's normative judgments, which can dilute scientific authority or complicate policy recommendations.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, pure_scientific_community, payer,
    organized, biographical, constrained, global).

% Can leverage the clock's symbolic power to galvanize public support for policy initiatives related to existential risks. However, the clock's ambiguous scientific grounding can also make it challenging to translate its warnings into specific, evidence-based policy actions.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers, beneficiary,
    institutional, immediate, mobile, global).

% Receives a simplified, impactful message about global threats, which can foster awareness and urgency. However, without a clear understanding of the clock's scientific versus normative basis, public opinion may be susceptible to misinterpretation, alarmism, or apathy, making it harder to engage constructively with complex risks.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, public_opinion, payer,
    powerless, immediate, constrained, global).

% Represents the desire for transparent, verifiable metrics and clear lines of responsibility in risk assessment. This demand is structurally excluded by the clock's hybrid legitimacy, which makes it difficult to pinpoint specific scientific or normative bases for its pronouncements, thereby diffusing accountability.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, clear_accountability_demands, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally recognized symbolic indicator for existential risks, fostering a shared (though ambiguously grounded) understanding and urgency around threats like nuclear war, climate change, and emerging technologies.
% TRANSFER_FUNCTION: Transfers attention and a sense of urgency from the scientific community and policymakers to the global public, while simultaneously transferring a degree of interpretive flexibility and reduced accountability to the clock's custodians (the Bulletin of Atomic Scientists).
% ABSENT_VOICES: Advocates for purely scientific, quantifiable risk metrics, and those demanding clear accountability for risk assessments, are marginalized by the clock's deliberate ambiguity. They would argue for a more transparent and verifiable system, but their calls are absorbed by the hybrid framing.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock and its associated narrative vanished overnight, it would leave a significant void in public discourse around existential risks. New mechanisms would be required to capture global attention and coordinate concern, leading to a rearrangement of how these risks are communicated and perceived, and potentially a fragmentation of public focus.
% FOUNDING_PROBLEM: To communicate the urgent threat of nuclear annihilation to a global public and policymakers in a way that transcends purely scientific reports and mobilizes action, given the unprecedented nature of the threat.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of Atomic Scientists (agenda-setter) asserts the problem is live, citing ongoing and evolving existential threats. Independent risk analysts and historians of science corroborate that the need for a compelling, accessible risk communication tool remains, even if its specific mechanism and legitimacy are debated.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.45) is moderate, reflecting the 'rent' of interpretive flexibility and reduced accountability captured by the agenda-setter, rather than direct financial extraction. `suppression` (0.65) is high because the hybrid framing actively suppresses calls for a purely scientific index or clear accountability. `theater_ratio` (0.40) is moderate, acknowledging the genuine scientific input while recognizing the performative aspect of maintaining the clock's symbolic power and ambiguous legitimacy. `accessibility_collapse` (0.55) reflects the difficulty in accessing purely scientific or purely normative framings of risk due to the entanglement. `resistance` (0.30) is low because the ambiguity makes direct challenge difficult.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bulletin of Atomic Scientists, the hybrid legitimacy is a necessary and effective means of communicating complex, existential threats. From the perspective of the pure scientific community or accountability advocates, this same hybridity represents an opacity that hinders clear assessment and responsibility. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of Atomic Scientists benefits from the clock's unique authority and interpretive flexibility (low d). Global risk discourse also benefits from a focal point (low d). The pure scientific community and public opinion bear the cost of the ambiguity and lack of clear metrics (high d). Clear accountability demands are structurally excluded, their concerns suppressed by the very nature of the hybrid legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to communicate existential risk effectively. The hybrid legitimacy approach, while effective in gaining attention, creates an 'accountability void' where the scientific basis and normative judgments are intertwined. This prevents mislabeling it as pure extraction, as there is a genuine coordination function, but highlights the cost of that coordination in terms of transparency and accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_normative_balance,
    'What is the precise weighting of scientific judgment versus normative stakes in the Doomsday Clock''s setting process?',
    'Detailed ethnographic study of the Bulletin''s Science and Security Board deliberations, or a formal audit of the inputs and decision criteria.',
    'If the balance is heavily skewed towards normative stakes, the clock''s scientific legitimacy would be further eroded, potentially reclassifying it closer to a Snare for the scientific community. If heavily scientific, its communication impact might be reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_normative_balance, empirical, 'The relative contribution of scientific data vs. normative values in clock setting.').

omega_variable(
    ambiguity_function_vs_shield,
    'Does the clock''s deliberate ambiguity primarily serve a necessary communication function (e.g., simplifying complex risks for public engagement) or primarily shield its custodians from accountability for specific scientific claims or policy recommendations?',
    'Comparative analysis of public engagement and policy outcomes in contexts with and without such hybrid communication tools, or a survey of expert and public perceptions of the clock''s role.',
    'If primarily a shield, the constraint''s extractiveness and suppression would be higher, pushing it closer to a Snare. If primarily a communication function, its coordination benefits would be emphasized, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_function_vs_shield, conceptual, 'The primary purpose of the clock''s hybrid legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(doom_tr_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(doom_tr_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(doom_be_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(doom_be_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(doom_su_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(doom_su_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(doom_su_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, climate_change_policy_framing).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, ai_governance_discourse).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'doomsday_clock_metric' kernel, focusing on its hybrid scientific-normative legitimacy. It coexists with the 'objective_index_reading' and 'performative_tool_reading', which emphasize different aspects of the clock's function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
