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
 *   Clock, where its setting is understood as tracking measurable existential
 *   risk levels through expert synthesis of empirical indicators. This
 *   reading emphasizes scientific objectivity and expert authority, often
 *   suppressing alternative normative or political framings of risk. The
 *   structural delta highlights high suppression of normative framing, with
 *   scientific authority as the beneficiary and democratic accountability as
 *   the victim due to an expert monopoly on interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.85).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.9).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, snare).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock: Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'aa6c2976-0a72-4371-a298-16879c13e18e').
narrative_ontology:cs_kernel_codification('aa6c2976-0a72-4371-a298-16879c13e18e', formalized).
narrative_ontology:cs_authority_grounding('aa6c2976-0a72-4371-a298-16879c13e18e', extraction).
narrative_ontology:cs_interpretation_layer_present('aa6c2976-0a72-4371-a298-16879c13e18e').
narrative_ontology:cs_reading_relation('aa6c2976-0a72-4371-a298-16879c13e18e', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_reading_relation('aa6c2976-0a72-4371-a298-16879c13e18e', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_axiom('aa6c2976-0a72-4371-a298-16879c13e18e', foundational, existential_risk_quantifiable_objectively).
narrative_ontology:cs_axiom_status(existential_risk_quantifiable_objectively, holdable).
narrative_ontology:cs_axiom_grounding('aa6c2976-0a72-4371-a298-16879c13e18e', existential_risk_quantifiable_objectively, empirically_contingent).
narrative_ontology:cs_axiom('aa6c2976-0a72-4371-a298-16879c13e18e', foundational, expert_consensus_is_epistemically_privileged).
narrative_ontology:cs_axiom_status(expert_consensus_is_epistemically_privileged, holdable).
narrative_ontology:cs_axiom_grounding('aa6c2976-0a72-4371-a298-16879c13e18e', expert_consensus_is_epistemically_privileged, conventional).
narrative_ontology:cs_reference_frame('aa6c2976-0a72-4371-a298-16879c13e18e', pure_scientific_objectivity).
narrative_ontology:cs_drift_state('aa6c2976-0a72-4371-a298-16879c13e18e', contemporary_science_communication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa6c2976-0a72-4371-a298-16879c13e18e', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability_advocates).
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

% The institutional body that convenes experts and publishes the Doomsday Clock setting. It benefits from the perceived objectivity and authority of the clock, which reinforces its role as a primary arbiter of global risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% The group of scientists and scholars whose expertise is synthesized to set the clock. They gain status, funding, and influence from their privileged position as interpreters of existential risk, even if their individual contributions are genuinely empirical.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_community, beneficiary,
    organized, biographical, constrained, global).

% Advocates who argue for broader public and democratic input into risk assessment and policy. They bear the cost of having a critical normative framing of risk suppressed by the 'objective' scientific framing, limiting avenues for public deliberation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability_advocates, payer,
    powerless, generational, constrained, national).

% The general public and the broader conversation around global risks. It is presented with the clock's setting as an objective fact, which can limit critical engagement with the underlying assumptions and normative choices embedded in the 'objective' index.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_discourse, payer,
    powerless, immediate, trapped, global).

% Government officials and international bodies who can leverage the scientific authority of the Doomsday Clock to justify policy decisions or mobilize public support for specific agendas, without necessarily engaging with the normative underpinnings.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, beneficiary,
    powerful, biographical, mobile, national).

% Scholars who analyze the epistemic claims of the Doomsday Clock, questioning the possibility of purely objective risk measurement and the suppression of normative dimensions. They operate outside the direct influence of the clock's authority structure.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_epistemologists, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizes diverse expert assessments into a single, comprehensible indicator of global catastrophic risk, providing a focal point for scientific consensus on existential threats.
% TRANSFER_FUNCTION: Transfers interpretive authority over existential risk from broader public and democratic processes to a specialized scientific and expert community, consolidating the power to define and prioritize threats.
% ABSENT_VOICES: Normative ethicists, philosophers of science, and representatives of communities disproportionately affected by certain risks are largely absent from the core clock-setting process, as their input would challenge the 'objective' framing. They would argue for explicit value-laden discussions.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock, as an objective index, vanished overnight, the scientific authority that underpins it would be significantly challenged. The public discourse around existential risk would become more fragmented, potentially opening space for more diverse framings, but also losing a central, authoritative (if contested) reference point. The expert community would need to find new mechanisms to assert its interpretive monopoly.
% FOUNDING_PROBLEM: The original problem was to alert humanity to the dangers of nuclear war and other global catastrophic risks, providing a clear, symbolic measure of proximity to global disaster.
% FOUNDING_PROBLEM_CORROBORATION: The scientific authority and expert community involved in setting the clock attest that the founding problem of communicating existential risk remains live and urgent. However, critical epistemologists and democratic accountability advocates argue that while the *problem* is live, the *solution* has drifted from objective assessment to a mechanism for expert power consolidation, a claim supported by analysis of the clock's framing and its impact on public discourse.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the transfer of interpretive authority from public discourse to a specialized expert community, effectively extracting the right to define and prioritize global risks. Suppression (0.90) is very high because this reading actively marginalizes or dismisses normative and political framings of risk, insisting on a purely empirical approach. The theater ratio (0.45) is moderate; while genuine scientific work is involved, there's a performative aspect in maintaining the facade of pure objectivity to legitimize the expert monopoly. Accessibility collapse (0.80) is high for non-experts to challenge the underlying assumptions, and resistance (0.20) is low due to the deference given to scientific authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the scientific authority and expert community, the clock is a vital coordination mechanism for objective risk assessment. From the perspective of democratic accountability advocates and public discourse, it functions as a snare, using the guise of objectivity to extract interpretive power and suppress broader participation in risk governance. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The scientific authority and expert community are clear beneficiaries (low directionality) as they gain status, influence, and control over the risk narrative. Democratic accountability advocates and public discourse are the primary targets (high directionality) as their capacity for independent risk framing and deliberation is suppressed. Policy makers are also beneficiaries, as they can leverage the clock's authority. Critical epistemologists act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_vs_normativity_boundary,
    'To what extent is the ''objective index'' truly free from normative assumptions and framing choices, or does it implicitly embed specific values?',
    'Detailed content analysis of expert deliberations and public statements, combined with philosophical analysis of the fact-value distinction in risk assessment. Comparison with alternative risk frameworks that explicitly declare their normative bases.',
    'If significant normative embedding is found, the claim of pure objectivity is undermined, reclassifying the constraint closer to a ''tangled_rope'' or even a ''performative_tool'' from the perspective of public discourse, as the coordination story becomes less credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(objectivity_vs_normativity_boundary, conceptual, 'Ambiguity of the fact-value boundary in existential risk assessment.').

omega_variable(
    expert_monopoly_impact,
    'Does the expert monopoly on interpretation genuinely enhance effective risk mitigation, or does it stifle diverse perspectives and potential solutions from broader societal engagement?',
    'Comparative studies of risk governance outcomes in domains with varying levels of expert vs. public participation. Analysis of ''blind spots'' in expert-only assessments that are later identified by broader input.',
    'If the monopoly is shown to hinder effective mitigation or introduce biases, the ''snare'' classification is strongly reinforced, highlighting the costs to democratic accountability. If it demonstrably leads to superior outcomes, the coordination function is strengthened, potentially shifting towards a ''tangled_rope'' with high but justified extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_monopoly_impact, empirical, 'Impact of expert interpretive monopoly on risk governance efficacy.').

omega_variable(
    reading_foreclosure_validity,
    'Is the ''objective_index_reading'' truly logically foreclosing of the ''hybrid_legitimacy_reading'' and ''performative_tool_reading'', or can these perspectives coexist within a more complex understanding of the clock?',
    'Philosophical analysis of the logical consistency of holding these different premises simultaneously within a single coherent framework for risk communication. Examination of how different actors actually reconcile (or fail to reconcile) these views.',
    'If coexistence is possible, the ''forecloses'' relation would be reclassified to ''coexists_with'', indicating a more pluralistic and less rigid kernel contest. This would reduce the ''suppression'' metric for this reading, as it would no longer be structurally denying alternative framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_validity, conceptual, 'Logical consistency of the ''objective index'' claim with alternative framings of the Doomsday Clock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(doom_tr_t6, doomsday_clock_metric__objective_index_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(doom_tr_t12, doomsday_clock_metric__objective_index_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(doom_tr_t18, doomsday_clock_metric__objective_index_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__objective_index_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__objective_index_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(doom_be_t6, doomsday_clock_metric__objective_index_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(doom_be_t12, doomsday_clock_metric__objective_index_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(doom_be_t18, doomsday_clock_metric__objective_index_reading, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__objective_index_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__objective_index_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(doom_su_t6, doomsday_clock_metric__objective_index_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(doom_su_t12, doomsday_clock_metric__objective_index_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(doom_su_t18, doomsday_clock_metric__objective_index_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__objective_index_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__objective_index_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
