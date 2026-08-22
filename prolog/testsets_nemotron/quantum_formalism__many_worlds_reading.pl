% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The many-worlds reading of the quantum formalism asserts that the
 *   universal wavefunction evolves deterministically under the Schrödinger
 *   equation at all times and all scales; what we call 'measurement' is the
 *   process of decoherence-induced branching where all outcomes are realized
 *   in effectively non-interacting branches. This reading eliminates the
 *   collapse postulate, removes observers from fundamental physics, and
 *   extends quantum mechanics to cosmology. It is claimed as a mountain — a
 *   consequence of taking the formalism literally — but carries low-level
 *   extraction in the form of career pressures on non-adherents in quantum
 *   foundations and pedagogical framing effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.12).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.03).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '0b297f24-a014-4548-81ae-2f3fe2fcb099').
narrative_ontology:cs_kernel_codification('0b297f24-a014-4548-81ae-2f3fe2fcb099', formalized).
narrative_ontology:cs_authority_grounding('0b297f24-a014-4548-81ae-2f3fe2fcb099', expertise).
narrative_ontology:cs_interpretation_layer_present('0b297f24-a014-4548-81ae-2f3fe2fcb099').
narrative_ontology:cs_reading_relation('0b297f24-a014-4548-81ae-2f3fe2fcb099', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b297f24-a014-4548-81ae-2f3fe2fcb099', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('0b297f24-a014-4548-81ae-2f3fe2fcb099', foundational, universal_unitarity_no_exceptions).
narrative_ontology:cs_axiom_status(universal_unitarity_no_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('0b297f24-a014-4548-81ae-2f3fe2fcb099', universal_unitarity_no_exceptions, empirically_contingent).
narrative_ontology:cs_axiom('0b297f24-a014-4548-81ae-2f3fe2fcb099', foundational, measurement_emergent_from_decoherence).
narrative_ontology:cs_axiom_status(measurement_emergent_from_decoherence, holdable).
narrative_ontology:cs_axiom_grounding('0b297f24-a014-4548-81ae-2f3fe2fcb099', measurement_emergent_from_decoherence, empirically_contingent).
narrative_ontology:cs_axiom('0b297f24-a014-4548-81ae-2f3fe2fcb099', secondary, probability_from_self_locating_uncertainty).
narrative_ontology:cs_axiom_status(probability_from_self_locating_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('0b297f24-a014-4548-81ae-2f3fe2fcb099', probability_from_self_locating_uncertainty, empirically_contingent).
narrative_ontology:cs_reference_frame('0b297f24-a014-4548-81ae-2f3fe2fcb099', everett_relative_state_formulation).
narrative_ontology:cs_drift_state('0b297f24-a014-4548-81ae-2f3fe2fcb099', post_decoherence_and_quantum_cosmology, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0b297f24-a014-4548-81ae-2f3fe2fcb099', '2026-06-11T14:30:00Z').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, many_worlds_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_foundations_community).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, copenhagen_adherents).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, pilot_wave_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, unitary_determinism_universal).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, observer_eliminable_from_fundamental_physics).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, measurement_as_decoherence).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, ontological_parsimony_via_eliminating_collapse_postulate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and defend the many-worlds interpretation as the natural consequence of taking unitary quantum mechanics seriously at all scales. Benefit from a research program that eliminates the measurement problem without adding postulates. Can shift to other interpretations or subfields without career collapse — the physics community treats interpretation choice as a legitimate scientific disagreement.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, many_worlds_physicists, agenda_setter,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, many_worlds_physicists, beneficiary).

% Gain a clear, well-defined research program: derive the Born rule from decision theory or self-locating uncertainty, solve the preferred basis problem via decoherence, extend quantum mechanics to cosmology without observers. No professional penalty for working on alternatives — the field sustains multiple live programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_foundations_community, beneficiary,
    organized, generational, mobile, global).

% Must engage with many-worlds arguments in publications, hiring, and funding contexts even if they reject the ontology. The interpretive debate shapes what counts as a 'solved problem' in foundations. Exit requires abandoning quantum foundations entirely or accepting the many-worlds framing as the default foil.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_adherents, payer,
    organized, generational, constrained, global).

% Work in a smaller, marginalized research program that many-worlds proponents often dismiss as adding 'superfluous' variables. Must constantly justify why definite particle positions are worth the mathematical cost. Career advancement is harder outside the dominant interpretive frame.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, pilot_wave_researchers, payer,
    moderate, biographical, constrained, global).

% Analyze the structural commitments of each interpretation: ontology, probability, locality, emergence. No professional stake in any single reading winning — the interpretive landscape itself is the object of study.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% Taught quantum mechanics with the measurement postulate as a black box, rarely exposed to the interpretive debate. If they encounter many-worlds, it is often presented as 'the crazy one' without serious engagement. Cannot exit the pedagogical frame until graduate school, if at all.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, physics_students, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unitary dynamical law (Schrödinger equation everywhere, always) that governs all physical systems without exception — eliminating the measurement postulate as a fundamental rule and making quantum mechanics a complete, universally applicable theory.
% TRANSFER_FUNCTION: Transfers explanatory burden from a postulated collapse process (with its preferred basis, trigger, and non-unitarity) to decoherence theory and the problem of probability in a branching universe. The cost is ontological proliferation (all outcomes realized); the benefit is dynamical unity and observer elimination.
% ABSENT_VOICES: Physics students and early-career researchers who inherit the pedagogical frame without ever encountering the interpretive choice as a live option. Also excluded: philosophers and physicists who reject the very category of 'interpretation' as empirically empty and would dissolve the debate entirely — they are not in the room because the debate presupposes interpretation is a meaningful category.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished overnight, the physics would not change — the Schrödinger equation and decoherence theory remain. But the research program deriving probability from branching, the cosmological applications assuming universal unitarity, and the default framing of 'the measurement problem' in foundations would lose their central organizing hypothesis. Copenhagen and pilot-wave proponents would claim vindication; many-worlds adherents would call it a sociological shift, not an empirical one.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics postulates two incompatible dynamical laws (unitary evolution + stochastic collapse) with no physical criterion for when each applies. The collapse postulate introduces observers as fundamental, breaks unitarity, and cannot be applied to the universe as a whole.
% FOUNDING_PROBLEM_CORROBORATION: Everett (1957) and DeWitt/Graham (1973) attest the measurement problem as the founding motivation. Zurek's decoherence program (1980s-present) corroborates that environment-induced superselection solves the preferred basis problem without collapse — corroboration from outside the many-worlds beneficiary set. Critics (e.g., Bell, Maudlin) attest the problem is real but argue many-worlds replaces it with the probability problem rather than solving it.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__many_worlds_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because no one is forced to accept many-worlds — it is a live interpretive option among others. Suppression is negligible (0.03): alternatives (Copenhagen, pilot wave, QBism, collapse models) are actively researched and published. Theater ratio is low (0.08): the interpretive debate is genuine, not performative. Accessibility collapse is very high (0.92): once you accept unitary-only quantum mechanics, the branching structure follows mathematically — alternatives require adding postulates. Resistance is low (0.15): the reading has grown from marginal to mainstream without coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the many-worlds seat, the constraint is a mountain — the formalism itself, taken literally. From the pilot-wave seat, it is a rope with extraction — a coordination frame that marginalizes their program. From the student seat, it is a snare — a pedagogical trap that presents one interpretation as the only serious option. The engine computes these per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Many-worlds physicists are agenda-setters and beneficiaries: they define the research program and gain a coherent framework. Copenhagen and pilot-wave researchers are payers: they must engage with many-worlds as the dominant foil, and pilot-wave researchers face marginalization. Physics students are excluded: they inherit a pedagogical frame that obscures the interpretive choice. Philosophers of physics are observers: they study the structure without professional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurement problem) remains contested: decoherence solves the preferred basis problem but the probability problem (Born rule derivation) is unresolved. The reading persists not because the founding problem is solved, but because the unitary-only framework generates productive research (decoherence, quantum cosmology, quantum information). No mandatrophy — the arrangement continues to do epistemic work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_problem_resolution,
    'Can the Born rule be derived from unitary quantum mechanics plus decision theory / self-locating uncertainty, or does it require an additional postulate?',
    'Consensus in the philosophy of physics community on whether the Deutsch-Wallace decision-theoretic derivation or the Sebens-Carroll self-locating uncertainty derivation succeeds without circularity.',
    'If the Born rule derivation fails, many-worlds must add a probability postulate — increasing extractiveness (extra postulate) and potentially shifting classification toward tangled_rope. If it succeeds, the mountain claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(probability_problem_resolution, conceptual, 'Whether the probability problem is a genuine gap or a solved problem in the many-worlds reading.').

omega_variable(
    preferred_basis_objectivity,
    'Does decoherence select a unique, objective preferred basis, or is basis selection environment-relative and thus perspectival?',
    'Theoretical analysis of whether different environment partitions yield the same pointer states, and whether cosmological decoherence (no external environment) defines a unique basis.',
    'If basis is perspectival, the branching structure is not objectively defined — the mountain claim (emerges_naturally) weakens. If objective, the branching is a genuine structural feature of the universal wavefunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preferred_basis_objectivity, empirical, 'Objectivity of the preferred basis in decoherence theory.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the many-worlds reading''s core premise (universal unitarity) logically foreclose the Copenhagen reading''s core premise (physical collapse), or do they coexist as different frameworks?',
    'Analysis of whether a single theoretical framework could consistently incorporate both unitary-only evolution at the fundamental level and physical collapse at the phenomenological level (e.g., objective collapse models).',
    'If they coexist, the relation is ''coexists_with'' — different parties hold different frameworks. If many-worlds forecloses Copenhagen within any single framework, the relation is ''forecloses'' — structural displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between many-worlds and Copenhagen core premises.').

omega_variable(
    committer_frame_naturalness,
    'Is the many-worlds reading a genuine natural consequence of the quantum formalism (mountain), or a constructed interpretive choice that benefits identifiable agents (false summit)?',
    'Track whether the reading''s dominance correlates with empirical progress (decoherence, cosmology, quantum information) or with sociological factors (institutional prestige, pedagogical inertia, funding flows).',
    'If false summit, FSM signature triggers reclassification to tangled_rope. The declared beneficiaries (many_worlds_physicists, quantum_foundations_community) are the candidate beneficiary groups for FSM evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_naturalness, preference, 'Natural-law vs. constructed-status ambiguity for FSM evaluation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mw_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.02).
narrative_ontology:measurement(mw_tr_t1970, quantum_formalism__many_worlds_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(mw_tr_t1985, quantum_formalism__many_worlds_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(mw_tr_t2000, quantum_formalism__many_worlds_reading, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(mw_tr_t2015, quantum_formalism__many_worlds_reading, theater_ratio, 2015, 0.07).
narrative_ontology:measurement(mw_tr_t2026, quantum_formalism__many_worlds_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(mw_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.05).
narrative_ontology:measurement(mw_be_t1970, quantum_formalism__many_worlds_reading, base_extractiveness, 1970, 0.06).
narrative_ontology:measurement(mw_be_t1985, quantum_formalism__many_worlds_reading, base_extractiveness, 1985, 0.08).
narrative_ontology:measurement(mw_be_t2000, quantum_formalism__many_worlds_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(mw_be_t2015, quantum_formalism__many_worlds_reading, base_extractiveness, 2015, 0.11).
narrative_ontology:measurement(mw_be_t2026, quantum_formalism__many_worlds_reading, base_extractiveness, 2026, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mw_su_t1957, quantum_formalism__many_worlds_reading, suppression_requirement, 1957, 0.01).
narrative_ontology:measurement(mw_su_t1970, quantum_formalism__many_worlds_reading, suppression_requirement, 1970, 0.02).
narrative_ontology:measurement(mw_su_t1985, quantum_formalism__many_worlds_reading, suppression_requirement, 1985, 0.02).
narrative_ontology:measurement(mw_su_t2000, quantum_formalism__many_worlds_reading, suppression_requirement, 2000, 0.03).
narrative_ontology:measurement(mw_su_t2015, quantum_formalism__many_worlds_reading, suppression_requirement, 2015, 0.03).
narrative_ontology:measurement(mw_su_t2026, quantum_formalism__many_worlds_reading, suppression_requirement, 2026, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.02).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Quantum formalism kernel decomposes into three readings with distinct ε and structural profiles: many-worlds (ε≈0.12, mountain) derives measurement from decoherence; Copenhagen (ε≈0.05, mountain) postulates collapse as fundamental; pilot-wave (ε≈0.25, tangled_rope) adds hidden variables. They share the same mathematical core (Schrödinger equation) but differ on ontology, probability, and the status of measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
