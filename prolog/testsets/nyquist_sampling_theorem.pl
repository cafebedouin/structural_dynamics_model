% ============================================================================
% CONSTRAINT STORY: nyquist_sampling_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nyquist_sampling_theorem, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nyquist_sampling_theorem
 *   human_readable: Nyquist Sampling Theorem
 *   domain: signal_processing/mathematics
 *
 * SUMMARY:
 *   The Nyquist sampling theorem establishes that a bandlimited signal
 *   containing no frequencies higher than f_max must be sampled at a rate of
 *   at least 2f_max to avoid aliasing and enable lossless reconstruction.
 *   This is a mathematical law, not a social arrangement, institution, or
 *   coercive mechanism. No agent benefits from the theorem; no agent is
 *   victimized by it. All perspectives perceive it identically as an
 *   irreducible constraint on information transmission. The theorem's
 *   universality, necessity, and mathematical proof make it the canonical
 *   example of a Mountain in the Deferential Realism system. The minimal
 *   extractiveness (0.12) and suppression (0.03) reflect that the constraint
 *   imposes no coercive overhead — agents align with Nyquist not because they
 *   are forced but because it is impossible to transmit bandlimited signals
 *   through sampling without respecting the bound.
 *
 * KEY AGENTS:
 *   - Bandlimited Signal: The information source — constrained by its own frequency content; cannot be sampled below Nyquist without loss
 *   - Signal Processor/Engineer: The agent implementing sampling — must choose a sample rate; constrained by the theorem to meet Nyquist or accept artifacts
 *   - Information Channel: The medium through which samples are transmitted — Nyquist defines the information capacity of any sampled pathway
 *   - Analytical Observer: The mathematical perspective — perceives the theorem as a natural law derivable from Fourier analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nyquist_sampling_theorem, 0.12).
domain_priors:suppression_score(nyquist_sampling_theorem, 0.03).
domain_priors:theater_ratio(nyquist_sampling_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nyquist_sampling_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(nyquist_sampling_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(nyquist_sampling_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nyquist_sampling_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nyquist_sampling_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nyquist_sampling_theorem, mountain).
narrative_ontology:human_readable(nyquist_sampling_theorem, "Nyquist Sampling Theorem").
narrative_ontology:topic_domain(nyquist_sampling_theorem, "signal_processing/mathematics").

domain_priors:emerges_naturally(nyquist_sampling_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SIGNAL WITHOUT SUFFICIENT BANDWIDTH (MOUNTAIN) — Any signal attempting to evade the Nyquist bound by undersampling faces inevitable aliasing. This is not a negotiable constraint; it is a mathematical limit inherent to the sampling process itself. No escape, no alternative, no transcendence possible.
constraint_indexing:constraint_classification(nyquist_sampling_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER DESIGNING SAMPLE RATE (MOUNTAIN) — Even with substantial resources and expertise, the engineer cannot violate the theorem. The constraint appears as a technical requirement, not an obstacle — meet the Nyquist criterion or accept aliasing artifacts. Suppression is the mathematical structure itself, not coercive overhead.
constraint_indexing:constraint_classification(nyquist_sampling_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the vantage of signal theory, the Nyquist theorem is a natural law of information transmission. Fourier analysis guarantees that bandlimited signals require a minimum sampling rate (2f_max) for lossless reconstruction. This is not institutional, not coercive, not extractive — it is the structure of reality.
constraint_indexing:constraint_classification(nyquist_sampling_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: SIGNAL PROCESSING STANDARDS BODY (MOUNTAIN) — Even institutional actors with power to set standards cannot codify a violation of Nyquist. Standards documents incorporate the theorem as a natural law, not as an enforceable rule. No suppression because no enforcement is needed — the mathematics enforces itself.
constraint_indexing:constraint_classification(nyquist_sampling_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nyquist_sampling_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nyquist_sampling_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nyquist_sampling_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nyquist_sampling_theorem, ExtMetricName, E),
    domain_priors:suppression_score(nyquist_sampling_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nyquist_sampling_theorem),
    narrative_ontology:constraint_metric(nyquist_sampling_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nyquist_sampling_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nyquist_sampling_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The theorem has no extractive function — it does not redistribute resources, does not concentrate power, does not create asymmetric obligations. The value reflects that there is minimal coercive overhead; the constraint is so fundamental that it appears as pure mathematics rather than as a constraint mechanism. The slight non-zero value accounts for the trivial overhead of documentation, education, and engineering practice around implementing Nyquist. Suppression (0.03): Minimal. No agent is suppressed by the theorem — all agents with technical knowledge can access the mathematics and understand it fully. No alternative is hidden; no exit is blocked by institutional authority. The value reflects only the inherent difficulty of the underlying mathematics (the theorem requires some technical sophistication to fully grasp). Theater ratio (0.15): Very low. There is no performative element to the Nyquist theorem — it either holds or it does not. Implementation (sample rate selection, filter design) may involve some ritual around standards compliance, hence the minimal non-zero value, but the theorem itself has zero theater. No institution needs to maintain the appearance of Nyquist's validity because the mathematics guarantees it.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for the Nyquist theorem. All perspectives agree: it is a mountain, a natural law. The engineer and the signal both perceive the same constraint. The standards body cannot alter it. The analytical observer sees mathematical necessity. Uniform-type constraints like Nyquist are invariant across all indexical positions — the theorem's universality is its defining feature. The absence of perspectival disagreement is strong evidence that the classification is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nyquist theorem has no directionality in the sense defined by the DR framework. There is no beneficiary (no agent who profits from the constraint) and no victim (no agent who bears a cost imposed by others). All agents face the same mathematical reality equally: signals cannot be reconstructed from undersampled data. The absence of directionality is a defining characteristic of Mountains. The theorem does not extract from anyone; it is a property of reality that applies uniformly across all contexts and perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: NOT APPLICABLE. The Nyquist theorem is a Mountain — mathematical law with no institutional form to degrade. The mandatrophy arises when a constraint's claimed type matches its structural data but misses contingent institutional arrangements (Piton disguised as Mountain). Nyquist has no institutional form to degrade. Its accessibility (mathematical proof available universally), resistance to falsification (Fourier analysis guarantees the bound), and natural emergence (from first principles of information theory) all confirm the mountain classification. The accessibility collapse (0.92) reflects that the theorem is difficult to fully internalize without mathematical training, but once understood, it is universally accessible. The resistance (0.08) reflects the mathematical certainty of the proof — there is minimal intellectual resistance to accepting the conclusion once the premises are granted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finite_precision_approximation,
    'Is the Nyquist theorem a true mathematical law or a limit derived from idealized assumptions (infinite precision, continuous time, infinite observation windows)?',
    'Analysis of real-world sampling in finite precision arithmetic; examination of whether practical ''violations'' (sub-Nyquist sampling with prior constraints) constitute true exceptions or approximations valid only under restricted conditions',
    'If idealized: the mountain classification holds universally. If approximation: the constraint transitions to Piton (degraded from its idealized form under real-world conditions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finite_precision_approximation, conceptual, 'Whether Nyquist is absolute mathematical law or limit from idealized assumptions').

omega_variable(
    prior_information_loophole,
    'Does compressive sensing (sub-Nyquist sampling with sparsity priors) represent a genuine violation of Nyquist or a different application of information theory that does not contradict the theorem?',
    'Rigorous comparison of information-theoretic requirements: CS requires prior knowledge (sparsity basis) that Nyquist assumes absent. Does CS reduce the effective information rate (and thus the effective Nyquist bound) or transcend the bound?',
    'If CS transcends: Nyquist is not a universal law — reclassify as Rope (pragmatic coordination around sampling conventions). If CS complies: Nyquist holds universally — mountain confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_information_loophole, conceptual, 'Whether compressive sensing violates or complies with Nyquist principle').

omega_variable(
    domain_specificity,
    'Is Nyquist a law of signal processing or a mathematical theorem specific to bandlimited signals and equidistant sampling?',
    'Examine constraints that appear to violate Nyquist (non-equidistant sampling, non-bandlimited signals, quantum systems). Determine whether each represents a true exception or a different domain with different constraints.',
    'If domain-specific: mountain classification is preserved for bandlimited signals but does not generalize. If universal: mountain classification applies across all signal classes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_specificity, conceptual, 'Domain specificity of Nyquist theorem applicability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nyquist_sampling_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nyqu_tr_t0, nyquist_sampling_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nyqu_tr_t50, nyquist_sampling_theorem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(nyqu_tr_t100, nyquist_sampling_theorem, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(nyqu_be_t0, nyquist_sampling_theorem, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nyqu_be_t50, nyquist_sampling_theorem, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(nyqu_be_t100, nyquist_sampling_theorem, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nyquist_sampling_theorem, information_standard).
narrative_ontology:affects_constraint(nyquist_sampling_theorem, signal_reconstruction_fidelity).
narrative_ontology:affects_constraint(nyquist_sampling_theorem, analog_digital_conversion).
narrative_ontology:affects_constraint(nyquist_sampling_theorem, data_compression_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
