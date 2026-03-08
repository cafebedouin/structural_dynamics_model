% ============================================================================
% CONSTRAINT STORY: measurement_apparatus_bidirectionality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_measurement_apparatus_bidirectionality, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: measurement_apparatus_bidirectionality
 *   human_readable: Measurement Apparatus Bidirectionality
 *   domain: epistemology/systems_theory/labor_studies
 *
 * SUMMARY:
 *   Measurement apparatus bidirectionality is the constraint that sustained
 *   observation through physical contact creates a bidirectional circuit
 *   where the observer becomes part of the observed system's substrate. This
 *   appears across scales: quantum measurement (wavefunction collapse encodes
 *   measurement basis into system state), workplace monitoring (Hawthorne
 *   effects encode observer's attention pattern into worker behavior),
 *   ethnographic fieldwork (researcher's presence and questions encode into
 *   community narratives), algorithmic recommendation (users optimize for
 *   measured metrics, encoding algorithm's attention pattern into content).
 *   The constraint is scale-invariant and appears to be a universal feature
 *   of observation itself, not a correctable bias or extractive mechanism.
 *   The primary observable is the duration and frequency of probe contact
 *   with substrate, and the presence of the observer's attention pattern in
 *   the decoded signal alongside the measured phenomena. This is a mountain
 *   constraint because it emerges naturally from the physics of information
 *   transfer, exhibits high accessibility collapse (no alternative
 *   observation method eliminates bidirectionality), and shows high
 *   resistance to circumvention (reflexivity and methodological awareness
 *   acknowledge but cannot eliminate the effect).
 *
 * KEY AGENTS:
 *   - Observed Worker: Powerless agent (powerless/trapped) experiencing bidirectionality as unavoidable — awareness of observation changes behavior regardless of intent
 *   - Management Observer: Institutional agent (institutional/arbitrage) discovering that measurement apparatus encodes itself into work substrate — productivity metrics shift to reflect what is measured rather than underlying work quality
 *   - Labor Union Negotiator: Organized agent (organized/constrained) recognizing bidirectionality as non-negotiable structural constraint — can negotiate what gets measured but not whether measurement creates bidirectionality
 *   - Ethnographic Researcher: Moderate-power agent (moderate/mobile) experiencing bidirectionality as irreducible methodological constraint — reflexivity acknowledges but cannot eliminate observer effects
 *   - Platform Algorithm Designer: Powerful agent (powerful/arbitrage) discovering Goodhart's Law as physical constraint — users optimize for measured observable, encoding algorithm's attention into content substrate
 *   - Epistemological Analyst: Analytical observer (analytical/analytical) identifying bidirectionality as universal constraint on observation across quantum, social, and computational domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(measurement_apparatus_bidirectionality, 0.08).
domain_priors:suppression_score(measurement_apparatus_bidirectionality, 0.02).
domain_priors:theater_ratio(measurement_apparatus_bidirectionality, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, extractiveness, 0.08).
narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(measurement_apparatus_bidirectionality, mountain).
narrative_ontology:human_readable(measurement_apparatus_bidirectionality, "Measurement Apparatus Bidirectionality").
narrative_ontology:topic_domain(measurement_apparatus_bidirectionality, "epistemology/systems_theory/labor_studies").

domain_priors:emerges_naturally(measurement_apparatus_bidirectionality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVED WORKER (MOUNTAIN) — The worker under sustained observation cannot exit the bidirectional circuit. Their behavior becomes part of the measurement apparatus through the Hawthorne effect, observer-expectancy effects, and performance monitoring feedback loops. This is experienced as an immutable constraint — the worker cannot prevent their awareness of being observed from affecting their behavior, and cannot prevent the observer's attention pattern from becoming encoded in the work output. The bidirectionality is a physical fact of sustained contact, not a policy choice.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MANAGEMENT OBSERVER (MOUNTAIN) — The observer attempting to measure worker productivity discovers that the measurement apparatus itself becomes part of the system. Productivity metrics shift not because underlying work changes, but because workers optimize for the measured observable. The observer's attention pattern (what gets measured, when, how frequently) encodes itself into the work substrate. This is not extraction — it is an epistemic limit. The observer cannot measure without contact, and contact creates bidirectionality.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EPISTEMOLOGICAL ANALYST (MOUNTAIN) — From the analytical perspective, measurement apparatus bidirectionality is a universal constraint on observation. Any sustained observation through physical contact (whether quantum measurement, ethnographic fieldwork, performance monitoring, or therapeutic relationship) creates a circuit where the observer's measurement apparatus becomes part of the observed system's substrate. This is not a bug to be fixed but a structural feature of observation itself. The constraint is scale-invariant: it appears in quantum mechanics (measurement collapse), social science (observer effects), labor monitoring (Hawthorne effects), and therapeutic contexts (transference). The universality and resistance to circumvention mark this as mountain.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: LABOR UNION NEGOTIATOR (MOUNTAIN) — Organized labor recognizes measurement bidirectionality as a structural constraint that cannot be negotiated away. Surveillance systems, productivity tracking, and performance metrics all create bidirectional circuits where workers' awareness of measurement changes their behavior, and management's attention patterns encode themselves into work output. Unions can negotiate WHAT gets measured and HOW, but cannot eliminate the bidirectionality itself. Even with high organizational power and constrained exit options, the union experiences this as mountain — the physics of sustained observation are not subject to collective bargaining.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ETHNOGRAPHIC RESEARCHER (MOUNTAIN) — The field researcher attempting to observe a community discovers that their presence changes what they are measuring. Participant observation creates bidirectionality: the researcher's questions, attention patterns, and presence become part of the community's substrate. Informants perform for the researcher, narratives shift to accommodate the observer's framework, and the researcher's own positionality encodes itself into the data. This is recognized as an irreducible methodological constraint, not a correctable bias. Reflexivity and positionality statements acknowledge the bidirectionality but cannot eliminate it.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: PLATFORM ALGORITHM DESIGNER (MOUNTAIN) — The designer of recommendation algorithms and engagement metrics discovers that users optimize for the measured observable (clicks, time-on-site, shares) rather than the latent quality the metric was meant to proxy. The measurement apparatus (the algorithm's attention pattern) becomes part of the content substrate — creators produce content optimized for algorithmic legibility rather than human value. This is Goodhart's Law as a physical constraint: when a measure becomes a target, it ceases to be a good measure, because the bidirectionality between measurement and substrate is unavoidable. Even with arbitrage exit options and powerful position, the designer cannot eliminate the circuit.
constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(measurement_apparatus_bidirectionality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(measurement_apparatus_bidirectionality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, ExtMetricName, E),
    domain_priors:suppression_score(measurement_apparatus_bidirectionality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(measurement_apparatus_bidirectionality),
    narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(measurement_apparatus_bidirectionality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(measurement_apparatus_bidirectionality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint extracts minimal value from any agent — it is an epistemic limit, not a rent-seeking mechanism. The small non-zero value reflects that bidirectionality does impose costs (researchers must account for observer effects, workers experience surveillance stress, platforms deal with metric gaming) but these are inherent costs of observation, not asymmetric extraction. Suppression (0.02): Very low. There are no meaningful alternatives being suppressed — all observation methods that involve sustained contact exhibit bidirectionality. Brief, non-contact observation (e.g., passive sensors, historical records) can reduce bidirectionality but cannot eliminate it for phenomena requiring sustained engagement. The constraint does not suppress alternatives; it IS the alternative space. Theater ratio (0.15): Low. Acknowledgment of bidirectionality (reflexivity statements, methodological notes, Hawthorne effect disclaimers) is mostly functional rather than performative. Researchers genuinely attempt to account for observer effects; the theater component is the residual pretense that these acknowledgments 'correct' for bidirectionality when they merely document it. Accessibility collapse (0.92): Very high. No observation method eliminates bidirectionality for sustained contact. Quantum measurement, ethnographic fieldwork, workplace monitoring, and algorithmic recommendation all exhibit the same structural feature through different physical mechanisms. Resistance (0.08): Very low. Attempts to circumvent bidirectionality (covert observation, passive sensors, historical analysis) either fail (covert observation still creates circuits once discovered) or change the phenomenon being measured (passive sensors cannot measure phenomena requiring interaction).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap — all six perspectives classify as mountain. The powerless observed worker, the institutional management observer, the organized labor negotiator, the moderate ethnographic researcher, the powerful platform designer, and the analytical epistemologist all experience measurement apparatus bidirectionality as an immutable constraint. This uniformity is diagnostic: it confirms that the constraint is a genuine natural law rather than a contingent institutional arrangement being naturalized. The resistance to circumvention is universal across power levels, time horizons, exit options, and spatial scopes. The constraint's scale-invariance (appearing identically in quantum mechanics, social science, labor studies, and platform design) further supports the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims because it is a universal epistemic limit, not an extractive mechanism. All agents experience bidirectionality as an immutable constraint regardless of their power level or exit options. The observer cannot measure without contact, and contact creates bidirectionality. The observed cannot prevent their awareness of observation from affecting their behavior. The analytical observer recognizes this as a structural feature of information transfer itself. Directionality values are not applicable — the constraint does not extract from some agents to benefit others; it constrains all observation equally.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint resolves the mandatrophy by demonstrating that not all constraints are extractive. Measurement apparatus bidirectionality is a genuine epistemic limit — it emerges naturally from the physics of information transfer, exhibits high accessibility collapse, and shows high resistance to circumvention. The constraint does not suppress alternatives (there are no alternative observation methods that eliminate bidirectionality for sustained contact), does not extract asymmetrically (all agents are equally constrained), and does not require active enforcement (it is self-enforcing through the structure of observation itself). The mountain classification is not a naturalization of contingent power relations but an accurate description of a universal constraint on observation. The omega variables address empirical uncertainties (contact duration thresholds, substrate coupling mechanisms, observer awareness necessity) but even if these resolve toward shorter thresholds or contingent mechanisms, the core constraint remains: sustained observation through physical contact creates bidirectionality. This is what a mountain looks like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contact_duration_threshold,
    'What duration and frequency of observation contact is required before bidirectionality becomes structurally significant?',
    'Empirical measurement of observer effect magnitude as a function of contact duration across domains (quantum measurement, workplace monitoring, ethnographic fieldwork, therapeutic relationships). Identification of threshold below which bidirectionality is negligible and above which it dominates signal.',
    'If threshold is very short (milliseconds to seconds): bidirectionality is nearly universal and mountain classification is robust. If threshold is long (months to years): many observation contexts avoid bidirectionality through brief contact, and mountain classification applies only to sustained observation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contact_duration_threshold, empirical, 'Duration threshold for structurally significant bidirectionality').

omega_variable(
    substrate_coupling_mechanism,
    'Is bidirectionality a universal physical constraint or a contingent feature of specific measurement technologies?',
    'Cross-domain analysis of observation mechanisms. If bidirectionality appears in quantum measurement (unavoidable physical coupling), social observation (unavoidable cognitive coupling), and algorithmic measurement (unavoidable optimization coupling) through different physical mechanisms, it is universal. If it appears only in specific technologies, it is contingent.',
    'If universal: mountain classification is correct across all scales. If contingent: some observation technologies may achieve unidirectional measurement, and mountain classification applies only to current technology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substrate_coupling_mechanism, conceptual, 'Whether bidirectionality is universal or technology-contingent').

omega_variable(
    observer_awareness_necessity,
    'Does bidirectionality require the observed agent to be aware of observation, or does it occur even with covert measurement?',
    'Comparison of observer effects in aware vs unaware observation contexts. Quantum measurement (no awareness required) vs workplace monitoring (awareness-dependent Hawthorne effects) vs covert ethnography (ethical issues prevent clean test).',
    'If awareness is necessary: bidirectionality is a cognitive/social constraint, not a physical one, and mountain classification may be too strong. If awareness is unnecessary: bidirectionality is a deeper physical constraint on information transfer, and mountain classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_awareness_necessity, empirical, 'Whether observer awareness is necessary for bidirectionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(measurement_apparatus_bidirectionality, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meas_bidir_tr_t0, measurement_apparatus_bidirectionality, theater_ratio, 0, 0.15).
narrative_ontology:measurement(meas_bidir_tr_t50, measurement_apparatus_bidirectionality, theater_ratio, 50, 0.15).
narrative_ontology:measurement(meas_bidir_tr_t100, measurement_apparatus_bidirectionality, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(meas_bidir_be_t0, measurement_apparatus_bidirectionality, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(meas_bidir_be_t50, measurement_apparatus_bidirectionality, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(meas_bidir_be_t100, measurement_apparatus_bidirectionality, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(measurement_apparatus_bidirectionality, information_standard).

% DUAL FORMULATION NOTE:
% Measurement apparatus bidirectionality is a foundational constraint that affects all observation-dependent constraints but is not itself downstream of any other constraint. It is a terminal node in the constraint dependency graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
