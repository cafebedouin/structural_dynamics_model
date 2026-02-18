% ============================================================================
% CONSTRAINT STORY: signal_without_control
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_signal_without_control, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: signal_without_control
 * human_readable: The Passive Observational Trap
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents a state where an agent has access to high-fidelity
 * data streams (signals) regarding a system's state but is structurally barred
 * from adjusting the system's parameters (control). It creates a "dashboard
 * without a steering wheel," leading to high psychological and economic extraction
 * by demanding attention without granting agency.
 * * KEY AGENTS:
 * - Data Consumer: Subject (Powerless)
 * - Platform Architect: Beneficiary (Institutional)
 * - Information Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.79) because the signal presence increases engagement/stress
% without enabling agency, siphoning attention/resources.
domain_priors:base_extractiveness(signal_without_control, 0.79).
domain_priors:suppression_score(signal_without_control, 0.60).
domain_priors:theater_ratio(signal_without_control, 0.85). % High theater: the "dashboard" looks functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(signal_without_control, extractiveness, 0.79).
narrative_ontology:constraint_metric(signal_without_control, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(signal_without_control, theater_ratio, 0.85).

% The platform claims this is a coordination tool for providing information.
narrative_ontology:constraint_claim(signal_without_control, tangled_rope).
narrative_ontology:human_readable(signal_without_control, "The Passive Observational Trap").
narrative_ontology:topic_domain(signal_without_control, "technological/social").

% Required for Tangled Rope: The illusion of control and the platform's terms of service
% require active enforcement to prevent users from building their own control interfaces.
domain_priors:requires_active_enforcement(signal_without_control).

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(signal_without_control, platform_architects).
narrative_ontology:constraint_victim(signal_without_control, data_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped by the signal; they cannot ignore it, yet cannot act on it.
constraint_indexing:constraint_classification(signal_without_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the signal as a 'Rope' for user retention and data harvesting.
constraint_indexing:constraint_classification(signal_without_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Acknowledges both the coordination claim (the signal) and the asymmetric extraction
% (the lack of control), classifying it as a hybrid.
constraint_indexing:constraint_classification(signal_without_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% The high theater ratio (0.85 > 0.70) triggers a Piton classification, focusing
% on the atrophied or non-existent control function. The dashboard is pure theater.
constraint_indexing:constraint_classification(signal_without_control, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(signal_without_control, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(signal_without_control_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict.
    constraint_indexing:constraint_classification(signal_without_control, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(signal_without_control, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(signal_without_control, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_threshold) :-
    % Ensure high theater ratio results in Piton classification for auditors.
    domain_priors:theater_ratio(signal_without_control, TR),
    ( TR > 0.70 ->
        constraint_indexing:constraint_classification(signal_without_control, piton,
            context(agent_power(analytical), _, exit_options(arbitrage), _))
    ; true
    ).

:- end_tests(signal_without_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.79) represents the massive cognitive and emotional
 * rent paid by subjects who monitor signals they cannot influence. The high
 * theater ratio (0.85) reflects that the user interface (the dashboard) is
 * designed to look functional and empowering, while being structurally inert.
 * The perspectival gap is stark: for the powerless user, it's a Snare demanding
 * attention. For the institutional architect, it's a Rope for user engagement.
 * The analytical view is split: one lens sees a Tangled Rope (a broken coordination
 * tool), while another sees a Piton (a purely theatrical artifact).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction could lead to a simple Snare classification, but this would
 * be a case of Mandatrophy, as it would ignore the constraint's coordination claim.
 * The Tangled Rope classification resolves this by correctly identifying the hybrid
 * nature: there is a genuine coordination function (providing information via a
 * signal), but it is coupled with severe, asymmetric extraction (denial of control),
 * which benefits the platform architect at the expense of the data consumer. This
 * prevents mislabeling a broken coordination system as pure, malicious extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_control_access,
    'Is the lack of control a technical latency (Mountain) or an intentional lockout (Snare)?',
    'Introduction of an "Action API" for a controlled subset of users.',
    'If action efficacy is non-zero: Snare/Tangled Rope. If action efficacy is zero: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(signal_without_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint often emerges from a useful tool that slowly becomes more
% extractive as control features are deprecated or paywalled, while the
% theatrical "dashboard" elements are enhanced.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(swc_tr_t0, signal_without_control, theater_ratio, 0, 0.40).
narrative_ontology:measurement(swc_tr_t5, signal_without_control, theater_ratio, 5, 0.70).
narrative_ontology:measurement(swc_tr_t10, signal_without_control, theater_ratio, 10, 0.85).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(swc_ex_t0, signal_without_control, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(swc_ex_t5, signal_without_control, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(swc_ex_t10, signal_without_control, base_extractiveness, 10, 0.79).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination function is providing a standardized data stream.
narrative_ontology:coordination_type(signal_without_control, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */