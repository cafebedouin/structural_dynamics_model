% ============================================================================
% CONSTRAINT STORY: PEROVSKITE_SELF_ETCHING
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_perovskite_etching, []).

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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: perovskite_self_etching
 * human_readable: The 2D Perovskite Machinability Constraint
 * domain: semiconductors/optoelectronics
 * * SUMMARY:
 * Traditional lithography acts as a high-cost, destructive constraint for
 * soft lead halide perovskites. This story identifies the
 * "self-etching" technique as an exit (Rope) from the "Mountain" of
 * lithographic scaling limits.
 * * KEY AGENTS:
 * - The Material Scientist (Zhang Shuchen): Subject (Individual Powerless)
 * - Traditional Lithography Industry: Beneficiary (Institutional) [cite: 1, 6]
 * - Joint US-China Research Team: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is 0.52 due to the exponential cost increases of traditional
% EUV scaling for advanced device nodes[cite: 1, 6].
domain_priors:base_extractiveness(perovskite_self_etching, 0.52).
domain_priors:suppression_score(perovskite_self_etching, 0.65).   % Lithography dominance
domain_priors:theater_ratio(perovskite_self_etching, 0.15).       % High functional utility elsewhere

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(perovskite_self_etching, extractiveness, 0.52).
narrative_ontology:constraint_metric(perovskite_self_etching, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(perovskite_self_etching, theater_ratio, 0.15).

% Binary flags
domain_priors:requires_active_enforcement(perovskite_self_etching). % Needs IPA solution activation.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESEARCHER (SNARE)
% Traditional lithography is a trap that damages soft materials.
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SEMICONDUCTOR INDUSTRY (MOUNTAIN)
% Viewed as an irreducible physical limit of "traditional craftsmanship".
constraint_indexing:constraint_classification(perovskite_self_etching, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE JOINT RESEARCH TEAM (ROPE)
% The self-etching platform acts as a coordination mechanism for "entirely
% new avenues" of device design.
constraint_indexing:constraint_classification(perovskite_self_etching, rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SUSTAINABLE PLATFORM (SCAFFOLD)
% A temporary research path (Scaffold) while exploring more efficient
% self-assembly platforms.
constraint_indexing:constraint_classification(perovskite_self_etching, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(perovskite_self_etching).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_etching_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the auditors.
    constraint_indexing:constraint_classification(perovskite_self_etching, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_self_etching, rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(perovskite_self_etching, E),

    E >= 0.46. % Correct for v3.4 Snare threshold[cite: 1].

:- end_tests(perovskite_etching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.52) is justified by the "exponential" costs and
 * material damage caused by traditional lithography in this domain.
 * The Perspectival Gap is clear: Industry incumbents see a 'Mountain' of
 * unavoidable scaling physics, while researchers see a 'Snare' to be exited
 * via chemical "self-etching".
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Rope classification prevents mislabeling this as pure extraction by
 * acknowledging its function as a coordination platform for atomic-level
 * heterojunctions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_stress_causality,
    'Is internal stress a fixed Mountain of physics or a Snare of growth conditions?',
    'Comparative study of crystal growth under varying mechanical constraints.',
    'Physics = Immutable Mountain; Growth = Controlled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(perovskite_self_etching, 2026, 2030).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
