% ============================================================================
% CONSTRAINT STORY: goodharts_law
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_goodharts_law, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: goodharts_law
 * human_readable: Goodhart's Law
 * domain: economic/social/technological
 * * SUMMARY:
 * Goodhart's Law states that "When a measure becomes a target, it ceases to be a good measure." It describes the process where agents optimize for a proxy metric rather than the intended outcome, leading to systemic distortion, gaming, and the collapse of the metric's informational value. This file models the application of a metric subject to this law.
 * * KEY AGENTS:
 * - The Over-Managed Worker: Subject (Powerless)
 * - The Data Architect/Administrator: Beneficiary (Institutional)
 * - The Systems Theorist/Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(goodharts_law, 0.50). % Extracts "systemic truth" and "genuine utility" by forcing optimization of a proxy.
domain_priors:suppression_score(goodharts_law, 0.70).   % The metric actively suppresses qualitative alternatives and work that doesn't contribute to the target.
domain_priors:theater_ratio(goodharts_law, 0.10).       % The work is real, but misdirected; not purely theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(goodharts_law, extractiveness, 0.50).
narrative_ontology:constraint_metric(goodharts_law, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(goodharts_law, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The metric is presented as a tool for alignment and efficiency.
narrative_ontology:constraint_claim(goodharts_law, tangled_rope).

% Binary flags
% Performance management systems require active enforcement of their metrics.
domain_priors:requires_active_enforcement(goodharts_law).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(goodharts_law, metric_gamers).
narrative_ontology:constraint_beneficiary(goodharts_law, bureaucratic_optimizers).
narrative_ontology:constraint_victim(goodharts_law, systemic_integrity).
narrative_ontology:constraint_victim(goodharts_law, qualitative_performers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE OVER-MANAGED WORKER (SNARE)
% The metric is a trap forcing meaningless work to "hit the number,"
% extracting professional integrity and purpose.
constraint_indexing:constraint_classification(goodharts_law, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE DATA ARCHITECT (ROPE)
% The metric is a necessary tool for coordination, a rope to pull a
% large, distributed organization toward a unified goal.
constraint_indexing:constraint_classification(goodharts_law, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS THEORIST (MOUNTAIN)
% Goodhart's Law is an unchangeable feature of the informational landscape,
% a natural law of social systems. Any proxy will eventually succumb.
constraint_indexing:constraint_classification(goodharts_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_tests).

test(perspectival_gap) :-
    % Verify the gap between Worker (Snare), Architect (Rope), and Theorist (Mountain).
    constraint_indexing:constraint_classification(goodharts_law, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(goodharts_law, ExtMetricName, E),
    E >= 0.46. % This is a high-extraction constraint.

:- end_tests(goodharts_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models the indexical nature of a systemic law. For the theorist, it is a 'Mountain'—an immutable feature of reality. For the institutional architect who wields it, a specific metric is a 'Rope'—a tool for coordination. For the worker trapped by it, that same metric is a 'Snare' that extracts their agency and professional integrity.
 * The base extractiveness of 0.50 reflects that the system extracts "truth" and "genuine utility" by redirecting effort towards a proxy. The high suppression (0.70) shows how the metric makes unquantifiable work invisible and valueless.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The classification of the institutional view as 'Rope' and the powerless view as 'Snare' correctly captures the dual nature of the constraint. A naive analysis might see only the Snare (the negative outcome) and miss the genuine coordination function the metric serves for the institution. This prevents mislabeling a complex system as pure extraction and highlights the perspectival gap as the core problem. The analytical view of 'Mountain' further clarifies that this is a systemic tendency, not just a policy failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_goodharts_law,
    'At what degree of optimization pressure does a metric cross the threshold from a useful Rope to a value-destroying Snare?',
    'Longitudinal study of KPI health vs. actual outcomes in high-pressure bureaucratic environments.',
    'If the threshold is low, almost all metrics are latent Snares. If high, metrics can be maintained as Ropes for longer periods with careful management.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(goodharts_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the degradation of the metric over time. It starts as a
% useful, low-extraction coordination tool (T=0) and becomes increasingly
% gamed and extractive as agents optimize for the proxy (T=10).

% Theater ratio over time (slight increase as gaming becomes more performative):
narrative_ontology:measurement(goodharts_law_tr_t0, goodharts_law, theater_ratio, 0, 0.05).
narrative_ontology:measurement(goodharts_law_tr_t5, goodharts_law, theater_ratio, 5, 0.08).
narrative_ontology:measurement(goodharts_law_tr_t10, goodharts_law, theater_ratio, 10, 0.10).

% Extraction over time (increases as the metric diverges from reality):
narrative_ontology:measurement(goodharts_law_ex_t0, goodharts_law, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(goodharts_law_ex_t5, goodharts_law, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(goodharts_law_ex_t10, goodharts_law, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% A performance metric is a classic example of an information standard.
narrative_ontology:coordination_type(goodharts_law, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */