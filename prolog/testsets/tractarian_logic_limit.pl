% ============================================================================
% CONSTRAINT STORY: tractarian_logic_limit
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_tractarian_logic_limit, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tractarian_logic_limit
 * human_readable: The Limits of Language (Tractatus)
 * domain: philosophical/logical
 * * SUMMARY:
 * The world consists of facts in logical space. Language is a "picture" of those facts.
 * Any attempt to use language outside this scope (e.g., to discuss ethics, aesthetics,
 * or metaphysics) hits a hard limit, resulting in "nonsense." The constraint is the
 * proposed logical structure of language itself, which suppresses alternative modes of discourse.
 * * KEY AGENTS:
 * - The Metaphysician: Subject (Powerless) - An agent whose field of inquiry is declared unspeakable.
 * - The Vienna Circle: Beneficiary (Institutional) - A philosophical movement that adopted the Tractatus as a core text.
 * - The Systems Analyst: Auditor (Analytical) - An observer classifying the structure of the philosophical claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(tractarian_logic_limit, 0.70). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(tractarian_logic_limit, 0.95).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(tractarian_logic_limit, 0.10).       % Piton detection (>= 0.70)
domain_priors:requires_active_enforcement(tractarian_logic_limit).

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(tractarian_logic_limit, extractiveness, 0.70).
narrative_ontology:constraint_metric(tractarian_logic_limit, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(tractarian_logic_limit, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(tractarian_logic_limit, snare).
narrative_ontology:human_readable(tractarian_logic_limit, "The Limits of Language (Tractatus)").

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(tractarian_logic_limit, natural_science_philosophers).
narrative_ontology:constraint_victim(tractarian_logic_limit, metaphysicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The metaphysician or ethicist whose entire domain is extracted and delegitimized.
% It feels like a Mountain due to its totalizing logical claims, but is a constructed Snare.
% χ = 0.70 * π(powerless=1.5) * σ(universal=1.0) = 1.05. High extraction + high suppression = Snare.
constraint_indexing:constraint_classification(tractarian_logic_limit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the logical positivists of the Vienna Circle, it was a pure coordination device.
% It provided a shared standard to "clean up" philosophy and eliminate metaphysics.
% χ = 0.70 * π(institutional=-0.2) * σ(continental=1.1) = -0.154. Negative extraction = Rope.
constraint_indexing:constraint_classification(tractarian_logic_limit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analyst sees a constructed system with high base extraction and suppression,
% which functions to asymmetrically benefit one mode of inquiry over another.
% χ = 0.70 * π(analytical=1.15) * σ(global=1.2) = 0.966. This is a clear Snare.
constraint_indexing:constraint_classification(tractarian_logic_limit, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tractarian_logic_limit_tests).

test(perspectival_gap_subject_beneficiary) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_observer_classification) :-
    % The analytical observer must classify this as a Snare based on the metrics.
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == snare).

test(high_extraction_and_suppression) :-
    % Verify the base metrics meet the Snare threshold.
    domain_priors:base_extractiveness(tractarian_logic_limit, E),
    domain_priors:suppression_score(tractarian_logic_limit, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(tractarian_logic_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a philosophical system that claims to be a 'Mountain' (a description
 * of logical reality, a natural law) but functions as a 'Snare'. The base extractiveness (0.70)
 * is high because the system extracts the legitimacy of entire fields like ethics, aesthetics,
 * and metaphysics, reclassifying them as "nonsense." The suppression (0.95) is near-total,
 * as it posits this exclusion not as a choice but as a logical necessity.
 *
 * The Perspectival Gap is stark:
 * - For the victim (metaphysician), it's a Snare that traps them by invalidating their language.
 *   While it feels like an inescapable Mountain, its constructed nature and high extraction
 *   make it a Snare.
 * - For the beneficiary (Vienna Circle), it's a Rope—a powerful coordination tool to build a
 *   new, rigorous philosophical movement. The negative effective extraction for them confirms this.
 * - The Analytical observer confirms the Snare classification based on the objective metrics.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This case is a classic example of a false natural law claim. The system avoids
 * misclassifying it as a Mountain by indexing it. While the powerless agent feels
 * trapped as if by a law of nature, the institutional agent's ability to use it as a
 * 'Rope' and the high base extraction reveal its constructed, extractive nature. The
 * system correctly identifies it as a Snare from the analytical perspective, resolving
 * the ambiguity created by its 'natural law' claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tractarian_logic_limit,
    'Is the Tractarian limit a genuine logical boundary (Mountain) or a sophisticated linguistic convention (Snare)?',
    'Analysis of Wittgenstein''s later work (Philosophical Investigations), which refutes the Tractarian model, provides strong evidence for the Snare interpretation.',
    'If Mountain, then metaphysics is impossible. If Snare, then the Tractatus is a coercive philosophical move that suppresses valid alternative language games.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(tractarian_logic_limit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint is modeled as being
% conceptually stable since its inception, with consistently high extraction
% and low theatricality.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(tractarian_logic_limit_tr_t0, tractarian_logic_limit, theater_ratio, 0, 0.10).
narrative_ontology:measurement(tractarian_logic_limit_tr_t5, tractarian_logic_limit, theater_ratio, 5, 0.10).
narrative_ontology:measurement(tractarian_logic_limit_tr_t10, tractarian_logic_limit, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(tractarian_logic_limit_ex_t0, tractarian_logic_limit, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(tractarian_logic_limit_ex_t5, tractarian_logic_limit, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(tractarian_logic_limit_ex_t10, tractarian_logic_limit, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination type is declared as the primary function is suppression,
% not coordination in the sense of a shared resource or standard, despite
% the Vienna Circle using it as such. Its claim is natural_law, not coordination.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */