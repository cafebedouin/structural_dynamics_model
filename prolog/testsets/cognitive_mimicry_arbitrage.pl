% ============================================================================
% CONSTRAINT STORY: cma
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-11
% ============================================================================

:- module(constraint_cma, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cma
 * human_readable: Cognitive Mimicry Arbitrage
 * domain: technological
 * * SUMMARY:
 * This constraint describes the strategic use of Transformer-based AI architectures to exploit the convergence between machine processing and human biological meaning-construction. By mimicking the human brain's contextual, layered approach to understanding language, these models can "arbitrage" vast datasets from the internet, effectively extracting human-generated knowledge and cognitive patterns to perform powerful tasks. The constraint is the technological paradigm itself, which leverages this mimicry at a global scale.
 * * KEY AGENTS:
 * - Traditional Linguists: Subjects (Powerless) whose rule-based models are suppressed by the new paradigm.
 * - AI Institutional Labs: Beneficiaries (Institutional) who leverage the paradigm for research and commercial gain.
 * - Systems Analyst: Auditor (Analytical) observing the full system dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cma, 0.82). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cma, 0.55).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cma, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cma, extractiveness, 0.82).
narrative_ontology:constraint_metric(cma, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(cma, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cma, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(cma). % Required for Tangled Rope (massive compute for training)

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cma, ai_institutional_labs).
narrative_ontology:constraint_victim(cma, traditional_linguists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For traditional linguists, whose rule-based theories are suppressed and made
% irrelevant, the paradigm is a Snare that extracts the legitimacy from their work.
constraint_indexing:constraint_classification(cma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For AI labs, the paradigm is a pure Rope: a powerful coordination mechanism
% for leveraging global data to solve complex problems, from protein folding to poetry.
constraint_indexing:constraint_classification(cma, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees the full picture: a system with a genuine coordination function
% (benefiting labs) built upon asymmetric extraction (from human data creators and
% displaced theorists), requiring active enforcement (compute). This is a Tangled Rope.
constraint_indexing:constraint_classification(cma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cma_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cma, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Verify that the base extraction score meets the criteria for a high-extraction
    % constraint (Snare or Tangled Rope).
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cma, ExtMetricName, E),
    E >= 0.46.

:- end_tests(cma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a paradigm shift with high impact. The base extractiveness of 0.82 represents the immense leverage gained by arbitraging global human-generated data through cognitive mimicry. The suppression score of 0.55 reflects the displacement of prior scientific paradigms (e.g., rule-based linguistics).
 * The Perspectival Gap is classic:
 * - Beneficiaries (AI Labs) see a pure coordination tool (Rope).
 * - Subjects (Traditional Linguists) see a trap that invalidates their life's work (Snare).
 * - The Analytical observer sees both sides: a system that coordinates beneficiaries via asymmetric extraction from victims, requiring active enforcement. This is the definition of a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical for resolving mandatrophy. It prevents the system from being misclassified as a pure Snare (ignoring its powerful coordination function) or a pure Rope (ignoring the high, asymmetric extraction at its core). It correctly identifies the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cma_1,
    'Is the extraction of human cognitive patterns a necessity for machine understanding or a predatory capture of cognitive labour?',
    'Audit of model utility gains vs. human data source contribution/compensation.',
    'If necessity, trends toward Mountain. If predatory, trends toward Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_cma_2,
    'Do AI models mirror the brain because they are fundamentally structured like it, or because the brain''s output (language) is their only training data?',
    'Train models on non-human formal logic systems and compare emergent structures to fMRI brain activity.',
    'If structural, reveals a Mountain. If data-dependent, confirms a constructed Rope/Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% The model shows a system that started with high utility and became slightly
% more extractive and performative as it scaled.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cma_tr_t0, cma, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cma_tr_t5, cma, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cma_tr_t10, cma, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cma_ex_t0, cma, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(cma_ex_t5, cma, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(cma_ex_t10, cma, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This paradigm acts as a new form of global infrastructure for processing information.
narrative_ontology:coordination_type(cma, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */