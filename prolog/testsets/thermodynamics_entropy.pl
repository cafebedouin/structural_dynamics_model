% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
 * * constraint_id: thermodynamics_entropy
 * human_readable: The Second Law of Thermodynamics (Entropy)
 * domain: technological
 * * SUMMARY:
 * The Second Law of Thermodynamics dictates that in any isolated system, entropy (disorder) must increase over time. This imposes a fundamental "arrow of time" and a hard limit on the efficiency of any engine or biological process, representing the inevitable decay of concentrated energy into unusable heat. It is a universal "tax" on existence.
 * * KEY AGENTS:
 * - The Biological Organism: Subject (Powerless), trapped in a local struggle to maintain order (negentropy) by extracting energy from the environment.
 * - The Industrial Engineer: Beneficiary (Institutional), who leverages the predictability of the law to design efficient systems.
 * - The Cosmologist: Auditor (Analytical), observing the "Heat Death" of the universe as the ultimate expression of the law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(thermodynamics_entropy, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(thermodynamics_entropy, 0.95).   % Structural property (raw, unscaled). Suppresses perpetual motion.
domain_priors:theater_ratio(thermodynamics_entropy, 0.0).       % Piton detection (>= 0.70). A physical law has no performative aspect.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.80).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(thermodynamics_entropy, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(thermodynamics_entropy, snare).

% Binary flags
% This constraint does not require active enforcement; it is an emergent property of statistical mechanics.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(thermodynamics_entropy, industrial_engineers).
narrative_ontology:constraint_victim(thermodynamics_entropy, all_ordered_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a biological organism, life is a constant struggle against decay. Entropy
% is a Snare that slowly tightens, extracting vitality and order until the
% system collapses into equilibrium (death).
% χ = 0.80 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 0.96
constraint_indexing:constraint_classification(thermodynamics_entropy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For an engineer with institutional power, the law's predictability is a Rope.
% It provides the rules for coordination (e.g., the Carnot limit) that allow
% for the design of efficient engines and power grids. The negative extraction
% reflects the value created by leveraging this predictable limit.
% χ = 0.80 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.16
constraint_indexing:constraint_classification(thermodynamics_entropy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% From a universal, analytical view, the Second Law is not a neutral Mountain
% (which requires low extraction). Its high base extraction (0.8) makes it a
% universal Snare, condemning the cosmos to an eventual "Heat Death" by
% inexorably extracting all useful energy.
% χ = 0.80 (ε) * 1.15 (π(analytical)) * 1.0 (σ(universal)) = 0.92
constraint_indexing:constraint_classification(thermodynamics_entropy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_observer_is_snare) :-
    % Verify the analytical observer sees a Snare due to high extraction, not a Mountain.
    constraint_indexing:constraint_classification(thermodynamics_entropy, snare, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the constraint is correctly identified as high-extraction.
    narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, E),
    E >= 0.46.

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This model challenges the intuitive classification of a physical law as a Mountain.
 * The Deferential Realism system defines a Mountain by its low extraction (ε <= 0.15).
 * The Second Law of Thermodynamics, however, is fundamentally extractive: it degrades
 * useful energy into waste heat, "extracting" the potential for work from every
 * system. Its base extraction is therefore set very high (0.80).
 *
 * This leads to a profound perspectival gap:
 * - For the powerless (an organism), this inescapable extraction is a Snare (aging, decay).
 * - For the institutional (an engineer), the law's predictability is a coordination
 *   mechanism (a Rope) for building efficient systems, generating value (negative
 *   effective extraction).
 * - For the analytical observer, the universal and inescapable nature of this
 *   extraction makes it the ultimate Snare, leading to the heat death of the universe.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this constraint as a Snare from the analytical perspective, despite
 * its `natural_law` claim, is a critical resolution of Mandatrophy. It prevents the
 * system from mislabeling a highly extractive, terminal process as a neutral,
 * low-extraction "Mountain." It correctly identifies that immutability combined
 * with high extraction is the definition of an inescapable trap, not a neutral feature
 * of the landscape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_thermodynamics_entropy,
    'Is the universe a truly isolated system subject to heat death (Snare), or could unknown physics (e.g., multiverse interactions, vacuum energy renewal) provide an escape (making it a contingent Tangled Rope)?',
    'Empirical validation of cosmological models beyond the Standard Model.',
    'If true Snare, all agency is ultimately futile. If contingent, long-term survival strategies may exist.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(thermodynamics_entropy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. For a physical law, the metrics are
% constant over the interval, representing its unchanging nature. This is
% required because base_extractiveness > 0.46.
%
% Theater ratio over time (flat at zero):
narrative_ontology:measurement(thermodynamics_entropy_tr_t0, thermodynamics_entropy, theater_ratio, 0, 0.0).
narrative_ontology:measurement(thermodynamics_entropy_tr_t5, thermodynamics_entropy, theater_ratio, 5, 0.0).
narrative_ontology:measurement(thermodynamics_entropy_tr_t10, thermodynamics_entropy, theater_ratio, 10, 0.0).

% Extraction over time (flat at 0.80):
narrative_ontology:measurement(thermodynamics_entropy_ex_t0, thermodynamics_entropy, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(thermodynamics_entropy_ex_t5, thermodynamics_entropy, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(thermodynamics_entropy_ex_t10, thermodynamics_entropy, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No Boltzmann or Network data is applicable for a fundamental physical law.
% narrative_ontology:coordination_type(thermodynamics_entropy, enforcement_mechanism).
% narrative_ontology:affects_constraint(thermodynamics_entropy, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */