% ============================================================================
% CONSTRAINT STORY: st_petersburg_paradox
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_st_petersburg_paradox, []).

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
 * * constraint_id: st_petersburg_paradox
 * human_readable: St. Petersburg Paradox (Divergent Expected Value)
 * domain: mathematical/economic
 * * SUMMARY:
 * The St. Petersburg Paradox describes a lottery game with an infinite expected
 * monetary value, yet for which rational players are only willing to pay a small
 * finite entrance fee. It exposes a fundamental constraint in classical decision
 * theory: that raw expected value is a poor model for human utility. The paradox
 * itself is a mathematical curiosity, but its resolution via utility theory
 * became a foundational tool in modern finance and insurance.
 * * KEY AGENTS:
 * - The Naive Gambler: Subject (Powerless) who, if following pure expected value,
 *   is trapped in a game that extracts all their finite wealth for a theoretical
 *   infinite prize.
 * - The Insurance Industry: Beneficiary (Institutional) that uses the resolution
 *   (utility theory) as a coordination mechanism to price risk and sell policies.
 * - The Economic Analyst: Auditor (Analytical) who sees both the coordination
 *   function and the asymmetric extraction from those who misunderstand it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(st_petersburg_paradox, 0.65). % Snare extraction >= 0.46
domain_priors:suppression_score(st_petersburg_paradox, 0.40).   % Suppresses linear value models.
domain_priors:theater_ratio(st_petersburg_paradox, 0.10).       % Low theater; the math is functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(st_petersburg_paradox, extractiveness, 0.65).
narrative_ontology:constraint_metric(st_petersburg_paradox, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(st_petersburg_paradox, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The paradox presents itself as an immutable mathematical law.
narrative_ontology:constraint_claim(st_petersburg_paradox, tangled_rope).
narrative_ontology:human_readable(st_petersburg_paradox, "St. Petersburg Paradox (Divergent Expected Value)").
narrative_ontology:topic_domain(st_petersburg_paradox, "mathematical/economic").

% Binary flags
% The application of utility theory requires enforcement (e.g., house limits,
% insurance premium regulations) to function as a stable system.
domain_priors:requires_active_enforcement(st_petersburg_paradox).

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(st_petersburg_paradox, insurance_industry).
narrative_ontology:constraint_beneficiary(st_petersburg_paradox, financial_risk_modelers).
narrative_ontology:constraint_victim(st_petersburg_paradox, pure_expected_value_adherents).
narrative_ontology:constraint_victim(st_petersburg_paradox, naive_gamblers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a naive gambler or a hypothetical agent bound by pure expected value,
% the paradox is a trap. It demands a high (or infinite) entry fee for a
% prize that is practically unrealizable, extracting all finite wealth.
% χ = 0.65 * 1.5 (powerless) * 1.0 (national) = 0.975
constraint_indexing:constraint_classification(st_petersburg_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the insurance industry, the paradox's resolution (utility theory) is a
% pure coordination tool. It provides a stable, universally accepted method
% for pricing risk, enabling the entire market to function.
% χ = 0.65 * -0.2 (institutional) * 1.2 (global) = -0.156 (felt as benefit)
constraint_indexing:constraint_classification(st_petersburg_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides. The resolution is a genuine coordination tool
% (Rope aspect) but it's built upon a mathematical structure that is inherently
% extractive (Snare aspect) for those who don't or can't use utility models.
% It requires active enforcement (house limits, regulations) to remain stable.
% χ = 0.65 * 1.15 (analytical) * 1.2 (global) = 0.897
constraint_indexing:constraint_classification(st_petersburg_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(st_petersburg_paradox_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(st_petersburg_paradox, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(st_petersburg_paradox, ExtMetricName, E),
    E >= 0.46.

:- end_tests(st_petersburg_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The St. Petersburg Paradox is a classic example of a constraint whose nature
 * changes dramatically with perspective. The base extractiveness (0.65) is high
 * because, in its pure form, the logic of infinite expected value extracts all
 * finite resources from an agent compelled to follow it.
 *
 * * Perspectival Gap:
 * - The 'powerless' gambler sees a Snare. They are trapped by their finite
 *   bankroll against a game whose value is theoretical and inaccessible. The
 *   promise of infinite return extracts their real, finite stake.
 * - The 'institutional' beneficiary (e.g., an insurer) sees a Rope. The paradox
 *   is not a trap but a foundational problem whose *solution* (utility theory)
 *   is a powerful tool for coordinating the entire field of risk management.
 * - The 'analytical' observer sees a Tangled Rope. They recognize the genuine
 *   coordination function for institutions, but also see that this function is
 *   predicated on an underlying mathematical structure that is asymmetrically
*    extractive for the uninitiated. The system requires active enforcement
 *   (e.g., casino limits, insurance regulations) to prevent it from collapsing
 *   back into a pure Snare. This dual nature is the hallmark of a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope prevents mischaracterizing the modern
 * financial system's use of utility theory. A simple Snare classification would
 * miss the genuine, system-stabilizing coordination it provides. A simple Rope
 * classification would ignore the inherent extraction from those who cannot
 * access or apply these sophisticated models, effectively hiding the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_st_petersburg_paradox,
    'Is the paradox a true mathematical Mountain or a Scaffold built on the flawed assumption of an infinitely funded house?',
    'Recalculating expected value with a finite cap on the payout (e.g., the total M2 money supply).',
    'If the house is finite, the expected value becomes finite and small. The paradox dissolves, and the constraint is revealed to be a constructed artifact of abstract math, not a natural law.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(st_petersburg_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (E > 0.46).
% Models the transition from a pure mathematical curiosity to a tool for
% financial extraction. Initial extraction was low (it was just a thought
% experiment), but grew as it was integrated into risk-pricing models.
%
% Theater ratio over time (remains low):
narrative_ontology:measurement(st_petersburg_paradox_tr_t0, st_petersburg_paradox, theater_ratio, 0, 0.05).
narrative_ontology:measurement(st_petersburg_paradox_tr_t5, st_petersburg_paradox, theater_ratio, 5, 0.10).
narrative_ontology:measurement(st_petersburg_paradox_tr_t10, st_petersburg_paradox, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(st_petersburg_paradox_ex_t0, st_petersburg_paradox, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(st_petersburg_paradox_ex_t5, st_petersburg_paradox, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(st_petersburg_paradox_ex_t10, st_petersburg_paradox, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The resolution of the paradox (utility theory) is a fundamental mechanism
% for allocating capital and pricing risk in finance.
narrative_ontology:coordination_type(st_petersburg_paradox, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */