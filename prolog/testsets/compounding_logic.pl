% ============================================================================
% CONSTRAINT STORY: compounding_logic
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_compounding_logic, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: compounding_logic
 * human_readable: The Law of Compounding Returns
 * domain: economic
 * * SUMMARY:
 * Compounding is the mathematical process where the value of a system increases
 * exponentially because earnings are reinvested to generate further earnings.
 * While mathematically neutral, its application in economic systems creates
 * vast perspectival gaps. It is constrained by time, rate of return, and the
 * stability of the underlying economic medium (e.g., currency, assets).
 * * KEY AGENTS:
 * - The Debtor: Subject to compounding interest on loans (Powerless).
 * - The Capital Allocator: Manages reinvestment cycles for growth (Institutional).
 * - The Systems Analyst: Observes the dual function of the mechanism (Analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(compounding_logic, 0.50). % In a savings context, it creates value; in a debt context, it is highly extractive. The base value reflects this duality.
domain_priors:suppression_score(compounding_logic, 0.40).   % One can choose not to participate by consuming immediately, but opting out of the dominant financial system has high costs.
domain_priors:theater_ratio(compounding_logic, 0.05).       % Compounding is a functional, not performative, mechanism.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(compounding_logic, extractiveness, 0.50).
narrative_ontology:constraint_metric(compounding_logic, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(compounding_logic, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a neutral, mathematical law of nature.
narrative_ontology:constraint_claim(compounding_logic, tangled_rope).
narrative_ontology:human_readable(compounding_logic, "The Law of Compounding Returns").
narrative_ontology:topic_domain(compounding_logic, "economic").

% Binary flags
domain_priors:requires_active_enforcement(compounding_logic). % Requires a stable currency, property rights, and contract law to function.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(compounding_logic, long_term_savers_and_investors).
narrative_ontology:constraint_victim(compounding_logic, debtors_and_non_capital_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEBTOR (SNARE)
% For the debtor, compounding is a coercive mechanism that extracts their
% future labor at an accelerating rate, strangling their financial freedom.
constraint_indexing:constraint_classification(compounding_logic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CAPITAL ALLOCATOR (ROPE)
% For the institutional investor, compounding is a pure coordination tool for
% generating long-term wealth and coordinating economic growth.
constraint_indexing:constraint_classification(compounding_logic, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides: a coordination mechanism for savers that
% simultaneously functions as an extractive mechanism against debtors,
% requiring active enforcement (stable currency, contract law) to operate.
constraint_indexing:constraint_classification(compounding_logic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compounding_logic_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the three core perspectives.
    constraint_indexing:constraint_classification(compounding_logic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypeAnalytical == tangled_rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    domain_priors:base_extractiveness(compounding_logic, E),
    E >= 0.46, E =< 0.70. % Validates it's in the high-extraction range for Snare/Tangled Rope but not extreme.

test(tangled_rope_conditions_met) :-
    % The analytical perspective should resolve to Tangled Rope.
    constraint_indexing:constraint_classification(compounding_logic, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(compounding_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is its dual nature. It is presented as a neutral
 * mathematical law (its `natural_law` claim), but its application within a
 * capitalist framework makes it a powerful tool for both coordination and
 * extraction. The base extractiveness of 0.50 reflects this average effect.
 *
 * The Perspectival Gap is stark:
 * - For a debtor (`powerless`), the felt extraction (χ) is amplified (0.50 * 1.5 * 1.0 = 0.75), making it a clear `Snare`.
 * - For an institution (`institutional`), the felt extraction is suppressed (0.50 * -0.2 * 1.2 = -0.12), appearing as a value-creating `Rope`.
 * - The `analytical` observer sees the whole system: the coordination function for beneficiaries, the asymmetric extraction from victims, and the need for active enforcement. This combination is the definition of a `Tangled Rope`.
 *
 * * MANDATROPHY ANALYSIS:
 * This is a classic case where Mandatrophy could occur. A system focused only
 * on the institutional perspective would see compounding as a pure `Rope` and
 * miss the immense extractive pressure placed on debtors. The `Tangled Rope`
 * classification prevents this by forcing an acknowledgment of the victims and
 * the coercive enforcement required, providing a complete structural picture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is whether sufficient reinvestment opportunities will
% persist to sustain the "coordination" side of the mechanism.
omega_variable(
    omega_compounding_logic,
    'Will market saturation and diminishing returns eventually eliminate high-return reinvestment opportunities, causing the coordination function to atrophy and leaving only the extractive (debt) function intact?',
    'Longitudinal analysis of risk-adjusted returns on capital across major economies over a multi-generational timeframe.',
    'If returns diminish, the constraint degrades from a Tangled Rope to a pure Snare. If opportunities persist, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(compounding_logic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction has increased over the interval as financial systems became more
% complex and leveraged, making the compounding effect more potent.
% Theater remains low as the mechanism is purely functional.

% Theater ratio over time (stable):
narrative_ontology:measurement(compounding_logic_tr_t0, compounding_logic, theater_ratio, 0, 0.05).
narrative_ontology:measurement(compounding_logic_tr_t5, compounding_logic, theater_ratio, 5, 0.05).
narrative_ontology:measurement(compounding_logic_tr_t10, compounding_logic, theater_ratio, 10, 0.05).

% Extraction over time (increasing):
narrative_ontology:measurement(compounding_logic_ex_t0, compounding_logic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(compounding_logic_ex_t5, compounding_logic, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(compounding_logic_ex_t10, compounding_logic, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Compounding is a fundamental mechanism for allocating capital over time.
narrative_ontology:coordination_type(compounding_logic, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */