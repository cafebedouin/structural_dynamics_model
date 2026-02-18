% ============================================================================
% CONSTRAINT STORY: tx_hispanic_pivot
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_tx_hispanic_pivot, []).

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
 * * constraint_id: tx_hispanic_pivot
 * human_readable: The Texas Hispanic Voting Block Volatility (2024-2026)
 * domain: political
 * * SUMMARY:
 * A political alliance formed between the GOP and a significant portion of the
 * Texas Hispanic electorate in 2024 is fracturing due to aggressive federal
 * immigration enforcement tactics. What was initially a cooperative arrangement
 * based on shared goals (border security) is now perceived by the community as
 * a coercive, extractive system that imposes high social costs, threatening to
 * reverse electoral gains and destabilize the GOP's national coalition.
 * * KEY AGENTS:
 * - South Texas Hispanic Voters: Subject (Powerless) - Experiencing the negative
 *   externalities of enforcement.
 * - GOP Electoral Apparatus: Beneficiary (Institutional) - Gained power from the
 *   2024 alliance and seeks to maintain it through enforcement claims.
 * - Political Strategists: Auditor (Analytical) - Observing the breakdown of
 *   the alliance and its structural properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(tx_hispanic_pivot, 0.70). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(tx_hispanic_pivot, 0.60).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(tx_hispanic_pivot, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(tx_hispanic_pivot, extractiveness, 0.70).
narrative_ontology:constraint_metric(tx_hispanic_pivot, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(tx_hispanic_pivot, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(tx_hispanic_pivot, tangled_rope).
narrative_ontology:human_readable(tx_hispanic_pivot, "The Texas Hispanic Voting Block Volatility (2024-2026)").
narrative_ontology:topic_domain(tx_hispanic_pivot, "political").

% Binary flags
domain_priors:requires_active_enforcement(tx_hispanic_pivot). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(tx_hispanic_pivot, gop_electoral_apparatus).
narrative_ontology:constraint_victim(tx_hispanic_pivot, south_texas_hispanic_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as a predatory trap. The initial promise of coordination
% (border security) has been replaced by coercive enforcement with high local costs.
constraint_indexing:constraint_classification(tx_hispanic_pivot, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential enforcement for national coordination. The negative
% externalities are dismissed as necessary costs or "propaganda".
constraint_indexing:constraint_classification(tx_hispanic_pivot, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. The system has a genuine coordination function
% (beneficiaries exist) but also imposes high, asymmetric extraction on a
% specific group (victims exist) and requires active enforcement. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(tx_hispanic_pivot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tx_hispanic_pivot_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(tx_hispanic_pivot, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tx_hispanic_pivot, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_observer_is_tangled_rope) :-
    constraint_indexing:constraint_classification(tx_hispanic_pivot, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tx_hispanic_pivot, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction constraint.

:- end_tests(tx_hispanic_pivot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.70) and suppression (0.60) are high, reflecting a
 * system that delivers significant political power to its beneficiaries while
 * imposing tangible costs (social disruption, fear) on its victims.
 *
 * The Perspectival Gap is stark:
 * - For the South Texas voter (powerless, trapped), the alliance has become a
 *   Snare. The perceived benefit of border security is now outweighed by the
 *   coercive cost of aggressive internal enforcement.
 * - For the GOP apparatus (institutional, mobile), the system is a Rope. It's
 *   a necessary tool for national political coordination and maintaining an
 *   electoral coalition, with local discontent seen as a manageable externality.
 * - The Analytical observer sees both sides. Because the constraint has a
 *   coordination function (beneficiary), asymmetric extraction (victim), and
 *   requires active enforcement, it is classified as a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this as a Tangled Rope is critical. A simple Snare classification
 * would miss the fact that this system emerged from a genuine, if temporary,
 * coordination pact. The Tangled Rope acknowledges the initial Rope-like
 * function (shared interest in border security) while correctly identifying
 * its degradation into a coercive, extractive mechanism. This prevents the
 * system from mislabeling the complex political dynamic as pure, one-sided
 * predation from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tx_pivot_stickiness,
    'Was the 2024 shift a permanent realignment or a temporary protest vote against Democrats?',
    'Compare 2026 and 2028 midterm precinct data in South Texas to 2024 results.',
    'If permanent, the GOP has more latitude for coercive tactics. If temporary, the coalition is fragile and the Snare classification becomes dominant.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_tx_pivot_catalyst,
    'Was the "Renee Good incident" the true catalyst for the approval drop, or just a focal point for pre-existing discontent?',
    'Sentiment analysis of local Texas social media and news coverage pre- and post-incident.',
    'If it was the true catalyst, the constraint is highly sensitive to single events. If not, the decay is structural and harder to reverse.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(tx_hispanic_pivot, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of the political alliance over the
% 2024-2026 period. Extraction rises as the political capital gained in 2024
% is spent on policies the community now opposes.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(tx_hispanic_pivot_tr_t0, tx_hispanic_pivot, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tx_hispanic_pivot_tr_t5, tx_hispanic_pivot, theater_ratio, 5, 0.10).
narrative_ontology:measurement(tx_hispanic_pivot_tr_t10, tx_hispanic_pivot, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(tx_hispanic_pivot_ex_t0, tx_hispanic_pivot, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tx_hispanic_pivot_ex_t5, tx_hispanic_pivot, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(tx_hispanic_pivot_ex_t10, tx_hispanic_pivot, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's primary function is to enforce a political coalition and
% a national policy, making it an enforcement mechanism.
narrative_ontology:coordination_type(tx_hispanic_pivot, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */