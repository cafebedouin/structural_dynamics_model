% ============================================================================
% CONSTRAINT STORY: matching_market_congestion_externality
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-18
% ============================================================================

:- module(constraint_matching_market_congestion_externality, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: matching_market_congestion_externality
 *   human_readable: Matching Market Congestion Externality
 *   domain: economic
 *
 * SUMMARY:
 *   In matching markets (e.g., ride-sharing, online dating, job markets), increased participation can lead to congestion, reducing the matching probability for individual participants. This externality disproportionately affects participants with fewer resources or less attractive profiles who are already disadvantaged. It represents a tangled rope because while the market provides a coordination function (matching), it also introduces a congestion cost that is unevenly distributed.
 *
 * KEY AGENTS (by structural relationship):
 *   - Disadvantaged Participants: Primary target (powerless/constrained) — bears the highest congestion costs.
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefits from increased participation (network effects, data).
 *   - Advantaged Participants: Secondary actors (powerful/mobile) — benefit relative to others, experiencing less congestion.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_market_congestion_externality, 0.35).
domain_priors:suppression_score(matching_market_congestion_externality, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(matching_market_congestion_externality, 0.20).       % Low theater; the function is real, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_market_congestion_externality, extractiveness, 0.35).
narrative_ontology:constraint_metric(matching_market_congestion_externality, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(matching_market_congestion_externality, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(matching_market_congestion_externality, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(matching_market_congestion_externality). % The platform's algorithm is the enforcement mechanism.

% --- Emergence flag (required for mountain constraints) ---
% Not applicable.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(matching_market_congestion_externality, platform_operators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(matching_market_congestion_externality, disadvantaged_participants).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Agent who bears the most extraction. The base metrics (ε=0.35, S=0.50) do not
% meet the minimums for a Snare (ε>=0.46, S>=0.60), so even with high d, the
% classification remains tangled_rope.
constraint_indexing:constraint_classification(matching_market_congestion_externality, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(matching_market_congestion_externality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
constraint_indexing:constraint_classification(matching_market_congestion_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_market_congestion_externality_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(matching_market_congestion_externality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_market_congestion_externality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(matching_market_congestion_externality, ExtMetricName, E),
    E >= 0.30, % Tangled rope requires ε >= 0.30
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(matching_market_congestion_externality, SuppMetricName, S),
    S >= 0.40. % Tangled rope requires suppression >= 0.40

:- end_tests(matching_market_congestion_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The congestion externality arises from increased participation diluting the matching probability, particularly for disadvantaged participants. Base extractiveness is set to 0.35, reflecting this dilution of opportunity. Suppression is 0.50, reflecting the structural difficulty for disadvantaged participants to improve their profile or find viable alternative matching platforms. The platform's matching algorithm constitutes active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The disadvantaged participant, bearing the costs of congestion with limited exit, perceives the market as a tangled_rope. While it provides a matching function, the extraction is high and asymmetric. The platform operator, who benefits from network effects and can arbitrage their position, sees a pure coordination mechanism (rope) with negligible extraction. The gap is between seeing the extraction as a core feature versus a negligible side effect.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are beneficiaries because their business model thrives on network effects from high participation. Disadvantaged participants are victims because they bear the disproportionate cost of the resulting congestion, which manifests as lower matching probability and wasted effort.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as tangled_rope correctly identifies the dual nature of the constraint. It prevents mislabeling the system as a pure snare, which would ignore its genuine coordination function. It also prevents mislabeling it as a pure rope, which would ignore the significant, asymmetrically distributed costs (extraction) imposed on a specific class of users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_matching_market_congestion_externality,
    'To what extent can algorithmic interventions mitigate the congestion externality without introducing new forms of bias or extraction?',
    'Empirical studies of different matching algorithms and their impact on various participant groups over long time horizons.',
    'If true (mitigation is possible and implemented), the constraint could shift towards a rope. If false (mitigation creates new problems), the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(matching_market_congestion_externality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. While not strictly required as ε < 0.46,
% it is included to model the market's maturation and intensification of congestion.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(matching_market_congestion_externality_tr_t0, matching_market_congestion_externality, theater_ratio, 0, 0.10).
narrative_ontology:measurement(matching_market_congestion_externality_tr_t5, matching_market_congestion_externality, theater_ratio, 5, 0.15).
narrative_ontology:measurement(matching_market_congestion_externality_tr_t10, matching_market_congestion_externality, theater_ratio, 10, 0.20).

% Extraction over time (congestion increases as the platform scales):
narrative_ontology:measurement(matching_market_congestion_externality_ex_t0, matching_market_congestion_externality, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(matching_market_congestion_externality_ex_t5, matching_market_congestion_externality, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(matching_market_congestion_externality_ex_t10, matching_market_congestion_externality, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The platform acts as a standard for information exchange to facilitate matches.
narrative_ontology:coordination_type(matching_market_congestion_externality, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */