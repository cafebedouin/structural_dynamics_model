% ============================================================================
% CONSTRAINT STORY: djia_as_economic_barometer
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_djia_as_economic_barometer, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: djia_as_economic_barometer
 *   human_readable: The Dow Jones Industrial Average as a primary barometer of national economic health.
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint is the widespread social and political practice of using the
 *   Dow Jones Industrial Average (DJIA), an index of 30 large-cap US companies,
 *   as a primary and often sole indicator of national economic well-being.
 *   While providing a coordination signal for investors, its prominence in media
 *   and politics masks underlying economic realities for the majority of the
 *   population, creating a significant perspectival gap.
 *
 * KEY AGENTS (by structural relationship):
 *   - Wage Earners & Non-Investors: Primary target (powerless/trapped) — The narrative of a "strong economy" based on the DJIA obscures their stagnant wages and rising costs, suppressing political demand for redistributive policies.
 *   - Institutional Investors & Capital Owners: Primary beneficiary (institutional/arbitrage) — The focus on the DJIA reinforces policies and sentiment favorable to capital growth, and provides a simple, high-visibility coordination signal.
 *   - Financial Media & Political Incumbents: Secondary beneficiary (organized/mobile) - Use the DJIA as a simple, powerful narrative tool to signal economic success or failure.
 *   - Analytical Observer: An economist or systems analyst (analytical/analytical) — Sees the full structure, including the coordination function for investors and the extractive, misleading function for the general public.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(djia_as_economic_barometer, 0.48).
domain_priors:suppression_score(djia_as_economic_barometer, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(djia_as_economic_barometer, 0.75).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(djia_as_economic_barometer, extractiveness, 0.48).
narrative_ontology:constraint_metric(djia_as_economic_barometer, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(djia_as_economic_barometer, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(djia_as_economic_barometer, tangled_rope).
narrative_ontology:human_readable(djia_as_economic_barometer, "The Dow Jones Industrial Average as a primary barometer of national economic health.").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(djia_as_economic_barometer).
domain_priors:requires_active_enforcement(djia_as_economic_barometer). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, institutional_investors).
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, political_incumbents).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(djia_as_economic_barometer, wage_earners).
narrative_ontology:constraint_victim(djia_as_economic_barometer, non_investors).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) - MET
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
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (WAGE EARNER / NON-INVESTOR)
% Bears the cost of policy misdirection. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
%   χ = 0.48 * 1.42 * 1.0 (national) ≈ 0.68. This meets the snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(djia_as_economic_barometer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (INSTITUTIONAL INVESTOR)
% Benefits from policy direction and coordination. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
%   χ = 0.48 * (-0.12) * 1.0 (national) ≈ -0.06. This is a clear Rope.
constraint_indexing:constraint_classification(djia_as_economic_barometer, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the dual function. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% The observer sees the global implications of US market signals.
% χ = 0.48 * 1.15 * 1.2 (global) ≈ 0.66. It has both a coordination function
% (beneficiary declared) and asymmetric extraction (victim declared), plus
% active enforcement. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(djia_as_economic_barometer, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(djia_as_economic_barometer_tests).

test(perspectival_gap) :-
    % Verify the core Rope/Snare perspectival gap.
    constraint_indexing:constraint_classification(djia_as_economic_barometer, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(djia_as_economic_barometer, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival Gap Test Passed: Snare(powerless) vs Rope(institutional)').

test(tangled_rope_analytical_view) :-
    % Verify the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(djia_as_economic_barometer, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Analytical View Test Passed: Classified as Tangled Rope').

test(tangled_rope_gate_compliance) :-
    % Verify all three structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, _),
    narrative_ontology:constraint_victim(djia_as_economic_barometer, _),
    domain_priors:requires_active_enforcement(djia_as_economic_barometer),
    format('Tangled Rope Gate Test Passed: Beneficiary, Victim, and Enforcement all present.').


:- end_tests(djia_as_economic_barometer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The extraction is not direct financial cost, but
 *     the opportunity cost of policy. By focusing national attention on a metric
 *     that primarily benefits capital, the constraint diverts political will and
 *     discourse away from policies that could aid labor (e.g., minimum wage
 *     hikes, corporate tax increases, stronger union laws). This represents a
 *     significant transfer of potential resources and political power.
 *   - Suppression (s=0.65): The DJIA's dominance in headlines and political
 *     discourse is immense. Alternative, more representative metrics (e.g.,
 *     median income, labor share of GDP, household debt) are systematically
 *     sidelined, making it very difficult to build a popular counternarrative.
 *   - Theater (θ=0.75): The public-facing function of the DJIA is almost entirely
 *     theatrical. Bell-ringing ceremonies, constant chyrons on news channels, and
 *     politicians celebrating milestones are performative acts far removed from
 *     the index's narrow technical function. This high theater ratio indicates a
 *     drift towards a Piton-like state, where the symbol is more important than
 *     the substance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the constraint. For an institutional investor
 *   with capital mobility (arbitrage exit), the DJIA is a useful, low-cost
 *   coordination signal (Rope). For a wage earner (trapped exit), the same
 *   constraint acts as a Snare: the triumphant narrative of a rising Dow invalidates
 *   their personal economic struggles and suppresses political avenues for redress.
 *   They are trapped in a narrative that does not reflect their reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by an agent's relationship to capital.
 *   - Beneficiaries (`institutional_investors`) own capital. The constraint reinforces
 *     the value of their assets and promotes a favorable policy environment.
 *     The engine derives a low `d` value, leading to a Rope classification.
 *   - Victims (`wage_earners`) sell labor. The constraint systematically
 *     devalues their economic contribution relative to capital and diverts
 *     policy attention away from their needs. The engine derives a high `d`
 *     value, leading to a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of Mandatrophy. A mechanism with a legitimate,
 *   if narrow, coordination function (Rope for investors) is presented as a
 *   universal public good. The Tangled Rope classification from the analytical
 *   perspective correctly diagnoses this duality, preventing the misclassification
 *   of the system as either a pure Rope (the beneficiary's view) or a pure Snare
 *   (the victim's view). It acknowledges the coordination function while correctly
 *   identifying the high, asymmetric extraction it enables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_djia_as_economic_barometer,
    'Is the DJIA''s dominance a result of deliberate institutional promotion to mask extraction, or an emergent property of media incentives and public demand for simple narratives?',
    'Historical analysis of financial media ownership, political campaign finance data, and public statements from regulatory bodies.',
    'If deliberate (Tangled Rope), it implies a system with correctable agency. If emergent (closer to Piton), it implies a more intractable problem of systemic inertia and cognitive bias.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(djia_as_economic_barometer, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint has intensified over
% the last 30 years with the rise of 24-hour financial news and the deepening
% of financialization. Time=0 is ~1994, Time=10 is ~2024.
% Base extraction (ε > 0.46) requires this section.

% Theater ratio over time (metric_substitution): The performative aspect has grown.
narrative_ontology:measurement(djia_tr_t0, djia_as_economic_barometer, theater_ratio, 0, 0.50).
narrative_ontology:measurement(djia_tr_t5, djia_as_economic_barometer, theater_ratio, 5, 0.65).
narrative_ontology:measurement(djia_tr_t10, djia_as_economic_barometer, theater_ratio, 10, 0.75).

% Extraction over time (extraction_accumulation): The gap between asset growth
% and wage growth has widened, increasing the extractive nature of the narrative.
narrative_ontology:measurement(djia_ex_t0, djia_as_economic_barometer, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(djia_ex_t5, djia_as_economic_barometer, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(djia_ex_t10, djia_as_economic_barometer, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(djia_as_economic_barometer, information_standard).

% Network relationships (structural influence edges)
% The focus on the DJIA directly influences monetary policy discussions and
% public perception of other economic indicators.
narrative_ontology:affects_constraint(djia_as_economic_barometer, federal_reserve_policy_focus).
narrative_ontology:affects_constraint(djia_as_economic_barometer, consumer_confidence_indices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain based on beneficiary/victim declarations and exit options accurately
% captures the structural relationships between capital owners and labor.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */