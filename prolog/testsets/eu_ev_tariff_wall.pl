% ============================================================================
% CONSTRAINT STORY: eu_ev_tariff_wall
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-18
% ============================================================================

:- module(constraint_eu_ev_tariff_wall, []).

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
 *   constraint_id: eu_ev_tariff_wall
 *   human_readable: EU Tariffs and Trade Barriers on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   In response to a surge of low-cost, technologically advanced electric
 *   vehicles from China, the European Union is implementing tariffs and
 *   anti-subsidy investigations. This policy acts as a protective barrier
 *   for legacy European automakers, but it increases costs for European
 *   consumers and restricts market access for Chinese EV manufacturers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Legacy European Automakers: Primary beneficiary (institutional/arbitrage) — benefits from reduced competition and protected market share.
 *   - European Consumers: Primary target (powerless/trapped) — bears extraction through higher prices and fewer choices.
 *   - Chinese EV Manufacturers: Primary target (organized/constrained) — bears extraction through blocked market access and reduced profitability.
 *   - EU Commission: Institutional architect/beneficiary (institutional/arbitrage) - benefits by executing a political mandate to protect domestic industry.
 *   - Analytical Observer: Analytical observer — sees both the coordination function (market protection) and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_ev_tariff_wall, 0.55). % High extraction of market share and consumer surplus.
domain_priors:suppression_score(eu_ev_tariff_wall, 0.75).   % Structural property (raw, unscaled). High suppression of competitive alternatives.
domain_priors:theater_ratio(eu_ev_tariff_wall, 0.10).       % Piton detection (>= 0.70). This is a highly functional, non-theatrical policy.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_ev_tariff_wall, extractiveness, 0.55).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_ev_tariff_wall, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_ev_tariff_wall). % Required for Tangled Rope. Tariffs require customs enforcement.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a constructed policy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, legacy_european_automakers).
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, eu_commission).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_ev_tariff_wall, chinese_ev_manufacturers).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, european_consumers).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (European Consumers)
% As victims with no exit, they perceive the tariffs as a pure Snare.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Legacy European Automakers)
% As beneficiaries with arbitrage exit, they perceive it as a pure coordination mechanism (Rope).
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination function and the asymmetric extraction, classifying it as a Tangled Rope.
% This is the basis for the constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates between multiple organized actors with different relationships.

% PERSPECTIVE 4: THE EXTERNAL TARGET (Chinese EV Manufacturers)
% As the direct victims of the policy, they see it as a Snare designed to extract their market share.
% Their 'organized' power and 'constrained' exit still result in a high d.
% Engine derives d from: victim membership + constrained exit → d ≈ 0.90 → f(d) ≈ 1.35 → high χ.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ARCHITECT (EU Commission)
% As the institutional actor implementing the policy, they see it as a legitimate Rope for market coordination.
% Their classification aligns with the other primary beneficiaries.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_ev_tariff_wall_tests).

test(perspectival_gap_consumer_vs_automaker, [nondet]) :-
    % Verify the gap between the trapped consumer (Snare) and the institutional beneficiary (Rope).
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_chinese_vs_european_industry, [nondet]) :-
    % Verify the gap between the targeted Chinese firms (Snare) and protected EU firms (Rope).
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical view must resolve the perspectival gaps into a Tangled Rope.
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_conditions_met) :-
    % Verify that all structural preconditions for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, _),
    narrative_ontology:constraint_victim(eu_ev_tariff_wall, _),
    domain_priors:requires_active_enforcement(eu_ev_tariff_wall).

:- end_tests(eu_ev_tariff_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The policy's primary function is to extract market
 *     share from foreign competitors and transfer value (via higher prices) from
 *     consumers to domestic producers. The value is high as it's intended to be
 *     prohibitive for a significant portion of imports.
 *   - Suppression Score (0.75): The tariffs and investigations are an active, coercive
 *     mechanism to suppress the availability of a superior/cheaper alternative.
 *   - The combination of high extraction, high suppression, a clear coordination
 *     function (protecting the domestic industrial base), and active enforcement
 *     makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For beneficiaries (legacy EU automakers, EU Commission), this is a
 *   'Rope'—a necessary tool to coordinate an orderly market transition and ensure fair
 *   competition. For targets (Chinese EV makers, EU consumers), it's a 'Snare'—a
 *   coercive mechanism that extracts value and limits choice with no coordinating benefit
 *   for them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'legacy_european_automakers' and the 'eu_commission' directly
 *     benefit from protected markets and the execution of industrial policy. Their
 *     'arbitrage' exit option gives them low directionality (d), leading to a low/negative
 *     effective extraction (χ) and a 'Rope' classification.
 *   - Victims: 'chinese_ev_manufacturers' are targeted directly, facing 'constrained'
 *     exit (building local factories is a costly, slow alternative). 'european_consumers'
 *     are 'trapped' in a higher-cost market. Both are assigned to the victim group,
 *     which derives a high d, leading to high χ and a 'Snare' classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the policy. A naive analysis
 *   might label it purely as protectionist extraction (Snare) or purely as fair-trade
 *   coordination (Rope). The Tangled Rope classification, derived from the analytical
 *   perspective, acknowledges both claims are structurally present. It prevents the EU's
 *   coordination narrative from obscuring the very real, asymmetric extraction imposed
 *   on Chinese firms and EU consumers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_eu_ev_tariff_wall,
    'Are the EU tariffs a legitimate response to unfair state subsidies (coordination) or simply protectionist rent-seeking (extraction)?',
    'A neutral, third-party (e.g., WTO) quantitative analysis of Chinese state support vs. the scale of EU tariffs.',
    'If subsidies are proven to be market-distorting, the coordination function is stronger, though extraction remains. If not, the policy is almost pure extraction (closer to Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_ev_tariff_wall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a threat to an active policy,
% and its potential future decay into a Piton if it remains after its purpose is served.
% Required since base_extractiveness (0.55) > 0.46.

% Theater ratio over time (low when functional, high if it becomes inertial)
narrative_ontology:measurement(eu_ev_tariff_wall_tr_t0, eu_ev_tariff_wall, theater_ratio, 0, 0.30).
narrative_ontology:measurement(eu_ev_tariff_wall_tr_t5, eu_ev_tariff_wall, theater_ratio, 5, 0.10).
narrative_ontology:measurement(eu_ev_tariff_wall_tr_t10, eu_ev_tariff_wall, theater_ratio, 10, 0.80).

% Extraction over time (low before tariffs, high now, lower if competition is eliminated)
narrative_ontology:measurement(eu_ev_tariff_wall_ex_t0, eu_ev_tariff_wall, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(eu_ev_tariff_wall_ex_t5, eu_ev_tariff_wall, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(eu_ev_tariff_wall_ex_t10, eu_ev_tariff_wall, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The policy allocates market share by restricting access.
narrative_ontology:coordination_type(eu_ev_tariff_wall, resource_allocation).

% Network relationships: This policy is a reaction to other economic constraints.
narrative_ontology:affects_constraint(china_industrial_subsidies, eu_ev_tariff_wall).
narrative_ontology:affects_constraint(us_ira_subsidies, eu_ev_tariff_wall).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, global_ev_adoption_rate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation based on
% beneficiary/victim declarations and exit options accurately captures the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */