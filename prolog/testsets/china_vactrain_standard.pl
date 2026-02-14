% ============================================================================
% CONSTRAINT STORY: china_vactrain_standard
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_china_vactrain_standard, []).

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
 *   constraint_id: china_vactrain_standard
 *   human_readable: China's Ultra-High-Speed Vacuum-Tube Maglev Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   China is developing an ultra-high-speed (700-1000 km/h) maglev train
 *   system that runs in a low-vacuum tube to reduce air resistance. This
 *   constraint represents the techno-economic and political framework
 *   required to develop, fund, and deploy such a network. It functions as
 *   both a massive national coordination project and a highly extractive
 *   system that concentrates benefits while distributing immense costs.
 *
 * KEY AGENTS (by structural relationship):
 *   - displaced_landowners: Primary target (powerless/trapped) — bear land acquisition costs with no recourse.
 *   - competing_transportation_sectors: Secondary target (organized/constrained) — lose funding and market share to the new system.
 *   - chinese_state_and_soes: Primary beneficiary (institutional/arbitrage) — gain technological prestige, economic control, and profits.
 *   - technology_policy_analyst: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_vactrain_standard, 0.55).
domain_priors:suppression_score(china_vactrain_standard, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(china_vactrain_standard, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_vactrain_standard, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_vactrain_standard, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_vactrain_standard, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(china_vactrain_standard, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(china_vactrain_standard). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(china_vactrain_standard, chinese_state_and_soes).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(china_vactrain_standard, displaced_landowners).
narrative_ontology:constraint_victim(china_vactrain_standard, competing_transportation_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (Displaced Landowner)
% For a person whose land is expropriated for the project, the system is
% purely extractive with no recourse. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(china_vactrain_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Chinese State & State-Owned Enterprises)
% For the state and its industrial champions, this is a powerful tool for
% national coordination, economic development, and asserting technological
% leadership. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(china_vactrain_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the genuine coordination function and the massive,
% asymmetric extraction required to achieve it. This dual nature is the
% definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL COMPETITOR (Domestic Aviation Sector)
% For competing sectors like aviation or existing high-speed rail, the
% project is a state-backed predator. It extracts capital, political favor,
% and ultimately market share. Their exit is constrained; they cannot
% simply ignore national policy. They are victims, but organized ones.
% Engine derives d from: victim membership + constrained exit -> d ~ 0.8
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_vactrain_standard_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(china_vactrain_standard, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(china_vactrain_standard, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions_met) :-
    % A constraint can only be a Tangled Rope if it has both coordination
    % (beneficiary) and extraction (victim) functions, and requires enforcement.
    narrative_ontology:constraint_claim(china_vactrain_standard, tangled_rope),
    narrative_ontology:constraint_beneficiary(china_vactrain_standard, _),
    narrative_ontology:constraint_victim(china_vactrain_standard, _),
    domain_priors:requires_active_enforcement(china_vactrain_standard).

test(threshold_validation) :-
    % Base extractiveness and suppression must be high for a Tangled Rope.
    domain_priors:base_extractiveness(china_vactrain_standard, E), E >= 0.30,
    domain_priors:suppression_score(china_vactrain_standard, S), S >= 0.40.

:- end_tests(china_vactrain_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.55) is high due to the monumental capital
 *   investment, land acquisition, and energy costs, representing a massive
 *   diversion of national resources. Suppression (0.65) is also high because
 *   such a project monopolizes transport infrastructure funding and political
 *   will, crowding out investment in competing or complementary modes like
 *   aviation or conventional high-speed rail. The Theater Ratio (0.20) is
 *   low, as this is a genuine, technologically ambitious project, not merely
 *   a performance of progress. These metrics, combined with the dual-use
 *   nature of the project, point squarely to Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Chinese state (beneficiary), it's a Rope: a
 *   tool to coordinate economic activity, bind the nation closer, and project
 *   power. For a landowner whose property is in the way (victim), it's a
 *   Snare: an inescapable, extractive force imposed from above. The analytical
 *   perspective reconciles this by classifying it as a Tangled Rope,
 *   acknowledging that the coordination is real but is achieved through
 *   coercive, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is top-down. The 'chinese_state_and_soes' are declared
 *   beneficiaries, as they initiate the constraint and reap the primary
 *   strategic and economic rewards. Their 'arbitrage' exit option gives them
 *   low directionality (d). 'displaced_landowners' and
 *   'competing_transportation_sectors' are declared victims, as they bear
 *   the direct and indirect costs. Their 'trapped' and 'constrained' exits
 *   give them high directionality (d), leading to a high effective
 *   extraction (χ) from their perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a
 *   pure Snare because the national transportation coordination it provides
 *   is a genuine, large-scale benefit. It is not a pure Rope because the
 *   cost is not borne symmetrically; it is a state-directed project that
 *   extracts immense resources and suppresses alternatives coercively. The
 *   Tangled Rope classification is essential for capturing this complex reality
 *   where a "public good" is created via highly extractive means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_china_vactrain_standard,
    'Will the vactrain system achieve a positive economic return on investment (ROI) on a civilizational timescale, or will it become a monumental debt trap and white elephant project?',
    'Long-term (50+ year) analysis of operational costs, ticket revenues, and quantified macroeconomic benefits (e.g., productivity gains from reduced travel time) vs. total construction and maintenance expenditure.',
    'If ROI is positive, its Rope-like characteristics are strengthened. If ROI is deeply negative, it drifts towards being a Piton maintained for national pride, or a pure Snare extracting from the national budget.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_vactrain_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε > 0.46) constraint.
% Models the project moving from R&D (t=0) to deployment (t=5) and
% projected maturity (t=10), with costs and nationalistic theater increasing.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cvs_tr_t0, china_vactrain_standard, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cvs_tr_t5, china_vactrain_standard, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cvs_tr_t10, china_vactrain_standard, theater_ratio, 10, 0.25).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cvs_ex_t0, china_vactrain_standard, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cvs_ex_t5, china_vactrain_standard, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cvs_ex_t10, china_vactrain_standard, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This is a textbook case of building large-scale infrastructure.
narrative_ontology:coordination_type(china_vactrain_standard, global_infrastructure).

% Network relationships: This project structurally impacts other transportation
% and technology standards.
narrative_ontology:affects_constraint(china_vactrain_standard, global_high_speed_rail_standards).
narrative_ontology:affects_constraint(china_vactrain_standard, domestic_aviation_market).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% based on the declared beneficiary/victim groups and their exit options
% accurately models the structural power dynamics of the scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */