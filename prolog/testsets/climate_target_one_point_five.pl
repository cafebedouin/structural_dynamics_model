% ============================================================================
% CONSTRAINT STORY: climate_target_one_point_five
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_climate_target_one_point_five, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_target_one_point_five
 *   human_readable: The 1.5°C Global Warming Target
 *   domain: political/environmental
 *
 * SUMMARY:
 *   The 1.5°C target is a global policy constraint that lowered the "safe"
 *   warming threshold from 2°C. Championed by the Alliance of Small Island
 *   States (AOSIS), it redefines acceptable climate risk based on the survival
 *   of the most vulnerable nations rather than the economic convenience of
 *   larger powers. It functions as both a coordination mechanism for global
 *   climate action and an extractive limit on high-emission development paths.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small Island States (AOSIS): Primary beneficiary (organized/trapped) — uses the target as a survival mechanism.
 *   - Fossil Fuel Reliant Economies: Primary target (institutional/constrained) — bears the cost of constrained development.
 *   - Citizen in a Coastal Community: Secondary target (powerless/trapped) — experiences the physical reality of climate change, for whom the policy target is largely abstract.
 *   - Analytical Observer: Sees the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The target extracts economic potential and development freedom from high-emission paths.
domain_priors:base_extractiveness(climate_target_one_point_five, 0.30).
% Rationale: The previous 2°C "safe" threshold has been largely suppressed and delegitimized.
domain_priors:suppression_score(climate_target_one_point_five, 0.60).
% Rationale: The target is a serious policy goal with high functional activity, not performative.
domain_priors:theater_ratio(climate_target_one_point_five, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_target_one_point_five, extractiveness, 0.30).
narrative_ontology:constraint_metric(climate_target_one_point_five, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(climate_target_one_point_five, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(climate_target_one_point_five, tangled_rope).

% --- Binary flags ---
% Rationale: Requires active enforcement through UN summits and international agreements.
domain_priors:requires_active_enforcement(climate_target_one_point_five).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(climate_target_one_point_five, small_island_states).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(climate_target_one_point_five, fossil_fuel_reliant_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN IN A COASTAL COMMUNITY (MOUNTAIN)
% For a citizen trapped by rising sea levels, the policy target is abstract.
% The physical reality of climate change is an unchangeable force, a Mountain.
constraint_indexing:constraint_classification(climate_target_one_point_five, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AOSIS NEGOTIATOR (ROPE)
% For the Alliance of Small Island States, the target is a pure coordination
% mechanism—a lifeline to pull the world away from a 2°C "death sentence".
constraint_indexing:constraint_classification(climate_target_one_point_five, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FOSSIL FUEL RELIANT ECONOMY (SNARE)
% For a nation focused on rapid industrialization via fossil fuels, the target
% is a Snare. It tightens emission limits, strangling perceived economic growth.
constraint_indexing:constraint_classification(climate_target_one_point_five, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees the dual nature: a genuine coordination function (Rope for
% AOSIS) combined with asymmetric extraction (Snare for developing nations).
constraint_indexing:constraint_classification(climate_target_one_point_five, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_target_one_point_five_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between different actors.
    constraint_indexing:constraint_classification(climate_target_one_point_five, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_target_one_point_five, TypeOrganized, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(climate_target_one_point_five, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOrganized,
    TypeOrganized \= TypeInstitutional,
    TypePowerless \= TypeInstitutional.

test(tangled_rope_structural_properties) :-
    narrative_ontology:constraint_claim(climate_target_one_point_five, tangled_rope),
    domain_priors:requires_active_enforcement(climate_target_one_point_five),
    narrative_ontology:constraint_beneficiary(climate_target_one_point_five, _),
    narrative_ontology:constraint_victim(climate_target_one_point_five, _).

:- end_tests(climate_target_one_point_five_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.30) is moderate, reflecting the constraint on
 *   economic development paths. The suppression score (0.60) is high because
 *   the 1.5°C target has effectively delegitimized the previous 2°C consensus.
 *   The theater ratio (0.10) is low, as this is a functional, actively negotiated policy.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a citizen in a vulnerable community, the policy is
 *   an abstract 'Mountain' against the physical reality of climate change. For
 *   AOSIS, it's a 'Rope' for survival. For a fossil-fuel-reliant economy, it's a
 *   'Snare' restricting development. This divergence is central to the constraint's identity.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'small_island_states' who championed the target to ensure their physical survival.
 *   - Victims: 'fossil_fuel_reliant_economies' who bear the cost of accelerated decarbonization and lost development opportunities.
 *   This clear opposition drives the Tangled Rope classification from an analytical view.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope correctly identifies the dual nature of
 *   the constraint. It prevents mislabeling it as a pure Snare (ignoring its
 *   vital coordination function for vulnerable nations) or a pure Rope
 *   (ignoring the real economic extraction it imposes on others).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_climate_target_one_point_five,
    'Is the 1.5°C target physically achievable given current emission inertia, or has it become a purely symbolic constraint?',
    'Monitoring global temperature anomalies and GHG concentrations over 2026-2030; assessment of national climate action plans.',
    'If unachievable: The constraint risks degrading into a Piton (high theater, low function). If achievable: It remains a powerful Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_climate_target_one_point_five, empirical, 'Physical attainability of the 1.5C target given current global inertia.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(climate_target_one_point_five, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required
% by the linter for lifecycle drift detection.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The target functions as a global enforcement mechanism for climate policy.
narrative_ontology:coordination_type(climate_target_one_point_five, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately captures the directionality of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */