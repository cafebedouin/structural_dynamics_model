% ============================================================================
% CONSTRAINT STORY: migration_decision_threshold
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_migration_decision_threshold, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: migration_decision_threshold
 *   human_readable: The Migration Decision Threshold (Cost-Benefit Equilibrium)
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint represents the threshold at which the expected utility of
 *   migrating exceeds the costs and risks of remaining in the origin country.
 *   While neo-classical theory frames the "wage differential" as a Mountain for
 *   individuals, the New Economics of Labor Migration (NELM) and World Systems
 *   Theory reveal a more complex structure involving coordination and extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Potential Migrants: Primary target (powerless/trapped) — bears the cost of displacement and brain drain from their origin community.
 *   - Migrant Households: Secondary beneficiary (organized/mobile) — uses migration as a coordination tool to diversify income and mitigate local market failures via remittances.
 *   - Destination Labor Markets: Primary beneficiary (institutional/arbitrage) — benefits from access to a global labor supply, often at lower cost.
 *   - Analytical Observer: Sees the full structure, including coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate extraction (0.3) representing "brain drain" from the
% origin, offset by remittances. Moderate suppression (0.4) representing
% the "market failures" (lack of credit/insurance) in the origin country
% that make migration a necessity rather than a pure choice.
domain_priors:base_extractiveness(migration_decision_threshold, 0.30).
domain_priors:suppression_score(migration_decision_threshold, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(migration_decision_threshold, 0.08).       % Low theater; this is a highly substantive economic process.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(migration_decision_threshold, extractiveness, 0.30).
narrative_ontology:constraint_metric(migration_decision_threshold, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(migration_decision_threshold, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
% The system has both a genuine coordination function (remittances, risk
% diversification) and asymmetric extraction (brain drain, suppressed local
% alternatives), enforced by border regimes. This is the definition of a Tangled Rope.
narrative_ontology:constraint_claim(migration_decision_threshold, tangled_rope).
narrative_ontology:human_readable(migration_decision_threshold, "The Migration Decision Threshold (Cost-Benefit Equilibrium)").

% --- Binary flags ---
% The system is upheld by legal border regimes, visa requirements, and
% immigration enforcement. Required for Tangled Rope.
domain_priors:requires_active_enforcement(migration_decision_threshold).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(migration_decision_threshold, destination_labor_markets).
narrative_ontology:constraint_beneficiary(migration_decision_threshold, migrant_households_remittances).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(migration_decision_threshold, potential_migrants).
narrative_ontology:constraint_victim(migration_decision_threshold, origin_country_local_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE POTENTIAL MIGRANT (TANGLED ROPE)
% Agent is trapped by local market failures and faces an extractive system.
% While they may perceive the wage gap as an unchangeable Mountain, the
% system's metrics (ε=0.3, s=0.4) and structure (enforcement, beneficiaries)
% classify it as a Tangled Rope from their powerless position.
% χ = 0.30 * f(d=0.95) * σ(national=1.0) ≈ 0.30 * 1.42 * 1.0 = 0.426
constraint_indexing:constraint_classification(migration_decision_threshold, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DESTINATION STATE/MARKET (ROPE)
% The primary institutional beneficiary, gaining access to labor. From this
% perspective, the system is a pure coordination mechanism for allocating
% human capital where it is most demanded.
% χ = 0.30 * f(d=0.05) * σ(national=1.0) ≈ 0.30 * -0.12 * 1.0 = -0.036
constraint_indexing:constraint_classification(migration_decision_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MIGRANT HOUSEHOLD (ROPE)
% Per the New Economics of Labor Migration (NELM), the household acts as an
% organized unit, using migration as a coordination tool (a Rope) to
% diversify income and manage risk, overcoming local market failures.
% χ = 0.30 * f(d=0.15) * σ(regional=0.9) ≈ 0.30 * -0.01 * 0.9 = -0.0027
constraint_indexing:constraint_classification(migration_decision_threshold, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A World Systems theorist sees both the coordination function (labor
% allocation, remittances) and the coercive, extractive core (disruption of
% periphery economies, creation of a dependent labor pool). This dual nature
% is the hallmark of a Tangled Rope.
% χ = 0.30 * f(d=0.72) * σ(global=1.2) ≈ 0.30 * 1.15 * 1.2 = 0.414
constraint_indexing:constraint_classification(migration_decision_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(migration_decision_threshold_tests).

test(perspectival_gap) :-
    % Verify that the target (powerless) and beneficiary (institutional) disagree.
    constraint_indexing:constraint_classification(migration_decision_threshold, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(migration_decision_threshold, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    TypeTarget == tangled_rope,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_analytical_claim) :-
    % The analytical claim must be Tangled Rope, reflecting the dual nature.
    narrative_ontology:constraint_claim(migration_decision_threshold, tangled_rope).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(migration_decision_threshold, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(migration_decision_threshold, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(migration_decision_threshold).

:- end_tests(migration_decision_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a Mountain.
 *   However, its metrics (ε=0.3, s=0.4) violate the Mountain thresholds
 *   (ε<=0.25, s<=0.05). The narrative itself describes a system with both
 *   coordination (remittances, risk diversification for households) and
 *   asymmetric extraction (brain drain, suppressed local alternatives),
 *   upheld by active enforcement (border regimes). This is the canonical
 *   definition of a Tangled Rope. The classification has been corrected to
 *   reflect this structure.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For beneficiaries (destination countries, migrant
 *   households), the system is a functional Rope for allocating labor or
 *   managing risk. For the individual migrant trapped by circumstance, it is
 *   an extractive Tangled Rope. The system's ability to appear as a Rope to
 *   its beneficiaries is key to its stability.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `destination_labor_markets` and `migrant_households_remittances`
 *     directly benefit from the flow of labor and money, respectively. The engine
 *     assigns them low directionality (d), resulting in low/negative effective
 *     extraction (χ) and a Rope classification.
 *   - Victims: `potential_migrants` and `origin_country_local_services` bear the
 *     costs of displacement and brain drain. The engine assigns them high
 *     directionality (d), resulting in high χ and a Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies that the system is
 *   not a neutral law of nature (Mountain) nor a purely beneficial coordination
 *   tool (Rope). It has a genuine coordination function that is coupled to an
 *   extractive process, preventing the mislabeling of the system's coercive
 *   aspects as simple economic efficiency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_migration_decision_threshold,
    'Is the primary driver the wage-gap "pull" (Mountain-like) or the local market-failure "push" (Snare-like)?',
    'Evaluation of migration rates in regions with newly introduced micro-credit and insurance programs.',
    'If rates drop, the "push" was dominant (Tangled Rope/Snare). If rates stay high, the "pull" is dominant (closer to a perceived Mountain).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(migration_decision_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the intensification of global migration pressures over the modern era.
% While not strictly required (ε < 0.46), this data shows a slight increase in
% the extractive nature of the system over time.

% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(migration_decision_threshold_tr_t0, migration_decision_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(migration_decision_threshold_tr_t5, migration_decision_threshold, theater_ratio, 5, 0.06).
narrative_ontology:measurement(migration_decision_threshold_tr_t10, migration_decision_threshold, theater_ratio, 10, 0.08).

% Extraction over time (slight increase reflecting brain drain effects):
narrative_ontology:measurement(migration_decision_threshold_ex_t0, migration_decision_threshold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(migration_decision_threshold_ex_t5, migration_decision_threshold, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(migration_decision_threshold_ex_t10, migration_decision_threshold, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system functions to allocate a key resource (human labor) across
% different economic zones.
narrative_ontology:coordination_type(migration_decision_threshold, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */