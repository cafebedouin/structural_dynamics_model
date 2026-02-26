% ============================================================================
% CONSTRAINT STORY: artificial_scarcity_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_artificial_scarcity_scaffold, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: artificial_scarcity_scaffold
 *   human_readable: The Resource-Migration Scaffold
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models a planned, temporary period of artificial scarcity
 *   designed to compel users to migrate from a legacy technology or resource
 *   to a new one. The 'scaffold' is the set of policies (e.g., ending
 *   support, raising prices, limiting availability) that makes staying on the
 *   old system untenable, thereby supporting the construction of a new,
 *   universal standard. Examples include forcing users from on-premise
 *   software to cloud subscriptions, or phasing out leaded gasoline. The core
 *   tension is between the stated goal of beneficial progress and the
 *   coercive, extractive nature of the mechanism used to achieve it.
 *
 * KEY AGENTS:
 *   - Platform Owner: Primary beneficiary (institutional/arbitrage) — orchestrates the migration to consolidate their market on the new platform.
 *   - Legacy Users: Primary victims (moderate/constrained) — bear the financial and operational costs of the forced migration.
 *   - Late Adopters: Secondary victims (powerless/trapped) — face the highest costs and risk being left with a non-functional system.
 *   - Future Userbase: Indirect beneficiary (organized/mobile) — inherits the benefits of the new standard without experiencing the transition costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(artificial_scarcity_scaffold, 0.35).
domain_priors:suppression_score(artificial_scarcity_scaffold, 0.65).
domain_priors:theater_ratio(artificial_scarcity_scaffold, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(artificial_scarcity_scaffold, scaffold).
narrative_ontology:human_readable(artificial_scarcity_scaffold, "The Resource-Migration Scaffold").
narrative_ontology:topic_domain(artificial_scarcity_scaffold, "technological/economic").

domain_priors:requires_active_enforcement(artificial_scarcity_scaffold).
narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, platform_owner).
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, new_resource_provider).
narrative_ontology:constraint_victim(artificial_scarcity_scaffold, legacy_users).
narrative_ontology:constraint_victim(artificial_scarcity_scaffold, late_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ADOPTER (SNARE) — Trapped after the legacy system is fully deprecated. Faces exorbitant migration costs or complete loss of function with no viable alternatives. Experiences the constraint as pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.60.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEGACY USER (TANGLED ROPE) — Experiences both the coordination benefit of an industry-wide standard shift and the extractive cost of a forced migration. Their exit is constrained by switching costs and dependencies on the legacy system. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OWNER (SCAFFOLD) — The architect of the transition. Sees the artificial scarcity as a necessary, temporary support structure to achieve a desirable end-state (universal adoption of the new resource). As the primary beneficiary with full control, they experience no extraction. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SCAFFOLD) — Recognizes the temporary nature ('has_sunset_clause') and the coordination function of the constraint. The classification holds because the coercive elements are time-bound and serve a stated transitional purpose, fitting the definition of a scaffold. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.48, but the sunset clause gate makes it a scaffold.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: FUTURE USERBASE (ROPE) — From a future standpoint, the turmoil of the transition is invisible. This group only experiences the new, superior, and standardized resource. The constraint appears as a pure coordination mechanism that established the beneficial status quo. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.004.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artificial_scarcity_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(artificial_scarcity_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.35): Moderate. The constraint imposes real costs (financial, time, labor) on users forced to migrate. It's not pure coordination; value is extracted during the transition. Suppression (0.65): High. The mechanism works by actively suppressing the alternative of staying with the legacy system, making it prohibitively expensive or non-functional. Theater Ratio (0.20): Low. While there is marketing rhetoric, the primary function—forcing migration—is direct and non-performative. The `has_sunset_clause` is true by definition, as the scarcity is designed to end once migration is complete.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The Platform Owner sees a necessary, temporary 'Scaffold' to build a better future for everyone. The Legacy User, bearing the costs, sees a 'Tangled Rope'—a mix of coercive extraction and a potential coordination benefit. The Late Adopter, with no options left, sees a pure 'Snare'. This perspectival divergence is characteristic of constraints where benefits and costs are asymmetrically distributed over time and across populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Platform Owner) have arbitrage exit and control the terms, leading to a low/negative effective extraction (χ), classifying the constraint as a Scaffold from their view. Victims (Legacy Users, Late Adopters) have constrained or trapped exit options, leading to a high derived directionality (d) and thus a high χ. This pushes their classification towards Tangled Rope and Snare, accurately reflecting their experience of being targeted by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This case prevents mandatrophy by correctly using the 'Scaffold' classification. A naive analysis might label it a 'Snare' due to the high suppression and coercive nature. However, the explicit and credible 'has_sunset_clause' is a critical structural feature. The DR system correctly identifies it as a Scaffold from the analytical and beneficiary perspectives, while still acknowledging the Snare-like experience of the victims. It distinguishes a temporary, goal-oriented coercive measure from a permanent system of pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_necessity,
    'Is the enforced scarcity truly a necessary mechanism for the migration, or is it a pretext for rent-seeking on the legacy platform before its obsolescence?',
    'Comparative analysis of similar technological transitions, contrasting outcomes of voluntary vs. forced migration paths. Economic modeling of platform revenue streams during the transition period.',
    'If the scarcity is proven unnecessary for a successful transition, the constraint''s primary classification shifts from Scaffold to Tangled Rope or Snare, as the ''coordination'' claim becomes theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_necessity, empirical, 'Whether the artificial scarcity is a necessary tool or a rent-seeking pretext.').

omega_variable(
    benefit_distribution,
    'Do the long-term benefits of the new resource equitably compensate for the short-term costs imposed on legacy users?',
    'Longitudinal cost-benefit analysis tracking total cost of ownership for migrated users versus the quantified benefits of the new platform over a 5-10 year horizon.',
    'If migration costs systematically outweigh the realized benefits for a significant user segment, the constraint functions as a Snare for that segment, even if it is a Scaffold for others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benefit_distribution, empirical, 'Whether long-term benefits outweigh the imposed short-term migration costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(artificial_scarcity_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, artificial_scarcity_scaffold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t5, artificial_scarcity_scaffold, theater_ratio, 5, 0.25).
narrative_ontology:measurement(arti_tr_t10, artificial_scarcity_scaffold, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, artificial_scarcity_scaffold, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(arti_be_t5, artificial_scarcity_scaffold, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(arti_be_t10, artificial_scarcity_scaffold, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(artificial_scarcity_scaffold, resource_allocation).
narrative_ontology:affects_constraint(artificial_scarcity_scaffold, legacy_system_interoperability).
narrative_ontology:affects_constraint(artificial_scarcity_scaffold, new_platform_lock_in).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
