% ============================================================================
% CONSTRAINT STORY: three_body_unpredicability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_three_body_unpredicability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: three_body_unpredicability
 *   human_readable: The Three-Body Problem (Computational Irreducibility)
 *   domain: physics/mathematics/chaos_theory
 *
 * SUMMARY:
 *   The three-body problem represents a fundamental and irreducible limit on
 *   the predictability of gravitational systems. Unlike the two-body problem
 *   (solved by Newton/Kepler, yielding closed-form elliptical orbits), no
 *   general analytical solution exists for three or more bodies under mutual
 *   gravitational attraction. This constraint is not imposed by any actor,
 *   institution, or coordination failure — it emerges from the mathematical
 *   structure of gravitational dynamics itself. The unpredictability is
 *   maximal not because of measurement error, computational capacity, or
 *   institutional barriers, but because the system is chaotic: arbitrarily
 *   small differences in initial conditions lead to exponentially diverging
 *   trajectories. This makes the three-body problem an exemplary mountain
 *   constraint: it has ε=0.12 (very low extractiveness, since no agent
 *   benefits from the constraint or bears a cost within the system),
 *   suppression=0.03 (no suppression of alternatives — this is the only
 *   mathematical reality), accessibility_collapse=0.92 (maximum
 *   inaccessibility — the problem is provably hard), and resistance=0.08
 *   (minimal resistance to accepting the constraint — it is mathematically
 *   proven). The constraint applies universally and invariantly across all
 *   observational frames: no institutional change, technological advancement,
 *   or resource allocation can circumvent the fundamental limit on analytical
 *   predictability. The history of the three-body problem from Newton through
 *   Poincaré to modern chaos theory reveals not a constraint being
 *   'tightened' by actors, but a progressive recognition that the
 *   mathematical landscape itself contains irreducible barriers.
 *
 * KEY AGENTS:
 *   - Mathematics/Physics Community: Observer/steward (analytical/analytical) — responsible for characterizing the limit and developing approximation methods
 *   - Computational Systems: Non-strategic actor (powerless/trapped) — any system attempting numerical integration faces the chaos horizon regardless of capability
 *   - Space Mission Planners: Institutional stakeholders (institutional/constrained) — must design around the predictability limit
 *   - Natural Systems (Binary Stars, Planetary Systems): Non-agent systems (system-relative) — exhibit the three-body dynamics directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_unpredicability, 0.12).
domain_priors:suppression_score(three_body_unpredicability, 0.03).
domain_priors:theater_ratio(three_body_unpredicability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_unpredicability, extractiveness, 0.12).
narrative_ontology:constraint_metric(three_body_unpredicability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(three_body_unpredicability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(three_body_unpredicability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(three_body_unpredicability, mountain).
narrative_ontology:human_readable(three_body_unpredicability, "The Three-Body Problem (Computational Irreducibility)").
narrative_ontology:topic_domain(three_body_unpredicability, "physics/mathematics/chaos_theory").

domain_priors:emerges_naturally(three_body_unpredicability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL PHYSICIST (MOUNTAIN) — The three-body problem represents a fundamental limit of predictability in gravitational systems. No closed-form solution exists; trajectories cannot be analytically predicted beyond short timescales regardless of initial condition precision. This is a property of the mathematical structure itself, not a limitation of current computational capacity. ε=0.12, suppression=0.03, accessibility_collapse=0.92, resistance=0.08. Classification stable across all observables.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ASTROPHYSICIST (MOUNTAIN) — Even with arbitrarily high computational power and precise initial measurements, gravitational three-body systems exhibit sensitive dependence on initial conditions (chaos). Prediction horizons remain finite and intrinsic to the dynamics. This is not a constraint imposed by external actors but an irreducible property of the system. The astrophysicist cannot 'negotiate' their way out of this limit through institutional arrangements or funding. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. The classification remains Mountain regardless of perspective shift.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SPACE AGENCY MISSION PLANNER (MOUNTAIN) — Space mission design for planetary encounters, satellite trajectories near binary stars, or multi-body gravitational interactions must account for fundamental computational irreducibility. No amount of institutional coordination or resource concentration can circumvent the mathematical constraint. The planner's exit option (constrained) reflects that design must accommodate the limit, not escape it. This perspective confirms the constraint is not a coordination problem masquerading as a natural law, but an actual natural law. d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.11. Mountain classification holds.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CELESTIAL MECHANICS STUDENT (MOUNTAIN) — From the perspective of someone learning orbital mechanics, the three-body problem appears as an irreducible barrier to understanding. Unlike the two-body problem, which yields closed Kepler orbits, three bodies show no simplifying pattern. The student cannot 'exit' this constraint through study or effort — it is a fundamental asymmetry in the mathematical landscape. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.14. Even from a structurally disadvantaged position, the constraint is recognized as natural law, not oppression.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_unpredicability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_unpredicability, ExtMetricName, E),
    domain_priors:suppression_score(three_body_unpredicability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(three_body_unpredicability),
    narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(three_body_unpredicability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(three_body_unpredicability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.12): Minimal. The three-body problem does not extract value from any agent or group — it is a neutral mathematical fact. The small nonzero value reflects that improved computational approximation methods are possible, meaning the constraint has no absolute ceiling (unlike a true ε=0 limit), but this approximation space is intrinsic to the problem structure, not a resource controlled by any actor. Suppression (0.03): Minimal. There are no suppressed alternatives — analytical solutions do not exist, and numerical approximations are available to anyone with computational access. No actor benefits from keeping the three-body problem difficult. Resistance to accepting the constraint is minimal because the mathematics is proven. Theater ratio (0.15): Low. The three-body problem requires little performative framing — its difficulty is widely acknowledged and mathematically transparent. The small nonzero value reflects that physics education includes simplified versions (restricted three-body problem, perturbative approximations) that create an appearance of tractability before students encounter the irreducible unpredictability. Accessibility collapse (0.92): Very high. The three-body problem is notoriously difficult to access — even professional physicists and mathematicians require substantial training to understand the chaos dynamics and proof of non-integrability. Resistance (0.08): Very low. The mathematical community has converged on the conclusion that three-body systems are chaotic and analytically intractable. There is minimal resistance to this characterization.
 *
 * PERSPECTIVAL GAP:
 *   MINIMAL PERSPECTIVAL GAP — This is a uniform-type mountain constraint. All perspectives classify the constraint as mountain, which is the expected outcome for a natural law. The perspectival gap is absence of a gap: the mathematical physicist sees irreducible unpredictability, the astrophysicist sees it as a boundary on prediction, the mission planner sees it as a design constraint they must accommodate, and even the student trapped by its difficulty recognizes it as a natural law, not extraction. This invariance across all observation points is exactly what validates the mountain classification. If one perspective had classified it as a snare or scaffold, the constraint would be suspected of hidden institutional extraction masquerading as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   NOT APPLICABLE — Mountain constraints have no directionality derivation. The three-body problem is not a relationship between agents; it is a property of physical systems. There are no beneficiaries or victims, no exit options that vary by actor, no institutional imposition. The derivation chain (beneficiary/victim + power + exit → d) does not apply because the constraint is not extractive. All perspectives derive from the analytical observer's position (d=0.72 canonical), but this is a formality — the classification is determined by ε and the natural law profile metrics (accessibility_collapse, resistance), not by directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    numerical_vs_analytical_irreducibility,
    'Is the three-body unpredictability a property of gravitational dynamics itself, or a consequence of computational limitations in numerical integration?',
    'Demonstration that no analytical method (symbolic computation, perturbative expansion, or alternative mathematical formalism) can predict trajectories beyond the empirical chaos horizon; proof that the horizon is independent of computational precision',
    'If analytical: true mountain — the constraint is mathematical/logical. If numerical: upgraded to scaffold (computational improvements might extend prediction horizon). Current evidence strongly supports analytical interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(numerical_vs_analytical_irreducibility, empirical, 'Whether unpredictability is inherent or computational').

omega_variable(
    chaos_vs_ergodicity_boundary,
    'Does the three-body system exhibit deterministic chaos (sensitive dependence) or ergodic randomness (trajectory fills phase space uniformly)?',
    'Lyapunov exponent calculation; long-term statistical analysis of escape vs bound orbits; correlation dimension measurement',
    'If deterministic chaos: classification stable — mountain. If ergodic: might require reframing as probabilistic constraint with different ε (still mountain, but with different underlying interpretation). This distinction does not change the classification but clarifies the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaos_vs_ergodicity_boundary, empirical, 'Mechanism underlying the predictability limit').

omega_variable(
    restricted_vs_general_three_body,
    'Does the constraint apply equally to the restricted three-body problem (two heavy bodies + one test mass) or only to the general problem (three comparable masses)?',
    'Comparison of prediction horizons for restricted vs general systems; analysis of whether perturbative methods extend prediction timescales in the restricted case',
    'If restricted is significantly different: might require separate constraint story for restricted problem with higher ε (scaffold-level). General case remains mountain. Current evidence suggests qualitative similarity despite quantitative differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restricted_vs_general_three_body, empirical, 'Scope of the unpredictability across three-body variants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(three_body_unpredicability, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tbp_tr_t0, three_body_unpredicability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tbp_tr_t100, three_body_unpredicability, theater_ratio, 100, 0.14).
narrative_ontology:measurement(tbp_tr_t300, three_body_unpredicability, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(tbp_be_t0, three_body_unpredicability, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tbp_be_t100, three_body_unpredicability, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(tbp_be_t300, three_body_unpredicability, base_extractiveness, 300, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(three_body_unpredicability, information_standard).
narrative_ontology:affects_constraint(three_body_unpredicability, chaos_theory_determinism).
narrative_ontology:affects_constraint(three_body_unpredicability, computational_complexity_limits).

% DUAL FORMULATION NOTE:
% The three-body problem is upstream of practical constraints in space mission design and celestial mechanics prediction. The computational irreducibility (three_body_unpredictability) is a mountain-level natural law; downstream constraints in orbital mechanics and mission planning are scaffolds and tangled ropes that work *within* the bounds set by this irreducibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
