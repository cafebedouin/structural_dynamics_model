% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
 *   constraint_id: thermodynamics_entropy
 *   human_readable: The Second Law of Thermodynamics (Entropy)
 *   domain: technological/physics/physical_law
 *
 * SUMMARY:
 *   The Second Law of Thermodynamics is a canonical example of a natural law
 *   constraint — a physical limit that applies uniformly across all
 *   observers, power levels, time horizons, and spatial scopes. The
 *   constraint states that in any isolated system, entropy (disorder) must
 *   increase or remain constant; it cannot decrease. This is not a policy
 *   enforced by institutions, not a coordination mechanism that could be
 *   negotiated, and not an extractive mechanism that benefits some agents at
 *   the expense of others. It is an immutable property of physical systems
 *   that emerges from the statistical mechanics of large ensembles. The
 *   constraint operates equally on technological systems, biological
 *   organisms, industrial processes, and cosmic phenomena. No agent,
 *   regardless of power or institutional position, can violate it through
 *   cleverness, organization, or arbitrage.
 *
 * KEY AGENTS:
 *   - Thermodynamic Systems: Universal subject (all systems of matter and energy)
 *   - Engineers and Technology Designers: Attempt to manage entropy through efficient processes and energy distribution; subject to the constraint
 *   - Analytical Observer: Examines the constraint from a mathematical and physical standpoint; recognizes its universality
 *   - Technological Society: Collective agent attempting to optimize energy use and minimize waste; constrained by Second Law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermodynamics_entropy, 0.08).
domain_priors:suppression_score(thermodynamics_entropy, 0.02).
domain_priors:theater_ratio(thermodynamics_entropy, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.08).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(thermodynamics_entropy, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).
narrative_ontology:human_readable(thermodynamics_entropy, "The Second Law of Thermodynamics (Entropy)").
narrative_ontology:topic_domain(thermodynamics_entropy, "technological/physics/physical_law").

domain_priors:emerges_naturally(thermodynamics_entropy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THERMODYNAMIC SYSTEM (MOUNTAIN) — All matter and energy configurations are subject to entropy increase. No escape from the fundamental constraint. Disorder in isolated systems must increase or remain constant. This is not a policy, not an institutional arrangement, not a coordination problem. It is an irreducible property of physical law. The system has zero degrees of freedom relative to this constraint.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a mathematical and physical standpoint, the Second Law follows from probabilistic arguments about microstates and macroscopic observable properties. It is not enforced by any agent or institution. It emerges necessarily from the statistical mechanics of large ensembles. No observer, regardless of power or position, can violate or escape this constraint. It applies uniformly across all contexts and time horizons.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENGINEERING INSTITUTION (MOUNTAIN) — Even institutional actors with maximal power and arbitrage options cannot circumvent entropy increase. They may manage energy flows, design efficient processes, and redistribute disorder (entropy export to surroundings), but they cannot violate the Second Law. It is a constraint on their capabilities, not a constraint they can exploit or negotiate. The law is unchanged by their position or exit options.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: COLLECTIVE OF AGENTS (MOUNTAIN) — Organized groups and societies cannot collectively escape entropy increase through coordination or coalition. Even global coordination mechanisms cannot violate the Second Law. The constraint is invariant across all forms of social organization and collective action. Technological advancement allows better management of entropy (Maxwell's demon designs, reversible computing), but not its violation.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermodynamics_entropy, ExtMetricName, E),
    domain_priors:suppression_score(thermodynamics_entropy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermodynamics_entropy),
    narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Second Law does not extract value from any agent for the benefit of another. It is a constraint on all systems equally, not a mechanism by which one agent's costs become another's benefits. The small non-zero value reflects that physical law does impose costs on all processes — irreversible processes dissipate energy — but this is not extraction in the institutional sense; it is a universal tax on all matter and energy transformations. Suppression (0.02): Negligible. There are no alternatives to the Second Law. It is not suppressed by keeping knowledge hidden or by preventing exit; it simply applies. Accessibility (0.92): Very high. The Second Law is experimentally verified, mathematically derived, and applies across all observed phenomena. There is no collapse or accessibility barrier — the law is fully transparent. Resistance (0.08): Very low. Empirical resistance to the Second Law is essentially zero. All observed physical systems obey it. Theater ratio (0.05): Minimal. The Second Law is not performed or theatrical. Its function is its reality — entropy increase happens whether or not anyone observes or documents it.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for this constraint. All four perspectives classify it identically as Mountain. This is the hallmark of a natural law: the classification is invariant across all observational contexts. The powerless agent and the institutional actor see the same constraint. The immediate-horizon trader and the civilizational observer see the same constraint. The agent trapped in a local region and the global analyst see the same constraint. The absence of perspectival variation is itself the evidence for the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Second Law does not have directionality in the institutional sense. All agents and systems are equally subject to it. The constraint applies uniformly regardless of power level (powerless individuals and institutional actors experience the same physical law), time horizon (immediate and civilizational perspectives both observe entropy increase), exit options (trapped and arbitrage agents cannot escape it), or spatial scope (local and universal systems all follow the law). The derivation chain for directionality is inapplicable to mountains — there are no beneficiaries or victims, only universal subjects of physical law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint exemplifies the mandatrophy solution through invariance. The mandatrophy asks: 'Is this pure coordination (Rope) mislabeled as extraction (Snare)?' For the Second Law, the answer is neither — it is pure constraint, not extraction or coordination. The constraint cannot be reframed as coordination because no agent benefits at another's expense; the cost (entropy increase) is universal and intrinsic, not transferred. It cannot be reframed as extraction because there is no beneficiary. The mandatrophy resolves to 'natural law' — a class distinct from both Rope and Snare. The Second Law's universality across all perspectives confirms that it is a Mountain, not a hidden Rope or Snare. If the perspectives had disagreed — if, for example, some observers saw it as extractive and others as coordination — that would signal a false mountain (naturalization of a contingent arrangement). The invariance proves the law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thermodynamic_boundary_definition,
    'Is entropy increase truly universal, or is it an artifact of how we define ''isolated system'' boundaries?',
    'Philosophical analysis of boundary conditions; examination of open vs closed systems in real-world scenarios; study of entropy accounting in complex multi-level systems',
    'If boundary definition is fundamental: Mountain classification is robust. If boundary is observer-dependent: classification may shift to Rope (coordination via boundary choice). This is the core omega for whether the law is truly universal or partially conventional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thermodynamic_boundary_definition, conceptual, 'Whether entropy universality depends on boundary definition').

omega_variable(
    reversibility_and_practical_limits,
    'Do reversible processes (Carnot cycles, adiabatic operations) represent exceptions or limiting cases of the Second Law?',
    'Theoretical examination of reversible vs irreversible processes; empirical testing of reversible computing and quantum algorithms; analysis of whether reversibility requires infinite time/energy',
    'If reversible processes are true exceptions: classification may downgrade to Rope or Scaffold. If they are unattainable limiting cases: Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_and_practical_limits, empirical, 'Whether reversible processes are true exceptions or limiting cases').

omega_variable(
    quantum_violations_and_measurement,
    'Do quantum phenomena (entanglement, time-reversal symmetry, measurement-induced state change) reveal fundamental violations of thermodynamic entropy in closed systems?',
    'Resolution of the measurement problem in quantum mechanics; analysis of entropy in entangled systems; determination of whether quantum coherence preserves or violates classical entropy bounds',
    'If quantum mechanics violates Second Law in principle: Mountain classification fails; constraint decomposes into classical (Mountain) and quantum (Snare or Tangled Rope) stories. If quantum mechanics preserves Second Law: Mountain is confirmed at all scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_violations_and_measurement, empirical, 'Whether quantum mechanics reveals violations of thermodynamic entropy').

omega_variable(
    cosmological_boundary_conditions,
    'Does the universe as a whole constitute an isolated system subject to the Second Law, or is it exempt by definition?',
    'Cosmological analysis of the early universe''s entropy state; determination of whether the universe''s initial low-entropy condition is a boundary condition or a constraint violation; study of the arrow of time',
    'If universe is genuinely isolated and constrained: Second Law is truly universal (Mountain). If universe''s boundary conditions are set independently: the Second Law may be local to subsystems, not universal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmological_boundary_conditions, conceptual, 'Whether the universe is subject to the Second Law or exempt by definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermodynamics_entropy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(therm_ent_tr_t0, thermodynamics_entropy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(therm_ent_tr_t5, thermodynamics_entropy, theater_ratio, 5, 0.05).
narrative_ontology:measurement(therm_ent_tr_t10, thermodynamics_entropy, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(therm_ent_be_t0, thermodynamics_entropy, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(therm_ent_be_t5, thermodynamics_entropy, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(therm_ent_be_t10, thermodynamics_entropy, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermodynamics_entropy, global_infrastructure).
narrative_ontology:affects_constraint(thermodynamics_entropy, heat_death_universe).
narrative_ontology:affects_constraint(thermodynamics_entropy, reversible_computing_limits).
narrative_ontology:affects_constraint(thermodynamics_entropy, maxwell_demon_impossibility).

% DUAL FORMULATION NOTE:
% The Second Law can be formulated in multiple ways (Clausius, Kelvin-Planck, Boltzmann statistical mechanics, information-theoretic) but all formulations are mathematically equivalent and empirically identical. These are not decomposed into separate constraint stories because they do not have different epsilon values or structural properties — they are alternative mathematical representations of the same physical law. The network links to downstream constraints that depend on the Second Law's universality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
