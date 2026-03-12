% ============================================================================
% CONSTRAINT STORY: theory_laden_perception
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theory_laden_perception, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: theory_laden_perception
 *   human_readable: Theory-Laden Perception
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   Theory-laden perception is the epistemic constraint that all observation
 *   is structured by prior conceptual frameworks — there is no unmediated
 *   perceptual access to phenomena. This constraint appears across multiple
 *   domains: Kuhn's paradigm-dependent observation in philosophy of science,
 *   Hanson's theory-laden seeing in philosophy of perception, top-down
 *   processing in cognitive psychology, predictive coding in neuroscience,
 *   and the hermeneutic circle in interpretive philosophy. The constraint is
 *   not that observers disagree about what they see (that would be a
 *   coordination problem), but that the act of seeing itself is constituted
 *   through conceptual apparatus. The empirical signature includes:
 *   experimental replication failures when paradigm shifts occur (observers
 *   trained in different frameworks literally see different things in the
 *   same data), perceptual psychology studies showing expectation effects
 *   (priming changes what subjects report perceiving), and the historical
 *   record of scientific revolutions (Priestley saw dephlogisticated air
 *   where Lavoisier saw oxygen — same phenomenon, incommensurable
 *   observations). This is a mountain-only constraint: it classifies as
 *   mountain from all perspectives because it reflects a structural feature
 *   of cognition, not a contingent institutional arrangement. The low
 *   extractiveness (0.08) reflects minimal asymmetric benefit — all observers
 *   are equally constrained. The low suppression (0.02) reflects that the
 *   constraint emerges from cognitive architecture, not from coercive
 *   enforcement. The constraint has no beneficiaries or victims because it is
 *   not an extraction mechanism — it is a limit condition on what observation
 *   can be.
 *
 * KEY AGENTS:
 *   - Naive Observer: Powerless/trapped — unaware of conceptual mediation; experiences perception as direct
 *   - Trained Scientist: Moderate/constrained — aware of paradigm dependence through exposure to competing frameworks; can switch theories but cannot escape theory-dependence
 *   - Scientific Community: Organized/mobile — can shift paradigms collectively over generational time; new paradigm still structures observation
 *   - Philosophical Tradition: Institutional/arbitrage — from Kant through Kuhn to contemporary embodied cognition; consensus that perception is always already structured
 *   - Analytical Observer: Analytical/analytical — confirms mountain classification through cross-paradigm analysis; meta-framework is itself a framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theory_laden_perception, 0.08).
domain_priors:suppression_score(theory_laden_perception, 0.02).
domain_priors:theater_ratio(theory_laden_perception, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theory_laden_perception, extractiveness, 0.08).
narrative_ontology:constraint_metric(theory_laden_perception, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(theory_laden_perception, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(theory_laden_perception, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(theory_laden_perception, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theory_laden_perception, mountain).
narrative_ontology:human_readable(theory_laden_perception, "Theory-Laden Perception").
narrative_ontology:topic_domain(theory_laden_perception, "epistemology/cognitive_science/philosophy_of_mind").

domain_priors:emerges_naturally(theory_laden_perception).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NAIVE OBSERVER (MOUNTAIN) — Cannot escape conceptual mediation even when unaware of it. Every perceptual act is structured by prior frameworks. The constraint is invisible precisely because it is inescapable — the observer has no vantage point outside their own conceptual apparatus.
constraint_indexing:constraint_classification(theory_laden_perception, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: TRAINED SCIENTIST (MOUNTAIN) — Recognizes paradigm dependence through exposure to competing frameworks, but cannot observe without some framework. Can switch between theories but cannot achieve theory-free observation. The constraint remains immutable even with meta-cognitive awareness.
constraint_indexing:constraint_classification(theory_laden_perception, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SCIENTIFIC COMMUNITY (MOUNTAIN) — Collective can shift paradigms over generational time, but the new paradigm still structures observation. Kuhnian revolutions replace one conceptual framework with another; they do not escape framework-dependence itself. The constraint is invariant across paradigm shifts.
constraint_indexing:constraint_classification(theory_laden_perception, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOSOPHICAL TRADITION (MOUNTAIN) — From Kant's categories of understanding through Kuhn's paradigms to contemporary embodied cognition, the philosophical consensus is that perception is always already structured. No institutional arrangement changes this — it is a feature of how cognition works, not a contingent social fact.
constraint_indexing:constraint_classification(theory_laden_perception, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The analytical position confirms rather than challenges the mountain classification. Cross-paradigm analysis reveals that the structure of theory-ladenness is invariant: every framework structures observation, and the meta-framework used to analyze this is itself a framework. This is not naturalization of a contingent constraint — it is recognition of a cognitive-structural limit.
constraint_indexing:constraint_classification(theory_laden_perception, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theory_laden_perception_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(theory_laden_perception, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theory_laden_perception, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(theory_laden_perception, ExtMetricName, E),
    domain_priors:suppression_score(theory_laden_perception, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(theory_laden_perception),
    narrative_ontology:constraint_metric(theory_laden_perception, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(theory_laden_perception, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(theory_laden_perception_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint does not create asymmetric benefit. All observers are equally subject to conceptual mediation. The minimal extractiveness reflects the cognitive cost of framework-dependence (observers cannot access phenomena directly, which imposes an epistemic tax), but this cost is universal and non-extractive. Suppression (0.02): Very low. The constraint is not enforced — it emerges from the structure of cognition itself. No agent or institution maintains it. Observers cannot exit because the constraint is constitutive of observation, not because alternatives are suppressed. Accessibility collapse (0.92): Very high. Attempts to observe without a framework collapse immediately into implicit framework use. The 'view from nowhere' is inaccessible. Resistance (0.08): Very low. The constraint has persisted across every paradigm shift, every philosophical tradition, and every attempt to achieve theory-free observation. No counterexample has survived scrutiny. Theater ratio (0.15): Very low. Claims about theory-ladenness are empirically testable through perceptual psychology experiments and historical analysis of paradigm shifts. The constraint is not maintained through performative ritual — it is demonstrated through replicable phenomena.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification type — all perspectives classify as mountain. The gap is in awareness, not in structural experience. The naive observer is unaware of conceptual mediation. The trained scientist recognizes paradigm dependence. The scientific community experiences paradigm shifts. The philosophical tradition theorizes the constraint. The analytical observer confirms its invariance. But all experience the same structural reality: observation is always already theory-laden. The uniformity of classification across perspectives is itself diagnostic — it confirms that this is a genuine natural law constraint, not a contingent institutional arrangement being naturalized.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims, so directionality values default to the canonical fallback for each power atom. All perspectives derive d from their power level alone, not from structural relationship to an extraction flow. The resulting chi values are uniformly low across all perspectives because base extractiveness is very low (0.08) and no perspective experiences amplification through victim status. The mountain classification is invariant because the constraint's structural properties (very low epsilon, very low suppression, very high accessibility collapse, very low resistance, emerges naturally) meet the mountain thresholds from every index.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that mountain classification is appropriate when structural properties meet the natural law thresholds from all perspectives. The constraint is not coordination (no collective action problem being solved), not extraction (no asymmetric benefit), not temporary (no sunset mechanism), not degraded (no theatrical maintenance), and not hybrid (no mixed function). It is an immutable limit on what observation can be. The analytical observer's mountain classification is NOT a false summit — the cross-paradigm analysis confirms that theory-ladenness is invariant across all frameworks, including the meta-framework used to analyze it. This is the paradigm case of a legitimate mountain: a constraint that emerges from cognitive architecture, exhibits maximum accessibility collapse and minimum resistance, and classifies identically from every structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theory_laden_perception, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theory_laden_perception, information_standard).
narrative_ontology:affects_constraint(theory_laden_perception, incommensurability_thesis).
narrative_ontology:affects_constraint(theory_laden_perception, underdetermination_of_theory).
narrative_ontology:affects_constraint(theory_laden_perception, observational_equivalence).

% DUAL FORMULATION NOTE:
% Theory-laden perception is the foundational constraint in a family of epistemological limits. Incommensurability (the claim that competing paradigms cannot be directly compared) is downstream of theory-ladenness (if observation is paradigm-dependent, paradigms structure what counts as evidence). Underdetermination (the claim that evidence does not uniquely determine theory choice) is also downstream (if observation is theory-laden, the same data can support multiple frameworks). Observational equivalence (the claim that empirically indistinguishable theories may differ in unobservable structure) is downstream (if observation is mediated by theory, theories can differ in ways observation cannot adjudicate). Each has its own epsilon value reflecting its own empirical status, but all depend structurally on theory-ladenness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
