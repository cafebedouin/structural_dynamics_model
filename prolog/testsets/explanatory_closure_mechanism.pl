% ============================================================================
% CONSTRAINT STORY: explanatory_closure_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_explanatory_closure_mechanism, []).

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
 *   constraint_id: explanatory_closure_mechanism
 *   human_readable: Explanatory Closure Mechanism in Human Cognition
 *   domain: epistemology/cognitive_science/social_psychology
 *
 * SUMMARY:
 *   The explanatory closure mechanism is the cognitive process by which an
 *   explanation feeling subjectively complete terminates further variable
 *   search, even when additional relevant variables exist. This mechanism
 *   appears to be a universal feature of human inference: when a causal story
 *   achieves internal coherence and accounts for salient observations, it
 *   produces a phenomenological sense of 'doneness' that inhibits continued
 *   search for alternative explanations or unobserved confounders. The
 *   constraint operates pre-reflectively — the feeling of closure precedes
 *   and shapes deliberate reasoning rather than resulting from it.
 *   Developmental psychology shows the mechanism is present in early
 *   childhood; cross-cultural studies show it operates across diverse
 *   epistemic traditions; neuroscience identifies consistent
 *   prefrontal-striatal activation patterns during search termination. The
 *   mechanism trades off computational cost (exhaustive search is
 *   prohibitively expensive for bounded agents) against inference quality
 *   (premature termination produces systematically incomplete causal models).
 *   This is a candidate natural law: an immutable feature of human cognitive
 *   architecture that shapes all downstream epistemic practices.
 *
 * KEY AGENTS:
 *   - Naive Reasoner: Powerless/trapped — experiences closure phenomenology with no metacognitive awareness or compensatory tools
 *   - Trained Scientist: Moderate/constrained — has metacognitive awareness and procedural safeguards but cannot eliminate the underlying mechanism
 *   - Research Institution: Institutional/arbitrage — can build structures that compensate for individual closure bias but cannot remove the cognitive primitive
 *   - Analytical Observer: Analytical/analytical — sees the mechanism as a structural feature of bounded rationality, not a contingent social arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(explanatory_closure_mechanism, 0.22).
domain_priors:suppression_score(explanatory_closure_mechanism, 0.04).
domain_priors:theater_ratio(explanatory_closure_mechanism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(explanatory_closure_mechanism, extractiveness, 0.22).
narrative_ontology:constraint_metric(explanatory_closure_mechanism, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(explanatory_closure_mechanism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(explanatory_closure_mechanism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(explanatory_closure_mechanism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(explanatory_closure_mechanism, mountain).
narrative_ontology:human_readable(explanatory_closure_mechanism, "Explanatory Closure Mechanism in Human Cognition").
narrative_ontology:topic_domain(explanatory_closure_mechanism, "epistemology/cognitive_science/social_psychology").

domain_priors:emerges_naturally(explanatory_closure_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NAIVE REASONER (MOUNTAIN) — Cannot override the phenomenology of explanatory closure through conscious effort alone. When an explanation feels complete, the search-termination signal is automatic and pre-reflective. The mechanism operates below the level of voluntary control.
constraint_indexing:constraint_classification(explanatory_closure_mechanism, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: TRAINED SCIENTIST (MOUNTAIN) — Can develop metacognitive awareness of the closure mechanism and implement procedural safeguards (systematic variable enumeration, pre-registered analysis plans, adversarial collaboration), but cannot eliminate the underlying phenomenology. The feeling of explanatory completeness still occurs; training provides tools to question it, not to prevent it.
constraint_indexing:constraint_classification(explanatory_closure_mechanism, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (MOUNTAIN) — Can build institutional structures that compensate for individual closure bias (peer review, replication requirements, adversarial review panels), but the underlying cognitive mechanism persists across all individual researchers. Institutional design works around the constraint rather than removing it.
constraint_indexing:constraint_classification(explanatory_closure_mechanism, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The explanatory closure mechanism appears to be a structural feature of human inference architecture, not a contingent institutional arrangement. Cross-cultural universality, developmental invariance, and neurobiological substrate all suggest this is a cognitive primitive. The mechanism trades off search costs against inference quality — a computational constraint on bounded rationality.
constraint_indexing:constraint_classification(explanatory_closure_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(explanatory_closure_mechanism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(explanatory_closure_mechanism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(explanatory_closure_mechanism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(explanatory_closure_mechanism, ExtMetricName, E),
    domain_priors:suppression_score(explanatory_closure_mechanism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(explanatory_closure_mechanism),
    narrative_ontology:constraint_metric(explanatory_closure_mechanism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(explanatory_closure_mechanism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(explanatory_closure_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low but non-zero. The mechanism extracts some epistemic cost — premature search termination produces systematically incomplete causal models, which propagates through all downstream inference. But the extraction is minimal because the mechanism also provides substantial coordination benefit: it enables bounded agents to reach 'good enough' conclusions in finite time rather than being paralyzed by infinite search. The trade-off is approximately optimal for most everyday inference tasks. Suppression (0.04): Very low. The mechanism does not suppress alternatives through coercion — it operates through phenomenology (the feeling of completeness) rather than through blocking access to alternative explanations. Agents with sufficient metacognitive training can recognize and question closure, though they cannot eliminate the underlying feeling. Theater ratio (0.15): Low. The mechanism is functional, not performative. The phenomenology of closure genuinely terminates search; it is not a ritual maintained for social signaling. Accessibility collapse (0.92): Very high. The mechanism is a cognitive primitive — it operates automatically and pre-reflectively across all human populations. Resistance (0.08): Very low. No cultural or institutional intervention has successfully eliminated the mechanism, only built compensatory structures around it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as mountain. The naive reasoner, trained scientist, research institution, and analytical observer all experience the mechanism as immutable, though they differ in their capacity to compensate for it. The naive reasoner has no metacognitive awareness; the trained scientist has procedural safeguards; the institution has structural compensations; the analytical observer has theoretical understanding. But none can eliminate the underlying cognitive primitive. The uniformity across perspectives is itself diagnostic: when a constraint classifies as mountain from powerless/trapped through analytical/analytical, it is a strong candidate for genuine natural law rather than naturalized social construction.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as mountain because the constraint appears to be a fixed feature of human cognitive architecture. There are no beneficiaries or victims in the structural sense — the mechanism is not an institutional arrangement that extracts from some agents to benefit others. Instead, it is a universal computational constraint that all agents face equally. The 'extraction' (incomplete causal models) is a necessary cost of bounded rationality, not asymmetric rent-seeking. The low extractiveness reflects that the mechanism provides substantial coordination benefit (enables finite-time inference) alongside its epistemic cost (premature search termination). All agents, regardless of power or exit options, experience the same underlying phenomenology and face the same computational trade-off.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that not all constraints are extractive institutional arrangements. The explanatory closure mechanism is a computational feature of bounded rationality — a necessary trade-off between search cost and inference quality. The low extractiveness (0.22) reflects that the mechanism provides substantial benefit (enables finite-time inference) alongside its cost (incomplete causal models). The mountain classification from all perspectives, combined with cross-cultural universality and neurobiological substrate, provides strong evidence that this is a genuine natural law rather than a false summit. The three omega variables identify the empirical tests that could falsify the mountain classification: if the neural substrate is modifiable, if alternative architectures avoid the trade-off, or if cultural variation is substantial, the constraint would reclassify. Until those tests are resolved, the mountain classification stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neural_substrate_modifiability,
    'Is the neural substrate implementing explanatory closure modifiable through intervention (pharmacological, neurofeedback, genetic), or is it a fixed architectural feature?',
    'Neuroscience research on prefrontal-striatal circuits mediating search termination; intervention studies testing whether closure threshold can be systematically shifted',
    'If modifiable: constraint is rope or scaffold (coordination mechanism with adjustable parameters). If fixed: constraint is mountain (immutable cognitive architecture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neural_substrate_modifiability, empirical, 'Whether neural closure mechanism is modifiable or architecturally fixed').

omega_variable(
    computational_necessity,
    'Is explanatory closure computationally necessary for bounded agents, or could alternative inference architectures achieve similar performance without premature search termination?',
    'Computational modeling of alternative search-termination strategies; analysis of whether closure-free architectures face prohibitive computational costs',
    'If computationally necessary: mountain classification confirmed (unavoidable trade-off). If alternatives exist: constraint is contingent on current cognitive architecture, not fundamental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(computational_necessity, conceptual, 'Whether closure is computationally necessary for bounded rationality').

omega_variable(
    cultural_variation_magnitude,
    'Do cultural differences in epistemic norms produce meaningful variation in closure threshold, or is cross-cultural variation negligible relative to within-culture individual differences?',
    'Cross-cultural experimental studies measuring closure threshold; decomposition of variance into cultural vs individual components',
    'If substantial cultural variation: some component of the constraint is socially constructed (tangled rope). If negligible: constraint is primarily biological (mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_variation_magnitude, empirical, 'Magnitude of cultural variation in closure threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(explanatory_closure_mechanism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expl_closure_tr_t0, explanatory_closure_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(expl_closure_tr_t50, explanatory_closure_mechanism, theater_ratio, 50, 0.15).
narrative_ontology:measurement(expl_closure_tr_t100, explanatory_closure_mechanism, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(expl_closure_be_t0, explanatory_closure_mechanism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(expl_closure_be_t50, explanatory_closure_mechanism, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(expl_closure_be_t100, explanatory_closure_mechanism, base_extractiveness, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(explanatory_closure_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% The explanatory closure mechanism is a cognitive primitive that affects all downstream epistemic constraints but is not itself decomposable into multiple structural claims with different epsilon values. The mechanism is the same whether measured by phenomenological report, behavioral search termination, or neural activation patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
