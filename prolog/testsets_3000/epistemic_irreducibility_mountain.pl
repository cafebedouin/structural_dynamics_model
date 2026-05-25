% ============================================================================
% CONSTRAINT STORY: epistemic_irreducibility_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_irreducibility_mountain, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_irreducibility_mountain
 *   human_readable: Epistemic Irreducibility in Computational Learning from Finite Data
 *   domain: philosophy_of_science/computational_learning_theory/epistemology
 *
 * SUMMARY:
 *   Epistemic irreducibility in computational learning from finite data is a
 *   formal constraint derived from information theory and computational
 *   learning theory. Any system that infers from a finite training corpus to
 *   novel cases faces an irreducible error rate bounded below by the coverage
 *   gap: facts appearing zero times in training cannot be learned, and facts
 *   appearing exactly once (singletons) are learned with high uncertainty.
 *   This constraint is not a coordination problem, an institutional
 *   arrangement, or a policy choice — it is a mathematical property of
 *   induction. The singleton rate in a training corpus (the fraction of facts
 *   appearing exactly once) provides a lower bound on the system's epistemic
 *   uncertainty when generalizing. PAC learning theory formalizes this: for
 *   any hypothesis class, the sample complexity required to achieve a target
 *   error rate grows with the VC dimension, and no finite sample eliminates
 *   error on the full distribution. The constraint applies universally across
 *   all learning paradigms: supervised learning, unsupervised learning,
 *   reinforcement learning, and human cognition. It is scale-invariant: a
 *   child learning language from parental input, a neural network trained on
 *   internet text, and a scientific community inferring laws from
 *   experimental data all face the same irreducibility. The constraint
 *   emerges naturally from the structure of induction and exhibits maximum
 *   accessibility collapse (0.92) — all agents, regardless of resources or
 *   perspective, encounter the same epistemic floor. Resistance is minimal
 *   (0.08) — no known technique circumvents the constraint, though active
 *   learning and uncertainty quantification can make the error rate
 *   transparent.
 *
 * KEY AGENTS:
 *   - Deployed System: Powerless/trapped — cannot exit the constraint; error rate is determined by training coverage
 *   - Research Institution: Institutional/arbitrage — can collect more data to reduce error rate but cannot eliminate the floor
 *   - Analytical Observer: Analytical/analytical — sees the constraint as a formal consequence of learning theory
 *   - Open Science Collective: Organized/mobile — can pool data globally but cannot remove the epistemic gap
 *   - Domain Practitioner: Moderate/constrained — faces the constraint in deployment; recognizes irreducibility despite resource limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_irreducibility_mountain, 0.08).
domain_priors:suppression_score(epistemic_irreducibility_mountain, 0.02).
domain_priors:theater_ratio(epistemic_irreducibility_mountain, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, extractiveness, 0.08).
narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_irreducibility_mountain, mountain).
narrative_ontology:human_readable(epistemic_irreducibility_mountain, "Epistemic Irreducibility in Computational Learning from Finite Data").
narrative_ontology:topic_domain(epistemic_irreducibility_mountain, "philosophy_of_science/computational_learning_theory/epistemology").

domain_priors:emerges_naturally(epistemic_irreducibility_mountain).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPLOYED SYSTEM (MOUNTAIN) — A learning system with finite training data faces an irreducible epistemic gap when encountering novel cases. No amount of computational power or architectural sophistication can eliminate errors on facts not represented in training. The system cannot exit this constraint — it is bound by the information-theoretic limits of induction from finite samples.
constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH INSTITUTION (MOUNTAIN) — Organizations deploying learning systems recognize the irreducibility: no training regime eliminates the singleton problem. Institutions with resources can collect more data and reduce error rates, but the structural constraint remains — the error floor is set by what the training corpus does not contain. Even with arbitrage-level resources, the mountain persists.
constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical position, this constraint is a formal consequence of computational learning theory: PAC learning bounds, VC dimension, and sample complexity theorems all formalize the same irreducibility. The error rate on novel cases is bounded below by the coverage gap in training data. This is not a contingent institutional arrangement or a coordination problem — it is a mathematical property of induction from finite samples.
constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: OPEN SCIENCE COLLECTIVE (MOUNTAIN) — Organized efforts to pool training data (open datasets, federated learning, collaborative benchmarks) can reduce the error rate by increasing coverage, but they cannot eliminate the constraint. Even with generational timescales and global data sharing, novel cases will always exist beyond the training distribution. The collective sees the mountain clearly: more data shifts the error rate down but does not remove the floor.
constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMAIN PRACTITIONER (MOUNTAIN) — Practitioners deploying learning systems in specific domains (medical diagnosis, legal reasoning, scientific discovery) face the constraint directly: the system will fail on cases not represented in training, and no amount of fine-tuning eliminates this. The practitioner is constrained by resource limits but recognizes the irreducibility — even unlimited resources would not remove the epistemic gap, only narrow it.
constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_irreducibility_mountain_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_irreducibility_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, ExtMetricName, E),
    domain_priors:suppression_score(epistemic_irreducibility_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(epistemic_irreducibility_mountain),
    narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(epistemic_irreducibility_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(epistemic_irreducibility_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint does not extract from agents in the sense of asymmetric resource transfer — it is a structural property of induction that applies uniformly. The minimal extractiveness reflects the opportunity cost of deploying systems that will fail on novel cases, but this is not extraction in the DR sense (one agent benefiting at another's expense). It is a shared epistemic cost. Suppression (0.02): Minimal. The constraint does not suppress alternatives through coercion or enforcement. Agents are free to collect more data, use different architectures, or deploy uncertainty quantification. The constraint is permissive — it sets a floor but does not prevent agents from approaching it. Theater ratio (0.05): Very low. There is minimal performative activity around this constraint. Researchers and practitioners acknowledge the epistemic gap openly; the constraint is not maintained through ritual or concealment. Some theater exists in claims of 'human-level performance' that ignore the singleton problem, but this is a minor component. Accessibility collapse (0.92): Very high. The constraint is accessible to all agents with basic training in learning theory or statistics. The formal results (PAC bounds, VC dimension, sample complexity) are well-established and widely taught. Resistance (0.08): Very low. No known technique circumvents the constraint. Active learning can reduce the sample complexity for a given error rate, and Bayesian methods can quantify uncertainty, but neither eliminates the error floor set by training coverage. The constraint is robust across all learning paradigms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify it as mountain. This is the expected signature of a genuine natural law: invariance across observation contexts. The deployed system sees an inescapable epistemic floor. The research institution sees an irreducible constraint even with maximum resources. The analytical observer sees a formal mathematical property. The open science collective sees a limit that data sharing cannot eliminate. The domain practitioner sees a constraint that persists despite fine-tuning. The uniformity of classification across power levels, time horizons, exit options, and spatial scopes confirms that this is not a contingent institutional arrangement being naturalized — it is a structural property of induction from finite samples. The constraint passes the false summit test: there is no plausible institutional reform, technological innovation, or coordination mechanism that would remove the epistemic gap. More data reduces the error rate but does not eliminate the floor set by coverage. The mountain classification is robust.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims in the structural sense. All agents face the same epistemic floor. Directionality values are derived from the canonical fallback for each power atom, but the effective extraction chi is uniformly low across all perspectives because base extractiveness is 0.08. The constraint does not create asymmetric costs — it imposes a shared epistemic limit. The deployed system (powerless/trapped) has the highest canonical d (1.00) but still experiences low chi because epsilon is so low. The research institution (institutional/arbitrage) has the lowest canonical d (0.00) but also experiences low chi. The perspectival gap is minimal — all agents classify this as mountain because the structural data (low epsilon, low suppression, high accessibility collapse, low resistance, emerges naturally) uniformly satisfies the mountain gates.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints are coordination problems or extraction mechanisms. Some constraints are genuine natural laws — formal properties of mathematics, logic, or information theory that apply universally and cannot be circumvented by institutional reform or resource allocation. The epistemic irreducibility of learning from finite data is such a constraint. It is not a snare (no victim group bears asymmetric costs), not a rope (no coordination function), not a tangled rope (no hybrid extraction-coordination), not a scaffold (no sunset clause), and not a piton (no theatrical maintenance). It is a mountain: an unchangeable structural property that emerges naturally from the mathematics of induction. The constraint's low extractiveness (0.08) and low suppression (0.02) reflect that it does not create asymmetric costs or suppress alternatives — it imposes a shared epistemic limit. The high accessibility collapse (0.92) and low resistance (0.08) reflect that the constraint is transparent and robust. The classification as mountain from all perspectives confirms that this is not a false summit — it is a genuine irreducibility that no observer position can dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_irreducibility_mountain, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_irreducibility_mountain, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a foundational epistemic limit that underlies many domain-specific learning constraints. It does not decompose into multiple stories with different epsilon values because the irreducibility applies uniformly across all observables: singleton rate, IIV misclassification rate, generative error rate, and PAC bounds all formalize the same structural property. The constraint is epsilon-invariant — changing the measurement methodology does not change the underlying irreducibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
