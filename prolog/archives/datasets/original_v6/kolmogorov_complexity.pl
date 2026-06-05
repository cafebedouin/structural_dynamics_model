% ============================================================================
% CONSTRAINT STORY: kolmogorov_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kolmogorov_complexity, []).

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
 *   constraint_id: kolmogorov_complexity
 *   human_readable: Kolmogorov Complexity Limit
 *   domain: computational_mathematics/information_theory
 *
 * SUMMARY:
 *   Kolmogorov Complexity (KC) is the length of the shortest possible program
 *   that can generate a given object (e.g., a string, image, or dataset) on a
 *   universal Turing machine. It represents an intrinsic property of
 *   information: no matter which compression algorithm is applied, no string
 *   can be compressed below its KC without loss of information. Unlike
 *   Shannon entropy (which measures statistical redundancy), KC measures the
 *   absolute minimum description length independent of the probability
 *   distribution. The constraint is uncomputable — no algorithm can determine
 *   the KC of an arbitrary string in finite time — yet mathematically proven
 *   to exist. This makes KC the canonical example of a natural law in
 *   information theory: it cannot be circumvented by choosing a better
 *   algorithm, changing the computational model, or applying more resources.
 *   Every observer (mathematician, engineer, industry) experiences it as a
 *   non-negotiable ceiling.
 *
 * KEY AGENTS:
 *   - The Mathematical Community: Analytical observers (civilizational/universal) — establish and prove the fundamental theorems defining KC as an invariant property of information
 *   - The Computer Science Community: Analytical observers (generational/global) — develop the theory and practical understanding of compression near the KC bound
 *   - Data Compression Engineers: Powerful actors (immediate/regional) — design algorithms and systems constrained by the KC bound; experience it as a hard resource ceiling
 *   - Technology Industry: Institutional beneficiaries (biographical/national) — capture value through algorithm efficiency within the KC bound; KC defines the asymptotic frontier of the market
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kolmogorov_complexity, 0.12).
domain_priors:suppression_score(kolmogorov_complexity, 0.03).
domain_priors:theater_ratio(kolmogorov_complexity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kolmogorov_complexity, extractiveness, 0.12).
narrative_ontology:constraint_metric(kolmogorov_complexity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(kolmogorov_complexity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kolmogorov_complexity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kolmogorov_complexity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kolmogorov_complexity, mountain).
narrative_ontology:human_readable(kolmogorov_complexity, "Kolmogorov Complexity Limit").
narrative_ontology:topic_domain(kolmogorov_complexity, "computational_mathematics/information_theory").

domain_priors:emerges_naturally(kolmogorov_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MATHEMATICIAN (MOUNTAIN) — Kolmogorov Complexity is a fundamental limit on description length. It is uncomputable in the Turing sense and represents an irreducible property of information itself. No observer can circumvent the limit by choosing a different measurement basis or compression scheme. The bound exists independent of computational substrate, encoding choice, or observer perspective. Zero degrees of freedom.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE COMPUTER SCIENTIST (MOUNTAIN) — Practical compression algorithms (zlib, LZMA, arithmetic coding) approach but cannot exceed the Kolmogorov bound. The constraint manifests as an asymptotic ceiling: no finite algorithm can reliably determine if a string is truly incompressible or merely lacks the right compression method. The undecidability is structural, not epistemic. The limit persists across all computational models and all time horizons.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: THE DATA COMPRESSION ENGINEER (MOUNTAIN) — Engineers designing compression systems for storage and transmission confront Kolmogorov Complexity as an absolute wall. Strings with high KC cannot be compressed further; the engineer's resource budget (storage, bandwidth, compute time) hits the fundamental limit. No institutional workaround or market solution changes the mathematical fact. The constraint is inescapable even for well-resourced actors.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE TECHNOLOGY INDUSTRY (MOUNTAIN) — Compression markets (cloud storage, streaming media, data centers) operate within the Kolmogorov bound as a hard constraint on achievable efficiency. The industry captures value through algorithm proximity to the bound, but cannot transcend it. The constraint defines the asymptotic frontier of the market, not an exploitable inefficiency. From a profit-maximization perspective, KC is a natural law limiting arbitrage.
constraint_indexing:constraint_classification(kolmogorov_complexity, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kolmogorov_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kolmogorov_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kolmogorov_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kolmogorov_complexity, ExtMetricName, E),
    domain_priors:suppression_score(kolmogorov_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kolmogorov_complexity),
    narrative_ontology:constraint_metric(kolmogorov_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kolmogorov_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kolmogorov_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Kolmogorov Complexity does not extract resources or value from any agent. It simply exists as a mathematical fact. The compressibility ceiling is a property of information itself, not a mechanism of coercion or advantage-taking. No institutional actor benefits disproportionately from KC's existence — all face the same limit. Suppression (0.03): Minimal. The constraint suppresses no alternatives because alternatives do not exist. There is no competing theory of description length that could replace KC. The mathematical proof is so robust that suppression is vacuous — alternatives are not suppressed; they are incoherent. Theater ratio (0.15): Low. While KC is sometimes invoked rhetorically in technology marketing ('quantum computing offers exponential speedup'), the technical content is minimal theater. The constraint's expression is direct and non-performative. The slight theater (0.15 vs 0.0) reflects popular misconceptions about quantum shortcuts around KC, but the professional and mathematical literature treats KC with clarity.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists. All observers — regardless of power level, time horizon, exit options, or spatial scope — experience and classify Kolmogorov Complexity identically as Mountain. This invariance across observables is the defining feature of natural law constraints. The constraint is not socially constructed, not amenable to institutional workaround, and not subject to perspectival reframing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain constraints in the classical sense. KC does not create beneficiaries and victims because it does not transfer resources or apply coercion to any agent. All agents (regardless of power, exit options, or position) face the same limit. The derivation chain produces d ≈ 0.5 (symmetric, no differential impact) for all agents, which maps to f(d) ≈ 0.65. However, this is a canonical baseline indicating 'not applicable' rather than a real structural asymmetry. Mountains are invariant across the directionality tuple.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY. Kolmogorov Complexity resolves the coordination-vs-extraction question decisively: it is neither. It is a mathematical fact about the structure of information that applies universally. The constraint cannot be decomposed into a hidden coordination mechanism (it has no beneficiaries to coordinate) nor into hidden extraction (it targets no specific agent). It simply is. The mandatrophy framework does not apply to natural law constraints, which by definition are invariant across all observation contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncomputability_vs_realizability,
    'Does the uncomputability of Kolmogorov Complexity imply that the bound is not a physical constraint on real systems, only a mathematical limit on theoretical description?',
    'Analysis of whether KC limits apply to physical information processing (e.g., quantum systems, biological entropy) or only to abstract Turing machines. Comparison of information-theoretic bounds (Shannon entropy, algorithmic information theory) with thermodynamic limits.',
    'If KC applies to physical systems: the constraint is truly universal. If KC is only about mathematical description: the constraint may have technological gaps (quantum shortcuts, biological encoding). Classification remains mountain either way, but the scope interpretation shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uncomputability_vs_realizability, conceptual, 'Whether KC is a physical law or a mathematical limit on description').

omega_variable(
    alternate_representation_basis,
    'Can alternative encoding schemes (e.g., non-string representations, quantum superposition, analog representation) evade the Kolmogorov bound for specific objects?',
    'Formal proof that KC is invariant across all Turing-complete representations; empirical testing of quantum and analog systems for information density exceeding KC predictions.',
    'If KC is invariant: classification confirmed as mountain across all observables. If representational evasion exists: KC becomes perspectival (different for different encodings), potentially Rope or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternate_representation_basis, empirical, 'Whether alternative representations can evade the Kolmogorov bound').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kolmogorov_complexity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kc_tr_t0, kolmogorov_complexity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kc_tr_t50, kolmogorov_complexity, theater_ratio, 50, 0.15).
narrative_ontology:measurement(kc_tr_t100, kolmogorov_complexity, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(kc_be_t0, kolmogorov_complexity, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(kc_be_t50, kolmogorov_complexity, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(kc_be_t100, kolmogorov_complexity, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kolmogorov_complexity, information_standard).
narrative_ontology:affects_constraint(kolmogorov_complexity, shannon_entropy_limit).
narrative_ontology:affects_constraint(kolmogorov_complexity, turing_halting_problem).
narrative_ontology:affects_constraint(kolmogorov_complexity, algorithmic_randomness).

% DUAL FORMULATION NOTE:
% Kolmogorov Complexity is upstream of several constraints in computational theory. Shannon entropy (which measures statistical redundancy) is a lower bound on expected compression but differs from KC (which measures absolute minimum). The Halting Problem (which is also uncomputable) and Algorithmic Randomness are structural kin to KC, sharing the property of undecidability. These form a constraint family in computational mathematics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
