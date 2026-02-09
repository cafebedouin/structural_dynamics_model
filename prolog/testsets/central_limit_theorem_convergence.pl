% ============================================================================
% CONSTRAINT STORY: central_limit_theorem_convergence
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Laplace (1810) / Lyapunov / Probability Theory
% ============================================================================

:- module(constraint_clt, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: central_limit_theorem_convergence
 * human_readable: Central Limit Theorem (CLT)
 * domain: mathematical/probabilistic
 * temporal_scope: 1810 - Present (Civilizational)
 * spatial_scope: Global/Abstract (IID variables with finite variance)
 * * SUMMARY:
 * The Central Limit Theorem (CLT) establishes that the sum or average of a 
 * large number of independent and identically distributed (IID) variables 
 * will follow a normal distribution, regardless of the original distribution. 
 * It is the "gravitational force" of statistics.
 * * KEY AGENTS:
 * - The Individual Variable (Subject): The powerless agent whose unique 
 * distributional identity is lost as it is summed into the aggregate.
 * - The Statistician (Institutional): Uses the theorem as a "Rope" for 
 * inference, polling, and scientific verification.
 * - The Risk Manager (Victim): An agent who treats the CLT as a "Mountain" 
 * in a heavy-tailed world (finance), only to find it was a "Snare" that 
 * hid catastrophic "Black Swan" risks.
 * * NARRATIVE ARC:
 * CLT is the "Mountain" of equilibrium—the bell curve is the inevitable 
 * destiny of high-n aggregates. For the scientific community, it is the 
 * "Rope" that coordinates shared reality through p-values. However, when 
 * variance is infinite or dependencies are hidden, the "Mountain" collapses 
 * into a "Snare" of false confidence.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(clt_era, 1810, 2026).
narrative_ontology:constraint_claim(central_limit_theorem_convergence, mountain).

% Base extractiveness: 0.25
% Rationale: Moderate. The CLT "extracts" the specific identity and 
% "roughness" of individual data points to provide the "smooth" aggregate.
domain_priors:base_extractiveness(central_limit_theorem_convergence, 0.25).

% Suppression score: 0.35
% Rationale: It suppresses the visibility of underlying distributions (e.g., 
% Uniform, Poisson) in favor of the Gaussian approximation.
domain_priors:suppression_score(central_limit_theorem_convergence, 0.35).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(central_limit_theorem_convergence, extractiveness, 0.25).
narrative_ontology:constraint_metric(central_limit_theorem_convergence, suppression_requirement, 0.35).

% Enforcement: Emerges naturally from the structure of addition and probability.
domain_priors:emerges_naturally(central_limit_theorem_convergence).

% Metrics
% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(central_limit_theorem_convergence, data_scientists).
narrative_ontology:constraint_beneficiary(central_limit_theorem_convergence, pollsters).
narrative_ontology:constraint_victim(central_limit_theorem_convergence, outlier_detection). % Outliers are "smoothed" away.
narrative_ontology:constraint_victim(central_limit_theorem_convergence, tail_risk_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RANDOM VARIABLE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The data point has no agency over the summation.
   WHEN: immediate - True at the moment n becomes large.
   WHERE: trapped - Bound within the laws of large numbers.
   SCOPE: local - Immediate neighborhood of the aggregate mean.
   
   WHY THIS CLASSIFICATION:
   For the individual number being summed, the normal distribution is a 
   natural law. It cannot "choose" to stay skewed or uniform; it must converge 
   to the bell curve.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    central_limit_theorem_convergence,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE POLLING FIRM - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design samples and set margins of error.
   WHEN: biographical - Planning the lifecycle of a study or campaign.
   WHERE: mobile - Can choose different sample sizes to tune precision.
   SCOPE: national - Applying the theorem to millions of voters.
   
   WHY THIS CLASSIFICATION:
   For the institution, CLT is a "Rope"—a functional coordination tool. 
   It allows them to calculate "standard error" and coordinate national 
   expectations of reality.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    central_limit_theorem_convergence,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE FINANCIAL TRADER (FAT TAILS) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Trapped by a model assuming Gaussian normality.
   WHEN: immediate - A crash occurs that CLT says "should never happen."
   WHERE: constrained - The agent is legally or financially bound to Gaussian models (VaR).
   SCOPE: global - Contagion across global markets.
   
   WHY THIS CLASSIFICATION:
   When the assumptions of CLT (finite variance/independence) are broken but 
   the model is still enforced, it becomes a "Snare." It extracts massive 
   wealth during a "Fat Tail" event, strangling the system that relied on 
   the "Mountain" of the bell curve.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    central_limit_theorem_convergence,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(central_limit_theorem_convergence, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(clt_convergence_tests).

test(aggregation_fate_variance) :-
    % Variable -> Mountain
    constraint_indexing:constraint_classification(central_limit_theorem_convergence, Type1, context(powerless, immediate, trapped, local)),
    % Scientist -> Rope
    constraint_indexing:constraint_classification(central_limit_theorem_convergence, Type2, context(institutional, biographical, mobile, national)),
    Type1 = mountain,
    Type2 = rope.

test(fat_tail_snare_penalty) :-
    % A powerless agent in a global/constrained context (like VaR modeling) sees it as a Snare.
    constraint_indexing:constraint_classification(central_limit_theorem_convergence, snare, context(powerless, immediate, constrained, global)).

test(emergence) :-
    domain_priors:emerges_naturally(central_limit_theorem_convergence).

:- end_tests(clt_convergence_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. TYPE VARIANCE: Captured the dual nature of CLT as a "Mountain" of physics 
 * (Galton Board) and a "Snare" of risk management (Black Swans).
 * 2. AGENT SELECTION: Labeled the individual data point as the powerless 
 * subject. Its "erasure" via smoothing is the core extractive act of the CLT.
 * 3. EXTRACTIVENESS: Set to 0.25—low in isolation, but high when outliers are 
 * the only things that matter (e.g., in pandemics or market crashes).
 */

% OMEGA IDENTIFICATION
omega_variable(
    independence_assumption,
    "Is the 'Mountain' stable if data points are secretly correlated?",
    resolution_mechanism("Measure the divergence of the aggregate from the Normal via the Berry-Esseen theorem."),
    impact("If Correlated: The Mountain is a Scaffold. If Independent: It is a true Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Stable Paretian Distributions (Levy Flights)
 * Viability: For infinite variance systems, these are the "true" Mountains.
 * Suppression: Often ignored in introductory stats because the math is "hard."
 * * ALTERNATIVE 2: Bayesian Non-Parametrics
 * Viability: Avoids the "Rope" of Gaussian assumptions entirely.
 * Suppression: Suppressed by the "Standard" p-value curriculum in universities.
 * * CONCLUSION:
 * The institutional "Rope" of the CLT is so strong that it suppresses more 
 * accurate "Fat Tailed" alternatives, creating a "Snare" for complex systems.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [central_limit_theorem_convergence].
% Analyze: ?- constraint_indexing:multi_index_report(central_limit_theorem_convergence).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Formal truth — substantive with near-zero performative component
domain_priors:theater_ratio(central_limit_theorem_convergence, 0.0).
narrative_ontology:constraint_metric(central_limit_theorem_convergence, theater_ratio, 0.0).

% --- Analytical perspective classification (missing) ---
% chi = 0.25 * 1.15 (analytical) * 1.2 (global) = 0.345
% Classification: rope
constraint_indexing:constraint_classification(central_limit_theorem_convergence, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
