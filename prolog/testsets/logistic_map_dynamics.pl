% ============================================================================
% CONSTRAINT STORY: logistic_map_dynamics
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Robert May (1976) / Logistic Map / Population Biology
% ============================================================================

:- module(constraint_logistic_map, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
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
 * * constraint_id: logistic_map_dynamics
 * human_readable: The Logistic Map (Bifurcation and Chaos)
 * domain: mathematical/technological/biological
 * temporal_scope: 1976 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Population constraints)
 * * SUMMARY:
 * The Logistic Map ($x_{n+1} = r x_n (1 - x_n)$) is a polynomial mapping that 
 * demonstrates how complex, chaotic behavior can arise from very simple 
 * non-linear dynamical equations. It serves as a fundamental constraint on 
 * the predictability of systems with limited resources and feedback.
 * * KEY AGENTS:
 * - The Population Iterate ($x_n$): The powerless subject whose "density" is 
 * dictated by the fixed parameter $r$.
 * - The Population Biologist (Institutional): An agent who uses the map as a 
 * "Rope" for coordination to prevent ecosystem collapse.
 * - The Chaos Theorist (Analytical): The observer who maps the "Mountain" of 
 * the bifurcation diagram.
 * * NARRATIVE ARC:
 * For a population in a stable environment ($r < 3$), the map is a "Mountain" 
 * of natural law leading to equilibrium. In management, it is a "Rope" for 
 * predicting sustainable yields. However, as $r$ enters the chaotic regime 
 * ($r > 3.57$), the "Mountain" becomes a "Snare," extracting all predictive 
 * certainty and "strangling" the observer with sensitive dependence on 
 * initial conditions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(logistic_map_era, 1976, 2026).
narrative_ontology:constraint_claim(logistic_map_dynamics, mountain).

% Base extractiveness: 0.35
% Rationale: Moderate. Chaos "extracts" information (precision) and returns 
% entropy. To maintain functional control at high $r$, massive re-investment 
% in computational precision is required.
domain_priors:base_extractiveness(logistic_map_dynamics, 0.35).

% Suppression score: 0.4
% Rationale: It suppresses the visibility of simple linear growth models 
% (Malthusian), rendering them functionally fraudulent in resource-limited 
% environments.
domain_priors:suppression_score(logistic_map_dynamics, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(logistic_map_dynamics, extractiveness, 0.35).
narrative_ontology:constraint_metric(logistic_map_dynamics, suppression_requirement, 0.4).

% Enforcement: Emerges naturally from non-linear feedback and resource limits.
domain_priors:emerges_naturally(logistic_map_dynamics).

% Metrics
% Beneficiaries & Victims (Required for E > 0.3)
narrative_ontology:constraint_beneficiary(logistic_map_dynamics, nonlinear_analysts). % Gain insights into complexity.
narrative_ontology:constraint_beneficiary(logistic_map_dynamics, cryptography_designers). % PRNGs using chaos.
narrative_ontology:constraint_victim(logistic_map_dynamics, long_term_planners). % Certainty is extracted.
narrative_ontology:constraint_victim(logistic_map_dynamics, ecological_stability). % Small shocks cause flips.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POPULATION FRACTION (SUBJECT) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The iterate $x$ has no agency over the update rule.
   WHEN: immediate - True at every single step $n \to n+1$.
   WHERE: trapped - Bound within the interval [0, 1].
   SCOPE: local - Immediate neighborhood of the current value.
   
   WHY THIS CLASSIFICATION:
   For the state variable, the iterative law is an absolute, unyielding 
   Mountain of reality. It cannot "choose" to exceed the carrying capacity 
   or avoid the chaotic attractor once $r$ is set.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    logistic_map_dynamics,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ECOSYSTEM MANAGER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to influence $r$ (e.g., via harvest rates).
   WHEN: biographical - Planning for the multi-year health of a fishery.
   WHERE: mobile - Can adjust parameters to maintain a "Stable" regime ($r < 3$).
   SCOPE: national/regional - Managing a specific environment.
   
   WHY THIS CLASSIFICATION:
   For the manager, the map is a "Rope"—a functional coordination tool. 
   By keeping the system in the non-chaotic region, they coordinate the 
   standard of achievement (sustainable yield) through predictable math.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    logistic_map_dynamics,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CHAOTIC REGIME OBSERVER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Bound by the "Butterfly Effect" of the map.
   WHEN: biographical - Attempting to predict outcomes over a long period.
   WHERE: constrained - Knowing the law but having no path to certainty.
   SCOPE: global - The mathematical limit of all such iterative systems.
   
   WHY THIS CLASSIFICATION:
   In the chaotic region ($r = 3.9$), the map is a "Snare." It "strangles" 
   the ability to plan, extracting enormous resources for data collection 
   that ultimately fails to provide more than a few steps of foresight.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    logistic_map_dynamics,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(logistic_map_dynamics, E),
    E >= 0.3,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(logistic_map_tests).

test(bifurcation_fate_variance) :-
    % Subject -> Mountain
    constraint_indexing:constraint_classification(logistic_map_dynamics, Type1, context(powerless, immediate, trapped, local)),
    % Manager (Short-term/Stable) -> Rope
    constraint_indexing:constraint_classification(logistic_map_dynamics, Type2, context(institutional, biographical, mobile, regional)),
    Type1 = mountain,
    Type2 = rope.

test(chaos_extraction_penalty) :-
    % Long-term observer (Powerless/Constrained) in Chaos sees it as a Snare.
    constraint_indexing:constraint_classification(logistic_map_dynamics, snare, context(powerless, biographical, constrained, global)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(logistic_map_dynamics).

:- end_tests(logistic_map_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.35): 
 * I chose this because the Logistic Map at high $r$ represents a form of 
 * "Geometrical Coercion." The system "takes" your initial precision and 
 * shreds it, making it an extractive mathematical constraint.
 * * 2. SUPPRESSION SCORE (0.4): 
 * It effectively suppressed the "simple growth" narratives of the early 
 * 20th century by proving that complexity is the default, not the exception.
 * * 3. PERSPECTIVES:
 * Chose the "Iterate" (Passive Subject), "Manager" (Institutional User), 
 * and "Chaotic Observer" (Victim of Entropy) to show how the *same* law 
 * shifts type based on the parameter $r$.
 */

% OMEGA IDENTIFICATION
omega_variable(
    precision_decay_rate,
    "How many iterates $n$ does it take for double-precision floating point to lose all physical meaning at $r=4$?",
    resolution_mechanism("Numerical audit of divergence between $x_n$ and $x_n + \epsilon$ in 64-bit hardware."),
    impact("If $n < 50$: The 'Mountain' is a 'Snare' for all digital simulation. If $n > 500$: It is a Rope."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Malthusian Growth ($x_{n+1} = rx_n$)
 * Viability: Historically the standard for 100 years.
 * Suppression: Rejected by the Logistic Map because it ignores the 
 * "Mountain" of limited resources (the $1-x$ term).
 * * ALTERNATIVE 2: Continuous-time Models (Logistic Differential Equation)
 * Viability: Smooth, predictable, no chaos in 1D.
 * Suppression: Chaos theory shows that discrete-time events (like seasons 
 * or distinct generations) are the true "Rope" for complex biology.
 * * CONCLUSION:
 * The shift from continuous to discrete modeling turned a "Stable Rope" into 
 * a "Chaotic Mountain/Snare."
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [logistic_map_dynamics].
% Analyze: ?- constraint_indexing:multi_index_report(logistic_map_dynamics).

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
domain_priors:theater_ratio(logistic_map_dynamics, 0.0).
narrative_ontology:constraint_metric(logistic_map_dynamics, theater_ratio, 0.0).

% --- Analytical perspective classification (missing) ---
% chi = 0.35 * 1.15 (analytical) * 1.2 (global) = 0.483
% Classification: tangled_rope
constraint_indexing:constraint_classification(logistic_map_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
