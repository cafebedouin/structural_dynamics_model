% ============================================================================
% CONSTRAINT STORY: abstraction_leakage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abstraction_leakage, []).

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
 *   constraint_id: abstraction_leakage
 *   human_readable: The Law of Leaky Abstractions
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The 'Law of Leaky Abstractions' states that all non-trivial abstractions,
 *   to some degree, fail to hide the details of their underlying
 *   implementation. This story models the socio-technical system built around
 *   this principle, not the principle as a pure natural law. The constraint
 *   is the practice of deploying and depending on abstractions where the
 *   provider benefits from the simplification, while the user bears the
 *   unpredictable cost of its failures (leaks). This creates a structural
 *   conflict of interest, where complexity is not eliminated but merely
 *   shifted.
 *
 * KEY AGENTS:
 *   - Abstraction Providers (Cloud vendors, library authors): Primary beneficiary (institutional/arbitrage) - Gain revenue and market share by simplifying complexity.
 *   - Downstream Developers (Application developers, engineers): Primary victim (moderate/constrained) - Benefit from initial speed but pay the 'tax' of debugging leaks.
 *   - End Users: Secondary victim (powerless/trapped) - Experience the bugs, downtime, and security flaws resulting from unmanaged leaks.
 *   - Legacy System Maintainers: Trapped actors (organized/trapped) - Forced to work with inertial, known-to-be-leaky systems.
 *   - Analytical Observer: Sees the pattern as a fundamental law (analytical/analytical), risking the naturalization of a designed system's flaws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abstraction_leakage, 0.32).
domain_priors:suppression_score(abstraction_leakage, 0.5).
domain_priors:theater_ratio(abstraction_leakage, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abstraction_leakage, extractiveness, 0.32).
narrative_ontology:constraint_metric(abstraction_leakage, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(abstraction_leakage, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abstraction_leakage, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(abstraction_leakage, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abstraction_leakage, tangled_rope).
narrative_ontology:human_readable(abstraction_leakage, "The Law of Leaky Abstractions").
narrative_ontology:topic_domain(abstraction_leakage, "technological/cognitive").

domain_priors:requires_active_enforcement(abstraction_leakage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abstraction_leakage, abstraction_providers).
narrative_ontology:constraint_victim(abstraction_leakage, downstream_developers).
narrative_ontology:constraint_victim(abstraction_leakage, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABSTRACTION PROVIDER (ROPE) — For the vendor or library author, the abstraction is a pure coordination good. It simplifies the problem space, enables a market, and creates a standard. Leaks are viewed as edge cases or documentation issues, not structural extraction. As a beneficiary with arbitrage, their effective extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(abstraction_leakage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM DEVELOPER (TANGLED ROPE) — The primary user experiences both the coordination benefits (faster development) and the extractive costs (hours lost debugging leaks). They are constrained by project requirements and switching costs. This is the canonical Tangled Rope experience. d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NOVICE DEVELOPER (TANGLED ROPE) — A novice is trapped by their lack of knowledge to see or fix the leak. The experience is one of pure, frustrating cost, feeling like a Snare. However, the base extractiveness is not high enough to meet the Snare threshold (χ≥0.66). The system classifies it as a high-extraction Tangled Rope. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.36.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: LEGACY SYSTEM MAINTAINER (PITON) — For those maintaining systems built on old, notoriously leaky abstractions (e.g., Win32, early DOM), the original coordination function is vestigial. The work is primarily ritualistic maintenance and patching workarounds. The high theater_ratio (0.75) captures this performative aspect, classifying the constraint as a Piton.
constraint_indexing:constraint_classification(abstraction_leakage, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: POLYFILL ADOPTER (SCAFFOLD) — A team adopting a temporary abstraction (like a polyfill or transpiler) sees it as a scaffold. It provides a coordination benefit now, with the explicit understanding it will be removed once the underlying platform matures (a sunset clause). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.29. Low effective extraction and a sunset clause define the Scaffold.
constraint_indexing:constraint_classification(abstraction_leakage, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — This observer naturalizes the constraint, viewing it as an unavoidable, fundamental law of information and complexity. They see any specific leak as an instance of a universal principle. The engine will flag this as a 'false summit': the base properties (ε=0.32, suppression=0.50, emerges_naturally=false) are inconsistent with a true Mountain, revealing the naturalization of a contingent socio-technical arrangement.
constraint_indexing:constraint_classification(abstraction_leakage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_leakage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abstraction_leakage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_leakage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(abstraction_leakage, TR),
    TR >= 0.70.

:- end_tests(abstraction_leakage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The value extracted is the developer time and operational cost externalized by the abstraction provider onto the user when a leak occurs. Suppression (0.50): Significant. Switching costs, network effects, and proprietary APIs make it difficult to move away from an established abstraction, even if it is leaky. Theater Ratio (0.75): High. Marketing, documentation, and 'hello world' tutorials present the abstraction as a perfect, simple solution, performing the function of hiding the messy reality. This ratio has increased over time as software stacks have become deeper and more complex.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The provider sees a Rope, a tool for coordination. The user sees a Tangled Rope, a mix of benefit and costly surprises. The maintainer of an old system sees a Piton, a ritual they are forced to perform. A team using a temporary fix sees a Scaffold. The analytical observer risks a category error, mistaking this man-made system for a Mountain (a law of nature). This story shows how a single, moderately extractive system can generate a wide range of classifications depending on the agent's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (abstraction_providers) have arbitrage exit options, leading to a low 'd' value and a Rope classification. Victims (downstream_developers) are constrained, leading to a high 'd' value and a Tangled Rope classification. The powerlessness of a novice further increases 'd', but not enough to cross the Snare threshold for this level of base extraction. The classification follows directly from the agent's structural power and exit options relative to the abstraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy where 'all software problems are just tangled ropes'. By decomposing the experience, we can distinguish the provider's Rope, the user's Tangled Rope, the maintainer's Piton, and the reformer's Scaffold. It also highlights the 'false summit' error, where a contingent, extractive system is mis-classified as a Mountain (a natural law), thereby justifying its costs as unavoidable. The framework correctly identifies the analytical perspective as a misreading of the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_vs_contingent,
    'Is the prevalence of leaky abstractions a fundamental limit of information theory, or a contingent result of market incentives that prioritize speed over robustness?',
    'Comparative analysis of systems built with different incentives (e.g., commercial software vs. aerospace-grade formal verification).',
    'If fundamental, the Mountain perspective is more accurate and ε is overstated. If contingent, the Tangled Rope classification is correct and represents a solvable market failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_vs_contingent, conceptual, 'Whether abstraction leakage is a fundamental limit or a contingent market failure.').

omega_variable(
    ai_abstraction_solvency,
    'Can next-generation AI-powered development tools create and manage abstractions that are effectively leak-proof from a human developer''s perspective?',
    'Empirical testing of AI-generated codebases for subtle, long-tail failures that require deep system knowledge.',
    'If yes, this constraint could become a Scaffold with a clear sunset. If no, the AI layer becomes just another leaky abstraction, potentially increasing the overall theater and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_abstraction_solvency, empirical, 'Whether AI tools can create truly non-leaky abstractions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abstraction_leakage, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abst_tr_t1970, abstraction_leakage, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(abst_tr_t1995, abstraction_leakage, theater_ratio, 1995, 0.55).
narrative_ontology:measurement(abst_tr_t2024, abstraction_leakage, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(abst_be_t1970, abstraction_leakage, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(abst_be_t1995, abstraction_leakage, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(abst_be_t2024, abstraction_leakage, base_extractiveness, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abstraction_leakage, information_standard).
narrative_ontology:affects_constraint(abstraction_leakage, technical_debt).
narrative_ontology:affects_constraint(abstraction_leakage, vendor_lock_in).

% DUAL FORMULATION NOTE:
% This story models the socio-technical system of leaky abstractions (ε=0.32). A separate constraint, 'information_theoretic_limits_on_compression', could be modeled as a pure Mountain (ε≈0.05) from which this constraint is downstream. The 'law' is a Mountain; the industry practice built upon it is a Tangled Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
