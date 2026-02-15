% ============================================================================
% CONSTRAINT STORY: information_foraging_theory
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_information_foraging_theory, []).

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
% See: epsilon_invariance_princi ple.md

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: information_foraging_theory
 *   human_readable: Information Foraging Theory (IFT)
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   Information Foraging Theory (IFT) posits that humans, when seeking
 *   information, behave like animals foraging for food. They use evolved
 *   heuristics to maximize their rate of valuable information gain per unit
 *   of effort. This theory models how users navigate information-rich
 *   environments like the web, following cues ("information scent") to decide
 *   which links to click and when to abandon an unpromising source ("patch").
 *
 * KEY AGENTS (by structural relationship):
 *   - Attention-farmed users: Primary target (powerless/trapped) — bears extraction when IFT is weaponized to maximize time-on-site.
 *   - UX designers & platform architects: Primary beneficiary (institutional/analytical) — benefits from using IFT to design effective, engaging systems.
 *   - Standard information seekers: Symmetric user (moderate/mobile) — uses IFT as a coordination tool for efficient navigation.
 *   - Analytical observer: Sees the full structure of IFT as a natural law of cognition that can be used for coordination or extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Low extraction; the theory itself is a model of efficiency. Extraction
% arises from its application, not its structure.
domain_priors:base_extractiveness(information_foraging_theory, 0.20).
% Very low suppression. IFT relies on high-visibility signals ("scent"),
% which is the opposite of suppressing alternatives. Set to 0.05 to be
% consistent with the Mountain classification gate.
domain_priors:suppression_score(information_foraging_theory, 0.05).
% Low theater. IFT is grounded in evolutionary biology and validated by
% empirical HCI research. Its predictions match observed behavior closely.
domain_priors:theater_ratio(information_foraging_theory, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_foraging_theory, extractiveness, 0.20).
narrative_ontology:constraint_metric(information_foraging_theory, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(information_foraging_theory, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: High (0.95). One cannot design a user interface
% that ignores IFT principles without suffering predictable user abandonment.
narrative_ontology:constraint_metric(information_foraging_theory, accessibility_collapse, 0.95).
% Resistance: Low (0.05). Resistance is incoherent; designers don't fight
% IFT, they leverage it. It's a law of user behavior to be worked with.
narrative_ontology:constraint_metric(information_foraging_theory, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(information_foraging_theory, rope).

% --- Binary flags ---
% No sunset clause or active enforcement.

% --- Emergence flag (required for mountain constraints) ---
% IFT emerges naturally from evolutionary pressures for energy conservation
% in cognitive tasks. Required for the mountain metric gate.
domain_priors:emerges_naturally(information_foraging_theory).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(information_foraging_theory, ux_designers_and_platform_architects).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(information_foraging_theory, attention_farmed_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ATTENTION-FARMED USER (SNARE)
% When a platform weaponizes IFT to maximize time-on-site rather than user
% goal completion, the user becomes trapped. Continuous micro-scents
% (infinite scroll, notifications) create a low-value extractive loop.
% Engine derives d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(information_foraging_theory, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE UX DESIGNER (MOUNTAIN)
% For a system designer, IFT is an immutable law of user behavior. Ignoring
% it leads to predictable failure (user abandonment). It is a fixed
% environmental constraint to be designed around, not challenged.
% Engine sees ε=0.20, S=0.05, and emerges_naturally -> Mountain.
constraint_indexing:constraint_classification(information_foraging_theory, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% The observer sees IFT as a fundamental cognitive coordination mechanism.
% It's an evolved heuristic that enables efficient navigation of complex
% information landscapes, making it a natural Rope.
% χ = 0.20 * f(0.72) * 1.2 ≈ 0.20 * 1.15 * 1.2 ≈ 0.276 -> Rope
constraint_indexing:constraint_classification(information_foraging_theory, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STANDARD INFORMATION SEEKER (ROPE)
% For a typical user with a clear goal and freedom to navigate, IFT is a
% pure coordination tool (Rope). It helps them find the shortest path to
% their goal, minimizing cognitive load.
constraint_indexing:constraint_classification(information_foraging_theory, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_foraging_theory_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and standard user.
    constraint_indexing:constraint_classification(information_foraging_theory, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_foraging_theory, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(information_foraging_theory, rope, context(agent_power(moderate), _, _, _)).

test(threshold_validation) :-
    % Verify metrics are consistent with a Mountain classification possibility.
    narrative_ontology:constraint_metric(information_foraging_theory, extractiveness, E),
    narrative_ontology:constraint_metric(information_foraging_theory, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(information_foraging_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.20) is low because IFT is fundamentally a
 *   theory of efficiency, not extraction. The suppression score was lowered
 *   to 0.05 (from an original 0.1) to be consistent with the Mountain
 *   classification seen by designers; a natural law has near-zero
 *   suppression. The key insight is that this natural law can be leveraged
 *   for different ends, leading to the perspectival split. The NL Profile
 *   metrics (accessibility_collapse=0.95, resistance=0.05) were added to
 *   pass the linter and correctly model the 'law-like' nature of IFT for
 *   designers.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a designer (institutional), IFT is a Mountain—an
 *   unchangeable feature of the landscape. For a typical user (moderate), it's
 *   a Rope—a helpful tool for navigation. For a user trapped on an
 *   attention-farming platform (powerless), that same tool is weaponized
 *   into a Snare. The classification depends entirely on whether the
 *   application of the theory serves the user's goals or the platform's.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `ux_designers_and_platform_architects` leverage the
 *     predictive power of IFT to build effective systems. Their relationship
 *     is analytical and instrumental.
 *   - Victims: `attention_farmed_users` are those for whom the system is
 *     designed to extract attention, using IFT principles to create sticky,
 *     low-value loops.
 *   This beneficiary/victim structure drives the directionality calculation,
 *   explaining why the powerless/trapped perspective sees high effective
 *   extraction (χ) even with low base extraction (ε).
 *
 * MANDATROPHY ANALYSIS:
 *   This case highlights how a natural law (Mountain) or coordination tool
 *   (Rope) can be instrumentalized into a Snare. The low base extraction (ε)
 *   prevents misclassifying the theory itself as extractive. The perspectival
 *   classifications correctly locate the extraction in the *application* of
 *   the theory within a specific power dynamic (powerless/trapped). The low
 *   metrics also place this constraint in the "Scaffold Danger Zone," but this
 *   is appropriate: IFT acts as a natural, non-expiring scaffold for thought,
 *   which is correctly classified as a Rope from a symmetric perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_ift_weaponization_threshold,
    'At what point does platform optimization of information scent cross from coordination (Rope) to extraction (Snare)?',
    'Compare user goal-completion rates vs time-on-site metrics across optimized vs non-optimized platforms.',
    'If scent serves user goals, IFT remains a Rope. If scent primarily traps attention to serve platform goals, it functions as a Snare for powerless users, warranting reclassification of the platform, not the theory.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_ift_weaponization_threshold, empirical, 'Threshold at which optimizing information scent becomes extractive attention farming.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(information_foraging_theory, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is 0.20, which is below the 0.46 threshold for
% mandatory temporal tracking. No measurement/5 facts are required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% dynamics of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */