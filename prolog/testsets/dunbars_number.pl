% ============================================================================
% CONSTRAINT STORY: dunbars_number
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21 - Regenerated to pass structural linter and apply v6.0 standards.
% ============================================================================

:- module(constraint_dunbars_number, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dunbars_number
 *   human_readable: Dunbar's Number (Cognitive Limit)
 *   domain: social/biological
 *
 * SUMMARY:
 *   This constraint models Dunbar's number as a biological cognitive limit on
 *   the number of stable social relationships an individual can maintain
 *   (approx. 150). It is based on a correlation between primate brain size
 *   (specifically, neocortex volume) and social group size. This story models
 *   the biological limit itself, which functions as a Mountain of human
 *   cognition.
 *
 * KEY AGENTS (by structural relationship):
 *   - Socially Overwhelmed Individual: Primary target (powerless/trapped) — experiences the limit as an unchangeable barrier.
 *   - Community Architect: Primary beneficiary (institutional/arbitrage) — uses knowledge of the limit as a coordination tool (Rope) to structure effective organizations.
 *   - Anthropologist: Analytical observer (analytical/analytical) — views the limit as a fixed feature of human biology (Mountain).
 *
 * DUAL FORMULATION NOTE:
 *   This constraint is one of 2 stories decomposed from the colloquial label "Dunbar's Number".
 *   Decomposed because ε differs across observables (ε-invariance principle).
 *   This story models the biological limit itself, which is non-extractive (ε=0.05, Mountain).
 *   A separate constraint, `platform_attention_economy`, models the *exploitation* of this limit
 *   by social media platforms, which is highly extractive (ε≈0.65, Snare).
 *   See: `constraint_platform_attention_economy.pl`
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a biological fact, the cognitive limit itself is not extractive.
% It is a background condition, not a mechanism of value transfer. ε is near zero.
domain_priors:base_extractiveness(dunbars_number, 0.05).

% Rationale: The limit "suppresses" the alternative of infinite social capacity,
% but this is a physical constraint, not a coercive one. Suppression is minimal.
domain_priors:suppression_score(dunbars_number, 0.02).
domain_priors:theater_ratio(dunbars_number, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dunbars_number, extractiveness, 0.05).
narrative_ontology:constraint_metric(dunbars_number, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(dunbars_number, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% accessibility_collapse: The alternative (infinite social capacity) is structurally inaccessible due to cognitive architecture.
narrative_ontology:constraint_metric(dunbars_number, accessibility_collapse, 0.95).
% resistance: Meaningful resistance to a cognitive limit is incoherent.
narrative_ontology:constraint_metric(dunbars_number, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
% The analytical view is that this is a fixed feature of human biology.
narrative_ontology:constraint_claim(dunbars_number, mountain).
narrative_ontology:human_readable(dunbars_number, "Dunbar's Number (Cognitive Limit)").

% --- Emergence flag (required for mountain constraints) ---
% This constraint arises from the structure of human cognition without design.
domain_priors:emerges_naturally(dunbars_number).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% The beneficiary declaration supports the Rope perspective. No victim is
% declared because the biological limit itself does not create victims;
% its exploitation by other constraints (e.g., platform design) does.
narrative_ontology:constraint_beneficiary(dunbars_number, community_architects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE SOCIALLY OVERWHELMED INDIVIDUAL (MOUNTAIN)
% From the perspective of an individual hitting their cognitive limit, it is an
% unchangeable and frustrating barrier. They are trapped by their own biology.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE COMMUNITY ARCHITECT (ROPE)
% For a founder structuring an organization, the number is a Rope. It is a
% vital coordination mechanism. Knowing that culture breaks down past ~150
% people allows them to proactively split teams to maintain trust and efficiency.
constraint_indexing:constraint_classification(dunbars_number, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANTHROPOLOGIST (MOUNTAIN)
% To the scientist, Dunbar's number is a Mountain. It is an unchangeable
% feature of the human "hardware." It is a fixed peak in the landscape of
% human nature. This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dunbars_number_tests).

test(perspectival_gap_mountain_vs_rope) :-
    % Verify the gap between the analytical/powerless view and the institutional view.
    constraint_indexing:constraint_classification(dunbars_number, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(dunbars_number, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunbars_number, rope, context(agent_power(institutional), _, _, _)).

test(mountain_threshold_adherence) :-
    % Verify that the base metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(dunbars_number, extractiveness, E),
    narrative_ontology:constraint_metric(dunbars_number, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(dunbars_number_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story was regenerated to resolve a MISSING_NL_PROFILE linter error.
 *   As a Mountain candidate, it required `emerges_naturally/1`, and metrics for
 *   `accessibility_collapse` and `resistance` to pass the natural law
 *   certification chain. These have been added with values reflecting a
 *   biological limit: it emerges naturally, alternatives are inaccessible, and
 *   resistance is incoherent.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between seeing the limit as a raw fact versus a useful tool.
 *   - The Anthropologist (analytical) and Overwhelmed User (powerless) both see a Mountain: an immutable feature of reality they cannot change.
 *   - The Community Architect (institutional) sees a Rope: they are not bound by the limit itself, but can use knowledge of it to coordinate human activity (e.g., structuring companies into sub-150-person divisions). Their arbitrage exit option allows them to design around the mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `community_architects` benefit from the *knowledge* of this constraint, using it to build more coherent organizations. This supports the Rope classification.
 *   - Victim: No victim is declared because the biological limit itself is neutral. Victims are created by a separate, downstream constraint (`platform_attention_economy`) that weaponizes this limit.
 *
 * MANDATROPHY ANALYSIS:
 *   By decomposing the colloquial "Dunbar's Number" into two distinct constraints (the biological Mountain and the platform Snare), this analysis prevents a critical error: misattributing the extraction performed by a technological system to a law of nature. It correctly locates the agency and extractiveness in the platform's design, not in human biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dunbars_number,
    'Is the cognitive limit truly fixed (Mountain), or can it be expanded by technology like AI-powered social assistants (making it a Scaffold)?',
    'Long-term studies of social cohesion in groups >150 using advanced cognitive offloading tools.',
    'If Mountain, human organization will always require subdivision. If Scaffold, technology could enable new, larger forms of high-trust social structures.',
    confidence_without_resolution(high) % High confidence it's a Mountain for now.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dunbars_number, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.05) is below the 0.46 threshold.
% As a biological constant, its metrics are assumed to be stable over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The knowledge of the number serves as an information standard for organizational design.
narrative_ontology:coordination_type(dunbars_number, information_standard).

% Network relationships (structural influence edges)
% This biological Mountain is a precondition for the platform Snare.
narrative_ontology:affects_constraint(dunbars_number, platform_attention_economy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options is accurate for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */