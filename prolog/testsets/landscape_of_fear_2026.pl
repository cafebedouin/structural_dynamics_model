% ============================================================================
% CONSTRAINT STORY: landscape_of_fear_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_landscape_of_fear_2026, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: landscape_of_fear_2026
 *   human_readable: The Landscape of Fear
 *   domain: biological
 *
 * SUMMARY:
 *   The "landscape of fear" describes how the mere presence or perception of predators
 *   constrains prey behavior, affecting feeding patterns, reproductive rates, and
 *   survival far beyond direct predation. This concept shifted the
 *   biological paradigm from counting "kills" to mapping the psychological
 *   constraints predators impose on an environment. Following the 1995
 *   reintroduction of wolves to Yellowstone, scientists noticed elk numbers
 *   falling faster than predation rates alone could explain, leading to this
 *   framework.
 *
 * KEY AGENTS (by structural relationship):
 *   - Prey Populations (Elk, Song Sparrow): Primary target (powerless/trapped) — bears extraction through reduced fitness and forgone opportunities.
 *   - Apex Predators (Wolf, Hawk): Primary beneficiary (institutional/mobile) — benefits from easier prey management and ecosystem stability.
 *   - Ecosystem Stability: Secondary beneficiary — the overall system benefits from regulated grazing and trophic cascade effects.
 *   - Ecologists (Laundré, Zanette): Analytical observer — identifies the causal mechanisms of fear-based ecological shifts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Predators extract time, energy, and reproductive potential from prey
% through the psychological burden of vigilance and avoidance of prime habitat.
domain_priors:base_extractiveness(landscape_of_fear_2026, 0.40).
% Rationale: Prey will completely avoid high-quality foraging areas where fear
% is high, effectively suppressing their best options.
domain_priors:suppression_score(landscape_of_fear_2026, 0.70).
% Rationale: The system is functional, not performative.
domain_priors:theater_ratio(landscape_of_fear_2026, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, 0.40).
narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(landscape_of_fear_2026, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(landscape_of_fear_2026, tangled_rope).
narrative_ontology:human_readable(landscape_of_fear_2026, "The Landscape of Fear").
narrative_ontology:topic_domain(landscape_of_fear_2026, "biological").

% --- Binary flags ---
% The predator's constant presence and threat of violence is the enforcement mechanism.
domain_priors:requires_active_enforcement(landscape_of_fear_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
% The predator benefits directly; the ecosystem benefits from the resulting stability.
narrative_ontology:constraint_beneficiary(landscape_of_fear_2026, apex_predators).
narrative_ontology:constraint_beneficiary(landscape_of_fear_2026, ecosystem_stability).
%
% Who bears disproportionate cost?
% Prey populations suffer reduced reproduction and feeding opportunities.
narrative_ontology:constraint_victim(landscape_of_fear_2026, prey_populations).

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

% PERSPECTIVE 1: THE PREY (ELK/SPARROW) - SNARE
% For the prey, fear is a Snare. It is a coercive psychological pressure
% that strangles their ability to feed and reproduce, forcing them into
% low-quality habitats even when superior food is nearby. They are trapped
% in the ecosystem and bear the full cost.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE APEX PREDATOR (WOLF) - ROPE
% For the predator, fear is a Rope—a functional tool for coordinating
% prey behavior and ensuring the predator's own energy-efficient survival
% by making prey easier to manage. The predator is a beneficiary with high
% exit (mobility across the territory).
constraint_indexing:constraint_classification(landscape_of_fear_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ECOLOGIST (ANALYTICAL OBSERVER) - TANGLED ROPE
% The analyst sees the dual function. The constraint provides a genuine
% coordination benefit (ecosystem stability, preventing overgrazing) while
% simultaneously imposing a severe, asymmetric extraction on prey populations.
% This combination of coordination and extraction is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(landscape_of_fear_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_metrics_are_valid) :-
    narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, E),
    narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics (ε=0.40, suppression=0.70) reflect a system with significant
 *   coercion and extraction. The extraction is not just direct predation but
 *   the "tax" of vigilance and lost opportunity imposed on prey. The suppression
 *   score reflects how prey are forced to abandon optimal foraging grounds.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The prey experiences a coercive Snare that reduces its
 *   fitness. The predator experiences a functional Rope that makes its life
 *   easier and regulates its food source. The analyst sees both sides: a
 *   Tangled Rope that provides ecosystem-level coordination at the direct
 *   expense of a specific population.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `apex_predators` and `ecosystem_stability`. Predators gain
 *     an energy advantage. The ecosystem gains stability via regulated grazing.
 *   - Victims: `prey_populations`. They bear the cost through stress, reduced
 *     feeding, and lower reproductive success.
 *   This clear division drives the directionality calculation, leading to a
 *   negative chi for the predator (Rope) and a high positive chi for the prey (Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   The original ecological model (predators only matter when they kill) could
 *   be seen as a misclassified Mountain. This analysis correctly identifies the
 *   active, agent-driven nature of the constraint. By classifying it as a
 *   Tangled Rope, the framework acknowledges both the system-level benefit
 *   (coordination) and the targeted cost (extraction), preventing the over-
 *   simplification of labeling it purely beneficial (Rope) or purely harmful (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_fear_habituation,
    'Do prey eventually habituate to predator signs, reducing the fear-constraint over generations?',
    'Long-term (multi-generational) study of prey response to chronic predator presence.',
    'If habituation occurs, the Snare could degrade towards a Rope. If not, it remains a persistent Snare.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fear_habituation, empirical, 'Whether prey habituate to predator presence over generations, reducing the constraint''s effect.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(landscape_of_fear_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint is a form of biological resource allocation.
narrative_ontology:coordination_type(landscape_of_fear_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the predator-prey dynamic.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */