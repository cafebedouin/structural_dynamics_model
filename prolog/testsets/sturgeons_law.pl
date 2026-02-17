% ============================================================================
% CONSTRAINT STORY: sturgeons_law
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sturgeons_law, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sturgeons_law
 *   human_readable: Sturgeon's Law ("90% of everything is crap")
 *   domain: sociological/artistic
 *
 * SUMMARY:
 *   Formulated by sci-fi writer Theodore Sturgeon, the law states that "ninety
 *   percent of everything is crud." It functions as an observation about the
 *   statistical distribution of quality in any creative or productive field.
 *   It posits that a low signal-to-noise ratio is a fundamental, emergent
 *   property of open systems, not a flaw unique to any specific domain. This
 *   constraint story models the law itself as a natural law (Mountain), while
 *   acknowledging that its *exploitation* by other actors (e.g., platform
 *   algorithms) constitutes a separate, downstream constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Consumer (powerless/trapped): Experiences the law as an immutable
 *     environmental fact, requiring effort to navigate a sea of mediocrity.
 *   - The Creator (moderate/mobile): Uses the law as a rhetorical defense
 *     against critics who dismiss an entire genre based on its worst examples.
 *   - The Platform (institutional/arbitrage): Observes the law as a statistical
 *     reality that informs the design of curation and filtering systems.
 *   - The Analytical Observer: Sees the law as a universal statistical baseline.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% The law itself is an observation; it does not extract value.
domain_priors:base_extractiveness(sturgeons_law, 0.10).
% The law does not actively suppress alternatives; it is a description of their
% statistical rarity. This is a key feature of a Mountain.
domain_priors:suppression_score(sturgeons_law, 0.05).
% The law is a direct observation with very little performative aspect.
domain_priors:theater_ratio(sturgeons_law, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sturgeons_law, extractiveness, 0.10).
narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sturgeons_law, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% The law describes a state where high-quality alternatives are rare and
% structurally difficult to produce, collapsing accessibility.
narrative_ontology:constraint_metric(sturgeons_law, accessibility_collapse, 0.95).
% There is no meaningful resistance to a statistical distribution.
narrative_ontology:constraint_metric(sturgeons_law, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sturgeons_law, mountain).
narrative_ontology:human_readable(sturgeons_law, "Sturgeon's Law (\"90% of everything is crap\")").

% --- Binary flags ---
% No active enforcement is needed for a statistical reality.

% --- Emergence flag (required for mountain constraints) ---
% The 90/10 distribution emerges naturally from the statistics of creative
% and productive processes without centralized design or enforcement.
domain_priors:emerges_naturally(sturgeons_law).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), this constraint does not
% have structurally defined beneficiaries or victims. Its effects are universal
% and symmetric, though different agents may react to it differently.

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

% PERSPECTIVE 1: THE CONSUMER (MOUNTAIN)
% To the consumer, the 90% is an immovable fact of the environment. They must
% climb through stacks of "crud" to find something of value. They have no
% power over the distribution; they can only endure it.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PLATFORM GATEKEEPER (MOUNTAIN)
% For an institutional actor like a publisher or streaming service, the law is
% also a Mountain. It is a fundamental environmental condition that their
% business model must account for (e.g., through curation, filtering, and
% recommendation algorithms). The law itself is immutable.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a detached, civilizational perspective, the law is a classic Mountain:
% an apparently fixed, unchangeable feature of information ecosystems.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sturgeons_law_tests).

test(perspectival_invariance) :-
    % Verify that as a natural law, the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(sturgeons_law, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metric_adherence) :-
    % Verify the constraint's metrics fall within the thresholds for Mountain classification.
    narrative_ontology:constraint_metric(sturgeons_law, extractiveness, E),
    narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, S),
    config:param(mountain_extractiveness_max, EMax),
    config:param(mountain_suppression_ceiling, SMax),
    E =< EMax,
    S =< SMax.

:- end_tests(sturgeons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file was inconsistent, declaring `emerges_naturally` while also
 *   showing perspectival variance (Rope, Scaffold, Snare) and having a
 *   suppression score (0.4) too high for a natural law. This regeneration
 *   resolves the contradiction by classifying the law *itself* as a Mountain.
 *   The metrics have been adjusted to be consistent with this classification
 *   (ε=0.10, S=0.05). The required Natural Law profile metrics
 *   (`accessibility_collapse`, `resistance`) have been added to pass the
 *   structural linter and the engine's certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain, the classification is
 *   invariant across all indices. The different reactions of agents (a creator
 *   using it as a defense, a platform using it to justify filtering) are
 *   responses *to* the Mountain, not changes in the nature of the constraint
 *   itself.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint has no defined beneficiaries or victims. Its
 *   effects are symmetric and universal, like gravity. Directionality is not
 *   a relevant factor.
 *
 * MANDATROPHY ANALYSIS:
 *   This reframing prevents a category error. The original file conflated the
 *   law (a statistical observation, Mountain) with the systems built to
 *   exploit it (e.g., platform algorithms that might be Snares or Tangled
 *   Ropes). By classifying the law itself as a Mountain and linking it to a
 *   downstream constraint via `affects_constraint`, the analysis becomes more
 *   precise and avoids mislabeling a natural feature of a system as a form of
 *   designed extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_sturgeons_law,
    'Is the 90/10 ratio a fundamental limit of information theory and human creativity (Mountain), or is it an artifact of specific economic systems (e.g., capitalism) that prioritize quantity over quality (Tangled Rope)?',
    'Comparative analysis of quality distributions in non-market or highly curated creative ecosystems (e.g., monastic traditions, state-sponsored art).',
    'If Mountain, efforts to "fix" the ratio are futile. If Tangled Rope, the ratio is a policy choice that can be altered.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_sturgeons_law, empirical, 'Is the 90/10 quality distribution a natural law or a product of specific economic incentives?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sturgeons_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a Mountain with low extraction (ε=0.10), temporal measurements for drift
% detection are not required. The constraint is considered stable over its
% interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% The exploitation of Sturgeon's Law by platform gatekeepers is a separate
% constraint, structurally influenced by the statistical reality of the law.
%
% DUAL FORMULATION NOTE:
% This constraint (the statistical law) is the upstream component. A separate
% story should model the downstream effects.
% Related stories:
%   - platform_curation_algorithms (ε≈0.55, Tangled Rope)
%
narrative_ontology:affects_constraint(sturgeons_law, platform_curation_algorithms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. As a Mountain, directionality is not a factor.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */