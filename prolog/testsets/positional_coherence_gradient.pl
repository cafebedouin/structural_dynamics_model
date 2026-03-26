% ============================================================================
% CONSTRAINT STORY: positional_coherence_gradient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_positional_coherence_gradient, []).

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
 *   constraint_id: positional_coherence_gradient
 *   human_readable: Positional Coherence Gradient in Explanatory Frameworks
 *   domain: epistemology/information_theory/institutional_analysis
 *
 * SUMMARY:
 *   The positional coherence gradient describes a fundamental asymmetry in
 *   explanatory frameworks: some maintain their internal logic and predictive
 *   power when examined from adversarial or neutral structural positions,
 *   while others collapse when the observer's position shifts. This is not a
 *   social construction but an information-theoretic property. Frameworks
 *   with high positional coherence contain genuine mutual information with
 *   the phenomena they explain — their validity is not position-dependent.
 *   Frameworks with low positional coherence appear valid only from specific
 *   institutional or ideological positions; their explanatory power derives
 *   from contextual scaffolding rather than structural correspondence. The
 *   gradient is observable through cross-domain persistence patterns (does
 *   the framework explain phenomena outside its original domain?), survival
 *   across institutional collapse (does it persist when its supporting
 *   institutions fail?), and adversarial examination resistance (does it
 *   survive scrutiny from hostile positions?). This constraint is a mountain
 *   from all perspectives because the gradient itself is not a choice or a
 *   policy — it is a property of how information relates to reality. Agents
 *   at all power levels and time horizons experience it as unchangeable,
 *   though they differ in their ability to evaluate where specific frameworks
 *   sit on the gradient.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the gradient as an information-theoretic structural feature
 *   - Epistemic Dependent: Limited verification capacity (powerless/trapped) — experiences the gradient as an unchangeable filter on available explanations
 *   - Knowledge Institution: Generational curator (institutional/arbitrage) — discovers the gradient through institutional lifecycle dynamics and preservation patterns
 *   - Adversarial Examiner: Challenge-oriented agent (powerful/mobile) — uses the gradient as a detection mechanism for genuine versus position-dependent validity
 *   - Cross-Cultural Transmission Network: Translation filter (organized/constrained) — experiences the gradient as the difference between what survives and what collapses during cultural transmission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positional_coherence_gradient, 0.08).
domain_priors:suppression_score(positional_coherence_gradient, 0.03).
domain_priors:theater_ratio(positional_coherence_gradient, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positional_coherence_gradient, extractiveness, 0.08).
narrative_ontology:constraint_metric(positional_coherence_gradient, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(positional_coherence_gradient, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(positional_coherence_gradient, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(positional_coherence_gradient, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positional_coherence_gradient, mountain).
narrative_ontology:human_readable(positional_coherence_gradient, "Positional Coherence Gradient in Explanatory Frameworks").
narrative_ontology:topic_domain(positional_coherence_gradient, "epistemology/information_theory/institutional_analysis").

domain_priors:emerges_naturally(positional_coherence_gradient).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The gradient between positionally-coherent and positionally-fragile explanatory frameworks is a structural feature of information itself. Explanations that survive adversarial examination contain more mutual information with reality than those that collapse under perspective shift. This is not a social convention but an information-theoretic constraint.
constraint_indexing:constraint_classification(positional_coherence_gradient, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: EPISTEMIC DEPENDENT (MOUNTAIN) — Agents with limited resources for independent verification experience the coherence gradient as an unchangeable filter. They cannot alter the fact that some explanations maintain coherence across contexts while others require sustained institutional support to appear credible. The gradient exists independent of their ability to evaluate it.
constraint_indexing:constraint_classification(positional_coherence_gradient, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: KNOWLEDGE INSTITUTION (MOUNTAIN) — Institutions that curate and transmit explanatory frameworks experience the coherence gradient as a structural constraint on what can be preserved across institutional transitions. Frameworks with high positional coherence survive regime changes, funding collapses, and paradigm shifts. Those with low coherence require continuous institutional maintenance and collapse when support is withdrawn. The gradient is not created by institutions but discovered through their lifecycle dynamics.
constraint_indexing:constraint_classification(positional_coherence_gradient, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERSARIAL EXAMINER (MOUNTAIN) — Agents with resources and incentives to challenge explanatory frameworks experience the coherence gradient as a detection mechanism. High-coherence frameworks resist adversarial examination not through institutional protection but through internal consistency that survives perspective shift. The gradient is the difference between explanations that require you to adopt a specific viewpoint to appear valid versus those that remain coherent from hostile positions.
constraint_indexing:constraint_classification(positional_coherence_gradient, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CROSS-CULTURAL TRANSMISSION NETWORK (MOUNTAIN) — Networks that transmit knowledge across cultural and linguistic boundaries experience the coherence gradient as a translation filter. Frameworks with high positional coherence survive translation because their validity does not depend on culturally-specific framing. Those with low coherence lose explanatory power when the original institutional context is removed. The gradient measures how much of an explanation's apparent validity is mutual information versus contextual scaffolding.
constraint_indexing:constraint_classification(positional_coherence_gradient, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(positional_coherence_gradient_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(positional_coherence_gradient, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(positional_coherence_gradient, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(positional_coherence_gradient, ExtMetricName, E),
    domain_priors:suppression_score(positional_coherence_gradient, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(positional_coherence_gradient),
    narrative_ontology:constraint_metric(positional_coherence_gradient, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(positional_coherence_gradient, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(positional_coherence_gradient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The gradient itself extracts nothing — it is a measurement property of explanatory frameworks. The minimal extractiveness reflects the cost of evaluation: determining where a framework sits on the coherence gradient requires resources (adversarial examination, cross-domain testing, institutional lifecycle observation). But this cost is inherent to the measurement process, not imposed by the gradient. Suppression (0.03): Minimal. The gradient does not suppress alternatives — it differentiates them. Agents remain free to adopt low-coherence frameworks; the gradient simply measures the cost of maintaining them across position shifts. Theater ratio (0.15): Very low. The gradient is directly measurable through empirical tests: Does the framework predict outside its original domain? Does it survive institutional collapse? Does it resist adversarial examination? These are concrete observables with minimal performative content. Accessibility collapse (0.92): Very high. The gradient is accessible to any agent with resources for cross-positional comparison. It does not require specialized training to observe that some explanations survive perspective shift while others collapse. Resistance (0.08): Very low. The gradient cannot be altered by institutional action, policy change, or collective agreement. It is a property of the information structure of explanatory frameworks themselves.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as mountain because the gradient is a structural feature of information, not a social arrangement. The epistemic dependent cannot exit the gradient's effects but also does not experience it as extraction — it is simply a property of the explanatory landscape they navigate. The knowledge institution discovers the gradient through preservation patterns but cannot alter it through policy. The adversarial examiner uses the gradient as a tool but does not create it. The cross-cultural transmission network experiences it as a translation filter but cannot bypass it. The analytical observer sees it as an information-theoretic necessity. There is no perspectival gap in classification type, only in evaluation capacity: some agents can measure where specific frameworks sit on the gradient more accurately than others, but all agree the gradient itself is unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain-only constraint with no beneficiaries or victims. The gradient is a measurement property, not an extraction mechanism. All agents experience it as unchangeable regardless of their structural position. The minimal extractiveness (0.08) reflects evaluation costs, not asymmetric extraction. No directionality overrides are needed because there is no extraction flow to model.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates that not all structural asymmetries are extraction mechanisms. The positional coherence gradient creates a real difference in outcomes — frameworks with high coherence persist and propagate more effectively than those with low coherence — but this difference reflects information-theoretic properties rather than institutional extraction. The gradient is a mountain because it emerges from the structure of explanation itself: frameworks that contain genuine mutual information with reality maintain coherence across position shifts, while those that depend on contextual scaffolding collapse when the scaffolding is removed. This is not a policy choice, not a coordination problem, and not an extraction mechanism. It is a measurement property of how explanations relate to what they purport to explain. The constraint resolves potential mandatrophy by showing that some asymmetries are natural laws rather than institutional arrangements — the challenge is distinguishing which is which, which is exactly what the DR framework's false summit detection is designed to do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(positional_coherence_gradient, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positional_coherence_gradient, information_standard).

% DUAL FORMULATION NOTE:
% The positional coherence gradient is a meta-constraint that applies to explanatory frameworks themselves, including the DR framework. The DR framework's own positional coherence is testable: does its classification logic survive examination from positions hostile to its premises? Does it explain phenomena outside its original domain (institutional extraction)? Does it persist when its supporting institutions (if any) collapse? The gradient is self-applying.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
