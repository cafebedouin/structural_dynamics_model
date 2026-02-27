% ============================================================================
% CONSTRAINT STORY: choice_architecture_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_choice_architecture_design, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: choice_architecture_design
 *   human_readable: Libertarian Paternalist Nudges
 *   domain: psychological/economic/social
 *
 * SUMMARY:
 *   Libertarian Paternalism proposes designing 'choice architectures' to
 *   nudge individuals towards outcomes deemed beneficial by the designer,
 *   while preserving the formal freedom to choose otherwise. This is commonly
 *   implemented via defaults, such as opt-out retirement savings plans or
 *   organ donation registries. The constraint lies in the cognitive cost
 *   imposed on individuals who wish to deviate from the
 *   paternalistically-chosen path, exploiting well-documented cognitive
 *   biases like status quo bias and inertia.
 *
 * KEY AGENTS:
 *   - Policy Designers (e.g., governments, corporations): Primary beneficiaries (institutional/arbitrage) — achieve policy or commercial goals with high compliance.
 *   - Nudged Individuals' Autonomy: Primary victim (powerless/trapped) — bears the cost of cognitive effort to dissent or has their choice made for them by default.
 *   - Individuals with Atypical Preferences: Secondary victims (moderate/constrained) — must actively fight the default to achieve their desired, non-standard outcome.
 *   - Society at Large: Secondary beneficiary — benefits from aggregate positive outcomes (e.g., higher national savings rate, more available organs for transplant).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(choice_architecture_design, 0.38).
domain_priors:suppression_score(choice_architecture_design, 0.5).
domain_priors:theater_ratio(choice_architecture_design, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(choice_architecture_design, extractiveness, 0.38).
narrative_ontology:constraint_metric(choice_architecture_design, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(choice_architecture_design, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(choice_architecture_design, tangled_rope).
narrative_ontology:human_readable(choice_architecture_design, "Libertarian Paternalist Nudges").
narrative_ontology:topic_domain(choice_architecture_design, "psychological/economic/social").

domain_priors:requires_active_enforcement(choice_architecture_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(choice_architecture_design, policy_designers).
narrative_ontology:constraint_beneficiary(choice_architecture_design, society_at_large).
narrative_ontology:constraint_victim(choice_architecture_design, nudged_individuals_autonomy).
narrative_ontology:constraint_victim(choice_architecture_design, individuals_with_atypical_preferences).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NUDGED INDIVIDUAL (SNARE) — Trapped by cognitive biases (e.g., status quo bias) that the choice architecture is designed to exploit. The 'freedom to choose' is illusory when significant cognitive cost is required to overcome the default. This perspective experiences the extraction of autonomy directly. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.54. While not meeting the snare threshold of χ≥0.66, this is the closest classification for an agent experiencing pure, uncompensated extraction.
constraint_indexing:constraint_classification(choice_architecture_design, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE POLICY DESIGNER (ROPE) — The 'libertarian paternalist' who designs the system. They see it as a pure coordination mechanism for achieving socially beneficial outcomes (e.g., higher savings rates, organ donation). From this position of power and control (arbitrage exit), the extractive component is invisible or justified as a net good. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.05. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(choice_architecture_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — This perspective sees both the genuine coordination function (steering towards better outcomes) and the asymmetric extraction of autonomy and cognitive effort. It recognizes that the system requires active design and enforcement and benefits designers while costing individuals who dissent. This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.53.
constraint_indexing:constraint_classification(choice_architecture_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE EMERGENCY HEALTH OFFICIAL (SCAFFOLD) — During a public health crisis, nudges (e.g., default vaccination appointments) are implemented as temporary measures to achieve a critical public good. These have an implicit or explicit sunset clause tied to the end of the emergency. The coercion is tolerated because it is temporary and serves a vital coordination function. This perspective requires adding `has_sunset_clause: true` to the base properties for this context.
constraint_indexing:constraint_classification(choice_architecture_design, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CORPORATE 'DARK PATTERN' USER (PITON) — This perspective sees nudges that have degraded into 'dark patterns' (e.g., confusing unsubscribe processes). The original justification (coordination) has atrophied, and the primary function is now inertial extraction, maintained through performative compliance with vague regulations. The theater_ratio of 0.40 is low for a Piton, but this perspective experiences the theatrical claim of 'user choice' as the dominant feature.
constraint_indexing:constraint_classification(choice_architecture_design, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE COGNITIVE REALIST (MOUNTAIN) — This perspective frames human cognitive biases as an unchangeable feature of reality. Therefore, *any* choice environment is inherently a 'nudge'. Designing them intentionally is simply acknowledging this natural law. This view naturalizes a contingent design choice into an inescapable feature of the world, a 'false summit' that the engine would flag because the base properties (ε=0.38, suppression=0.50) do not meet the Mountain classification thresholds.
constraint_indexing:constraint_classification(choice_architecture_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(choice_architecture_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(choice_architecture_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(choice_architecture_design, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(choice_architecture_design, TR),
    TR >= 0.70.

:- end_tests(choice_architecture_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system doesn't extract money directly, but it extracts cognitive resources and autonomy. The cost is the effort required to understand the options and override the default. Suppression (0.50): Moderate. While 'libertarian' in name, the effectiveness of nudges relies on the fact that most people will *not* exit the default path. The suppression is soft and psychological, not physical. Theater Ratio (0.40): The 'libertarian' label is a key piece of theater, framing a paternalistic intervention as one that preserves freedom. It masks the coercive, albeit soft, nature of the design.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The designer (e.g., Cass Sunstein) sees a benevolent Rope, a tool for helping people help themselves. The individual who is harmed by the default (e.g., someone who needs ready cash and is auto-enrolled in a savings plan) experiences a Snare that traps them in a costly default. The analytical observer sees the synthesis: a Tangled Rope that combines a genuine coordination function with a non-trivial, asymmetrically applied extraction of autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the policy designers who achieve their goals and, arguably, the societal aggregate that benefits from the nudged behavior. They have low directionality (d). Victims are the individuals whose autonomy is bypassed and especially those for whom the default choice is suboptimal. They bear the cognitive costs and have high directionality (d). The entire mechanism functions by creating a gradient of cognitive effort, which is the source of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case where misclassification leads to mandatrophy. Labeling it a pure Rope (the designer's view) ignores the extraction of autonomy and the costs imposed on dissenters. Labeling it a pure Snare (the dissenter's view) ignores the genuine, often widespread, benefits that result from the coordination function. The Tangled Rope classification, from the analytical perspective, correctly identifies and holds in tension both the coordination and extraction elements, preventing the collapse into a simplistic moral judgment and allowing for a structural analysis of its costs and benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(choice_architecture_design, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(choice_architecture_design, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
