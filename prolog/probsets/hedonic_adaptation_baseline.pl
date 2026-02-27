% ============================================================================
% CONSTRAINT STORY: hedonic_adaptation_baseline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hedonic_adaptation_baseline, []).

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
 *   constraint_id: hedonic_adaptation_baseline
 *   human_readable: The Hedonic Adaptation Baseline
 *   domain: psychological/biological
 *
 * SUMMARY:
 *   Hedonic adaptation is the biological tendency of humans to quickly return
 *   to a relatively stable level of happiness despite major positive or
 *   negative life events. This biological adaptation provides a baseline for
 *   emotional regulation and cognitive resource allocation, allowing
 *   individuals to cope with changing circumstances. The hedonic adaptation
 *   baseline ensures emotional equilibrium, facilitating long-term survival
 *   and well-being.
 *
 * KEY AGENTS:
 *   - Emotional Regulation Systems: Primary beneficiary (powerless/trapped) — benefits from a stable baseline that ensures a predictable emotional landscape.
 *   - Cognitive Resource Allocation: Secondary beneficiary (powerless/trapped) — adaptation frees cognitive resources from being overly focused on recent changes, enabling better coping.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hedonic_adaptation_baseline, 0.35).
domain_priors:suppression_score(hedonic_adaptation_baseline, 0.2).
domain_priors:theater_ratio(hedonic_adaptation_baseline, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, extractiveness, 0.35).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hedonic_adaptation_baseline, rope).
narrative_ontology:human_readable(hedonic_adaptation_baseline, "The Hedonic Adaptation Baseline").
narrative_ontology:topic_domain(hedonic_adaptation_baseline, "psychological/biological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hedonic_adaptation_baseline, emotional_regulation_systems).
narrative_ontology:constraint_beneficiary(hedonic_adaptation_baseline, cognitive_resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: From the perspective of an individual experiencing major life changes, hedonic adaptation functions as a rope, providing a baseline for emotional stability and resilience. Though changes might seem impactful initially, adaptation allows a return to a stable state, facilitating coping mechanisms. Trapped because the mechanism is biological and unavoidable.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: From an evolutionary biology perspective, hedonic adaptation serves as a rope, coordinating resource allocation. This mechanism prevents extreme and prolonged states of euphoria or distress, which could compromise survival. This provides analytical exit to understand the biological underpinnings.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 3: The analytical observer understands hedonic adaptation as a rope: a mechanism that allows for emotional equilibrium despite fluctuations in external stimuli. Analytical exit, long time horizon, and universal scope allows understanding of the biological mechanism.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hedonic_adaptation_baseline_tests).
:- end_tests(hedonic_adaptation_baseline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score is relatively low because while it does take something from a person's experience (limiting the highs and lows), it provides a much larger benefit in terms of overall stability. The suppression score is low because it is a biological imperative.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap here, as all viewpoints classify this mechanism as a rope. Individuals benefit from emotional resilience, while evolutionary biology benefits from efficient resource allocation, ensuring survival and overall well-being.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is focused on benefits. Both systems (emotional regulation and cognitive resource allocation) get a benefit from this adaptation, making them the beneficiaries. The mechanism's influence supports maintaining a stable internal environment.
 *
 * MANDATROPHY ANALYSIS:
 *   Hedonic adaptation is clearly a rope. This is not a snare because it is not actively extracting resources, it is providing equilibrium. It is not tangled, because it doesn't require active enforcement, it is a biological imperative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hedonic_adaptation_baseline, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hedonic_adaptation_baseline, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
