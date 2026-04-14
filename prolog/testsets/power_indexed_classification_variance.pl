% ============================================================================
% CONSTRAINT STORY: power_indexed_classification_variance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_indexed_classification_variance, []).

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
 *   constraint_id: power_indexed_classification_variance
 *   human_readable: Power-Indexed Classification Variance as Structural Invariant
 *   domain: social_epistemology/constraint_classification/indexical_realism
 *
 * SUMMARY:
 *   Power-indexed classification variance is the structural property that
 *   identical constraint parameters (base extractiveness ε, suppression,
 *   coordination function, asymmetry) yield different type classifications
 *   when evaluated from different indexical positions (P, T, E, S). This is
 *   not a measurement error or epistemic limitation — it is the central claim
 *   of Deferential Realism: constraints have no observer-independent type.
 *   The chi formula χ = ε × f(d) × σ(S) mathematically encodes this
 *   dependency through the directionality function f(d) and scope modifier
 *   σ(S), which transform base extractiveness into effective extractiveness
 *   based on the observer's structural relationship to the constraint. A
 *   constraint with ε = 0.40 may classify as rope (χ ≈ 0.20) from an
 *   institutional beneficiary's perspective, tangled_rope (χ ≈ 0.45) from a
 *   moderate constrained observer's perspective, and snare (χ ≈ 0.70) from a
 *   powerless trapped victim's perspective — all from the same base
 *   properties. This variance is not a failure of the classification system;
 *   it is what the classification system measures. The framework treats this
 *   indexical structure as a natural law: no intervention can eliminate the
 *   dependency of classification on observer position without eliminating the
 *   distinction between beneficiaries and victims, which would collapse the
 *   framework's ability to detect extraction at all.
 *
 * KEY AGENTS:
 *   - Trapped Agent: Powerless/trapped — experiences variance as immutable epistemic constraint on their own classification capacity
 *   - Constrained Observer: Moderate/constrained — recognizes variance but cannot eliminate it
 *   - Mobile Actor: Powerful/mobile — can observe variance directly by occupying multiple positions
 *   - Organized Collective: Organized/mobile — can change which constraints exist but not the indexical structure
 *   - Institutional Arbitrageur: Institutional/arbitrage — exploits variance but cannot eliminate it
 *   - Framework Designer: Analytical/analytical — encodes variance as structural invariant in the chi formula
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_indexed_classification_variance, 0.08).
domain_priors:suppression_score(power_indexed_classification_variance, 0.03).
domain_priors:theater_ratio(power_indexed_classification_variance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_indexed_classification_variance, extractiveness, 0.08).
narrative_ontology:constraint_metric(power_indexed_classification_variance, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(power_indexed_classification_variance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(power_indexed_classification_variance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(power_indexed_classification_variance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_indexed_classification_variance, mountain).
narrative_ontology:human_readable(power_indexed_classification_variance, "Power-Indexed Classification Variance as Structural Invariant").
narrative_ontology:topic_domain(power_indexed_classification_variance, "social_epistemology/constraint_classification/indexical_realism").

domain_priors:emerges_naturally(power_indexed_classification_variance).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED AGENT (MOUNTAIN) — Experiences classification variance as immutable: cannot change their structural position, cannot access alternative framings, cannot exit the constraint being classified. The fact that their classification differs from a powerful observer's is a fixed feature of their epistemic situation.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED OBSERVER (MOUNTAIN) — Recognizes that classification depends on index but cannot change the dependency itself. Can shift their own position over biographical time but cannot eliminate the structural fact that position determines classification.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE ACTOR (MOUNTAIN) — Has access to multiple positions and can observe classification variance directly by moving between them. This mobility reveals the invariance: no matter which position they occupy, the structural relationship between index and classification remains fixed.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED COLLECTIVE (MOUNTAIN) — Collective action can change which constraints exist but cannot change the indexical structure of classification itself. A union can eliminate a snare but cannot eliminate the fact that snares appear differently to powerless vs institutional observers.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL ARBITRAGEUR (MOUNTAIN) — Can exploit classification variance (e.g., regulatory arbitrage based on different jurisdictions' framings) but cannot eliminate the variance itself. The arbitrage opportunity exists precisely because the indexical structure is invariant.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FRAMEWORK DESIGNER (MOUNTAIN) — The Deferential Realism framework itself treats indexical variance as a structural invariant. The chi formula χ = ε × f(d) × σ(S) encodes the dependency: given fixed base properties, classification MUST vary with index. This is not a bug to be fixed but the core insight: constraints have no observer-independent type.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_indexed_classification_variance_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(power_indexed_classification_variance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_indexed_classification_variance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(power_indexed_classification_variance, ExtMetricName, E),
    domain_priors:suppression_score(power_indexed_classification_variance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(power_indexed_classification_variance),
    narrative_ontology:constraint_metric(power_indexed_classification_variance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(power_indexed_classification_variance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(power_indexed_classification_variance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The indexical structure itself extracts minimally — it is a logical property of how classification works, not a mechanism that transfers resources or agency. The small non-zero value reflects the cognitive cost of recognizing that one's own classification is position-dependent, which can be disorienting or destabilizing for agents who expect objective constraint types. Suppression (0.03): Negligible. No agent is prevented from recognizing indexical variance; the structure is accessible to all positions (though differently experienced). The framework's documentation makes the variance explicit. Accessibility collapse (0.92): Very high. Once an agent understands the chi formula and the indexical tuple structure, the variance becomes immediately apparent and cannot be unseen. The collapse is not quite total (0.92 rather than 1.0) because some agents may resist the implication that their classifications are position-dependent. Resistance (0.08): Very low. Attempts to eliminate indexical variance by fiat (declaring one perspective 'correct' and others 'biased') fail immediately — the structural positions still exist, and the chi values still diverge. The framework's mathematical encoding makes resistance futile. Theater ratio (0.05): Negligible. The indexical structure is functional, not performative. The chi formula does real computational work; the perspectival tuple captures real structural differences.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this constraint as mountain, but they experience the invariance differently. The trapped agent experiences it as an epistemic limit on their own classification capacity — they cannot access the institutional perspective's framing. The mobile actor experiences it as an observable structural regularity — they can move between positions and watch the classifications shift predictably. The analytical observer experiences it as a mathematical necessity encoded in the chi formula. These are different phenomenological experiences of the same invariant structure. The gap is not in classification type but in what the mountain means: for the trapped agent, it is a barrier; for the mobile actor, it is a map; for the analytical observer, it is a theorem.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the conventional sense — it is a structural property of the classification system itself, not a mechanism that extracts from some agents and benefits others. All agents experience the indexical variance, but none are harmed by it (beyond the minimal cognitive cost of recognizing position-dependence). The framework treats this as a mountain from all perspectives because no agent can change the dependency of classification on index. The chi formula encodes the variance mathematically: f(d) maps the agent's structural relationship (beneficiary vs victim) to an effective power modifier, and σ(S) scales by scope. Given these functions, classification MUST vary with index when base properties are held constant. This is not contingent on institutional arrangements or policy choices — it follows from the definitions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by being its own resolution mechanism. The mandatrophy asks: 'How do we distinguish genuine coordination from extraction when both can be framed either way?' The answer is: 'By measuring from multiple indices and observing the variance.' A genuine rope shows low chi from all perspectives (universal coordination). A genuine snare shows high chi from the victim's perspective and low chi from the beneficiary's perspective (asymmetric extraction). A tangled rope shows moderate chi from most perspectives with a gap between beneficiary and victim (mixed coordination and extraction). The indexical variance is not noise to be eliminated — it is the signal. The framework's mountain classification of this variance means: you cannot eliminate the dependency of classification on perspective without eliminating the framework's ability to detect extraction. The variance is the detection mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_indexed_classification_variance, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_indexed_classification_variance, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is self-referential: it is a claim about the structure of constraint classification, and it is itself a constraint that must be classified. The framework treats it as a mountain (natural law of indexical realism) rather than as a rope (conventional choice of classification scheme) because attempts to eliminate indexical variance fail — the structural positions and their different experiences persist regardless of how we choose to model them. The chi formula encodes the variance as a mathematical necessity, not a modeling convention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
