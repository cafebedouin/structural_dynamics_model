% ============================================================================
% CONSTRAINT STORY: indexical_extraction_variance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_extraction_variance, []).

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
 *   constraint_id: indexical_extraction_variance
 *   human_readable: Indexical Extraction Variance as Structural Invariant
 *   domain: constraint_theory/structural_dynamics/social_systems
 *
 * SUMMARY:
 *   Indexical extraction variance is the structural principle that the same
 *   constraint produces different chi values (experienced extraction) for
 *   agents at different structural positions. This is not a contingent
 *   feature of specific constraints but a mathematical property of the chi
 *   formula: chi = epsilon × f(d) × sigma(S), where d (directionality) is
 *   derived from the agent's power level, exit options, and relationship to
 *   the constraint (beneficiary vs victim). Given this formula, variance in
 *   experienced extraction across positions is mathematically necessary. A
 *   constraint with epsilon = 0.40 produces chi ≈ -0.05 for an institutional
 *   beneficiary with arbitrage exit (d ≈ 0.05, f(d) ≈ -0.12) and chi ≈ 0.68
 *   for a powerless victim with trapped exit (d ≈ 0.95, f(d) ≈ 1.42). The
 *   variance is not a bug — it is the core insight of indexical
 *   classification. This constraint story models the variance principle
 *   itself as a mountain: an unchangeable structural fact that all agents
 *   encounter regardless of their position. The extractiveness is near-zero
 *   because the variance principle itself does not extract — it describes how
 *   extraction is distributed. The accessibility collapse is high because no
 *   alternative framework can eliminate position-dependent experience without
 *   eliminating the concept of structural position itself. Resistance is low
 *   because attempting to 'reform' the variance principle is a category error
 *   — you can reform specific constraints, but you cannot reform the
 *   mathematical relationship between position and experience.
 *
 * KEY AGENTS:
 *   - Trapped Agent: Powerless/trapped — experiences high chi from most constraints; cannot change position or variance principle
 *   - Beneficiary Institution: Institutional/arbitrage — experiences low or negative chi from most constraints; cannot change variance principle
 *   - Reform Coalition: Organized/mobile — can change specific constraints but not the variance principle itself
 *   - Framework Observer: Analytical/analytical — recognizes variance as theorem, not contingency
 *   - Constrained Middle: Moderate/constrained — experiences moderate chi; observes variance across positions
 *   - Mobile Elite: Powerful/mobile — can choose constraints but not eliminate variance principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_extraction_variance, 0.08).
domain_priors:suppression_score(indexical_extraction_variance, 0.02).
domain_priors:theater_ratio(indexical_extraction_variance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_extraction_variance, extractiveness, 0.08).
narrative_ontology:constraint_metric(indexical_extraction_variance, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(indexical_extraction_variance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_extraction_variance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(indexical_extraction_variance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_extraction_variance, mountain).
narrative_ontology:human_readable(indexical_extraction_variance, "Indexical Extraction Variance as Structural Invariant").
narrative_ontology:topic_domain(indexical_extraction_variance, "constraint_theory/structural_dynamics/social_systems").

domain_priors:emerges_naturally(indexical_extraction_variance).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED AGENT (MOUNTAIN) — Experiences indexical variance as unchangeable structural fact. Cannot alter their position in the extraction flow, cannot change the indices that determine their chi value. The variance itself is immutable even when the specific constraint producing it is contingent.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BENEFICIARY INSTITUTION (MOUNTAIN) — Experiences indexical variance as structural law. The institution benefits from its position but cannot change the mathematical fact that chi varies with indices. The variance is a property of how constraints interact with structural position, not a policy choice.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REFORM COALITION (MOUNTAIN) — Can change specific constraints (reduce epsilon, lower suppression, add sunset clauses) but cannot eliminate the indexical variance itself. Even in a perfectly just system, agents with different power/exit/scope will experience the same coordination mechanism differently. The variance is a mathematical property of the chi formula.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FRAMEWORK OBSERVER (MOUNTAIN) — Indexical extraction variance is a theorem, not a contingent arrangement. Given chi = epsilon × f(d) × sigma(S), and given that d derives from structural position (beneficiary vs victim, power level, exit options), variance in experienced extraction across positions is mathematically necessary. This is not a social construct that can be reformed — it is a property of how constraints map to experience.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CONSTRAINED MIDDLE (MOUNTAIN) — Experiences moderate extraction from most constraints. Can observe that others experience the same constraint as net benefit or severe extraction, but cannot change the structural fact that position determines experience. The variance is visible and unchangeable.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MOBILE ELITE (MOUNTAIN) — Can exit specific constraints but cannot exit the indexical variance principle. Even when choosing which constraints to engage with, the mathematical relationship between structural position and experienced extraction remains invariant. The variance is a law of constraint dynamics.
constraint_indexing:constraint_classification(indexical_extraction_variance, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_extraction_variance_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(indexical_extraction_variance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_extraction_variance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(indexical_extraction_variance, ExtMetricName, E),
    domain_priors:suppression_score(indexical_extraction_variance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(indexical_extraction_variance),
    narrative_ontology:constraint_metric(indexical_extraction_variance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(indexical_extraction_variance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(indexical_extraction_variance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The variance principle itself does not extract — it is a descriptive mathematical property of how constraints interact with structural position. The small non-zero value reflects that recognizing indexical variance requires conceptual overhead (learning the framework, understanding the chi formula, accepting that experience is position-dependent). This overhead is real but minimal compared to the extractiveness of actual constraints. Suppression (0.02): Near-zero. No agent is coerced into accepting indexical variance — it is a mathematical fact, not a policy. The small non-zero value reflects that some agents may resist the framework's implications (the uncomfortable recognition that their experience is not universal), but this resistance does not change the underlying mathematics. Theater ratio (0.05): Near-zero. The variance principle has no performative component — it is a theorem with empirical consequences. The small non-zero value reflects that some discourse about 'structural position' may be theatrical (using the language without the mathematics), but the principle itself is functional. Accessibility collapse (0.92): Very high. No alternative framework can eliminate position-dependent experience without eliminating the concept of structural position. Any system that acknowledges that agents have different power levels, exit options, and relationships to constraints must acknowledge that they experience constraints differently. Resistance (0.08): Very low. Attempting to 'change' indexical variance is a category error. You can change specific constraints (reduce epsilon, lower suppression, redistribute power), but you cannot change the mathematical fact that chi varies with indices.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify indexical extraction variance as mountain because the variance is a mathematical property of the chi formula, not a contingent social arrangement. The trapped agent cannot change their position or the variance principle. The beneficiary institution cannot eliminate the fact that their low chi depends on their structural advantages. The reform coalition can change specific constraints but not the variance principle. The framework observer recognizes the variance as a theorem. The constrained middle observes the variance empirically. The mobile elite can choose which constraints to engage but cannot eliminate position-dependent experience. The uniform mountain classification is not a failure of perspectival diversity — it is the correct diagnosis of a genuine structural invariant. The variance principle is one of the rare cases where the mountain classification is not a naturalization of contingency but an accurate recognition of mathematical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Indexical extraction variance has no beneficiaries or victims because it is not itself a constraint that extracts — it is a structural property of how constraints map to experience. All agents encounter the variance principle regardless of their position. The variance principle is position-invariant (all agents face it) even though the variance it describes is position-dependent (agents experience specific constraints differently). This is not a contradiction — it is the distinction between a meta-level structural principle and object-level constraints. The chi formula applies to object-level constraints; the variance principle is a theorem about the chi formula itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CERTIFICATION: Indexical extraction variance passes all mountain gates. (1) Emerges naturally: The variance is a direct consequence of the chi formula chi = epsilon × f(d) × sigma(S) and the derivation of d from structural position. No enforcement mechanism is required — the variance exists wherever the formula applies. (2) Accessibility collapse ≥ 0.85: No alternative framework can eliminate position-dependent experience without eliminating structural position itself. Any system acknowledging power differentials must acknowledge experiential differentials. (3) Resistance ≤ 0.15: Attempting to 'reform' a mathematical theorem is incoherent. (4) Low extraction and suppression: The principle itself does not extract or coerce. This is a legitimate mountain, not a naturalized contingency. The constraint models a meta-level structural property of the framework itself — the theorem that experience is indexical. This is the framework eating its own tail in the most productive way: using its own classification system to model the invariance of indexical classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_extraction_variance, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_extraction_variance, information_standard).

% DUAL FORMULATION NOTE:
% Indexical extraction variance is a meta-constraint: it describes a property of all other constraints rather than being a constraint that agents directly encounter. It does not decompose into multiple stories because it has a single, stable epsilon — the variance principle itself has near-zero extractiveness regardless of how it is measured. This is the correct application of the epsilon-invariance principle to a meta-level structural theorem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
