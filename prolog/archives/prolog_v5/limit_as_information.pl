% ============================================================================
% CONSTRAINT STORY: limit_as_information
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_limit_as_information, []).

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
 *   constraint_id: limit_as_information
 *   human_readable: Constitutive Limits as Information Boundaries
 *   domain: philosophy_of_mind/systems_theory/phenomenology
 *
 * SUMMARY:
 *   The constraint 'limit as information' captures a structural principle
 *   from information theory, phenomenology, and cognitive science:
 *   constitutive limits do not restrict agency but enable it by filtering
 *   representational noise and making choice tractable. This is a genuine
 *   mountain — a natural law of information processing that holds across all
 *   contexts and cannot be circumvented by power, resources, or institutional
 *   design. The constraint exhibits uniform mountain classification across
 *   all perspectives because it derives from the logical structure of
 *   representation itself: a signal requires a boundary to be distinguished
 *   from noise; intentionality requires horizons that exclude the irrelevant;
 *   autopoietic systems maintain identity through selective closure. Attempts
 *   to remove limits do not produce freedom but paralysis: infinite option
 *   spaces collapse decision-making into random selection or regression to
 *   crude heuristics. The limit is not imposed on cognition from outside — it
 *   is constitutive of cognition. This constraint is the philosophical
 *   foundation for why bounded rationality is not a defect but a design
 *   feature, and why choice architecture that makes limits explicit improves
 *   decision quality.
 *
 * KEY AGENTS:
 *   - Overwhelmed Decision-Maker: Immediate experiencer (powerless/trapped) — faces option paralysis without limits; experiences boundary as relief rather than restriction
 *   - Reflective Agent: Biographical learner (moderate/constrained) — discovers through experience that limit recognition improves decision quality
 *   - Institutional Designer: Systems architect (organized/mobile) — designs choice architectures; observes that removing limits degrades system performance
 *   - Cognitive Science Community: Empirical researchers (institutional/arbitrage) — cross-cultural studies confirm bounded rationality as universal feature
 *   - Resource-Rich Actor: High-capacity agent (powerful/mobile) — has resources to expand options; discovers resources cannot eliminate information-theoretic limits
 *   - Analytical Observer: Theoretical synthesizer (analytical/analytical) — integrates information theory, phenomenology, and systems theory to identify the limit as constitutive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(limit_as_information, 0.08).
domain_priors:suppression_score(limit_as_information, 0.02).
domain_priors:theater_ratio(limit_as_information, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(limit_as_information, extractiveness, 0.08).
narrative_ontology:constraint_metric(limit_as_information, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(limit_as_information, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(limit_as_information, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(limit_as_information, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(limit_as_information, mountain).
narrative_ontology:human_readable(limit_as_information, "Constitutive Limits as Information Boundaries").
narrative_ontology:topic_domain(limit_as_information, "philosophy_of_mind/systems_theory/phenomenology").

domain_priors:emerges_naturally(limit_as_information).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVERWHELMED DECISION-MAKER (MOUNTAIN) — Faces infinite option space without constitutive limits. Experiences the limit not as constraint but as relief: the boundary makes choice possible by filtering noise. Cannot exit the need for limits — attempting to decide without boundaries produces paralysis, not freedom. The limit is experienced as natural law of cognition.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REFLECTIVE AGENT (MOUNTAIN) — Recognizes limits as constitutive of agency itself. Can reflect on the boundary but cannot operate without it. Biographical experience confirms that decision quality improves with limit recognition: constraints don't restrict choice, they enable it by reducing entropy. The limit is structural to intentionality.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL DESIGNER (MOUNTAIN) — Designs choice architectures and recognizes that well-designed systems make limits explicit rather than hidden. Can choose which limits to make salient but cannot eliminate the need for limits. Generational observation: systems that attempt to remove all constraints produce decision paralysis and regression to heuristic shortcuts. The limit is a design invariant.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: COGNITIVE SCIENCE COMMUNITY (MOUNTAIN) — Empirical research across cultures and contexts confirms that bounded rationality is not a bug but a feature. Can study different limit structures but cannot find agents who function without limits. The Hick-Hyman law, satisficing behavior, and choice overload effects are cross-cultural universals. The limit is a natural law of information processing.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From information theory: a signal requires a boundary to be distinguished from noise. From phenomenology: intentionality requires horizons that exclude the irrelevant. From systems theory: autopoietic systems maintain identity through selective closure. The limit is not imposed on decision-making; it is constitutive of decision-making. This is a genuine mountain: the constraint emerges from the logical structure of representation itself.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: RESOURCE-RICH ACTOR (MOUNTAIN) — Has resources to expand option sets and hire advisors to process information. Discovers that expanding options without limit recognition degrades decision quality: more choices do not produce better outcomes past a threshold. The limit is not a resource constraint but an information-theoretic one. Even infinite resources cannot eliminate the need for boundaries that make signals distinguishable.
constraint_indexing:constraint_classification(limit_as_information, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(limit_as_information_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(limit_as_information, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(limit_as_information, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(limit_as_information, ExtMetricName, E),
    domain_priors:suppression_score(limit_as_information, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(limit_as_information),
    narrative_ontology:constraint_metric(limit_as_information, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(limit_as_information, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(limit_as_information_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The limit does not extract from agents — it enables them. The minimal extractiveness reflects only the cognitive cost of recognizing and working with limits (meta-cognitive overhead). This cost is inherent to information processing, not imposed by any beneficiary. Suppression (0.02): Negligible. Agents are not coerced into accepting limits — they discover limits as structural features of decision-making. Attempting to operate without limits produces worse outcomes (paralysis, random choice, heuristic regression), which agents recognize and avoid. The minimal suppression reflects only the inevitability of the constraint, not active enforcement. Theater ratio (0.15): Very low. The constraint is functional, not performative. Limit recognition genuinely improves decision quality by reducing option-space entropy. Some theater exists in institutional contexts where limits are ritualized (e.g., bureaucratic procedures that claim to structure choice but actually obscure it), but the core constraint — that boundaries enable signals — is not theatrical. Accessibility collapse (0.92): Very high. The constraint is accessible to all agents through direct experience: everyone who has faced choice overload recognizes that boundaries help. No specialized knowledge required. Resistance (0.08): Very low. Agents do not resist the constraint once they recognize it. The minimal resistance reflects only initial confusion (mistaking limits for restrictions) before experiential learning reveals limits as enabling.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists for this constraint. All agents, regardless of power, time horizon, exit options, or scope, classify the limit as mountain. The powerless agent with no exit experiences the limit as natural law (cannot escape the need for boundaries). The institutional designer with arbitrage options experiences the limit as natural law (cannot design systems that function without boundaries). The analytical observer experiences the limit as natural law (derives from information theory and phenomenology). This uniform classification is diagnostic: the constraint is a genuine mountain, not a naturalized institutional arrangement. The absence of any snare, rope, or tangled_rope perspectives confirms that the limit is not extractive, coordinative, or hybrid — it is a structural invariant of representation.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-mountain constraint with no beneficiaries or victims. All agents experience the limit as a structural feature of cognition, not as extraction. Directionality values are irrelevant — the constraint does not flow from or toward any agent. The engine will compute near-zero or negative chi for all perspectives because base extractiveness is very low and no agent occupies a victim position. The mountain classification derives from the natural law signature (emerges naturally, high accessibility collapse, low resistance, low extractiveness, low suppression), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates what a genuine mountain looks like in the DR framework. It is NOT a naturalized institutional arrangement (no false summit). It is NOT a coordination mechanism with low extraction (no beneficiaries). It is NOT a degraded ritual (theater ratio is low and functional). It is a logical/information-theoretic limit that emerges from the structure of representation itself. The mandatrophy resolution is straightforward: this is mountain from all perspectives because the constraint is constitutive of agency, not imposed on agency. Attempting to classify it as rope, snare, or tangled_rope would require identifying beneficiaries or victims, but none exist — the limit does not extract from anyone or coordinate between anyone. It simply is. The constraint passes all mountain gates: emerges naturally (true), accessibility collapse (0.92 > 0.85), resistance (0.08 < 0.15), extractiveness (0.08 < 0.25), suppression (0.02 < 0.05). The natural law signature is unambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(limit_as_information, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(limit_as_information, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is foundational and does not decompose. It is not observer-dependent: all measurement methodologies (phenomenological introspection, cognitive experiments, information-theoretic analysis, systems modeling) converge on the same structural principle. The limit is invariant across observables because it derives from the logical structure of representation, not from contingent institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
