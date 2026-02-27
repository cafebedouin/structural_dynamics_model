% ============================================================================
% CONSTRAINT STORY: omelet_perfection_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omelet_perfection_complexity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: omelet_perfection_complexity
 *   human_readable: The French Omelet Paradox (Chasing Perfection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The French omelet paradox highlights the disparity between the perceived
 *   simplicity of a task and the actual skill required to achieve perfection.
 *   The commodification of 'perfection' in cooking acts as a tangible example
 *   for a broader array of pursuits. The desire to achieve something
 *   seemingly simple becomes a constraint.
 *
 * KEY AGENTS:
 *   - Home Cooks: Primary victim (powerless/trapped) — subject to internalized pressure to perform.
 *   - Aspiring Chefs: Secondary victim (moderate/constrained) — experience pressure to succeed in professional contexts.
 *   - Cooking Schools: Beneficiary (institutional/arbitrage) — monetize the pursuit of perfection through courses.
 *   - Online Cooking Platforms: Beneficiary (institutional/arbitrage) — provide accessible paths to perfection, driving traffic.
 *   - The Culinary Canon: Powerful agent maintaining what might be considered an ideal standard. High cultural value, but also a theatrical element in its endurance as a concept.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omelet_perfection_complexity, 0.55).
domain_priors:suppression_score(omelet_perfection_complexity, 0.4).
domain_priors:theater_ratio(omelet_perfection_complexity, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omelet_perfection_complexity, extractiveness, 0.55).
narrative_ontology:constraint_metric(omelet_perfection_complexity, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(omelet_perfection_complexity, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omelet_perfection_complexity, tangled_rope).
narrative_ontology:human_readable(omelet_perfection_complexity, "The French Omelet Paradox (Chasing Perfection)").
narrative_ontology:topic_domain(omelet_perfection_complexity, "social/psychological").

domain_priors:requires_active_enforcement(omelet_perfection_complexity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, cooking_schools).
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, online_cooking_platforms).
narrative_ontology:constraint_victim(omelet_perfection_complexity, home_cooks).
narrative_ontology:constraint_victim(omelet_perfection_complexity, aspiring_chefs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The frustrated home cook, trapped by the apparent simplicity and high standards, experiences repeated failure and diminished confidence. Feels extracted from by the idealized vision. No easy exit.
constraint_indexing:constraint_classification(omelet_perfection_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: The aspiring chef is constrained by rigorous training and performance expectations. They benefit from skill development but are also extracted from by the pressure to perform flawlessly. Can become mobile with skill but initially constrained.
constraint_indexing:constraint_classification(omelet_perfection_complexity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 3: Cooking schools and online platforms benefit by commoditizing the pursuit of perfection, offering courses, tools, and instruction. They arbitrage the perceived gap between novice and expert. Minimal extraction from their perspective.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: The culinary canon represents an inertial constraint. While the pursuit of the perfect omelet drives culinary innovation, the established standard of perfection can be seen as theatrically maintained even though mastery is very difficult. Benefit is less about current utility, and more about cultural reference point. Constrained exit.
constraint_indexing:constraint_classification(omelet_perfection_complexity, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omelet_perfection_complexity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(omelet_perfection_complexity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(omelet_perfection_complexity, TR),
    TR >= 0.70.

:- end_tests(omelet_perfection_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Novices invest time, effort, and money without easily attaining the desired result. Cooking schools and platforms derive monetary benefit. Suppression (0.40): Moderate. Social pressure and internalized performance standards suppress satisfaction with imperfection. Theater ratio (0.60): Moderate. The pursuit of perfection is driven more by the optics of skill than utility. There is performative element, and the gap between aspirational goals and results creates an extractive tension.
 *
 * PERSPECTIVAL GAP:
 *   The home cook sees repeated failure, a snare. Cooking institutions benefit from continuous demand. An analytical perspective looks at the culinary canon of difficult-to-master recipes with their long shelf life, suggesting an almost inertial piton-like quality.
 *
 * DIRECTIONALITY LOGIC:
 *   High directionality scores are assigned to those who are targeted with a particular standard of perfection and unable to escape it. Low directionality scores go to those who benefit economically or socially from that goal.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification avoids mislabeling coordination as pure extraction by acknowledging that the standard of perfection—though difficult and possibly theatrically maintained—also functions as an organizational goal and point of reference for a large group. It is not purely exploitative; it is also a cultural reference point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjective_definition_of_perfection,
    'How much does the perception of complexity and difficulty depend on the subjective definition of perfection?',
    'Compare the success rate against varying criteria (texture, appearance, etc.).',
    'If perfection is narrowly defined, complexity is high. If broadly, complexity is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjective_definition_of_perfection, conceptual, 'Dependence on definition').

omega_variable(
    tacit_knowledge_transfer,
    'To what extent can tacit knowledge involved in executing this task be transferred through formal instruction?',
    'Study various approaches and knowledge transmission. Measure success rates and instruction modes.',
    'High transfer reduces complexity. Low transfer means expertise remains elusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_transfer, empirical, 'Role of tacit knowledge.').

omega_variable(
    cultural_valuation_of_technique,
    'How much does the cultural valuation of technique and skill contribute to the perceived difficulty?',
    'Measure the psychological impact of different cultural representations of technique.',
    'Higher value will increase difficulty, lower value might make it easier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_valuation_of_technique, preference, 'Culture’s Role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omelet_perfection_complexity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omel_tr_t0, omelet_perfection_complexity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(omel_tr_t5, omelet_perfection_complexity, theater_ratio, 5, 0.5).
narrative_ontology:measurement(omel_tr_t10, omelet_perfection_complexity, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(omel_be_t0, omelet_perfection_complexity, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(omel_be_t5, omelet_perfection_complexity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(omel_be_t10, omelet_perfection_complexity, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omelet_perfection_complexity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
