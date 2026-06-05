% ============================================================================
% CONSTRAINT STORY: second_order_unintended_consequences
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_order_unintended_consequences, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: second_order_unintended_consequences
 *   human_readable: The Cobra Effect Trap
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The Cobra Effect Trap describes a structural pathology in incentive
 *   design where a coordination mechanism created to solve a primary problem
 *   generates unexpected secondary effects that worsen the overall systemic
 *   state. The archetype is the colonial Indian cobra bounty: British
 *   administrators offered payment for dead cobras to reduce cobra attacks.
 *   The incentive structure (reward = dead cobra) coordinated cobra hunting
 *   but generated an unintended secondary effect: bounty hunters began
 *   breeding cobras in captivity to earn the reward, dramatically increasing
 *   the cobra population. When the program was abandoned, the captive-bred
 *   cobra population was released, leaving the systemic state worse than
 *   before. The constraint exhibits the signature of a tangled rope: it has a
 *   genuine coordination function (solving the primary problem) AND
 *   asymmetric extraction (unintended consequences fall on those not party to
 *   the original coordination mechanism). The primary problem solver captures
 *   the benefit of solving the primary problem. The systemic state (and
 *   actors bearing secondary effects) captures the cost of the unintended
 *   consequences. The extraction emerges from the mechanism's blindness to
 *   second-order effects — the designer's success metric (primary problem
 *   metric) is independent of the mechanism's actual systemic impact. Over
 *   time, theater ratio increases as oversight focuses on procedural
 *   compliance with the mechanism rather than on whether the mechanism
 *   achieves its intended effect. Adaptation cohorts see both the benefit and
 *   the harm, creating a perspectival gap between the mechanism's designer
 *   and those who experience its secondary effects.
 *
 * KEY AGENTS:
 *   - Primary Problem Solver: Institutional beneficiary (institutional/arbitrage) — captures credit and success metrics for solving primary problem; externalizes secondary effects
 *   - Systemic State & Secondary Effect Bearers: Powerless victims (powerless/trapped) — did not consent to coordination mechanism; experience worsened state with no exit option
 *   - Adaptation Cohort: Moderate agents (moderate/constrained) — initially benefit from primary solution but face accumulating secondary effects over biographical timescale
 *   - Problem Definition Institution: Regulatory body (institutional/arbitrage) — maintains mechanism through procedural oversight; theater ratio reflects performative compliance
 *   - Reform Coalition: Organized advocates (organized/mobile) — work to redesign mechanism to preserve primary benefit while mitigating secondary effects; see sunset pathway
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — recognizes cobra effect as structural pattern in incentive design; sees tangled rope as inevitable unless mechanism fundamentally redesigned
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_order_unintended_consequences, 0.52).
domain_priors:suppression_score(second_order_unintended_consequences, 0.58).
domain_priors:theater_ratio(second_order_unintended_consequences, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_order_unintended_consequences, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_order_unintended_consequences, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_order_unintended_consequences, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_order_unintended_consequences, tangled_rope).
narrative_ontology:human_readable(second_order_unintended_consequences, "The Cobra Effect Trap").
narrative_ontology:topic_domain(second_order_unintended_consequences, "social/economic/technological").

domain_priors:requires_active_enforcement(second_order_unintended_consequences).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_order_unintended_consequences, primary_problem_solver).
narrative_ontology:constraint_beneficiary(second_order_unintended_consequences, metric_optimization_agent).
narrative_ontology:constraint_victim(second_order_unintended_consequences, systemic_state).
narrative_ontology:constraint_victim(second_order_unintended_consequences, unintended_consequence_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINTENDED CONSEQUENCE BEARERS (SNARE) — Actors who experience the secondary effects (worsened systemic state) have no exit option. They did not consent to the original coordination mechanism. They bear concentrated costs while benefits are diffuse. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(second_order_unintended_consequences, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIMARY PROBLEM SOLVER (ROPE) — Institution that designed the coordination mechanism to solve the primary problem experiences it as pure coordination. Captures immediate success metrics and credit for problem-solving. The secondary effects are externalized. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(second_order_unintended_consequences, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ADAPTATION COHORT (TANGLED ROPE) — Secondary agents who initially benefit from the coordination mechanism but gradually face worsening systemic effects. They see both the coordination function (solving primary problem) and the extraction (bearing unintended consequences). d≈0.62, f(d)≈0.90, σ=1.0 → χ≈0.47.
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROBLEM DEFINITION INSTITUTION (PITON) — Regulatory or governance bodies that continue to maintain the coordination mechanism despite recognized secondary effects. Theater ratio reflects performative monitoring and incremental adjustments that preserve the mechanism's form without addressing causal structure. theater_ratio=0.64 reflects that oversight has become largely procedural. d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(second_order_unintended_consequences, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (NGOs, research networks, policy advocates) working to replace or sunset the coordination mechanism with an alternative that preserves the primary benefit while mitigating secondary effects. See the constraint as temporary, with a defined exit path. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.27.
constraint_indexing:constraint_classification(second_order_unintended_consequences, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the cobra effect reveals a structural pattern: incentive systems designed to solve first-order problems have an inherent tendency to generate second-order effects that worsen the state. The coordination (solving the primary problem) and the extraction (unintended consequences) are structurally inseparable in the current design. d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_order_unintended_consequences_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_order_unintended_consequences, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_order_unintended_consequences, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_order_unintended_consequences, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_order_unintended_consequences, TR),
    TR >= 0.70.

:- end_tests(second_order_unintended_consequences_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, and rising. Initial extractiveness (0.18) reflects the mechanism's early success in solving the primary problem with minimal visible secondary effects. Over the interval, extractiveness increases as secondary effects accumulate and become apparent. By time=6, extractiveness has reached 0.52 because the mechanism continues to be deployed despite recognized secondary effects. The escalation reflects that the problem solver has optimized for the primary metric (cobra deaths, or whatever the target was) rather than for systemic impact. Suppression (0.58): Moderate-high. Significant barriers to exiting the mechanism include: (1) the primary problem is still unsolved if the mechanism is withdrawn; (2) institutional inertia in governance structures that maintain the mechanism; (3) narrative suppression — the mechanism's success is celebrated; its secondary effects are framed as unrelated or inevitable. Suppression is not total because reform coalitions can and do mobilize. Theater ratio (0.64): High and increasing (0.32 → 0.64). Initial theater reflects that the mechanism looks like a legitimate coordination solution (transparent reward structure, clear success metric). Over time, theater increases because oversight becomes procedural — monitoring compliance with the mechanism rather than outcomes. Adjustments to the mechanism become performative: tightening eligibility rules, claiming to address specific secondary effects, while leaving the causal structure intact.
 *
 * PERSPECTIVAL GAP:
 *   The primary problem solver and problem definition institution see the mechanism as legitimate coordination (Rope/Piton) with occasional unintended consequences treated as correctable failures. The analytical observer at the civilizational level recognizes the pattern as structural (Tangled Rope) — the secondary effects are inherent to the incentive structure, not contingent failures. The systemic state and secondary effect bearers see pure extraction (Snare) — they experience only costs with no coordination benefit. The adaptation cohort sees both (Tangled Rope) — they initially benefit from solving the primary problem but gradually bear increasing secondary effects. The reform coalition sees a temporary problem with a redesign pathway (Scaffold) — if the primary problem can be solved through alternative mechanisms, the current one can be sunsetted. The perspectival gap reflects who was included in the original coordination problem and who bears the secondary effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Primary problem solver: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Captures benefit of solving primary problem; has exit option (can walk away from mechanism responsibility). Secondary effect bearers: Victim + trapped → d≈0.92, f(d)≈1.40. Did not consent to mechanism; experience only costs; cannot exit. Adaptation cohort: Victim + constrained → d≈0.62, f(d)≈0.90. Initially aligned with primary solution but face accumulating costs; constrained exit because primary problem remains unsolved without mechanism. Reform coalition: Organized + mobile → d≈0.45, f(d)≈0.48. See alternative pathways and have organizational capacity to mobilize; d is moderate because they have agency but face institutional resistance. Problem definition institution: Institutional + arbitrage → d≈0.10, f(d)≈-0.09. Maintains mechanism for policy continuity; can shift policy if political consensus changes. Analytical observer: analytical → d≈0.70, f(d)≈1.12. Sees the structural pattern but has limited power to change institutions; high d reflects that civilizational-level analysis must bear the cognitive burden of recognizing the pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by establishing that the cobra effect is a genuine tangled rope, not a rope-gone-wrong or a snare disguised as rope. The coordination function (solving the primary problem) is real and valuable. The extraction (unintended consequences) is also real and structural. The constraint is 'both' not 'neither.' The mandatrophy would resolve to snare if the primary problem was never actually solved, or to rope if the secondary effects were negligible. But in the canonical cobra effect, both the coordination and the extraction are substantial and inseparable in the current mechanism design. The reform coalition perspective (scaffold) offers an exit path: redesign the mechanism to preserve the coordination function while eliminating the incentive for secondary effects. The analytical observer confirms this is possible in principle but notes that institutional inertia typically prevents the redesign until the secondary effects become catastrophic. The mandatrophy resolves by confirming the tangled rope classification as stable and by identifying the design flaw (success metric divorced from systemic outcome) as the structural source.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_benefit_measurement,
    'How much of the original primary problem was actually solved by the coordination mechanism, versus how much was solved by exogenous factors or displacement to secondary forms?',
    'Counterfactual analysis comparing problem trajectory with/without the mechanism; decomposition of solution sources (mechanism vs exogenous vs displacement)',
    'If mechanism solved <30% of primary problem: it was extraction disguised as coordination. If >70%: the coordination benefit is real and the mechanism justified despite secondary effects. If 30-70%: true tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_benefit_measurement, empirical, 'Proportion of primary problem solved by mechanism vs exogenous factors').

omega_variable(
    secondary_effect_inevitability,
    'Are the secondary effects inherent to the incentive structure, or were they contingent choices that could have been anticipated and prevented?',
    'Historical reconstruction of design process; identification of ignored warnings or alternative designs that were proposed but rejected; causal pathway analysis from incentive to secondary effect',
    'If inherent: the mechanism requires fundamental redesign. If contingent: the mechanism was negligent design. If mixed: partial redesign is sufficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_effect_inevitability, empirical, 'Whether secondary effects were inherent or contingent design outcomes').

omega_variable(
    systemic_state_degradation_threshold,
    'At what point does the cumulative cost of secondary effects exceed the benefit of solving the primary problem?',
    'Cost-benefit analysis over time; identification of inflection point where unintended consequences outweigh primary benefit; stakeholder impact accounting',
    'If threshold already crossed: mechanism should be sunsetted immediately. If threshold approaching: urgent redesign needed. If threshold distant: mechanism retains temporary validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_state_degradation_threshold, preference, 'Threshold where unintended consequences exceed primary benefit').

omega_variable(
    alternative_mechanism_existence,
    'Do alternative coordination mechanisms exist that solve the primary problem with lower secondary-effect risk?',
    'Systematic search of design space; pilot testing of alternatives; causal analysis of why alternatives were not adopted originally',
    'If viable alternatives exist: scaffold perspective confirmed — sunset is feasible. If no alternatives: mechanism may be locally optimal despite costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_existence, empirical, 'Existence and viability of alternative coordination mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_order_unintended_consequences, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobra_tr_t0, second_order_unintended_consequences, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cobra_tr_t3, second_order_unintended_consequences, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cobra_tr_t6, second_order_unintended_consequences, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(cobra_be_t0, second_order_unintended_consequences, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cobra_be_t3, second_order_unintended_consequences, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(cobra_be_t6, second_order_unintended_consequences, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_order_unintended_consequences, resource_allocation).
narrative_ontology:affects_constraint(second_order_unintended_consequences, goodhart_metric_substitution).
narrative_ontology:affects_constraint(second_order_unintended_consequences, perverse_incentive_cascade).

% DUAL FORMULATION NOTE:
% The cobra effect trap is upstream of specific perverse incentive failures. It establishes the general structural pattern: any resource allocation mechanism that creates a success metric independent of systemic outcome is vulnerable to generating secondary effects that worsen the state. Goodhart's Law (metric substitution) and perverse incentive cascades are downstream instantiations of this pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
