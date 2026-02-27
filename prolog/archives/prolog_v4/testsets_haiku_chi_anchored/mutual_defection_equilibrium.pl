% ============================================================================
% CONSTRAINT STORY: mutual_defection_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mutual_defection_equilibrium, []).

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
 *   constraint_id: mutual_defection_equilibrium
 *   human_readable: The Infinite Prisoner's Dilemma Stalemate
 *   domain: economic/social
 *
 * SUMMARY:
 *   The infinite prisoner's dilemma stalemate represents a fundamental
 *   coordination failure where rational individual incentives produce
 *   collectively irrational outcomes. Each agent, facing no guarantee that
 *   others will cooperate, defects. All agents defect. All agents are worse
 *   off than if all cooperated. Yet no single agent can unilaterally escape
 *   this equilibrium — switching to cooperation alone guarantees
 *   exploitation. This constraint exemplifies how extraction can be enforced
 *   by mathematical structure rather than by explicit coercion. The
 *   extraction is mutual and symmetric: every agent both extracts from and is
 *   extracted from by every other agent. Yet because the extraction is stable
 *   (no single agent can improve by deviating), it persists indefinitely.
 *   Over time, defection becomes culturally normalized, and attempts at
 *   cooperation are viewed as weakness or irrationality. The constraint's
 *   extractiveness increases (0.55 → 0.68) as the defection norm hardens into
 *   institutions and cultural expectations. The theater ratio increases as
 *   well (0.30 → 0.45) because actors begin to perform justifications for
 *   defection ('self-interest is rational,' 'cooperation is naive') rather
 *   than simply defecting without commentary.
 *
 * KEY AGENTS:
 *   - Individual agents: Primary victims (powerless/trapped) — rationally defect despite knowing collective harm results; no unilateral escape.
 *   - All agents collectively: Aggregate victim (powerless/trapped) — generate mutual extraction through rational pursuit of self-interest; trapped in coordination failure.
 *   - Cultural institutions: Secondary enforcer (institutional/arbitrage) — propagate defection norms, making cooperation seem irrational or weak; benefit from norm stabilization.
 *   - Local cooperative coalitions: Experimental agents (organized/constrained) — attempt to break equilibrium through tit-for-tat strategies or trust-based communities; constrained by defection pressure.
 *   - External enforcers (e.g., states, regulators): Powerful coordinator (powerful/mobile) — can impose cooperation mechanisms through binding agreements and punishment regimes; extract legitimacy rent.
 *   - Game-theoretic analyst: Analytical observer (analytical/analytical) — risks naturalizing the equilibrium as an immutable law when it depends on contingent payoff structures and information asymmetries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mutual_defection_equilibrium, 0.68).
domain_priors:suppression_score(mutual_defection_equilibrium, 0.75).
domain_priors:theater_ratio(mutual_defection_equilibrium, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mutual_defection_equilibrium, extractiveness, 0.68).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mutual_defection_equilibrium, snare).
narrative_ontology:human_readable(mutual_defection_equilibrium, "The Infinite Prisoner's Dilemma Stalemate").
narrative_ontology:topic_domain(mutual_defection_equilibrium, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(mutual_defection_equilibrium, all_agents).
narrative_ontology:constraint_victim(mutual_defection_equilibrium, collective_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL DEFECTOR (SNARE) — Each agent is locked in defection because unilateral cooperation guarantees exploitation. No escape from the strategy without coordination mechanism. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.96. Maximum extraction: rational individual choice produces collective harm with no exit.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ITERATED POPULATION (SNARE) — Over multiple generations, defection becomes cultural norm. Cooperation attempts are branded as weakness. Exit requires cultural shift with massive coordination cost. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.93. Severe extraction with cultural enforcement.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LOCAL COALITION COORDINATORS (ROPE) — Some institutional actors benefit from coordination on mutual defection: they enforce the norms, extract legitimacy, and exploit the stability. d≈0.10, f(d)≈0.00, σ=0.8 → χ≈0.00. They see defection coordination as a pure coordination problem they solve.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: GAME-THEORETIC OBSERVER (MOUNTAIN) — From a mathematical perspective, mutual defection is a Nash equilibrium: no player can improve by unilateral deviation. This appears as an immutable law of rational choice. ε≈0.15, suppression≈0.03. However, this is a false summit — the 'law' depends on the payoff matrix and information structure, both contingent.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: EXTERNAL ENFORCER (TANGLED ROPE) — A powerful external actor can impose cooperation mechanisms (treaties, enforcement institutions, punishment regimes) and benefit from the coordination. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.41. Mixed: provides coordination but extracts legitimacy and compliance rent.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EXPERIMENTAL DEFECTOR COALITION (SCAFFOLD) — Some organized groups attempt to break the equilibrium via local cooperation bubbles (TFT networks, trust-based communities). If successful, these create an exit path. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.29. Temporary support structure with potential sunset if alternative equilibria stabilize.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutual_defection_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mutual_defection_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mutual_defection_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint generates welfare loss equal to the gap between mutual cooperation payoff and mutual defection payoff. This loss is borne by all agents. In standard PD payoffs (mutual cooperation: 3, mutual defection: 1, temptation: 5, sucker: 0), the loss is 2 units per agent per round. Over infinite rounds, cumulative loss approaches infinity. The extraction is mutual but mathematically guaranteed. Suppression (0.75): Very high. Agents cannot cooperate unilaterally without severe punishment (sucker payoff: 0). The threat of exploitation by potential cooperators creates complete suppression of unilateral cooperation attempts. This is not enforced by external coercion but by the payoff structure itself — the structure is the enforcement mechanism. Theater ratio (0.45): Moderate. Initially, agents simply defect without commentary. Over time, as the equilibrium hardens, they develop justifications ('rationality,' 'self-interest,' 'human nature') that perform a narrative role distinct from the simple strategic fact. This narrative layer grows as the defection norm becomes cultural; hence the trajectory from 0.30 to 0.45.
 *
 * PERSPECTIVAL GAP:
 *   The individual agent sees a snare (powerless/trapped) — they are locked in by rational incentives with no escape. The population sees a snare with cultural hardening (moderate/constrained) — over generations, defection becomes the 'natural' or 'realistic' strategy, and cooperation attempts are dismissed as naive. The institutional actor (defection coordinators) sees a rope (institutional/arbitrage) — they coordinate the defection norm and benefit from its stability. The game-theoretic analyst risks seeing a mountain (analytical/analytical) — mutual defection is a Nash equilibrium by the laws of game theory, making it seem immutable. The powerful external enforcer sees a tangled rope (powerful/mobile) — they can impose cooperation mechanisms and extract compliance rent. The organized experimental coalition sees a scaffold (organized/constrained) — they attempt local cooperation bubbles that might grow into an alternative equilibrium with a sunset to widespread cooperation. These perspectives are not reconcilable into a single 'true' classification — each reflects a real structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual agent (victim): Rational self-interest + trapped exit → d≈0.95, f(d)≈1.42. Maximum extraction: no escape. Population (victim): Cultural norm + constrained exit → d≈0.85, f(d)≈1.15. High extraction: cultural entrenchment makes exit costly. Institutional defection coordinators (beneficiary): Coordinate norm + arbitrage exit → d≈0.10, f(d)≈0.00. They benefit from defection coordination. External enforcer (powerful actor): Can impose cooperation + mobile exit → d≈0.45, f(d)≈0.50. Balanced: provides coordination mechanism but extracts legitimacy. Experimental coalition (organized agent): Attempt cooperation break + constrained exit → d≈0.50, f(d)≈0.65. Moderate extraction: they bear cost of cooperation attempts but have some agency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy (potential mislabeling of extraction as coordination) through five diagnostic tests: (1) BENEFICIARY TEST: The constraint has NO clear beneficiary. All agents are harmed by mutual defection. The only candidates for benefit are institutional actors who propagate the defection norm — but they benefit from the norm's stability, not from coordination function. (2) COOPERATION TEST: If all agents cooperated, all would be better off. The existence of a Pareto-superior outcome proves this is not pure coordination. (3) UNILATERAL DEVIATION TEST: No single agent can improve by switching strategy. This locks in the equilibrium but is characteristic of extraction (snare), not coordination (rope). A rope equilibrium would allow individual improvement through coordination; a snare equilibrium does not. (4) EXIT TEST: Agents cannot exit unilaterally. Exit requires collective coordination on an alternative equilibrium — itself a coordination problem. (5) HISTORICAL TEST: As defection norms harden (theater_ratio 0.30 → 0.45), agents develop narratives ('self-interest is rational') that perform a normative role beyond the strategic fact. This narrative layer is characteristic of snare consolidation, not rope maturation. Conclusion: This is a SNARE (mutual extraction under Nash equilibrium) that can be temporarily transformed into a TANGLED ROPE (if external enforcement is imposed) or a SCAFFOLD (if local cooperation bubbles grow). The mandatrophy is fully resolved: the constraint is extraction enforced by mathematical structure and cultural norm, not coordination failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payoff_matrix_contingency,
    'Is mutual defection a Nash equilibrium of the game''s inherent structure, or a consequence of the specific payoff values we have chosen to represent it?',
    'Parametric analysis: vary payoff values (rewards for cooperation, punishment for defection, temptation payoff) and track equilibrium type. Map the payoff space to equilibrium regions.',
    'If contingent on payoffs: the constraint is not a natural law but an artifact of representation. Reframing the payoff matrix (e.g., long-term reputation effects, altruistic preferences) can dissolve the equilibrium. If robust across payoff families: mutual defection is a deeper structural property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_matrix_contingency, empirical, 'Whether the Nash equilibrium depends on contingent payoff choices').

omega_variable(
    iteration_finite_or_infinite,
    'Is the iteration truly infinite, or does uncertainty about the game''s end create de facto finite horizons? How does this uncertainty affect equilibrium stability?',
    'Comparison of infinite-horizon analysis with finite-horizon models; empirical measurement of agent expectations about game duration; analysis of backward-induction logic in real populations.',
    'If finite: backward induction destabilizes cooperation even further, making the snare even more severe. If infinite but agents believe it finite: cooperation becomes viable in tail games, and the mountain classification fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iteration_finite_or_infinite, empirical, 'Whether infinite iteration is a realistic or idealized assumption').

omega_variable(
    information_asymmetry_escape,
    'Can bounded agents with limited information about others'' payoffs or strategies escape the defection trap through strategic ignorance or misunderstanding?',
    'Agent-based models with information asymmetry; empirical studies of cooperation in conditions of payoff uncertainty; analysis of whether misbeliefs stabilize cooperation.',
    'If yes: the snare is dependent on complete information. Information opacity creates alternative equilibria. If no: opacity merely adds noise without changing the fundamental extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_escape, empirical, 'Whether information asymmetry enables escape from defection equilibrium').

omega_variable(
    reputation_mechanism_binding,
    'Can reputation effects (losses from being known as a defector) generate sufficient incentive to sustain cooperation even in the infinite horizon?',
    'Empirical measurement of reputation penalties in real cooperation environments; game-theoretic models embedding reputation costs; longitudinal tracking of cooperators vs defectors in repeated-game populations.',
    'If reputation penalties are severe: cooperation becomes locally stable even without external enforcement (Rope classification dominates). If weak or absent: snare classification persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reputation_mechanism_binding, empirical, 'Whether reputation costs can sustain cooperation against defection').

omega_variable(
    collective_action_threshold,
    'Is there a critical mass of cooperators above which the equilibrium tips toward cooperation, or is defection stable even with high cooperation levels?',
    'Phase-transition analysis of cooperation in large populations; measurement of tipping points in real economies; spatial game-theoretic models with local interaction neighborhoods.',
    'If threshold exists and is reachable: the scaffold perspective is viable (sunset to cooperation equilibrium). If no threshold or it is unreachable: defection is locked in unless external intervention occurs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Whether a critical mass of cooperators can shift the equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mutual_defection_equilibrium, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mde_tr_t0, mutual_defection_equilibrium, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mde_tr_t50, mutual_defection_equilibrium, theater_ratio, 50, 0.38).
narrative_ontology:measurement(mde_tr_t100, mutual_defection_equilibrium, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(mde_be_t0, mutual_defection_equilibrium, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mde_be_t50, mutual_defection_equilibrium, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(mde_be_t100, mutual_defection_equilibrium, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mutual_defection_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, arms_race_dynamics).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, tragedy_of_the_commons).
narrative_ontology:affects_constraint(mutual_defection_equilibrium, regulatory_capture_equilibrium).

% DUAL FORMULATION NOTE:
% The infinite prisoner's dilemma stalemate is a foundational constraint that appears in multiple domain-specific instantiations (arms races, tragedy of the commons, market races to the bottom). Each domain story should link to this abstract constraint via network.affects_constraints to show how the general coordination failure manifests in specific contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
