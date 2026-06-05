% ============================================================================
% CONSTRAINT STORY: nash_equilibrium_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nash_equilibrium_coordination, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nash_equilibrium_coordination
 *   human_readable: Nash Equilibrium as Coordination Mechanism
 *   domain: economic/social
 *
 * SUMMARY:
 *   Nash equilibrium is a coordinate system for understanding strategic
 *   interaction where no agent can improve their outcome by unilateral
 *   strategy change, given the strategies of others. The constraint operates
 *   across multiple levels: as a mathematical inevitability (mountain from
 *   the analyst's view), as a pure coordination mechanism (rope for
 *   individual agents optimizing within constraints), as a tool for
 *   institutional design (tangled rope for powerful game designers), as a
 *   trap mechanism (snare for agents locked in Pareto-inferior equilibria),
 *   as a target for collective escape (scaffold for organized coalitions),
 *   and as a vestigial concept (piton in complex real-world systems where the
 *   assumptions break down). The core analytical challenge is that 'Nash
 *   equilibrium' is not a single constraint but a family of structurally
 *   distinct phenomena depending on which properties are foregrounded: the
 *   existence theorem (mountain), the coordination function (rope), the
 *   designer's tool (tangled rope), the Pareto-inferior trap (snare), or the
 *   post-hoc rationalization (piton). The measurable trend over 100 years
 *   since Nash's 1950 theorem shows theater ratio increasing (0.20 → 0.40) as
 *   applications to complex systems with violated assumptions have
 *   proliferated, while base extractiveness remains moderate (0.25 → 0.32)
 *   because the concept serves both coordination and extraction functions.
 *
 * KEY AGENTS:
 *   - Rational Individual Agents: Primary beneficiary (powerless/mobile) — experience Nash equilibrium as coordinating mechanism that solves their strategic problem optimally given others' moves
 *   - Mathematical Community: Primary beneficiary (institutional/arbitrage) — established theorems, proof techniques, academic field around equilibrium concept; benefits from explanatory authority
 *   - Game Designers: Secondary beneficiary/architect (powerful/arbitrage) — market designers, mechanism designers, organizational architects who use equilibrium concept to engineer specific outcomes
 *   - Trapped Agents in Suboptimal Equilibria: Primary victim (moderate/trapped) — locked in Pareto-inferior equilibria (arms races, commons tragedies, wage races) with no unilateral escape
 *   - Organized Coalitions: Secondary victim attempting escape (organized/constrained) — unions, trade associations, regulatory coalitions seeking binding coordination to shift equilibrium selection
 *   - Applied Analysts: Institutional observer (institutional/mobile) — economists, strategists, policy makers who invoke Nash equilibrium as post-hoc explanation in complex systems where assumptions fail
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nash_equilibrium_coordination, 0.32).
domain_priors:suppression_score(nash_equilibrium_coordination, 0.28).
domain_priors:theater_ratio(nash_equilibrium_coordination, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nash_equilibrium_coordination, extractiveness, 0.32).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nash_equilibrium_coordination, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nash_equilibrium_coordination, rope).
narrative_ontology:human_readable(nash_equilibrium_coordination, "Nash Equilibrium as Coordination Mechanism").
narrative_ontology:topic_domain(nash_equilibrium_coordination, "economic/social").

domain_priors:emerges_naturally(nash_equilibrium_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nash_equilibrium_coordination, rational_agents).
narrative_ontology:constraint_beneficiary(nash_equilibrium_coordination, game_structure_designers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RATIONAL AGENT (ROPE) — Agents within a Nash equilibrium experience it as pure coordination. No agent benefits from unilateral deviation; each agent's strategy is optimal given others' strategies. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.17. Low extraction because the agent is already optimized relative to available moves.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: MATHEMATICAL ANALYST (MOUNTAIN) — Nash equilibrium is a mathematical inevitability for finite games with rational agents. The theorem (Nash 1950) proves existence for mixed-strategy equilibria in all finite non-cooperative games. ε≈0.08, emergence is mathematical necessity, not contingent institutional arrangement. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. This is a true mountain: the constraint emerges from logical structure of preference and rationality.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: STRATEGIC GAME DESIGNER (TANGLED ROPE) — Designers of economic or institutional games (market structure, voting rules, auction mechanisms, organizational incentives) use Nash equilibrium to engineer specific outcomes. They benefit from equilibria that extract rent or consolidate power while maintaining the appearance of rational coordination. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.09. The equilibrium provides both genuine coordination (agents remain in place) and hidden asymmetry (designer chose which equilibrium to instantiate among multiple possible ones).
constraint_indexing:constraint_classification(nash_equilibrium_coordination, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRAPPED IN SUBOPTIMAL EQUILIBRIUM (SNARE) — Many games have multiple Nash equilibria, and agents can be trapped in Pareto-inferior equilibria (e.g., arms races, pollution commons, wage races). Each agent is locally optimal (no unilateral deviation improves their payoff) but collectively worse off. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.37. High extraction because agents cannot escape the Pareto-inferior trap without coordinated simultaneous deviation (which non-cooperative games forbid).
constraint_indexing:constraint_classification(nash_equilibrium_coordination, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: COALITION SEEKING COOPERATIVE ESCAPE (SCAFFOLD) — Organized agents (unions, trade associations, regulatory coalitions) attempt to coordinate deviations from suboptimal Nash equilibria by binding themselves to higher-payoff correlated equilibria or cooperative solutions (e.g., cartel agreements, labor contracts, regulatory standards). χ≤0.30 if the coalition has real enforcement power; theater≤0.70 if the solution is functionally durable. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.14. The constraint (suboptimal equilibrium trap) has a sunset: binding commitment mechanisms can shift the game structure itself.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL VESTIGIAL USE (PITON) — In complex social systems (geopolitics, organizational hierarchies, financial markets), the Nash equilibrium concept persists as explanatory framework even where the preconditions (finite games, rational agents, common knowledge of payoffs) are severely violated. The concept is maintained through academic inertia and provides theater for explaining what are actually path-dependent institutional arrangements. theater_ratio≈0.75, ε≈0.12. d≈0.20, f(d)≈0.05, σ=1.0 → χ≈0.01. The constraint is mostly drained of explanatory power.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nash_equilibrium_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nash_equilibrium_coordination, TR),
    TR >= 0.70.

:- end_tests(nash_equilibrium_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The Nash equilibrium concept functions primarily as a coordination mechanism — agents are already optimizing given others' strategies, so effective extraction from the mechanism itself is low. However, when multiple equilibria exist, designers can extract by choosing which equilibrium to instantiate. The moderate value reflects that some extraction is possible through equilibrium selection, but most contexts exhibit pure coordination. Suppression (0.28): Low-moderate. Rational agents in well-defined games are not suppressed — they are solving their optimization problem. Suppression arises only in Pareto-inferior equilibria where agents cannot unilaterally deviate. Theater ratio (0.35): Moderate. In finite, well-defined games with common knowledge (rare in reality), Nash equilibrium is a genuine mathematical tool, not theater. In complex systems with violated assumptions, invoking Nash equilibrium functions as theoretical theater — post-hoc rationalization of institutional arrangements. The trend toward higher theater (0.20 → 0.40) reflects application domain expansion into complexity where the model assumptions fail.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival heterogeneity. From the individual agent's perspective (mobile, powerless), Nash equilibrium is pure coordination — they are optimized and cannot improve unilaterally (Rope). From the mathematician's perspective (analytical, universal), Nash equilibrium is a mathematical mountain — existence is proven for all finite games, emergence is logical necessity, not contingent. From the game designer's perspective (powerful, arbitrage), Nash equilibrium is a tool (Tangled Rope) — they benefit from selecting which equilibrium is played while maintaining the fiction of natural coordination. From the Pareto-inferior trapped agent's perspective (moderate, trapped), Nash equilibrium is a snare — they are locally optimal but globally worse off, unable to escape without coordinated deviation. From the organized coalition's perspective (organized, constrained), Nash equilibrium is a target structure that can be escaped through binding commitment (Scaffold). From the institutional observer's perspective (institutional, mobile), Nash equilibrium in complex real systems is largely post-hoc theater (Piton) — the mathematical assumptions are violated but the concept persists through academic inertia. No single perspective is false; each reveals a genuine structural property of the equilibrium concept.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual rational agents: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Symmetric — agents benefit from being in equilibrium (no profitable deviation) but are not targeted. Mathematical analyst: Analytical → d≈0.72, canonical fallback (mountain). Game designer: Beneficiary + arbitrage → d≈0.35, derived from powerful/arbitrage selection capability. Low d because designer benefits from equilibrium concept (can extract through selection); moderate because designer does not face direct coercion. Trapped agent: Victim + trapped → d≈0.92, f(d)≈1.40. High d because agent is locked in and cannot improve unilaterally; benefits flow upward to designer/institutional structure. Organized coalition: Organized + constrained → d≈0.45, f(d)≈0.45. Moderate because coalition has real agency but faces constraints (coordination difficulty, enforcement fragility). Applied analyst: Institutional + mobile → d≈0.20, derived from institutional power and mobile exit (can change analytical framework). Low d because analyst has authority and flexibility; not directly targeted by the concept.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Nash equilibrium is not primarily extractive but coordinative, with extraction emerging only through equilibrium selection when multiple equilibria exist. The primary function is coordination (agents optimizing given others' strategies), which explains why the constraint classifies as Rope for individual agents and Mountain for the analyst. Extraction (Snare perspective, Tangled Rope designer perspective) arises contingently when (1) multiple equilibria exist and (2) a powerful designer selects among them. The Scaffold perspective (organized escape) confirms that the Pareto-inferior trap is escapable through collective binding — the constraint is not immutable. The Piton perspective warns against false naturalization: invoking Nash equilibrium in complex systems where rationality and common knowledge fail is institutional theater, not genuine application of the theorem. The mandatrophy is fully resolved: Nash equilibrium IS a coordination mechanism (mountain + rope perspectives are valid), but deployment of the concept to extract or rationalize extraction is contingent on institutional context (designer power, equilibrium multiplicity, assumption violations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    multiple_equilibria_selection,
    'When multiple Nash equilibria exist, what mechanism selects which equilibrium will be played?',
    'Empirical game experiments; study of focal points (Schelling points), historical precedent, communication pre-play; analysis of refinements (perfect equilibrium, proper equilibrium) in real strategic situations',
    'If selection is exogenous (designer chooses): Nash equilibrium is a tool for extraction engineering (Tangled Rope or Snare from designer''s perspective). If selection is endogenous (agents coordinate on focal point): Nash equilibrium is pure coordination (Rope). If selection is path-dependent and opaque: Nash equilibrium is a piton (post-hoc rationalization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multiple_equilibria_selection, empirical, 'Mechanism for selecting among multiple equilibria').

omega_variable(
    rationality_assumption_validity,
    'Do actual agents in economic/social systems satisfy the rationality assumptions (expected utility maximization, belief formation, perfect foresight) required for Nash equilibrium prediction?',
    'Behavioral game experiments; systematic deviation studies; cognitive load analysis; comparison of predicted vs observed play in diverse strategic settings',
    'If rationality holds: Nash equilibrium is a valid mountain (mathematical inevitability of rational play). If rationality fails systematically: Nash equilibrium concept is a false summit (naturalized contingent behavior). Deviations become the primary phenomenon, and Nash equilibrium is post-hoc narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_assumption_validity, empirical, 'Whether actual agents exhibit required rationality').

omega_variable(
    common_knowledge_requirement,
    'Is common knowledge of the game structure, payoffs, and rationality achievable or sustainable in real strategic situations?',
    'Information asymmetry studies; incomplete information game analysis; learning dynamics in repeated games; misaligned expectations studies',
    'If common knowledge is typical: Nash equilibrium is robust mountain. If common knowledge is rare or breaks under learning: Nash equilibrium concept is aspirational (Scaffold or Piton). The constraint becomes ''agents trying to coordinate despite uncertainty'' rather than ''inevitable equilibrium outcome.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_knowledge_requirement, empirical, 'Achievability of common knowledge in real strategic settings').

omega_variable(
    pareto_efficiency_alignment,
    'In how many real strategic contexts do Nash equilibria coincide with Pareto-efficient outcomes? How frequently is the constraint Pareto-inferior?',
    'Systematic survey of Nash equilibria across market types, organizational games, and geopolitical scenarios; calculation of efficiency gaps; correlation with extractive outcomes',
    'If alignment is frequent: Nash equilibrium is benign (Rope). If misalignment is frequent: Nash equilibrium is often a trap mechanism (Snare or Tangled Rope). The answer determines whether the constraint is coordination or extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pareto_efficiency_alignment, empirical, 'Frequency of Pareto efficiency in Nash equilibria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nash_equilibrium_coordination, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nash_tr_t0, nash_equilibrium_coordination, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nash_tr_t50, nash_equilibrium_coordination, theater_ratio, 50, 0.35).
narrative_ontology:measurement(nash_tr_t100, nash_equilibrium_coordination, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(nash_be_t0, nash_equilibrium_coordination, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(nash_be_t50, nash_equilibrium_coordination, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(nash_be_t100, nash_equilibrium_coordination, base_extractiveness, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nash_equilibrium_coordination, information_standard).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, game_theory_foundations).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, equilibrium_selection_problem).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, mechanism_design_rent_extraction).
narrative_ontology:affects_constraint(nash_equilibrium_coordination, commons_tragedy_coordination).

% DUAL FORMULATION NOTE:
% Nash equilibrium as a mathematical theorem (mountain, ε≈0.08) is distinct from Nash equilibrium as an applied explanatory concept in complex systems (piton, ε≈0.35). The theorem is downstream — it establishes the existence claim. Applications in market design, organizational behavior, and geopolitics are downstream of the theorem but operate with violated assumptions, shifting the constraint into tangled rope / snare territory. This story focuses on the applied concept (ε=0.32, Rope); separate constraint stories could decompose the pure mathematical claim and specific application contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nash_equilibrium_coordination, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
