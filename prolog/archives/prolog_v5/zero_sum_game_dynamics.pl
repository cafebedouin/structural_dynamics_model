% ============================================================================
% CONSTRAINT STORY: zero_sum_game_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_sum_game_dynamics, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zero_sum_game_dynamics
 *   human_readable: Zero Sum Game Dynamics
 *   domain: general/game_theory/behavioral_economics
 *
 * SUMMARY:
 *   Zero-sum game dynamics represents a structural trap where agents locked
 *   into competitive framing fail to recognize or access positive-sum
 *   solutions. The constraint is both mathematical (payoff matrices can be
 *   objectively zero-sum) and cognitive (agents often treat non-zero-sum
 *   games as zero-sum through incomplete information or identity commitment
 *   to competitive roles). This creates a multi-layer extraction mechanism:
 *   relative-gain maximizers capture surplus from absolute-gain seekers,
 *   institutional redesigners benefit from mediating the transition to
 *   cooperation, and the zero-sum narrative itself suppresses recognition
 *   that alternative equilibria exist. The constraint's extractiveness has
 *   increased over the measured interval (0.45 → 0.68) as competitive
 *   escalation layers additional zero-sum logic onto initially mixed-motive
 *   problems. Theater ratio (0.55) reflects that much institutional
 *   competition is performative rather than functionally necessary — firms,
 *   states, and individuals maintain competitive postures that exceed
 *   payoff-maximizing levels, signaling commitment to competitive identity.
 *
 * KEY AGENTS:
 *   - Cooperative Agents: Primary victims (powerless/trapped) — cannot achieve mutual benefit despite possibility; rational defectors exploit their cooperation attempt
 *   - Rational Defectors: Secondary victims (moderate/constrained) — benefit from relative gains but trapped in mutual defection equilibrium with suboptimal absolute payoffs
 *   - Relative Gain Maximizers: Primary beneficiaries (institutional/arbitrage) — extract surplus from the zero-sum framing itself, capturing positive returns while others focus on denying opponent gains
 *   - Institutional Redesigners: Organized beneficiaries (organized/mobile) — game designers, contract enforcers, norm-setters who benefit from mediating transitions to positive-sum games
 *   - Evolutionary Game Theorists: Analytical observers (institutional/arbitrage) — see full solution space and recognize that cooperation equilibria are mathematically available
 *   - Collective Welfare: Abstract victim (powerless/trapped) — bears the cost of mutual defection and lost cooperation gains with no exit or advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_sum_game_dynamics, 0.68).
domain_priors:suppression_score(zero_sum_game_dynamics, 0.72).
domain_priors:theater_ratio(zero_sum_game_dynamics, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_sum_game_dynamics, extractiveness, 0.68).
narrative_ontology:constraint_metric(zero_sum_game_dynamics, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zero_sum_game_dynamics, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_sum_game_dynamics, snare).
narrative_ontology:human_readable(zero_sum_game_dynamics, "Zero Sum Game Dynamics").
narrative_ontology:topic_domain(zero_sum_game_dynamics, "general/game_theory/behavioral_economics").

domain_priors:requires_active_enforcement(zero_sum_game_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_sum_game_dynamics, relative_gain_maximizers).
narrative_ontology:constraint_victim(zero_sum_game_dynamics, absolute_gain_seekers).
narrative_ontology:constraint_victim(zero_sum_game_dynamics, cooperative_agents).
narrative_ontology:constraint_victim(zero_sum_game_dynamics, collective_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COOPERATIVE AGENT (SNARE) — Trapped in zero-sum framing; cannot achieve mutual benefit despite structural possibility. Every defection by others confirms the zero-sum narrative, making cooperation appear naive. The agent bears full extraction: rational defectors capture surplus while cooperation generates nothing. No exit because the game structure itself prevents unilateral withdrawal.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RATIONAL DEFECTOR (SNARE) — Constrained by mutual defection outcome (suboptimal absolute payoff) but benefits from relative gain dominance over cooperators. Remains trapped in escalating defection cycles despite mutual loss. Suppression prevents recognition that the game structure itself is the constraint, not opponent strategy.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL REDESIGN (TANGLED ROPE) — Organized agents (game designers, contract enforcers, norm-setters) benefit from the zero-sum framing (it justifies their mediating role and enforcement infrastructure) while coordinating moves toward positive-sum games through institutional redesign. Active enforcement of new game rules required. Genuine coordination function (establishing Pareto-improving rules) alongside asymmetric extraction (designers capture policy influence).
constraint_indexing:constraint_classification(zero_sum_game_dynamics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EVOLUTIONARY GAME THEORIST (ROPE) — Institutional analyst with arbitrage exit (can publish, move between domains, theorize without enforcement). Sees zero-sum dynamics as a coordination problem: multiple equilibria exist, including cooperative ones. No meaningful extraction from this perspective — the theorist benefits from intellectual access to the full solution space.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TEMPORARY COOPERATION MECHANISM (SCAFFOLD) — Programs, contracts, or norms that enforce cooperation for limited duration (trade agreements, prisoner's dilemma tournaments, team-based incentives). High suppression during enforcement period but designed to sunset as internalized cooperation norms mature. Genuine coordination function with explicit sunset clause.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MATHEMATICAL INEVITABILITY (MOUNTAIN CLAIM) — Zero-sum game structure is often presented as an immutable mathematical property: one agent's gain is literally another's loss by definition. This perspective risks confusing the mathematical definition of a zero-sum game with the claim that all real strategic interactions ARE zero-sum. The engine will detect this as a false summit — most real-world strategic problems have positive-sum solutions that the zero-sum framing obscures.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: RITUALIZED COMPETITIVE FRAMING (PITON) — The zero-sum narrative persists in institutional contexts (corporate competition, international relations, sports) through inertia rather than structural necessity. Theater ratio (0.55) reflects that much competitive posturing is performative — genuinely positive-sum collaboration is available but suppressed by reputation costs and identity commitments to competitive roles. The ritual maintains itself through institutional identity, not functional necessity.
constraint_indexing:constraint_classification(zero_sum_game_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_sum_game_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zero_sum_game_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zero_sum_game_dynamics, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_sum_game_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(zero_sum_game_dynamics, TR),
    TR >= 0.70.

:- end_tests(zero_sum_game_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The zero-sum framing extracts value from absolute-gain seekers by forcing them into relative-gain competition where mutual defection dominates. The measured increase (0.45 → 0.68) reflects escalating competitive commitment and lock-in effects. The extraction is structural (payoff matrix) not purely performative. Suppression (0.72): High. Multiple barriers prevent agents from recognizing or accessing positive-sum solutions: incomplete information about payoff structure, identity investment in competitive roles, fear that unilateral cooperation will be exploited, institutional inertia favoring competitive norms. Theater ratio (0.55): Moderate. Some competitive behavior (posturing, reputation signaling, identity maintenance) is performative and could be reduced through norm change. But the underlying payoff structure forces genuine strategic defection in zero-sum contexts. Theater represents the cognitive/identity layer on top of the structural game.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon (zero-sum game payoff matrix) generates radically different classifications from different positions. The cooperative victim sees extraction as inescapable. The defector sees the same game but experiences relative gain dominance (which feels like winning despite mutual loss). The institutional designer sees the game as a coordination problem with a redesign solution. The theorist sees multiple equilibria coexist mathematically. The false-mountain perspective naturalizes what is often a cognitive framing choice. The piton perspective reveals that institutional competitive posturing exceeds payoff optimization. No single classification is 'correct' — they are all accurate readings from their respective structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Cooperative agents with trapped exit face maximum d (0.95 → f(d) ≈ 1.42). Defectors with constrained exit but relative-gain benefit face moderate d (0.65 → f(d) ≈ 1.00). Institutional designers with arbitrage exit and beneficiary status face low d (0.15 → f(d) ≈ -0.01). Game theorists with analytical exit face canonical d (0.73 → f(d) ≈ 1.15). The engine derives these values from beneficiary/victim declarations and exit options; the perspectival gap (snare vs rope vs mountain) emerges from the different d values, not from disagreement about ε.
 *
 * MANDATROPHY ANALYSIS:
 *   Zero-sum game dynamics resolves the mandatrophy by showing that the constraint has genuine snare properties (mutual defection trap, suppression of cooperation recognition) alongside genuine rope properties (the game is mathematically solvable through institutional redesign). The mandate is resolved by specifying the CONTEXT-DEPENDENT classification: in contexts where payoff matrices are truly zero-sum AND agents are locked into competitive identity, the snare classification is correct. In contexts where positive-sum solutions exist but are obscured by framing or incomplete information, the rope classification (pure coordination problem) is correct, and the snare is a false summit created by cognitive capture. The mandatrophy is not 'which is it really?' but 'for which agent in which game structure?' The constraint family should be decomposed: one story for true zero-sum games (snare), another for positive-sum games misframed as zero-sum (rope with cognitive capture), and a third for institutional mechanisms that shift between them (tangled rope with sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payoff_structure_observable_dependence,
    'Is a given strategic interaction truly zero-sum, or does the payoff structure depend on how outcomes are measured?',
    'Explicit payoff matrix specification with sensitivity analysis across measurement frames (absolute vs relative gains, monetary vs status outcomes, individual vs collective welfare)',
    'If payoff structure is measurable-frame-dependent: the constraint is observation-relative, suggesting decomposition into multiple stories per the ε-invariance principle. If payoff structure is frame-invariant: the mathematical zero-sum property is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_structure_observable_dependence, empirical, 'Whether zero-sum property depends on payoff measurement frame').

omega_variable(
    relative_vs_absolute_gain_primacy,
    'Do agents actually maximize relative gains (zero-sum logic), or do they maximize absolute gains with relative comparison as a secondary constraint?',
    'Behavioral experiments with explicit payoff conditions; decision analysis of real strategic choices when absolute and relative gains conflict',
    'If agents prioritize absolute gains: zero-sum framing is cognitive capture (identity_locked), not structural. The constraint''s extractiveness is the cost of the false framing, not an inherent game property. If agents prioritize relative gains: the zero-sum dynamic is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_vs_absolute_gain_primacy, empirical, 'Whether agents prioritize relative or absolute gains').

omega_variable(
    institutional_escape_velocity,
    'What institutional investment (contract strength, norm enforcement, repeated-game structure) is sufficient to break zero-sum dynamics and sustain cooperation?',
    'Longitudinal analysis of cooperation rates as institutional support increases; identification of tipping points and stability domains',
    'If escape velocity is low: scaffold sunset is realistic and cooperation can be internalized. If escape velocity is high or infinite: zero-sum logic may be locked in by deeper evolutionary or cognitive structures, making the snare classification permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_escape_velocity, empirical, 'Institutional threshold for sustainable cooperation').

omega_variable(
    cognitive_capture_by_game_framing,
    'Does the zero-sum game label itself suppress recognition of positive-sum solutions? Is the binding mechanism structural (payoff matrix) or cognitive (identity-lock to competitive narrative)?',
    'Comparison of strategic choices under identical payoff conditions with vs without zero-sum framing; analysis of when agents discover cooperative solutions',
    'If framing is primary: many agents are identity_locked rather than structurally trapped. The constraint''s suppression is cognitive rather than material. If payoff structure is primary: cognitive capture is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capture_by_game_framing, conceptual, 'Whether suppression is structural or cognitive-narrative-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_sum_game_dynamics, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zsg_tr_t0, zero_sum_game_dynamics, theater_ratio, 0, 0.42).
narrative_ontology:measurement(zsg_tr_t3, zero_sum_game_dynamics, theater_ratio, 3, 0.48).
narrative_ontology:measurement(zsg_tr_t6, zero_sum_game_dynamics, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(zsg_be_t0, zero_sum_game_dynamics, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(zsg_be_t3, zero_sum_game_dynamics, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(zsg_be_t6, zero_sum_game_dynamics, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_sum_game_dynamics, resource_allocation).
narrative_ontology:affects_constraint(zero_sum_game_dynamics, prisoner_dilemma_dynamics).
narrative_ontology:affects_constraint(zero_sum_game_dynamics, competitive_escalation_trap).
narrative_ontology:affects_constraint(zero_sum_game_dynamics, institutional_redesign_capture).

% DUAL FORMULATION NOTE:
% Zero-sum game dynamics as a single story conflates mathematical zero-sum games with cognitive zero-sum framing. A complete analysis requires decomposition: (1) truly_zero_sum_games (ε=0.72, snare) for contexts where payoff matrices are objectively zero-sum; (2) positive_sum_misframing (ε=0.55, tangled_rope) for contexts where positive-sum solutions exist but competitive framing suppresses them; (3) institutional_game_shifting (ε=0.45, scaffold) for mechanisms that transition games from zero-sum to cooperative equilibria. This story represents the family's trunk; specifics in each domain require domain-specific decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_sum_game_dynamics, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
