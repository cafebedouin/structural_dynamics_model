% ============================================================================
% CONSTRAINT STORY: gacha_game_mechanics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gacha_game_mechanics, []).

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
 *   constraint_id: gacha_game_mechanics
 *   human_readable: Gacha Game Monetization Mechanics
 *   domain: digital_entertainment/behavioral_economics
 *
 * SUMMARY:
 *   Gacha game mechanics represent a structural constraint that converts
 *   player engagement and social coordination into behavioral extraction via
 *   variable ratio reinforcement scheduling. The constraint operates across
 *   global digital entertainment and targets vulnerable populations through
 *   intentional psychological manipulation. Gacha mechanics (randomized
 *   reward systems with monetary escalation) have evolved over two decades
 *   from cosmetic monetization into primary revenue extraction mechanisms
 *   that often gate gameplay power and progression. The underlying behavioral
 *   mechanism — intermittent reinforcement — is borrowed directly from
 *   behavioral psychology literature on operant conditioning. The constraint
 *   exhibits the full spectrum of DR classifications depending on observer
 *   position: publishers experience coordination (funding sustainable games),
 *   casual players experience mixed coordination and extraction (tangled
 *   rope), compulsive spenders experience pure extraction with behavioral
 *   lock-in (snare), regulatory actors see a solvable temporary problem
 *   (scaffold), and the cosmetic-justification narrative persists through
 *   institutional inertia (piton). The analytically coherent view recognizes
 *   that variable ratio scheduling has no genuine coordination benefit — it
 *   is behavioral extraction, and the game coordination (multiplayer, social
 *   features) is orthogonal to the gacha mechanism itself.
 *
 * KEY AGENTS:
 *   - Compulsive Spenders: Primary victims (powerless/trapped) — locked into variable ratio reinforcement cycles; cannot exit without social isolation and sunk-cost abandonment
 *   - Casual Players: Secondary victims (moderate/constrained) — face coordination benefit (social play) alongside extraction pressure (FOMO, cosmetic gatekeeping, event urgency)
 *   - Vulnerable Populations: Tertiary victims (powerless/trapped) — disproportionately targeted by time-gated mechanics, circadian-aligned events, and social-proof dark patterns
 *   - Game Publisher: Primary beneficiary (institutional/arbitrage) — captures revenue from behavioral extraction; can shift monetization model but benefits from high-extraction regime
 *   - Game Developer Team: Secondary beneficiary — dependent on publisher revenue; may not directly benefit from extraction but team sustainability is coupled to high monetization pressure
 *   - Regulatory Coalition: Organized agents (organized/constrained) — loot box regulation advocates, consumer protection agencies, age-rating boards building alternative pathways through transparency and caps
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes gacha as pure behavioral extraction mechanisms beneath coordination framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gacha_game_mechanics, 0.68).
domain_priors:suppression_score(gacha_game_mechanics, 0.72).
domain_priors:theater_ratio(gacha_game_mechanics, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gacha_game_mechanics, extractiveness, 0.68).
narrative_ontology:constraint_metric(gacha_game_mechanics, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gacha_game_mechanics, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gacha_game_mechanics, snare).
narrative_ontology:human_readable(gacha_game_mechanics, "Gacha Game Monetization Mechanics").
narrative_ontology:topic_domain(gacha_game_mechanics, "digital_entertainment/behavioral_economics").

domain_priors:requires_active_enforcement(gacha_game_mechanics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gacha_game_mechanics, game_publisher).
narrative_ontology:constraint_beneficiary(gacha_game_mechanics, gacha_system_operator).
narrative_ontology:constraint_victim(gacha_game_mechanics, vulnerable_player_populations).
narrative_ontology:constraint_victim(gacha_game_mechanics, player_financial_autonomy).
narrative_ontology:constraint_victim(gacha_game_mechanics, game_ecosystem_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPULSIVE SPENDER (SNARE) — Player locked into spending cycle through variable ratio reinforcement schedule. Lacks information about odds, cannot exit without abandoning social bonds with other players, faces intermittent reinforcement that sustains pull-spending behavior. Experiences maximal extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(gacha_game_mechanics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CASUAL PLAYER (TANGLED ROPE) — Faces both genuine coordination benefit (social play, cosmetic progression) and asymmetric extraction (monetization pressure, FOMO mechanics). Can exit by switching games but faces sunk-cost barriers and social penalty. Moderate experienced extraction alongside real coordination function.
constraint_indexing:constraint_classification(gacha_game_mechanics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GAME PUBLISHER (ROPE) — Experiences gacha mechanics as pure coordination: enabling player connection, cosmetic differentiation, and sustainable development funding. Net beneficiary with exit capacity (can shift monetization models). Extraction flows toward this agent through variable-ratio scheduling.
constraint_indexing:constraint_classification(gacha_game_mechanics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized actors (loot box regulation, age-gating proposals, odds disclosure mandates) see gacha mechanics as a temporary coordination failure with sunset logic. Loot box transparency laws, spending caps, and odds-disclosure requirements create enforcement pathways. Suppression declines as regulation matures.
constraint_indexing:constraint_classification(gacha_game_mechanics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COSMETIC JUSTIFICATION (PITON) — Industry claims that gacha mechanics fund cosmetics and optional content ('cosmetics-only' frame) persist through institutional inertia despite evidence that monetary extraction is primary functional goal. Theater persists because regulation hasn't fully replaced it; the cosmetic frame is maintained not because it accurately describes the system but because alternatives haven't solidified.
constraint_indexing:constraint_classification(gacha_game_mechanics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, gacha mechanics are pure extraction dressed in coordination language. Variable ratio reinforcement (the underlying psychological mechanism) has no coordination benefit — it is pure behavioral capture. The game coordination (multiplayer, social features) is orthogonal to gacha monetization; games can coordinate socially without gacha. This perspective sees the constraint as structural behavioral extraction at global scale.
constraint_indexing:constraint_classification(gacha_game_mechanics, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gacha_game_mechanics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gacha_game_mechanics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gacha_game_mechanics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gacha_game_mechanics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gacha_game_mechanics, TR),
    TR >= 0.70.

:- end_tests(gacha_game_mechanics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The base measurement reflects that gacha mechanics have evolved specifically to maximize spending from willing and unwilling players. The 2009-2026 interval shows steady escalation: early games (2009) used gacha for optional cosmetics (ε ≈ 0.35); by 2015 power gating emerged (ε ≈ 0.52); by 2026 gacha is primary revenue stream with aggressive FOMO design (ε ≈ 0.68). The measurement trajectory reveals intentional optimization for extraction. Suppression (0.72): Very high. Multiple suppression mechanisms operate: (1) information suppression — odds hidden or poorly disclosed, probability of competitive viability unclear; (2) cognitive suppression — variable ratio schedule creates behavioral lock that persists despite explicit odds knowledge; (3) social suppression — leaving the game means abandoning friendships and guild identity; (4) financial suppression — sunk cosmetics and time investment create exit costs. Theater ratio (0.65): Moderate-high. Industry narrative frames gacha as 'optional cosmetics for sustainable development' despite evidence that power-granting gacha is primary revenue. This frame persists because: (a) regulatory pressure hasn't fully replaced it with disclosure requirements, (b) publishers actively maintain the narrative, (c) game design obscures what is actually gated behind gacha. The theater has increased over the interval as regulatory pressure mounted and publishers refined the cosmetic justification language.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Publishers see coordination (rope) — gacha funds sustainable games and enables cosmetic expression. Casual players see mixed benefit and extraction (tangled rope) — social coordination is real but monetization pressure intrudes. Compulsive spenders see pure extraction (snare) — behavioral lock-in with no perceived coordination benefit. Regulatory actors see a solvable problem (scaffold) — odds disclosure, spending caps, age-gating can reduce extraction over a generational timeline. The cosmetic-justification narrative sees its own degradation (piton) — persists through institutional inertia as regulation and consumer awareness increase. The analytical observer sees behavioral extraction at civilizational scale (snare) — variable ratio reinforcement is not a coordination mechanism; it is a behavioral capture mechanism that extracts value through psychological manipulation. The gap between publisher's rope and victim's snare reveals the directional asymmetry of the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Compulsive spenders: powerless + trapped exit = high d → maximum f(d) → maximum experienced extractiveness (χ ≈ 0.85). Casual players: moderate power + constrained exit = moderate d → moderate f(d) → moderate experienced extraction (χ ≈ 0.55). Publishers: institutional power + arbitrage exit = low d (beneficiary status) → negative f(d) → negative/low experienced extraction (χ ≈ -0.10). The automatic derivation from beneficiary/victim status plus exit options produces these relationships. The snare classification for both compulsive spenders and analytical observer reflects that the structural mechanism (variable ratio reinforcement) is extraction-only with no genuine coordination benefit. Gacha mechanics could be removed without affecting multiplayer coordination or social features — the coordination function exists independently of the extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION MANDATROPHY RESOLUTION: The constraint qualifies as snare (ε=0.68 > 0.46, suppression=0.72 > 0.60, χ ≥ 0.66 for victims) and the mandatrophy is resolved by recognizing that gacha mechanics are behavioral extraction mechanisms without genuine coordination benefit. The industry's rope-framing ('optional cosmetics funding sustainability') is a false summit — games can and do fund development through battle passes, cosmetic-only monetization, or buy-once models without gacha. The snare classification prevents mislabeling gacha as pure coordination (rope) by requiring explicit evidence that: (1) removal of gacha would not disable the game's coordination function (it wouldn't — multiplayer and social features are orthogonal), (2) the extraction mechanism is behavioral rather than economic (variable ratio scheduling is behavioral, not voluntary exchange), and (3) alternatives exist but are suppressed by publisher preference for high extraction (they exist but publishers adopt gacha specifically for higher revenue). The mandatrophy resolves by showing that 'both publishers and players benefit from gacha' (false rope reading) conflates legitimate coordination with behavioral extraction. Publishers benefit; players at compulsive spending levels experience pure extraction. Casual players experience mixed, which is tangled rope for them but snare for vulnerable populations. The classification prevents the false consensus that gacha is coordination by requiring victim testimony and exit option measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmetic_vs_power_boundary,
    'What proportion of gacha monetization funds cosmetics (genuine non-extractive) vs gameplay power (extractive)?',
    'Revenue attribution analysis: track spending distribution across cosmetic-only vs power-granting gacha pools; player survey on perceived power impact',
    'If cosmetic-heavy (>70%): constraint is tangled_rope (coordination + moderate extraction). If power-heavy (<30% cosmetic): constraint is snare (pure extraction). Most games fall in the middle, but the ratio determines whether ''optional'' framing is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmetic_vs_power_boundary, empirical, 'Proportion of monetization that is cosmetic vs power-granting').

omega_variable(
    player_numeracy_and_odds_transparency,
    'Does odds disclosure (when required by regulation) actually reduce spending, or does it have minimal effect on behavioral extraction?',
    'Comparison of spending patterns before/after odds disclosure mandates (Korea, China, Japan regulatory data); correlation between player odds-awareness and spending behavior',
    'If disclosure reduces spending significantly: suppression is primarily informational (can be reduced by transparency). If minimal effect: suppression is behavioral (variable ratio schedule persists regardless of odds knowledge), indicating deeper extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(player_numeracy_and_odds_transparency, empirical, 'Effect of odds disclosure on player spending behavior').

omega_variable(
    alternative_monetization_viability,
    'Can games fund ongoing development and studio sustainability with non-gacha monetization (battle pass only, cosmetic cosmetics only, pay-once)?',
    'Financial analysis of gacha-free or low-gacha games (Helldivers 2, Path of Exile, Satisfactory model); comparison of revenue per player and per-developer payroll sustainability',
    'If alternative models are viable: gacha''s high extractiveness is not necessary for coordination (games can socially coordinate without behavioral extraction). If alternatives fail: gacha represents genuine tradeoff between extraction and development sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_monetization_viability, empirical, 'Whether non-gacha monetization can sustainably fund game development').

omega_variable(
    vulnerable_population_identification,
    'What demographic and psychological profiles correlate with gacha spending above sustainable levels? Are specific player populations disproportionately targeted?',
    'Spending distribution analysis by age/income/psychological trait; analysis of FOMO mechanics and event-structure timing targeting specific circadian/work patterns',
    'If vulnerable populations are disproportionately targeted: suppression is intentional and mechanism is predatory (snare classification strengthened). If distribution is uniform: mechanism is broadly-acting behavioral extraction without targeting (still snare but less predatory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_identification, empirical, 'Demographic/psychological vulnerability to gacha mechanics').

omega_variable(
    regulatory_effectiveness_and_ceiling,
    'Can regulation (spending caps, odds disclosure, age-gating, loot box bans) meaningfully reduce extraction, or does it reach a point of diminishing returns?',
    'Longitudinal tracking of regulated jurisdictions (Belgium, Netherlands, Korea, China) versus unregulated ones; measurement of player spending post-regulation and publisher adaptation strategies',
    'If regulation achieves extraction floor reduction: scaffold sunset is credible and suppression can decline significantly with regulatory maturity. If publishers successfully route around regulation: extraction persists and scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_and_ceiling, empirical, 'Effectiveness ceiling of regulatory approaches to gacha mechanics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gacha_game_mechanics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gacha_tr_t0, gacha_game_mechanics, theater_ratio, 0, 0.4).
narrative_ontology:measurement(gacha_tr_t5, gacha_game_mechanics, theater_ratio, 5, 0.52).
narrative_ontology:measurement(gacha_tr_t10, gacha_game_mechanics, theater_ratio, 10, 0.65).
narrative_ontology:measurement(gacha_tr_t7, gacha_game_mechanics, theater_ratio, 7, 0.58).

% Extraction over time
narrative_ontology:measurement(gacha_be_t0, gacha_game_mechanics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gacha_be_t5, gacha_game_mechanics, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gacha_be_t10, gacha_game_mechanics, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gacha_be_t7, gacha_game_mechanics, base_extractiveness, 7, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gacha_game_mechanics, resource_allocation).
narrative_ontology:affects_constraint(gacha_game_mechanics, loot_box_regulation).
narrative_ontology:affects_constraint(gacha_game_mechanics, game_addiction_mechanics).
narrative_ontology:affects_constraint(gacha_game_mechanics, cosmetic_monetization_ethics).

% DUAL FORMULATION NOTE:
% Gacha game mechanics decompose into multiple structurally distinct constraints: (1) gacha_game_mechanics (this story, ε=0.68, snare) — behavioral extraction through variable ratio reinforcement at the player level; (2) loot_box_regulation (downstream, ε varying by jurisdiction) — regulatory response to suppress gacha mechanisms; (3) game_addiction_mechanics (upstream, ε=0.75+, snare) — broader behavioral capture mechanisms of which gacha is one instantiation; (4) cosmetic_monetization_ethics (sibling, ε≈0.25, rope) — legitimate cosmetic-only monetization as coordination mechanism. Each has distinct ε and structural data. Gacha_game_mechanics affects the regulatory and addiction stories downstream because regulatory response and addiction research are directly targeted at gacha mechanics specifically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gacha_game_mechanics, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
