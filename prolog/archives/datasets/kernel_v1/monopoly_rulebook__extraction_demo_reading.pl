% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__extraction_demo_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook as Extraction Mechanism (Pedagogical Demonstration)
 *   domain: game_theory/economic_simulation/institutional_design
 *
 * SUMMARY:
 *   This is the extraction-demo reading of the Monopoly rulebook kernel. The
 *   reading interprets the game as a deliberate pedagogical demonstration
 *   that wealth concentration is mechanically inevitable under capitalist
 *   rules (property ownership, rent extraction, geometric cost scaling, no
 *   redistribution mechanisms). The game begins with rough initial parity but
 *   within 60-90 minutes produces a single winner and multiple eliminated
 *   players through a process that feels both determined by chance (dice) and
 *   determined by mathematics (wealth compounds). From this reading's
 *   perspective, the rules do not describe capitalism — they instantiate
 *   capitalism's essential extraction logic in miniature, making its
 *   inequality inevitable visible and measurable. The core claim is that
 *   Monopoly's rulebook, as-written, eliminates the possibility of
 *   alternative outcomes through its specific design choices:
 *   winner-takes-all payoff structure, no debt forgiveness, property costs
 *   that scale geometrically with ownership concentration, rent multipliers
 *   that accelerate wealth accumulation, and a game duration (60-90 minutes)
 *   short enough that elimination appears inevitable rather than contingent.
 *   The reading's pedagogical stance is that recognizing this inevitability
 *   is itself the game's 'truth': players learn by forced participation in a
 *   system that demonstrates capitalism's crushing logic.
 *
 * KEY AGENTS:
 *   - Eliminated Players: Primary victims (powerless/trapped) — undergo forced exit from game; bear psychological cost of witnessing inevitable ruin; remain at table as spectators to their own economic destruction.
 *   - Mid-Game Struggling Players: Secondary victims (moderate/constrained) — experience mixed coordination and extraction; have marginal exit options but face social cost of quitting; witness the asymmetric rules producing their disadvantage.
 *   - Wealth Accumulator / Winner: Primary beneficiary (institutional/arbitrage) — experiences rulebook as pure coordination mechanism; accumulates capital and property; achieves victory through mechanical advantage compounding.
 *   - Game System (Collective Rule Authority): Neutral enforcer (institutional/analytical) — the rulebook itself, treating the written rules as the binding constraint; produces inevitable outcomes through mechanical operation, not through conscious enforcement.
 *   - Capitalism Itself: Macro reading (analytical/universal) — the reading treats Monopoly as a scale model demonstrating capitalism's structural properties; the game is evidence that inequality is mechanically, not contingently, produced.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.58).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.62).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, snare).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Extraction Mechanism (Pedagogical Demonstration)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/economic_simulation/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'extraction-demo-reading-monopoly-kernel').
narrative_ontology:cs_kernel_codification('extraction-demo-reading-monopoly-kernel', formalized).
narrative_ontology:cs_authority_grounding('extraction-demo-reading-monopoly-kernel', extraction).
narrative_ontology:cs_interpretation_layer_present('extraction-demo-reading-monopoly-kernel').
narrative_ontology:cs_reading_relation('extraction-demo-reading-monopoly-kernel', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('extraction-demo-reading-monopoly-kernel', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('extraction-demo-reading-monopoly-kernel', foundational, wealth_concentration_is_mechanically_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_is_mechanically_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('extraction-demo-reading-monopoly-kernel', wealth_concentration_is_mechanically_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('extraction-demo-reading-monopoly-kernel', foundational, pedagogical_function_is_demonstration_of_capitalism).
narrative_ontology:cs_axiom_status(pedagogical_function_is_demonstration_of_capitalism, holdable).
narrative_ontology:cs_axiom_grounding('extraction-demo-reading-monopoly-kernel', pedagogical_function_is_demonstration_of_capitalism, conventional).
narrative_ontology:cs_reference_frame('extraction-demo-reading-monopoly-kernel', mechanical_wealth_reproduction_capitalism).
narrative_ontology:cs_drift_state('extraction-demo-reading-monopoly-kernel', contemporary_social_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('extraction-demo-reading-monopoly-kernel', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, wealth_accumulator).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, early_losers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELIMINATED PLAYER (SNARE) — Trapped within the game duration (60-90 minutes) with no meaningful exit. Lacking capital, cannot continue play. Faces maximum suppression through cumulative disadvantage: early bad rolls create debt, which creates forced property sales at unfavorable rates, which accelerates elimination. Zero degrees of freedom once trapped. The rulebook extracts labor attention (continued gameplay watching) and psychological investment (witnessing own defeat become inevitable) from those no longer participating.
constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-GAME STRUGGLING PLAYER (TANGLED ROPE) — Constrained by the momentum asymmetry: wealth generates wealth (collect more rent, purchase more properties, accumulate more capital), while poverty creates debt (land on expensive properties, forced sales, accelerating elimination). Experiences both coordination function (the rule system is coherent and mathematically tractable) and extraction (the rules ensure eventual ruin for those starting with disadvantage). Has marginal exit options (may negotiate trades or quit voluntarily) but faces significant social cost for quitting mid-game. Suppression is substantial but not absolute.
constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: WEALTH ACCUMULATOR / WINNER (ROPE) — Experiences the rulebook as pure coordination mechanism that solves the shared problem of determining a legitimate winner. The game has reduced complexity, clear decision trees, and a transparent victory condition. Extraction flows toward this agent but is experienced as reward for successful play, not as predatory extraction. Exit options are strong (can quit when winning to secure victory) and asymmetric.
constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: CAPITALISM AS PEDAGOGICAL SYSTEM (SNARE) — From the reading's own intended vantage point: Monopoly demonstrates that wealth concentration is mechanically inevitable under capitalist rules absent redistribution mechanisms. Players collectively are trapped in a system demonstrating its own crushing logic: initial wealth asymmetries compound; capital accumulation accelerates; debt becomes inescapable; elimination becomes certain for losers. The game teaches the lesson ('this is how capitalism works') through enforced participation and forced witnessing of others' ruin. The suppression mechanism is pedagogical: the emotional/cognitive cost of recognizing the game demonstrates a harsh truth about the system it models.
constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 5: MATHEMATICAL INEVITABILITY / NATURAL LAW (MOUNTAIN) — From the civilizational analytical perspective: the rulebook produces wealth concentration as a mathematical inevitability, not as contingent institutional design. The game's mechanics (rent collection compounds wealth; wealth buys access to higher-rent properties; higher-rent properties compound wealth faster; debt spirals downward for those with insufficient capital) guarantee eventual monoply regardless of starting conditions or player agency. This perspective treats the outcome as a law of game-mechanics, analogous to physical laws. However, the structural data contradicts this — the reading itself identifies specific rule choices (no redistribution, winner-takes-all, geometric property cost structure) that could be modified.
constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_rulebook__extraction_demo_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The rulebook produces wealth concentration as a mechanical outcome, and this concentration extracts value from losers in the form of eliminated capital, lost future action, and forced spectation. Early bad rolls create debt spirals; debt forces property liquidation at unfavorable rates; liquidation removes capital from losers; removed capital enters the accumulator's position. The extraction is smooth and mathematically traceable: Rent = (base_rent) × (1 + property_count), so owning 3 properties produces 3x + 6x + 9x rent collection on landing, while others pay into this pool. The 0.58 reflects that extraction is not experienced as coercive by the winner and feels like legitimate competition to participants, but the structural outcome (inevitable elimination) is the mechanism itself. Suppression (0.62): High. The suppression mechanism operates through cumulative disadvantage and mathematical certainty. Once a player lacks sufficient capital to meet rent obligations, they have no exit: bankruptcy rules force property sales; property sales reduce future earning capacity; reduced capacity guarantees elimination. The suppression is enforced not by the game-master but by the rules themselves — each player is compelled to play to the end or voluntarily quit (which carries social cost in competitive contexts). The suppression requirement increases over the game interval (early game allows hope; late game reveals inevitability) but remains stable in magnitude. Theater ratio (0.35): Low. The game has high functional transparency: dice rolls are pure chance, property acquisition is deterministic, rent calculations are explicit, elimination is mathematically predictable. There is little performative activity — the game does what it claims to do. The low theater ratio distinguishes this reading from piton (which would have high theater). The game genuinely produces its claimed outcome; the question is whether that outcome reveals a 'truth' about capitalism (extraction reading) or merely a 'competitive simulation' (social scaffold reading).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same rulebook produces radically different classifications depending on observer position. Eliminated players experience snare: they are trapped, targeted, and experience maximum extraction as their capital flows to the winner. Mid-game players experience tangled rope: mixed coordination and extraction, constrained exit, significant suppression but not absolute. The winner experiences rope: the rulebook solves their problem of determining legitimate competition. The pedagogical system (perspective 4) also reads as snare, but from the vantage point of the game-teaching-about-capitalism, where all players (including the winner) are trapped by the demonstration's logic: they cannot escape the system showing inequality is inevitable. The analytical observer risks false summit (treating the outcome as natural law) but the structural data contradicts this — the outcome depends on specific rule choices that could be modified. The perspectival gap between the winner's rope and the eliminated player's snare is the gap between those who benefit from the current rules and those whom the rules eliminate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the beneficiary/victim structure and the power/exit asymmetries. Eliminated players occupy d ≈ 0.98 (full targets: trapped powerless agents bearing maximum extraction). The wealth accumulator occupies d ≈ 0.05 (near-full beneficiary: institutional power with arbitrage exit options; extraction flows toward them). Mid-game struggling players occupy d ≈ 0.62 (moderate power, constrained exit, mixture of costs and benefits). The analytical observer at civilizational scope occupies d ≈ 0.72 (analytical observer standard). These d values drive f(d) calculations that produce the chi values sustaining the perspective classifications: trapped powerless agents see snare (high chi); beneficiaries see rope (low/negative chi); moderate agents see mixed outcomes (tangled rope); analytical observers at highest scope risk false summits (natural law perspective). The engine's directionality derivation produces these values automatically from the declared beneficiary/victim structure; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The extraction-demo reading resolves the mandatrophy by claiming that Monopoly instantiates a specific teaching about capitalism: inequality is mechanically inevitable under the game's rules. This is neither pure coordination (Rope) nor a temporary coordination problem (Scaffold) — it is a designed mechanism for producing a specific outcome (wealth concentration and elimination). The mandatrophy is whether the game teaches its players a true lesson about capitalism or merely simulates a competitive scenario. The reading asserts the former. The sibling readings would dispute this: the social-scaffold reading would claim the game teaches coordination with a sunset (as players learn to modify rules), and the tournament-orthodoxy reading would claim the game teaches legitimate competition. The extraction-demo reading stands on the claim that the rules, as-written and played-as-written, make elimination inevitable and thus demonstrate capitalism's mechanical production of inequality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    redistribution_counterfactual,
    'Does elimination occur under alternative rule-sets that include wealth redistribution, wealth cap, or debt forgiveness mechanics?',
    'Playtest Monopoly variants with: (a) forced wealth redistribution at intervals, (b) property rent caps, (c) bankruptcy restructuring. Compare game length, winner emergence timeline, and elimination patterns to baseline.',
    'If elimination persists: wealth concentration is robust mathematical feature independent of specific rules (supports mountain reading). If elimination disappears: wealth concentration is contingent institutional design choice (falsifies mountain reading, supports snare reading as product of specific rules).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(redistribution_counterfactual, empirical, 'Whether wealth concentration is inevitable or contingent on specific rule choices').

omega_variable(
    pedagogical_intent_ambiguity,
    'Was the original Monopoly rulebook designed to demonstrate capitalism''s inevitable inequality, or to demonstrate successful capitalist competition?',
    'Historical analysis of designer intent (Parker Bros internal documents, interviews with Charles Darrow/Henry George predecessors); comparison of game mechanics to The Landlord''s Game (Henry George''s proto-Monopoly emphasizing monopoly as evil). Examine whether ''teaching inequality'' was explicit design goal or post-hoc interpretation.',
    'If originally intended as critique: the extraction-demo reading aligns with designer intent (strengthens reading authority). If originally intended as celebration: the extraction-demo reading is counterinterpretation (weakens reading authority, may be influences rather than coexists_with relative to other readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_intent_ambiguity, conceptual, 'Whether pedagogical demonstration of inequality was original design intent').

omega_variable(
    natural_law_vs_design_choice,
    'Is the inevitable wealth concentration a mathematical law of any zero-sum wealth-distribution game with geometric rent scaling, or is it specific to Monopoly''s particular rule parameters?',
    'Mathematical analysis of rent-scaling sensitivity: vary property cost structures, rent multipliers, starting capital, and movement rates. Identify the minimal rule set that produces inevitable concentration vs the minimal rule set that permits non-concentrated outcomes.',
    'If law-like: extraction-demo reading''s mountain perspective is justified (concentration is inherent to any such game). If parameter-dependent: mountain perspective is false summit (concentration is contingent on specific choices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_design_choice, empirical, 'Whether wealth concentration is a law of zero-sum games or specific to Monopoly rules').

omega_variable(
    capital_intensity_temporal_path,
    'Does the time to reach winner determination (and elimination of all other players) scale with number of players in a way that reveals extraction as time-dependent phenomenon?',
    'Empirical study: measure game duration and elimination timeline for 2-player, 3-player, 4-player, and 6-player Monopoly under standard rules. Track wealth concentration curve and elimination order. If extraction time is O(n²) in player count, elimination is feature; if O(n), concentration may be noise artifact.',
    'If extraction time is scalable feature: supports snare reading (the system is designed to produce elimination). If extraction time is invariant: suggests elimination is secondary outcome of wealth concentration mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_intensity_temporal_path, empirical, 'Temporal dynamics of elimination relative to game mechanics').

omega_variable(
    pedagogical_truth_claim_grounding,
    'What is the epistemic status of ''Monopoly demonstrates capitalism''s truth''? Is this claim empirically grounded in real capitalism, theoretically derived from first principles, or interpretively assigned to the game?',
    'Compare Monopoly''s wealth-concentration mechanics to real-economy mechanisms: property ownership, rent extraction, debt dynamics, bankruptcy rules, wealth inheritance. Identify structural analogies and structural differences. Assess whether Monopoly''s ''lesson'' is descriptively accurate about capitalism or prescriptively ideological.',
    'If true analog: extraction-demo reading has strong epistemic warrant (game teaches accurate lesson). If partial analog: reading has weak warrant (game teaches distorted lesson). Affects both the axiom status and the false-summit risk of the mountain perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_claim_grounding, conceptual, 'Epistemic grounding of Monopoly-as-capitalism-pedagogy claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_extract_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(monopoly_extract_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(monopoly_extract_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.35).

% Extraction over time
narrative_ontology:measurement(monopoly_extract_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(monopoly_extract_be_t20, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(monopoly_extract_be_t40, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(monopoly_extract_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(monopoly_extract_be_t80, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(monopoly_extract_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_extract_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(monopoly_extract_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(monopoly_extract_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Monopoly rulebook kernel contains three structurally distinct readings producing different epsilon values and classifications. extraction_demo_reading: ε=0.58 (Snare). social_scaffold_reading: ε≈0.30-0.40 (Tangled Rope or Scaffold). tournament_orthodoxy_reading: ε≈0.25-0.35 (Rope). Each reading instantiates a different constraint with different beneficiary/victim structures, axioms, and epistemic warranties. They coexist as readings held by different communities: educators teaching about capitalism's inequality, game designers emphasizing fair play, and competitive players emphasizing meritocratic reward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
