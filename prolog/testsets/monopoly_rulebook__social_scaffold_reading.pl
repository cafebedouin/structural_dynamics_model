% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__social_scaffold_reading, []).

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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly Rulebook as Social Scaffold (House Rules Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   The Monopoly rulebook presents a contested institutional kernel: a
 *   published text that prescribes how the game should be played. This story
 *   instantiates the SOCIAL SCAFFOLD READING — the perspective that the text
 *   rulebook mandates extraction (rapid elimination, prolonged endgames,
 *   harsh winner-takes-all dynamics) that is incompatible with casual social
 *   play, and that house rules are a scaffolding mechanism that preserves
 *   group coordination by providing a sunset exit from the text's harshness.
 *   The constraint operates over the interval of a typical multi-hour game
 *   session (t=0 at game start with strict text rules; t=6 at the point where
 *   house-rule modifications have been established as group norm). The core
 *   structural observation: a group can either follow the published rulebook
 *   (experiencing high extractiveness and suppression as early-eliminated
 *   players are locked into spectatorship) or adopt house rules (experiencing
 *   lower extractiveness, longer engagement, and a cooperative modification
 *   of the rules themselves). The scaffold reading treats house rules as a
 *   temporary coordination solution — a formal, recognized deviation from the
 *   text that the group will abandon if (1) they start playing a different
 *   game, (2) the social function of Monopoly nights ends, or (3) explicit
 *   rule negotiation becomes too high-effort. The sunset timeline is 10-15
 *   years, reflecting the generational obsolescence of Monopoly in casual
 *   play as modern board games with built-in balance displace it. The
 *   measurement trajectory shows declining extractiveness and suppression as
 *   house rules are negotiated and adopted, but stable theater (house-rule
 *   negotiation itself is performative — the group is collectively deciding
 *   how to play, which is both functional and theatrical).
 *
 * KEY AGENTS:
 *   - Social Playing Group: Moderate power, constrained exit (moderate/constrained) — agents who choose to modify rules; experience scaffold constraint as beneficial coordination
 *   - Late-Game Eliminated Players: Powerless, trapped (powerless/trapped) — experience snare: locked into spectatorship under text rules, escaped by house-rule adoption
 *   - Winning/Optimal Player: Powerful, arbitrage exit (powerful/arbitrage) — benefits from text rulebook; experiences rope (coordination standard)
 *   - Hasbro/Text Authority: Institutional, arbitrage exit (institutional/arbitrage) — publishes standard rulebook; benefits from coordination of competitive play and uniform rules across markets
 *   - House-Rule Community: Organized, constrained exit (organized/constrained) — forums, casual play communities that formalize and propagate house rules; see sunset as real (modern games replace Monopoly)
 *   - Analytical Observer: Analytical position (analytical/analytical) — risks naturalizing text rulebook as inevitable design rather than contingent institutional choice benefiting certain actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.38).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook as Social Scaffold (House Rules Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'd3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f').
narrative_ontology:cs_kernel_codification('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', formalized).
narrative_ontology:cs_authority_grounding('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', practice).
narrative_ontology:cs_interpretation_layer_present('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f').
narrative_ontology:cs_reading_relation('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', foundational, house_rules_legitimacy_principle).
narrative_ontology:cs_axiom_status(house_rules_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', house_rules_legitimacy_principle, instrumental).
narrative_ontology:cs_axiom('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', secondary, sunset_sunset_temporality).
narrative_ontology:cs_axiom_status(sunset_sunset_temporality, holdable).
narrative_ontology:cs_axiom_grounding('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', sunset_sunset_temporality, conventional).
narrative_ontology:cs_reference_frame('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', casual_social_playability).
narrative_ontology:cs_drift_state('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', contemporary_house_rule_adoption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d3c4e5f6-a7b8-4c9d-8e1f-2a3b4c5d6e7f', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLAYING GROUP (SCAFFOLD) — Moderate power agents who choose to modify the rulebook via house rules. Constrained by desire to play with available group; experienced extractiveness is low because the group has agency to adjust rules. Sees the text rulebook as a template, not a mandate. Sunset logic: house rules can be abandoned (reverting to text rules) if coordination no longer needs them, but typically persist as long as the group plays together.
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: LATE-GAME PLAYERS (SNARE) — Economically eliminated early in a strict-text game, locked into spectatorship. The text rulebook enforces rapid elimination and an extractive endgame (winner takes the table's attention for 1+ hours). These players cannot exit without leaving the social setting entirely. The social pressure to 'stick around' and watch the endgame creates suppression. From this perspective, the text rulebook is a pure extraction mechanism — the early-eliminated pay the cost of the table's entertainment.
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: WINNING CONDITION / OPTIMAL PLAYER (ROPE) — Player who wins under text rules. Experiences high arbitrage exit (can always switch to a variant where they win more easily). The text rulebook serves them well. They see elimination mechanics as fair competition. For this perspective, the rulebook is pure coordination: clear rules, transparent winning condition, legitimate asymmetry based on play skill.
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: HASBRO / TEXT AUTHORITY (ROPE) — The published rulebook is a coordination standard. Hasbro benefits from clear, universally understood rules (tournaments, competitive play, intellectual property clarity). The text rulebook is a coordination device that enables the game industry to exist. Rules tournaments enforce fidelity. For this perspective, house rules are a deviation, not a feature — they represent local coordination failure that generates value for Hasbro (selling the rulebook as the solution).
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HOUSE-RULE COMMUNITY (SCAFFOLD) — Organized agents (online forums, board game communities, casual game groups) who have formalized alternative rulesets. Sees the text rulebook as a starting point, not a mandate. Actively generates house rules that extend game duration, preserve player engagement, and inject liquidity (Free Parking pools, reduced elimination harshness). These organizations see a genuine sunset: as new games with built-in balance emerge, dependence on Monopoly house rules decreases. Constraint lifespan: 10-15 years as modern board game design displaces Monopoly in casual play.
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the text rulebook might appear as an immutable design: a simple, elegant game emerges from those specific rules, and any modification breaks the designer's intent. This perspective risks naturalizing the text rulebook as inevitable rather than recognizing it as one reading of a contested institutional kernel (the Monopoly design space). However, declared beneficiaries (social_group_cohesion) indicate this is likely a false-summit candidate — the 'design necessity' framing obscures contingent social choices.
constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_rulebook__social_scaffold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, declining over the interval. The text rulebook extracts from late-eliminated players (forced spectatorship for 1+ hours) and from early players (long passive waiting). House rules reduce this by shortening games, preserving participation, and injecting liquidity (Free Parking rule, reduced rental costs, or accelerated starting cash). The decline from 0.52 → 0.38 reflects rule negotiation shifting the balance toward coordination. Suppression (0.42): Moderate, declining. The text rulebook suppresses alternatives (players feel obliged to follow 'official' rules, creating social pressure to endure the harsh endgame). House rules reduce suppression by formalizing the freedom to modify. The decline from 0.60 → 0.42 reflects social legitimization of house rules. Theater ratio (0.58): Moderate, stable. House-rule negotiation itself is partly performative — the group is engaging in a collective decision ritual, not just optimizing for fun. This is not purely functional (the rules do change how the game plays) but not purely theatrical either (the rules address real coordination problems).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The text-rule beneficiary (optimal player, Hasbro) sees rope — a coordination standard that enables competition and legitimacy. The late-eliminated player sees snare — locked into an extractive endgame. The house-rule community sees scaffold — a sunset solution to a known problem. The playing group sees scaffold — constrained agents with agency to modify. The analytical observer risks seeing mountain — naturalizing the text as inevitable design, which the false-summit detector will likely flag because beneficiaries are declared (the text rulebook benefits specific institutional actors: Hasbro, competitive standardization).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from power level, exit options, and beneficiary/victim status. The text rulebook benefits Hasbro (institutional/arbitrage) and optimal players (powerful/arbitrage) — both have low d values (beneficiaries experience low or negative chi). It extracts from late-eliminated players (powerless/trapped) — high d, high chi, high experienced extractiveness. House rules reduce d for the playing group (moderate/constrained) by giving them agency to modify the rules themselves — constrained exit is higher-cost than trapped, but the group's ability to negotiate rules reduces the derived d relative to strict adherence to text. The scaffold reading depends on this directionality shift: the group's power to modify the constraint (via house rules) is what makes it a scaffold rather than a snare. Without explicit house-rule adoption, the constraint would classify as snare from the playing group's perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    house_rules_necessity_vs_design_flaw,
    'Are house rules necessary to make Monopoly socially playable, or do they indicate a genuine design flaw in the text rulebook?',
    'Historical analysis: pre-1960 play logs vs post-1970 house rule adoption rates; comparison with contemporary board games that achieve balance without house rules; survey of player satisfaction in strict-text vs house-rule play',
    'If necessity: constraint is genuine scaffold (coordinates around a known problem). If design flaw: constraint is a symptom of textual extractiveness (players escape via modification). If both: the text rulebook is a snare that generates its own escape mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rules_necessity_vs_design_flaw, empirical, 'Whether house rules address a design necessity or a textual flaw').

omega_variable(
    free_parking_injection_mechanism,
    'Does Free Parking cash injection actually lengthen games and preserve player engagement, or does it merely redistribute wealth without changing elimination dynamics?',
    'Controlled play tests: strict-text vs Free Parking variants, measuring game duration, elimination timing, final player participation (how many players are still engaged at game end vs spectatorship). Statistical analysis of cash flow: does injected liquidity prevent early bankruptcy or just delay it?',
    'If liquidity injection effective: house rules genuinely reduce suppression and extend playability (scaffold confirmed). If ineffective: Free Parking is theater, not coordination (snare underlying remains, rule modification is performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_parking_injection_mechanism, empirical, 'Whether Free Parking injection lengthens games and preserves engagement').

omega_variable(
    sunset_timeline_accuracy,
    'As modern board game design evolves, will Monopoly house rules become obsolete, or will they persist as a cultural institution independent of game quality?',
    'Longitudinal tracking: participation rates in Monopoly vs modern alternatives (Catan, Ticket to Ride, etc.) over 10-year intervals; survey data on play frequency and rule modification adoption; cultural analysis of Monopoly''s role in family game nights as tradition vs competition',
    'If obsolescence on schedule: scaffold sunset is real (10-15 years). If persistence despite alternatives: house rules have become a stable cultural institution rather than a temporary coordination solution (reclassify toward rope or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_timeline_accuracy, empirical, 'Timeline and likelihood of Monopoly house-rule obsolescence').

omega_variable(
    text_rulebook_as_false_summit,
    'Is the text rulebook a natural, inevitable game design, or is it a contingent institutional choice that benefits specific agents (Hasbro, competitive play standardization) while extracting from casual social players?',
    'Comparative design analysis: historical development of Monopoly rules; designer intent documentation (Parker Brothers archives); analysis of rule changes across editions; comparison with house-rule variants that achieve similar objectives as the text (engagement, duration, payoff distribution) with higher social playability',
    'If contingent institutional choice: text rulebook is a false summit (mountain classification becomes tangled_rope). Declaring beneficiaries (social_group_cohesion) will trigger FSM. If natural design: genuine mountain, and house-rule reading is a misapplication of DR framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_rulebook_as_false_summit, conceptual, 'Whether text rulebook is natural design or contingent institutional choice').

omega_variable(
    kernel_reading_contest_scope,
    'What is the contested kernel — is it the specific published Monopoly rulebook, or is it the game design space (all possible Monopoly variants)?',
    'Document the three sibling readings'' core premises about the kernel: extraction_demo_reading assumes the text rulebook is a fixed pedagogical tool; tournament_orthodoxy_reading assumes the text is a coordination standard; social_scaffold_reading (this one) assumes the kernel is the game design space with the text as one reading. Each reading instantiates a different constraint.',
    'Determines whether sibling readings forecloses each other or coexist. If kernel is the published text alone, readings may foreclose. If kernel is the design space, readings coexist as different legitimate instantiations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_scope, conceptual, 'Definition of the contested kernel (text rulebook vs game design space)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_theater_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(monopoly_social_theater_t3, monopoly_rulebook__social_scaffold_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(monopoly_social_theater_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(monopoly_social_extractiveness_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(monopoly_social_extractiveness_t3, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(monopoly_social_extractiveness_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_social_suppression_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(monopoly_social_suppression_t3, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(monopoly_social_suppression_t6, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The three readings of the Monopoly rulebook kernel are structurally distinct constraints, not alternative perspectives of a single constraint. They differ in ε (0.65 for extraction_demo, 0.22 for tournament_orthodoxy, 0.38 for social_scaffold), in beneficiary/victim structure, and in classification type. The social_scaffold reading decomposes the text rulebook's function into two components: (1) the text as it exists (extractive, unmodified), and (2) the house-rules modification space (scaffold, sunset). Each sibling reading emphasizes a different aspect of the institutional kernel. They are linked by network affects_constraints to reflect that they are interpretations of the same published text, not separate empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
