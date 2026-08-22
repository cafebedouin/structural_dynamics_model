% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook as Inevitable Wealth Concentration Demonstration
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This reading treats the Monopoly rulebook as a constraint that
 *   structurally demonstrates monopoly capitalism's pedagogical truth: wealth
 *   concentration through rent extraction is inevitable, elimination is the
 *   necessary outcome, and the game's 60-90 minute duration with no
 *   redistribution mechanisms makes winner-take-all the only stable
 *   equilibrium. The rulebook is read as a mountain constraint — its dynamics
 *   emerge naturally from the mathematical structure of rent-seeking on a
 *   closed board with finite liquidity, and the high extractiveness (0.72)
 *   reflects how thoroughly the constraint extracts from eliminated players
 *   to concentrate wealth in the winner. The claimed mountain type asserts
 *   the rulebook's elimination dynamic is a natural law of the system, not a
 *   design choice.
 *
 * KEY AGENTS:
 *   - winning_player: Primary beneficiary (powerful/arbitrage) — collects all rents and wins
 *   - eliminated_players: Primary victims (powerless/trapped) — bear 100% of extraction, eliminated from game
 *   - casual_players_forced_into_tournament_mode: Secondary victims (moderate/constrained) — experience the constraint as coercive when house rules are forbidden
 *   - rulebook_publisher: Beneficiary (institutional/arbitrage) — sells the constraint as authoritative text
 *   - tournament_organizer: Beneficiary (organized/arbitrage) — uses text authority for competitive ranking
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.72).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.89).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Inevitable Wealth Concentration Demonstration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '4868d935-5f15-4e58-8da5-4411eb6261bb').
narrative_ontology:cs_kernel_codification('4868d935-5f15-4e58-8da5-4411eb6261bb', fixed_text).
narrative_ontology:cs_authority_grounding('4868d935-5f15-4e58-8da5-4411eb6261bb', lineage).
narrative_ontology:cs_interpretation_layer_present('4868d935-5f15-4e58-8da5-4411eb6261bb').
narrative_ontology:cs_reading_relation('4868d935-5f15-4e58-8da5-4411eb6261bb', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('4868d935-5f15-4e58-8da5-4411eb6261bb', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('4868d935-5f15-4e58-8da5-4411eb6261bb', foundational, elimination_demonstrates_monopoly_truth).
narrative_ontology:cs_axiom_status(elimination_demonstrates_monopoly_truth, holdable).
narrative_ontology:cs_axiom_grounding('4868d935-5f15-4e58-8da5-4411eb6261bb', elimination_demonstrates_monopoly_truth, empirically_contingent).
narrative_ontology:cs_axiom('4868d935-5f15-4e58-8da5-4411eb6261bb', foundational, rulebook_as_natural_law).
narrative_ontology:cs_axiom_status(rulebook_as_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('4868d935-5f15-4e58-8da5-4411eb6261bb', rulebook_as_natural_law, deontological).
narrative_ontology:cs_reference_frame('4868d935-5f15-4e58-8da5-4411eb6261bb', raw_rulebook_elimination_dynamic).
narrative_ontology:cs_drift_state('4868d935-5f15-4e58-8da5-4411eb6261bb', house_rule_prevalence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4868d935-5f15-4e58-8da5-4411eb6261bb', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, rulebook_publisher).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, tournament_organizer).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, casual_players_forced_into_tournament_mode).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_pedagogical_truth).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, elimination_inevitability_without_redistribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects all rents from other players through the rulebook's mechanics; wins by eliminating all opponents. Can stop playing at any time as victor. The constraint fully subsidizes their position — they bear no extraction costs and receive all transfers.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, winning_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Bear 100% of the constraint's extraction — their assets are systematically drained through rent payments until elimination. Once eliminated, they have zero exit from the constraint's outcome (they have lost). No redistribution mechanisms exist to re-enter. The rulebook's structure makes their elimination mathematically inevitable given finite liquidity and no wealth injection.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Experience the rulebook as coercive when house rules (Free Parking cash, no auctions, mercy rules) are forbidden by tournament orthodoxy. They would prefer social play with redistribution but are constrained by the text's authority. Their exit is constrained — they can refuse to play, but lose the social game occasion. They are excluded from the rule-setting conversation when tournament organizers enforce text purity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, casual_players_forced_into_tournament_mode, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, casual_players_forced_into_tournament_mode, excluded).

% Sells the rulebook as authoritative text; benefits from the constraint's status as the definitive Monopoly. The publisher has arbitrage-grade exit (can publish other games) but extracts ongoing value from the rulebook's canonical status. The constraint's mountain claim (natural law demonstration) serves the publisher's commercial interest in text authority.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rulebook_publisher, beneficiary,
    institutional, generational, arbitrage, global).

% Uses the rulebook's text authority for competitive ranking and comparison. Benefits from the constraint's immutability — house rules would destroy the comparability that makes tournaments viable. Has arbitrage exit (can organize other games) but extracts status and legitimacy from this constraint's orthodoxy. The tournament_orthodoxy_reading is their native frame; this reading's mountain claim reinforces their interest in text purity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, tournament_organizer, beneficiary,
    organized, biographical, arbitrage, national).

% Sees the full structure across all three readings: the rulebook as natural law demonstration (this reading), as social scaffold requiring house rules (social_scaffold_reading), and as competitive orthodoxy (tournament_orthodoxy_reading). Bears no material stake; exit is analytical (can adopt any reading).
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a complete, deterministic, self-contained game system where all players operate under identical rules with no external arbitration needed. The rulebook coordinates by eliminating ambiguity — every transaction, auction, and elimination follows from the text.
% TRANSFER_FUNCTION: Moves all liquidity from eliminated players to the winning player through rent payments on owned properties. The transfer is accelerated by the absence of redistribution mechanisms (no Free Parking cash, no wealth tax, no bankruptcy protection). Winner takes all; eliminated players leave with zero.
% ABSENT_VOICES: Players who would prefer social/cooperative play but are excluded when tournament orthodoxy enforces text purity. Children and casual players who experience the elimination dynamic as harsh rather than pedagogical. Historical players who used house rules as the default social mode — their practice is erased by the tournament_orthodoxy_reading's claim to text authority.
% DISAPPEARANCE_RATIONALE: If the rulebook's elimination constraint vanished overnight (e.g., house rules became canonical), the game would rearrange: games would last indefinitely with liquidity circulation, elimination would become rare, the pedagogical demonstration of monopoly capitalism would disappear, and tournament comparability would collapse. The world of Monopoly play would fundamentally reorganize around social sustainability rather than competitive elimination.
% FOUNDING_PROBLEM: The rulebook was built to provide a complete deterministic game for competitive play where strategic skill determines outcomes without ambiguity or house-rule variance. Elizabeth Magie's original Landlord's Game (1904) was built to demonstrate the injustice of rent extraction; Parker Brothers' Monopoly (1935) inverted this to celebrate competitive elimination.
% FOUNDING_PROBLEM_CORROBORATION: The tournament_orthodoxy_reading attests the founding problem (competitive determinism) is still live. The social_scaffold_reading attests the founding problem was social playability and is dead for tournament play but live for social play — corroborated by the universal historical prevalence of house rules. The extraction_demo_reading attests the founding problem (demonstrating monopoly capitalism's truth) is live and the rulebook succeeds at it. No single corroborating source outside all beneficiary sets exists — the three readings disagree on what the founding problem was.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, ExtMetricName, E),
    domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.72 over the game interval as liquidity drains from eliminated players to the winner — the measurement grid captures this accumulation. Suppression requirement intensifies from 0.65 to 0.89 because the rulebook actively forbids house rules (redistribution, liquidity injection, mercy rules) that would soften elimination; the constraint's persistence depends on enforcing the text as written. Theater ratio stays low (0.05-0.15) because the rulebook's coordination function (providing a complete deterministic game) is genuine but its extraction function dominates. Accessibility collapse (0.78) is high because once the rent-extraction dynamic is understood, alternative play modes (cooperative, redistributive) collapse — the rulebook's structure makes them incoherent. Resistance (0.22) is low because eliminated players have no structural power to resist; the constraint operates through mathematical necessity, not persuasion.
 *
 * DIRECTIONALITY LOGIC:
 *   The winning_player sits at the beneficiary extreme (d ~ 0.0): the constraint subsidizes them entirely, they collect all rents, and their exit option is arbitrage (they can stop playing anytime as victor). Eliminated_players are full targets (d ~ 1.0): they bear 100% of extraction, are trapped by the elimination mechanic, and have zero exit once assets are depleted. Casual_players_forced_into_tournament_mode are constrained targets (d ~ 0.7): they experience the text as coercive when house rules are disallowed, but have mobile exit (refuse to play). Rulebook_publisher and tournament_organizer are institutional beneficiaries (d ~ 0.1): they extract value from the constraint's authority without bearing its elimination costs. The analytical_observer is symmetric (d ~ 0.5): sees the full structure without material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (providing a complete deterministic game for competitive play) is contested — the extraction_demo_reading claims the rulebook demonstrates monopoly capitalism's truth, while the social_scaffold_reading claims the founding problem was social playability requiring house rules. The constraint persists despite the social_scaffold_reading's demonstration that house rules make it socially playable, because tournament_orthodoxy_reading locks the text as immutable for competitive comparison. This is not mandatrophy — the constraint's function (demonstrating inevitable concentration) is live for this reading. The mountain claim with declared beneficiaries triggers FSM evaluation: the rulebook presents itself as natural law (emerges_naturally: true) but identifiable beneficiaries (winner, publisher, tournament organizer) collect from its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_demonstration,
    'Is the rulebook''s wealth concentration dynamic a genuine natural law of rent-seeking systems, or a constructed demonstration that benefits identifiable agents (winner, publisher, tournament organizer)?',
    'Compare the rulebook''s mathematical structure to other closed rent-seeking systems with finite liquidity and no redistribution; test whether the elimination inevitability holds across parameter variations or depends on specific rule choices (e.g., auction rules, mortgage rules, Free Parking cash).',
    'If natural law, the mountain classification holds and beneficiaries are incidental to the structure; if constructed demonstration, FSM triggers reclassification to tangled_rope with the rulebook as a pedagogical tool that extracts from players to serve publisher/tournament interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_demonstration, conceptual, 'Whether the constraint''s mountain claim is a genuine natural law or a false summit with identifiable beneficiaries').

omega_variable(
    elimination_mechanism_necessity,
    'Is player elimination structurally necessary for the rulebook''s demonstration, or does it serve the tournament_orthodoxy_reading''s need for decisive competitive outcomes?',
    'Analyze whether alternative end conditions (wealth threshold, time limit, cooperative victory) preserve the pedagogical demonstration while avoiding elimination; examine historical rule variants.',
    'If elimination is pedagogically necessary, the mountain claim strengthens; if elimination serves competitive orthodoxy, the constraint is tangled_rope with tournament organizers as beneficiaries of decisive outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elimination_mechanism_necessity, empirical, 'Whether elimination is a necessary feature of the demonstration or a design choice serving tournament orthodoxy').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does this reading''s core premise (rulebook as natural law demonstration of monopoly capitalism) logically foreclose the social_scaffold_reading (house rules required for social playability) within any single framework, or do they coexist as different play communities?',
    'Test whether a single play community can simultaneously hold that the rulebook demonstrates natural law AND requires house rules for social playability; examine whether competitive and social play communities are structurally separate or overlapping.',
    'If forecloses, reading_relations = forecloses; if coexist, reading_relations = coexists_with; if this reading''s natural law claim creates pressure on social play communities to adopt tournament orthodoxy, reading_relations = influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between extraction_demo_reading and social_scaffold_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.14).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.15).

% Extraction over time
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.84).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.87).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.88).
narrative_ontology:measurement(monopoly_rulebook_extraction_demo_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint family (monopoly_rulebook kernel, three readings) decomposes the single rulebook text into structurally distinct constraints: extraction_demo_reading (mountain, high epsilon, elimination as natural law), social_scaffold_reading (rope/tangled_rope with house rules as coordination mechanisms), tournament_orthodoxy_reading (piton/scaffold with text authority as coordination). The epsilon values differ because each reading assesses a different standing arrangement: this reading assesses the raw rulebook's elimination dynamic; social_scaffold_reading assesses the rulebook+house-rules arrangement; tournament_orthodoxy_reading assesses the rulebook-as-competitive-standard arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerful, 0.05).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerless, 0.95).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, moderate, 0.7).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, institutional, 0.1).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, organized, 0.1).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
