% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Monopoly Rulebook as Rent-Extraction Demonstration (Text-Literal Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story evaluates the printed Monopoly rulebook strictly as written:
 *   the rent schedule escalates monotonically with development, bankruptcy is
 *   resolved by forced asset liquidation and removal from play, and no
 *   mechanism in the text redistributes wealth back toward trailing players.
 *   Read this way, the elimination of all but one player is not an accident
 *   of bad luck or poor play but the structurally guaranteed terminus of the
 *   rent-extraction mechanic once any player secures an early monopoly on a
 *   color group. This is the literal-text reading of the kernel; it is
 *   deliberately narrow and does not describe how the game is commonly played
 *   with house rules, nor does it adjudicate whether the mechanic is
 *   'legitimate' competition. Those are the social_scaffold_reading and
 *   tournament_orthodoxy_reading, respectively — separate constraints sharing
 *   this kernel, linked via network.affects_constraints, each with its own
 *   epsilon and its own stakeholder set.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.61).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Rent-Extraction Demonstration (Text-Literal Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '5a2e6f0e-591e-451f-ab15-6753855e4b78').
narrative_ontology:cs_kernel_codification('5a2e6f0e-591e-451f-ab15-6753855e4b78', fixed_text).
narrative_ontology:cs_authority_grounding('5a2e6f0e-591e-451f-ab15-6753855e4b78', practice).
narrative_ontology:cs_reading_relation('5a2e6f0e-591e-451f-ab15-6753855e4b78', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('5a2e6f0e-591e-451f-ab15-6753855e4b78', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('5a2e6f0e-591e-451f-ab15-6753855e4b78', foundational, elimination_is_the_text_intended_terminus).
narrative_ontology:cs_axiom_status(elimination_is_the_text_intended_terminus, holdable).
narrative_ontology:cs_axiom_grounding('5a2e6f0e-591e-451f-ab15-6753855e4b78', elimination_is_the_text_intended_terminus, empirically_contingent).
narrative_ontology:cs_axiom('5a2e6f0e-591e-451f-ab15-6753855e4b78', secondary, printed_text_alone_constitutes_the_game_no_amendment_authority).
narrative_ontology:cs_axiom_status(printed_text_alone_constitutes_the_game_no_amendment_authority, holdable).
narrative_ontology:cs_axiom_grounding('5a2e6f0e-591e-451f-ab15-6753855e4b78', printed_text_alone_constitutes_the_game_no_amendment_authority, conventional).
narrative_ontology:cs_reference_frame('5a2e6f0e-591e-451f-ab15-6753855e4b78', landlord_game_pedagogical_intent).
narrative_ontology:cs_drift_state('5a2e6f0e-591e-451f-ab15-6753855e4b78', post_1935_commercial_publication, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5a2e6f0e-591e-451f-ab15-6753855e4b78', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, board_game_publisher).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, mid_game_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, mid_game_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, capital_concentration_is_structurally_inevitable_under_property_rent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulates properties, builds houses/hotels, and collects escalating rent from opponents' forced landings. Once ahead, compounding advantage is nearly automatic under the printed rules: more cash buys more property, more property collects more rent, more rent buys more property. Faces no rule-text mechanism that redistributes back to laggards.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Land repeatedly on developed properties, pay rent that outpaces their income from salary/GO passes, mortgage and sell assets to stay solvent, and are eliminated when unable to pay. The rulebook provides no loan, welfare, or bankruptcy-forgiveness mechanism beyond forced liquidation and exit from the game. Once eliminated they have no further move available under the text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Sells the rulebook as printed, profits from continued sales regardless of the play experience it generates, and has no stake in whether the endgame mechanic produces elimination or gridlock. Benefits from the text remaining stable and unmodified across editions.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, board_game_publisher, beneficiary,
    institutional, generational, analytical, global).

% Hold a partial property set and oscillate between collecting modest rent and paying larger rent to whoever is ahead. Their trajectory under the literal rules bends toward elimination as the leader's board position compounds; the text gives them no lever to arrest the trend beyond risky trades that usually benefit the leader more.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, mid_game_players, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, mid_game_players, beneficiary).

% Play the game with informal modifications (auctioning unpurchased properties, free-parking jackpots, no-elimination endings) that alter the outcome the printed text produces. Under this reading their modifications are not part of the constraint being evaluated — they constitute a different, socially-corrected game, not evidence against what the text itself mandates.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_communities, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates a shared, replayable turn structure, property valuation schedule, and rent table so that any group of strangers can play an identical game without negotiating terms from scratch.
% TRANSFER_FUNCTION: Moves cash and, ultimately, playing status from players who land on developed properties to the player(s) who developed them; net direction over a full game is unidirectional toward whoever achieves an early monopoly, terminating in the elimination of all but one player.
% ABSENT_VOICES: House-rule communities and casual players who experience the eliminative endgame as unpleasant are not represented in the text itself, which fixes rent schedules and bankruptcy procedure without any amendment mechanism; their objections surface only as informal, unofficial modifications outside the rulebook's authority.
% DISAPPEARANCE_RATIONALE: If the printed rent-and-bankruptcy mechanic were removed, the game would need a substitute win condition; sales of the physical product depend on the rulebook being a stable, licensable, mass-produced text, and the elimination endgame is what gives Monopoly its recognizable pedagogical/competitive identity in tournament and cultural contexts.
% FOUNDING_PROBLEM: The rulebook (descended from the Landlord's Game) was built to demonstrate, in miniature and in a fixed play session, how unregulated land rent concentrates wealth and drives non-owners into ruin — an explicitly pedagogical anti-monopolist design later stripped of its redistributive counterpart mechanics and commercialized as pure competition.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the game (documenting Lizzie Magie's 1904 Landlord's Game patent and its anti-monopolist intent) and economists using Monopoly as a classroom illustration of rent-seeking corroborate the founding problem from outside the commercial beneficiary chain; Hasbro/publisher marketing materials, by contrast, frame the same mechanic purely as competitive entertainment, which is not corroboration but restatement by an interested party.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from near-zero at the game's opening (no one yet holds a monopoly; rent flows are small and roughly symmetric) to 0.68 by turn 90 as monopolies consolidate and rent payments compound geometrically for the trailing players. Suppression (0.61) reflects that the printed rules give a losing player literally no textual recourse other than mortgaging, trading at disadvantageous rates, and eventual elimination — there is no appeal mechanism inside the rulebook itself. Accessibility_collapse is high (0.70) because once a monopoly is established, the mathematics of escalating rent versus fixed salary income leaves the trailing player very little real strategic space; the outcome converges structurally. Theater_ratio stays low throughout (0.10 to 0.22) because almost all of the activity — dice rolls, property purchases, rent collection — is functionally load-bearing to the extraction mechanic; little of it is performative.
 *
 * DIRECTIONALITY LOGIC:
 *   The eventual monopolist is the clean structural beneficiary: rent income compounds, board position compounds, and the printed rules supply no ceiling. Eliminated players are the clean structural victims: forced payment obligations under a rent schedule they did not set, no bankruptcy-forgiveness clause, removal from play as the terminal state. Mid-game players sit closer to the victim end as the game progresses because the same compounding dynamic that benefits the leader works against everyone else simultaneously. The board game publisher is a beneficiary once removed — it profits from selling a stable, unmodified text regardless of which player wins or how unpleasant the endgame feels to trailing players.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming this constraint as a mountain (emerges_naturally: true) does real work here: the rulebook's rent-and-bankruptcy arithmetic is a closed formal system whose elimination endpoint follows deductively from the printed rent table and turn order, independent of who is playing or whether anyone wants that outcome. But mountain status is not innocence — declaring beneficiaries (the eventual monopolist, the publisher) on a mountain triggers FSM evaluation deliberately: this reading claims the rulebook's mathematics is genuinely law-like AND that identifiable parties (whichever player gets ahead first, and the commercial publisher indifferent to outcome) collect from that law-like operation. The omega below documents that this is exactly the ambiguity the FSM signature exists to surface — is this 'natural' game mathematics, or a constructed rule choice (no redistribution mechanic) dressed as inevitability?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_designed_absence,
    'Is the rent-extraction endgame a mathematical inevitability given any rent-escalation schedule with fixed player capital (mountain), or is elimination a specific, removable design choice — the printed rules could have included a redistribution mechanic (as Lizzie Magie''s original Landlord''s Game companion ruleset arguably did) but the published Parker Brothers/Hasbro text does not?',
    'Formal game-theoretic analysis of variant rulesets: does ANY rent-plus-property-development mechanic converge to single-winner elimination absent a redistribution rule, or does elimination depend specifically on the absence of a mechanic that could have been included and was historically present in an earlier version?',
    'If elimination is inevitable under any such mechanic regardless of redistribution rules, the mountain claim is well-grounded and the beneficiaries are incidental riders on genuine mathematical structure. If a redistribution mechanic would have prevented elimination, the ''inevitability'' is a designed-and-removed feature, and this constraint is better classified as a constructed extraction mechanism (tangled_rope or snare) dressed in mountain language — the FSM signature this story deliberately invites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_designed_absence, conceptual, 'Whether the elimination endpoint is genuine mathematical necessity or a specific, historically-reversible design omission.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (extraction_demo, social_scaffold, tournament_orthodoxy) disagree — on what the text SAYS, or on what authority the text SHOULD have over actual play?',
    'Textual analysis confirms all three readings agree on what the 1935 Parker Brothers rulebook literally states (rent schedule, no forgiveness clause). The disagreement is entirely over whether community house-rule practice or tournament-body enforcement should govern actual play, and whether the literal text''s elimination outcome is a feature to be demonstrated, softened, or defended.',
    'Because the disagreement is located in authority-over-practice rather than in textual content, all three readings can coexist without any single one being straightforwardly falsified by the others — each names a different legitimate locus of authority (the text itself, community correction, tournament codification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the kernel disagreement in authority-over-practice, not in disputed textual content.').

omega_variable(
    eliminated_player_coalition_potential,
    'Could trailing/eliminated players in principle coordinate (side payments, trade alliances against the leader) to resist the leader''s compounding advantage within the text''s own rules, and does the rulebook''s trading mechanic permit this?',
    'Game-theoretic simulation of coalition trading strategies among trailing players versus a leading monopolist under the literal trade rules (which permit any player-to-player trade the parties agree to).',
    'If coalition resistance is mechanically permitted and can meaningfully slow elimination, the ''structurally necessary'' framing in the constraint''s title overstates inevitability — some resistance capacity exists within the text, even if rarely exercised in practice. This would soften, not eliminate, the mountain claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eliminated_player_coalition_potential, empirical, 'Whether the rulebook''s own trading mechanic permits organized resistance by trailing players.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.21).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.22).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__extraction_demo_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the monopoly_rulebook kernel. extraction_demo_reading (this file) takes the printed text at face value and treats elimination as the mechanically inevitable output of the rent schedule — claimed as mountain, epsilon 0.68 at game end, victims = eliminated players. social_scaffold_reading treats the same text as socially unplayable without house-rule correction and would carry a lower effective epsilon once redistribution mechanics are modeled in. tournament_orthodoxy_reading treats the text as the sole legitimate competitive standard, with house rules reframed as illegitimate noise; its epsilon centers on competitive-legitimacy extraction (excluded house-rule communities) rather than in-game wealth transfer. The three do not average into one epsilon — each is a structurally distinct constraint reading the same kernel differently, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
