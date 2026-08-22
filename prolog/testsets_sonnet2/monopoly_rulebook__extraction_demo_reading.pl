% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Monopoly Rulebook as Instantiated Wealth-Concentration Mechanism (Extraction-Demonstration Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story reads the printed Monopoly rulebook as instantiating an
 *   inevitable concentration mechanism: rent scales with development, there
 *   is no debt relief or wealth-redistribution mechanic, and bankruptcy is
 *   permanent removal from play. Under this reading, elimination of all but
 *   one player is not a contingent social outcome but the structurally
 *   necessary conclusion the fixed rules formally entail — treated here as
 *   the text's demonstrated pedagogical content, consistent with the game's
 *   documented anti-monopolist design origin. This is one of three linked
 *   readings of the same kernel (monopoly_rulebook): the
 *   social_scaffold_reading holds that community house-rules are required to
 *   make the text socially playable at all, and the
 *   tournament_orthodoxy_reading holds the printed text as the immutable
 *   competitive standard against which house rules are noise. Each reading is
 *   authored as its own constraint with its own epsilon; this file does not
 *   average across them.
 *
 * KEY AGENTS:
 *   - eventual_monopolist_player: primary beneficiary (powerful/arbitrage) — collects compounding rent
 *   - eliminated_players: primary victims (powerless/trapped) — permanently removed from play
 *   - mid_game_low_liquidity_players: secondary victims (moderate/constrained) — erode toward elimination
 *   - board_game_publisher: institutional beneficiary (institutional/arbitrage) — profits from the text's durability regardless of table outcome
 *   - house_rule_communities: excluded voice (organized/mobile) — modify the text specifically to avoid the outcome this reading treats as necessary
 *   - analytical_observer: analytical seat tracing the closed formal system to its terminal state
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.61).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.52).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Instantiated Wealth-Concentration Mechanism (Extraction-Demonstration Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'fbdb612d-4b61-46b6-b54a-9e5c68cef51f').
narrative_ontology:cs_kernel_codification('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', fixed_text).
narrative_ontology:cs_authority_grounding('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', practice).
narrative_ontology:cs_interpretation_layer_present('fbdb612d-4b61-46b6-b54a-9e5c68cef51f').
narrative_ontology:cs_reading_relation('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', foundational, elimination_is_the_texts_necessitated_endpoint).
narrative_ontology:cs_axiom_status(elimination_is_the_texts_necessitated_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', elimination_is_the_texts_necessitated_endpoint, empirically_contingent).
narrative_ontology:cs_axiom('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', secondary, printed_mechanics_vindicate_designers_didactic_intent).
narrative_ontology:cs_axiom_status(printed_mechanics_vindicate_designers_didactic_intent, holdable).
narrative_ontology:cs_axiom_grounding('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', printed_mechanics_vindicate_designers_didactic_intent, conventional).
narrative_ontology:cs_reference_frame('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', landlords_game_didactic_origin).
narrative_ontology:cs_drift_state('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', contemporary_mass_market_packaging, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbdb612d-4b61-46b6-b54a-9e5c68cef51f', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, board_game_publisher).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, mid_game_low_liquidity_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, rent_extraction_produces_inevitable_concentration).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, elimination_is_structurally_necessary_endpoint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulates a critical mass of properties early through positional luck and reinvestment, then collects escalating rent from every opponent landing on their squares. As the board's rent gradient compounds, they need take no further action beyond holding position — the rulebook's fixed rent schedule and lack of any redistribution mechanism does the extracting for them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player, beneficiary,
    powerful, immediate, arbitrage, local).

% Land repeatedly on developed properties, mortgage and sell buildings to stay solvent, and eventually cannot meet a rent demand. The rulebook provides no bankruptcy relief, no debt forgiveness, and no re-entry path — they are removed from play entirely, ending their participation while the game continues without them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Hold some property but insufficient cash reserves once the leading player's rent gradient steepens. They can trade, mortgage, or sell buildings back to the bank at a loss to delay elimination, but the rulebook offers no mechanism that redistributes accumulated wealth back into circulation — their position erodes turn over turn until they join the eliminated group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, mid_game_low_liquidity_players, payer,
    moderate, immediate, constrained, local).

% Sets and prints the rulebook text, licenses it internationally, and profits from unit sales regardless of how the printed rules play out at any given table. Has no stake in whether groups actually complete games as written; benefits from the rulebook's cultural durability and pedagogical reputation as a lesson about capitalism.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, board_game_publisher, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, board_game_publisher, agenda_setter).

% Widespread informal practice modifies the printed rules — free parking jackpots, no-auction skipping, interest-free loans — specifically to prevent the elimination endgame this reading treats as the text's structurally necessary conclusion. Under this reading their modifications are treated as departures from the constraint under study, not as evidence against it; their objection that the printed game is rarely played to its lethal conclusion is not admitted here.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_communities, excluded,
    organized, biographical, mobile, local).

% Reads the printed rulebook as a closed formal system: fixed rent schedule, no redistribution, no re-entry after bankruptcy, monotonic property concentration under repeated play. Traces how these mechanics necessarily produce a single surviving player and treats that outcome as the text's demonstrated content rather than as a contingent social event.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, eventual_monopolist_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates turn order, property acquisition, and rent collection into a single deterministic procedure so that any group of players, anywhere, using the same physical set, can resolve a game to a definite outcome without external adjudication.
% TRANSFER_FUNCTION: The rules move cash and property from players who land on developed squares to the player who developed them, with no mechanism returning wealth to circulation once concentrated — transfer is strictly one-directional from the many toward whichever player achieves early positional advantage.
% ABSENT_VOICES: House-rule communities and casual players who never finish games as printed would object that the game is not, as actually played, an elimination machine — but under this reading their departures from the text are exactly the phenomenon the text's rent-and-bankruptcy mechanics are read against, not evidence bearing on what the printed rules formally entail.
% DISAPPEARANCE_RATIONALE: If the printed rent schedule and no-redistribution rule were removed, property ownership would stop compounding into a single winner; games would either run indefinitely or require an alternate victory condition. The elimination mechanic is not incidental — remove it and the entire monopolist-endpoint reading of the text collapses along with the pedagogical claim built on it.
% FOUNDING_PROBLEM: The rulebook (descended from The Landlord's Game) was built to formally model, in playable form, how private land monopoly under unregulated rent extraction concentrates wealth and forces competitors out of the market — an explicitly pedagogical design intent documented by its original creator.
% FOUNDING_PROBLEM_CORROBORATION: The original designer's own patent filings and writings attest to the didactic anti-monopolist intent from outside the game's later commercial beneficiaries. The publisher, which profits from the game's mass-market packaging as light entertainment rather than economic critique, does not corroborate this reading and markets the same text under the tournament-orthodoxy and social-scaffold framings instead.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.12 to 0.61) because rent income compounds nonlinearly with property development while opponents' liquidity is monotonically depleted with no offsetting mechanic — this is the mathematically inevitable trajectory of a system with one-directional transfer and no redistribution. Suppression (0.52) reflects that the rules themselves, once agreed to as the game played, permit no in-game recourse once a player is bankrupt — there is no vote, no appeal, no partial forgiveness written into the text. Theater ratio stays comparatively low (0.28) because the extraction is real and functional, not performative — the rent schedule genuinely transfers wealth; there is little pretense involved in the printed mechanic itself. Accessibility collapse (0.58) and resistance (0.46) are authored below full-mountain intensity precisely because, unlike a genuine natural law, this outcome is only inevitable GIVEN the rules as printed and unmodified — house rules demonstrably collapse the inevitability in nearly all real play, which is why this reading's claimed_type as mountain is a contestable claim about the formal system in isolation, not a claim about how the game is actually played at most tables.
 *
 * PERSPECTIVAL GAP:
 *   From the eventual monopolist's seat, the trajectory looks like skillful accumulation rewarded by consistent rules. From an eliminated player's seat, the same fixed rent schedule and absent bankruptcy relief look like an engineered removal mechanism they had no way to resist once liquidity ran out. The engine computes these as different seat classifications from the same structural data — this reading does not require the monopolist to feel like an extractor for the extraction to be structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The eventual monopolist and the publisher are declared beneficiaries: the monopolist collects the rent stream directly (d near the beneficiary end, given their arbitrage-grade positional control once dominant), and the publisher collects licensing revenue independent of any table's outcome. Eliminated players and mid-game low-liquidity players are declared victims: their exit is trapped or constrained by the rules' own bankruptcy procedure, which removes them from the game entirely rather than allowing recovery — this pushes their derived d toward the full-target end. House-rule communities are excluded rather than positioned as victims or beneficiaries under this specific reading, because their modification of the rules removes them from the formal system this reading is about.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — modeling how unregulated rent extraction concentrates land and capital — remains, under this reading, live at the level of the formal rule system even though the social practice of the game (house rules) has largely mooted it at the table. This reading does not claim the founding problem is dead; it claims the printed rules still formally instantiate the mechanism regardless of whether any given group plays them straight. This prevents the mandatrophy trap of concluding 'nobody plays it that way, so the extraction claim is false' — the extraction is a property of the unmodified rule system, corroborated independently by the designer's own stated intent, not merely an artifact of unusually harsh play groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_system_vs_practiced_game,
    'Is the ''inevitable elimination'' this reading describes a property of the printed rulebook as a formal mathematical system, or a property of how the game is actually played by most social groups (who almost universally modify the rules)?',
    'Survey data on house-rule prevalence and game-completion rates as printed versus as modified; formal proof of the elimination endpoint under strict rule adherence versus empirical observation of typical play sessions.',
    'If elimination is overwhelmingly a formal-system property rarely realized in practiced play, the mountain claim describes the text-as-written but should not be read as a claim about the lived social institution of playing Monopoly — the social_scaffold_reading would then better describe the lived practice. If elimination is realized whenever house rules are absent (e.g. digital/enforced versions, tournament play), the mountain claim extends further into practiced reality than commonly assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_system_vs_practiced_game, conceptual, 'Whether inevitability is a property of the formal rules or of typical social practice — the central fork between this reading and its social_scaffold sibling.').

omega_variable(
    designer_intent_as_corroboration,
    'Does the documented anti-monopolist design intent of the original creator (Lizzie Magie''s Landlord''s Game patent) settle what the rulebook ''is really about,'' or is authorial intent irrelevant to the structural analysis of the rules as they now exist and are played?',
    'Historical/textual analysis of whether the extraction mechanics in the current commercial rulebook are materially unchanged from the original didactic design, versus philosophical argument about whether design intent should bear on structural classification at all.',
    'If intent is load-bearing evidence, this reading''s founding_problem corroboration is strong and independent of the current publisher. If intent is irrelevant to present structural analysis, the founding_problem_status becomes harder to corroborate from outside the game''s current commercial packaging as entertainment, weakening this reading''s genealogical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designer_intent_as_corroboration, conceptual, 'Whether historical designer intent is admissible corroboration for the founding problem, or whether only current structural operation counts.').

omega_variable(
    mountain_claim_contestability,
    'Is claiming ''mountain'' (emerges_naturally) appropriate for a board game''s printed rules, which are unambiguously a human artifact rather than a natural law — even though, within the closed formal system, the elimination outcome is mathematically necessitated by the stated mechanics?',
    'Distinguish ''necessary given fixed premises'' (a logical/mathematical mountain internal to the formal system) from ''natural law'' (a mountain in the sense of physics) — evaluate whether the schema''s mountain category is meant to cover the former.',
    'If the mountain claim is judged too strong for a human-authored ruleset, this reading would reclassify closer to a tangled_rope or snare (genuine coordination function — resolving turn order and property claims — bundled with the asymmetric extraction the beneficiary declarations describe), which would still preserve the high-epsilon, elimination-victim structure this reading is built around.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_claim_contestability, conceptual, 'Whether logical necessity within a human-authored formal system warrants the mountain claim, distinct from natural-law necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.26).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.47).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.59).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__extraction_demo_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.1).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the monopoly_rulebook kernel. extraction_demo_reading (this file) treats the printed rules as a formal system whose mechanics necessitate concentration and elimination — high epsilon (0.61), mountain claim, victim set = eliminated players. social_scaffold_reading treats the same text as requiring community house-rule correction to remain socially playable, implying the printed elimination endpoint is rarely the practiced one. tournament_orthodoxy_reading treats the printed text as the immutable competitive standard, reframing the same rent mechanics as legitimate skill differentiation rather than extraction. All three share the same kernel text but instantiate structurally distinct constraints with different beneficiary/victim structures and different epsilon values — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
