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
 *   human_readable: Monopoly Rulebook as Pedagogical Model of Inevitable Rent-Extraction Concentration
 *   domain: game_theory_social_coordination_institutional_design
 *
 * SUMMARY:
 *   This story generates ONE reading of the contested Monopoly rulebook
 *   kernel: the extraction-demonstration reading. Under this reading, the
 *   rulebook's fixed transfer mechanics (rent scaling with development, no
 *   forgiveness, no redistribution, mandatory elimination on insolvency) are
 *   read as instantiating a structural, near-deterministic law of wealth
 *   concentration under unregulated rent extraction — a mountain in the sense
 *   that, given the rules as written and enough turns, concentration and
 *   elimination follow with the same necessity as a mathematical proof,
 *   regardless of who plays or how skillfully. This is NOT a story about
 *   house-ruled or tournament play; those are separate constraints
 *   (social_scaffold_reading, tournament_orthodoxy_reading) with their own ε
 *   values, linked here only as siblings, not blended in.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.61).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.52).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Pedagogical Model of Inevitable Rent-Extraction Concentration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory_social_coordination_institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '21673dbf-e53d-45a7-b4f8-56dcc83b16b5').
narrative_ontology:cs_kernel_codification('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', fixed_text).
narrative_ontology:cs_authority_grounding('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', practice).
narrative_ontology:cs_interpretation_layer_present('21673dbf-e53d-45a7-b4f8-56dcc83b16b5').
narrative_ontology:cs_reading_relation('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', foundational, rent_extraction_produces_necessary_concentration).
narrative_ontology:cs_axiom_status(rent_extraction_produces_necessary_concentration, holdable).
narrative_ontology:cs_axiom_grounding('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', rent_extraction_produces_necessary_concentration, empirically_contingent).
narrative_ontology:cs_axiom('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', secondary, elimination_outcome_is_pedagogically_true_not_incidental).
narrative_ontology:cs_axiom_status(elimination_outcome_is_pedagogically_true_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', elimination_outcome_is_pedagogically_true_not_incidental, instrumental).
narrative_ontology:cs_reference_frame('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', strict_text_deterministic_concentration).
narrative_ontology:cs_drift_state('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', contemporary_family_game_night, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21673dbf-e53d-45a7-b4f8-56dcc83b16b5', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, leading_property_accumulator).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, bank_as_structural_rentier_proxy).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, late_game_low_capital_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_concentration_thesis).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, rent_extraction_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulates a monopoly color group early through positional luck and trading skill, then collects compounding rent from every opponent landing on developed squares. Their advantage grows geometrically as houses and hotels are added; they have no incentive to stop the mechanism because it is the mechanism delivering their win.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, leading_property_accumulator, beneficiary,
    powerful, immediate, arbitrage, local).

% A non-agent structural position: the bank enforces the payment rules, never negotiates, never forgives debt, and administers the auction and mortgage mechanics precisely as written. It has no capital of its own to lose and no capacity to intervene in the concentration it processes; it simply executes the rulebook's transfer instructions without discretion.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, bank_as_structural_rentier_proxy, agenda_setter,
    institutional, immediate, analytical, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, bank_as_structural_rentier_proxy, beneficiary).

% Land repeatedly on developed rent squares, exhaust cash, mortgage properties, sell houses back at half value, and eventually cannot meet an obligation. They are removed from the board entirely — no partial participation, no reduced role, no path back in. Their capital transfers in full to the accumulator who bankrupted them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Still on the board but structurally doomed — holding undeveloped or mortgaged properties while facing an opponent's compounding rent schedule. Every turn is a countdown; their only moves are stalling trades that delay but do not reverse the trajectory toward elimination.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, late_game_low_capital_players, payer,
    powerless, immediate, constrained, local).

% Controls the printed rulebook and its franchise value but has no seat at any actual table; the pedagogical-demonstration reading of the game's mechanics is not one the publisher endorses in marketing, which instead sells the game as family fun and skill-based competition. Their commercial interest in a non-alarming reading is structurally excluded from this constraint's own account of itself.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, designer_estate_and_publisher, excluded,
    institutional, generational, analytical, global).

% Read the game (originally 'The Landlord's Game,' designed by Lizzie Magie as an anti-monopolist teaching tool) as a formal model demonstrating that unregulated rent extraction under a fixed ruleset drives deterministic wealth concentration and mass elimination — treating the mechanics as evidence for a real economic claim rather than as incidental features of a leisure product.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, political_economy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates turn order, property acquisition, and debt settlement so that a bounded group can play a legible game to a determinate conclusion without ongoing dispute about whose turn it is or what a landing on a given square costs.
% TRANSFER_FUNCTION: Moves cash and eventually all liquid and real assets from players who land on developed properties to the player who owns and develops them, with the bank as a zero-sum-preserving conduit that manufactures no new wealth and forgives no debt — until one player holds effectively all capital and the rest are eliminated.
% ABSENT_VOICES: The designer's estate and the publisher, who market the game as skill-based family entertainment, are structurally absent from this reading's account — their commercial framing would object that the elimination mechanic is incidental to fun, not a deliberate economic lesson, but that voice is excluded here by the reading's own analytical stance.
% DISAPPEARANCE_RATIONALE: If the fixed rulebook (no redistribution, no bankruptcy forgiveness, no rent caps) disappeared and any house-rule liquidity injection took its place, the deterministic path to single-winner concentration would break — games would run longer, more players would remain solvent, and the demonstration of inevitable concentration would no longer hold. The elimination outcome depends entirely on the rules as written; remove them and the pedagogical claim has no vehicle.
% FOUNDING_PROBLEM: The game (as 'The Landlord's Game') was built to make the mechanics of land monopoly and rent extraction under georgist economic critique visceral and legible to ordinary players — to teach, through play, how unregulated land rent concentrates wealth and impoverishes the propertyless.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and georgist scholars, entirely outside the game's commercial beneficiaries (the modern publisher and any given table's winning player), attest the original didactic intent from Magie's own patent filings and writings; the publisher and casual players, who profit from or enjoy the game as entertainment, do not corroborate — and have commercial and recreational reasons not to.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from near-zero at game start (T=0, symmetric starting capital, no developed properties, ε=0.08) to substantial by game end (T=90, one player holding effective monopoly rents, ε=0.61) — this trajectory is the mountain's operation unfolding over the interval, not drift in the rule's nature. Suppression (0.52) reflects that the rulebook offers no in-text mechanism to resist concentration once it begins compounding — the rules themselves foreclose intervention. Accessibility collapse is high (0.72): once a monopoly color group is developed, alternative strategic paths for opponents collapse rapidly. Resistance is moderate-low (0.4): players can trade and stall but cannot alter the underlying rent schedule, so resistance changes timing, not outcome.
 *
 * PERSPECTIVAL GAP:
 *   From the accumulator's seat, the rulebook is simply the game working as designed — coordination that lets a clear winner emerge. From the eliminated players' seat, the identical rules operate as a deterministic extraction machine that removed them from participation entirely. The engine's per-seat computation should show this divergence starkly: the same mountain claim reads as near-neutral from the beneficiary seat and as severe extraction from the payer seat, which is exactly the seat-divergence the mountain classification with declared beneficiaries (FSM candidate) is designed to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The leading accumulator is the structural beneficiary — every rule that transfers wealth on landing does so INTO their position once they hold developed monopoly squares; their exit option is best described as arbitrage because they can convert board position into decisive advantage at will. The bank is not a beneficiary in any capturing sense — it is a non-agent enforcement proxy, hence marked accordingly and excluded from directionality-bearing capture. Eliminated and late-game players are full targets: trapped or constrained exit, immediate horizon (the game ends for them specifically at bankruptcy), powerless position.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not claim the rulebook's mandate has been outlived — it claims the opposite: the mandate (demonstrate inevitable concentration) is fully live and executing as designed every time the game is played to completion. Mandatrophy is not declared here; the constraint is doing exactly what this reading says it was built to do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_pedagogy,
    'Is the rulebook''s concentration dynamic a genuine mountain (a structural, near-mathematical property of the payoff matrix and turn-based rent mechanics that would emerge under this rule-set regardless of designer intent) or a constructed artifact of one designer''s specific didactic choices that could have been built otherwise?',
    'Formal game-theoretic analysis of alternative rule variants (e.g., capped rent, periodic redistribution, no elimination) to determine whether concentration is a necessary consequence of ANY closed-economy multi-round property auction with compounding rent, or a contingent feature of THIS specific ruleset''s parameters.',
    'If concentration is necessary under a broad class of similar rulesets, the mountain claim strengthens (the rulebook is one instance of a general law). If concentration is highly parameter-sensitive and this specific ruleset was tuned by Magie to produce it deliberately, the constraint is better read as an intentionally constructed demonstration wearing mountain clothing — which is precisely the false-summit signature this story''s declared beneficiaries are meant to trigger for engine evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_pedagogy, conceptual, 'Whether the concentration dynamic is a structural game-theoretic necessity or a designed didactic artifact.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the three sibling readings of the monopoly_rulebook kernel locate their disagreement — is it about what the TEXT says (none dispute the printed rent schedule), about what COUNTS as legitimate play (house rules vs. strict text), or about what the OUTCOME MEANS (pedagogical indictment vs. incidental game feature vs. skill demonstration)?',
    'Compare the three readings'' treatment of identical rule text: extraction_demo_reading and tournament_orthodoxy_reading both accept strict-text play as authoritative but disagree on what the resulting elimination MEANS; social_scaffold_reading disputes whether strict-text play is even the relevant object of study, holding that actual social play always incorporates correction.',
    'If the disagreement is purely interpretive (meaning-level), all three readings can coexist indefinitely as different lenses on the same mechanics. If the disagreement is about what counts as the LEGITIMATE game (rules-level), social_scaffold_reading and tournament_orthodoxy_reading are in tension that this reading does not resolve either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the structural disagreement among the three kernel readings — text, legitimacy, or meaning.').

omega_variable(
    elimination_as_necessary_vs_contingent_endpoint,
    'Does every completed game under strict rules necessarily terminate in single-winner elimination of all others, or only probably, given enough turns and no external time limit?',
    'Monte Carlo simulation of the game under strict rules across many random dice/card sequences to establish whether elimination-to-one-winner is a measure-one outcome (near-certain given the mechanics) or merely a common but non-necessary outcome contingent on specific board layouts and trade decisions.',
    'A measure-one result strongly supports the mountain claim within this reading. A merely-probable result would suggest this reading overclaims necessity where only strong tendency is warranted, which would push the constraint toward a tangled_rope or snare classification rather than mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elimination_as_necessary_vs_contingent_endpoint, empirical, 'Whether elimination-to-single-winner is mathematically necessary or merely highly probable under strict play.').


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
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.17).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.44).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__extraction_demo_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the monopoly_rulebook kernel and the identical printed text: this story (extraction_demo_reading, mountain, high ε trajectory ending at 0.61, victim set = eliminated/doomed players), monopoly_rulebook__social_scaffold_reading (holds the strict-text arrangement is not actually socially playable without house-rule correction, implying a lower effective ε once correction is admitted), and monopoly_rulebook__tournament_orthodoxy_reading (holds the strict text is the legitimate competitive standard and elimination reflects skill, implying near-zero extraction from the competitive-legitimacy frame). Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways — each carries its own stable ε and stakeholder set, linked here by network edges rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
