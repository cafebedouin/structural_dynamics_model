% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: House-Rule Correction Regime (Monopoly Rulebook, Social-Scaffold Reading)
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   A recurring game night plays Monopoly under a standing house-rule regime:
 *   cash payouts on Free Parking, bonus salary on GO, a fixed end-time with
 *   an asset count instead of play-to-the-last-bankruptcy. This story authors
 *   the SOCIAL_SCAFFOLD_READING of the monopoly_rulebook kernel: the
 *   house-rule regime is a transitional coordination structure whose
 *   justification is carrying a mixed-preference group through one full
 *   social evening that the printed text's elimination economy would break.
 *   Per the ε-invariance rule this file authors ONLY this reading as a clean
 *   constraint: ε's referent is the standing arrangement under contest — the
 *   house-rule regime as actually played, assessed by this reading's own
 *   lights — never the strict-text game this reading corrects, and never a
 *   blend across readings. The extraction-demo reading (the text as
 *   pedagogical rent-extraction demonstration, house rules as suppression of
 *   the lesson) and the tournament-orthodoxy reading (the text as the
 *   legitimate competitive framework, house rules as noise) instantiate
 *   different constraints with their own ε, beneficiary structures, and
 *   types; they are linked, not merged. Claim and metrics are authored
 *   independently: claimed_type scaffold states what I believe structurally
 *   true (transitional, sunset-bounded, consent-renewed); the metrics state
 *   what I believe descriptively true (moderate extraction borne by a
 *   minority, mild social suppression, low theater). The kernel context's
 *   'beneficiary = social group cohesion' is mapped to actors per the naming
 *   rules: the group's member seats appear under beneficiaries, and the
 *   abstract good appears in vindicated_propositions.
 *
 * KEY AGENTS:
 *   - game_night_host: agenda-setter and beneficiary (moderate/mobile) — proposes the house-rule set, arbitrates disputes, calls the sunset; the evening's success flows to her
 *   - casual_social_players: primary beneficiaries (organized/mobile) — the majority bloc whose renewed consent sustains the regime
 *   - elimination_vulnerable_players: beneficiaries (moderate/constrained) — the players the strict text would eject first; the regime's motivating case
 *   - tournament_minded_players: primary target/payer (moderate/constrained) — bears the regime's cost in diluted competition and outvoted preference
 *   - game_designers: excluded (institutional/trapped) — authored the text's economy; overruled at every table with no seat
 *   - game_theory_analysts: analytical observer — studies the correction layer across tables without holding one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.4).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.32).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "House-Rule Correction Regime (Monopoly Rulebook, Social-Scaffold Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game theory / social coordination / institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'd8c9d37b-99f5-4302-9e08-01ebf0e07b4f').
narrative_ontology:cs_kernel_codification('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', fixed_text).
narrative_ontology:cs_authority_grounding('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', practice).
narrative_ontology:cs_interpretation_layer_present('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f').
narrative_ontology:cs_reading_relation('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', foundational, social_playability_over_textual_fidelity).
narrative_ontology:cs_axiom_status(social_playability_over_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', social_playability_over_textual_fidelity, instrumental).
narrative_ontology:cs_axiom('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', foundational, community_correction_authority).
narrative_ontology:cs_axiom_status(community_correction_authority, holdable).
narrative_ontology:cs_axiom_grounding('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', community_correction_authority, conventional).
narrative_ontology:cs_reference_frame('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', text_as_provisional_instrument).
narrative_ontology:cs_drift_state('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', contemporary_game_night, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d8c9d37b-99f5-4302-9e08-01ebf0e07b4f', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_social_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, elimination_vulnerable_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, game_night_host).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, tournament_minded_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, social_playability_doctrine).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, player_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the evening, proposes the house-rule set before the box opens — money on Free Parking, bonus salary on GO, a fixed end-time with an asset count — arbitrates mid-game disputes by pointing back to what the table agreed, and decides when to call the end. The evening's success flows to her: guests who stay late, a table that laughs, an invitation that gets reciprocated. She could announce strict rules any week, but would then spend the evening refereeing disappointment instead of hosting it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_night_host, agenda_setter,
    moderate, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, game_night_host, beneficiary).

% Attend for the evening, not the game. Under the house rules they stay solvent, stay consequential, and stay seated; the Free Parking pot and GO bonuses hand them periodic windfalls that keep the night eventful. Their alternative — a different game, or no game — is always open, which is why their agreement is renewed each session rather than assumed.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_social_players, beneficiary,
    organized, immediate, mobile, local).

% The players the printed economy would remove first — unlucky early purchases, one bad rent payment from bankruptcy by hour two. The house rules keep them liquid enough to matter: the pot lands in their lap often enough to buy back into the game, and the endgame they would not survive under the text becomes a slow, countable arc to the fixed end-time. They gain the most, directly, and hold the least say over what the rules say; the group's solicitude for them is the reason the corrections exist.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, elimination_vulnerable_players, beneficiary,
    moderate, immediate, constrained, local).

% Plays to win under the printed economy: bids hard at auction, assembles monopolies, and drives toward the decisive endgame the rulebook describes. Under the house rules his tools are blunted — the pot refunds his victims, the GO bonus dilutes his rent pressure, and his wins are discounted afterward ('you only won because we played strict tonight'). His objections are heard at the start of each session and outvoted; he stays because the group is his friends, and the strict game he prefers is playable anywhere except with the people he wants at his table.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_minded_players, payer,
    moderate, biographical, constrained, local).

% Authored the printed economy — the auctions, the rent spiral, the elimination endgame — as a designed system, and in the game's deepest lineage as a demonstration of what rent concentration does. At every table playing house rules, that design is silently overruled; they have no seat, cannot object, and the rulebook they published is cited at them by rule-literal players as though it were still in force anywhere.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_designers, excluded,
    institutional, generational, trapped, global).

% Study the correction layer as a phenomenon: surveys showing most players do not play by the printed rules, the cross-cultural persistence of the Free Parking payout, the endgame-length problem in the printed economy. They describe what the corrections do and what they cost across thousands of tables without holding a seat at any of them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_theory_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mixed-group evening problem: one shared activity that a wide range of skill and luck can stay inside for a whole session. The house-rule set coordinates expectations in advance (everyone knows the pot pays out, everyone knows the game ends at the fixed time), removes the need to renegotiate mid-game, and converts the printed game's early-elimination dynamics into a bounded arc that keeps the weakest players consequential until the sunset.
% TRANSFER_FUNCTION: Moves purchasing power inside the game from the bank's accumulated pot and from would-be bankruptcy outcomes to the trailing players — a modest, repeated redistribution that delays elimination. It also moves the evening's outcome-determination away from the printed game's rent-accumulation logic toward a time-bounded asset count, and moves the competitive player's preferred mode of play (strict text, auctions, decisive endgame) into disuse for the night.
% ABSENT_VOICES: The game's designers, and the printed text itself as an authored position, would object that the corrections break the designed economy and mask what the game demonstrates; they have no seat at the table and no standing in the pre-game vote. Also absent: former group members who stopped coming over rules disputes in past seasons — their absence is part of why tonight's consensus looks unanimous.
% DISAPPEARANCE_RATIONALE: Under the printed rules the weakest players are eliminated within the first two hours; they would stop attending, the host's evening would contract to the two or three players who enjoy the decisive endgame, and the group would either renegotiate a new correction layer within a few sessions or abandon the game for one whose text does not need correcting. The table's social economy visibly depends on the regime.
% FOUNDING_PROBLEM: The printed rulebook's elimination economy: strict play reliably ejects the weakest-positioned players early and then grinds through a long, decided endgame — a coordination failure for a group convened for a shared evening rather than a ranking. The house-rule set was adopted to keep everyone playing, and consequential, until the evening's natural end.
% FOUNDING_PROBLEM_CORROBORATION: The payer seat attests the mechanism while disputing its valence: the tournament-minded player concedes that under strict rules the weakest player is usually out by hour two — he calls that the game working, not a problem. Outside the group entirely, the game's design history corroborates the text's harshness: the elimination endgame is a designed feature with a documented pedagogical lineage, which is why the printed economy reliably produces the problem the corrections answer. No one disputes that the mechanism exists; the dispute is over whether it is a defect.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).
:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.40 is moderate and concentrated: the regime taxes the tournament player's preferred mode of play (auctions blunted, rent pressure softened, wins discounted) and taxes textual fidelity itself, but every beneficiary seat pays little and the payer's exits — leaving, or proposing a strict-rules night — remain open, which caps how much the arrangement can take. Suppression 0.32 is real but mild: majority vote, social management of the recurring objector, and the framing of strict play as 'not what game night is for'; there are no structural barriers to exit or to proposal. Theater 0.24: the regime is mostly functional — the evening demonstrably survives because of it — but ritual accretes late in the evening (the jackpot ceremony, the 'anyone can win' performance over a decided board), peaking at hour three and easing at the count-up. Accessibility_collapse 0.30: strict play is one sentence away, other games are on the shelf, absence is always available; alternatives are not collapsed. Resistance 0.38: the tournament player objects every session, cites the rulebook aloud, and occasionally extracts concessions (a stricter end-time; a no-jackpot trial that failed and was dropped). Scaffold logic: the regime's justification is the transition — table set to graceful end — it carries an explicit sunset (the fixed end-time/asset count), and it is re-adopted, re-litigated, and re-earned each session. The measurement series run on one shared five-point grid (hours 0–4 of a typical evening) with all three metrics authored at every point; suppression_requirement is tracked because the narrative specifically traces enforcement effort — the consent-maintenance work the majority must perform rising toward the endgame — not merely extraction. Coordination type is attachment_coordination: the regime's primary function is keeping the group's shared activity (and the bonds it maintains) alive; the attachment framing is not cover here — the group re-adopts the regime because the evening fails without it — though the redistribution_efficacy omega guards the gaming risk that the redistribution component is ritual. Spatial scope is the table (local), which keeps verification cheap and caps scope amplification of extraction; the designers' and analysts' global scopes reflect their relationship to the text, not to this table's arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same table. From the casual majority's seat the regime is coordination they would re-choose from a menu. From the tournament player's seat the same structure is enforced extraction of the game he is good at, renewed by a vote he loses every time. From the host's seat it is the tool that makes the event work, and the sunset is hers to call. The engine computes per-seat classifications from role, power, and exit; the authored scaffold claim does not adjudicate among them. The divergence is sharpest on exit options: identical social setting, but the majority's exit (any other way to have an evening) is cheap while the tournament player's exit (the strict game exists elsewhere, but the friends are here) is constrained — power differs here not by global standing but by what the arrangement is FOR each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: casual_social_players and elimination_vulnerable_players are subsidized by the regime (low d — it funds their evening and their solvency), and the host is dual-positioned (agenda_setter administering the arrangement, with a beneficiary secondary role — she both runs it and collects its success). tournament_minded_players is the declared victim and the principal target (high d — he pays in diluted competition and overridden preference, and his constrained exit keeps him nearer the full-target end than a mobile payer would sit). game_designers is excluded rather than targeted: their cost — a silently overruled text — is real but off-table, and they are not coordinated by the regime. game_theory_analysts is the analytical seat. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already yield the structural relationships, and overrides would duplicate what the derivation computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification blocks two symmetric mislabels. As pure extraction: the regime does take from an identifiable payer, but the taking is consent-renewed each session, the sunset is explicit, removal is one declaration away (fixing_cost: cheap), and no seat captures the gains (gain_flow: diffuse) — the coercion here is a recurring majority vote among friends, not the suppression of exits. As pure coordination: the arrangement is not costless — it has a standing payer whose preferred mode of play is systematically overridden, which is asymmetric by construction. The load-bearing scaffold fact is sunset-plus-re-adoption: the regime persists as a sequence of temporary adoptions, each re-earned when the group re-litigates the rules at the table's edge. The mandatrophy risk runs the other direction: if 'shall we?' hardens into 'we've always played with the jackpot,' the correction layer fossilizes into unexamined tradition — a drift the sunset_readoption_drift omega tracks and the drift_state (practice_drift, minor, unacknowledged) records. founding_problem_status is live: the text still mandates the harsh endgame every session, so the founding problem recurs and is re-solved each time — the arrangement has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the social_scaffold_reading of the monopoly_rulebook kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No empirical resolution — the readings are context-partitioned positions (classroom versus table, ranking versus evening). Resolution would require the parties to accept a single context as privileged, which is a values settlement, not a measurement.',
    'Under the extraction-demo reading, this same table''s house-rule regime is a suppression mechanism preventing the lesson from landing — high epsilon, victims re-authored as everyone the lesson is for, type pulled toward snare. Under the tournament-orthodoxy reading the regime is a defect corrupting competitive depth — victims re-authored as competitive integrity itself, type pulled toward snare or piton. The disagreement is located in the normative status of the text-mandated endgame: defect, payload, or definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a three-reading kernel; each sibling would re-author epsilon, victims, and type for the same printed text.').

omega_variable(
    sunset_readoption_drift,
    'Does per-session re-adoption preserve the regime''s transitional character, or has the ritual of re-adoption hardened into a steady-state tradition the group no longer re-litigates?',
    'Track the table''s rule conversation across sessions: a live transitional arrangement re-opens the question (''shall we play the pot tonight?''); a fossilized one treats the rules as fixed and manages dissent instead. Observable proxies: whether the rules are announced or asked, amendment frequency, and whether new members are told the rules or invited into setting them.',
    'If re-litigation has died, the arrangement is no longer transitional support but a standing institution — reclassify from scaffold toward rope (if consent remains genuine) or piton (if maintenance has turned theatrical and the objector has stopped objecting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_readoption_drift, empirical, 'Whether the sunset-and-re-adopt cycle still runs, or the transitional regime has quietly become a steady state.').

omega_variable(
    redistribution_efficacy,
    'Do the liquidity injections actually change elimination timing and final outcomes, or do they merely lengthen a foregone conclusion — redistribution as ritual rather than function?',
    'Compare win rates, elimination times, and asset spreads across the group''s house-rule sessions and any strict-rules sessions; if the pot''s recipients almost never convert windfalls into competitiveness, the redistribution is theatrical and the coordination claim narrows.',
    'If redistribution never redirects outcomes, part of the regime''s coordination function is performance: theater_ratio and epsilon rise, and the justification narrows from ''keeps everyone consequential'' to ''keeps everyone seated''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_efficacy, empirical, 'Whether house-rule redistribution functions or merely performs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the tournament player''s conformity structural (a recurring majority vote he always loses) or internalized (he has stopped proposing strict rules because the proposal has become socially costly)?',
    'Post-exit suppression trajectory: observe his rule preferences at other tables and in ranked online play. If he plays strict and argues for strict elsewhere, the suppression was situational to this table''s majority; if he defends house rules even where they are not enforced, the suppression has internalized.',
    'If internalized, the arrangement''s effective suppression exceeds the structural measure — the payer carries the overridden preference out of the room, and its true cost to him is higher than the vote margin suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression for the payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mono_tr_t1, monopoly_rulebook__social_scaffold_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(mono_tr_t2, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(mono_tr_t3, monopoly_rulebook__social_scaffold_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.24).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mono_be_t1, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 1, 0.33).
narrative_ontology:measurement(mono_be_t2, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2, 0.37).
narrative_ontology:measurement(mono_be_t3, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mono_su_t1, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 1, 0.24).
narrative_ontology:measurement(mono_su_t2, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement(mono_su_t3, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(mono_su_t4, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 4, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Monopoly rulebook' covers at least three structurally distinct claims, decomposed per the epsilon-invariance principle into three linked stories: this social-scaffold reading (moderate epsilon, transitional correction layer, claimed scaffold); the extraction-demo reading (the text as a true demonstration of rent concentration, house rules as suppression of the lesson — high epsilon); and the tournament-orthodoxy reading (the text as the legitimate competitive framework, house rules as noise — an authority claim with ranking stakes). Each story carries its own epsilon, beneficiary/victim structure, and type; they are linked here because the same printed text is the object each reads, and the text-authority claims are cited as evidence within the sibling readings. The epsilon values differ because the readings disagree about the normative status of the text-mandated endgame — not because one constraint is being measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
