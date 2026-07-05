% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Monopoly Rulebook — Social Scaffold Reading (House-Rule Coordination Buffer)
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   This story is the social-scaffold reading of the Monopoly rulebook
 *   kernel: house rules (free parking jackpots, loans from the bank, refusal
 *   to auction) are read here as a deliberate, socially-necessary buffer that
 *   prevents the printed text's harsh, fast elimination mechanic from ending
 *   a group evening too early or too bitterly. The coordination function is
 *   real — a mixed-skill, multi-player social event needs everyone engaged
 *   for most of its length — and the extraction is moderate: leading players
 *   are structurally denied the closure and payoff the printed rules would
 *   give them, and rules-literalist players are socially pressured out of
 *   insisting on the text. This is a scaffold, not a rope, because the
 *   buffer's justification is explicitly the transition through a long,
 *   uneven social evening to a natural close, not a steady-state alternative
 *   ruleset meant to persist as the 'real' rules; the game is understood by
 *   all parties to sunset back to the printed text (or simply end) once the
 *   evening's social purpose is served.
 *
 * KEY AGENTS:
 *   - casual_host_household: agenda_setter (moderate/constrained) — administers house rules informally
 *   - social_group_cohesion: beneficiary, non-agent — the preserved good
 *   - slower_eliminated_players: beneficiary+payer (powerless/constrained) — kept liquid, game runs long
 *   - leading_players_denied_closure: payer (moderate/constrained) — dominant position diluted
 *   - rules_literalist_players: payer+excluded (powerless/trapped) — preference for text unvoiced
 *   - board_game_publisher: observer (institutional/analytical) — no stake in home enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.38).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.22).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook — Social Scaffold Reading (House-Rule Coordination Buffer)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game theory / social coordination / institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'f4de5e85-fdcd-4e99-beb4-1a27efbb4159').
narrative_ontology:cs_kernel_codification('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', fixed_text).
narrative_ontology:cs_authority_grounding('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', practice).
narrative_ontology:cs_interpretation_layer_present('f4de5e85-fdcd-4e99-beb4-1a27efbb4159').
narrative_ontology:cs_reading_relation('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', foundational, social_continuity_outranks_textual_fidelity).
narrative_ontology:cs_axiom_status(social_continuity_outranks_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', social_continuity_outranks_textual_fidelity, instrumental).
narrative_ontology:cs_axiom('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', secondary, house_rules_are_provisional_not_canonical).
narrative_ontology:cs_axiom_status(house_rules_are_provisional_not_canonical, holdable).
narrative_ontology:cs_axiom_grounding('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', house_rules_are_provisional_not_canonical, conventional).
narrative_ontology:cs_reference_frame('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', printed_rulebook_hard_elimination).
narrative_ontology:cs_drift_state('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', contemporary_casual_play_norm, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f4de5e85-fdcd-4e99-beb4-1a27efbb4159', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, slower_eliminated_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_host_household).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, leading_players_denied_closure).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, rules_literalist_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, slower_eliminated_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the table's actual operating rules before play: free parking jackpot, no-auction-on-refusal, loans from the bank when a player is short. Administers enforcement informally by social pressure and by owning the physical game. Wants the evening to stay pleasant and to end at a natural stopping point, not at the harsh liquidation the printed rules specify.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_host_household, agenda_setter,
    moderate, immediate, constrained, local).

% Not an actor itself, but the thing the house rules protect: the ongoing willingness of this group of friends/family to play games together again. Preserved by slowing elimination and injecting liquidity so no single evening produces a humiliated loser who leaves early or resents the group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_cohesion, beneficiary,
    moderate, biographical, analytical, local).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).

% Players with early bad luck or weak trading skill who, under the printed text, would be bankrupted and eliminated within the first hour. House rules (bank loans, free parking cash, informal rent forgiveness) keep them liquid and in the game for the full session. They pay a smaller cost: the game runs long past the point their position is actually competitive, and they know it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, slower_eliminated_players, beneficiary,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, slower_eliminated_players, payer).

% Players who built a dominant board position through legitimate trading and property development. Under the text, their advantage should convert to a win within a bounded timeframe via forced bankruptcy of opponents. House-rule liquidity injections repeatedly rescue the trailing players, extending the game by hours and diluting the competitive payoff of the leading position. Exit means refusing to host or refusing to play by these house rules, which costs them standing in the group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, leading_players_denied_closure, payer,
    moderate, immediate, constrained, local).

% Players who came to play the game as written — auctions on refused purchases, no bailouts, hard bankruptcy — and find the social contract of the table has quietly substituted a different game. They can object, but objecting reads as poor sportsmanship or as prioritizing 'winning' over the group's fun; their preference for textual fidelity is the absent voice at most casual tables.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rules_literalist_players, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, rules_literalist_players, excluded).

% Publishes the official rulebook that specifies hard bankruptcy, mandatory auctions, and no free-parking jackpot. Has no enforcement mechanism over home play and no stake in whether any given table follows the text; its interest is in the printed game being recognizable as itself, not in adjudicating house variants.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, board_game_publisher, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a multi-hour social game socially viable by preventing the printed rules' harsh, fast elimination mechanic from ending the evening early for weaker or unluckier players, and by giving the group a shared, informally-agreed alternative ruleset that everyone can follow without renegotiating mid-game.
% TRANSFER_FUNCTION: Moves competitive advantage away from players who achieved a dominant board position under the printed rules and toward trailing/eliminated players, via cash injections (free parking, bank loans, rent forgiveness) that the text does not authorize. The transfer is liquidity and turn-survival, not victory itself.
% ABSENT_VOICES: Rules-literalist players who would prefer the printed endgame — hard bankruptcy, mandatory auctions — are structurally present at the table but functionally excluded from setting the actual rules in play; raising the objection carries a social cost framed as poor sportsmanship, so the preference rarely surfaces as an explicit veto.
% DISAPPEARANCE_RATIONALE: If the house rules vanished and the printed text were enforced strictly, most casual games would end in under 90 minutes with one or two players eliminated early and visibly resentful; the social event the group actually wants (a multi-hour shared activity ending near a natural close, like bedtime or dinner) would not occur in the same form. The group would either abandon the game, adopt a different game, or reconstruct equivalent house rules almost immediately.
% FOUNDING_PROBLEM: The printed Monopoly ruleset, played as written, produces fast, harsh, decisive elimination — a good demonstration of rent-extraction dynamics but a poor multi-hour social activity for a mixed-skill group that wants everyone engaged for most of the evening.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated informally by decades of near-universal convergent house-rule adoption across unrelated households and cultures (free parking, no-auction, loans-from-bank appear independently in surveys of casual play and in the publisher's own periodic public acknowledgment that house rules are near-universal); the publisher itself, an outside party with no stake in extending sessions, has commented publicly that most owners do not play by the strict text. No party benefiting from the scaffold is the sole source of this attestation.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness sits at 0.38 by interval end (moderate band, consistent with the expected 0.31-0.55 delta for this reading): real value moves from leading players to trailing ones via unauthorized liquidity injections, but the amount involved per player is small relative to full elimination stakes and no party is bankrupted or permanently harmed. Suppression is low (0.22) because exit is mostly social-cost-based, not coercive — a literalist player can leave the table, just not without friction. Theater ratio rises modestly over the session (0.10 to 0.28) as free-parking jackpots and informal loans accumulate turns and the game increasingly runs on social-maintenance activity (extending play, redistributing cash) rather than the printed competitive mechanic. Accessibility collapse (0.35) and resistance (0.40) are mid-range: alternatives (playing strictly, or not playing) remain visible and are occasionally exercised, unlike a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The host/agenda-setter administers but does not extract for themselves; d sits near symmetric. Social group cohesion is a non-agent beneficiary and is excluded from directionality math. Slower/eliminated players are structural near-beneficiaries of the liquidity mechanism (low d) but bear a secondary cost of an artificially prolonged, low-stakes endgame. Leading players are the clearest targets (higher d): their legitimately earned board dominance is diluted by rules not in the text. Rules-literalist players are targets of suppression rather than extraction — their cost is social, not financial.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents this constraint from being mislabeled as pure extraction (as the sibling extraction_demo_reading would treat the same physical table) by requiring the sunset condition and the beneficiary declaration to both be present: house rules here explicitly exist to get the group through a long uneven evening, not to permanently supplant the printed game as a competing 'real' ruleset. If the house rules calcified into an unquestioned alternate canon that no one at the table remembered was optional, the classification would drift toward tangled_rope or piton — the omega on rule ossification below addresses exactly this drift risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_vs_permanent_house_canon,
    'Do the house rules at a given table remain understood as a temporary social buffer (scaffold) or have they calcified into an unquestioned permanent alternate ruleset that no participant remembers is optional (which would push the constraint toward tangled_rope or piton)?',
    'Ask participants directly whether they know the printed rules differ, and whether they would consider playing the printed version; a table where no one recalls the text exists suggests calcification.',
    'If calcified, the has_sunset_clause declaration would no longer be honestly assertible and the classification should move away from scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_permanent_house_canon, empirical, 'Whether the social buffer remains understood as temporary or has become unquestioned permanent practice.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the correct reading of a given real-world Monopoly session''s house rules the social_scaffold_reading (coordination buffer), the extraction_demo_reading (inevitable wealth concentration demonstrated by the printed mechanic), or the tournament_orthodoxy_reading (text as legitimate competitive framework, house rules as noise)? A single physical table''s practice can be read all three ways depending on which party in the room you ask.',
    'No single measurement resolves this — it is a genuine committer-frame ambiguity. Interview all players present about their felt purpose for playing (social evening vs. competitive contest vs. pedagogical exercise) and note whether the readings are held by different players simultaneously at the same table.',
    'The three readings are authored as separate constraint stories per the ε-invariance principle; a single table may in practice instantiate more than one reading among its own participants, which is exactly the structural disagreement the kernel-reading frame is built to surface rather than resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of the three kernel readings applies to a given table is itself contested and may vary by participant.').

omega_variable(
    leading_player_dilution_magnitude,
    'How much competitive value does the free-parking/loan liquidity mechanism actually strip from a legitimately dominant board position, versus how much would that position have converted to a win anyway absent house rules?',
    'Simulation or replay analysis comparing win probabilities for a given mid-game board state under printed rules versus common house rules.',
    'A larger measured dilution would push extractiveness and the payer-seat experience of leading_players_denied_closure higher; a smaller one would support the scaffold''s coordination framing as low-cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leading_player_dilution_magnitude, empirical, 'Quantifying the actual competitive cost house rules impose on players with dominant positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__social_scaffold_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(mono_tr_t80, monopoly_rulebook__social_scaffold_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(mono_tr_t120, monopoly_rulebook__social_scaffold_reading, theater_ratio, 120, 0.23).
narrative_ontology:measurement(mono_tr_t160, monopoly_rulebook__social_scaffold_reading, theater_ratio, 160, 0.26).
narrative_ontology:measurement(mono_tr_t200, monopoly_rulebook__social_scaffold_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement(mono_tr_t240, monopoly_rulebook__social_scaffold_reading, theater_ratio, 240, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(mono_be_t80, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(mono_be_t120, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 120, 0.34).
narrative_ontology:measurement(mono_be_t160, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 160, 0.36).
narrative_ontology:measurement(mono_be_t200, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 200, 0.37).
narrative_ontology:measurement(mono_be_t240, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 240, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__social_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'the Monopoly rulebook / house rules controversy': extraction_demo_reading (the printed text as inevitable rent-extraction demonstration, near-mountain claim about the mechanic), social_scaffold_reading (this file — moderate-epsilon scaffold preserving group cohesion), and tournament_orthodoxy_reading (text as immutable competitive framework, house rules as illegitimate noise). Each reading has a distinct epsilon and distinct beneficiary/victim structure; they are linked here rather than merged because the underlying observable — 'what actually happens at a Monopoly table' — supports genuinely different structural claims depending on which party's stated purpose for playing is taken as authoritative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
