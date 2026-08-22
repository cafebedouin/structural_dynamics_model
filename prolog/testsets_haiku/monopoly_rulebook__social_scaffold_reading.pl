% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook_social_scaffold_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Monopoly Rulebook as Social Coordination Scaffold (House-Rules Reading)
 *   domain: social/game-theory/institutional-design
 *
 * SUMMARY:
 *   Under strict Monopoly rulebook text, rapid wealth concentration leads to
 *   cascading player bankruptcies, transforming what began as a 1-2 hour game
 *   into a 4+ hour slog with most players eliminated and watching. Casual
 *   player coalitions have developed compensatory house rules: free parking
 *   payouts, interest redistributions to losers, modified rent caps, and
 *   negotiated property-trading thresholds. This reading frames the
 *   collective authority to modify rules as a temporary scaffold — a
 *   coordination mechanism that preserves group cohesion and all-player
 *   engagement during the game's actual duration (the stated sunset: when
 *   everyone goes home, the rules revert or dissolve). This reading is ONE
 *   interpretation of the Monopoly rulebook kernel; it stands against
 *   extraction_demo_reading (which reads the text as intentional
 *   wealth-concentration pedagogy) and tournament_orthodoxy_reading (which
 *   holds the text immutable for competitive validity). The claim and metrics
 *   are authored independently: this reading CLAIMS the constraint is a
 *   scaffold (coordination with declared sunset) while metrics show moderate
 *   extraction (0.42) and persistent suppression requirement (0.38),
 *   indicating the house-rule coalitions must actively negotiate and enforce
 *   their modifications against both text-orthodoxy objections and emergence
 *   of new eliminations.
 *
 * KEY AGENTS:
 *   - player_coalition: collectively author and enforce house rules to preserve all-player engagement
 *   - early_eliminated_player: receives redistribution via house rules, remains in game longer
 *   - rent_extractor: dominant player whose advantage is blunted by modified rent structures
 *   - casual_observer_host: initiates and proposes house rules; moderate power over which rules apply
 *   - text_orthodoxy_authority: excluded seat — tournament players, designers, rule-purists who would object that house rules corrupt the design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.38).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook as Social Coordination Scaffold (House-Rules Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "social/game-theory/institutional-design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '28d5449d-2fcd-4f8d-ad81-66088fb18895').
narrative_ontology:cs_kernel_codification('28d5449d-2fcd-4f8d-ad81-66088fb18895', fixed_text).
narrative_ontology:cs_authority_grounding('28d5449d-2fcd-4f8d-ad81-66088fb18895', extraction).
narrative_ontology:cs_interpretation_layer_present('28d5449d-2fcd-4f8d-ad81-66088fb18895').
narrative_ontology:cs_reading_relation('28d5449d-2fcd-4f8d-ad81-66088fb18895', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('28d5449d-2fcd-4f8d-ad81-66088fb18895', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('28d5449d-2fcd-4f8d-ad81-66088fb18895', foundational, coordination_over_fidelity).
narrative_ontology:cs_axiom_status(coordination_over_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('28d5449d-2fcd-4f8d-ad81-66088fb18895', coordination_over_fidelity, deontological).
narrative_ontology:cs_axiom('28d5449d-2fcd-4f8d-ad81-66088fb18895', foundational, social_group_cohesion_permissible_grounds_for_rule_modification).
narrative_ontology:cs_axiom_status(social_group_cohesion_permissible_grounds_for_rule_modification, holdable).
narrative_ontology:cs_axiom_grounding('28d5449d-2fcd-4f8d-ad81-66088fb18895', social_group_cohesion_permissible_grounds_for_rule_modification, instrumental).
narrative_ontology:cs_reference_frame('28d5449d-2fcd-4f8d-ad81-66088fb18895', text_authority_modifiable_by_player_consent).
narrative_ontology:cs_drift_state('28d5449d-2fcd-4f8d-ad81-66088fb18895', contemporary_casual_monopoly_play, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('28d5449d-2fcd-4f8d-ad81-66088fb18895', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, player_coalition).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, early_eliminated_player).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, rent_extractor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The group of players gathered to play. They collectively author and enforce house rules to keep the game socially playable across 3+ hours. They benefit from prolonged engagement, meaningful participation for all players, and preservation of the group's social bonds across the game's duration. They set the actual ruleset by negotiating modifications that prevent early elimination and preserve turn-taking opportunities. Their exit is to stop playing or switch games entirely.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, player_coalition, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, player_coalition, agenda_setter).

% The published rulebook, tournament organizers, game designers, and the competitive gaming tradition that treats the text as immutable. This seat is structurally absent from the casual play context — tournament players and competitive purists would object to house rules on grounds of rule integrity and comparative validity, but they are not present when a casual group plays. Their exclusion is what allows the social scaffold to persist without immediate contestation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, text_orthodoxy_authority, excluded,
    institutional, generational, trapped, global).

% Under strict text rules, faces elimination in the first 45-90 minutes of a 3+ hour game, leaving them with nothing to do but watch. House rules redistribute their position through interest payments, free parking bonuses, or reduced rent — effectively giving them structured re-entry mechanisms. They pay by having their fate partially controlled by collective rule-modification rather than their own strategic choices. Their exit is to leave the game entirely or refuse future play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, early_eliminated_player, payer,
    powerless, immediate, trapped, local).

% The player who, under strict text rules, would accumulate property and extract rents that drive others to bankruptcy quickly. House rules cap this player's accumulation rate through modified rent structures, interest redistribution, or property trading thresholds. They pay by having their strategic advantage blunted. Their constrained exit reflects that leaving now admits defeat; continuing under modified rules means playing a diluted version of their dominant strategy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rent_extractor, payer,
    powerful, biographical, constrained, local).

% The person who hosts or initiates the game, responsible for proposing and maintaining house rules. They benefit from a game that keeps all players engaged. They set the enforcement frame by deciding which rules apply tonight. Their exit is to stop hosting; their power is moderate because any player can refuse to show up or propose alternate rules.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_observer_host, agenda_setter,
    moderate, biographical, mobile, local).

% A player in a formal tournament or ranking context who sees house rules as cheating or rule-corruption. They occupy an analytical seat — they see the same text rulebook, but their stake (ranking validity, comparative legitimacy) is absent in casual play. This reading does not describe their constraint; it excludes them by design. They would have standing to object if present, but casual groups do not invite them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_tournament_entrant, observer,
    institutional, biographical, analytical, global).

% The original designer/publisher. Their stake is canon authority and pedagogical intent. They observe that players modify their text, which contradicts their design specifications. This reading assigns them an analytical seat because casual play does not solicit their consent or adjudication — they are outside the play context.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_designer, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, player_coalition).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustain a multiplayer game session where all participants remain engaged and retain meaningful agency across 3+ hours of play. The coordination problem: strict text rules produce rapid elimination and create long waiting periods that undermine social cohesion. House rules solve this by preserving turn-taking, redistributing wealth before bankruptcy, and explicitly trading off text fidelity for group-sustained play.
% TRANSFER_FUNCTION: Moves authority from the published text to the player coalition. Players collectively transfer rule-setting power from designer-to-player to player-to-player, and in doing so, they move player elimination risk from individuals (bankruptcy) to the group (negotiated acceptable duration). Players also transfer the experience: from zero-sum competitive demonstration to collaborative social maintenance.
% ABSENT_VOICES: Tournament players, competitive ranking authorities, game designers, and rule-orthodoxy purists. These seats would argue that house rules invalidate the game's pedagogical point (demonstrating wealth concentration), compromise ranking validity, and corrupt the design intent. They are structurally excluded because casual play does not solicit their presence or approval — they have no seat at the table.
% DISAPPEARANCE_RATIONALE: If the coalition's collective rule-modification authority disappeared and only text rules were permitted, players would stop playing Monopoly in casual contexts — they would choose games with shorter duration, gentler elimination mechanics, or explicit social-play variants. The publishing industry has responded to this exact dynamic by creating Monopoly house-rule variants (Speed, Party, Team versions) and explicitly sanctioning non-text play. The casual Monopoly ecosystem would collapse into the tournament subset or fragment into house-rule-based quasi-games the designer no longer recognizes.
% FOUNDING_PROBLEM: Early Monopoly play revealed that strict text rules produce a socially uncomfortable endgame: one player's rapid wealth concentration triggers cascading bankruptcies, leaving most players watching rather than playing. Casual groups discovered that injecting liquidity (free parking money, interest payments to losers, modified rent structures) and slowing elimination preserved social playability and kept all participants engaged.
% FOUNDING_PROBLEM_CORROBORATION: Casual players, game researchers studying Monopoly variants (Parker Brothers eventually published Speed Monopoly and Team Monopoly acknowledging this exact dynamic), and sociological studies of house rules in board games all corroborate that the founding problem persists: strict text rules produce social friction in non-competitive contexts. Tournament organizers and designers attest that the founding problem is DESIGN INTENT rather than a problem — they argue elimination and wealth concentration IS the pedagogical point. This divergence is the core disagreement the kernel contest captures.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the player coalition exercises authority to REVERSE wealth concentration that the text mandates. The text produces high extraction (rapid bankruptcy, elimination); house rules inject redistribution mechanisms (free parking, interest payments, modified rents) that dampen extraction. However, extraction does not fall to near-zero because the coalition still plays a modified version of a game where property ownership confers advantage and wealth inequality persists — they just slow its progression. Suppression stays elevated (0.38) because the coalition must actively enforce house-rule compliance: a player who accumulates property too quickly will resist modified rent rules; a player who reaches bankruptcy will question the free-parking intervention. The coalition has to suppress both text-orthodoxy objections ("we're playing the real game") and cheating attempts (refusing house rules mid-game). Theater ratio (0.44) reflects that a significant portion of the house-rule enforcement is performative negotiation — players explicitly discussing and re-agreeing to modified rules mid-game, framing it as social maintenance rather than rule-following. The measurement trajectory shows extractiveness rising slightly through the game (as property concentrates despite house rules) then flattening when bankruptcy is averted; suppression rises as eliminations approach, then stabilizes as the coalition's authority to modify rules is exercised. Accessibility collapse (0.62) reflects that once players understand the text rules produce social friction, the alternatives (strict adherence or quitting) are visible but unattractive; house rules collapse alternatives by being the only path to sustained play. Resistance (0.58) indicates meaningful push-back: some players prefer strict text rules as more 'authentic,' some resist redistribution as unfair, some object to the overhead of mid-game rule negotiation.
 *
 * PERSPECTIVAL GAP:
 *   From the player_coalition's seat, the constraint appears as legitimate coordination (we modified the rules to keep everyone playing). From the text_orthodoxy_authority's excluded seat, the constraint appears as illegitimate rule-corruption (you are cheating the game's design). The engine computes a per-seat type for each: the coalition seat likely computes as rope (coordination benefit, limited suppression, players voluntarily agree to rules). The tournament seat, if it were admitted, would compute as snare (the modifications trap players in an agreement that dilutes their skill advantage). The gap is structural, not measurable: the same constraint produces different types from different seats precisely because the seats' power and exit options differ. This reading instantiates the coalition's frame; tournament_orthodoxy_reading instantiates the tournament seat's frame. Both are structurally sound; they coexist because the seats are geographically separated (casual play vs. tournaments).
 *
 * DIRECTIONALITY LOGIC:
 *   The player_coalition occupies a beneficiary-agenda_setter seat (d near 0.2–0.3): they benefit from group cohesion and prolonged engagement, and they set the rules. Early_eliminated_player is the primary target under strict text rules (d near 0.8–0.9 under text, but drops to d near 0.4–0.5 under house rules because redistribution reshapes their position); under this reading, the house-rule modification moves them toward symmetric (d ~ 0.5). Rent_extractor bears a cost (their dominant position is constrained), so their directionality under house rules rises (d near 0.6–0.7) compared to text (d near 0.1 — they benefit hugely). Text_orthodoxy_authority is excluded, so its directionality is analytical (d ~ 0.5 by default exclusion). The reading's central move is that the coalition's collective rule-modification shifts the directionality landscape from high concentration (text: text_beneficiary d~0.0, losers d~1.0) to moderated concentration (house rules: coalition d~0.3, losers d~0.5, rent_extractor d~0.6). No directionality override is needed; structural derivation from beneficiary/victim declarations + exit options produces the correct shape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is that text rules produce early elimination and social friction. Under this reading, the founding problem remains LIVE — even with house rules, players must actively negotiate and redistribute to avoid it. The constraint is therefore a temporary scaffold, not a permanent solution. The has_sunset_clause: true declaration captures this: players adopt house rules for the duration of tonight's game; tomorrow, if a new coalition plays, they re-negotiate from scratch or opt into text rules. The constraint does NOT persist because players want it to; it persists because they want to play together tonight and the text rules make that unpleasant. Mandatrophy is resolved (or deferred) by the explicit sunset: when the game ends, the house-rule authority dissolves. This reading avoids misclassifying a coordination mechanism (scaffolding) as pure extraction (snare) or permanent coordination (rope) by declaring its temporal boundary upfront.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the rulebook text a natural law for play (immutable, discovered, canonical) or a constructed convention whose authority derives only from agreement?',
    'Kernel contest resolution: if extraction_demo_reading (text as pedagogy) wins consensus, the text is natural law; if social_scaffold_reading (text as modifiable convention) wins, the text is constructed. The contest is not resolvable by measurement — it is a value/frame question settled by which authority structure the player coalition endorses.',
    'If text is natural law, the house-rule modifications are illegitimate deviations; if text is constructed convention, house rules are legitimate local variations. This bifurcates type assignment: natural-law reading -> mountain or rope; constructed-convention reading -> scaffold or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether rulebook authority is discovered or constructed.').

omega_variable(
    suppression_internalization_post_exit,
    'Does a player who exits the casual coalition continue to experience suppression (guilt, internalized norm) when playing Monopoly under text rules later, or does internalized suppression dissolve with the group?',
    'Interview ex-players 3–6 months post-exit; assess whether they report reduced rule-modification impulses when playing with new groups, or whether they actively prefer house-rule variants. Measure whether house-rule preference is stable or group-dependent.',
    'If suppression persists, the constraint has residual extractive force (internalized rule adoption). If suppression dissolves, the constraint was entirely structural (group-dependent coordination norms). Persistence would raise theater_ratio and suggest the coalition partly internalizes the constraint; dissolution would lower both, suggesting pure structural coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_post_exit, empirical, 'Whether suppression mechanism is structural or internalized.').

omega_variable(
    scaffold_sunset_enforcement,
    'How reliably does the coalition''s rule-modification authority actually expire when the game ends? Do players carry house-rule expectations into future games with the same or different coalitions?',
    'Track the same coalition across multiple play sessions; document whether house rules from session N persist as defaults in session N+1, or whether they must be re-negotiated each time. If they persist as defaults, the sunset is not enforced and the constraint is becoming permanent.',
    'If sunset is reliably enforced, the constraint is genuinely temporary (true scaffold). If house rules become sticky defaults across sessions, the constraint is becoming institutional (transitional rope or permanent tangled_rope). Type assignment depends on whether the sunset is real or nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_enforcement, empirical, 'Whether the declared sunset clause is actually enforced or becomes a nominal boundary.').

omega_variable(
    kernel_reading_coexistence_stability,
    'Can social_scaffold_reading, extraction_demo_reading, and tournament_orthodoxy_reading coexist in the same player coalition, or does adoption of one reading foreclose the others?',
    'Observe mixed-attitude coalitions (some players believing in pedagogical intention, others believing in social modification authority); track whether they can sustain play together or whether disagreement forces coalition dissolution or rule-choice escalation.',
    'If coexistence is stable, the readings are genuinely coexists_with peers (different frames for different contexts). If disagreement forces choice, some readings foreclose others. Current analysis assumes coexistence via spatial separation (tournaments vs. casual); if same coalition adopts multiple readings, the stability claim breaks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_stability, empirical, 'Stability of coexisting readings within a single player coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_scaffold_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2, 0.43).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.44).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t8, monopoly_rulebook__social_scaffold_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t10, monopoly_rulebook__social_scaffold_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.44).

% Extraction over time
narrative_ontology:measurement(monopoly_social_scaffold_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(monopoly_social_scaffold_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(monopoly_social_scaffold_be_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(monopoly_social_scaffold_be_t8, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(monopoly_social_scaffold_be_t10, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(monopoly_social_scaffold_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_social_scaffold_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(monopoly_social_scaffold_su_t2, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2, 0.36).
narrative_ontology:measurement(monopoly_social_scaffold_su_t4, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(monopoly_social_scaffold_su_t6, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(monopoly_social_scaffold_su_t8, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(monopoly_social_scaffold_su_t10, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(monopoly_social_scaffold_su_t12, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 12, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel decomposes into three structurally distinct constraints, one per reading. social_scaffold_reading frames the rulebook as temporary coordination scaffolding (moderate extraction, active enforcement, declared sunset). extraction_demo_reading frames the rulebook as intentional pedagogical extraction (high extraction, no enforcement needed, no sunset). tournament_orthodoxy_reading frames the rulebook as immutable competitive authority (moderate extraction, enforcement of text fidelity, no sunset). These three readings share the same object (the published rulebook text) but differ radically in how they interpret the text's role, beneficiary structure, and legitimacy. They are linked via affects_constraints because the social_scaffold reading's persistence depends on excluding the tournament seat — if tournament players join, the reading forecloses or becomes contested. The readings are not observable variations of one constraint; they are distinct constraints instantiating different authority structures grounded in the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
