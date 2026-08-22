% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: House-Rule Scaffold Preserving Social Playability of Monopoly
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   This story reads the Monopoly rulebook as a kernel through the lens of
 *   the table that plays it as a social occasion rather than a competitive
 *   contest. Under this reading, the printed elimination mechanics are not
 *   the point of the game as actually convened; they are an obstacle the
 *   group must actively route around with house rules (free parking pot, bank
 *   loans, forgiven rent, deferred bankruptcy, sometimes a soft time-based
 *   ending) in order for the multi-hour session to remain socially playable
 *   for everyone at the table, especially the eventual losers. The constraint
 *   being evaluated here is the house-rule scaffold itself, not the printed
 *   text and not the group's endorsed alternative — ε is authored for the
 *   scaffold's own operation (moderate, rising as the session lengthens and
 *   liquidity injections accumulate), not for some idealized
 *   fully-cooperative game. This is one of three readings of the shared
 *   monopoly_rulebook kernel; the extraction_demo_reading treats the same
 *   text as a pedagogical demonstration of inevitable wealth concentration
 *   where elimination is the necessary and correct outcome, and the
 *   tournament_orthodoxy_reading treats the printed text as the immutable
 *   competitive standard against which house rules are illegitimate noise.
 *   All three readings share the printed rulebook as their kernel but
 *   instantiate structurally distinct constraints with distinct
 *   beneficiary/victim structures and distinct ε.
 *
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
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "House-Rule Scaffold Preserving Social Playability of Monopoly").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game theory / social coordination / institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '103029b3-e53c-4e02-b2c1-c90f4058bd36').
narrative_ontology:cs_kernel_codification('103029b3-e53c-4e02-b2c1-c90f4058bd36', fixed_text).
narrative_ontology:cs_authority_grounding('103029b3-e53c-4e02-b2c1-c90f4058bd36', practice).
narrative_ontology:cs_interpretation_layer_present('103029b3-e53c-4e02-b2c1-c90f4058bd36').
narrative_ontology:cs_reading_relation('103029b3-e53c-4e02-b2c1-c90f4058bd36', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('103029b3-e53c-4e02-b2c1-c90f4058bd36', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('103029b3-e53c-4e02-b2c1-c90f4058bd36', foundational, social_playability_supersedes_textual_fidelity).
narrative_ontology:cs_axiom_status(social_playability_supersedes_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('103029b3-e53c-4e02-b2c1-c90f4058bd36', social_playability_supersedes_textual_fidelity, instrumental).
narrative_ontology:cs_axiom('103029b3-e53c-4e02-b2c1-c90f4058bd36', secondary, prolonged_group_coordination_is_the_games_true_end).
narrative_ontology:cs_axiom_status(prolonged_group_coordination_is_the_games_true_end, holdable).
narrative_ontology:cs_axiom_grounding('103029b3-e53c-4e02-b2c1-c90f4058bd36', prolonged_group_coordination_is_the_games_true_end, conventional).
narrative_ontology:cs_reference_frame('103029b3-e53c-4e02-b2c1-c90f4058bd36', printed_rulebook_literal_elimination_endgame).
narrative_ontology:cs_drift_state('103029b3-e53c-4e02-b2c1-c90f4058bd36', contemporary_casual_household_play, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('103029b3-e53c-4e02-b2c1-c90f4058bd36', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, host_family_or_friend_group).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, rules_literalist_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, eliminated_or_near_bankrupt_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, informal_norm_supersedes_written_text_when_group_survival_is_at_stake).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the house rules at the table before play starts — free parking jackpot, no-auction-of-forfeited-properties, loans from the bank, slower bankruptcy. Administers the game session and could revert to strict rules at any point but chooses not to because the group's continued willingness to play together matters more than any single game's fidelity to the printed text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, host_family_or_friend_group, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, host_family_or_friend_group, beneficiary).

% Get to stay in the game longer, avoid early humiliating elimination, and experience the session as a multi-hour social occasion rather than a fast elimination tournament. Their tolerance for the literal rules is low; without the house-rule buffer several would simply stop playing partway through, which is the outcome the scaffold is built to prevent.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, constrained, local).

% Understand the printed rules well and would benefit strategically from harsh, fast elimination mechanics — auctions on forfeited property, no free bailouts, strict bankruptcy timing. The house rules dilute their competitive advantage and extend a game they may already have functionally won under the text; they bear the cost of the group's coordination preference in the form of a diluted, prolonged, less decisive contest.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rules_literalist_players, payer,
    moderate, immediate, constrained, local).

% Would be knocked out of the game early under strict rules, ending their participation in the shared social event. House-rule liquidity injections (loans, forgiven rent, slower bankruptcy) keep them at the table, which is the primary coordination good the scaffold delivers to the weakest position in the game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, eliminated_or_near_bankrupt_players, beneficiary,
    powerless, immediate, constrained, local).

% Wrote and holds copyright over the printed rules that the house rules deviate from. Has no voice in any given living-room session and no mechanism to object; the printed text's authority is treated by the table as a default to be locally amended, not a binding contract.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, board_game_publisher, excluded,
    institutional, generational, analytical, global).

% Study the gap between the designed mechanics (rapid, decisive elimination) and the played mechanics (near-universal informal amendment) across households, and can characterize the house-rule layer as an emergent social-coordination patch on a a text that fails as a social artifact when followed literally.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_theory_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a group of unequally-skilled or unequally-lucky players at the same table for the multi-hour duration of a session, by injecting liquidity and slowing elimination so no one is expelled from the shared social occasion early enough to sour it for everyone remaining.
% TRANSFER_FUNCTION: Moves in-game liquidity (free parking pots, bank loans, forgiven rent, deferred bankruptcy) from the players who are ahead under strict rules to the players who are behind, and moves competitive decisiveness away from literalist players toward a longer, more socially even session.
% ABSENT_VOICES: The board game publisher, whose printed rules are the deviated-from baseline, has no seat at any given table and no way to object to the informal amendment; rules-literalist players are present but structurally outvoted by the group's preference for continued play.
% DISAPPEARANCE_RATIONALE: If the house-rule scaffold vanished and groups played the literal text, sessions would end faster with earlier, harsher eliminations; several players in most casual groups would stop playing before the game concluded, and repeat play of the game as a social occasion (rather than a strict competitive contest) would decline sharply.
% FOUNDING_PROBLEM: The printed rulebook's fast, decisive elimination mechanics make the game socially unplayable for a mixed-skill group across a multi-hour session: early-eliminated players are stuck watching with nothing to do, which undermines the shared social occasion the game is actually convened for.
% FOUNDING_PROBLEM_CORROBORATION: Widely corroborated outside the beneficiary group itself: game designers and board-game journalists writing about Monopoly's design history (including the publisher's own later 'Speed Die' variant, an acknowledgment that base-rule pacing is a known problem) and academic game-theory commentary on informal norm layers both attest that the strict text produces poor social outcomes at typical table sizes and durations.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.38 at session end) because the house-rule layer does redistribute value away from the players who are ahead under strict rules, but the redistribution is bounded, transparent to all parties at the table, and revocable session-to-session (no persistent institutional lock-in). Suppression is low (0.22): a literalist player can object, refuse to play by the group's rules, or simply not attend future sessions — there is real exit, just social cost attached to it. Theater ratio rises modestly across the session (0.10 to 0.28) as the improvised rules accumulate ad hoc justifications ('it's how we've always played') that increasingly perform tradition rather than solve the liquidity problem directly. Accessibility collapse is moderate (0.35): alternatives (playing strictly, adopting the official Speed Die, quitting early) remain visible and available throughout, they are just socially costly to invoke once the table has settled into its house-rule equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Social group cohesion and casual/near-bankrupt players are the structural beneficiaries: the scaffold exists specifically to keep them at the table and extend their participation, so their directionality sits near the beneficiary end. Rules-literalist players are the payers: the redistribution measurably erodes the competitive advantage they would hold under strict text, and their d sits nearer the target end, though moderated by the fact that they retain real (if socially costly) exit and are not trapped or identity-locked. The publisher is excluded rather than positioned on the beneficiary/victim axis at all — the scaffold operates entirely outside its authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's declared sunset is the end of the game session itself — house rules are renegotiated or dropped from one session to the next and carry no institutional memory beyond the group's habit. This prevents the constraint from being mislabeled as a permanent extractive arrangement: it is a scoped, temporary patch justified entirely by the transition it manages (from 'group starts playing' to 'group finishes playing without anyone storming off'), not by any steady-state distributional goal. Because the founding problem (elimination mechanics that make the printed game socially unplayable at typical table sizes) remains live session after session, the scaffold is reinstantiated rather than institutionalized — it does not accumulate into a permanent extractive structure, though the T17-style question of whether repeated, ritualized house-rule adoption calcifies into inertial tradition independent of its founding function is worth tracking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the social_scaffold_reading''s core premise diverge from the tournament_orthodoxy_reading''s, given that both readings can be held about the same physical game session?',
    'Identify the specific structural element in dispute: whether the printed text''s authority over outcomes is treated as binding-for-ranking-purposes (tournament_orthodoxy) or as a default amendable by the table for coordination purposes (social_scaffold). No empirical test resolves this because it is a disagreement about which function the text is FOR, not about what the text says.',
    'If tournament_orthodoxy''s premise (text authority is immutable for competitive comparison) is adopted as the sole legitimate reading, the social_scaffold_reading''s house-rule layer becomes illegitimate noise rather than a coordination good, collapsing this constraint''s scaffold classification into something closer to a snare against literalist players. If the social_scaffold premise is adopted, tournament orthodoxy''s claim to legitimacy over informally-played home games weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the exact structural disagreement between this reading and the tournament_orthodoxy sibling reading.').

omega_variable(
    sibling_reading_structural_delta,
    'What would the extraction_demo_reading change about this constraint''s structure if adopted instead?',
    'Compare beneficiary/victim sets and epsilon: extraction_demo_reading names the eventual monopolist as beneficiary and the eliminated players as victims of a pedagogically-necessary process, with the harsh endgame as the correct outcome rather than a problem to route around. This reading names social cohesion and near-bankrupt players as beneficiaries and reads the harsh endgame itself as the thing being managed away.',
    'The two readings do not merely disagree evaluatively; they name different beneficiaries and treat the same house-rule deviations as either sabotage-of-the-lesson (extraction_demo view) or essential-social-repair (this view). This is why they are separate constraint stories rather than one story with a measurement parameter — their epsilon, beneficiary sets, and classifications differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents the structural delta to the extraction_demo_reading sibling constraint.').

omega_variable(
    scaffold_versus_ritualized_tradition,
    'Is the house-rule layer genuinely transitional (justified only by getting this particular session to a livable conclusion) or has it calcified into an inertial tradition that persists independent of any given session''s coordination need?',
    'Track whether groups that adopt house rules ever revert to strict play when the social-coordination problem is absent (e.g., a group of equally-skilled competitive players who all want a fast decisive game) — if house rules persist even there, the scaffold has drifted toward piton-like inertia rather than remaining a genuine transitional support.',
    'If house rules persist independent of coordination need, the scaffold classification may be masking a piton (theatrical tradition-maintenance) rather than a genuine sunset-bound coordination patch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_versus_ritualized_tradition, empirical, 'Whether the scaffold remains genuinely transitional or has drifted toward ritualized inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__social_scaffold_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__social_scaffold_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__social_scaffold_reading, theater_ratio, 90, 0.21).
narrative_ontology:measurement(mono_tr_t120, monopoly_rulebook__social_scaffold_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement(mono_tr_t150, monopoly_rulebook__social_scaffold_reading, theater_ratio, 150, 0.26).
narrative_ontology:measurement(mono_tr_t180, monopoly_rulebook__social_scaffold_reading, theater_ratio, 180, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 90, 0.34).
narrative_ontology:measurement(mono_be_t120, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 120, 0.36).
narrative_ontology:measurement(mono_be_t150, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 150, 0.37).
narrative_ontology:measurement(mono_be_t180, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 180, 0.38).

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
% This story is one of three siblings sharing the monopoly_rulebook kernel. extraction_demo_reading reads the same printed text as vindicating inevitable wealth concentration with elimination as the correct outcome (low-to-moderate epsilon from that reading's own lights, different beneficiary set — the eventual monopolist). tournament_orthodoxy_reading reads the printed text as the immutable competitive standard against which any house-rule deviation is illegitimate (epsilon authored from a fidelity-to-text perspective, victims being the players whose strategic skill is diluted by informal amendment). This story (social_scaffold_reading) treats the house-rule layer itself as the operative constraint, with social group cohesion as beneficiary and rules-literalist players as payer. All three share the printed rulebook as kernel but are structurally distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
