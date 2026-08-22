% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: House-Rule Social Scaffold for Community Play (Social Scaffold Reading)
 *   domain: game theory/social coordination/institutional design
 *
 * SUMMARY:
 *   A recurring game group plays the classic property-trading board game
 *   under a standing body of house rules: cash awards on the free-parking
 *   space, doubled salaries for passing Go, suspension of forced property
 *   auctions, and a declared end time. Together these recycle liquidity back
 *   to the table, delay elimination, and steer the session toward a socially
 *   shared close rather than the text's mandated attritional endgame in which
 *   all but one player is bankrupted and benched. This story authors THAT
 *   arrangement — the standing house-rule regime — as the social-scaffold
 *   reading assesses it: a transitional support whose justification is
 *   carrying the group through the mid-game crunch to a natural close, not
 *   the steady state of play itself. Epsilon (0.44) is authored for this
 *   standing arrangement under this reading's lights: the redistribution
 *   taxes skilled players' positional gains and extends everyone's time
 *   commitment, while delivering a coordination benefit the reading holds
 *   genuine. The claimed type (scaffold) and the metrics are authored
 *   independently; the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - game_session_host: agenda-setter (moderate/mobile) — ratifies and administers the house-rule packet, banks the recycled funds
 *   - casual_game_night_players: primary beneficiary (organized/constrained) — collective assent sustains the regime; exit means leaving the evening
 *   - early_elimination_vulnerable_players: protected beneficiary (powerless/trapped) — kept solvent and seated by the liquidity machinery
 *   - competitive_purist_players: primary payer (moderate/constrained) — bears the taxation of skill and the dilution of victory
 *   - printed_rulebook: non-agent excluded party (institutional/trapped) — the canonical text whose endgame mandate sits suspended
 *   - play_pattern_analysts: analytical observer — sees the cross-group aggregate no seat perceives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.44).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.35).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "House-Rule Social Scaffold for Community Play (Social Scaffold Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game theory/social coordination/institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, 'f16c6647-1382-4cd6-9569-08043579f1aa').
narrative_ontology:cs_kernel_codification('f16c6647-1382-4cd6-9569-08043579f1aa', fixed_text).
narrative_ontology:cs_authority_grounding('f16c6647-1382-4cd6-9569-08043579f1aa', practice).
narrative_ontology:cs_interpretation_layer_present('f16c6647-1382-4cd6-9569-08043579f1aa').
narrative_ontology:cs_reading_relation('f16c6647-1382-4cd6-9569-08043579f1aa', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('f16c6647-1382-4cd6-9569-08043579f1aa', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('f16c6647-1382-4cd6-9569-08043579f1aa', foundational, communal_correction_authority).
narrative_ontology:cs_axiom_status(communal_correction_authority, holdable).
narrative_ontology:cs_axiom_grounding('f16c6647-1382-4cd6-9569-08043579f1aa', communal_correction_authority, conventional).
narrative_ontology:cs_axiom('f16c6647-1382-4cd6-9569-08043579f1aa', foundational, participation_continuity_over_fidelity).
narrative_ontology:cs_axiom_status(participation_continuity_over_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('f16c6647-1382-4cd6-9569-08043579f1aa', participation_continuity_over_fidelity, deontological).
narrative_ontology:cs_reference_frame('f16c6647-1382-4cd6-9569-08043579f1aa', rulebook_as_correctable_base).
narrative_ontology:cs_drift_state('f16c6647-1382-4cd6-9569-08043579f1aa', contemporary_household_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f16c6647-1382-4cd6-9569-08043579f1aa', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_game_night_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, early_elimination_vulnerable_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, game_session_host).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, competitive_purist_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the board, schedules the evenings, proposes and re-ratifies the house-rule packet each season, banks the recycled money, and adjudicates table disputes. The evening happens at their table; they can decline to host or swap games, and everyone knows it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_session_host, agenda_setter,
    moderate, biographical, mobile, local).

% Attend for the company more than the competition. Their collective assent is what sustains the house rules season over season. Opting out of the modified rules effectively means opting out of the table and the evening's social fabric, which is the thing they came for.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_game_night_players, beneficiary,
    organized, biographical, constrained, local).

% Novices and unlucky rollers who would be bankrupt within the first hour under the printed procedures. Windfall payouts, doubled salaries, and delayed property development keep them seated and solvent. Without the cushion they would spend the evening watching from the couch, which is the outcome the whole packet exists to prevent.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, early_elimination_vulnerable_players, beneficiary,
    powerless, immediate, trapped, local).

% Play to win and treat the printed procedures as the game itself. The house-rule packet taxes their positional advantages: windfalls flow to opponents at random, forced auctions are skipped, and victories are diluted by declared end times. Objecting carries visible social cost at the table, and declining attendance costs them the group itself, so their leverage is mostly grumbling and occasional counter-proposals.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_purist_players, payer,
    moderate, biographical, constrained, regional).

% The canonical text whose endgame mandates — forced auctions of unpurchased properties, attritional elimination until one holder remains — sit suspended at this table. It is present on the shelf, consulted when disputes arise, and overridden whenever its procedures threaten the evening. It cannot leave, and it cannot enforce itself.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, printed_rulebook, excluded,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, printed_rulebook).

% Study why households overwhelmingly modify the game: simulating rule variants, documenting how liquidity recycling changes session length and dropout rates, and publishing findings that no kitchen table is obliged to read. They see the aggregate pattern no single seat perceives.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, play_pattern_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps every participant engaged in one shared activity for the full session: liquidity recycling prevents early bankruptcy knockouts, slowed elimination keeps every seat occupied, and the group stays coordinated around a single continuing game instead of splintering into active players and eliminated spectators.
% TRANSFER_FUNCTION: Moves expected winnings and game-time advantage away from the strongest-positioned players toward the table as a whole (windfall jackpots, doubled salaries, skipped auctions, declared end times), and moves decision authority over game structure from the printed text to the assembled group.
% ABSENT_VOICES: Players who would prefer strict competitive play are often outvoted or socially eased into assent rather than genuinely absent; the deeper absence is procedural — house rules are typically adopted by whoever brings the box, before anyone who might object has a seat in the conversation. Future sessions' players are also bound by tonight's precedent without having been present.
% DISAPPEARANCE_RATIONALE: If the house-rule packet vanished overnight, sessions would revert to the printed procedures: rapid wealth concentration, first-hour eliminations, attritional multi-hour endgames, and a measurable share of the table dropping out of game night entirely. The social calendar built around the game — who attends, how long they stay, whether the evening ends in shared laughter or one winner and three casualties — rearranges immediately.
% FOUNDING_PROBLEM: The printed game produces a long attritional endgame in which most players are eliminated early and then idle while one player grinds out victory. The house-rule packet was built to solve the problem that the game stops being socially playable for most of the table long before it ends.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any single benefiting table: published play-pattern analyses and simulations documenting early elimination and session abandonment under strict procedures; the convergent, independent reinvention of the same corrections (free-parking jackpots, doubled salaries, end-time declarations) across unrelated households and cultures, which is adoption evidence no benefiting group coordinated; and publisher surveys acknowledging that most households play with house rules.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.44 at interval end) because the packet measurably transfers win-probability and time from the strongest seat to the table, yet the transferred amounts are small-stakes and the paying seat retains real recourse (counter-proposals, hosting its own text-faithful sessions). Suppression (0.35) is real but soft: the enforcement mechanism is majority assent plus social expectation, not sanction — declining the packet means declining the evening. Theater ratio (0.20) is low and rising slowly: the liquidity machinery genuinely functions, but a growing fraction of the packet persists by ritual (rules kept after anyone remembers the reason), which is the early signature of ossification. Accessibility collapse (0.30) is low — the strict-text alternative, other games, and other groups all remain reachable; the convention closes few doors. Resistance (0.40) reflects recurring purist grumbling and occasional defection. The measurement series run on one shared grid (t=0..12, years of a group's tradition): base extractiveness climbs as deviations from the text accumulate and entrench, and theater ratio creeps up as justification memory fades. Suppression_requirement series are deliberately NOT authored: enforcement capacity is static across the interval (the same soft social mechanism throughout), so the scalar captures it and the temporal rule against gratuitous suppression series applies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary/agenda-setter seats should compute differently. From the host's and casual players' positions the packet is the thing that makes the evening work — coordination they built and renew each season. From the purist's position the same packet operates as a standing tax on competence enforced by popularity. The vulnerable-player seat experiences neither: it experiences rescue. The engine computes this three-way divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The host sits near the beneficiary end (sets and banks the arrangement, mobile exit) but collects no systematic cut of the transfers — the banking is custodial. Casual players derive low directionality (net subsidized, constrained exit deepens their stake). Vulnerable players sit nearest the full-beneficiary pole: the machinery exists for them, they pay nothing for it, and they cannot exit anyway. Purist players sit near the full-target pole: they fund the redistribution with precisely the asset (positional advantage) the constraint exists to blunt, and their exit is socially priced. Gain_flow is authored as 'diffuse' as an affirmative checked claim: windfalls land stochastically on whoever reaches the payout squares, the durable gain (an evening that works) accrues to no named seat, and re-reading every stakeholder situation confirms none systematically captures the extraction. Fixing_cost is 'cheap': the host can suspend the packet any session at zero structural cost — nothing locks it in — which is exactly the removable-in-one-motion profile a transitional support should have.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing prevents two opposite misreadings. Read as pure extraction, the packet looks like a majority taxing a skilled minority — but that ignores the transitional justification: the tax exists to bridge the session past the point where the text's own dynamics destroy the activity, and the packet dissolves with the session. Read as pure coordination, the packet looks costless — but the purist's burden is real, recurs every session, and grows as the tradition entrenches. The lifecycle risk this story tracks is mandatrophy-by-ritual: individual rules outliving their justifications (a jackpot maintained after the vulnerability it addressed changed), signaled by the slowly rising theater_ratio series. If the packet is never retired rule-by-rule — see the sunset_genuineness omega — the scaffold reading fails and the arrangement hardens into a standing regime whose justification is habit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the monopoly_rulebook kernel (social_scaffold_reading). Which structural element do the sibling readings contest, and what would adopting them change?',
    'Adopting extraction_demo_reading relocates the victim set to all non-winning players and drives epsilon toward the extraction ceiling; adopting tournament_orthodoxy_reading dissolves the house-rule arrangement entirely and restores the text''s endgame as the operative constraint. The disagreement is located in the status of the text''s endgame mandate: defect to be corrected, lesson to be demonstrated, or standard to be preserved.',
    'Classification flips across the family: the extraction-demo instantiation computes a heavily extractive profile with a universal victim set; the tournament-orthodoxy instantiation computes a text-bound competitive regime with minimal table-level extraction but high suppression of variant play. This file''s verdict holds only for the scaffold reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: which kernel element the sibling readings contest and how classification moves if they are adopted.').

omega_variable(
    sunset_genuineness,
    'Is the house-rule packet genuinely transitional — individual rules retired when their justification lapses — or has it hardened into a permanent parallel constitution renewed by habit?',
    'Longitudinal observation of whether groups ever sunset specific rules (dropping the free-parking jackpot when someone finally runs the odds aloud, restoring auctions once the table learns them) or ratify the identical packet unchanged year over year.',
    'If rules are never retired, the transitional justification fails and the arrangement reclassifies toward a standing regime bearing the text-fidelity cost indefinitely — the scaffold claim collapses and the rising theater_ratio becomes the dominant signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_genuineness, empirical, 'Whether the packet''s sunset clause is operative or vestigial.').

omega_variable(
    redistribution_efficacy,
    'Does the liquidity machinery actually protect the elimination-vulnerable, or does stochastic payout distribute windfalls irrespective of need?',
    'Session-level records correlating windfall receipts with pre-windfall net worth across many groups; simulation of payout targeting versus uniform-random payout against elimination timing.',
    'If payouts are need-blind noise, part of the coordination claim is cover: effective extraction from skilled players rises while the protective benefit the reading cites shrinks, shifting the balance from scaffold toward unjustified transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_efficacy, empirical, 'Whether the redistribution reaches the players it claims to protect.').

omega_variable(
    consent_vs_social_pressure,
    'Is purist assent to the house-rule packet voluntary acceptance of a group norm, or acquiescence under social pressure that would not survive a private ballot?',
    'Compare stated rule preferences elicited privately versus at-table ratification; observe whether purists run text-faithful sessions when they host elsewhere.',
    'If assent is pressured, the measured suppression understates the constraint''s hold on the minority seat and the arrangement leans toward enforced transfer from a dissenting payer rather than negotiated convention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_social_pressure, empirical, 'Structural versus social-pressure component of minority-seat compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t2, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2, 0.1).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(mono_tr_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__social_scaffold_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__social_scaffold_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.2).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mono_be_t2, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2, 0.34).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(mono_be_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.44).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__social_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the rules of Monopoly' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the extraction-demo reading (the text as pedagogy of inevitable concentration), the tournament-orthodoxy reading (the text as immutable competitive standard), and this social-scaffold reading (the text as a base requiring communal correction to stay socially playable). Each has its own epsilon, beneficiary/victim structure, and classification; they are linked here as one constraint family because the upstream text-authority claim is cited as evidence within the sibling readings. This file authors only the social-scaffold reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
