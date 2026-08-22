% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Monopoly Rulebook as Social Cohesion Scaffold (House-Rule-Corrected Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the social_scaffold_reading of the
 *   monopoly_rulebook kernel: the printed text mandates an
 *   elimination-and-concentration endgame that is structurally sound for a
 *   short adversarial contest but socially unplayable for a multi-hour casual
 *   gathering, so groups universally graft on house rules (free parking
 *   jackpots, bank loans, refusal of auctions) that inject liquidity and slow
 *   elimination. Under this reading the coordination function is real and
 *   dominant — keeping the social group intact and engaged for the session —
 *   and the extraction is moderate and largely confined to the residual harm
 *   borne by whoever is still eliminated despite the cushioning. This is
 *   explicitly one reading among three of the same rulebook-kernel: the
 *   extraction_demo_reading treats the identical text as vindicating
 *   inevitable rent concentration (near-zero house-rule legitimacy,
 *   elimination as the point), and the tournament_orthodoxy_reading treats
 *   house rules as illegitimate noise obscuring a fixed competitive
 *   framework. All three share the same printed text but author different ε,
 *   different beneficiaries, and different classifications — this file is the
 *   social_scaffold reading only; the siblings are separate constraint
 *   stories linked via network.affects_constraints.
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
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook as Social Cohesion Scaffold (House-Rule-Corrected Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '06024d70-0a8d-49e8-bca4-9816c4d48a92').
narrative_ontology:cs_kernel_codification('06024d70-0a8d-49e8-bca4-9816c4d48a92', fixed_text).
narrative_ontology:cs_authority_grounding('06024d70-0a8d-49e8-bca4-9816c4d48a92', practice).
narrative_ontology:cs_interpretation_layer_present('06024d70-0a8d-49e8-bca4-9816c4d48a92').
narrative_ontology:cs_reading_relation('06024d70-0a8d-49e8-bca4-9816c4d48a92', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('06024d70-0a8d-49e8-bca4-9816c4d48a92', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('06024d70-0a8d-49e8-bca4-9816c4d48a92', foundational, social_playability_supersedes_textual_fidelity).
narrative_ontology:cs_axiom_status(social_playability_supersedes_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('06024d70-0a8d-49e8-bca4-9816c4d48a92', social_playability_supersedes_textual_fidelity, instrumental).
narrative_ontology:cs_axiom('06024d70-0a8d-49e8-bca4-9816c4d48a92', secondary, house_rule_practice_is_legitimate_communal_amendment).
narrative_ontology:cs_axiom_status(house_rule_practice_is_legitimate_communal_amendment, holdable).
narrative_ontology:cs_axiom_grounding('06024d70-0a8d-49e8-bca4-9816c4d48a92', house_rule_practice_is_legitimate_communal_amendment, conventional).
narrative_ontology:cs_reference_frame('06024d70-0a8d-49e8-bca4-9816c4d48a92', casual_multiplayer_social_session_norm).
narrative_ontology:cs_drift_state('06024d70-0a8d-49e8-bca4-9816c4d48a92', contemporary_casual_play, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('06024d70-0a8d-49e8-bca4-9816c4d48a92', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, player_social_group).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, informal_host_rule_setter).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, early_eliminated_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, rules_as_living_practice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The friends or family playing together want an evening of shared activity, not a 90-minute elimination gauntlet ending in one winner and everyone else sidelined for hours. They benefit from house rules (free parking jackpot, no-auction-on-refusal, loans from the bank) that keep more players active longer and preserve the social occasion the game is actually being played for.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, player_social_group, beneficiary,
    organized, immediate, constrained, local).

% Usually the person who owns the board or has hosted before. Announces the house rules at the start ('we do free parking, we don't auction'), enforces them informally during play, and can add or drop a rule mid-game by consensus. Has no stake in strict textual fidelity — their goal is a playable, sociable evening that ends with people still talking to each other.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, informal_host_rule_setter, agenda_setter,
    moderate, immediate, mobile, local).

% Even with liquidity injections and slowed elimination, someone still goes bankrupt first under bad luck or poor early trades. House rules push their elimination later and often soften it (they get to keep watching, or the game ends before true endgame), but they still bear the cost of being knocked out of active participation while the group continues, and their in-game losses (property, cash) are not recoverable regardless of the social cushioning around the fact of losing.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, early_eliminated_players, payer,
    powerless, immediate, trapped, local).

% Wrote and licenses the printed rules, which specify no free parking bonus, no bank loans, and mandatory auctioning of declined properties. Has commercial interest in the rules being followed as printed (supports official tournament products, brand consistency) but has no presence at the actual kitchen table and no mechanism to prevent house-rule substitution once the box is sold.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_publisher, excluded,
    institutional, generational, analytical, global).

% Would prefer strict rules-as-written play because it rewards the trading and negotiation skill they value, and consider house rules a dilution that removes the game's actual strategic content and extends it pointlessly. At a casual social table they are usually outvoted or simply overruled by the host and go along with the group consensus rather than insist.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_purist_players, excluded,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, player_social_group).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a multi-hour group activity socially viable: without correction, one or two players are eliminated within the first hour and sit out for the remaining two-plus hours while others play on, which most casual groups experience as an unacceptable social cost of the base ruleset.
% TRANSFER_FUNCTION: House rules redistribute liquidity (free parking cash, bank loans, refused-property non-auction) from the game's designed scarcity mechanism back toward players who would otherwise be eliminated early, at the cost of prolonging the endgame and diluting the wealth-concentration dynamic the printed rules are built around.
% ABSENT_VOICES: The rulebook publisher and competitive purists would object that this is not 'really' Monopoly anymore, or that it defeats the game's designed teaching point about capital concentration — but neither has a vote at a casual social table where the informal host sets the terms by consensus or by simply announcing them.
% DISAPPEARANCE_RATIONALE: If house-rule correction vanished and strict text governed every casual game, early-eliminated players would sit out for the bulk of long sessions, group play would fracture (people leaving the table, phones out, disengagement), and many casual groups would simply stop playing the game rather than tolerate the harsh, prolonged endgame the unmodified rules produce.
% FOUNDING_PROBLEM: The printed ruleset was designed (originally as a pedagogical/anti-monopolist demonstration, later commercialized as a competitive game) to produce decisive wealth concentration and elimination — a structurally sound design goal for a short, adversarial contest but a poor fit for a multi-hour social gathering where continued group participation is the actual desired outcome.
% FOUNDING_PROBLEM_CORROBORATION: Board game historians and design critics outside the player groups themselves have documented the game's origin as a deliberate anti-monopolist teaching tool (Lizzie Magie's Landlord's Game) whose mechanics were preserved through commercialization even as its social use shifted toward casual family play; this corroborates that the 'harsh endgame' is original design intent, not house-rule players' misreading, while the tournament-orthodoxy camp (via official rules and organized play communities) disputes that the founding problem is dead, insisting competitive fidelity is still the game's live purpose.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises modestly over the session (0.15 to 0.38) because early play under house rules resembles genuine mutual coordination (everyone still in, liquidity flowing) while later play still produces a residual elimination dynamic the house rules only delay, not eliminate — someone still loses their stake permanently. Suppression stays low throughout (peaking at 0.22) because house-rule adoption is consensual and reversible by the group at any point; no one is coerced into compliance beyond normal social pressure to go along with the table's agreed variant. Theater ratio rises moderately (0.10 to 0.28) as the game's later stages increasingly perform 'still playing' rather than deliver fresh coordination value — extended sessions post-elimination sometimes continue mostly for social-face reasons rather than active strategic content.
 *
 * DIRECTIONALITY LOGIC:
 *   The player social group and the informal host who sets house rules sit near the beneficiary end: the modification exists to serve their continued participation and enjoyment, and they can change or drop it at will (mobile/organized exit). Early-eliminated players sit nearer the target end despite the cushioning — the house rules delay and soften their elimination but do not prevent it, and once eliminated they are trapped outside active play for the remainder of the session with no recourse to recover in-game losses. Their d is elevated by trapped exit even though the overall constraint is comparatively low-extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification (with sunset clause = the game ending) prevents mislabeling this as pure extraction: the house-rule regime exists precisely to solve a real, recurring social-coordination failure (early elimination destroying group cohesion) and is understood by all parties as provisional to this session, not a permanent claim on anyone. It also prevents mislabeling it as pure coordination (rope) — some residual, unevenly distributed cost to early-eliminated players persists even after correction, which is why extractiveness is authored as moderate (0.31-0.55 band) rather than negligible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the socially-corrected house-rule practice the rulebook''s legitimate operating mode, or a deviation from a fixed competitive standard that happens to be socially popular?',
    'This is not resolvable by further empirical study of Monopoly play alone — it depends on which reading of the monopoly_rulebook kernel is adopted (social_scaffold_reading, extraction_demo_reading, or tournament_orthodoxy_reading), each a separate constraint story with its own ε and stakeholder structure.',
    'Under this reading (social_scaffold), house rules are the legitimate coordination mechanism and the text''s harsh endgame is the deviation needing correction. Under tournament_orthodoxy_reading, the relationship inverts entirely. Under extraction_demo_reading, the house rules are themselves the extraction-obscuring move. The three readings are not competing measurements of one constraint — they are three different constraints sharing a text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Committer-level ambiguity: which reading of the monopoly_rulebook kernel governs this table''s practice.').

omega_variable(
    house_rule_content_variance,
    'Different house-rule bundles (free parking only, vs. free parking plus bank loans plus no-auction) inject very different amounts of liquidity and produce very different elimination timelines — does ''the'' social_scaffold_reading actually name one constraint or a family of house-rule variants with meaningfully different epsilon?',
    'Survey actual house-rule combinations in casual play (BoardGameGeek forum data, informal ethnography) and check whether extractiveness clusters tightly around the 0.31-0.55 band across common variants or splits into distinguishable sub-bands.',
    'If variance is wide, this story should itself decompose further by house-rule bundle severity; as authored it treats the moderate, common bundle (free parking + loans + non-mandatory auction) as representative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rule_content_variance, empirical, 'Whether house-rule bundle variance requires further decomposition within this reading.').


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
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__social_scaffold_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__social_scaffold_reading, theater_ratio, 90, 0.23).
narrative_ontology:measurement(mono_tr_t120, monopoly_rulebook__social_scaffold_reading, theater_ratio, 120, 0.26).
narrative_ontology:measurement(mono_tr_t150, monopoly_rulebook__social_scaffold_reading, theater_ratio, 150, 0.27).
narrative_ontology:measurement(mono_tr_t180, monopoly_rulebook__social_scaffold_reading, theater_ratio, 180, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 90, 0.33).
narrative_ontology:measurement(mono_be_t120, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 120, 0.36).
narrative_ontology:measurement(mono_be_t150, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 150, 0.37).
narrative_ontology:measurement(mono_be_t180, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 180, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 60, 0.17).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 90, 0.19).
narrative_ontology:measurement(mono_su_t120, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 120, 0.2).
narrative_ontology:measurement(mono_su_t150, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 150, 0.21).
narrative_ontology:measurement(mono_su_t180, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 180, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monopoly_rulebook kernel. social_scaffold_reading (this file, scaffold, moderate epsilon ~0.38) treats house-rule correction as legitimate coordination preserving group play. extraction_demo_reading treats the same text as vindicating inevitable elimination/concentration, with house rules read as illegitimate dilution. tournament_orthodoxy_reading treats the printed text as an immutable competitive standard with house rules as noise. All three share the identical printed rulebook but author different beneficiaries, different epsilon, and different classifications — they are not the same constraint measured three ways; per the epsilon-invariance principle they are three distinct constraints linked here rather than one story with a reading parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
