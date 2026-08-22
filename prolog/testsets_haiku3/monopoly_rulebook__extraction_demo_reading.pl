% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook: Extraction Demo Reading
 *   domain: social/economic/institutional design
 *
 * SUMMARY:
 *   This constraint story instantiates the extraction_demo_reading of the
 *   Monopoly rulebook kernel. The reading asserts that the rulebook's fixed
 *   structure — no redistribution, winner-takes-all, irreversible elimination
 *   — IS DESIGNED to demonstrate how capitalism's rules necessarily
 *   concentrate wealth. The game itself is the extraction mechanism: players
 *   move through forced wealth transfer until all but one are eliminated.
 *   This reading treats the rulebook as a pedagogical text whose truth-claim
 *   is about economic inevitability. The reading competes with the
 *   social_scaffold_reading (which interprets the rulebook as needing
 *   community correction to be socially playable) and the
 *   tournament_orthodoxy_reading (which treats it as a skill-testing
 *   competitive framework where the text authority is immutable for ranking).
 *   This story models the extraction_demo_reading as a tangled_rope: it
 *   coordinates players around shared rules (coordination function) while
 *   simultaneously extracting from eliminated players through those same
 *   rules (asymmetric harm). The beneficiary is the winning player; the
 *   victims are the eliminated. The structural enforcement is the rulebook's
 *   irreversibility and the absence of alternative legitimate play frames
 *   within this reading.
 *
 * KEY AGENTS:
 *   - rulebook_author_designer: Establishes the rule authority; from this reading's view, intends the structure to teach wealth concentration mechanics (analytical power, generational time horizon)
 *   - winning_player: Accumulates all capital; benefits from elimination of competitors (powerful, immediate horizon, mobile exit)
 *   - eliminated_players: Forced out of play with zero remaining capital; bear the full extraction cost (powerless, immediate, trapped)
 *   - active_players_mid_game: Engaged in rent collection and accumulation; exposed to elimination risk; simultaneously extract from and are extracted from (moderate power, immediate, constrained)
 *   - alternative_ruleset_proponents: Advocate house rules but are excluded from authority to reframe the kernel's meaning in this reading (moderate, biographical, constrained)
 *   - pedagogical_observer: The analytical seat that reads the rulebook as demonstrating capitalist mechanics (analytical power, generational, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.72).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, tangled_rope).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Extraction Demo Reading").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "social/economic/institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '79a9517d-191d-438f-b3c3-3638646b2dc8').
narrative_ontology:cs_kernel_codification('79a9517d-191d-438f-b3c3-3638646b2dc8', fixed_text).
narrative_ontology:cs_authority_grounding('79a9517d-191d-438f-b3c3-3638646b2dc8', lineage).
narrative_ontology:cs_interpretation_layer_present('79a9517d-191d-438f-b3c3-3638646b2dc8').
narrative_ontology:cs_reading_relation('79a9517d-191d-438f-b3c3-3638646b2dc8', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('79a9517d-191d-438f-b3c3-3638646b2dc8', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('79a9517d-191d-438f-b3c3-3638646b2dc8', foundational, elimination_structurally_necessary).
narrative_ontology:cs_axiom_status(elimination_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('79a9517d-191d-438f-b3c3-3638646b2dc8', elimination_structurally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('79a9517d-191d-438f-b3c3-3638646b2dc8', foundational, rulebook_text_authority_immutable).
narrative_ontology:cs_axiom_status(rulebook_text_authority_immutable, holdable).
narrative_ontology:cs_axiom_grounding('79a9517d-191d-438f-b3c3-3638646b2dc8', rulebook_text_authority_immutable, conventional).
narrative_ontology:cs_reference_frame('79a9517d-191d-438f-b3c3-3638646b2dc8', rulebook_as_pedagogical_demonstration).
narrative_ontology:cs_drift_state('79a9517d-191d-438f-b3c3-3638646b2dc8', contemporary_house_rules_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('79a9517d-191d-438f-b3c3-3638646b2dc8', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, active_players_mid_game).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, active_players_mid_game).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rulebook's fixed text — no redistribution mechanisms, winner-takes-all, elimination endgame — is the enforced constraint. The designer's intent to instantiate wealth concentration as a pedagogical demonstration is not a role in the game itself but grounds the reading's interpretation of what the rules ARE ABOUT. From this reading's perspective, the designer creates a structure that MUST concentrate wealth and eliminate players as a condition of demonstrating capitalism's mechanics.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rulebook_author_designer, agenda_setter,
    analytical, generational, analytical, global).

% Accumulates all remaining capital and property through the elimination of other players. The rulebook's winner-takes-all structure and absence of redistribution mechanisms ensure the winning seat collects the full extraction the constraint generates. Victory is the constraint's natural endpoint.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, winning_player, beneficiary,
    powerful, immediate, mobile, local).

% Lose their remaining capital and are forced out of play. They have paid the full cost the rulebook extracts — their assets transferred to other players, their agency removed. Elimination is irreversible within a single game session. From this reading, their elimination is the POINT: it demonstrates that the rulebook structure necessarily concentrates wealth by liquidating the losers.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Are engaged in the extraction process themselves — collecting rents, trading, accumulating capital — while remaining at risk of elimination. They bear the suppressive force of the rulebook (the threat of elimination) and participate in its extraction mechanism simultaneously. Their position is unstable: they are payers today and may be beneficiaries of elimination of others or payers-to-elimination themselves tomorrow.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, active_players_mid_game, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, active_players_mid_game, beneficiary).

% Advocate for house rules (liquidity injection, slowed elimination, redistribution) to make the game socially playable. They are excluded from the authority to declare the rulebook's meaning — the extraction_demo_reading asserts the official text's authority and the necessity of its wealth concentration. House rules are framed as noise or as deviation from the constraint's true structure. Their objection goes unheard in this reading's framework.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, alternative_ruleset_proponents, excluded,
    moderate, biographical, constrained, local).

% Treats the game as a living demonstration of monopoly capitalism's mechanics: how rules instantiate inevitable concentration, how elimination is the natural outcome, how the structure's operation teaches structural economic truth. This reading's epistemic seat — the one that reads the rulebook as didactic rather than recreational.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, pedagogical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates players around a shared set of rules for capital accumulation, property ownership, rent collection, and elimination. It solves the problem of orderly competitive accumulation: without the rules, capital disputes would be chaotic; with them, players have a transparent, repeated, learnable system for wealth transfer.
% TRANSFER_FUNCTION: Moves capital, property, and cash flow from losers to winners through rent collection, forced sales, mortgages, and elimination. The constraint itself IS the transfer mechanism — the rulebook is the path through which wealth concentrates. As players are eliminated, their remaining assets are liquidated and transferred to the surviving player pool.
% ABSENT_VOICES: Players whose house rules are suppressed or rejected by this reading — advocates for redistribution, liquidity injection, slowed elimination — are structurally excluded from authority over what the constraint means. The tournament_orthodoxy_reading, which prioritizes competitive purity and text immutability, also has no voice here; its claim that the rulebook is about skill testing rather than about demonstrating capitalism's mechanics is treated as a competing interpretation that has lost epistemic authority in this reading's framework.
% DISAPPEARANCE_RATIONALE: If the rulebook and its enforcement (no redistribution, winner-takes-all, irreversible elimination) disappeared and were replaced with alternative rules (liquidity mechanisms, slower elimination, property redistribution), the game would no longer demonstrate inevitable wealth concentration. The pedagogical point — that capitalism's rulebook NECESSARILY eliminates players and concentrates wealth — would collapse. Players would remain in play longer, property would be more evenly distributed, and the demonstration of concentration would fail. The world of the game (and the lessons it teaches) rearranges entirely.
% FOUNDING_PROBLEM: The game was designed to teach players about real-estate capitalism: how property ownership becomes monopolistic, how players without property become powerless, how the rules of capital accumulation lead to eventual elimination of all but the wealthiest. The rulebook instantiates these mechanics to make the lesson experiential and memorable.
% FOUNDING_PROBLEM_CORROBORATION: The game's designer (via historical accounts and documented interviews) has stated the game was created to critique monopoly capitalism and teach its mechanics. Players who adopt the extraction_demo_reading attest the founding problem — demonstrating inevitable concentration — remains the game's true function. House-rules advocates and tournament players contest this reading: they claim the founding problem is to create an engaging game experience (social_scaffold_reading) or to provide a fair competitive test (tournament_orthodoxy_reading). No neutral third party outside the game community has definitive corroboration, but the designer's documented intent supports the extraction_demo_reading's founding problem statement.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at game end) and rises monotonically over the 90-minute interval because the rulebook's structure ensures capital flows from losers to winners with no redistribution mechanism. Early in the game, extractiveness is moderate (0.35) because many players are still in play and accumulation is distributed; as eliminations accelerate (t=45 onward), the concentration effect intensifies and extractiveness rises sharply (0.66–0.72). Suppression is also high and rising: eliminated players have zero agency; remaining players face the constant threat of elimination; the rulebook enforces this through irreversibility (you cannot re-enter once eliminated, your assets cannot be recovered). Theater_ratio is low (0.22 at end) because the rulebook's operation is largely functional — the rules deliver the extraction they promise; there is little pretense that elimination serves a pedagogical purpose beyond the demonstration itself. Accessibility_collapse is high (0.81): once players understand the rulebook's structure, alternatives effectively vanish — this IS how Monopoly is played under this reading; no alternative legitimate frame is available within the extraction_demo frame. Resistance is moderate (0.54) because players know they are at risk of elimination but continue to play, accepting the risk as part of the game's structure — resistance is present (players might choose not to play, or adopt house rules), but within the frame of the extraction_demo_reading, such resistance is treated as external to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (eliminated_players, active_players) and the beneficiary seat (winning_player) should compute different types from the same rulebook. The beneficiary perceives a coordinate game they won; the payers perceive an extractive structure that eliminated them. The engine computes this divergence from the power/exit/beneficiary atoms: eliminated players (powerless, trapped exit) in a constraint that benefits others systematically extracts from them. The claim (tangled_rope) reflects the reading's own frame: the rulebook DOES coordinate all players around shared rules (the rope function), AND it asymmetrically extracts through those rules (the extraction function). The metrics (high extractiveness, high suppression, low theater) describe how the structure operates, independent of whether the designer intended it as a pedagogical critique or a competitive sport.
 *
 * DIRECTIONALITY LOGIC:
 *   The winning_player is the structural beneficiary (d ≈ 0.1–0.2: they collect the constraint's gains with minimal cost). The eliminated_players are the structural targets (d ≈ 0.95–1.0: they bear the full cost of elimination with zero benefit). Active_players_mid_game are asymmetrically positioned (d ≈ 0.6–0.7): they participate in extraction of other players (moderate beneficiary position) while remaining exposed to extraction themselves (moderate target position). The rulebook_author_designer and pedagogical_observer are analytical seats outside the game's outcome (d = analytical frame). The alternative_ruleset_proponents are excluded from authority in this reading (their d would be recomputed in a different constraint story, the social_scaffold_reading, where house rules reframe the structure entirely).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (demonstrating inevitable wealth concentration through elimination) remains CONTESTED. The extraction_demo_reading asserts it is LIVE (the rulebook is still being used to teach this lesson), but the social_scaffold_reading asserts it is DEAD (the ruling function has been superseded by house rules as the dominant play mode in most communities). The tournament_orthodoxy_reading also asserts the founding_problem is mischaracterized (the problem is not about teaching capitalism but about testing competitive skill). The mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) is the signal: if the rulebook's actual founding problem IS to demonstrate wealth concentration, then its disappearance or replacement would rearrange the world (the pedagogical lesson would vanish). If the ruling problem is DEAD (the lesson is no longer operative or is widely rejected), then the rulebook persists as a zombie constraint sustained by textual authority rather than functional need — a piton candidate. This reading stops short of declaring mandatrophy (it asserts the founding problem is contested, not dead), so no mandatrophy_resolved flag is set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    designer_intent_vs_reading_claim,
    'Does the game''s designer intend the rulebook to demonstrate capitalism''s mechanics (extraction_demo_reading), or to provide a social game experience (social_scaffold_reading), or to test competitive skill (tournament_orthodoxy_reading)?',
    'Historical documentation: designer interviews, letters, early rulebook editions, designer''s own published commentary on the game''s purpose. Genealogical evidence from the game''s evolution and how it was marketed/taught.',
    'If the designer''s documented intent aligns with the extraction_demo_reading, the reading''s founding_problem claim is corroborated from outside the benefiting parties. If intent aligns with another reading, the extraction_demo_reading becomes a reinterpretation imposed on a text with different original purposes. If intent is ambiguous or evolves over time, the readings remain genuinely underdetermined by the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designer_intent_vs_reading_claim, empirical, 'The designer''s actual intent relative to the three competing readings').

omega_variable(
    elimination_necessity_vs_contingency,
    'Is elimination a structurally necessary feature of the rulebook''s wealth-concentration mechanism, or is it contingent — could wealth concentrate under the rules even if eliminated players were allowed to re-enter or recover assets?',
    'Variant-game analysis: play the game with modified rules (eliminated players re-enter with small capital, or their assets are partially recovered, or redistribution mechanisms are introduced mid-game) and measure whether wealth concentration still occurs monotonically or whether the distribution patterns change.',
    'If concentration is necessary, elimination is the engine''s irreplaceable component and the extraction_demo_reading is structurally sound. If concentration persists without elimination, then elimination is a feature that ACCELERATES extraction but is not strictly necessary — the reading''s claim about necessity would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elimination_necessity_vs_contingency, empirical, 'Whether elimination is structurally required for the rulebook to demonstrate wealth concentration').

omega_variable(
    house_rules_as_subversion_vs_repair,
    'Are house rules (liquidity injection, slowed elimination, redistribution) a subversion of the rulebook''s TRUE function (pedagogical demonstration), or a repair of a game that the rulebook left broken for social play?',
    'Empirical record: how do different player communities adopt or reject house rules? Are house rules presented as ''ways to fix a broken game'' or ''ways to subvert the demonstrative point''? Historical adoption rates and rhetoric around house rules in published game guides and community forums.',
    'If house rules are framed and adopted as repair, this suggests the extraction_demo_reading''s founding problem is DEAD in actual practice (players don''t want the harsh demonstration, they want a playable game). If house rules are explicitly framed as subversion or as optional alternatives to the ''real game,'' the reading''s claim to authority is sustained. The (status=contested, verdict=world_rearranges) mismatch flags this as a key uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rules_as_subversion_vs_repair, empirical, 'Whether house rules are understood as repair or as subversion, and what this implies for the founding problem''s status').

omega_variable(
    kernel_codification_ambiguity,
    'Is the rulebook a fixed_text kernel that cannot be legitimately modified, or a distributed kernel where community interpretation (house rules) is part of the legitimate play?',
    'Authority structure analysis: does the game''s publisher/designer claim exclusive authority to define legitimate play, or do they acknowledge house rules as legitimate variants? Is ''official rules'' a enforced category or a suggested starting point?',
    'If fixed_text authority is claimed, the extraction_demo_reading''s insistence on textual fidelity is structurally sound. If distributed authority is acknowledged, house rules are not subversion but legitimate alternative readings of the kernel — which would support the social_scaffold_reading''s frame. This affects whether alternative readings are foreclosed or coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, empirical, 'The authority structure around the rulebook: is it fixed or distributed?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(mono_tr_t0, observed).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(mono_tr_t15, observed).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(mono_tr_t30, observed).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.19).
narrative_ontology:measurement_basis(mono_tr_t45, observed).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement_basis(mono_tr_t60, observed).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement_basis(mono_tr_t75, observed).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement_basis(mono_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(mono_be_t0, observed).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(mono_be_t15, observed).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(mono_be_t30, observed).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement_basis(mono_be_t45, observed).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(mono_be_t60, observed).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.71).
narrative_ontology:measurement_basis(mono_be_t75, observed).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.72).
narrative_ontology:measurement_basis(mono_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(mono_su_t0, observed).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(mono_su_t15, observed).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(mono_su_t30, observed).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement_basis(mono_su_t45, observed).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(mono_su_t60, observed).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.67).
narrative_ontology:measurement_basis(mono_su_t75, observed).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.68).
narrative_ontology:measurement_basis(mono_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.18).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel instantiates three structurally distinct constraints depending on which reading frames the rulebook's function. extraction_demo_reading: the rulebook NECESSARILY demonstrates wealth concentration and elimination (tangled_rope, high extraction). social_scaffold_reading: the rulebook is a foundation that REQUIRES community house rules to be socially playable (scaffold, moderate extraction until repair rules are applied). tournament_orthodoxy_reading: the rulebook defines a competitive skill-test framework (rope, low extraction, emphasizing coordination around shared competitive rules). Each reading assigns different epsilon values, different victim/beneficiary structures, and different foundational purposes. They share the same kernel (the rulebook text) but diverge on what the text is structurally about. The three stories form a constraint family where each reading's authority claim attempts to foreclose or marginalize the others' interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
