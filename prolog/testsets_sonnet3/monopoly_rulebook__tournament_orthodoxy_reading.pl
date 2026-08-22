% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__tournament_orthodoxy_reading, []).

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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook as Tournament Orthodoxy: Fixed-Text Competitive Standard
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the tournament-orthodoxy reading of the Monopoly
 *   rulebook kernel: the published rules constitute the legitimate, immutable
 *   referent for competitive ranking, and house-rule variation is treated not
 *   as a defect requiring correction but as noise that breaks comparability.
 *   This is one of three readings of the same kernel text. The
 *   extraction-demonstration reading treats the identical text as an
 *   inevitable-monopoly pedagogy device culminating in elimination; the
 *   social-scaffold reading treats the identical text as unplayable without
 *   community-injected house rules. Those are separate constraint files, not
 *   alternative measurements of this one — per the ε-invariance principle,
 *   this story's ε (very low, ~0.06) is stable and does not average against
 *   the siblings' very different values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.06).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.15).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament Orthodoxy: Fixed-Text Competitive Standard").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6').
narrative_ontology:cs_kernel_codification('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', fixed_text).
narrative_ontology:cs_authority_grounding('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', practice).
narrative_ontology:cs_interpretation_layer_present('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6').
narrative_ontology:cs_reading_relation('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', foundational, fixed_text_is_sole_legitimate_ranking_referent).
narrative_ontology:cs_axiom_status(fixed_text_is_sole_legitimate_ranking_referent, holdable).
narrative_ontology:cs_axiom_grounding('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', fixed_text_is_sole_legitimate_ranking_referent, conventional).
narrative_ontology:cs_axiom('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', foundational, house_rules_are_noise_not_correction).
narrative_ontology:cs_axiom_status(house_rules_are_noise_not_correction, holdable).
narrative_ontology:cs_axiom_grounding('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', house_rules_are_noise_not_correction, conventional).
narrative_ontology:cs_reference_frame('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', sanctioned_tournament_practice_standard).
narrative_ontology:cs_drift_state('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', contemporary_competitive_circuit, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d3a86aa-5f48-4ef6-a5f1-8cb2678b74a6', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_community).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, ranked_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer sanctioned play by adopting the published rulebook verbatim as the ranking standard, rejecting house-rule variants (free parking jackpots, no-auction skipping, etc.) as noise that would make cross-event results incomparable. They gain nothing personally beyond a functioning competitive circuit; they could adopt house rules but choose the fixed text because it is the only basis for ranking players against each other.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    organized, generational, mobile, national).

% Train and compete under the official rules because doing so lets their results be compared across tournaments, seasons, and opponents. They voluntarily opt in; anyone preferring house-rule casual play simply plays casually instead and forfeits nothing they were entitled to. Their skill investment (auction strategy, trading, property-set timing) only cashes out as comparative standing if the ruleset is held fixed.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, ranked_players, beneficiary,
    organized, biographical, mobile, national).

% Benefits as a whole from having one stable, text-anchored standard: without it there is no shared basis for declaring a world champion or comparing regional results. The community's collective good (a legible competitive record) depends on nobody being able to unilaterally alter the standard for a given event.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_community, beneficiary,
    organized, generational, mobile, global).

% Play with free parking jackpots, taxes, or other informal variants at home; they have no stake in tournament ranking and are not addressed by, bound by, or excluded from anything by the orthodoxy reading — they simply play a different game by choice. Their variant play does not compete for the same legitimacy claim this reading makes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, immediate, arbitrage, local).

% Study the rulebook's textual history, including its contested origin (Lizzie Magie's The Landlord's Game) and note that the tournament-orthodoxy reading treats the codified 1935 rules as the sole legitimate referent for competitive purposes, independent of that origin story or of any pedagogical or social-correction reading of the same text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rules_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one textual standard so that strategic outcomes across independent tournaments, regions, and years are comparable — solving the genuine problem that without a common rule-text, 'winning' has no stable meaning across events.
% TRANSFER_FUNCTION: Moves very little: participation is voluntary, entry is symmetric, and the standard does not transfer resources from a payer class to a beneficiary class — it allocates comparative standing among players who all play under the same text.
% ABSENT_VOICES: Casual house-rule players are not in the tournament room and would find the fixed text needlessly harsh for a living-room game, but they are not addressed by this reading — the orthodoxy claim is scoped to competitive ranking, not to how anyone plays at home. Pedagogical and social-correction readers of the same rulebook would object that the text produces predictable elimination dynamics the orthodoxy reading treats as a feature (skill expression) rather than a defect (need for correction); those readers are the sibling constraints, not silenced parties within this one.
% DISAPPEARANCE_RATIONALE: If the fixed-text standard vanished, tournament results would become incomparable across events overnight — there would be no way to say a regional champion's result meant the same thing as a national champion's, and the competitive circuit (rankings, seeding, historical records) would have to reconstruct some other common referent from scratch or dissolve into disconnected local metas.
% FOUNDING_PROBLEM: Competitive play needs a stable, shared rule-text so that strategic skill (not rule-variant luck) determines who wins, and so results can be ranked and compared across independent events.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizing bodies attest the problem is live (they cite the fixed text explicitly when rejecting proposed house-rule amendments at sanctioned events). Independent competitive-gaming researchers studying esports and board-game tournament structures corroborate from outside the beneficiary set: cross-event comparability in any competitive format requires a fixed, shared rule referent, a finding not specific to Monopoly.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).
:- end_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.06) because entry is voluntary, no payer class is coerced into the tournament format, and the standard's function — enabling comparability — accrues to the whole participating community rather than concentrating rents on any seat. Suppression is low-moderate (0.15): the only 'suppressive' element is that sanctioned events reject house-rule variants for ranking purposes, which forecloses one narrow claim (that a house-ruled result should count as a ranked result) without foreclosing house-rule play itself, which remains fully available outside the tournament context. Theater ratio is near-zero because rule enforcement at sanctioned events is functional (arbitrating disputes under the fixed text), not performative.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (organizers) and the beneficiary seats (players, community) should compute similarly under this reading precisely because the coordination function is genuine and symmetric — this is the seat-convergence signature of a real rope, in contrast to the seat-divergence expected under the sibling readings where either an eliminated-player seat (extraction_demo) or a house-rule-suppressed seat (social_scaffold) would compute very differently from the agenda-setter seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Tournament organizers and ranked players are both beneficiaries: the fixed text is what makes their competitive investment (skill, strategy, historical ranking) meaningful across events, and no party extracts rents from another party's participation. There is no victim set in this reading because participation is voluntary and non-participants (casual house-rule players) are simply outside the scope of the claim rather than harmed by it — this is the structural reason the schema's tangled_rope/snare victim requirements do not apply and the story is authored as a clean rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a stable comparability referent) remains live and is corroborated by independent competitive-format research, not just by organizers — this blocks a mandatrophy read where the standard persists only through inertia after its function died. Because the standard's cost is cheap to change (organizers could adopt a different fixed text, or dissolve sanctioned play, at negligible cost) and gains are diffuse across the whole community rather than captured by any seat, this reading is structurally distant from a piton or snare despite being a fixed, non-negotiable text — fixedness alone does not imply extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthodoxy_reading_selection_ambiguity,
    'Is the tournament-orthodoxy reading the historically dominant reading of the Monopoly rulebook, or is it a minority reading confined to sanctioned competitive play while the social-scaffold reading dominates informal household play, which is the vastly more common context of actual use?',
    'Survey data on household vs. sanctioned-tournament play prevalence; historical record of when official tournament sanctioning bodies formed relative to the game''s informal diffusion.',
    'If household social-scaffold play vastly outnumbers sanctioned tournament play, the orthodoxy reading''s claim to be ''the'' legitimate framework (rather than ''a'' niche legitimate framework) is weaker than the reading itself asserts — though this does not change this story''s own ε, since ε is authored per-reading, not per-prevalence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodoxy_reading_selection_ambiguity, empirical, 'Whether tournament orthodoxy is the dominant or a minority reading of the shared rule-text.').

omega_variable(
    elimination_endpoint_valuation_framing,
    'Is the rulebook''s elimination endgame properly read as ''the competitive substance skill measures'' (this reading) or does that framing itself smuggle in an implicit endorsement of the extraction-demo reading''s pedagogical claim that elimination demonstrates something true about capitalism?',
    'Textual and design-history analysis of whether tournament rules bodies (e.g., official Monopoly tournament committees) frame elimination as sport mechanics or as social commentary in their own rules documentation and public statements.',
    'If tournament bodies'' own framing leans toward sport-mechanics-only, this reading is cleanly distinct from extraction_demo_reading as claimed; if their framing imports pedagogical language, the two readings are less cleanly separable than the kernel decomposition assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elimination_endpoint_valuation_framing, conceptual, 'Whether the orthodoxy reading''s neutral sport framing is fully independent of the extraction-demo reading''s pedagogical framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 4, 0.04).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 12, 0.05).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 4, 0.05).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 12, 0.06).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 16, 0.06).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__tournament_orthodoxy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the monopoly_rulebook kernel. extraction_demo_reading treats the fixed text as vindicating inevitable monopoly concentration (high ε, pedagogical framing). social_scaffold_reading treats the fixed text as unplayable without community-injected correction (moderate ε, house rules as necessary buffer). This reading (tournament_orthodoxy_reading) treats the fixed text as the sole legitimate competitive referent and authors the lowest ε of the three (~0.06) because it denies both the extraction narrative's inevitability claim and the scaffold narrative's correction-necessity claim, modeling voluntary competitive coordination instead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
