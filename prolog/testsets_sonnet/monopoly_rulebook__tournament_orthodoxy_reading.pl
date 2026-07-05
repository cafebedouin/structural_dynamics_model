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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Official Monopoly Rules as Tournament Standard (Orthodox Reading)
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   This story instantiates the tournament-orthodoxy reading of the shared
 *   Monopoly-rulebook kernel: the published rules constitute the legitimate
 *   competitive standard, house rules are simply a different
 *   (non-competitive) activity rather than a corrective, and text authority
 *   is treated as fixed for purposes of ranking and cross-event comparison.
 *   This reading is deliberately narrow and clean — it does not describe the
 *   kitchen-table social dynamics (that is the social_scaffold_reading) and
 *   does not describe the game's function as capitalism pedagogy (that is the
 *   extraction_demo_reading). Those are different constraints instantiated
 *   from the same kernel text; this file's epsilon and stakeholder set
 *   describe only the competitive-tournament instantiation.
 *
 * KEY AGENTS:
 *   - competitive_tournament_community: primary beneficiary (organized/mobile) — gains comparability and rankable skill expression
 *   - national_and_regional_monopoly_associations: agenda_setter (organized/mobile) — administers the standard, extracts nothing beyond logistics costs
 *   - rules_officials: agenda_setter/observer (moderate/mobile) — adjudicate by citing text, hold no independent discretionary power
 *   - casual_house_rule_players: excluded (powerless/arbitrage) — not victims, simply non-participants in a different game
 *   - commentator_analysts: analytical observer — treats the fixed text as the measurement instrument for strategic depth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.15).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Official Monopoly Rules as Tournament Standard (Orthodox Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game theory / social coordination / institutional design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '799ccb64-c682-4ab0-8d3c-34361eebe50a').
narrative_ontology:cs_kernel_codification('799ccb64-c682-4ab0-8d3c-34361eebe50a', fixed_text).
narrative_ontology:cs_authority_grounding('799ccb64-c682-4ab0-8d3c-34361eebe50a', practice).
narrative_ontology:cs_interpretation_layer_present('799ccb64-c682-4ab0-8d3c-34361eebe50a').
narrative_ontology:cs_reading_relation('799ccb64-c682-4ab0-8d3c-34361eebe50a', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('799ccb64-c682-4ab0-8d3c-34361eebe50a', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_axiom('799ccb64-c682-4ab0-8d3c-34361eebe50a', foundational, text_fixity_grounds_competitive_legitimacy).
narrative_ontology:cs_axiom_status(text_fixity_grounds_competitive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('799ccb64-c682-4ab0-8d3c-34361eebe50a', text_fixity_grounds_competitive_legitimacy, conventional).
narrative_ontology:cs_axiom('799ccb64-c682-4ab0-8d3c-34361eebe50a', foundational, voluntary_nonparticipation_is_not_exclusion).
narrative_ontology:cs_axiom_status(voluntary_nonparticipation_is_not_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('799ccb64-c682-4ab0-8d3c-34361eebe50a', voluntary_nonparticipation_is_not_exclusion, conventional).
narrative_ontology:cs_reference_frame('799ccb64-c682-4ab0-8d3c-34361eebe50a', published_rulebook_as_tournament_canon).
narrative_ontology:cs_drift_state('799ccb64-c682-4ab0-8d3c-34361eebe50a', contemporary_sanctioned_play_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('799ccb64-c682-4ab0-8d3c-34361eebe50a', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_community).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, national_and_regional_monopoly_associations).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, rules_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A voluntary body of players who agree to play by the published rulebook precisely because it produces a comparable, rankable game across venues and years. They benefit from a stable ruleset that lets skill differences (trading strategy, auction timing, mortgage leverage, jail-timing tactics) actually determine outcomes rather than being scrambled by ad hoc local variants. Anyone unhappy with the standard can simply not enter sanctioned play; nothing traps them at the table.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_community, beneficiary,
    organized, generational, mobile, national).

% Bodies (e.g. national Monopoly federations feeding into world championship qualification) that adopt Hasbro's published rules as the tournament standard, publish clarifications, and run sanctioned events. They administer the standard but do not extract rents from it beyond ordinary event fees that fund logistics; they could in principle adopt a house-ruled variant but have no incentive to, since the coordination value comes precisely from using the same text everyone else uses.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, national_and_regional_monopoly_associations, agenda_setter,
    organized, generational, mobile, national).

% Volunteer or lightly compensated tournament directors and rules judges who apply the text to adjudicate disputes (auction procedure, free parking variant exclusion, exact-change rules). Their authority is entirely derivative of the text; they resolve ambiguity by citing the rulebook rather than personal discretion, and players who disagree can appeal to the written standard itself.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rules_officials, agenda_setter,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, rules_officials, observer).

% Family and friend-group players who add free-parking jackpots, slow elimination, or other social-lubricant variants for kitchen-table play. From the tournament-orthodoxy standpoint, they are not victims of anything — they have simply opted into a different, non-competitive game and have full freedom to do so; their preferences are irrelevant to what counts as the tournament standard, not suppressed by it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    powerless, immediate, arbitrage, local).

% Game-theory and competitive-strategy writers who analyze sanctioned play, publish strategy guides, and treat the official text as the fixed ruleset against which strategic depth (opening trades, monopoly-building sequencing, cash-flow management) can be measured and compared across events and eras.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, commentator_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adopting one immutable published rulebook lets geographically dispersed players and events compare outcomes, rank performance, and develop transferable strategic skill, because everyone is playing the identical game rather than a locally mutated one.
% TRANSFER_FUNCTION: The arrangement moves almost nothing extractively: entry/event fees cover logistics, and the main thing transferred is comparability itself — a shared standard that lets skill be legible and rankable across the community. No party is charged a rent for the standard's existence.
% ABSENT_VOICES: Casual house-rule players are not represented in tournament governance, but they are not seeking representation there either — this reading holds their absence is simply non-participation in a different activity, not exclusion from a shared one. If anyone would formally object, it is a player wanting official recognition of a house variant as competitive, but no such faction is organized within sanctioned play.
% DISAPPEARANCE_RATIONALE: If the fixed rulebook standard vanished, sanctioned tournament play would fragment into incompatible local variants; rankings, records, and cross-event skill comparison would become meaningless because 'winning' would no longer refer to the same game. The competitive community's entire evaluative apparatus depends on the text staying fixed.
% FOUNDING_PROBLEM: Early 20th-century commercial board games needed a single arbitrable rule text so that disputes at the table (and later, across clubs and tournaments) could be resolved by appeal to a shared authority rather than by argument or house preference.
% FOUNDING_PROBLEM_CORROBORATION: Tournament directors and competitive strategy analysts outside the rules-issuing associations (independent game-theory commentators, national federation audits of dispute logs) corroborate that rule disputes are still routinely resolved by citation to the published text, and that cross-event ranking systems still presuppose a single fixed ruleset — the coordination problem the rulebook was built to solve remains actively operative in sanctioned play today.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored very low (0.08 at interval end) because no party collects rent from the standard's existence beyond ordinary event-logistics fees, and participation is fully voluntary. Suppression is low (0.15): nothing coerces anyone into sanctioned play, and the 'suppression' of house-rule variants within tournament contexts is simply definitional exclusion from a different ruleset for a different activity, not force applied to unwilling participants. Theater ratio is low (0.10) — rules officiating is functional dispute resolution, not performance. Accessibility collapse is moderate (0.4) rather than near-zero: once inside sanctioned play, deviating from the text is not really an option if the point is cross-event comparability, but exiting to casual play remains fully available at all times, which caps how far alternatives can be said to collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive community and associations sit near the beneficiary end of directionality: they get exactly what they organized for (comparability, rankable skill) at minimal cost. Rules officials are close to symmetric — they invest effort in adjudication but derive the same standing benefit as any player. Casual house-rule players are NOT declared as victims in this reading: they are excluded from tournament governance but bear no cost from the standard's operation, since they never enter sanctioned play in the first place. This is the key structural claim this reading makes and the sibling readings dispute — see kernel_context and omegas.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for an arbitrable, shared rule text to resolve disputes and enable comparison) remains live and is corroborated by ongoing citation practice in tournament dispute resolution — this is not a mandatrophy case. The rulebook-as-tournament-standard has not outlived its function; if anything the function (cross-event skill comparability) has scaled with the growth of sanctioned competitive play.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthodoxy_vs_scaffold_playability_claim,
    'Is the unmodified published rulebook genuinely sufficient for sustained social play (as this reading holds), or does sustained multi-hour play among non-tournament players structurally require house-rule correction to avoid abandonment (as the social_scaffold_reading holds)?',
    'Comparative retention data: session-completion rates and player satisfaction for strict-rules casual play versus house-ruled casual play, controlling for player experience level and group composition.',
    'If strict-rules casual play reliably completes and satisfies non-competitive players, this reading''s claim that house rules are unnecessary noise (rather than functional correction) is supported. If strict-rules casual sessions systematically stall or produce abandonment, the social_scaffold_reading''s claim that correction is structurally required gains support, and this reading''s dismissal of house rules as mere noise would need revision for the casual (non-tournament) context — though it would remain valid for the tournament context specifically, since that is a different population with different exit incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodoxy_vs_scaffold_playability_claim, empirical, 'Whether the bare text is playable without correction outside the tournament population this reading describes.').

omega_variable(
    text_immutability_vs_pedagogical_intent,
    'Is treating the rulebook''s endgame mechanics (mortgage spirals, forced liquidation, elimination) as neutral competitive procedure a defensible reading, or does it require suppressing the documented pedagogical origin of the game (Lizzie Magie''s Landlord''s Game, designed to demonstrate the injustice of land monopoly) that the extraction_demo_reading foregrounds?',
    'Historical-intent analysis of the game''s design lineage weighed against contemporary community self-understanding of competitive play; neither resolves the other, since original design intent does not bind how a voluntary community subsequently uses a shared text.',
    'If original pedagogical intent is treated as binding on legitimate use, this reading''s neutral-competitive-framework claim is weakened relative to the extraction_demo_reading. If subsequent voluntary community adoption for a different purpose is treated as legitimate regardless of origin, this reading''s low-epsilon rope classification is well-supported. This is a conceptual disagreement about which fact governs legitimacy, not an empirical one resolvable by further data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_immutability_vs_pedagogical_intent, conceptual, 'Whether design-intent history or subsequent voluntary use governs how the text''s legitimacy should be read.').

omega_variable(
    beneficiary_only_structure_stability,
    'Does the beneficiary-only, no-victim structure asserted here hold up if tournament play scales to include economically vulnerable participants for whom entry fees, travel costs, or prize-money dependency change the voluntariness calculus?',
    'Survey of sanctioned tournament entrants'' economic dependence on prize outcomes and financial burden of participation, compared against the assumption of frictionless voluntary entry/exit this reading relies on.',
    'If a material share of the competitive population is participating under economic pressure rather than free preference, a victim set may need to be declared and this reading''s ε would need re-authoring upward — this would not collapse into the social_scaffold or extraction_demo readings but would require a fourth reading or a revision of this one''s beneficiary-only claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_only_structure_stability, empirical, 'Whether the assumed frictionless voluntariness of tournament entry holds as the population scales.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(mono_tr_t32, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 16, 0.07).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(mono_be_t32, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__tournament_orthodoxy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the Monopoly rulebook' per the ε-invariance principle. tournament_orthodoxy_reading (this file, ε≈0.08, rope) treats the text as a low-extraction competitive standard. extraction_demo_reading treats the same text as instantiating pedagogically-intended wealth-concentration mechanics with a declared victim set (eliminated players) and a much higher ε. social_scaffold_reading treats the bare text as insufficient for social playability, requiring house-rule correction, and locates its coordination function in the corrected (not the bare) ruleset. All three share the same kernel_id (monopoly_rulebook) but are structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications — they are linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
