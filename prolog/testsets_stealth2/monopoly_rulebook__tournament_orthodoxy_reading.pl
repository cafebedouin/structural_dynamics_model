% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Tournament Rulebook Orthodoxy (Monopoly Kernel, Text-Authority Reading)
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested monopoly_rulebook
 *   kernel: the tournament-orthodoxy reading, under which the published
 *   rulebook is the legitimate competitive framework, strategic skill
 *   determines outcomes under exact text, house rules are noise obscuring
 *   competitive depth, and text authority is immutable for ranking and
 *   comparison purposes. The standing arrangement under contest — the
 *   rulebook-governed competitive order — is the epsilon referent, assessed
 *   by this reading's own lights: it sees a voluntary, low-cost coordination
 *   standard that makes skill commensurable across tables, venues, and years.
 *   The sibling readings (extraction_demo_reading, social_scaffold_reading)
 *   are separate constraint files with their own epsilon values and
 *   stakeholder structures; they are linked via network.affects_constraints
 *   and discussed only in kernel_context, not imported into this constraint's
 *   classification. Assumptions stated: the interval anchors to the 1935
 *   commercial publication of the canonical text; metric values are author
 *   judgments from the historical record of organized play; sampling
 *   parameters were assumed (see provenance).
 *
 * KEY AGENTS:
 *   - - ranked_tournament_players: Primary beneficiary (moderate/mobile) — competes under the shared standard, collects comparable rankings and portable standing
 *   - - tournament_sanctioning_bodies: Agenda setter (institutional/mobile) — administers text authority, publishes errata and FAQ rulings, certifies events
 *   - - rulebook_publisher: Secondary beneficiary (institutional/arbitrage) — owns the canonical text, profits from uniform brand consistency
 *   - - casual_house_rule_players: Excluded outsider (powerless/mobile) — plays by negotiated deviations outside the ranked world, dismissed rhetorically but untouched operationally
 *   - - game_theory_analysts: Analytical observer (analytical/analytical) — sees the full structure of the standard and its strategic consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.07).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.12).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Tournament Rulebook Orthodoxy (Monopoly Kernel, Text-Authority Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game theory / social coordination / institutional design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '7d93a2cd-8e59-4ed6-abb4-5e72988c019d').
narrative_ontology:cs_kernel_codification('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', fixed_text).
narrative_ontology:cs_authority_grounding('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', lineage).
narrative_ontology:cs_interpretation_layer_present('7d93a2cd-8e59-4ed6-abb4-5e72988c019d').
narrative_ontology:cs_reading_relation('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_axiom('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', foundational, text_authority_immutable_for_ranking).
narrative_ontology:cs_axiom_status(text_authority_immutable_for_ranking, holdable).
narrative_ontology:cs_axiom_grounding('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', text_authority_immutable_for_ranking, conventional).
narrative_ontology:cs_axiom('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', foundational, skill_determines_outcomes_under_exact_text).
narrative_ontology:cs_axiom_status(skill_determines_outcomes_under_exact_text, holdable).
narrative_ontology:cs_axiom_grounding('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', skill_determines_outcomes_under_exact_text, empirically_contingent).
narrative_ontology:cs_axiom('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', secondary, house_rules_degrade_ranking_comparability).
narrative_ontology:cs_axiom_status(house_rules_degrade_ranking_comparability, holdable).
narrative_ontology:cs_axiom_grounding('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', house_rules_degrade_ranking_comparability, instrumental).
narrative_ontology:cs_reference_frame('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', canonical_text_supremacy).
narrative_ontology:cs_drift_state('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('7d93a2cd-8e59-4ed6-abb4-5e72988c019d', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, ranked_tournament_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, rulebook_publisher).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_sanctioning_bodies).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, textual_standardization_enables_skill_comparison).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter events where every table runs the identical published procedure. Their wins and losses accumulate into rankings that mean the same thing across venues and years, letting skill, not table negotiation, determine standing. Their main cost is learning the exact text; leaving the circuit costs nothing but forfeited standings, and many players cycle in and out freely across life stages.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, ranked_tournament_players, beneficiary,
    moderate, biographical, mobile, global).

% Publish errata and FAQ rulings, certify events, train referees, and adjudicate disputes by appeal to the text. They decide which interpretations become official and collect prestige, event fees, and institutional continuity from the order they administer. Their authority depends on the community continuing to treat the text as final; they can amend the text but rarely do, preferring clarification to revision.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_sanctioning_bodies, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, tournament_sanctioning_bodies, beneficiary).

% Owns the game's intellectual property and prints the canonical text. Uniform play protects brand consistency and keeps licensing simple. The publisher profits from sales of the game rather than from the standard's day-to-day operation, and retains full freedom to reprint, rebrand, or license variants at will.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rulebook_publisher, beneficiary,
    institutional, generational, arbitrage, global).

% Play socially with negotiated deviations — bonus payouts on lucky spaces, auction tweaks, shortened endgames. The orthodoxy's public rhetoric ranks their practice as noise obscuring depth, though nothing in the standard's operation touches their tables. They have no seat in rules-committee deliberations and mostly ignore the conversation that defines legitimate competitive play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    powerless, biographical, mobile, global).

% Study how the standard shapes strategic depth: which procedures reward calculation, where the text's incentive structure concentrates play, and how reliably rankings track skill. They take no side in the house-rules dispute and bear no costs from the arrangement.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_theory_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces comparable competitive outcomes: a single published standard makes games played at different tables, times, and venues commensurable, so rankings, records, and skill claims share common referents, and in-game disputes have a determinate textual resolution instead of table-by-table negotiation.
% TRANSFER_FUNCTION: Moves interpretive authority from individual tables to the canonical text; moves learning effort (exact-procedure memorization) from players into the standard; moves reputational standing through rankings computed on the shared standard. No material wealth transfers through the arrangement itself; entry fees purchase event services, not standing under the rule.
% ABSENT_VOICES: Casual house-rule players and variant designers would object that the orthodoxy ranks their play as noise rather than legitimate parallel practice. They sit outside the ranked circuit — in homes, cafes, and online forums — and have no seat in rules-committee deliberations. Variant designers proposing alternate modes are heard only as product proposals to the publisher, never as challenges to text authority inside the competitive apparatus.
% DISAPPEARANCE_RATIONALE: If text authority vanished overnight, each event would negotiate its own ruleset; cross-event rankings and historical records would lose comparability; the competitive calendar would fragment into incompatible leagues until new standards emerged. The casual world would continue unchanged — the rearrangement is confined to the competitive apparatus built on the standard, which is precisely the set of arranged interests the stakeholders occupy.
% FOUNDING_PROBLEM: Early competitive play faced table-to-table variation: identical skill produced incomparable outcomes, disputes had no resolution authority, and a win at one venue proved nothing at another. The standard was adopted to make competitive results commensurable and disputes decidable by text.
% FOUNDING_PROBLEM_CORROBORATION: Game historians and archival records of early tournament disputes corroborate the founding problem independently of the benefiting parties, and academic game-studies literature documents pre-standardization incomparability. Casual players do not contest that the problem existed — they contest only its relevance to unranked play. No source attests the problem is solved-and-gone while ranked play continues, which is what a dead-status finding would require.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored very low (0.07 at interval end): the standard's principal demand on participants is learning effort — memorizing exact procedures — which is approximately the price of the comparability good itself, and no material wealth transfers through the arrangement. Suppression is low (0.12): referees and FAQ rulings settle in-event disputes, but participation is voluntary, exit is open, and the standard compels no one outside entered events; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater is low (0.11): championship ceremony and official branding have grown with professionalization, but the core function — producing comparable results — remains fully operational. Accessibility_collapse is 0.30: alternatives (house rules, variants, other games) remain fully available outside ranked play; within ranked play the standard is constitutive rather than coercive, since rankings over incomparable rulesets are meaningless by definition. Resistance is 0.20: casual culture ignores rather than fights the orthodoxy, with occasional friction from variant advocates and players chafing at strict text calls. The suppression_requirement series is authored deliberately: the story tracks enforcement-capacity change (informal club refereeing, to formal sanctioning bodies with trained referees, to digitally auto-enforced rules), so a rising-but-absolutely-low trajectory is the honest picture; the scalar base_properties.suppression (0.12) reflects the end-state. All three tracked metrics share one time grid — every metric is authored at every examined time point — so no end-state substitution contaminates earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer seat; the divergence the engine should compute runs between insider and outsider seats. From the ranked players' and sanctioning bodies' positions the standard is enabling infrastructure — the thing that makes their activity possible and meaningful. From the casual players' position the same orthodoxy exists mainly as rhetoric that ranks their play as noise: they experience its cultural shadow, not its operation, and bear no operational cost. The publisher's seat benefits incidentally without administering. The engine computes per-seat classifications from the structural data; the authored rope claim does not adjudicate these differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (ranked_tournament_players, rulebook_publisher) derive low directionality — the standard subsidizes their activity — amplified slightly for the publisher by arbitrage-grade exit (it owns the text and can reprint or rebrand at will). No victims are declared: the expected structural delta is a no-victim-set, voluntary-participation arrangement, and the descriptive record supports it. The sanctioning bodies, as agenda-setting administrators with a genuine secondary beneficiary position, derive low directionality from their beneficiary data. The excluded casual players have no declarable structural relationship — they are outside the arrangement's operation entirely — and the canonical fallback would leave their position ambiguous; the directionality override (powerless -> 0.5) encodes 'unaffected symmetric': neither subsidized nor taxed, costs and benefits both approximately zero. Scope is global for the ranked circuit, which mildly amplifies effective extraction for any target seat — but with no targets declared, the amplification has nothing to act on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making competitive results commensurable and disputes decidable — is live for as long as ranked play exists, so no mandate-outlived-function declaration is made and no piton signature is expected. The theater_ratio series is the tripwire for the opposite failure: if competitive play migrates to platforms that auto-enforce rules in software, the human referee and errata apparatus could persist ceremonially after its function atrophied; a rising theater ratio past 0.5 would flag that transition. The classification equally guards against mislabeling in the other direction: critics reading any enforced standard as extraction must confront the absence of a victim set, the openness of exit, and the near-zero material transfer — the structural data that distinguish this rope from an enforced monopoly arrangement wearing a rulebook's clothes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the tournament-orthodoxy framing describe the rulebook''s operative function for the population of players, or is it a minority competitive institution''s self-description? This story instantiates one reading of the monopoly_rulebook kernel; the extraction_demo_reading and social_scaffold_reading siblings would relocate the disagreement to rent extraction or to mandated communal correction respectively.',
    'Participation-weighted measurement: what fraction of total play occurs under exact published text versus negotiated house rules, weighted by player-hours rather than by institutional visibility.',
    'If house-rule play dominates aggregate play, this reading describes a niche institution and the population-level account shifts toward the social_scaffold_reading''s structure; the orthodoxy would remain accurate only for the ranked subpopulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether this reading''s low-extraction coordination account generalizes beyond the ranked subpopulation.').

omega_variable(
    competitor_exit_voluntariness,
    'Is exit from ranked play genuinely costless (mobile), or do sunk rating investment, earned standing, and competitive identity fuse committed players to the standard in ways that function as identity lock?',
    'Longitudinal study of lapsed ranked competitors: retention pressure reports, return rates, and whether departure carries perceived identity loss comparable to leaving a profession.',
    'If a committed-player subset is identity_locked, effective extraction rises for that seat and the seat''s computed classification could shift away from pure voluntary-coordination toward a hybrid with a locked target stratum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitor_exit_voluntariness, empirical, 'Whether the voluntary-participation premise holds for the most invested competitors.').

omega_variable(
    text_immutability_descriptive_status,
    'Is text authority actually immutable in operation, or does the authority revise substantively through errata, FAQ rulings, and digital-edition patches while presenting revisions as mere clarification?',
    'Errata-history audit classifying published revisions as clarificatory versus substantive rule changes, across print and digital editions.',
    'Frequent substantive quiet revision would raise theater_ratio, contradict the immutability axiom descriptively, and register as drift from the declared reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_immutability_descriptive_status, empirical, 'Whether the immutability claim survives contact with the revision record.').

omega_variable(
    status_pressure_on_casual_play,
    'Does the orthodoxy govern only ranked contexts, or does its ''house rules are noise'' rhetoric exert status-hierarchy pressure on casual play, imposing a diffuse reputational cost on players who never enter the ranked world?',
    'Survey and discourse analysis of casual communities: measured stigma, defensive framing, and abandonment of enjoyed house rules attributable to official-standard prestige.',
    'If status pressure is real, casual players bear a thin diffuse cost and a marginal victim set exists that this story declines to declare; the classification would sit nearer a hybrid coordination/extraction boundary at the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_pressure_on_casual_play, conceptual, 'Scope of the orthodoxy''s normative reach beyond the ranked circuit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t1935, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1935, 0.03).
narrative_ontology:measurement_basis(mono_tr_t1935, observed).
narrative_ontology:measurement(mono_tr_t1955, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1955, 0.04).
narrative_ontology:measurement_basis(mono_tr_t1955, observed).
narrative_ontology:measurement(mono_tr_t1975, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1975, 0.05).
narrative_ontology:measurement_basis(mono_tr_t1975, observed).
narrative_ontology:measurement(mono_tr_t1995, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1995, 0.06).
narrative_ontology:measurement_basis(mono_tr_t1995, observed).
narrative_ontology:measurement(mono_tr_t2015, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement_basis(mono_tr_t2015, observed).
narrative_ontology:measurement(mono_tr_t2025, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 2025, 0.11).
narrative_ontology:measurement_basis(mono_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t1935, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1935, 0.04).
narrative_ontology:measurement_basis(mono_be_t1935, observed).
narrative_ontology:measurement(mono_be_t1955, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1955, 0.04).
narrative_ontology:measurement_basis(mono_be_t1955, observed).
narrative_ontology:measurement(mono_be_t1975, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1975, 0.05).
narrative_ontology:measurement_basis(mono_be_t1975, observed).
narrative_ontology:measurement(mono_be_t1995, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement_basis(mono_be_t1995, observed).
narrative_ontology:measurement(mono_be_t2015, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 2015, 0.06).
narrative_ontology:measurement_basis(mono_be_t2015, observed).
narrative_ontology:measurement(mono_be_t2025, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 2025, 0.07).
narrative_ontology:measurement_basis(mono_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t1935, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1935, 0.03).
narrative_ontology:measurement_basis(mono_su_t1935, observed).
narrative_ontology:measurement(mono_su_t1955, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1955, 0.04).
narrative_ontology:measurement_basis(mono_su_t1955, observed).
narrative_ontology:measurement(mono_su_t1975, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1975, 0.05).
narrative_ontology:measurement_basis(mono_su_t1975, observed).
narrative_ontology:measurement(mono_su_t1995, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1995, 0.07).
narrative_ontology:measurement_basis(mono_su_t1995, observed).
narrative_ontology:measurement(mono_su_t2015, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement_basis(mono_su_t2015, observed).
narrative_ontology:measurement(mono_su_t2025, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 2025, 0.13).
narrative_ontology:measurement_basis(mono_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, social_scaffold_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Monopoly rulebook' covers three structurally distinct claims instantiated by three readings of one kernel. This file (tournament_orthodoxy_reading) authors epsilon ~0.07 for the rulebook-governed competitive order as a voluntary coordination standard. The extraction_demo_reading authors high epsilon over the same standing arrangement, read as rent-concentration demonstration; the social_scaffold_reading authors its own epsilon over the arrangement read as unplayable-without-correction. Per the epsilon-invariance principle these are separate stories with separate beneficiary/victim structures, linked here; the upstream orthodoxy reading influences the downstream scaffold reading's operating environment (official-dismissal rhetoric) without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__tournament_orthodoxy_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
