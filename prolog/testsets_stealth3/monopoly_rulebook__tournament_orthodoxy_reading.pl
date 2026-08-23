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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Tournament Orthodoxy Reading of the Monopoly Rulebook
 *   domain: game theory/social coordination/institutional design
 *
 * SUMMARY:
 *   In this reading, competitive Monopoly treats the printed rulebook as the
 *   sole legitimate framework of play: auctions are mandatory, rents follow
 *   the printed schedule, and no table amendment enters ranked results. The
 *   arrangement coordinates a dispersed player population around a shared
 *   standard, making ratings, titles, and cross-decade records possible;
 *   participation is voluntary, and the population that plays amended
 *   variants stands outside the ranked conversation rather than beneath it.
 *   Assumptions: interval 0–50 maps approximately to 1975–2025, spanning the
 *   revival of standardized national tournament play through the era of
 *   online ranked ladders. Claim and metrics are independently authored: the
 *   claimed type states what this reading's structure is; the metric values
 *   state what its operation looks like — both authored from the reading's
 *   own lights, with the engine computing per-seat classifications. KEY
 *   AGENTS (by structural relationship): - ranked_tournament_players: Primary
 *   beneficiary (organized/mobile) — competes under the fixed text, collects
 *   comparable standing - championship_sanctioning_bodies: Agenda-setter
 *   (institutional/constrained) — certifies events, maintains the rating
 *   ladder and archive - game_publisher_rules_office: Text owner and
 *   administrator (institutional/arbitrage) — issues editions and errata,
 *   collects incidental brand value - tournament_organizers: Local
 *   enforcer-beneficiary (organized/mobile) — runs compliant events -
 *   casual_house_rule_players: Excluded voice (moderate/mobile) — plays
 *   amended variants outside the ranked conversation -
 *   competitive_game_theorists: Analytical observer (analytical/analytical) —
 *   measures the skill/luck structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.07).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.16).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Tournament Orthodoxy Reading of the Monopoly Rulebook").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game theory/social coordination/institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__tournament_orthodoxy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '9a1fc74c-7ebd-4663-9dfd-701838d1a78e').
narrative_ontology:cs_kernel_codification('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', fixed_text).
narrative_ontology:cs_authority_grounding('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', lineage).
narrative_ontology:cs_interpretation_layer_present('9a1fc74c-7ebd-4663-9dfd-701838d1a78e').
narrative_ontology:cs_reading_relation('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', foundational, text_immutable_for_ranking).
narrative_ontology:cs_axiom_status(text_immutable_for_ranking, holdable).
narrative_ontology:cs_axiom_grounding('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', text_immutable_for_ranking, conventional).
narrative_ontology:cs_axiom('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', foundational, skill_determines_outcomes_under_text).
narrative_ontology:cs_axiom_status(skill_determines_outcomes_under_text, holdable).
narrative_ontology:cs_axiom_grounding('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', skill_determines_outcomes_under_text, empirically_contingent).
narrative_ontology:cs_axiom('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', secondary, house_rules_obscure_competitive_depth).
narrative_ontology:cs_axiom_status(house_rules_obscure_competitive_depth, holdable).
narrative_ontology:cs_axiom_grounding('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', house_rules_obscure_competitive_depth, conventional).
narrative_ontology:cs_reference_frame('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', immutable_canonical_text).
narrative_ontology:cs_drift_state('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', contemporary_edition_churn, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9a1fc74c-7ebd-4663-9dfd-701838d1a78e', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, ranked_tournament_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, championship_sanctioning_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, game_publisher_rules_office).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, text_immutability_for_ranking).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, meritocratic_skill_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter sanctioned events and play by the printed text exactly: auctions are mandatory, rents follow the printed schedule, no table additions enter the result. They collect comparable ratings, titles, and a shared competitive history. Leaving ranked play is a genre switch to casual tables, not a flight from penalty — nothing binds them except the wish to be ranked.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, ranked_tournament_players, beneficiary,
    organized, biographical, mobile, global).

% Certify events, publish clarifications, and maintain rating ladders keyed to the printed text. Their authority and their archival records both rest on the text staying put; adopting a revised canon would orphan decades of comparable results, so they pin editions and defend the text as written.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, championship_sanctioning_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Owns the text, issues editions and errata, and licenses official play. Collects brand-integrity value and an official-event economy anchored to the standard. Holds unilateral power to revise the text yet rarely exercises it, because revision would break the comparability the competitive scene organizes around.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_publisher_rules_office, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, game_publisher_rules_office, beneficiary).

% Run local and regional events under the printed text: verify setups, adjudicate disputes, apply published clarifications. Standardization gives them viable, marketable events with portable results; hosting unsanctioned variant nights remains open to them but forfeits the ranked draw.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    organized, biographical, mobile, regional).

% Play amended versions — jackpot pots, skipped auctions, negotiated loans — in homes and cafés. They sit outside the ranked conversation entirely and would object to having their play dismissed as noise; nothing penalizes them, they simply never appear in standings.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, biographical, mobile, global).

% Study the game's structure: luck-versus-skill ratios, first-player advantage, edition differences, strategic depth of the canonical ruleset. Take no seat in ranked legitimacy; publish measurements that both the ranked community and its critics cite.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_game_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one canonical standard so that game outcomes are comparable across tables, cities, and years; solves the collective-action problem that without a fixed text, 'who won' is table-relative and titles, ratings, and records cannot aggregate.
% TRANSFER_FUNCTION: Allocates comparative standing and title legitimacy among players according to performance under the fixed text; moves no money, goods, or labor — the only flows are status upward to winners and a small learning cost onto every entrant.
% ABSENT_VOICES: Casual house-rule players and variant designers are outside tournament governance; they would argue the printed text is one playable convention among many rather than the legitimate one. Game historians who read the text as cultural artifact are likewise absent from rules committees.
% DISAPPEARANCE_RATIONALE: If text-authority for ranking vanished overnight, championships would lose comparability, rating ladders and historical records would invalidate, and organizers would improvise ad hoc standards until some canon re-emerged; casual play would continue untouched.
% FOUNDING_PROBLEM: House-rule divergence made competitive comparison impossible and produced degenerate marathon sessions — skipped auctions slowed property transfer and games stalled for hours; early tournament organizers needed a fixed canon before any champion could be meaningfully crowned.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: published survey research on household rule modification attests the divergence is real and persistent; game historians document the tournament standardization episodes; the publisher's own historical official-play campaigns targeted casual households — evidence the divergence problem existed independently of the ranked community's interests.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction is very low (0.07): the arrangement's costs are a learning burden on entrants and the exclusion of variant preferences from ranked results, with no material transfer away from anyone. Suppression (0.16) is definitional rather than coercive — non-compliant games simply do not count, upheld by event verification and rating-eligibility checks; it is authored as a raw structural property and is not scaled by power or scope. Theater is near zero (0.05): clarification publishing and event verification are functional acts. Accessibility collapse is moderate-low (0.40): amended play flourishes everywhere outside ranked comparison, but within the ranking use case the alternative collapses completely once a player understands that only canonical results count. Resistance is low (0.12): grumbling about the official game's harshness exists, but no constituency actively fights the canon, and dissenters exit to casual play at zero cost. All three tracked series share one six-point grid (0, 10, 20, 30, 40, 50); trajectories are nearly flat, matching a stable coordination standard, with end-state values equal to the scalar base properties. Receipt surface: each seat was checked — players receive diffuse comparability, organizers receive event viability, the publisher receives incidental brand value; no seat captures the minimal compliance burden as concentrated gain, so gain_flow is authored 'diffuse' as an affirmative finding, not a default. Fixing cost is 'prohibitive': replacing the canon would invalidate the accumulated comparative record and fragment the ranked community, while the benefit of fixing (accommodating variant tastes) is marginal. The prohibitive-plus-diffuse cell nominally resembles a degraded-institution signature; here the live founding problem, near-zero theater, and flat trajectories place the arrangement in functioning-coordination territory — the receipt surface records facts, not a type.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the publisher's chair the arrangement is stewardship of an asset whose stability customers pay for; from the ranked player's chair it is a fair arena bought with a small learning cost; from the sanctioning body's chair it is the load-bearing wall of an archival project; from the excluded casual player's chair it is an invisible gate that ranks somebody else's game. Nothing structural forces these seats into conflict — no seat bears concentrated costs — so divergence surfaces as indifference and rhetorical dismissal rather than extraction politics.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (ranked players, organizers, sanctioning bodies) derive directionality near the beneficiary end: the standard subsidizes their activity at trivial cost. No victim group is declared and none exists — participation is voluntary and exit is a genre switch — so no seat sits near the target end and no amplification path engages. The publisher is deliberately not listed in the beneficiary array: it administers the text and collects incidental brand value, so its seat falls to the canonical fallback for its power atom, landing near-symmetric with a mild beneficiary tilt; no override is needed because the structural declarations already produce the right relationships for every declared seat. Excluded casual players stand outside the flow entirely: nothing transfers to or from them. Global scope mildly scales whatever extraction exists, but with base extraction this low the scaled figure remains negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — incomparable outcomes under divergent house rules — is still live wherever competitive play arises, so there is no outlived mandate to resolve. The classification's job here is the reverse of the usual: guarding against misreading genuine coordination cost as extraction. The identity-flavored cover-story risk ('this is simply how serious play works') is checked by the structure itself: exit is mobile, participation voluntary, and no seat profits from another's burden, so the low measured extraction is credible as coordination cost rather than cover. The Boltzmann floor for an information standard (0.02) sits below the measured 0.07, leaving a small excess flagged for review — appropriate conservatism for a standard maintained by an interested publisher.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the monopoly_rulebook kernel; would instantiating extraction_demo_reading or social_scaffold_reading instead change the beneficiary/victim structure and epsilon?',
    'Author and compile the sibling stories; compare computed per-seat classifications and epsilon across readings of the same text.',
    'If the scaffold reading is adopted, house-rule deviation becomes functional rather than noise and epsilon rises modestly with text-purist seats bearing costs; if the demo reading is adopted, a victim set appears (eliminated players as demonstration subjects) and epsilon rises sharply. This file''s low epsilon holds only within the orthodoxy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    edition_immutability_ambiguity,
    'Is ''the text'' a single immutable object, or an edition-indexed family (US/UK divergence, errata cycles, official speed-die variant) such that cross-edition rankings silently compare different standards?',
    'Audit sanctioned events'' pinned editions and rating systems'' edition handling; test whether cross-edition matches occur and how they are normalized.',
    'If rankings span editions without normalization, the immutability axiom is strained and effective extraction rises slightly as comparison validity degrades; if events pin editions, the reading is internally consistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edition_immutability_ambiguity, empirical, 'Whether the immutability premise survives edition churn in ranked practice.').

omega_variable(
    skill_determination_degree,
    'How much of outcome variance under the canonical text is strategic skill versus dice luck (first-player advantage, chance draws)? The reading''s legitimacy claim that skill determines outcomes depends on the answer.',
    'Statistical analysis of rated tournament results: repeat-winner rates, rating predictive power, first-player advantage studies.',
    'If luck dominates, the orthodoxy''s legitimacy premise weakens toward the scaffold reading''s correction stance; if skill dominates robustly, the coordination classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_determination_degree, empirical, 'Empirical load-bearing test of the reading''s foundational skill-determination premise.').

omega_variable(
    boundary_pressure_voluntariness,
    'Is the measured suppression purely boundary-definitional (non-compliant games simply do not count), or does status pressure from ranked-community discourse push casual groups toward canonical play they would not freely choose?',
    'Longitudinal survey of house-rule groups exposed to ranked-community discourse: track rule-adoption drift absent any material incentive.',
    'If drift occurs, suppression is partly internalized or social and effective suppression exceeds the structural measure; if stable, the arrangement''s suppression stays definitional and low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_pressure_voluntariness, conceptual, 'Whether the low suppression figure reflects pure boundary definition or misses soft status pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tournament_orthodoxy_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(tournament_orthodoxy_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(tournament_orthodoxy_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(tournament_orthodoxy_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement(tournament_orthodoxy_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(tournament_orthodoxy_tr_t50, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(tournament_orthodoxy_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(tournament_orthodoxy_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(tournament_orthodoxy_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(tournament_orthodoxy_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(tournament_orthodoxy_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(tournament_orthodoxy_be_t50, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 50, 0.07).

% Suppression requirement over time
narrative_ontology:measurement(tournament_orthodoxy_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(tournament_orthodoxy_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(tournament_orthodoxy_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(tournament_orthodoxy_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(tournament_orthodoxy_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(tournament_orthodoxy_su_t50, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 50, 0.16).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Monopoly rulebook' covers three structurally distinct constraints instantiated by three readings of one kernel. Epsilon differs sharply across the family: this orthodoxy reading is near-zero (a voluntary measurement standard), the scaffold reading carries moderate extraction with text-purist seats bearing costs, and the demo reading is highest, with eliminated players cast as demonstration subjects. The orthodoxy reading is the empirically settled baseline — sanctioned play actually operates on it — and the sibling readings define themselves against it, so edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
