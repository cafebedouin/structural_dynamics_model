% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Monopoly Rulebook — Tournament Orthodoxy Reading (Fixed-Text Competitive Standard)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the tournament_orthodoxy_reading of the
 *   monopoly_rulebook kernel: the printed rulebook as the legitimate
 *   competitive framework for Monopoly. Within this reading, the fixed text
 *   is what makes ranked play possible — a championship result means the same
 *   thing in any venue because the text is immutable; strategic skill, not
 *   rule negotiation, determines outcomes; and house rules, whatever their
 *   social charms, are noise for ranking and comparison purposes. The
 *   constraint coordinates the competitive community around a shared
 *   standard; participation is voluntary, exit is open, and within this
 *   reading's scope there is no victim set. The same printed text supports
 *   two sibling constraints — the extraction_demo reading (the rulebook as
 *   rent-concentration pedagogy) and the social_scaffold reading (the
 *   rulebook as requiring house-rule correction) — which are separate stories
 *   with their own epsilon and beneficiary/victim structures; this story
 *   authors only the orthodoxy reading, clean and epsilon-invariant. KEY
 *   AGENTS (by structural relationship): - game_publisher_rules_office:
 *   Agenda-setter (institutional/arbitrage) — owns the canonical text,
 *   maintains the FAQ, sanctions championship play -
 *   competitive_tournament_players: Primary beneficiary (moderate/mobile) —
 *   the competitive community the standard makes legible -
 *   tournament_rules_judges: Enforcement seat (organized/mobile) — adjudicate
 *   disputes under the text at sanctioned events - casual_house_rule_players:
 *   Excluded voice (moderate/mobile) — the statistical majority of play,
 *   outside the competitive conversation - game_historians_comparativists:
 *   Secondary beneficiary (analytical/analytical) — depend on textual
 *   invariance for cross-era comparison
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.12).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook — Tournament Orthodoxy Reading (Fixed-Text Competitive Standard)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '81213e91-c675-4d15-8425-5cc764909e65').
narrative_ontology:cs_kernel_codification('81213e91-c675-4d15-8425-5cc764909e65', fixed_text).
narrative_ontology:cs_authority_grounding('81213e91-c675-4d15-8425-5cc764909e65', lineage).
narrative_ontology:cs_interpretation_layer_present('81213e91-c675-4d15-8425-5cc764909e65').
narrative_ontology:cs_reading_relation('81213e91-c675-4d15-8425-5cc764909e65', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('81213e91-c675-4d15-8425-5cc764909e65', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_axiom('81213e91-c675-4d15-8425-5cc764909e65', foundational, skill_determines_competitive_outcomes_under_fixed_text).
narrative_ontology:cs_axiom_status(skill_determines_competitive_outcomes_under_fixed_text, holdable).
narrative_ontology:cs_axiom_grounding('81213e91-c675-4d15-8425-5cc764909e65', skill_determines_competitive_outcomes_under_fixed_text, empirically_contingent).
narrative_ontology:cs_axiom('81213e91-c675-4d15-8425-5cc764909e65', foundational, textual_invariance_required_for_ranking_comparability).
narrative_ontology:cs_axiom_status(textual_invariance_required_for_ranking_comparability, holdable).
narrative_ontology:cs_axiom_grounding('81213e91-c675-4d15-8425-5cc764909e65', textual_invariance_required_for_ranking_comparability, instrumental).
narrative_ontology:cs_axiom('81213e91-c675-4d15-8425-5cc764909e65', secondary, house_rules_excluded_from_ranked_play).
narrative_ontology:cs_axiom_status(house_rules_excluded_from_ranked_play, holdable).
narrative_ontology:cs_axiom_grounding('81213e91-c675-4d15-8425-5cc764909e65', house_rules_excluded_from_ranked_play, conventional).
narrative_ontology:cs_reference_frame('81213e91-c675-4d15-8425-5cc764909e65', immutable_canonical_text_standard).
narrative_ontology:cs_drift_state('81213e91-c675-4d15-8425-5cc764909e65', contemporary_publisher_variant_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('81213e91-c675-4d15-8425-5cc764909e65', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, game_historians_comparativists).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, ranked_comparison_requires_fixed_reference).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, skill_outcome_correlation_under_canonical_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the rulebook as intellectual property, publishes each edition, maintains the official FAQ and tournament rules supplements, and sanctions championship play. Decides what counts as the canonical text and who may adjudicate under it. Bears the cost of maintaining the standard — errata, rulings, event oversight — and collects brand coherence, archive value, and licensing control from its stability. Could revise or fork the text at will; its exit is total ownership of the asset.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_publisher_rules_office, agenda_setter,
    institutional, generational, arbitrage, global).

% Compete in sanctioned events for titles, ratings, and prizes under the canonical text. The fixed standard is what makes their preparation and skill legible — a win means the same thing in any venue and any year. They bear the cost of learning the text and accepting its edge rulings. Exit is open: any player can stop competing and play casually without sanction or penalty.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_tournament_players, beneficiary,
    moderate, biographical, mobile, global).

% Adjudicate rules disputes at sanctioned events, apply published rulings, and enforce disqualification where required. They administer the standard rather than collect from it; their cost is the labor and social friction of enforcement. They step down from judging without penalty, and their authority exists only inside sanctioned events.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_rules_judges, agenda_setter,
    organized, biographical, mobile, global).

% The statistical majority of play happens outside the text — free-parking jackpots, auction-optional purchases, negotiated loans, shortened endgames. They are not part of the competitive conversation and would object to the orthodoxy's characterization of their play as noise. Nothing binds them to the text; their exit from its authority is already complete, and their play continues regardless of what tournaments decide.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, biographical, mobile, global).

% Compare editions, rule changes, and play eras across the game's published history. Textual invariance is the reference their comparisons depend on — a 1975 tournament result is legible against a 2023 one only because the text held still. They collect comparability without running or funding the standard.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_historians_comparativists, beneficiary,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, game_publisher_rules_office).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single fixed standard of play so that competitive results are comparable across players, venues, and time: a win under the rulebook means the same thing everywhere, disputes have a resolution point, and skill claims are falsifiable against a common reference.
% TRANSFER_FUNCTION: Moves ranking legitimacy and comparability to players who master the canonical text, and moves rule-adjudication authority (edge-case rulings, dispute settlement) from players to the publisher's text and its judges. No money or goods move through the constraint itself; the transferred goods are standing, legibility, and adjudication authority.
% ABSENT_VOICES: Casual house-rule players — the statistical majority of actual play — are outside the competitive conversation and would object to the orthodoxy's characterization of their play as noise. The extraction-demo lineage (game pedagogues) would object that the text's harsh endgame is the point, not noise to be played through. Neither seat is present in tournament discourse; both are commentary-grade absences, not structural victims within this reading's scope.
% DISAPPEARANCE_RATIONALE: If text authority for rankings vanished overnight, sanctioned competition would fragment into incomparable local rule sets: titles, ratings, and cross-venue comparison would dissolve, and skill claims would lose their falsifiability. Casual play, by contrast, would continue essentially unchanged — the rearrangement is confined to the competitive sphere this reading governs, but within that sphere the dependence is total.
% FOUNDING_PROBLEM: Competitive play needs a fixed reference: without an immutable text, a championship result in one venue cannot be compared to another, disputes have no resolution point, and rankings reset with every local variation. The founding problem was establishing a single authoritative standard of play for ranking and comparison.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the casual-play community concedes that ranked competition requires a fixed reference even as it rejects the orthodoxy's extension of textual authority to casual play; comparative governance literature on ranked games (chess/FIDE, poker, esports) independently attests that cross-venue ranking structurally requires an immutable rules reference. No beneficiary-only attestation is relied on.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.08: the constraint's costs are coordination costs — learning the text, ceding edge-case rulings to the publisher's FAQ — sitting just above the information_standard Boltzmann floor (0.02); no wealth transfer runs through the constraint. Suppression is 0.12: enforcement exists (judges, published rulings, disqualification) but is bounded by voluntary participation and open exit. Theater is 0.09: tournament play is functionally rule-governed; a minor performative stratum ('rules as written' identity signaling online) is visible but not load-bearing. Accessibility_collapse is 0.45: alternatives remain fully workable outside ranked play — house rules are the statistical majority of actual play — so collapse is confined to the comparative purpose itself, which is constitutive rather than suppressive. Resistance is 0.30: low inside tournaments (participants consented), real at the cultural boundary (the casual majority, and the publisher's own house-rules product line, push back on the orthodoxy's exclusivity claim). The measurement series share one grid (t=0..50 at decade steps, mapping roughly to 1973–2023 of sanctioned tournament play), with every tracked metric authored at every point: base_extractiveness drifts 0.03→0.08 as prize circuits and ranking stakes professionalized; suppression_requirement drifts 0.05→0.12 as enforcement machinery matured from booklet self-adjudication to certified judges and published FAQ rulings; theater_ratio drifts 0.04→0.09 with the rise of performative orthodoxy online. All trajectories are gentle — the reading's expected structural delta is a stable, near-floor rope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the competitive player's seat the text is enabling infrastructure — the thing that makes skill legible and titles meaningful. From the casual player's seat (outside the conversation) the same orthodoxy reads as a cultural claim that their play is defective. From the publisher's seat the text is an asset whose immutability preserves archive and brand value; from the judge's seat it is a workload of disputes to adjudicate. Same text, four experiences; the engine computes per-seat classifications from power, exit, and declared position — the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (competitive_tournament_players, game_historians_comparativists) derive low directionality — the standard subsidizes them with comparability at the cost of learning the text. No victim set is declared: within this reading's scope the constraint takes nothing from anyone; the closest seat to cost-bearing is tournament_rules_judges, who pay enforcement labor voluntarily and exit by stepping down. The agenda-setter (game_publisher_rules_office) has no beneficiary/victim declaration for the derivation chain to read, so a directionality override is authored at d=0.35 to encode its actual position: it collects brand coherence, archive value, and adjudication authority from the standard's stability while bearing real maintenance costs — beneficiary-side but not subsidized, and the seat the arrangement's value demonstrably accrues to (hence gain_flow). Suppression is authored as a raw structural property (0.12) and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Scope is global at the tournament tier, which amplifies effective extraction modestly — from a base already near the coordination floor.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification guards against two mislabelings. First, against extraction: the rent-concentration dynamics critics observe are properties of the game's mechanics, not of text authority for rankings — the standard is the measuring stick, not the extractor; folding the pedagogical dynamic into this story would fabricate victims the orthodoxy reading does not have. Second, against mandatrophy: the founding problem (cross-venue comparability) is live — rankings, titles, and championships still depend on the fixed text — so no piton reading is available and the constraint has not outlived its function. The live risk to this rope is scope creep: if text authority extends from ranked play into a claim that all play is defective without the text, the orthodoxy begins collecting cultural rents its coordination function never justified — the seam where the sibling readings live. Within its declared scope (ranking/comparison), the constraint is what it claims: coordination around a shared standard, with fixing cost prohibitive relative to a near-zero benefit of change (revising the text would break archive comparability to solve nothing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_delta,
    'This story instantiates the tournament_orthodoxy_reading of the monopoly_rulebook kernel only. What changes structurally under the sibling readings — extraction_demo (rulebook as rent-extraction pedagogy; adds a victim set of players eliminated by rent concentration and raises epsilon sharply) and social_scaffold (rulebook as socially unplayable without correction; demotes the text to a transitional support and makes casual play groups the beneficiaries)? Where is the disagreement located?',
    'Author each sibling as its own epsilon-invariant constraint story linked through the constraint family network. The disagreement localizes on the status of house rules (noise vs. correction vs. the demonstrative point) and on the text''s purpose (ranking vs. pedagogy vs. social play).',
    'Adopting a sibling reading changes the beneficiary/victim structure and epsilon substantially; the rope classification and near-floor extraction authored here hold only within the tournament-orthodoxy frame and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Committer-frame omega: one printed text, three readings, three distinct constraints.').

omega_variable(
    voluntariness_boundary_of_ranked_play,
    'Is participation in the canonical standard genuinely voluntary for every competitive participant, or do ranking-dependent rewards (titles, invitations, prize circuits) give some players constrained exit, narrowing the no-victim-set claim?',
    'Survey competitive participants on exit costs across tournament tiers; examine whether any tier carries livelihood-level or career-level dependence on rankings and sanctioning.',
    'If a ranking-dependent tier exists, that tier''s seat derives higher directionality and effective extraction rises above the near-floor value; the no-victim-set declaration would narrow to the genuinely casual tier of competitive play.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_boundary_of_ranked_play, empirical, 'Whether voluntary participation holds at every tier of competitive play.').

omega_variable(
    immutability_vs_interpretation_layer,
    'Is the text actually immutable in the operation that matters, or is authority already shared with the interpretation layer (official FAQ rulings, tournament supplements, errata such as the speed die)? If edge cases are settled by the interpreter rather than the printed text, is the operative constraint text-authority or interpreter-authority?',
    'Compare dispute resolutions across tournament eras: if outcomes turn on FAQ rulings rather than printed text, the interpreter is the operative authority and the printed text''s immutability is partly theatrical.',
    'If interpreter-authority dominates, theater_ratio rises, the lineage grounding weakens toward practice-authority, and the reference frame shifts from the text to the interpretive body — the rope likely survives but its justification changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_interpretation_layer, conceptual, 'Whether immutability is a property of the text or a division of labor between text and interpreter.').

omega_variable(
    orthodoxy_cultural_externality,
    'Does the orthodoxy''s cultural authority impose costs outside the competitive sphere — casual players internalizing that unmodified play is ''wrong'' — and if so, is that cost attributable to this constraint''s prestige or to the game''s mechanics (the social_scaffold sibling''s territory)?',
    'Player-culture studies of house-rule adoption and stated reasons, distinguishing players who never encountered tournament discourse from those who did.',
    'If the externality is real and attributable to the orthodoxy''s prestige, a diffuse cost-bearing population exists outside the declared scope and epsilon is understated; if attributable to the text''s harsh endgame mechanics, the cost belongs to the sibling story, not this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodoxy_cultural_externality, preference, 'Whether the orthodoxy''s cultural authority leaks costs beyond the competitive sphere.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(mono_tr_t0, observed).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement_basis(mono_tr_t10, observed).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(mono_tr_t20, observed).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement_basis(mono_tr_t30, observed).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(mono_tr_t40, observed).
narrative_ontology:measurement(mono_tr_t50, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement_basis(mono_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(mono_be_t0, observed).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement_basis(mono_be_t10, observed).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement_basis(mono_be_t20, observed).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement_basis(mono_be_t30, observed).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement_basis(mono_be_t40, observed).
narrative_ontology:measurement(mono_be_t50, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(mono_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(mono_su_t0, observed).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement_basis(mono_su_t10, observed).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement_basis(mono_su_t20, observed).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(mono_su_t30, observed).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement_basis(mono_su_t40, observed).
narrative_ontology:measurement(mono_su_t50, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(mono_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Monopoly rulebook' covers three structurally distinct claims with different epsilon values: text-authority-for-rankings (this story — rope, epsilon 0.08, no victim set, voluntary participation); rent-concentration pedagogy (extraction_demo reading — where the game's mechanics, not the standard, do the extracting, with eliminated players as victims); and house-rule social correction (social_scaffold reading — where the text's harsh endgame creates the need the house rules answer, with casual play groups as beneficiaries). Each is authored as its own story with its own beneficiary/victim structure per the epsilon-invariance principle; they are linked here as one constraint family because the same printed text is the referent of all three. The upstream reading with the most established empirical footing (textual invariance enabling observed ranked play) influences the downstream contested readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__tournament_orthodoxy_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
