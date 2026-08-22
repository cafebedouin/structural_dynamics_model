% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Official Rulebook as Standardized Competitive Framework (Tournament Orthodoxy Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This story authors the tournament-orthodoxy reading of the contested
 *   Monopoly rulebook kernel: the published rules are the legitimate
 *   competitive framework, house rules are noise that degrades comparability
 *   rather than corrective wisdom, and text authority is treated as fixed for
 *   the purpose of ranking and cross-event comparison. Under this reading the
 *   rulebook is a low-extraction coordination device — a shared standard,
 *   like a measurement convention, that lets voluntary competitors compare
 *   strategic performance meaningfully. This is a distinct constraint from
 *   the extraction_demo_reading (which reads the same text as inevitable
 *   wealth-concentration pedagogy) and the social_scaffold_reading (which
 *   reads it as requiring community correction to remain socially playable) —
 *   each reading has its own ε, beneficiary structure, and type; they are
 *   linked here only through the shared kernel_id.
 *
 * KEY AGENTS:
 *   - tournament_competitive_community: beneficiary/agenda_setter (organized/mobile) — maintains and benefits from the shared standard
 *   - certified_tournament_directors: agenda_setter (moderate/mobile) — administers the text as adjudicative authority
 *   - aspiring_ranked_players: beneficiary (moderate/mobile) — gains a transferable competitive credential
 *   - casual_house_rule_players: excluded (moderate/mobile) — outside the ranked-comparison system by choice, not by suppression
 *   - rules_committee_stewards: observer (institutional/analytical) — custodians of the fixed text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.06).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.18).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Official Rulebook as Standardized Competitive Framework (Tournament Orthodoxy Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '3419a895-71b7-419f-ae20-5c0b5f0f5f9f').
narrative_ontology:cs_kernel_codification('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', fixed_text).
narrative_ontology:cs_authority_grounding('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', practice).
narrative_ontology:cs_interpretation_layer_present('3419a895-71b7-419f-ae20-5c0b5f0f5f9f').
narrative_ontology:cs_reading_relation('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', foundational, text_immutability_for_ranking_purposes).
narrative_ontology:cs_axiom_status(text_immutability_for_ranking_purposes, holdable).
narrative_ontology:cs_axiom_grounding('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', text_immutability_for_ranking_purposes, conventional).
narrative_ontology:cs_axiom('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', foundational, strategic_skill_determinism_thesis).
narrative_ontology:cs_axiom_status(strategic_skill_determinism_thesis, holdable).
narrative_ontology:cs_axiom_grounding('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', strategic_skill_determinism_thesis, empirically_contingent).
narrative_ontology:cs_axiom('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', secondary, house_rules_as_noncomparable_noise).
narrative_ontology:cs_axiom_status(house_rules_as_noncomparable_noise, holdable).
narrative_ontology:cs_axiom_grounding('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', house_rules_as_noncomparable_noise, conventional).
narrative_ontology:cs_reference_frame('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', official_published_rules_as_competitive_canon).
narrative_ontology:cs_drift_state('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', contemporary_organized_tournament_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3419a895-71b7-419f-ae20-5c0b5f0f5f9f', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_competitive_community).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, certified_tournament_directors).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, aspiring_ranked_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, text_authority_immutability_for_ranking).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, strategic_skill_determinism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A voluntary association of players who opt into ranked, sanctioned play precisely because the fixed rule text lets results be compared across events, years, and opponents. They maintain the standard through published rulings and official tournament structures rather than coercion; anyone uninterested in comparability can play casually elsewhere at zero cost.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_competitive_community, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, tournament_competitive_community, agenda_setter).

% Administer sanctioned events strictly by the published rules, adjudicating disputes by citing rule text rather than local custom. They gain standing and credibility from enforcing a stable, portable standard; they could run house-rule events instead but would lose sanctioning status and the community that recognizes it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, certified_tournament_directors, agenda_setter,
    moderate, biographical, mobile, national).

% Study the fixed rule set specifically so their skill development and tournament results mean something comparable to other players' results elsewhere. The immutable text is what makes their practice legible as a transferable competitive credential. Nothing traps them in ranked play; casual and house-rule formats remain freely available and are not disparaged as illegitimate, only as non-comparable for ranking purposes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, aspiring_ranked_players, beneficiary,
    moderate, biographical, mobile, national).

% Play with free parking jackpots, slowed elimination, and other informal modifications for social enjoyment. From the tournament-orthodoxy reading, their variants are simply outside the ranked-comparison system, not wrong — but this reading does not seek their voice on rule content since they are not participating in the comparability project the rulebook exists to serve.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, immediate, mobile, local).

% Maintain the official rule text through periodic clarification and errata, treating the published rules as the stable reference against which all tournament rulings are checked. They see their role as custodial — preserving a fixed standard, not authoring social policy or economic pedagogy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rules_committee_stewards, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable rule text so that competitive results (rankings, records, tournament outcomes) are comparable across players, events, and time — solving the coordination problem of what 'the same game' means when strategic comparison is the point.
% TRANSFER_FUNCTION: Moves very little of value between parties; what it 'transfers' is legibility — comparability of competitive achievement — from an ungoverned space of idiosyncratic house variants into a shared, portable credential system that any willing entrant can access.
% ABSENT_VOICES: Casual and house-rule players are not represented in rule-text maintenance, but this reading holds their absence as appropriate rather than a defect: they are not participating in the ranked-comparison project the text exists to serve, and nothing bars them from organizing their own competitive structure if they wanted comparability on their own terms.
% DISAPPEARANCE_RATIONALE: If the fixed rule text vanished, sanctioned tournament play would fragment into incompatible local variants overnight; results would stop being comparable, rankings would lose meaning, and the entire competitive infrastructure (directors, sanctioning bodies, ranked ladders) built on a shared standard would have no stable referent to administer.
% FOUNDING_PROBLEM: Early informal play produced incompatible, drifting local variants that made it impossible to say who was actually the better player across different tables, clubs, or regions — there was no shared basis for competitive comparison.
% FOUNDING_PROBLEM_CORROBORATION: Tournament directors and sanctioning bodies outside the immediate beneficiary group (e.g., independent regional clubs that voluntarily adopt the official rules solely to gain access to cross-club ranking systems) attest that the comparability problem remains active: absent a fixed text, their own informal experiments with local rule drift produced exactly the incomparability the standard was built to prevent.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored very low (0.06) because no party pays a structural cost through this reading's operation — participation is voluntary, exit is costless (casual play remains freely available), and the text confers a genuine comparability good rather than extracting rent. Suppression is modest (0.18): the rulebook does discourage house-rule deviation for ranking purposes (a real, if mild, form of gatekeeping against alternative local standards being treated as equally rankable), but it does not suppress casual play itself, only its comparability claims. Theater ratio is low (0.10) — enforcement of the text is functional (adjudicating disputes, maintaining a stable reference) rather than performative. Accessibility collapse (0.35) is moderate-low: once a player understands the rules exist to enable ranking, deviating from them for ranked purposes becomes clearly non-viable, but casual alternatives remain fully accessible and undiminished — collapse applies only within the ranked-comparison frame, not globally.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (directors, stewards) the text is neutral infrastructure they administer without personal stake in outcomes. From the beneficiary seat (players) the text is a credentialing device they use instrumentally to make their skill legible. From the excluded seat (casual players) the text is simply inapplicable to their preferred mode of play — not oppressive, just irrelevant to a game they are not trying to play. No seat in this reading experiences the text as extractive; the divergence that WOULD appear (players experiencing the rules as harsh or extractive) belongs structurally to the sibling readings, not to this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (the competitive community, directors, and aspiring ranked players) sit near the subsidized end of directionality: they get a genuine comparability good relative to what they'd have without a fixed standard, and none are trapped — exit to casual play is unconstrained. There is no victim set in this reading because participation in ranked play is voluntary and the text imposes no cost on those who decline it; casual players are excluded from rule-authorship but not extracted from. This is the structural basis for the rope classification and for the absence of a `victims` array.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incomparable, drifting local variants undermining competitive ranking) remains live and is corroborated by independent regional clubs outside the direct beneficiary set, so this reading resists a mandatrophy read: the rulebook's continued authority is not a stale mandate persisting past its function but an active answer to a standing coordination problem. This is precisely the discipline the ε-invariance principle demands: rather than force one story to average across the pedagogy reading, the correction reading, and this orthodoxy reading, each gets its own clean classification, and only this one's mandate is being evaluated here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparability_versus_gatekeeping,
    'Is the tournament-orthodoxy insistence on immutable text authority purely a comparability mechanism, or does it also function as a status gate that excludes house-rule players from legitimacy claims they might otherwise reasonably make?',
    'Survey excluded house-rule communities on whether they perceive orthodoxy enforcement as merely non-inclusive of their preferences or as an active delegitimization of their competitive practice; compare rhetoric used by tournament bodies about house-rule play.',
    'If the exclusion carries active delegitimization rhetoric beyond comparability necessity, suppression should be revised upward and the reading''s rope classification would need re-examination for tangled-rope drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparability_versus_gatekeeping, empirical, 'Whether text-authority enforcement is pure coordination or also carries a status-suppression function against non-orthodox play.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the monopoly_rulebook kernel (orthodoxy, extraction-demo, social-scaffold) genuinely about the same kernel object, or does ''the rulebook'' mean something different enough in each community''s practice that they are not actually contesting the same text?',
    'Compare whether adherents of each reading would recognize the others'' description of ''the rulebook''s purpose'' as a description of the same artifact they engage with, or as a foreign object.',
    'If the readings are not commensurable, the kernel_id linkage is a convenience label rather than a structural fact, and the sibling relationships (coexists_with) should be understood as describing parallel practices rather than genuine contest over one shared commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three sibling readings genuinely contest one kernel or merely share a label.').

omega_variable(
    voluntariness_under_social_pressure,
    'Is participation in ranked orthodox play truly costless to exit, or does social pressure within gaming communities (reputation, peer status) make casual defection more costly than the ''mobile'' exit_options rating suggests?',
    'Interview players who left ranked play for casual formats about social costs experienced (loss of standing, peer disapproval) versus purely voluntary preference shifts.',
    'If meaningful social cost attaches to exit, the beneficiary directionality softens slightly toward the target end and the very-low extractiveness score would need modest upward revision, though likely still within rope range.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntariness_under_social_pressure, empirical, 'Whether stated ''mobile'' exit options for ranked players understate real social exit costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(mono_tr_t32, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 8, 0.05).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 16, 0.06).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 24, 0.06).
narrative_ontology:measurement(mono_be_t32, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.06).

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
% This story is one of three sibling constraints decomposing the natural-language concept 'the Monopoly rulebook' per the ε-invariance principle: the same text, read three structurally distinct ways by three different communities of practice, yields three different ε values, beneficiary/victim structures, and classifications. tournament_orthodoxy_reading (this file, rope, ε≈0.06, no victims) coexists with extraction_demo_reading (pedagogical elimination framing, likely much higher ε) and social_scaffold_reading (community-correction framing, likely tangled_rope or scaffold with distinct victim/beneficiary sets). None of the three is more 'correct' about the underlying kernel; each is a clean, internally consistent constraint linked by shared kernel_id rather than shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
