% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Monopoly Tournament Orthodoxy Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the tournament orthodoxy reading of the
 *   Monopoly rulebook: the printed rules are the legitimate competitive
 *   framework; strategic skill determines outcomes; house rules are noise
 *   that obscures competitive depth; text authority is immutable for ranking
 *   and comparison purposes. The reading instantiates a rope constraint — a
 *   pure coordination mechanism around a shared standard with very low
 *   extraction (epsilon ≤ 0.10), voluntary participation, and no victim set.
 *   It explicitly rejects both the extraction_demo_reading's pedagogical
 *   frame (Monopoly as capitalism critique) and the social_scaffold_reading's
 *   correction frame (house rules as necessary social lubrication).
 *
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
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Tournament Orthodoxy Reading").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, 'fcb669be-8181-44bd-b0e2-86188b43acd8').
narrative_ontology:cs_kernel_codification('fcb669be-8181-44bd-b0e2-86188b43acd8', fixed_text).
narrative_ontology:cs_authority_grounding('fcb669be-8181-44bd-b0e2-86188b43acd8', expertise).
narrative_ontology:cs_interpretation_layer_present('fcb669be-8181-44bd-b0e2-86188b43acd8').
narrative_ontology:cs_reading_relation('fcb669be-8181-44bd-b0e2-86188b43acd8', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcb669be-8181-44bd-b0e2-86188b43acd8', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('fcb669be-8181-44bd-b0e2-86188b43acd8', foundational, text_immutability_for_ranking).
narrative_ontology:cs_axiom_status(text_immutability_for_ranking, holdable).
narrative_ontology:cs_axiom_grounding('fcb669be-8181-44bd-b0e2-86188b43acd8', text_immutability_for_ranking, conventional).
narrative_ontology:cs_axiom('fcb669be-8181-44bd-b0e2-86188b43acd8', secondary, house_rules_are_noise).
narrative_ontology:cs_axiom_status(house_rules_are_noise, holdable).
narrative_ontology:cs_axiom_grounding('fcb669be-8181-44bd-b0e2-86188b43acd8', house_rules_are_noise, conventional).
narrative_ontology:cs_reference_frame('fcb669be-8181-44bd-b0e2-86188b43acd8', tournament_standard_text).
narrative_ontology:cs_drift_state('fcb669be-8181-44bd-b0e2-86188b43acd8', contemporary_tournament_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fcb669be-8181-44bd-b0e2-86188b43acd8', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, ranking_systems).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, standardized_competitive_framework).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, skill_based_outcome_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily enter tournaments knowing the exact rule set; the immutable text lets them invest in strategic mastery that transfers across events. Exit is easy — they can stop competing — but the coordination value of a shared standard makes the constraint beneficial.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_players, beneficiary,
    organized, biographical, mobile, global).

% Administer events using the published rulebook as the sole authority; they benefit from reduced dispute resolution overhead and legitimacy derived from adherence to the canonical text. They could adopt house rules but would lose cross-event comparability and sanctioning body recognition.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, beneficiary).

% Maintain Elo-style or points-based rankings across tournaments; the immutable rulebook is the substrate that makes scores commensurable. Without text fixity, rankings would conflate skill with rule-variant luck.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, ranking_systems, beneficiary,
    organized, generational, mobile, global).

% Play Monopoly with liquidity injections, free parking jackpots, and other house rules that prevent elimination and extend social play. They are not participants in the tournament ecosystem and would reject text immutability as the only legitimate frame, but their play occurs in a separate social space.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, immediate, arbitrage, local).

% Study the rulebook as a designed artifact; they evaluate whether the text's elimination mechanic serves competitive depth or merely reflects 1930s design assumptions. They neither compete nor organize.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_design_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative rule set enabling fair comparison of strategic skill across players, tables, and tournaments without rule-variance contamination.
% TRANSFER_FUNCTION: Moves nothing materially; coordinates shared understanding so that outcomes reflect skill variance rather than rule variance.
% ABSENT_VOICES: Casual and house-rule players who prefer social play over competitive purity; they are not in tournament spaces but would object to text immutability as the only legitimate frame.
% DISAPPEARANCE_RATIONALE: If the immutable rulebook vanished overnight, tournament results could not be compared across events, ranking systems would lose their commensurability substrate, and competitive Monopoly would fragment into incompatible rule variants — the tournament ecosystem would reorganize around a new coordination point or dissolve.
% FOUNDING_PROBLEM: Need for a stable, authoritative rule set that allows competitive skill to be measured and compared across time and space without rule-variance contamination.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers (Hasbro-sanctioned event runners, world championship structures), ranking bodies, and competitive players outside the rulebook's authorship attest the problem persists — competitive play still requires a fixed text for cross-event legitimacy.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.08) because participation is voluntary, no material transfer occurs, and the constraint's only 'cost' is the opportunity cost of learning the standard — which is the price of admission for any coordination mechanism. Suppression is low (0.15) because the constraint operates through voluntary adoption and sanctioning-body recognition, not coercion; players who dislike the text simply don't enter tournaments. Theater ratio is low (0.10) — the rulebook's competitive function is genuine and its enforcement (tournament judges applying the text) is functional, not performative. Accessibility collapse is moderate (0.40) because alternative rule sets exist and are played socially, but they cannot serve the tournament coordination function. Resistance is low (0.15) — the constraint faces no organized opposition because it makes no claim on non-participants.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute rope from every seat because the structural data shows voluntary adoption, zero extraction, and pure coordination. The extraction_demo_reading and social_scaffold_reading would compute different types from their own structural premises (snare/tangled_rope and scaffold respectively), but those are separate constraint stories with different beneficiary/victim declarations — not perspectival variants of this one.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared stakeholders are beneficiaries or agenda_setters who benefit from the coordination function. Competitive players gain commensurable skill measurement; organizers gain dispute-reduction and legitimacy; ranking systems gain a stable substrate. No stakeholder bears net extraction — the constraint is a pure coordination good. Casual players are excluded (not victims) because they operate in a separate social space with different success criteria; they are not harmed by the tournament standard's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-event skill comparability) remains live; the arrangement has not outlived its function. Mandatrophy is not resolved because the constraint continues to serve its coordination purpose without accumulating extractive layers. The slight theater_ratio rise (0.05→0.10) reflects marginal bureaucratization of tournament judging, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the tournament orthodoxy reading a distinct constraint with its own epsilon, or merely a perspectival slice of a single monopoly_rulebook constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint via tournament play yields epsilon ≈ 0.08 while measuring via extraction demo yields epsilon ≈ 0.7+, they are different constraints. The structural data (beneficiaries, victims, enforcement) diverges fundamentally across readings.',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the framework must model observable-dependent epsilon — which violates the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate constraints per epsilon-invariance.').

omega_variable(
    coordination_purity_boundary,
    'Does the tournament ecosystem''s sanctioning-body revenue (entry fees, licensing) constitute extraction that this reading''s epsilon fails to capture?',
    'Audit whether tournament organizers or sanctioning bodies collect rents above the marginal cost of administration, and whether those rents are structurally necessary for the coordination function or contingent on the rulebook''s proprietary status.',
    'If sanctioning rents exist and are necessary, epsilon should be higher and the constraint may be tangled_rope. If rents are separable (e.g., open-source rulebook with competing tournament operators), the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_purity_boundary, empirical, 'Whether proprietary control of the rulebook text introduces extraction invisible to the pure coordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t1935, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1935, 0.05).
narrative_ontology:measurement(mono_tr_t1975, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(mono_tr_t2000, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(mono_tr_t2025, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t1935, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1935, 0.08).
narrative_ontology:measurement(mono_be_t1975, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 1975, 0.07).
narrative_ontology:measurement(mono_be_t2000, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(mono_be_t2025, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 2025, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t1935, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1935, 0.1).
narrative_ontology:measurement(mono_su_t1975, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 1975, 0.12).
narrative_ontology:measurement(mono_su_t2000, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(mono_su_t2025, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the monopoly_rulebook kernel. The tournament orthodoxy reading claims rope (pure coordination, epsilon ≈ 0.08); extraction_demo_reading claims snare/tangled_rope (high extraction, pedagogical frame); social_scaffold_reading claims scaffold (transitional, house rules as correction). They share the same physical text but instantiate different constraints because their epsilon referents (the standing arrangements they assess) differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
