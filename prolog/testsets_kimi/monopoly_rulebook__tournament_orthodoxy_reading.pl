% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Monopoly Tournament Rulebook Orthodoxy
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the tournament-orthodoxy reading of the
 *   monopoly_rulebook kernel: the rulebook is treated as an immutable,
 *   fixed-text authority that legitimates competitive ranking by ensuring
 *   strategic skill determines outcomes. House rules are rejected as noise
 *   that obscures competitive depth, and pedagogical or social-correction
 *   framings are explicitly foreclosed. The constraint is a coordination
 *   standard (rope) with negligible extraction, serving the competitive
 *   community through voluntary participation.
 *
 * KEY AGENTS:
 *   - ranked_competitors: Primary beneficiaries (moderate/mobile) â gain ranking legitimacy from a uniform standard
 *   - tournament_arbiters: Agenda-setters (institutional/constrained) â maintain and enforce text authority for competitive comparison
 *   - casual_house_rule_players: Excluded voices (moderate/mobile) â absent from governance, would advocate for house-rule legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Tournament Rulebook Orthodoxy").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, 'f6ceca28-1293-4597-9dee-73bd3c167411').
narrative_ontology:cs_kernel_codification('f6ceca28-1293-4597-9dee-73bd3c167411', fixed_text).
narrative_ontology:cs_authority_grounding('f6ceca28-1293-4597-9dee-73bd3c167411', lineage).
narrative_ontology:cs_interpretation_layer_present('f6ceca28-1293-4597-9dee-73bd3c167411').
narrative_ontology:cs_reading_relation('f6ceca28-1293-4597-9dee-73bd3c167411', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('f6ceca28-1293-4597-9dee-73bd3c167411', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('f6ceca28-1293-4597-9dee-73bd3c167411', foundational, immutable_text_authority_for_ranking).
narrative_ontology:cs_axiom_status(immutable_text_authority_for_ranking, holdable).
narrative_ontology:cs_axiom_grounding('f6ceca28-1293-4597-9dee-73bd3c167411', immutable_text_authority_for_ranking, conventional).
narrative_ontology:cs_axiom('f6ceca28-1293-4597-9dee-73bd3c167411', foundational, skill_meritocracy_over_determinism).
narrative_ontology:cs_axiom_status(skill_meritocracy_over_determinism, holdable).
narrative_ontology:cs_axiom_grounding('f6ceca28-1293-4597-9dee-73bd3c167411', skill_meritocracy_over_determinism, instrumental).
narrative_ontology:cs_reference_frame('f6ceca28-1293-4597-9dee-73bd3c167411', immutable_text_competitive_standard).
narrative_ontology:cs_drift_state('f6ceca28-1293-4597-9dee-73bd3c167411', contemporary_popular_play, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f6ceca28-1293-4597-9dee-73bd3c167411', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, ranked_competitors).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, tournament_arbiters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily enter sanctioned tournaments to have strategic skill measured against a fixed, published ruleset; benefit from cross-event comparability and ranking legitimacy; can exit to casual or house-rule play but forfeit official standing.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, ranked_competitors, beneficiary,
    moderate, biographical, mobile, national).

% Administer official tournament play under an immutable text; enforce rule uniformity for ranking and comparison; adjudicate edge cases without altering the published rulebook; reject house rules as illegitimate for competitive results.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_arbiters, agenda_setter,
    institutional, generational, constrained, national).

% Play primarily for social enjoyment using widespread house-rule variants; their preferences are absent from tournament governance; they would argue that strict official rules produce an overly harsh, antisocial endgame but are not present in competitive legitimacy discourse.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_house_rule_players, excluded,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, fixed ruleset enabling cross-tournament comparison of strategic skill and legitimate ranking of players across time and geography.
% TRANSFER_FUNCTION: Moves status and ranking legitimacy from unsanctioned or house-rule play to sanctioned tournament participants; no concentrated monetary extraction at the constraint level.
% ABSENT_VOICES: Casual and family players who prefer house-rule variants for social enjoyment; they are structurally excluded from tournament governance and would contest the claim that house rules are mere noise.
% DISAPPEARANCE_RATIONALE: Without the immutable text authority, tournament results become incomparable, competitive rankings dissolve into local house-rule variants, and the coordination function of a shared standard collapses.
% FOUNDING_PROBLEM: The need for a common, comparable standard to determine strategic skill in a complex board game across different events, eras, and player populations.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and ranked competitors attest the need for a fixed standard; game theorists and institutional design scholars outside the benefiting competitive community corroborate that coordination around shared rules is necessary for valid competitive measurement.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.05, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint coordinates around a shared standard without asymmetric rent transfer; suppression is near-zero (0.05) because participation is fully voluntary; theater_ratio is near-zero (0.05) because enforcement is functional rather than performative. Accessibility_collapse is moderate (0.45): house rules remain widely accessible for casual play, but they collapse as viable alternatives once a player seeks ranking legitimacy. Resistance is negligible (0.05) because no party is coerced. Temporal measurements show stable, flat profiles consistent with a durable rope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats converge on a low-directionality, low-extraction experience because both are net beneficiaries of coordination. The excluded seat (casual house-rule players) experiences the constraint as an arbitrary wall that delegitimizes their preferred mode of play, but because they are not trapped or coerced, their structural directionality remains moderate rather than target-like.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ranked_competitors and tournament_arbiters) both derive value from the shared standard: competitors gain comparability, arbiters gain a clear mandate. Neither extracts from the other. No victim set is declared because the constraint is opt-in and leaves non-participants unharmed. The excluded casual players are not victims in the structural sense because they retain full exit mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy because its founding problem â the need for a common standard to compare strategic skill â remains live. It is not a scaffold because it carries no sunset clause and is not transitional; it is not a piton because it is not atrophied or maintained theatrically. The classification as rope is grounded in the absence of extraction and the presence of genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contextual_stability,
    'Does the tournament orthodoxy reading remain stable when the same kernel is used in non-competitive social contexts, or does it fracture into the social-scaffold reading under casual-play conditions?',
    'Comparative ethnography of rule discourse across tournament, club, and family play contexts; tracking whether the same agents switch readings situationally.',
    'If the reading is context-bound rather than kernel-bound, the constraint''s scope narrows to tournament-specific coordination, potentially reducing its claimed universality and altering network coupling estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contextual_stability, conceptual, 'Whether tournament orthodoxy is a stable kernel reading or a context-dependent frame').

omega_variable(
    house_rule_prevalence_gap,
    'How prevalent are house rules among serious or formerly-ranked players, and does this prevalence undermine the claim that text authority is effectively immutable for ranking?',
    'Survey and observational study of rule adherence in ranked play, semi-competitive clubs, and online platforms measuring actual rule variance.',
    'If house rules are widely practiced even among serious players, the accessibility_collapse metric may be overstated and the rope classification may need to account for a weaker coordination floor than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rule_prevalence_gap, empirical, 'Empirical gap between claimed text immutability and actual rule variance in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 16, 0.06).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 24, 0.06).
narrative_ontology:measurement(mono_tr_t32, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 8, 0.05).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 16, 0.06).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(mono_be_t32, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 32, 0.05).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__tournament_orthodoxy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the monopoly_rulebook kernel, decomposed per the Îµ-invariance principle because the three sibling readings instantiate structurally distinct constraints with different epsilon profiles, beneficiary/victim structures, and coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
