% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-09
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly Social Scaffold Reading
 *   domain: game_theory/social_coordination
 *
 * SUMMARY:
 *   This constraint story instantiates the social_scaffold_reading of the
 *   monopoly_rulebook kernel. The standing arrangement is the widespread
 *   casual practice of overlaying house rules onto the Monopoly rulebook to
 *   prolong play, inject liquidity, and prevent elimination, thereby
 *   converting a competitive elimination game into a social coordination
 *   device. Sibling readings include the extraction_demo_reading (Marxist
 *   pedagogical frame) and the tournament_orthodoxy_reading (immutable
 *   competitive framework). This reading treats the house-rule layer as a
 *   temporary scaffold whose legitimacy derives from the social occasion
 *   rather than the text.
 *
 * KEY AGENTS:
 *   - social_play_group (beneficiary, moderate/constrained): the assembled players who gain extended social interaction
 *   - competitive_players (payer, moderate/constrained): players who prefer orthodox rules and bear the cost of prolonged suboptimal play
 *   - host_player (agenda_setter, moderate/mobile): proposes and administers house rules for the session
 *   - tournament_orthodoxy (excluded, organized/analytical): competitive rulebook authorities not present at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.45).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.38).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Social Scaffold Reading").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '5bc4bc36-190b-4297-9881-7887331fa43e').
narrative_ontology:cs_kernel_codification('5bc4bc36-190b-4297-9881-7887331fa43e', fixed_text).
narrative_ontology:cs_authority_grounding('5bc4bc36-190b-4297-9881-7887331fa43e', practice).
narrative_ontology:cs_interpretation_layer_present('5bc4bc36-190b-4297-9881-7887331fa43e').
narrative_ontology:cs_reading_relation('5bc4bc36-190b-4297-9881-7887331fa43e', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bc4bc36-190b-4297-9881-7887331fa43e', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('5bc4bc36-190b-4297-9881-7887331fa43e', foundational, house_rules_instrumentally_necessary).
narrative_ontology:cs_axiom_status(house_rules_instrumentally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5bc4bc36-190b-4297-9881-7887331fa43e', house_rules_instrumentally_necessary, instrumental).
narrative_ontology:cs_axiom('5bc4bc36-190b-4297-9881-7887331fa43e', foundational, social_cohesion_overrides_text_fidelity).
narrative_ontology:cs_axiom_status(social_cohesion_overrides_text_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('5bc4bc36-190b-4297-9881-7887331fa43e', social_cohesion_overrides_text_fidelity, conventional).
narrative_ontology:cs_reference_frame('5bc4bc36-190b-4297-9881-7887331fa43e', communal_play_tradition).
narrative_ontology:cs_drift_state('5bc4bc36-190b-4297-9881-7887331fa43e', official_rulebook_authority, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5bc4bc36-190b-4297-9881-7887331fa43e', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_play_group).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, competitive_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proposes and enforces session-specific house rules before and during play; can end the game or change the activity but is socially expected to sustain group engagement for the evening.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, host_player, agenda_setter,
    moderate, immediate, mobile, local).

% The assembled players who benefit from extended inclusion and interaction; they informally enforce house rules by mutual agreement and social pressure to keep eliminated or near-eliminated players in the game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_play_group, beneficiary,
    moderate, immediate, constrained, local).

% Players who prefer strategic, by-the-book play; their early advantages are eroded by house-rule redistribution and their objections are overridden by majority preference, costing them time and competitive satisfaction.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_players, payer,
    moderate, immediate, constrained, local).

% Competitive tournament communities and rulebook purists not present at the casual table; they maintain official rankings and would insist on strict text fidelity, including auctions and rapid elimination.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_orthodoxy, excluded,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a mixed-skill social group engaged in a single board game for an evening by preventing early player elimination and maintaining liquidity, converting a competitive elimination design into a social coordination device.
% TRANSFER_FUNCTION: Moves time, competitive advantage, and game resources from players who would win quickly under orthodox rules to the group as a whole via house-rule mechanisms such as Free Parking jackpots, skipped auctions, and lenient trading, prolonging the session.
% ABSENT_VOICES: Tournament orthodoxy players and rulebook purists are absent; they would insist on strict adherence including auctions and rapid elimination, but they are not in the room when casual house rules are adopted.
% DISAPPEARANCE_RATIONALE: If house-rule corrections vanished and strict rulebook enforcement replaced them, competitive players would eliminate casual players earlier, the social event would fragment, and the group would likely abandon the game or switch activities.
% FOUNDING_PROBLEM: The Monopoly rulebook as written produces rapid wealth concentration and player elimination that ends the game too quickly for a mixed-skill social group, undermining the evening's social function.
% FOUNDING_PROBLEM_CORROBORATION: Game designers and sociologists of play have documented that Monopoly's official rules are widely unknown or ignored in casual settings because they truncate social interaction; corroboration comes from outside the benefiting party (e.g., ethnographic studies of family game nights), while the benefiting social group merely experiences the adjustment as making the game more fun.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because the house rules extract competitive advantage and time from skilled players to redistribute to the group, but the extraction is bounded by the social occasion's duration and the group's mutual interest. Suppression is moderate-low (0.38): the official rules are known but socially overridden through peer consensus rather than formal coercion. Theater ratio is low-moderate (0.25) because the house rules perform a genuine coordination function, though some ritualistic repetition occurs. Accessibility collapse is moderate (0.45): alternatives (playing by the book, stopping early) exist but carry social cost. Resistance is low-moderate (0.30): competitive players may grumble but rarely escalate. The claim is scaffold because the arrangement is temporary (session-bound, has_sunset_clause: true) and justified by the social transition, not a steady-state competitive order.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (social_play_group) experiences the constraint as enabling coordination and inclusion; the payer seat (competitive_players) experiences the same rules as devaluing their skill and extending a game they would prefer to end. The host sits near the beneficiary end because they share the social goal, while the tournament orthodoxy seat, if computed analytically, would read the constraint as severe authority erosion. The engine captures this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The social_play_group is declared beneficiary, yielding low directionality; their constrained exit (social belonging) does not shift them toward target because the structural relationship is subsidizing. Competitive_players are declared victim/payer, yielding high directionality; their constrained exit amplifies effective extraction because they cannot easily leave without social rupture. The host_player, as agenda_setter with mobile exit, derives moderate directionality: they administer the constraint but could terminate it, which limits their computed extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview, this constraint could be misread as a rope (pure coordination) because everyone appears to agree at the table, or as a snare (pure extraction) from the competitive player's viewpoint. The scaffold classification is supported by the live founding problem (rapid elimination under official rules), the active enforcement requirement (house rules must be remembered and enforced session-by-session), and the sunset clause (the arrangement expires when the social occasion ends). The theater ratio remains below the piton threshold, indicating the function is not merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_extraction_vs_coordination,
    'Does the social scaffold reading describe a genuine coordination mechanism, or does it rationalize extraction from competitive players by framing it as social cohesion?',
    'Examine whether competitive players experience net negative utility across repeated sessions; if they consistently endure the game for social belonging, extraction is present.',
    'If rationalization, effective extractiveness is higher than the coordination framing suggests, and the computed seat type for competitive players shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_extraction_vs_coordination, conceptual, 'Whether the scaffold is coordination or extraction rationalized as cohesion').

omega_variable(
    house_rule_origin_ambiguity,
    'Are house rules spontaneously generated by each social play group, or are they transmitted culturally as a stable tradition?',
    'Cross-group comparison of house rule variants; high similarity suggests cultural transmission, high variation suggests spontaneous generation.',
    'If transmitted, the constraint has lineage authority and may compute as commitment-system extraction; if spontaneous, it is distributed practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rule_origin_ambiguity, empirical, 'Origin of house rules as spontaneous or transmitted tradition').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of orthodox rule objections structural (social exclusion for insisting) or internalized (players genuinely prefer house rules)?',
    'Observe competitive players in isolation: do they revert to orthodox rules when removed from the social group?',
    'If internalized, suppression is higher than structural measures suggest; if purely structural, competitive players carry latent resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of orthodox play preferences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_scaffold_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t1, monopoly_rulebook__social_scaffold_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t3, monopoly_rulebook__social_scaffold_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t5, monopoly_rulebook__social_scaffold_reading, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(monopoly_social_scaffold_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(monopoly_social_scaffold_be_t1, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 1, 0.35).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(monopoly_social_scaffold_be_t3, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(monopoly_social_scaffold_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(monopoly_social_scaffold_be_t5, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 5, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__social_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the monopoly_rulebook kernel, decomposed from the colloquial 'Monopoly rules' label into structurally distinct commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
