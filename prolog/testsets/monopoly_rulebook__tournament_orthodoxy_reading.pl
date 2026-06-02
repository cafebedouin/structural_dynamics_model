% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook as Tournament Orthodoxy (Rope Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   The Monopoly rulebook is a contested kernel of meaning in tabletop game
 *   culture. This constraint story instantiates the tournament_orthodoxy
 *   reading: the rulebook is the legitimate competitive framework where
 *   strategic skill determines outcomes. House rules are treated as noise
 *   that obscures competitive depth. Textual authority is immutable for
 *   ranking and comparison purposes. This reading denies that the rulebook
 *   extracts value or imposes control—it coordinates voluntary participation
 *   in a shared game. From this view, the rulebook is a Rope constraint: pure
 *   coordination with minimal overhead. The sibling readings
 *   (extraction_demo_reading, social_scaffold_reading) are distinct
 *   constraints with different epsilon values and different structural
 *   justifications. They coexist in game culture as live positions held by
 *   different players and communities.
 *
 * KEY AGENTS:
 *   - Competitive Game Community: Primary beneficiary (organized/mobile) — derives mutual benefit from shared rulebook; benefits from skill differentiation and fair comparison
 *   - Rules Authority (Parker Brothers, USPA): Secondary beneficiary (powerful/arbitrage) — maintains textual authority and adjudicates edge cases; low-cost coordination provider
 *   - Casual Player Community: Tertiary beneficiary (moderate/constrained) — benefits from low-cost coordination when playing with strangers; voluntary participation
 *   - Analytical Observer: Neutral observer (analytical/analytical) — sees rulebook as pure coordination mechanism enabling voluntary alignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament Orthodoxy (Rope Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, 'mrto-20260226-001').
narrative_ontology:cs_kernel_codification('mrto-20260226-001', fixed_text).
narrative_ontology:cs_authority_grounding('mrto-20260226-001', practice).
narrative_ontology:cs_interpretation_layer_present('mrto-20260226-001').
narrative_ontology:cs_reading_relation('mrto-20260226-001', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('mrto-20260226-001', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('mrto-20260226-001', foundational, textual_immutability_coordinates_skill).
narrative_ontology:cs_axiom_status(textual_immutability_coordinates_skill, holdable).
narrative_ontology:cs_axiom_grounding('mrto-20260226-001', textual_immutability_coordinates_skill, instrumental).
narrative_ontology:cs_axiom('mrto-20260226-001', secondary, house_rules_are_coordination_noise).
narrative_ontology:cs_axiom_status(house_rules_are_coordination_noise, holdable).
narrative_ontology:cs_axiom_grounding('mrto-20260226-001', house_rules_are_coordination_noise, instrumental).
narrative_ontology:cs_reference_frame('mrto-20260226-001', formalized_rulebook_authority).
narrative_ontology:cs_drift_state('mrto-20260226-001', contemporary_casual_variant_proliferation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('mrto-20260226-001', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_game_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPETITIVE GAME COMMUNITY (ROPE) — Organized agents (tournament organizers, competitive players, rules adjudicators) benefit from a shared, immutable rulebook. The rulebook solves a collective action problem: without textual authority, every game becomes a negotiation, and competitive depth (the skill differentiation that makes the game worth playing) dissolves. The rulebook provides zero-extraction coordination—it enables fair comparison across contexts and time periods. Agents are mobile; they can choose other games or host variants, but the shared text creates mutual benefit.
constraint_indexing:constraint_classification(monopoly_rulebook__tournament_orthodoxy_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: RULES AUTHORITY (ROPE) — Publishers (Parker Brothers, successor entities) and tournament governance bodies (USPA, equivalent orgs) derive legitimacy from maintaining textual authority. They benefit from rules stability, but the coordination function is genuine: they provide the costly service of adjudicating edge cases, publishing errata, and maintaining version control. Low extractiveness; the authority has arbitrage options (could stop publishing, could introduce rule variants) but maintains the standard because coordination is the primary function.
constraint_indexing:constraint_classification(monopoly_rulebook__tournament_orthodoxy_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CASUAL PLAYER COMMUNITY (ROPE) — Agents who play informally derive low-cost coordination from the rulebook. They can choose to play or not (mobile), but when they play with strangers or across households, the shared text eliminates transaction costs. No significant extraction—the rulebook enables their play rather than constraining it. Suppression is minimal because exit is costless.
constraint_indexing:constraint_classification(monopoly_rulebook__tournament_orthodoxy_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal analytical context, the rulebook is a pure coordination mechanism. It solves the alignment problem: 'What game are we playing?' By fixing the rules at the textual level, it enables skill differentiation, strategic depth, and fair comparison. Zero-extraction view—the rulebook is not extracting value from players; it is enabling voluntary coordination among them.
constraint_indexing:constraint_classification(monopoly_rulebook__tournament_orthodoxy_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.08): Minimal. The rulebook provides pure coordination—it solves the alignment problem of 'what game are we playing?' without extracting value from players. No agent bears costs that exceed their benefits. The rules authority provides the service of maintaining textual authority and adjudicating edge cases, but this is a genuine coordination function, not extractive overhead. Suppression (0.05): Negligible. Agents can exit costlessly (choose not to play, host variants) or remain for genuine mutual benefit. No mechanism prevents alternatives or suppresses knowledge. Theater ratio (0.15): Low. Rules enforcement is functional rather than performative—the rulebook drives game-theoretic outcomes, not ritual compliance. This reading treats the rules as substrate for strategic competition, not as a social performance.
 *
 * PERSPECTIVAL GAP:
 *   The tournament_orthodoxy reading produces a uniform classification (Rope across all perspectives) because the constraint is purely coordinative. Unlike the verification_bottleneck exemplar (which produces all six types), this constraint has no structural asymmetry that generates different classifications from different positions. Agents with different power levels, exit options, and temporal horizons all perceive the same coordinate benefit: alignment on shared rules. The perspectival gap emerges only when sibling readings are considered. The extraction_demo reading would show that the rulebook masks property-accumulation mechanics as skill-driven competition—this would produce Snare or Tangled Rope classifications from the perspective of players who start with unequal resources. The social_scaffold reading would show that the rulebook enables adaptive play across social contexts—this would produce Scaffold classifications emphasizing the sunset logic of informal variants. From within the tournament_orthodoxy frame alone, there is no perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   In the tournament_orthodoxy reading, the directionality (d) for all agents is low because the constraint provides net benefit to organized participants who choose to engage. Competitive game communities are organized agents with mobile exit options; they benefit from coordination and bear no extractive cost. Rules authorities are powerful agents with arbitrage options; they maintain the framework because coordination is the primary function and because reputation depends on fair adjudication. Casual players are moderate agents who voluntarily participate; they derive low-cost coordination benefit. The analytical observer sees the rulebook as providing value to all agents proportionally—it enables fair comparison and skill differentiation. No agent is positioned as a beneficiary extracting from victims; all agents benefit from the shared text.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rulebook_immutability_necessity,
    'Is textual immutability a necessary structural feature of competitive coordination, or merely one design choice among many?',
    'Comparative analysis of coordination mechanisms with and without textual authority (chess vs improvisational variants; poker tournament rules vs casual house-rule poker). Measurement of coordination cost and skill differentiation under each regime.',
    'If necessary: rope classification is structurally sound—immutability is coordination substrate. If contingent: the ''immutability'' framing may naturalize a design choice that serves other readings'' interests (extraction, pedagogical control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rulebook_immutability_necessity, conceptual, 'Whether textual immutability is necessary for competitive coordination or a design choice').

omega_variable(
    sibling_reading_discrimination,
    'How do the extraction_demo and social_scaffold readings differ structurally from this tournament_orthodoxy reading? What observable difference would distinguish them?',
    'Empirical comparison: (1) Does the rulebook enforce competitive discipline or enable pedagogical control? (2) Do players perceive the rules as neutral coordination or as legitimation of outcomes determined by prior social asymmetry? (3) Are house rules treated as noise (tournament_orthodoxy) or as legitimate adaptive coordination (social_scaffold)?',
    'If extraction_demo is correct: the rulebook naturalizes extraction mechanisms (property accumulation, wealth concentration). If social_scaffold is correct: the rulebook enables adaptive social play. If tournament_orthodoxy is correct: the rulebook is purely coordinative—the game''s outcome reflects skill under shared constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_discrimination, conceptual, 'Observable differences that would distinguish the three kernel readings').

omega_variable(
    voluntary_participation_boundary,
    'What constraints on entry, exit, and rule knowledge condition ''voluntary participation'' in the competitive game community?',
    'Ethnographic analysis of tournament entry barriers, rule knowledge requirements, economic cost of participation, social access patterns. Comparison with casual play communities.',
    'If barriers are minimal: rope classification holds—all agents choose to participate in a shared coordination framework. If barriers are high: the reading may naturalize exclusion and competitive inequality (pushing classification toward snare/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_boundary, empirical, 'Whether tournament participation is genuinely voluntary or conditioned on prior constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrto_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mrto_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(mrto_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(mrto_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mrto_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(mrto_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three constraint stories with different epsilon values and different structural readings. Each reading is a distinct constraint with its own classification type, beneficiary/victim structure, and temporal measurements. The three stories are linked as siblings via reading_relations in cs_structure. The tournament_orthodoxy_reading has minimal extractiveness (0.08, Rope) because it treats the rulebook as pure coordination. The extraction_demo_reading would have higher extractiveness (estimated 0.45-0.65) because it frames the rulebook as naturalizing wealth inequality. The social_scaffold_reading would have moderate extractiveness (estimated 0.25-0.35) because it treats the rulebook as a temporary standard that enables adaptive play.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
