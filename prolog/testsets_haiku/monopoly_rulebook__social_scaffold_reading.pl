% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly Rulebook as Social Coordination Scaffold (House Rules Reading)
 *   domain: social/recreational
 *
 * SUMMARY:
 *   The Monopoly rulebook is a contested kernel. Three different structural
 *   readings instantiate three different constraints from the same text. This
 *   story is the SOCIAL_SCAFFOLD_READING: the rulebook as written produces
 *   endgame dynamics (rapid elimination, harsh wealth concentration) that are
 *   hostile to casual social play, where the coordination goal is sustained
 *   group engagement. House rules (free parking money, slower rent
 *   progression, negotiated trades) adapt the rulebook to serve that
 *   coordination function. The constraint is CLAIMED as scaffold because
 *   house rules are explicitly transitional: they suspend the text's
 *   harshness for the duration of the game session, then dissolve when the
 *   group agrees the coordination goal is achieved. This reading coexists
 *   with the extraction_demo_reading (the rulebook demonstrates capitalism's
 *   wealth concentration pedagogically, elimination is the necessary outcome)
 *   and tournament_orthodoxy_reading (the rulebook is immutable competitive
 *   standard). Each reading instantiates a different epsilon, beneficiary
 *   set, and temporal arc.
 *
 * KEY AGENTS:
 *   - casual_players: beneficiaries of sustained social coordination; moderate power; exit via other games or stopping play
 *   - group_coordinator: agenda-setter; recognizes endgame problem early and proposes house rules
 *   - rulebook_text: non-agent; represents the contested kernel (inert; enforcement depends on reading choice)
 *   - competitive_players: excluded from social_scaffold reading (present in tournament_orthodoxy reading); would object house rules obscure skill
 *   - game_designer: observer; represents extraction_demo reading (pedagogical intent, archived, not present in casual game night)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.38).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.22).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook as Social Coordination Scaffold (House Rules Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "social/recreational").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '8d8cec22-589f-46c3-b9fd-22a81e09403a').
narrative_ontology:cs_kernel_codification('8d8cec22-589f-46c3-b9fd-22a81e09403a', fixed_text).
narrative_ontology:cs_authority_grounding('8d8cec22-589f-46c3-b9fd-22a81e09403a', distributed).
narrative_ontology:cs_reading_relation('8d8cec22-589f-46c3-b9fd-22a81e09403a', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d8cec22-589f-46c3-b9fd-22a81e09403a', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('8d8cec22-589f-46c3-b9fd-22a81e09403a', foundational, coordination_authority_supersedes_text_authority_in_social_play).
narrative_ontology:cs_axiom_status(coordination_authority_supersedes_text_authority_in_social_play, holdable).
narrative_ontology:cs_axiom_grounding('8d8cec22-589f-46c3-b9fd-22a81e09403a', coordination_authority_supersedes_text_authority_in_social_play, conventional).
narrative_ontology:cs_axiom('8d8cec22-589f-46c3-b9fd-22a81e09403a', foundational, endgame_harshness_is_separable_from_coordination_function).
narrative_ontology:cs_axiom_status(endgame_harshness_is_separable_from_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('8d8cec22-589f-46c3-b9fd-22a81e09403a', endgame_harshness_is_separable_from_coordination_function, instrumental).
narrative_ontology:cs_reference_frame('8d8cec22-589f-46c3-b9fd-22a81e09403a', group_consensus_authority).
narrative_ontology:cs_drift_state('8d8cec22-589f-46c3-b9fd-22a81e09403a', contemporary_casual_gaming, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8d8cec22-589f-46c3-b9fd-22a81e09403a', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, participants_collective).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).

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
 *   Extractiveness (0.38) is MODERATE — not high. The social_scaffold_reading does NOT claim zero extraction; house rules inject liquidity and slow elimination (themselves extractive modifications), and group coordination effort itself is a cost. But extractiveness is LOWER than strict rulebook play (0.55 projected) because house rules explicitly suppress wealth concentration and elimination. The constraint's extractive component is the group's labor (negotiation, mediation, sustained play effort) required to maintain the coordination; the beneficiary is group cohesion itself, not any individual player. Suppression (0.22) is LOW because house rules are actively supported by group consensus and players can exit to other games. Theater (0.18) is LOW-MODERATE; the game is mostly mechanical with some narrative embellishment, but not theatrical in the sense of performative maintenance. Accessibility_collapse (0.45) is moderate: alternatives exist (other games, not playing) but are not as accessible during active play — once Monopoly starts, the social pressure to continue is real. Resistance (0.72) is HIGH: many casual players actively resist strict rulebook play and demand house rules explicitly. The scaffold claim is supported by has_sunset_clause=true and the measurement series showing extractiveness plateauing and then suppression collapsing when the group agrees the game is over.
 *
 * PERSPECTIVAL GAP:
 *   The social_scaffold_reading and extraction_demo_reading should compute very differently from the SAME RULEBOOK. From a casual-player perspective, house rules are necessary corrections that preserve coordination; from a designer-intent perspective, house rules are corruptions that obscure the pedagogical structure. The engine does NOT compute type from the rulebook directly — it computes type from the declared constraint structure (beneficiary/victim declarations, suppression, extractiveness, claims). The social_scaffold constraint declares beneficiary=group_cohesion, has_sunset_clause=true, and measures moderate extractiveness with high resistance. The extraction_demo constraint would declare beneficiary=none (or pedagogical_vindication), requires_active_enforcement=true (text enforcement against player resistance), and would measure higher extractiveness with lower resistance (pedagogical fidelity enforced). Same kernel, different constraint structures, different terminal types. The gap between readings is where the committer frame does its work.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (social_group_cohesion, participants_collective) is collective, not concentrated in a single agent seat. This is unusual but proper for a coordinate-goods constraint: the benefit flows to the group as a whole (sustained engagement, relational continuity), not to an agenda-setter or a specific payer. Casual_players are both beneficiaries (they get the sustained social experience) and payers (they must invest coordination labor to negotiate and enforce house rules). Group_coordinator bears elevated payer burden (must mediate disputes, clarify rules) and elevated beneficiary benefit (is the connector whose agenda-setting effort preserves the group). This multi-role structure is captured by stakeholder secondary_role declarations. No single agent directionality dominates; the constraint is structurally symmetric in extraction — the group redistributes its own coordination effort to preserve its own cohesion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (endgame harshness is hostile to social coordination) is LIVE — the problem persists across casual game nights. The disappearance_verdict (world_rearranges if the social_scaffold_reading vanished) indicates arrangements depend on house rules for social coordination to persist. There is NO mandatrophy here in the traditional sense — the constraint's function (sustained group engagement via house rules) is still needed and still served. Mandatrophy would be if the founding problem (endgame harshness) became irrelevant (group was okay with elimination) while the arrangement persisted anyway. No evidence of that in casual play — the opposite is observed (house rules are actively deployed to PREVENT the problem). The scaffold is transitional (sunset_clause=true), but transitionality itself is the design — the constraint is meant to suspend harsh mechanics for the duration of play, not to phase out as the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three structurally distinct constraints: social_scaffold_reading (THIS story — house rules, group coordination, moderate extraction, sunset), extraction_demo_reading (text fidelity, pedagogical intent, higher extraction, persistence), and tournament_orthodoxy_reading (text authority, competitive standardization, lower extraction, fidelity required). Each reading instantiates a different epsilon, beneficiary structure, and temporal arc from the SAME rulebook. The three constraints are linked via network.affects_constraints to document the kernel decomposition. They are NOT three perspectives on one constraint — they are three different constraints whose core disagreement is about legitimate authority (coordination vs. text), beneficiary structure (group cohesion vs. pedagogical demonstration vs. competitive ranking), and persistence (sunset vs. permanent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
