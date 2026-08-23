% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Monopoly Rulebook — Social Scaffold Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   The Monopoly rulebook as commercially published mandates a ruthless
 *   elimination game: players are bankrupted one by one, the game lasts 3-4
 *   hours, and eliminated players have no role but to watch. In practice,
 *   almost no social group plays this way. House rules — free parking
 *   jackpot, no property auctions, unlimited houses, money for landing on Go
 *   — are near-universal. These rules inject liquidity, slow elimination, and
 *   extend the game, preserving the social gathering. This reading
 *   (social_scaffold) treats the house-rule layer as the real constraint: a
 *   scaffold that makes the rulebook socially playable by subordinating
 *   competitive fidelity to group cohesion. The extraction is moderate
 *   (0.42): tournament purists pay a cost (their preferred framework
 *   displaced), but the primary beneficiary is the social group itself, not a
 *   concentrated extractor. The constraint has a sunset clause: it persists
 *   only while the group chooses to play Monopoly rather than switching to a
 *   game that doesn't require community correction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.28).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook — Social Scaffold Reading").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '2731c24b-087d-44d6-b8d7-f91e15d289cc').
narrative_ontology:cs_kernel_codification('2731c24b-087d-44d6-b8d7-f91e15d289cc', fixed_text).
narrative_ontology:cs_authority_grounding('2731c24b-087d-44d6-b8d7-f91e15d289cc', practice).
narrative_ontology:cs_interpretation_layer_present('2731c24b-087d-44d6-b8d7-f91e15d289cc').
narrative_ontology:cs_reading_relation('2731c24b-087d-44d6-b8d7-f91e15d289cc', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('2731c24b-087d-44d6-b8d7-f91e15d289cc', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('2731c24b-087d-44d6-b8d7-f91e15d289cc', foundational, social_continuity_over_competitive_fidelity).
narrative_ontology:cs_axiom_status(social_continuity_over_competitive_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('2731c24b-087d-44d6-b8d7-f91e15d289cc', social_continuity_over_competitive_fidelity, conventional).
narrative_ontology:cs_axiom('2731c24b-087d-44d6-b8d7-f91e15d289cc', foundational, house_rules_are_load_bearing_not_deviant).
narrative_ontology:cs_axiom_status(house_rules_are_load_bearing_not_deviant, holdable).
narrative_ontology:cs_axiom_grounding('2731c24b-087d-44d6-b8d7-f91e15d289cc', house_rules_are_load_bearing_not_deviant, empirically_contingent).
narrative_ontology:cs_reference_frame('2731c24b-087d-44d6-b8d7-f91e15d289cc', commercial_rulebook_as_published).
narrative_ontology:cs_drift_state('2731c24b-087d-44d6-b8d7-f91e15d289cc', contemporary_social_play, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2731c24b-087d-44d6-b8d7-f91e15d289cc', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, tournament_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, casual_players).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, eliminated_early_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, play_as_social_maintenance).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, coordination_over_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective social fabric that house rules protect — the ongoing gathering, the shared evening, the relationships that persist beyond the game. House rules that inject liquidity and slow elimination serve this beneficiary by preventing the harsh endgame that would fracture the group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_cohesion, beneficiary,
    analytical, biographical, analytical, local).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).

% Players who participate for social connection rather than competitive mastery. They benefit from house rules (free parking jackpot, no auction, unlimited houses) that keep everyone in the game longer. They would leave if elimination came early, but they rarely organize to change the rules — they just adopt whatever the host proposes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% The person or household that owns the game, sets the table, and declares which house rules apply. They administer the constraint by choosing the rule variant. Their power is real but bounded — if the game breaks the social evening, they lose the gathering. They can switch to the tournament ruleset or any variant at will.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, host_household, agenda_setter,
    moderate, biographical, arbitrage, local).

% Players who value the rulebook as a competitive framework and see house rules as corruption of the game's strategic depth. They pay a cost when the social scaffold reading prevails: their preferred competitive test is displaced, their skill advantage is diluted by randomness (free parking), and the game duration extends beyond what tournament play allows. They cannot easily exit the social game without exiting the social group.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_purists, payer,
    organized, biographical, constrained, local).

% Players who would be eliminated early under the tournament ruleset but are kept alive by house rules. Paradoxically, they are payers in the tournament reading (they lose) but beneficiaries in the scaffold reading (they stay in the social circle). In this reading, they benefit from the scaffold — but they bear the cost of a prolonged game they cannot win, sitting through hours of inevitable loss.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, eliminated_early_players, payer,
    powerless, immediate, trapped, local).

% Analysts who study Monopoly as a designed system. They observe that the rulebook as written produces a harsh elimination dynamic that few groups actually play. They note the near-universal adoption of house rules as evidence that the text requires community correction to function socially.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_design_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook provides a shared activity frame that structures a social evening — a reason to gather, a common focus, a bounded time container. The coordination problem is not the game mechanics but the social maintenance: how to keep a mixed-skill group engaged for 3+ hours without anyone feeling excluded or the evening collapsing into boredom or conflict.
% TRANSFER_FUNCTION: House rules transfer game-outcome certainty (who wins, when) into social continuity (everyone stays, the evening continues). The free parking jackpot redistributes from the bank to the trailing players; no-auction and unlimited-houses rules slow the asset concentration that drives elimination. The transfer is from competitive fidelity to social duration.
% ABSENT_VOICES: Competitive players who would prefer the tournament ruleset but are not present at this table — they self-select out of social game nights. Also absent: the game's original designer (Elizabeth Magie) whose pedagogical intent was precisely the harsh elimination the house rules undo. Her voice is structurally excluded by the game's commercial repackaging.
% DISAPPEARANCE_RATIONALE: If the social scaffold reading vanished — if groups played strictly by the tournament ruleset — most casual game nights would end in early elimination, hurt feelings, and abandoned games. The social institution of 'Monopoly night' would largely disappear or shift to other games. The rulebook text alone does not sustain the social practice; the house-rule layer is load-bearing.
% FOUNDING_PROBLEM: The commercial rulebook (1935 Darrow/Parker Brothers edition) codified a game that eliminates players one by one over 3-4 hours, leaving eliminated players with nothing to do but watch. This creates a social problem: how to keep a group together for an evening when the game's mechanics actively fracture it.
% FOUNDING_PROBLEM_CORROBORATION: Game historians (e.g., Philip Orbanes, Mary Pilon) document that house rules emerged immediately and universally — the 'standard' rules were never the standard in practice. The persistent 3+ hour game duration in social settings, documented across decades of oral tradition, corroborates that the founding problem (social fracture from elimination) remains live. Tournament players contest this, but they are a distinct subpopulation.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects that the constraint does extract from tournament purists — their competitive test is displaced — but the extraction is diffuse and the primary flow is toward social maintenance, not a concentrated beneficiary. Suppression (0.28) is low: house rules are adopted voluntarily, not enforced by authority; the 'enforcement' is social pressure to conform to the group's chosen variant. Theater ratio (0.35) captures that the rulebook text is performed (read aloud, referenced) but the actual operation runs on the unwritten house-rule layer. Accessibility collapse (0.45) is moderate: alternatives exist (other games, tournament play) but the social inertia of 'Monopoly night' makes them costly to reach. Resistance (0.55) is significant: tournament purists actively resist house rules, and the tension between the two readings is a persistent feature of the game's social life.
 *
 * PERSPECTIVAL GAP:
 *   From the host/casual seat, the constraint is a genuine coordination scaffold — it solves the social maintenance problem. From the tournament_purist seat, the same structure operates as extraction — their competitive framework is displaced by a randomized, prolonged variant. From the eliminated_early seat, the constraint is a mixed blessing: inclusion at the cost of agency. The engine computes this seat divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The social_group_cohesion beneficiary (analytical, non-agent) sits at d ≈ 0.0 — the constraint exists for it. Casual_players (moderate, mobile) are near-symmetric beneficiaries: they gain social continuity, pay little cost. The host_household (agenda_setter, moderate, arbitrage) controls the rule choice and can exit the constraint entirely by choosing a different game. Tournament_purists (organized, constrained) are payers: they bear the cost of displaced competitive fidelity with limited exit (leaving the game means leaving the social circle). Eliminated_early_players (powerless, trapped) are paradoxical: in this reading they benefit from inclusion, but they pay the cost of prolonged inevitable loss. The engine will compute these directionalities from the structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (social fracture from elimination) remains live — groups still need a 3-hour shared activity that accommodates mixed skill levels. The scaffold has not atrophied; it is actively maintained because the alternative (tournament rules) fails the social test. However, the mandate has shifted: the rulebook's original pedagogical mandate (demonstrate monopoly capitalism's cruelty) is dead, replaced by a social maintenance mandate. This is not mandatrophy — the function persists, but the function is not the one the text claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the monopoly_rulebook kernel admit these three readings as structurally distinct constraints, or are they interpretive variations of a single constraint?',
    'If the three readings produce different ε values, different beneficiary/victim structures, and different computed types under the engine, they are distinct constraints. The ε-invariance principle requires decomposition.',
    'If they are one constraint, the ε-invariance principle is violated — the same constraint would have different ε under different readings. The decomposition into three stories is structurally necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel admits multiple ε-invariant constraints or one constraint with observer-dependent classification').

omega_variable(
    scaffold_sunset_condition,
    'What would trigger the sunset clause — groups switching to other games, or the rulebook being revised to match social play?',
    'Track game-night substitution rates: if Monopoly nights decline in favor of Catan, Ticket to Ride, etc., the scaffold sunsets by abandonment. If Hasbro revises the official rules to incorporate common house rules (as they have partially done), the scaffold sunsets by absorption.',
    'If sunset comes by absorption, the scaffold successfully transitions to a rope (the corrected rulebook becomes the coordination standard). If by abandonment, the scaffold was a temporary patch on a failing product.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_condition, empirical, 'Whether the scaffold''s sunset comes by institutional absorption or social abandonment').

omega_variable(
    tournament_purist_coalition,
    'Can tournament purists form a coalition that shifts the local norm to tournament rules, or are they structurally constrained by the social group''s preference?',
    'Observe mixed groups where a tournament purist is present: does the group adopt tournament rules, house rules, or split? The power/exit structure suggests purists are constrained — but coalition formation could change the local equilibrium.',
    'If purists can shift the norm, the scaffold is less stable than modeled. If they cannot, their payer role is structurally locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tournament_purist_coalition, empirical, 'Whether the payer seat can organize to change the constraint''s operation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_scaffold_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t18, monopoly_rulebook__social_scaffold_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t24, monopoly_rulebook__social_scaffold_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t30, monopoly_rulebook__social_scaffold_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(monopoly_social_scaffold_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(monopoly_social_scaffold_be_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(monopoly_social_scaffold_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(monopoly_social_scaffold_be_t18, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 18, 0.41).
narrative_ontology:measurement(monopoly_social_scaffold_be_t24, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(monopoly_social_scaffold_be_t30, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_social_scaffold_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_su_t6, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement(monopoly_social_scaffold_su_t12, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 12, 0.24).
narrative_ontology:measurement(monopoly_social_scaffold_su_t18, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 18, 0.27).
narrative_ontology:measurement(monopoly_social_scaffold_su_t24, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(monopoly_social_scaffold_su_t30, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.08).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Monopoly rulebook kernel decomposes into three readings: extraction_demo (pedagogical snare), social_scaffold (social maintenance scaffold), tournament_orthodoxy (competitive rope/mountain). This reading (social_scaffold) treats the house-rule layer as the real constraint; the rulebook text is the kernel that requires correction. The extraction_demo reading treats the text as the active constraint; the tournament reading treats the text as the immutable standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__social_scaffold_reading, powerless, 0.75).
constraint_indexing:directionality_override(monopoly_rulebook__social_scaffold_reading, organized, 0.65).
constraint_indexing:directionality_override(monopoly_rulebook__social_scaffold_reading, moderate, 0.35).
constraint_indexing:directionality_override(monopoly_rulebook__social_scaffold_reading, analytical, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
