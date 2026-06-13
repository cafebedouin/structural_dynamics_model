% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__extraction_demo_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
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
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook as Inevitable Wealth Concentration Mechanism
 *   domain: game_theory/institutional_design/pedagogy
 *
 * SUMMARY:
 *   The Monopoly rulebook, in this reading, is not merely a game
 *   specification but a constraint that instantiates and demonstrates the
 *   inevitable wealth concentration of capitalism without redistribution. The
 *   unmodified rulebook creates a dynamic where early property acquisition
 *   triggers exponential rent-collection growth, leading to the bankruptcy
 *   and elimination of other players. The reading treats this outcome as
 *   pedagogically essential: the rulebook's failure to prevent harsh
 *   elimination is the point, not a design flaw. The game's structure is read
 *   as a structural law: given property-auction mechanics, unidirectional
 *   rent collection without income redistribution, and elimination on
 *   bankruptcy, wealth concentration is inevitable. This is ONE reading of
 *   the contested Monopoly rulebook kernel. Sibling
 *   readings—social_scaffold_reading (house rules are necessary community
 *   correction to preserve playability) and tournament_orthodoxy_reading
 *   (unmodified rulebook defines legitimate competitive
 *   skill-testing)—instantiate different structural claims about the same
 *   text. The readings share a kernel (the printed rules) but diverge on what
 *   the rulebook's operation demonstrates.
 *
 * KEY AGENTS:
 *   - rulebook_text: The fixed specification of property auction, rent collection, bankruptcy elimination, and no redistribution. The kernel.
 *   - early_winner_player: Benefits from the extraction mechanism; exemplifies the player whose early advantage creates exponential wealth accumulation.
 *   - landed_middle_player: Trapped between inadequate cash reserves and no escape path; demonstrates the mechanism's harshness.
 *   - eliminated_players: Victims; permanently removed from play once bankruptcy occurs; no recovery option under the rulebook.
 *   - player_coalition: Could modify rules but is excluded by the reading's pedagogical claim that modification loses the signal.
 *   - pedagogical_observer: Witnesses the game's operation as a demonstration of how capitalism's logic works when redistribution is absent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.72).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Inevitable Wealth Concentration Mechanism").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/institutional_design/pedagogy").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '4407fc04-09d8-4968-ab1d-9b168e82e3b1').
narrative_ontology:cs_kernel_codification('4407fc04-09d8-4968-ab1d-9b168e82e3b1', fixed_text).
narrative_ontology:cs_authority_grounding('4407fc04-09d8-4968-ab1d-9b168e82e3b1', extraction).
narrative_ontology:cs_reading_relation('4407fc04-09d8-4968-ab1d-9b168e82e3b1', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('4407fc04-09d8-4968-ab1d-9b168e82e3b1', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('4407fc04-09d8-4968-ab1d-9b168e82e3b1', foundational, extraction_inevitable_without_redistribution).
narrative_ontology:cs_axiom_status(extraction_inevitable_without_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('4407fc04-09d8-4968-ab1d-9b168e82e3b1', extraction_inevitable_without_redistribution, empirically_contingent).
narrative_ontology:cs_axiom('4407fc04-09d8-4968-ab1d-9b168e82e3b1', secondary, rulebook_modification_obscures_signal).
narrative_ontology:cs_axiom_status(rulebook_modification_obscures_signal, holdable).
narrative_ontology:cs_axiom_grounding('4407fc04-09d8-4968-ab1d-9b168e82e3b1', rulebook_modification_obscures_signal, instrumental).
narrative_ontology:cs_reference_frame('4407fc04-09d8-4968-ab1d-9b168e82e3b1', unmodified_rulebook_as_pedagogical_specification).
narrative_ontology:cs_drift_state('4407fc04-09d8-4968-ab1d-9b168e82e3b1', contemporary_houseration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4407fc04-09d8-4968-ab1d-9b168e82e3b1', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, winner_player).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, ExtMetricName, E),
    domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) reflects the cumulative transfer of wealth from losers to winner; it grows over time as rent collection accelerates. The suppression metric (0.72) captures the elimination mechanism: players have no way to recover once bankrupt, no alternative, no exit except quitting (which validates the rulebook by acknowledging its inevitability). Theater ratio is low (0.18) because the constraint's function is not performative—it is the authentic operation of the system, not a simulacrum. Accessibility collapse is high (0.81) because once a player understands the rulebook's mechanics, the set of viable alternatives (ways to win) shrinks drastically: only property monopoly-acquisition works; other strategies lead predictably to elimination. Resistance is moderate-to-high (0.64) because players regularly attempt to modify rules (house rules), negotiate loans, or organize coalitions—but these resistance moves are read by the extraction_demo reading as attempts to obscure the constraint's truth-telling function, not as legitimate alternatives. The measurement series shows extractiveness and suppression increasing over the interval (0-90 minutes) as the game progresses: early on, all players have cash and options; by endgame, most are eliminated and the constraint operates at full force. Theater is flat and low throughout, consistent with the reading's claim that this is no simulacrum—it is the constraint's authentic operation.
 *
 * PERSPECTIVAL GAP:
 *   The early_winner_player, if interviewed, would likely describe the rulebook as a fair competitive framework where superior strategy and luck lead to just rewards. The eliminated_players and landed_middle_player would describe it as a trap that punishes early disadvantage with no recovery path. The pedagogical_observer would describe it as a demonstration of capitalism's logic. These perspectives diverge sharply not because the rulebook is ambiguous but because the constraint's benefits and costs are asymmetrically distributed by design. The engine will compute different effective-extraction (χ) values for each seat: near-zero or negative for the winner (the rulebook subsidizes them), high positive for the losers and eliminated. The extraction_demo reading asserts this divergence is not a failure of fairness but a demonstration of how systems without redistribution work. The perspectival gap IS the pedagogical content.
 *
 * DIRECTIONALITY LOGIC:
 *   The early_winner_player has directionality near 0.0 (beneficiary): the rulebook subsidizes their position; their d derives from role:agenda_setter + power:moderate + exit:arbitrage (they can leverage their advantage). The landed_middle_player has directionality near 0.7 (partial target): they pay rent to the winner and have constrained exit (d from role:payer + power:moderate + exit:constrained). The eliminated_players have directionality near 1.0 (full target): they are trapped, have lost all cash, and are removed by the rulebook's mandatory elimination (d from role:payer + power:powerless + exit:trapped). The pedagogical_observer has directionality at 0.5 (symmetric): they neither benefit nor pay; they witness. These directionalities are derived from the stakeholder structural data without override. The rulebook itself (agent:false, role:observer) does not have a directionality; it is the constraint being evaluated, not a stakeholder in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (modeling capitalism's inevitable wealth concentration) was the game's original pedagogical intent. The founding_problem_status is 'contested' because contemporary audiences disagree on whether the rulebook still serves that pedagogical function or has become entertainment capitalism's own rent-extraction mechanism (Hasbro profits from the game; competitive players profit from tournaments; neither has an interest in house rules that would blur the signal). If the founding problem is read as 'dead'—the game is now primarily entertainment, not pedagogy—while the world_rearranges verdict stands (players invent house rules to survive the unmodified game), this would trigger a mandatrophy flag: the rulebook persists in its original form despite losing its original function. However, the extraction_demo reading refuses this decay narrative: it asserts the constraint's function IS to demonstrate inevitability, and that demonstration is precisely what the rulebook's unmodified operation provides. No mandatrophy resolution is declared in base_properties because the reading maintains the function=operation alignment. The omega on pedagogical_signal_decay addresses the empirical question that would settle this: if the signal persists across house rules, mandatrophy becomes visible and the extraction_demo reading's claim to naturalness weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_designed_extraction,
    'Is the rulebook a description of an inevitable natural law about competitive systems without redistribution, or is it a designed extraction mechanism that benefits the reading''s author (Hasbro, in the market for game sales) by creating memorable, engaging competitive dynamics?',
    'Historical analysis of the rulebook''s design iterations and intentional choices. Comparison with alternative rulebook versions that would produce different extraction profiles. Analysis of whether the contemporary game serves pedagogical aims or entertainment/profit aims.',
    'If the rulebook describes natural law, the constraint is a genuine mountain: it demonstrates something unavoidable about competition. If the rulebook is designed extraction, it is a snare or tangled_rope defended by the false claim of naturality (false summit candidate). The difference determines whether the game can be ''fixed'' (designed differently) or only observed (a law of systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_designed_extraction, empirical, 'Whether the extraction profile is inevitable or designed.').

omega_variable(
    pedagogical_signal_decay,
    'Does the rulebook''s pedagogical signal (demonstrating wealth concentration) degrade if players deploy house rules, or is the signal robust to modification?',
    'Empirical comparison: measure the extraction metrics, elimination rate, and wealth-concentration speed under three conditions: (1) unmodified rulebook, (2) common house rules (free parking pool, lenient landing), (3) designed catch-up mechanics (bankruptcy protection, income redistribution). If (2) and (3) still produce the same extraction pattern, the signal is robust and the rulebook is one instantiation of a broader law, not the unique locus of the demonstration.',
    'If the signal requires the unmodified rulebook, then house rules are a loss of pedagogical content, and the extraction_demo reading stands: the rulebook''s specific form is the truth-telling mechanism. If the signal persists across modifications, the rulebook is a vehicle, not a law, and the social_scaffold reading''s claim that community correction is compatible with the core logic gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_signal_decay, empirical, 'Whether pedagogical signal depends on rule fidelity.').

omega_variable(
    reading_committer_beneficiary,
    'Who benefits from framing the rulebook as a demonstration of natural law rather than as a modifiable design? And does that benefit constitute a false summit?',
    'Structural analysis of incentives: does the extraction_demo reading serve pedagogical aims (authentic critique of capitalism) or commercial aims (Hasbro''s interest in selling the canonical game, defenders'' interest in tournament rules, competitive players'' interest in skill-based ranking)? Does the reading use ''natural law'' framing to suppress discussion of redistribution mechanics that would reduce the game''s market differentiation?',
    'If the reading benefits interested parties (commercial, competitive) by invoking naturality, the constraint is a false summit: it is actually a designed snare or tangled_rope defended by a claim of inevitability. This would trigger FSM evaluation and potential reclassification. The presence of beneficiaries (the winner player, the publisher, the competitive establishment) already flags this omega as critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_beneficiary, conceptual, 'Whether the natural-law framing serves hidden interests.').

omega_variable(
    victim_set_definition_ambiguity,
    'Are the ''victims'' in this reading the players eliminated during play, or is the victim set broader—including anyone who plays and does not win, or anyone in the society the game models?',
    'Definitional clarity: does elimination (bankruptcy without recovery option) constitute harm sufficient to mark someone as a victim, or only players who survive but never accumulate property? And does the game''s pedagogical role mean the harm is an intended demonstration (thus not ''harm'' but ''signal''), shifting the victim analysis?',
    'If victims = eliminated players only, the victim set shrinks as the game progresses (starting at 0, ending at n-1 by endgame). If victims = all non-winners, the victim set is constant but less sharply defined. The measure of suppression and extractiveness depends on victim-set definition. The elimination mechanism is the clearest suppression vector; ambiguity on victim identity affects the directionality calculus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_definition_ambiguity, conceptual, 'Definitional ambiguity in victim-set boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.18).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel decomposes into three structurally distinct constraint readings: extraction_demo_reading (this story, treating the rulebook as inevitable law), social_scaffold_reading (treating it as a starting point requiring community correction), and tournament_orthodoxy_reading (treating it as the legitimate competitive specification). The three readings share a kernel (the printed rules) but instantiate different structural claims. Each has its own ε, beneficiary/victim structure, and type certification. Extraction_demo has high ε (0.68, pure extraction) and claims mountain status (inevitable law). Social_scaffold would have lower ε with redistribution mechanisms and claim scaffold or rope status (temporary coordination requiring community correction). Tournament_orthodoxy would have moderate ε with framing as skill-testing and claim rope status (fair competitive mechanism). The readings coexist as live positions but influence each other: extraction_demo's claim to inevitability pressures the others to justify their modifications or rankings. The network links enable contamination analysis: if extraction_demo's mountain claim is undermined (revealed as false summit via beneficiary analysis), the scaffolding and orthodoxy readings gain relative credibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
