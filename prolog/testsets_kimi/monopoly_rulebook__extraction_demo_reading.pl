% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook - Extraction Demo Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the Monopoly rulebook through the
 *   'extraction_demo' reading: the official rules are interpreted not as a
 *   game design choice but as a demonstration of monopoly capitalism's iron
 *   laws. Player elimination and wealth concentration are treated as
 *   structurally necessary outcomesânatural laws of the systemârather
 *   than artifacts of a specific rule-set. The reading claims Mountain status
 *   (inevitability) but the metrics show substantial extraction and
 *   victimization, producing the false-summit signature. The kernel is the
 *   Monopoly rulebook; this reading competes with the social-scaffold reading
 *   (which adds house rules to preserve play) and the tournament-orthodoxy
 *   reading (which treats the rules as a neutral competitive frame).
 *
 * KEY AGENTS:
 *   - eliminated_players: Primary targets (powerless/trapped) â structurally bankrupted and forced into spectator status with no formal exit
 *   - rent_concentrating_player: Primary beneficiary (powerful/constrained) â captures rents within the game's incentive structure once monopolies are secured
 *   - rule_administrator: Agenda setter (moderate/mobile) â enforces official rules against house-rule deviation and controls the pedagogical framing
 *   - house_rule_advocates: Excluded (moderate/constrained) â propose redistributive modifications that the reading rejects as obscuring truth
 *   - analytical_observer: Observer seat â sees the divergence between the mountain claim and the extractive structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.72).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.45).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook - Extraction Demo Reading").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '250268b0-b87d-4f47-9a2c-734bb5f32d14').
narrative_ontology:cs_kernel_codification('250268b0-b87d-4f47-9a2c-734bb5f32d14', fixed_text).
narrative_ontology:cs_authority_grounding('250268b0-b87d-4f47-9a2c-734bb5f32d14', lineage).
narrative_ontology:cs_interpretation_layer_present('250268b0-b87d-4f47-9a2c-734bb5f32d14').
narrative_ontology:cs_reading_relation('250268b0-b87d-4f47-9a2c-734bb5f32d14', monopoly_rulebook__social_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('250268b0-b87d-4f47-9a2c-734bb5f32d14', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('250268b0-b87d-4f47-9a2c-734bb5f32d14', foundational, elimination_reveals_capitalist_necessity).
narrative_ontology:cs_axiom_status(elimination_reveals_capitalist_necessity, holdable).
narrative_ontology:cs_axiom_grounding('250268b0-b87d-4f47-9a2c-734bb5f32d14', elimination_reveals_capitalist_necessity, empirically_contingent).
narrative_ontology:cs_axiom('250268b0-b87d-4f47-9a2c-734bb5f32d14', foundational, redistribution_obscures_systemic_truth).
narrative_ontology:cs_axiom_status(redistribution_obscures_systemic_truth, holdable).
narrative_ontology:cs_axiom_grounding('250268b0-b87d-4f47-9a2c-734bb5f32d14', redistribution_obscures_systemic_truth, conventional).
narrative_ontology:cs_reference_frame('250268b0-b87d-4f47-9a2c-734bb5f32d14', textual_orthodoxy).
narrative_ontology:cs_drift_state('250268b0-b87d-4f47-9a2c-734bb5f32d14', house_rule_prevalence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('250268b0-b87d-4f47-9a2c-734bb5f32d14', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, rent_concentrating_player).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bankrupted through rent extraction under the official rules, they lose all agency and must remain at the table as spectators for 30-plus minutes while the endgame plays out; social pressure prevents them from leaving without being called poor sports.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Acquires color monopolies early and extracts escalating rents from opponents; the rulebook's no-bailout, no-redistribution structure guarantees that wealth flows to this seat regardless of which specific person occupies it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rent_concentrating_player, beneficiary,
    powerful, immediate, constrained, local).

% Owns the game box and enforces the official rulebook against house-rule proposals, framing elimination as an educational demonstration of monopoly capitalism; can allow alternative rules at any time but chooses not to in order to preserve the pedagogical integrity of the demonstration.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rule_administrator, agenda_setter,
    moderate, biographical, mobile, local).

% Players who propose liquidity injections, free parking bonuses, or shared endings to prevent elimination; their proposals are ruled out of order as 'not real Monopoly' and excluded from the rule-setting conversation once the extraction-demo frame is invoked.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_advocates, excluded,
    moderate, immediate, constrained, local).

% Game theorist or critical pedagogue who observes that the rulebook's mathematical structureâfixed rents, no redistribution, eliminationâguarantees wealth concentration and player removal regardless of variance in skill or dice rolls.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, analytical_observer, observer,
    analytical, biographical, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, rent_concentrating_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the conventional sense; the arrangement is explicitly anti-coordinating. The only 'coordination' is the shared acceptance of the rulebook as a demonstration apparatus that participants agree to treat as revealing an economic law rather than as a mutually enjoyable activity.
% TRANSFER_FUNCTION: Moves time, attention, play-equity, and social standing from eliminated players to the rent-concentrating player; simultaneously transfers pedagogical authority from the player group to the rule administrator who controls the official text.
% ABSENT_VOICES: Eliminated players mid-game have no formal mechanism to alter the rulebook; house-rule advocates are structurally excluded from rule-setting once the 'official game' begins; cooperative-play advocates and care-oriented players are absent from the extraction-demo framing entirely.
% DISAPPEARANCE_RATIONALE: If the rulebook and its extraction-demo reading disappeared overnight, the players would no longer be organized around a demonstration of monopoly capitalism; the social gathering would either dissolve or reorganize around a different game or house-ruled cooperative frame. The specific relationships of rent extraction and forced spectatorhood would vanish.
% FOUNDING_PROBLEM: How to demonstrate the inevitable concentration of wealth and the elimination of competitors under conditions of rent-extracting monopoly capitalism.
% FOUNDING_PROBLEM_CORROBORATION: No external corroboration exists outside the benefiting pedagogical authority; the 'problem' is self-asserted by the reading itself. Marxist political economy provides external analytical frameworks that observe similar dynamics in historical economies, but no independent party attests that a board game session is the necessary or appropriate solution to demonstrating these dynamics.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness (0.72) is authored high because the rulebook systematically strips eliminated players of agency and transfers play-value to the leader over the 60-90 minute duration. Suppression (0.45) captures the active rule-enforcement that bars redistributive house rules and treats them as illegitimate. Accessibility collapse (0.88) is high because once the rules are understood, the endgame trajectory is nearly deterministicâalternatives such as cooperative endings or shared victories are cognitively closed off by the 'official rules' and 'pedagogical truth' frames. Resistance (0.15) is low because players accept the rules as given, especially under the demonstration framing. Theater ratio (0.28) reflects the performative aspect of eliminated players staying at the table to 'learn the lesson' rather than leaving.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (rule administrator) experiences the constraint as a neutral educational apparatusâa Mountain revealing truth about capitalism. The eliminated players experience it as a coercive extraction structure with no exit mid-game. The rent-concentrating player experiences a mix: they benefit materially within the game but may experience moral discomfort or social stigma, giving them a more symmetric directionality than a pure beneficiary position.
 *
 * DIRECTIONALITY LOGIC:
 *   Eliminated players are declared victims with trapped exitâstructurally they cannot leave without social penalty and cannot alter the rules, placing their d near the full-target end. The rent-concentrating player is the declared beneficiary, but their exit is constrained (they are bound by the same rulebook and social contract of the game session), giving them a moderate-low d rather than zero. The rule administrator has mobile exit (can stop enforcing or change the rules) but chooses not to, placing their d closer to the agenda-setter/beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims to be a Mountain (inevitable law of monopoly capitalism) but functions as an extraction mechanism with identifiable beneficiaries and victims. The mandatrophy test prevents mislabeling: a genuine Mountain would have no beneficiaries or victims and near-zero extraction. Here, the presence of beneficiaries, victims, and high epsilon signals that the 'inevitability' is a constructed framing serving a pedagogical function, not a natural law. The reading persists because it validates a worldview, not because the laws of physics or logic demand it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_truth_vs_construct,
    'Is the elimination outcome a natural law of economic systems revealed by the game, or a constructed rule-set producing an artifactual outcome that is then framed as inevitable?',
    'Comparative analysis of alternative board game designs with different rent and redistribution mechanics: if those designs produce different wealth-concentration outcomes, the Monopoly result is artifactual rather than revelatory.',
    'If the outcome is artifactual, the Mountain claim collapses and the constraint reclassifies as Snare or Tangled Rope; if genuinely revelatory, the high epsilon must be reinterpreted as the cost of a necessary demonstration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_vs_construct, empirical, 'Whether the rulebook reveals a natural law or constructs an extractive demonstration.').

omega_variable(
    enforcement_dependency_of_inevitability,
    'Does the ''inevitable'' outcome require active suppression of house rules and alternative play-styles, or would wealth concentration emerge even if players freely modified rules mid-game?',
    'Natural experiment comparing official-rules sessions with house-rule sessions: if house-rule sessions show reduced elimination and slower concentration, the inevitability claim depends on enforcement.',
    'If the outcome is enforcement-dependent, the Mountain claim is falsified and the constraint is exposed as actively extractive rather than naturally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependency_of_inevitability, empirical, 'Whether the constraint''s inevitability claim depends on active rule enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.35).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel decomposes into three structurally distinct constraints: this extraction_demo_reading (high epsilon, claimed mountain, extractive operation), the social_scaffold_reading (house rules as corrective coordination), and the tournament_orthodoxy_reading (neutral competitive frame). Each has a distinct epsilon, stakeholder set, and classification. They are linked as a constraint family because they share the same underlying kernel text but instantiate different constraints through divergent readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
