% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Monopoly Rulebook: Extraction Demonstration Reading
 *   domain: game_theory/institutional_design/political_economy
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Monopoly rulebook,
 *   interpreting it as a 'mountain' that instantiates and demonstrates the
 *   inevitable wealth concentration and rent extraction inherent in monopoly
 *   capitalism. From this perspective, player elimination is a structurally
 *   necessary outcome, serving as a pedagogical truth. The high
 *   extractiveness and suppression reflect the game's design to produce these
 *   outcomes, while low theater ratio indicates its direct, unadorned
 *   function as a demonstration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.75).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.85).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Extraction Demonstration Reading").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/institutional_design/political_economy").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '95836456-5e5c-408c-ae47-3312bb76386a').
narrative_ontology:cs_kernel_codification('95836456-5e5c-408c-ae47-3312bb76386a', fixed_text).
narrative_ontology:cs_authority_grounding('95836456-5e5c-408c-ae47-3312bb76386a', lineage).
narrative_ontology:cs_interpretation_layer_present('95836456-5e5c-408c-ae47-3312bb76386a').
narrative_ontology:cs_reading_relation('95836456-5e5c-408c-ae47-3312bb76386a', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('95836456-5e5c-408c-ae47-3312bb76386a', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('95836456-5e5c-408c-ae47-3312bb76386a', foundational, capitalist_accumulation_is_natural).
narrative_ontology:cs_axiom_status(capitalist_accumulation_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('95836456-5e5c-408c-ae47-3312bb76386a', capitalist_accumulation_is_natural, empirically_contingent).
narrative_ontology:cs_axiom('95836456-5e5c-408c-ae47-3312bb76386a', foundational, game_as_pedagogical_tool).
narrative_ontology:cs_axiom_status(game_as_pedagogical_tool, holdable).
narrative_ontology:cs_axiom_grounding('95836456-5e5c-408c-ae47-3312bb76386a', game_as_pedagogical_tool, instrumental).
narrative_ontology:cs_reference_frame('95836456-5e5c-408c-ae47-3312bb76386a', classical_monopoly_model).
narrative_ontology:cs_drift_state('95836456-5e5c-408c-ae47-3312bb76386a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('95836456-5e5c-408c-ae47-3312bb76386a', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, dominant_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, capitalist_ideologues).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, subordinate_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_inevitability).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, wealth_concentration_natural_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Players who successfully accumulate property and wealth, ultimately eliminating others. They benefit directly from the game's mechanics and see their success as a demonstration of the 'truth' of the system.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, dominant_players, agenda_setter,
    powerful, immediate, arbitrage, local).

% Players who struggle to accumulate, pay rents, and are gradually stripped of assets. They are subject to the game's extractive mechanics but may not fully grasp its 'pedagogical truth' or feel trapped by its inevitability.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, subordinate_players, payer,
    moderate, immediate, constrained, local).

% Players who have lost all their assets and are removed from the game. They are the ultimate victims of the game's extractive logic, demonstrating the 'necessary outcome' of wealth concentration.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Those who designed the game or interpret its outcomes as a model for real-world economic systems. They frame the rulebook as a demonstration of inevitable capitalist dynamics.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_designers_theorists, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, game_designers_theorists, agenda_setter).

% Individuals or institutions whose worldview is reinforced by the game's outcomes. They use the game as a pedagogical tool to illustrate and legitimize wealth concentration and market-driven elimination.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, capitalist_ideologues, beneficiary,
    institutional, generational, analytical, global).

% Analysts who study the game's mechanics and outcomes, often critiquing its underlying assumptions or its role in normalizing capitalist dynamics. They observe the extraction but may dispute its 'inevitability'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, critical_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, dominant_players).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally understood framework for competitive play, resource accumulation, and player elimination, ensuring a predictable progression towards a single winner.
% TRANSFER_FUNCTION: Systematically transfers property, money, and ultimately player agency from subordinate and eliminated players to dominant players, culminating in a winner-takes-all scenario.
% ABSENT_VOICES: Players advocating for redistribution mechanisms, cooperative play modes, or alternative victory conditions that would challenge the game's inherent extractive and eliminatory logic. Their absence ensures the 'pedagogical truth' is demonstrated without structural interference.
% DISAPPEARANCE_RATIONALE: If the rulebook vanished, the game's core mechanism for demonstrating inevitable wealth concentration and elimination would disappear. The 'pedagogical truth' it instantiates would no longer have this concrete, widely understood model, requiring a new system to illustrate these dynamics.
% FOUNDING_PROBLEM: To provide a tangible, experiential model that demonstrates the inherent, inevitable dynamics of capitalist accumulation, wealth concentration, and the elimination of less successful actors.
% FOUNDING_PROBLEM_CORROBORATION: Game theorists, political economists, and educators who use the game as a teaching tool or a simplified model of complex economic systems, independent of the players' immediate experiences. Historical analyses of the game's reception and interpretation also corroborate this framing.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.75) because the game's core design ensures a systematic transfer of wealth from many to few, culminating in elimination. Suppression is very high (0.85) as the rules actively prevent alternatives like redistribution or cooperative play, enforcing the 'inevitable' outcome. Theater ratio is low (0.10) because the game's function is primarily to directly demonstrate its 'pedagogical truth,' with minimal performative overhead. Accessibility collapse is high (0.90) as, once the game's logic is understood, alternatives to its extractive path are seen as structurally impossible within its framework.
 *
 * PERSPECTIVAL GAP:
 *   The 'pedagogical truth' perspective of the game designers/theorists and capitalist ideologues fundamentally differs from the lived experience of subordinate and eliminated players. The former see an inevitable demonstration, while the latter experience direct, enforced extraction and loss. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant players and capitalist ideologues are beneficiaries (d near 0.0) as they directly profit from or are vindicated by the game's outcomes. Subordinate and eliminated players are targets (d near 1.0) as they bear the costs of extraction and elimination. Game designers/theorists and critical theorists are observers (d near 0.5), analyzing the system without directly participating in its extractive flows, though their interpretations shape its perceived 'truth'.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_demonstration,
    'Is the Monopoly rulebook a genuine ''mountain'' reflecting an inevitable natural law of wealth concentration, or a ''snare'' (or ''tangled_rope'') that is a constructed demonstration designed to legitimize a particular economic ideology?',
    'Comparative analysis with alternative game designs that model different economic systems, and historical analysis of the game''s design intentions and ideological reception. If alternative designs yield different outcomes, it suggests construction over natural law.',
    'If found to be a constructed demonstration, its classification would shift from Mountain to Snare or Tangled Rope, highlighting its active enforcement and identifiable victims, rather than its claimed inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_demonstration, conceptual, 'Ambiguity between claimed natural law and constructed ideological tool.').

omega_variable(
    pedagogical_truth_vs_ideological_justification,
    'Is the ''pedagogical truth'' claim a genuine analytical insight into economic systems, or an ideological justification that normalizes and legitimizes the outcomes of a specific economic model?',
    'Analysis of the game''s impact on players'' real-world economic beliefs and behaviors, and critical examination of the theoretical frameworks used to interpret the game. If it primarily serves to entrench existing beliefs rather than foster critical understanding, it leans towards ideological justification.',
    'If primarily an ideological justification, the ''beneficiary'' status of capitalist_ideologues would be amplified, and the constraint''s overall extractiveness might be re-evaluated as more insidious, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_vs_ideological_justification, conceptual, 'Distinction between objective demonstration and ideological framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__extraction_demo_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__extraction_demo_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__extraction_demo_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(mono_tr_t50, monopoly_rulebook__extraction_demo_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(mono_be_t50, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(mono_su_t50, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
