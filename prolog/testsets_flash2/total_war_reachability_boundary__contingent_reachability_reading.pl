% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Total War Reachability Boundary
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint represents the 'contingent reachability' reading of the
 *   total war boundary, asserting that the current contraction of total war
 *   options is a technology-dependent piton. It argues that while total war
 *   may seem less reachable now, this is due to a specific technological
 *   equilibrium (atrophied capability) that could reverse with new military
 *   innovations. The constraint is classified as a piton because its primary
 *   function (preventing total war) has atrophied due to technological
 *   shifts, but the perception of its persistence remains, maintained by
 *   institutional inertia and rhetorical performance. Beneficiaries are
 *   states investing in destabilizing technologies, and victims are the
 *   global population.
 *
 * KEY AGENTS:
 *   - states_investing_in_destabilizing_technologies: Primary beneficiary (powerful/mobile) — benefits from the fluid perception of reachability
 *   - global_population: Primary victim (powerless/trapped) — bears the ultimate risk if reachability increases
 *   - nuclear_deterrence_theorists: Analytical observer (analytical/analytical) — studies the dynamics of reachability
 *   - arms_control_advocates: Excluded (organized/constrained) — would object to increasing reachability but are not in the direct conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.4).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Total War Reachability Boundary").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'd44d16c3-ddde-4356-a76a-aaf0213ac894').
narrative_ontology:cs_kernel_codification('d44d16c3-ddde-4356-a76a-aaf0213ac894', distributed).
narrative_ontology:cs_authority_grounding('d44d16c3-ddde-4356-a76a-aaf0213ac894', practice).
narrative_ontology:cs_interpretation_layer_present('d44d16c3-ddde-4356-a76a-aaf0213ac894').
narrative_ontology:cs_reading_relation('d44d16c3-ddde-4356-a76a-aaf0213ac894', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d44d16c3-ddde-4356-a76a-aaf0213ac894', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('d44d16c3-ddde-4356-a76a-aaf0213ac894', foundational, strategic_stability_is_technologically_contingent).
narrative_ontology:cs_axiom_status(strategic_stability_is_technologically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('d44d16c3-ddde-4356-a76a-aaf0213ac894', strategic_stability_is_technologically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('d44d16c3-ddde-4356-a76a-aaf0213ac894', foundational, total_war_reachability_is_reversible).
narrative_ontology:cs_axiom_status(total_war_reachability_is_reversible, holdable).
narrative_ontology:cs_axiom_grounding('d44d16c3-ddde-4356-a76a-aaf0213ac894', total_war_reachability_is_reversible, empirically_contingent).
narrative_ontology:cs_reference_frame('d44d16c3-ddde-4356-a76a-aaf0213ac894', post_cold_war_technological_equilibrium).
narrative_ontology:cs_drift_state('d44d16c3-ddde-4356-a76a-aaf0213ac894', contemporary_emerging_technologies_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d44d16c3-ddde-4356-a76a-aaf0213ac894', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the perception that total war is less reachable, as it creates a window for conventional or limited nuclear aggression. They actively pursue technologies (e.g., hypersonic missiles, advanced missile defense) that could shift the strategic balance and make total war more 'winnable' or survivable, thereby reversing the current contraction of reachability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies, beneficiary,
    powerful, generational, mobile, global).

% The global population bears the ultimate cost if the reachability boundary shifts and deterrence fails. They are victims of the underlying risk, even if the immediate extraction is diffuse. Their existence is threatened by any increase in the perceived feasibility of total war.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_population, payer,
    powerless, immediate, trapped, universal).

% These analysts study the conditions of strategic stability and the factors influencing the perceived reachability of total war. They observe technological developments and their potential impact on deterrence theory, often advocating for arms control or stability measures.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_deterrence_theorists, observer,
    analytical, generational, analytical, global).

% These groups argue for international treaties and norms to limit the development and proliferation of destabilizing technologies. They are often excluded from the direct decision-making processes of states investing in such technologies, but their advocacy aims to influence the perception and reality of total war reachability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates states around a shared (though contested) understanding of the current technological limits on total war, which underpins existing deterrence postures.
% TRANSFER_FUNCTION: The constraint transfers a sense of relative security (or false security) to populations, while transferring strategic advantage and research funding to states developing technologies that could alter reachability.
% ABSENT_VOICES: Future generations and populations directly threatened by emerging technologies are absent from the strategic calculus that might reverse reachability. They would argue for a more robust and permanent contraction of total war options.
% DISAPPEARANCE_RATIONALE: If the perception of technology-dependent reachability vanished, and total war was universally seen as either permanently impossible or immediately imminent, global strategic postures would fundamentally reorganize. Investment in destabilizing technologies would either cease or accelerate dramatically, and deterrence theory would require a complete overhaul.
% FOUNDING_PROBLEM: The problem of managing the existential threat of total war in an era of rapidly evolving military technology, where perceived strategic boundaries are fluid.
% FOUNDING_PROBLEM_CORROBORATION: Strategic analysts and defense planners across multiple states corroborate that managing technological shifts in relation to total war reachability is an ongoing, live problem. This is evidenced by continuous investment in military R&D and ongoing debates about strategic stability.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the diffuse costs of maintaining a state of readiness and the implicit transfer of risk to populations. Suppression (0.6) is significant, as the discourse around total war reachability is heavily managed by state actors and defense establishments. The high theater ratio (0.7) indicates that much of the current strategic posturing and rhetoric performs the 'unreachability' of total war, even as underlying technological developments suggest otherwise. Accessibility collapse is low (0.3) because technological innovation constantly creates new pathways to total war, preventing a complete collapse of alternatives. Resistance is low (0.2) because the diffuse nature of the threat and the complexity of the technological landscape make organized resistance difficult.
 *
 * PERSPECTIVAL GAP:
 *   States investing in destabilizing technologies perceive this constraint as a strategic opportunity, allowing them to gain an edge by shifting the technological equilibrium. The global population, however, experiences it as a persistent, existential threat that is being theatrically managed rather than genuinely resolved. Nuclear deterrence theorists view it as a dynamic, evolving problem requiring constant re-evaluation, while arms control advocates see it as a dangerous illusion that must be actively countered.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are beneficiaries because the ambiguity of reachability allows them to pursue strategic advantages. The global population is a victim because they bear the ultimate risk of miscalculation or technological breakthrough. Nuclear deterrence theorists are observers, analyzing the dynamics without direct benefit or cost. Arms control advocates are excluded, as their proposals often run counter to the interests of the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies the constraint as a piton, suggesting that the mandate to prevent total war has atrophied in its effectiveness due to technological shifts, but the constraint persists through institutional inertia and performative maintenance. This prevents mislabeling it as a stable 'rope' (coordination) or 'mountain' (fixed natural law), which would obscure the underlying technological contingency and the potential for reversal. The piton classification highlights the gap between the claimed stability of deterrence and the dynamic reality of technological change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_reversal_timeline,
    'What is the actual timeline and feasibility of emerging technologies (e.g., AI in command and control, hypersonic weapons, advanced missile defense) reversing the current contraction of total war reachability?',
    'Ongoing military R&D assessments, wargaming simulations, and independent expert analysis of technological breakthroughs and their strategic implications.',
    'If reversal is imminent and highly feasible, the constraint''s extractiveness and suppression would be re-evaluated upwards, reflecting a more immediate and severe threat, potentially reclassifying it towards a snare or tangled rope. If reversal is distant or unlikely, the piton classification would be strengthened, emphasizing the theatricality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reversal_timeline, empirical, 'Uncertainty regarding the pace and impact of technological advancements on total war reachability.').

omega_variable(
    deterrence_stability_vs_technological_drift,
    'Is the perceived stability of nuclear deterrence a robust, independent equilibrium, or is it merely a temporary artifact of a specific technological plateau that is now eroding?',
    'Historical analysis of past strategic shifts, comparative studies of deterrence in different technological eras, and theoretical modeling of strategic stability under conditions of rapid technological change.',
    'If deterrence stability is found to be highly dependent on technological stasis, its classification as a rope (in the ''dropping_reading'' sibling) would be undermined, lending more weight to this reading''s piton classification. If it proves robust, this reading''s piton claim would be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_technological_drift, conceptual, 'Ambiguity regarding the fundamental nature of deterrence stability in the face of technological drift.').

omega_variable(
    mandatrophy_vs_active_suppression,
    'To what extent is the current ''unreachability'' of total war due to genuine atrophy of capability (mandatrophy) versus active, though subtle, suppression of escalatory pathways by state actors?',
    'Detailed analysis of state military doctrines, command and control protocols, and crisis management procedures to identify explicit and implicit suppression mechanisms versus genuine loss of capability or will.',
    'If active suppression is found to be dominant, the constraint''s suppression metric would be higher, and its classification might shift towards a tangled rope or snare, as it would imply more deliberate, extractive control rather than mere atrophy. If atrophy is dominant, the piton classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_vs_active_suppression, empirical, 'Distinguishing between genuine atrophy of total war capability and active, subtle suppression of escalatory pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.6).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.63).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.66).
narrative_ontology:measurement(tota_tr_t2020, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2020, 0.68).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(tota_be_t2020, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(tota_su_t2020, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. This 'contingent reachability' reading emphasizes the technology-dependent and potentially reversible nature of the current strategic equilibrium, contrasting with the 'contraction_reading' (permanent unfeasibility) and the 'dropping_reading' (reduced probability, stable deterrence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
