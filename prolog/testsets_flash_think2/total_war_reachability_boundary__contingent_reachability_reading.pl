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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary (Contingent Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint is the 'contingent reachability' reading of the 'total
 *   war reachability boundary' kernel. It posits that the current contraction
 *   of strategic space, which makes total war difficult, is a temporary and
 *   technology-dependent 'scaffold'. The current state of this contraction is
 *   described as a 'piton' (atrophied capability), meaning its effectiveness
 *   is degrading due to technological shifts. This reading contrasts with the
 *   'contraction_reading' (which sees the contraction as permanent) and the
 *   'dropping_reading' (which sees it as a stable coordination equilibrium).
 *   The constraint's claimed type is Scaffold because its existence and
 *   efficacy are temporary and contingent on the technological equilibrium,
 *   with a 'sunset clause' implicitly tied to technological change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.5).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary (Contingent Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence_theory").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '50542d03-a15a-4be1-8cc9-7d060edce2fb').
narrative_ontology:cs_kernel_codification('50542d03-a15a-4be1-8cc9-7d060edce2fb', implicit).
narrative_ontology:cs_authority_grounding('50542d03-a15a-4be1-8cc9-7d060edce2fb', self_enforcing).
narrative_ontology:cs_reading_relation('50542d03-a15a-4be1-8cc9-7d060edce2fb', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('50542d03-a15a-4be1-8cc9-7d060edce2fb', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('50542d03-a15a-4be1-8cc9-7d060edce2fb', foundational, technological_determinism_of_strategic_space).
narrative_ontology:cs_axiom_status(technological_determinism_of_strategic_space, holdable).
narrative_ontology:cs_axiom_grounding('50542d03-a15a-4be1-8cc9-7d060edce2fb', technological_determinism_of_strategic_space, empirically_contingent).
narrative_ontology:cs_axiom('50542d03-a15a-4be1-8cc9-7d060edce2fb', secondary, strategic_stability_is_dynamic_equilibrium).
narrative_ontology:cs_axiom_status(strategic_stability_is_dynamic_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('50542d03-a15a-4be1-8cc9-7d060edce2fb', strategic_stability_is_dynamic_equilibrium, empirically_contingent).
narrative_ontology:cs_reference_frame('50542d03-a15a-4be1-8cc9-7d060edce2fb', post_cold_war_strategic_equilibrium).
narrative_ontology:cs_drift_state('50542d03-a15a-4be1-8cc9-7d060edce2fb', emerging_technologies_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50542d03-a15a-4be1-8cc9-7d060edce2fb', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, international_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states actively develop and deploy technologies (e.g., hypersonic missiles, advanced cyber warfare, space-based weapons) that could erode the current strategic stability and make total war more 'reachable'. They benefit from the potential for strategic advantage if the boundary shifts.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies, agenda_setter,
    powerful, generational, mobile, global).

% Bear the existential risk and psychological burden of a potentially eroding total war boundary. They pay the cost of increased defense spending and live under the shadow of potential catastrophic conflict if deterrence fails or the boundary becomes too permeable.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_populations, payer,
    powerless, generational, trapped, universal).

% Analyze the dynamics of strategic stability, the impact of new technologies, and the conditions under which total war might become more or less likely. They provide intellectual frameworks for understanding the constraint but do not directly enforce or benefit from it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_theorists, observer,
    analytical, biographical, analytical, global).

% Benefit from the existence of the total war boundary, as it provides a mandate for arms control, non-proliferation, and conflict resolution efforts. Their legitimacy and function are tied to managing the risks associated with the boundary, even as it erodes.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, international_organizations, beneficiary,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates states to avoid total war by making it difficult and costly, but this coordination is fragile and dependent on the prevailing technological equilibrium.
% TRANSFER_FUNCTION: Transfers the burden of maintaining strategic stability (and the risk of its failure) to global populations, while transferring potential strategic advantage to states developing destabilizing technologies.
% ABSENT_VOICES: Future generations, who would bear the full cost of a failed boundary and have no say in the technological and strategic choices made today.
% DISAPPEARANCE_RATIONALE: If the total war reachability boundary vanished (i.e., total war became easily winnable or unavoidable due to technological shifts), global strategic calculations would fundamentally reorganize, likely leading to catastrophic outcomes and a complete reordering of international relations.
% FOUNDING_PROBLEM: Preventing total war in an era of advanced weaponry, particularly nuclear weapons, by establishing a strategic boundary that makes such conflict infeasible or too costly.
% FOUNDING_PROBLEM_CORROBORATION: International security experts, historical analysis of nuclear crises, and ongoing strategic arms race dynamics all corroborate that the problem of preventing total war remains live, albeit with changing technological parameters.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.5, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because its function (preventing total war) is temporary and dependent on a specific technological equilibrium, which is subject to change. The 'piton' aspect of the current contraction is reflected in the moderate-high theater_ratio (0.6), indicating that the boundary's effectiveness is atrophying, and its maintenance is becoming more performative or inertial rather than robustly functional. Extractiveness is moderate (0.5) as states invest in destabilizing technologies, creating a dynamic where some benefit from the erosion of the boundary. Suppression is moderate (0.6) but shows a declining trend, reflecting the weakening of the boundary's ability to prevent total war. The accessibility_collapse is moderate (0.4), indicating that total war is difficult but not impossible, and could become easier.
 *
 * PERSPECTIVAL GAP:
 *   Different actors perceive the stability and permanence of the total war boundary differently. States investing in destabilizing technologies might view the boundary as a challenge to be overcome, while global populations perceive it as a critical, but eroding, safeguard. Deterrence theorists debate its true nature (mountain, rope, or scaffold), reflecting the core ambiguity of the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are the agenda-setters and beneficiaries, as they actively shape the boundary and stand to gain strategic advantage from its erosion. Global populations are the payers, bearing the risk and cost of this strategic instability. International organizations benefit from the existence of the boundary, as it provides their mandate, even as its stability is questioned.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Scaffold, with a high theater_ratio and declining suppression, prevents mislabeling the current strategic contraction as a stable Rope or Mountain. It highlights that the mandate of preventing total war, while still live, is being met by an increasingly atrophied and contingent mechanism, rather than a robust and self-sustaining one. The 'piton' description of the current contraction emphasizes the inertial persistence of a degrading function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_reachability_boundary,
    'Is the total war reachability boundary a permanent ''Mountain'' of strategic reality, a stable ''Rope'' of coordination, or a contingent ''Scaffold'' dependent on technology?',
    'Long-term historical analysis of strategic stability across multiple technological paradigms, and empirical observation of whether new technologies fundamentally alter the feasibility of total war.',
    'If resolved as a Mountain, the constraint is fixed and unchangeable. If a Rope, it''s a stable coordination equilibrium. If a Scaffold, its temporary nature and dependence on technology are confirmed, implying a need for active management or adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_reachability_boundary, conceptual, 'Ambiguity regarding the fundamental nature of the total war reachability boundary.').

omega_variable(
    impact_of_emerging_technologies,
    'Will emerging technologies (e.g., AI in command and control, hypersonics, space warfare) truly reverse the contraction of strategic space, or merely shift the form of deterrence?',
    'Empirical observation of future strategic conflicts, arms races, and the actual operational impact of these technologies on escalation control and crisis stability.',
    'If technologies enable ''winnable'' total war, the Scaffold collapses, leading to catastrophic outcomes. If they only shift deterrence dynamics without making total war feasible, the Scaffold might persist in a modified form, or transition to a different type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_of_emerging_technologies, empirical, 'Uncertainty about the transformative power of future military technologies on strategic stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1990, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1990, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.53).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.56).
narrative_ontology:measurement(tota_tr_t2020, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2020, 0.58).
narrative_ontology:measurement(tota_tr_t2030, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2030, 0.6).
narrative_ontology:measurement(tota_tr_t2040, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2040, 0.62).

% Extraction over time
narrative_ontology:measurement(tota_be_t1990, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(tota_be_t2020, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(tota_be_t2030, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2030, 0.5).
narrative_ontology:measurement(tota_be_t2040, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2040, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1990, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(tota_su_t2020, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(tota_su_t2030, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2030, 0.6).
narrative_ontology:measurement(tota_su_t2040, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2040, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel, focusing on its contingent and technology-dependent nature. It is linked to 'contraction_reading' and 'dropping_reading' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
