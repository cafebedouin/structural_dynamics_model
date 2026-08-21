% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'graded sovereignty' reading of the
 *   Westphalian system, where state authority is viewed as existing on a
 *   spectrum from full capacity to nominal capacity, and the legitimacy of
 *   external intervention is calibrated to these capacity deficits. This
 *   reading emerged particularly in the post-Cold War era to address 'failed
 *   states' and humanitarian crises. It creates a de facto hierarchical state
 *   system, with powerful states and international bodies acting as
 *   evaluators and potential interveners. The constraint is presented as a
 *   'tangled_rope' because it purports to coordinate international responses
 *   to state failure while simultaneously enabling asymmetric extraction of
 *   autonomy from weaker states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.75).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.8).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '21607d60-cb4e-408e-990d-e3c7fc74df48').
narrative_ontology:cs_kernel_codification('21607d60-cb4e-408e-990d-e3c7fc74df48', formalized).
narrative_ontology:cs_authority_grounding('21607d60-cb4e-408e-990d-e3c7fc74df48', extraction).
narrative_ontology:cs_interpretation_layer_present('21607d60-cb4e-408e-990d-e3c7fc74df48').
narrative_ontology:cs_reading_relation('21607d60-cb4e-408e-990d-e3c7fc74df48', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('21607d60-cb4e-408e-990d-e3c7fc74df48', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('21607d60-cb4e-408e-990d-e3c7fc74df48', foundational, state_capacity_is_scalar).
narrative_ontology:cs_axiom_status(state_capacity_is_scalar, holdable).
narrative_ontology:cs_axiom_grounding('21607d60-cb4e-408e-990d-e3c7fc74df48', state_capacity_is_scalar, empirically_contingent).
narrative_ontology:cs_axiom('21607d60-cb4e-408e-990d-e3c7fc74df48', foundational, intervention_legitimacy_from_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_from_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('21607d60-cb4e-408e-990d-e3c7fc74df48', intervention_legitimacy_from_capacity_deficit, instrumental).
narrative_ontology:cs_reference_frame('21607d60-cb4e-408e-990d-e3c7fc74df48', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('21607d60-cb4e-408e-990d-e3c7fc74df48', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21607d60-cb4e-408e-990d-e3c7fc74df48', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, powerful_intervening_states).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are international bodies, expert panels, or powerful state departments that develop and apply metrics to assess the capacity of states. They define the 'grades' of sovereignty and legitimize interventions based on their assessments. They benefit from their epistemic authority and influence over international policy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, analytical, global).

% These states gain a legitimate basis for intervention, oversight, or influence in the domestic affairs of weaker states, often under the guise of capacity building or humanitarian protection. They benefit from expanded geopolitical leverage and access to resources or strategic positions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_intervening_states, beneficiary,
    institutional, generational, arbitrage, global).

% These states are subject to external scrutiny, capacity evaluations, and potential intervention. Their autonomy is curtailed, and they bear the costs of complying with external demands or facing the consequences of non-compliance, including loss of full sovereign control. Exit means risking further marginalization or intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% These populations live under the direct consequences of their state's graded sovereignty, experiencing either the (claimed) benefits of external intervention or the costs of reduced national autonomy, paternalistic oversight, and potential instability. Their agency in determining their own governance is often diminished.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, payer,
    powerless, biographical, trapped, national).

% Organizations like the UN or regional bodies often provide the institutional framework for capacity evaluations and interventions. They benefit from an expanded mandate and increased relevance in global governance, even if their actions are sometimes constrained by powerful member states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_organizations, beneficiary,
    institutional, generational, constrained, global).

% These are states, scholars, and activists who uphold the traditional Westphalian principle of absolute territorial inviolability. They are largely excluded from the decision-making processes that legitimize graded sovereignty and would object to any external judgment of state capacity as a basis for intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international order by differentiating state responsibilities and legitimizing external action in cases of state capacity deficit, aiming to manage global security and humanitarian crises.
% TRANSFER_FUNCTION: Transfers aspects of autonomy, decision-making power, and resource control from weak states to international bodies and powerful states, in exchange for (claimed) stability, security, or humanitarian protection.
% ABSENT_VOICES: Advocates for absolute non-intervention are structurally excluded from the discourse that legitimizes graded sovereignty. They would argue that any external judgment of state capacity as a basis for intervention is a violation of fundamental sovereign equality.
% DISAPPEARANCE_RATIONALE: If the graded sovereignty doctrine and its enforcement mechanisms vanished overnight, the legitimacy of interventions in fragile states would become highly contested, leading to either more unilateral and potentially illegal actions by powerful states, or a paralysis of international response to state failure and humanitarian crises. This would fundamentally alter the landscape of global governance and international relations.
% FOUNDING_PROBLEM: The challenge of managing international security and humanitarian crises in states unable or unwilling to govern effectively, without resorting to purely unilateral or illegal interventions, particularly in the post-Cold War era of increased state fragility.
% FOUNDING_PROBLEM_CORROBORATION: International organizations (e.g., UN, regional bodies) and many powerful states attest to the ongoing problem of state fragility and humanitarian crises. While the *solution* (graded sovereignty) is contested, the underlying problem is widely acknowledged by a broad range of international actors and independent analyses.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant loss of autonomy and decision-making power imposed on weak states. Suppression (0.80) is high due to the limited alternatives for weak states to resist external oversight or intervention without facing severe consequences. The theater ratio (0.40) indicates that while there's a genuine (claimed) function of promoting stability and human security, a substantial portion of the activity involves legitimizing the power dynamics and justifying interventions that may serve the interests of powerful actors. The increasing trend in metrics over the interval reflects the growing formalization and application of this doctrine, leading to more pronounced extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   Powerful intervening states and capacity-evaluation authorities perceive this doctrine as a necessary coordination mechanism for global stability and humanitarian action. From their seat, it's a rational response to complex challenges. In contrast, weak states and their populations experience it as a highly extractive and suppressive system that curtails their sovereignty and perpetuates dependency, often leading to resentment and resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluation authorities and powerful intervening states are clear beneficiaries, gaining legitimacy and influence. International organizations also benefit from an expanded mandate. Weak states and their populations are the primary targets, bearing the costs of reduced autonomy and potential intervention. Advocates for absolute non-intervention are structurally excluded, their perspective actively suppressed by the prevailing discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of managing state failure and humanitarian crises remains live. However, the 'graded sovereignty' solution is contested, with critics arguing that its function has drifted from genuine coordination and capacity building to a mechanism for paternalistic oversight and geopolitical extraction. The persistence of the constraint, despite its high extractiveness and resistance, suggests that the benefits to the agenda-setters and powerful beneficiaries outweigh the costs of maintaining it, preventing a resolution of its mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''graded_sovereignty'' reading of the ''westphalia_sovereignty'' kernel?',
    'Analysis of historical and contemporary international legal and political discourse to confirm the distinct conceptualization and application of state capacity as a scalar for intervention legitimacy.',
    'If misidentified, the analysis of the kernel''s contestation and the relationships between sibling readings would be flawed, leading to incorrect classification of the broader Westphalian system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the Westphalian kernel.').

omega_variable(
    capacity_metrics_objectivity,
    'Are the metrics and processes used to evaluate state capacity truly objective and universally applicable, or do they inherently reflect the biases and interests of powerful states and Western-centric norms?',
    'Independent, cross-cultural studies of state capacity metrics, including participatory research with ''weak states'' to assess their validity and perceived fairness, and analysis of intervention patterns for selective application.',
    'If metrics are found to be biased, the constraint''s claimed coordination function is undermined, and its extractiveness and suppression would be re-evaluated as higher, potentially reclassifying it closer to a Snare. If objective, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metrics_objectivity, empirical, 'Assesses the objectivity of state capacity evaluation.').

omega_variable(
    intervention_efficacy_and_dependency,
    'Does intervention based on graded sovereignty genuinely improve state capacity and human security in the long term, or does it perpetuate dependency, undermine local agency, and create new forms of instability?',
    'Longitudinal studies of states that have undergone interventions based on capacity deficits, comparing their post-intervention trajectories with control groups, focusing on self-sufficiency, governance quality, and human development indicators.',
    'If interventions are found to perpetuate dependency, the constraint''s claimed coordination benefits are significantly reduced, increasing its effective extraction and potentially shifting its classification towards a Snare. If effective, the coordination function is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_and_dependency, empirical, 'Evaluates the long-term impact of interventions based on graded sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t1997, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(west_tr_t2004, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(west_tr_t2018, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(west_be_t1997, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1997, 0.62).
narrative_ontology:measurement(west_be_t2004, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2004, 0.68).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(west_be_t2018, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(west_su_t1997, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1997, 0.68).
narrative_ontology:measurement(west_su_t2004, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2004, 0.74).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2011, 0.78).
narrative_ontology:measurement(west_su_t2018, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2018, 0.79).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, r2p_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel. It is structurally distinct from the 'absolute_non_intervention' and 'conditional_responsibility' readings, which are modeled as separate constraints due to differing epsilon values and structural properties. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
