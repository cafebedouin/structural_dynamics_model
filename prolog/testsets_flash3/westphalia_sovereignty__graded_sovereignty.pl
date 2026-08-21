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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Graded Sovereignty Doctrine (Intervention based on State Capacity)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint represents the 'graded sovereignty' reading of the
 *   Westphalian kernel, where a state's territorial authority is seen as a
 *   scalar quantity, ranging from full (e.g., Western democracies) to nominal
 *   (e.g., 'failed states'). The legitimacy of external intervention is
 *   calibrated to perceived deficits in a state's capacity to govern or
 *   protect its population. This reading creates a de facto hierarchical
 *   international system, with powerful states and capacity-evaluation
 *   authorities as beneficiaries, and weak states as victims subject to
 *   paternalistic oversight and potential intervention. The claimed type is
 *   'tangled_rope' because it purports to coordinate international responses
 *   to crises while simultaneously enabling asymmetric extraction of
 *   sovereignty from weaker states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.75).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty Doctrine (Intervention based on State Capacity)").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '35cccdc3-215e-43e7-9d3e-70fbe0e7a327').
narrative_ontology:cs_kernel_codification('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', formalized).
narrative_ontology:cs_authority_grounding('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', extraction).
narrative_ontology:cs_interpretation_layer_present('35cccdc3-215e-43e7-9d3e-70fbe0e7a327').
narrative_ontology:cs_reading_relation('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', westphalia_sovereignty__absolute_non_intervention, influences).
narrative_ontology:cs_reading_relation('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', foundational, sovereignty_is_scalar_capacity).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_capacity, holdable).
narrative_ontology:cs_axiom_grounding('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', sovereignty_is_scalar_capacity, empirically_contingent).
narrative_ontology:cs_axiom('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', foundational, intervention_legitimacy_calibrated_to_capacity_deficits).
narrative_ontology:cs_axiom_status(intervention_legitimacy_calibrated_to_capacity_deficits, holdable).
narrative_ontology:cs_axiom_grounding('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', intervention_legitimacy_calibrated_to_capacity_deficits, instrumental).
narrative_ontology:cs_reference_frame('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', contemporary_multipolar_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('35cccdc3-215e-43e7-9d3e-70fbe0e7a327', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, powerful_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International bodies, NGOs, and think tanks that develop and apply metrics to assess state capacity (e.g., governance indicators, human development indices). Their assessments provide the 'objective' basis for intervention legitimacy, granting them significant influence over international policy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, constrained, global).

% States with high capacity that often lead intervention efforts. They benefit from the legitimization of intervention in weak states, allowing them to pursue strategic interests under the guise of humanitarianism or state-building. They also define and enforce the capacity metrics.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_states, beneficiary,
    institutional, generational, arbitrage, global).

% States with low capacity, often post-conflict or developing nations, that are subject to external oversight, conditionality, and potential intervention. They bear the costs of paternalistic governance, loss of self-determination, and the imposition of external models of statehood. Their 'sovereignty' is conditional on meeting externally defined benchmarks.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% The citizens of weak states, who may benefit from interventions that improve security or governance, but also suffer from the instability, violence, and loss of agency that can accompany external interference. Their well-being is often the stated justification for intervention, but their voice in the process is minimal.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, beneficiary).

% Scholars, activists, and some states who argue for strict adherence to territorial inviolability, regardless of internal conditions. Their arguments are often marginalized in discussions dominated by capacity-based intervention rationales.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international actors to coordinate responses to state fragility and humanitarian crises, by calibrating intervention to a state's demonstrated capacity to govern and protect its population.
% TRANSFER_FUNCTION: Transfers legitimacy for external intervention from the principle of absolute state sovereignty to a conditional, capacity-based assessment, effectively transferring decision-making power over weak states' internal affairs to external actors.
% ABSENT_VOICES: The voices of weak states' populations, particularly those who might prefer self-determination and internal solutions over externally imposed 'capacity building' or intervention, are largely absent from the formulation and application of graded sovereignty doctrines. Advocates for strict non-intervention are also marginalized.
% DISAPPEARANCE_RATIONALE: If the graded sovereignty doctrine vanished, the international system would lose a key justification for intervention in 'failed' or 'fragile' states. Powerful states would need new legitimizing narratives for their actions, and weak states might reclaim greater autonomy, leading to a significant rearrangement of international power dynamics and intervention practices.
% FOUNDING_PROBLEM: The perceived failure of the international system to respond effectively to humanitarian crises and state collapse in the post-Cold War era, where traditional notions of absolute sovereignty prevented intervention in states unable or unwilling to protect their own populations.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and international organizations continue to attest that the problem of state fragility and its humanitarian consequences is live, citing ongoing crises. Critics from weak states and non-interventionist scholars argue that while the problem is real, the graded sovereignty solution often exacerbates it or serves as a pretext for other interests.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because weak states lose significant autonomy and decision-making power, effectively paying a 'sovereignty tax' for their perceived fragility. Suppression is also high (0.75) as the doctrine actively legitimizes external interference and limits the options for weak states to resist such interventions without further delegitimizing themselves. The theater ratio is moderate (0.20) because while there is genuine concern for humanitarian outcomes, the capacity metrics and intervention rationales can also serve as a cover for geopolitical interests. The increasing trend in extractiveness and suppression reflects the hardening of this doctrine over time, moving from a nascent idea to a more entrenched practice.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and capacity-evaluation authorities perceive this as a necessary and legitimate coordination mechanism for global stability and human protection. Weak states, however, experience it as an extractive and suppressive mechanism that undermines their self-determination and perpetuates dependency. The engine's classification will highlight this divergence, showing a 'tangled_rope' from the perspective of weak states, even if powerful states claim it as a 'rope' or 'scaffold'.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and capacity-evaluation authorities are clear beneficiaries, as the doctrine grants them expanded legitimate scope for action and influence (low directionality). Weak states and their populations are primary targets, as their sovereignty is made conditional and they bear the costs of external oversight and intervention (high directionality). Populations in weak states are also secondary beneficiaries if interventions genuinely improve their security or welfare, but this is often a contested outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_of_capacity_metrics,
    'Are the metrics used to evaluate state capacity truly objective and universally applicable, or do they reflect the biases and priorities of powerful states and Western-centric models of governance?',
    'Independent, decolonized research into alternative models of state capacity and governance, developed and validated by diverse global communities, particularly from the Global South.',
    'If metrics are found to be biased, the ''coordination'' function of graded sovereignty would be revealed as a cover for cultural and political imposition, shifting its classification closer to a ''snare''. If truly objective, it would strengthen the ''tangled_rope'' classification by validating its coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_capacity_metrics, conceptual, 'Assesses the epistemic grounding and potential bias of state capacity evaluation.').

omega_variable(
    intervention_effectiveness_vs_sovereignty_cost,
    'Does external intervention, justified by graded sovereignty, consistently lead to improved outcomes for populations in weak states, outweighing the costs of lost sovereignty and potential destabilization?',
    'Longitudinal, independent impact evaluations of interventions, comparing outcomes in intervened states with non-intervened but similarly fragile states, accounting for counterfactuals and unintended consequences.',
    'If interventions consistently fail or cause more harm, the ''coordination'' claim would be undermined, pushing the classification towards ''snare''. If consistently beneficial, it would reinforce the ''tangled_rope'' classification by validating its purported benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_effectiveness_vs_sovereignty_cost, empirical, 'Evaluates the actual impact of interventions against their stated goals and costs to sovereignty.').

omega_variable(
    mandate_creep_vs_original_intent,
    'Has the graded sovereignty doctrine experienced ''mandate creep,'' where its application has expanded beyond its original intent (e.g., humanitarian protection) to include broader geopolitical or economic interests?',
    'Historical analysis of intervention rationales and outcomes over time, comparing stated justifications with observed strategic interests and resource flows of intervening powers.',
    'Evidence of mandate creep would increase the perceived extractiveness and theater ratio, pushing the classification towards ''snare'' by revealing a hidden agenda. Absence of creep would support the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_creep_vs_original_intent, empirical, 'Examines whether the doctrine''s application has expanded beyond its initial humanitarian or state-building goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1998, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(west_tr_t2006, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(west_tr_t2014, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(west_be_t1998, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(west_be_t2014, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2014, 0.66).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(west_su_t1998, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(west_su_t2006, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(west_su_t2014, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Westphalia Sovereignty' kernel. It is linked to 'absolute_non_intervention' and 'conditional_responsibility' through the cs_structure.reading_relations field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
