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
 *   Westphalian system, where a state's territorial authority is not absolute
 *   but exists on a spectrum from full (e.g., Western democracies) to nominal
 *   (e.g., 'failed states'). The legitimacy of external intervention is
 *   calibrated to perceived capacity deficits. This reading emerged in
 *   response to challenges posed by state collapse and humanitarian crises,
 *   but it creates a de facto hierarchical international system. This is one
 *   reading of the 'westphalia_sovereignty' kernel, distinct from
 *   'absolute_non_intervention' and 'conditional_responsibility'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.75).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '4f217b25-de06-474e-beac-b3743d17bdfe').
narrative_ontology:cs_kernel_codification('4f217b25-de06-474e-beac-b3743d17bdfe', distributed).
narrative_ontology:cs_authority_grounding('4f217b25-de06-474e-beac-b3743d17bdfe', extraction).
narrative_ontology:cs_interpretation_layer_present('4f217b25-de06-474e-beac-b3743d17bdfe').
narrative_ontology:cs_reading_relation('4f217b25-de06-474e-beac-b3743d17bdfe', westphalia_sovereignty__absolute_non_intervention, influences).
narrative_ontology:cs_reading_relation('4f217b25-de06-474e-beac-b3743d17bdfe', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('4f217b25-de06-474e-beac-b3743d17bdfe', foundational, state_capacity_is_measurable_and_gradable).
narrative_ontology:cs_axiom_status(state_capacity_is_measurable_and_gradable, holdable).
narrative_ontology:cs_axiom_grounding('4f217b25-de06-474e-beac-b3743d17bdfe', state_capacity_is_measurable_and_gradable, empirically_contingent).
narrative_ontology:cs_axiom('4f217b25-de06-474e-beac-b3743d17bdfe', foundational, intervention_legitimacy_scales_with_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_scales_with_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('4f217b25-de06-474e-beac-b3743d17bdfe', intervention_legitimacy_scales_with_capacity_deficit, instrumental).
narrative_ontology:cs_reference_frame('4f217b25-de06-474e-beac-b3743d17bdfe', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('4f217b25-de06-474e-beac-b3743d17bdfe', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4f217b25-de06-474e-beac-b3743d17bdfe', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, powerful_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, non_state_actors_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International organizations and expert bodies that develop and apply metrics to assess state capacity. Their assessments legitimize or delegitimize interventions, creating a de facto hierarchy of states. They benefit from the demand for their expertise and the influence their evaluations wield.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% States with high capacity that gain legitimacy for interventions in weaker states, often under the guise of humanitarian aid, stabilization, or counter-terrorism. They benefit from expanded spheres of influence and reduced international legal barriers to projecting power.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_states, beneficiary,
    institutional, generational, mobile, global).

% States with low capacity, often labeled 'failed' or 'fragile,' that are subject to external oversight, conditionality, and potential intervention. They bear the cost of diminished sovereignty, paternalistic governance, and loss of self-determination, with few viable options to resist or exit the system.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% Academics and legal experts who analyze the evolution and application of sovereignty concepts. They document the shift from absolute to graded sovereignty and its implications for international law and state practice.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% Local populations, rebel groups, or civil society organizations within weak states who experience the direct effects of external interventions. Their agency is often sidelined in favor of state-centric capacity building, and they bear the costs of instability or imposed solutions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, non_state_actors_in_weak_states, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international actors to coordinate responses to state fragility and instability, theoretically preventing humanitarian crises or regional spillover by allowing calibrated external engagement.
% TRANSFER_FUNCTION: Transfers authority and decision-making power from weak states to international bodies and powerful states, along with resources and mandates for intervention, in exchange for (the promise of) stability and capacity building.
% ABSENT_VOICES: Many weak states and their populations, particularly those most affected by interventions, are often excluded from the processes that define 'capacity' and legitimize external action. They would argue for self-determination and against paternalistic oversight.
% DISAPPEARANCE_RATIONALE: If the graded sovereignty doctrine vanished, the international system would revert to a more rigid interpretation of non-intervention, making it harder for powerful states to justify interventions in weak states. This would force a re-evaluation of international responsibility and potentially lead to more unresolved internal conflicts or new justifications for intervention.
% FOUNDING_PROBLEM: The traditional Westphalian model of absolute sovereignty proved inadequate in addressing humanitarian crises, state collapse, and cross-border threats emanating from 'failed states,' leading to calls for a more flexible approach to intervention.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, humanitarian organizations, and powerful states consistently attest to the ongoing challenges posed by state fragility and the need for mechanisms to address them. Weak states, while often critical of the doctrine's application, generally acknowledge the underlying problems of instability and lack of capacity.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it purports to coordinate international responses to instability (a genuine coordination function) but simultaneously enables asymmetric extraction of sovereignty and decision-making power from weaker states by powerful ones. Extractiveness (0.65) is substantial due to the loss of self-determination and the imposition of external agendas. Suppression (0.75) is high because weak states have limited options to resist interventions or challenge capacity assessments without further jeopardizing their international standing. The theater ratio (0.4) reflects that while some capacity-building efforts are genuine, a significant portion of the 'coordination' is performative justification for power projection.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and international bodies perceive this as a necessary, benevolent coordination mechanism for global stability. Weak states and their populations often experience it as a form of neo-colonialism or paternalistic control that undermines their sovereignty and self-determination. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and capacity-evaluation authorities are beneficiaries, gaining influence and legitimacy for their actions (low directionality). Weak states and non-state actors within them are targets, bearing the costs of diminished sovereignty and external control (high directionality). The system is actively enforced through diplomatic pressure, conditionality, and military intervention, making it a Tangled Rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_assessment_objectivity,
    'Are the metrics and processes used by capacity-evaluation authorities truly objective and neutral, or do they reflect the biases and interests of powerful states?',
    'Independent audits of capacity assessment methodologies, analysis of correlation between assessment outcomes and geopolitical interests of powerful states, and inclusion of diverse, non-Western perspectives in metric development.',
    'If assessments are biased, the ''coordination'' function is largely cover for extraction, increasing the effective extractiveness and shifting the classification closer to a Snare. If objective, the coordination aspect is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_objectivity, empirical, 'Objectivity of state capacity metrics and their potential for political manipulation.').

omega_variable(
    intervention_efficacy_vs_sovereignty_cost,
    'Does external intervention, legitimized by graded sovereignty, genuinely improve state capacity and stability in weak states, or does it primarily serve the interests of intervening powers while undermining local agency?',
    'Longitudinal studies comparing outcomes in intervened vs. non-intervened weak states, disaggregated analysis of local vs. external priorities in capacity-building programs, and evaluation of local ownership of development processes.',
    'If interventions consistently fail to build capacity or exacerbate instability, the coordination claim is weakened, and the constraint''s extractive nature is amplified. If effective, the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_vs_sovereignty_cost, empirical, 'Effectiveness of interventions in building state capacity versus their cost to sovereignty.').

omega_variable(
    sovereignty_as_scalar_vs_categorical,
    'Is sovereignty fundamentally a scalar capacity (as this reading claims) or a categorical status (as absolute non-intervention claims)?',
    'Conceptual analysis of international legal precedent, state practice, and philosophical arguments regarding statehood and self-determination. This is a foundational conceptual debate.',
    'If sovereignty is fundamentally categorical, this ''graded'' reading is a conceptual distortion that legitimizes extraction. If scalar, this reading provides a more accurate, albeit potentially problematic, description of international reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_as_scalar_vs_categorical, conceptual, 'Conceptual debate over the nature of sovereignty itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t1998, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(west_tr_t2006, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2006, 0.33).
narrative_ontology:measurement(west_tr_t2014, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(west_be_t1998, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(west_be_t2006, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(west_be_t2014, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2024, 0.65).

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
% This constraint is the 'graded_sovereignty' reading of the 'westphalia_sovereignty' kernel, which also includes 'absolute_non_intervention' and 'conditional_responsibility' readings. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
