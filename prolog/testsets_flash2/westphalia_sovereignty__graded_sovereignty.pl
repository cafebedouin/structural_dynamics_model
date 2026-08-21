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
 *   This constraint describes the 'graded sovereignty' reading of the
 *   Westphalian system, where a state's territorial authority is not absolute
 *   but exists on a spectrum, with the legitimacy of external intervention
 *   calibrated to perceived deficits in state capacity. This reading emerged
 *   in the post-Cold War era, driven by responses to 'failed states' and
 *   humanitarian crises. It creates a de facto hierarchical international
 *   system, where powerful states and international bodies act as evaluators
 *   and potential interveners, while weaker states are subject to
 *   paternalistic oversight.
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
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '9b75a81b-6898-41ec-96de-2eb92ae9ebf4').
narrative_ontology:cs_kernel_codification('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', formalized).
narrative_ontology:cs_authority_grounding('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', extraction).
narrative_ontology:cs_interpretation_layer_present('9b75a81b-6898-41ec-96de-2eb92ae9ebf4').
narrative_ontology:cs_reading_relation('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', westphalia_sovereignty__absolute_non_intervention, influences).
narrative_ontology:cs_reading_relation('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', foundational, state_capacity_is_scalar).
narrative_ontology:cs_axiom_status(state_capacity_is_scalar, holdable).
narrative_ontology:cs_axiom_grounding('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', state_capacity_is_scalar, empirically_contingent).
narrative_ontology:cs_axiom('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', foundational, intervention_legitimacy_is_capacity_dependent).
narrative_ontology:cs_axiom_status(intervention_legitimacy_is_capacity_dependent, holdable).
narrative_ontology:cs_axiom_grounding('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', intervention_legitimacy_is_capacity_dependent, instrumental).
narrative_ontology:cs_reference_frame('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', contemporary_multipolar_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9b75a81b-6898-41ec-96de-2eb92ae9ebf4', '').
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

% International organizations, think tanks, and academic bodies that develop and apply metrics for state capacity (e.g., governance indicators, human development indices). Their assessments provide the 'objective' basis for intervention legitimacy, enhancing their influence and funding.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, constrained, global).

% States with high capacity that benefit from the doctrine by legitimizing their interventions in weaker states, often under the guise of humanitarian aid, stabilization, or counter-terrorism. They gain geopolitical influence and access to resources without violating a strict non-intervention norm.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_states, beneficiary,
    institutional, generational, arbitrage, global).

% States whose sovereignty is deemed 'partial' or 'failed' based on capacity metrics. They are subject to paternalistic oversight, conditional aid, and potential intervention, losing effective control over their domestic affairs and resource allocation. Their resistance is often met with further capacity assessments.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% Ostensibly the beneficiaries of interventions aimed at improving state capacity and human security. However, they often bear the direct costs of intervention (displacement, violence, loss of self-determination) and may experience the imposition of external governance models that do not align with local needs or values.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, beneficiary).

% Analyze the evolution of sovereignty concepts and the legal implications of graded sovereignty. They document the shift from absolute non-intervention to capacity-based intervention, often critiquing its potential for neo-colonialism or its selective application.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international actors to coordinate responses to state fragility and humanitarian crises, theoretically preventing state collapse and protecting populations where domestic authorities are unable or unwilling.
% TRANSFER_FUNCTION: Transfers legitimacy for intervention from the principle of absolute state consent to a calculus of state capacity, effectively transferring decision-making power over domestic affairs from weak states to powerful international actors and capacity evaluators.
% ABSENT_VOICES: Many post-colonial states and Global South scholars would object, arguing that capacity metrics are culturally biased and that graded sovereignty perpetuates a neo-colonial hierarchy, undermining the principle of sovereign equality. Their voices are often marginalized in the institutions that define and apply these metrics.
% DISAPPEARANCE_RATIONALE: If the doctrine of graded sovereignty vanished, the legitimacy of many current interventions would collapse, powerful states would lose a key justification for their actions in weaker states, and the international system would face a renewed debate on the limits of non-intervention, potentially leading to a more rigid, or alternatively, a more chaotic, international order.
% FOUNDING_PROBLEM: The perceived failure of the international system to respond effectively to humanitarian crises and state collapse in the post-Cold War era, where strict non-intervention prevented action in situations of mass suffering.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and many international organizations attest that the problem of state fragility and its consequences (e.g., refugee flows, terrorism) remains live. Critics, including many weak states and Global South scholars, corroborate the existence of state fragility but contest whether graded sovereignty is a legitimate or effective solution, often viewing it as part of the problem.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) is high because it allows powerful states to bypass traditional non-intervention norms, extracting geopolitical influence, resources, or security benefits from weaker states under the guise of capacity building or stabilization. Suppression (0.75) is also high, as weak states have limited means to resist these interventions or challenge the capacity metrics used against them. The theater ratio (0.20) is moderate; while genuine efforts at capacity building exist, a significant portion of the discourse and action serves to legitimize interventions that primarily benefit external actors. The metrics show a clear trend of increasing extractiveness and suppression since the 1990s, reflecting the hardening of this doctrine.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and capacity evaluators perceive this as a necessary, benevolent coordination mechanism to address global instability. Weak states and many Global South observers perceive it as a highly extractive mechanism that perpetuates inequality and undermines self-determination. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity evaluation authorities and powerful states are clear beneficiaries, gaining legitimacy and influence (low directionality). Weak states and their populations are the primary targets, bearing the costs of diminished sovereignty and external interference (high directionality). Populations in weak states are a complex case, as they are ostensibly the beneficiaries of interventions but often bear significant direct costs, leading to a high directionality despite the stated humanitarian goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (genuine coordination) or a Snare (pure extraction). It acknowledges the genuine coordination problem of state fragility while highlighting the asymmetric extraction and active enforcement required to maintain the hierarchical structure. The 'live' status of the founding problem (state fragility) is contested, but the persistence of the doctrine suggests that even if the original problem is partially addressed, the extractive elements have become self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metrics_bias,
    'Are the metrics used to evaluate state capacity culturally biased or designed in a way that systematically disadvantages certain types of states or governance models?',
    'Independent, cross-cultural validation of capacity metrics, involving diverse epistemic communities and local populations, to identify and mitigate inherent biases.',
    'If biased, the measured extractiveness and suppression are artificially inflated, as the ''deficit'' is a construct of the measurement, not an objective reality. This would shift the classification towards a Snare, as the coordination story becomes a cover for imposed norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metrics_bias, conceptual, 'Bias in state capacity metrics and its impact on intervention legitimacy.').

omega_variable(
    intervention_effectiveness,
    'Do interventions based on graded sovereignty genuinely improve state capacity and human security in the long term, or do they primarily serve the geopolitical interests of intervening powers?',
    'Longitudinal, independent impact assessments of interventions, comparing outcomes in intervened states with non-intervened but similarly fragile states, controlling for external factors.',
    'If interventions consistently fail to improve capacity or security, the coordination function is largely theatrical, and the constraint''s classification would shift closer to a Snare. If effective, the coordination aspect is stronger, potentially moving it towards a Rope, though the extractive elements would likely remain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'Effectiveness of capacity-based interventions in achieving stated goals.').

omega_variable(
    legitimacy_of_hierarchy,
    'Is a hierarchical international system, where some states have the right to evaluate and intervene in others, a legitimate and stable basis for global order, or does it inherently breed resentment and instability?',
    'A global deliberative process or a shift in international norms towards a more egalitarian conception of sovereignty, potentially through a new UN charter or treaty.',
    'If a hierarchical system is deemed illegitimate, the entire framework of graded sovereignty loses its normative grounding, and its persistence would be seen as pure power projection, pushing it towards a Snare. If accepted, the constraint''s coordination function is strengthened by a shared normative framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_hierarchy, preference, 'Normative legitimacy of a hierarchical international state system.').


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
narrative_ontology:measurement(west_tr_t2014, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(west_be_t1998, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(west_be_t2006, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(west_be_t2014, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2014, 0.65).
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
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Westphalian Sovereignty' kernel. It is linked to other readings (absolute non-intervention, conditional responsibility) as they represent competing interpretations of state sovereignty and intervention legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
