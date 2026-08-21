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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Sovereignty as Graded Capacity
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint describes the reading of Westphalian sovereignty where
 *   territorial authority is not a binary, absolute concept, but a scalar
 *   capacity. Intervention legitimacy is calibrated to perceived deficits in
 *   a state's capacity to govern, protect its population, or maintain order.
 *   This creates a de facto hierarchical international system where powerful
 *   states and international bodies can exercise paternalistic oversight or
 *   intervention, particularly over 'weak' or 'failed' states. The constraint
 *   is claimed as a Tangled Rope because it purports to offer a coordination
 *   function (global stability, humanitarian protection) but operates with
 *   significant asymmetric extraction from weaker states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.7).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.65).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Sovereignty as Graded Capacity").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '320b4e4e-6c75-40c9-9d5e-681ce3756268').
narrative_ontology:cs_kernel_codification('320b4e4e-6c75-40c9-9d5e-681ce3756268', formalized).
narrative_ontology:cs_authority_grounding('320b4e4e-6c75-40c9-9d5e-681ce3756268', lineage).
narrative_ontology:cs_interpretation_layer_present('320b4e4e-6c75-40c9-9d5e-681ce3756268').
narrative_ontology:cs_reading_relation('320b4e4e-6c75-40c9-9d5e-681ce3756268', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('320b4e4e-6c75-40c9-9d5e-681ce3756268', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('320b4e4e-6c75-40c9-9d5e-681ce3756268', foundational, state_capacity_is_measurable_and_gradable).
narrative_ontology:cs_axiom_status(state_capacity_is_measurable_and_gradable, holdable).
narrative_ontology:cs_axiom_grounding('320b4e4e-6c75-40c9-9d5e-681ce3756268', state_capacity_is_measurable_and_gradable, empirically_contingent).
narrative_ontology:cs_axiom('320b4e4e-6c75-40c9-9d5e-681ce3756268', foundational, sovereignty_is_contingent_on_capacity).
narrative_ontology:cs_axiom_status(sovereignty_is_contingent_on_capacity, holdable).
narrative_ontology:cs_axiom_grounding('320b4e4e-6c75-40c9-9d5e-681ce3756268', sovereignty_is_contingent_on_capacity, conventional).
narrative_ontology:cs_reference_frame('320b4e4e-6c75-40c9-9d5e-681ce3756268', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('320b4e4e-6c75-40c9-9d5e-681ce3756268', contemporary_multipolar_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('320b4e4e-6c75-40c9-9d5e-681ce3756268', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_humanitarian_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, states_under_intervention).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, global_governance_imperative).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a framework that legitimizes intervention and oversight in states deemed to have insufficient capacity, allowing them to pursue strategic interests under the guise of capacity building or humanitarian concern. They are often the primary actors in capacity evaluation and intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_states, beneficiary,
    institutional, generational, arbitrage, global).

% International organizations or expert bodies tasked with assessing state capacity. They define the metrics, conduct evaluations, and issue reports that can legitimize or delegitimize a state's full sovereign claims. Their authority is derived from the powerful states that fund and empower them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Bear the costs of this constraint through reduced autonomy, external oversight, and potential intervention. Their sovereignty is de facto conditional on external assessments of their capacity, which can be subjective and politically motivated. Exiting the system means further isolation and vulnerability.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% Are the direct targets of interventions justified by their perceived capacity deficits. They experience the most severe loss of sovereignty and often face internal and external pressures that make resistance difficult or impossible.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, states_under_intervention, payer,
    powerless, immediate, trapped, national).

% Benefit from the framework's stated goal of addressing humanitarian crises and protecting populations, as it can open doors for aid delivery and protection mandates. However, they are also constrained by the political agendas of powerful states and the potential for interventions to exacerbate conflict.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_humanitarian_organizations, beneficiary,
    organized, biographical, constrained, global).

% Academics, NGOs, and some states that argue for strict adherence to the principle of non-intervention, regardless of internal state capacity. They are largely excluded from the decision-making processes that legitimize graded sovereignty and intervention, but continue to voice dissent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for managing global security and humanitarian concerns by legitimizing external action in states deemed incapable of fulfilling sovereign responsibilities, thereby preventing internal crises from destabilizing the international system.
% TRANSFER_FUNCTION: Transfers autonomy and decision-making power from weak states to powerful states and international bodies, in exchange for (purported) stability, security, or humanitarian aid. It also transfers legitimacy for intervention from a categorical principle to a scalar assessment.
% ABSENT_VOICES: Advocates for absolute non-intervention and many weak states themselves are often marginalized or excluded from the discourse that defines and applies 'graded sovereignty.' They would argue that capacity assessments are inherently biased and serve as a pretext for neo-colonial intervention.
% DISAPPEARANCE_RATIONALE: If the concept of graded sovereignty and its associated intervention legitimacy vanished, the international system would revert to a more traditional Westphalian model, where intervention is less easily justified. Powerful states would lose a key tool for projecting influence, and weak states might experience increased autonomy but also potentially greater vulnerability without external oversight or aid.
% FOUNDING_PROBLEM: The perceived failure of the traditional Westphalian system to address humanitarian crises, internal conflicts, and state collapse in the post-Cold War era, leading to regional instability and human suffering.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and international humanitarian organizations attest that the problem of state fragility and its consequences remains live. Academic theorists and some international legal scholars also corroborate the persistence of these challenges, though they may dispute the efficacy or legitimacy of 'graded sovereignty' as a solution. Absolute non_intervention advocates contest the framing of the problem itself as justifying intervention.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because weak states lose significant autonomy and are subject to external dictates, often without genuine consent. Suppression is also high (0.65) as weak states typically lack the power to resist interventions or the imposition of capacity-building mandates. Theater ratio is moderate (0.4): while there are genuine efforts at capacity building and humanitarian aid, the framework also serves as a legitimizing cover for powerful states' strategic interests, making some activities performative rather than purely functional. Accessibility collapse is high (0.7) because weak states have few viable alternatives to engaging with the international system on these terms, and resistance (0.55) is present but often overcome.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and capacity evaluation authorities perceive this as a necessary, legitimate framework for global governance and stability, a 'coordination' mechanism. Weak states and those under intervention, however, experience it as a highly extractive and suppressive system that undermines their fundamental sovereignty. The engine's classification will reflect this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and capacity evaluation authorities are beneficiaries (low d) as they gain influence, legitimacy for action, and a framework for managing global challenges. Weak states and states under intervention are clear targets (high d) as they bear the costs of lost autonomy and potential intervention. International humanitarian organizations are partial beneficiaries, gaining access and mandates, but also constrained by the political context.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the significant extraction from weak states) or a pure Snare (which would ignore the genuine, albeit often co-opted, coordination function related to global stability and humanitarian concerns). The 'mandate' of global stability and humanitarian protection is still live, but its implementation through 'graded sovereignty' has become highly extractive and asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capacity_objectivity,
    'To what extent are ''state capacity'' metrics objective and universally applicable, versus being culturally biased or serving the political interests of powerful states?',
    'Independent, cross-cultural studies of state functionality that de-link capacity assessments from geopolitical interests, or a shift in international consensus towards more inclusive definitions of sovereignty.',
    'If capacity metrics are found to be highly subjective or biased, the legitimacy of interventions based on them would collapse, reclassifying the constraint closer to a Snare. If they are robustly objective, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_objectivity, conceptual, 'Ambiguity in the objectivity and political neutrality of state capacity assessments.').

omega_variable(
    intervention_motive_ambiguity,
    'Are interventions justified by ''capacity deficits'' primarily driven by genuine humanitarian concern and capacity building, or by the strategic and economic interests of intervening powers?',
    'Longitudinal studies tracking post-intervention outcomes, comparing stated goals with actual impacts on local populations and the geopolitical landscape, and analyzing resource flows.',
    'If strategic interests consistently outweigh humanitarian outcomes, the constraint''s extractiveness and suppression would be re-evaluated upward, pushing it closer to a Snare. If genuine capacity building is the consistent outcome, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_motive_ambiguity, empirical, 'Ambiguity regarding the true motives behind interventions based on graded sovereignty.').

omega_variable(
    westphalia_sovereignty_kernel_reading,
    'This constraint is one reading of the ''westphalia_sovereignty'' kernel, specifically the ''graded_sovereignty'' reading. How would the classification change if a sibling reading were adopted?',
    'Conceptual analysis of the structural implications of adopting the ''absolute_non_intervention'' or ''conditional_responsibility'' readings.',
    'Adopting ''absolute_non_intervention'' would likely classify the international system as a Rope (pure coordination) or even a Mountain (natural law) regarding non-intervention, with zero extraction from weak states. Adopting ''conditional_responsibility'' would still allow intervention but would shift the justification from capacity deficits to mass atrocities, potentially altering the victim set and the nature of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(westphalia_sovereignty_kernel_reading, conceptual, 'This constraint is a specific reading of the Westphalian sovereignty kernel, with alternative readings having different structural implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t1998, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(west_tr_t2006, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(west_tr_t2014, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(west_be_t1998, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2006, 0.65).
narrative_ontology:measurement(west_be_t2014, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2014, 0.68).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(west_su_t1998, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(west_su_t2006, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(west_su_t2014, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2014, 0.63).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, global_infrastructure).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_aid_regime).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('graded_sovereignty') of the 'westphalia_sovereignty' kernel. It differs from 'absolute_non_intervention' by making sovereignty conditional on capacity, and from 'conditional_responsibility' by focusing on capacity deficits rather than mass atrocities as the primary trigger for intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
