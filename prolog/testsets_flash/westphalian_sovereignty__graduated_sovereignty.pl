% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The doctrine of 'graduated sovereignty' posits that a state's sovereignty
 *   is not absolute but exists on a spectrum, determined by its capacity to
 *   govern effectively and the legitimacy of its governance. This reading of
 *   Westphalian sovereignty allows external actors (powerful states,
 *   international organizations) to intervene in or impose conditions on
 *   states deemed to have 'low capacity' or 'illegitimate governance,' often
 *   leading to a high degree of extraction and suppression from the affected
 *   states and their populations. It is claimed as a 'snare' due to its
 *   inherent asymmetry and the identifiable victims.
 *
 * KEY AGENTS:
 *   - powerful_states: Agenda-setter (institutional/arbitrage) — defines criteria, justifies intervention
 *   - international_organizations: Beneficiary (institutional/mobile) — gains expanded mandates and influence
 *   - weak_states: Payer (powerless/trapped) — subject to intervention, loses autonomy
 *   - marginalized_populations_in_weak_states: Payer (powerless/identity_locked) — suffers consequences of intervention, loss of self-determination
 *   - international_law_scholars: Observer (analytical/analytical) — critiques and analyzes the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '0ffa582f-c989-4052-8ca9-34ed9b9ad1b3').
narrative_ontology:cs_kernel_codification('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', distributed).
narrative_ontology:cs_authority_grounding('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', extraction).
narrative_ontology:cs_interpretation_layer_present('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3').
narrative_ontology:cs_reading_relation('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', westphalian_sovereignty__absolute_sovereignty, influences).
narrative_ontology:cs_reading_relation('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', foundational, sovereignty_is_performance_based).
narrative_ontology:cs_axiom_status(sovereignty_is_performance_based, holdable).
narrative_ontology:cs_axiom_grounding('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', sovereignty_is_performance_based, empirically_contingent).
narrative_ontology:cs_axiom('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', secondary, external_actors_assess_capacity).
narrative_ontology:cs_axiom_status(external_actors_assess_capacity, holdable).
narrative_ontology:cs_axiom_grounding('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', external_actors_assess_capacity, conventional).
narrative_ontology:cs_reference_frame('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', contemporary_multipolar_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0ffa582f-c989-4052-8ca9-34ed9b9ad1b3', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define the criteria for 'state capacity' and 'governance legitimacy,' and use these definitions to justify intervention or non-recognition of weaker states. They benefit from expanded discretion and reduced accountability for their actions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These organizations gain expanded mandates and legitimacy for intervention in states deemed to have 'low capacity' or 'illegitimate governance.' They benefit from increased funding and influence, often aligning with the interests of powerful states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_organizations, beneficiary,
    institutional, generational, mobile, global).

% These states are subject to external scrutiny, intervention, and conditional aid based on subjective assessments of their capacity and legitimacy. They bear the costs of lost autonomy, resource exploitation, and imposed governance models.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% While sometimes presented as beneficiaries of intervention, these populations often suffer from the instability and unintended consequences of external interference, including exacerbation of internal conflicts, displacement, and loss of self-determination. Their identity is often tied to their national or ethnic group, making exit from the state's fate impossible.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_weak_states, payer,
    powerless, biographical, identity_locked, local).

% Analyze the theoretical underpinnings and practical implications of graduated sovereignty, often critiquing its potential for abuse and its departure from traditional Westphalian principles. They provide intellectual frameworks for understanding the constraint.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international actors to coordinate responses to 'failed' or 'failing' states, ostensibly to prevent humanitarian crises or regional instability, by allowing for differentiated treatment based on state performance.
% TRANSFER_FUNCTION: Transfers decision-making authority and resources from 'weak' states to 'powerful' states and international organizations, along with the right to define and enforce 'legitimate governance' standards.
% ABSENT_VOICES: The populations and leaders of 'weak' states, particularly those whose governance is deemed 'illegitimate' by external actors, are often excluded from the processes that determine their sovereignty status. They would argue for self-determination and non-interference.
% DISAPPEARANCE_RATIONALE: If the doctrine of graduated sovereignty vanished, powerful states would lose a key justification for intervention, and international organizations would face significant challenges in legitimizing their mandates in 'weak' states. The international system would revert to a more traditional, albeit imperfect, Westphalian model, requiring new justifications for cross-border actions.
% FOUNDING_PROBLEM: The perceived inability of some states to govern effectively, leading to humanitarian crises, terrorism, and regional instability, which traditional absolute sovereignty doctrine struggled to address without violating international law.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and many international organizations attest that the problem of 'failed states' and their consequences remains live. Critics, including many scholars from the Global South, corroborate the existence of governance challenges but contest that 'graduated sovereignty' is the appropriate or just solution, arguing it exacerbates power imbalances.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because powerful states and international organizations gain significant discretion and resources by classifying and intervening in 'weak' states, often leading to resource extraction or imposition of favorable policies. Suppression (0.75) is also high, as 'weak' states have limited options to resist external pressures and interventions, with their sovereignty being actively curtailed. The theater ratio (0.20) is relatively low, as the interventions often have real, albeit contested, effects, but there is a performative aspect in framing interventions as purely humanitarian or capacity-building, masking underlying power dynamics. Accessibility collapse (0.40) is moderate, as alternatives for weak states are severely constrained but not entirely absent (e.g., seeking alliances with other powerful states). Resistance (0.70) is high, reflecting ongoing diplomatic, political, and sometimes armed resistance from affected states and their populations.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and international organizations perceive this as a necessary, albeit complex, coordination mechanism for global stability and human rights. Weak states and marginalized populations experience it as a coercive snare that undermines their self-determination and facilitates neo-colonial extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and international organizations are clear beneficiaries (d near 0.0) as they gain discretion, influence, and resources. Weak states and marginalized populations are clear targets (d near 1.0) as they bear the costs of lost autonomy, intervention, and imposed conditions. The 'graduated' nature of sovereignty means that the constraint is explicitly designed to differentiate treatment based on perceived capacity, creating an inherent asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare prevents mislabeling this as a 'rope' (genuine coordination) or 'scaffold' (temporary support). While it claims to address a 'founding problem' of state failure, its persistence and increasing extractiveness suggest it has evolved beyond a purely coordinative or transitional function, becoming a mechanism for power projection and resource control. The 'contested' status of the founding problem further supports this, indicating that the original mandate is now a cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_objectivity,
    'Are the criteria for ''state capacity'' and ''governance legitimacy'' objective and universally applicable, or are they subjectively defined by powerful states to serve their interests?',
    'Independent, cross-cultural expert consensus on objective metrics, or a shift in international law towards a more inclusive, multilateral process for defining these terms.',
    'If subjective, the constraint''s extractiveness and suppression are higher than measured, as the criteria themselves are part of the extractive mechanism. If objective, the constraint might move closer to a ''tangled rope'' if the coordination function is genuinely served.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criteria_objectivity, conceptual, 'Objectivity of criteria for sovereignty graduation.').

omega_variable(
    intervention_effectiveness,
    'Do interventions justified by graduated sovereignty consistently improve state capacity and governance legitimacy, or do they often lead to unintended negative consequences and prolonged instability?',
    'Longitudinal empirical studies of intervention outcomes, comparing intervened states with non-intervened states facing similar challenges, using metrics defined by affected populations.',
    'If interventions are ineffective or counterproductive, the ''coordination function'' is largely theatrical, increasing the constraint''s theater_ratio and reinforcing its snare classification. If effective, it might suggest a more complex, albeit still extractive, ''tangled rope'' dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'Effectiveness of interventions under graduated sovereignty.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is a ''graduated sovereignty'' reading of the ''Westphalian sovereignty'' kernel. What would change structurally if an ''absolute sovereignty'' or ''conditional sovereignty'' reading were adopted?',
    'Analysis of legal and political outcomes in jurisdictions or historical periods where alternative readings were dominant, or a hypothetical modeling of international relations under those frameworks.',
    'An ''absolute sovereignty'' reading would drastically reduce external intervention, shifting the constraint towards a ''mountain'' or ''rope'' for all states, but potentially increasing internal extraction within states. A ''conditional sovereignty'' reading would focus intervention on human rights violations, potentially reducing the broad discretion of powerful states and shifting the constraint towards a ''tangled rope'' with more specific triggers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative Westphalian sovereignty readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1998, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(west_tr_t2006, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t1998, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(west_su_t1998, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(west_su_t2006, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2014, 0.73).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Westphalian sovereignty' kernel. It is linked to 'westphalian_sovereignty__absolute_sovereignty' and 'westphalian_sovereignty__conditional_sovereignty' through the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
