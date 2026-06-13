% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate authority, where the legitimacy of mandates (e.g., vaccination
 *   requirements, lockdowns) is contingent on a sliding scale of factors: the
 *   severity of the threat, the availability of less coercive alternatives,
 *   the magnitude of the coercion imposed, and the duration of the
 *   imposition. It aims to balance collective health with individual rights,
 *   making it a dynamic constraint whose extractiveness and suppression
 *   fluctuate with the perceived threat level and societal context. This
 *   reading acknowledges that both immunocompromised individuals and those
 *   subject to mandates can be 'victims' depending on the proportionality
 *   assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.45).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.6).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '2b0e28fb-a7b3-4829-ac61-118f9f317079').
narrative_ontology:cs_kernel_codification('2b0e28fb-a7b3-4829-ac61-118f9f317079', formalized).
narrative_ontology:cs_authority_grounding('2b0e28fb-a7b3-4829-ac61-118f9f317079', lineage).
narrative_ontology:cs_interpretation_layer_present('2b0e28fb-a7b3-4829-ac61-118f9f317079').
narrative_ontology:cs_reading_relation('2b0e28fb-a7b3-4829-ac61-118f9f317079', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('2b0e28fb-a7b3-4829-ac61-118f9f317079', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('2b0e28fb-a7b3-4829-ac61-118f9f317079', foundational, mandate_legitimacy_is_contingent).
narrative_ontology:cs_axiom_status(mandate_legitimacy_is_contingent, holdable).
narrative_ontology:cs_axiom_grounding('2b0e28fb-a7b3-4829-ac61-118f9f317079', mandate_legitimacy_is_contingent, empirically_contingent).
narrative_ontology:cs_axiom('2b0e28fb-a7b3-4829-ac61-118f9f317079', foundational, individual_liberty_and_collective_health_must_be_balanced).
narrative_ontology:cs_axiom_status(individual_liberty_and_collective_health_must_be_balanced, holdable).
narrative_ontology:cs_axiom_grounding('2b0e28fb-a7b3-4829-ac61-118f9f317079', individual_liberty_and_collective_health_must_be_balanced, deontological).
narrative_ontology:cs_reference_frame('2b0e28fb-a7b3-4829-ac61-118f9f317079', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('2b0e28fb-a7b3-4829-ac61-118f9f317079', post_2020_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2b0e28fb-a7b3-4829-ac61-118f9f317079', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, individuals_with_conscientious_objections).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing and enforcing public health mandates, balancing individual liberties with collective well-being. Their legitimacy depends on demonstrating the proportionality of interventions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from mandates that reduce pathogen transmission, as they are at higher risk of severe illness. Their ability to participate in public life depends on the collective adherence to health measures.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Benefits from mandates that prevent overwhelming surges in patient load, ensuring capacity for all medical needs. Mandates reduce the strain on resources and personnel.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_system, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of mandates through restrictions on movement, employment, or participation in public activities. Their choices are limited by the mandate's coercive force, which varies with its proportionality.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Experience mandates as a direct challenge to their deeply held beliefs, leading to significant personal and social costs. Their identity is fused with their objection, making compliance a form of self-betrayal.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, individuals_with_conscientious_objections, payer,
    moderate, biographical, identity_locked, local).

% Monitor public health mandates for potential overreach and violations of individual rights. They engage in legal challenges and public discourse to ensure proportionality is maintained.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health threats by imposing measures that are proportional to the threat's severity, the availability of alternatives, the magnitude of coercion, and the duration of imposition.
% TRANSFER_FUNCTION: Transfers a degree of individual liberty and autonomy from those subject to mandates (e.g., unvaccinated individuals) to the collective good of public health and the protection of vulnerable populations.
% ABSENT_VOICES: Future generations who might bear the long-term consequences of either insufficient public health protection or excessive state power; their interests are represented by advocates but they cannot speak directly.
% DISAPPEARANCE_RATIONALE: If the authority to impose proportional public health mandates vanished, society would struggle to respond effectively to pandemics, leading to greater morbidity, mortality, and economic disruption. The balance between individual rights and collective safety would be lost, requiring new frameworks to emerge.
% FOUNDING_PROBLEM: The need to balance individual liberties with the collective imperative to protect public health during epidemics and pandemics, preventing widespread illness and healthcare system collapse.
% FOUNDING_PROBLEM_CORROBORATION: Public health crises are recurrent, and the tension between individual rights and collective action remains a live issue, as attested by ongoing legal challenges, bioethical debates, and the continuous need for public health policy development from legal scholars and medical professionals outside of public health agencies.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).
:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective action for public health (beneficiaries: immunocompromised, healthcare system) but also involves asymmetric extraction from individuals whose liberties are curtailed (victims: unvaccinated, conscientious objectors). Active enforcement is required to maintain compliance. The extractiveness (0.45) and suppression (0.6) are moderate, reflecting the ongoing contestation and the need for mandates to be justified. The theater ratio is low (0.1), indicating that the mandates are largely functional, though some performative aspects may exist during periods of high public anxiety. The temporal measurements show a spike in extractiveness and suppression during the 2020 pandemic, followed by a reduction as the immediate crisis subsided and proportionality concerns gained traction.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and beneficiaries (immunocompromised, healthcare system) perceive this constraint as a necessary and legitimate coordination mechanism, with extraction being a justified cost for collective safety. Individuals subject to mandates (unvaccinated, conscientious objectors) perceive it as an infringement on their autonomy, with the extraction being an unjust burden. The proportionality reading attempts to bridge this gap by providing a framework for evaluating the legitimacy of the constraint from both perspectives, but the inherent tension remains.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they wield the power to implement mandates for the collective good. Immunocompromised individuals and the healthcare system are clear beneficiaries, as the constraint directly protects their well-being and capacity. Unvaccinated individuals and those with conscientious objections are payers/victims, as they bear the direct costs of the mandates. Civil liberties advocates act as observers, scrutinizing the proportionality of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading inherently guards against mandatrophy by requiring continuous re-evaluation of the mandate's justification. If the founding problem (public health threat) diminishes, or if less coercive alternatives become available, the mandate's legitimacy (and thus its extractiveness/suppression) should decrease. This prevents the constraint from persisting as a Snare or Piton after its original function has atrophied, as it demands an ongoing, evidence-based justification for its existence and severity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''proportionality'' objectively measured across diverse threats, alternatives, coercion magnitudes, and durations?',
    'Development of standardized, empirically validated metrics for each proportionality factor, agreed upon by interdisciplinary expert consensus and subject to judicial review.',
    'If proportionality can be objectively measured, the constraint''s legitimacy is strengthened, potentially reducing resistance and perceived extractiveness. If it remains subjective, the constraint''s application will be contested, leading to higher perceived extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in objectively measuring the proportionality of public health mandates.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''proportionality reading'' of public health mandate authority, or is it a rhetorical cover for a ''public_health_primary'' or ''bodily_autonomy_primary'' reading?',
    'Analysis of judicial decisions, legislative intent, and public health policy implementation over time: does the actual application of mandates consistently demonstrate a dynamic, context-dependent balancing act, or does it consistently default to one extreme?',
    'If it''s a genuine proportionality reading, the constraint''s classification as a Tangled Rope is robust. If it''s a cover, it would reclassify to a Snare (if primarily extractive under a public_health_primary guise) or a Piton (if primarily performative under a bodily_autonomy_primary guise).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing the genuine proportionality reading from rhetorical cover for other readings of public health mandate authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t1900, public_health_mandate_authority__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(publ_tr_t1950, public_health_mandate_authority__proportionality_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(publ_tr_t2000, public_health_mandate_authority__proportionality_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(publ_tr_t2010, public_health_mandate_authority__proportionality_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(publ_tr_t2020, public_health_mandate_authority__proportionality_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(publ_tr_t2024, public_health_mandate_authority__proportionality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t1900, public_health_mandate_authority__proportionality_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(publ_be_t1950, public_health_mandate_authority__proportionality_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(publ_be_t2000, public_health_mandate_authority__proportionality_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(publ_be_t2010, public_health_mandate_authority__proportionality_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(publ_be_t2020, public_health_mandate_authority__proportionality_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(publ_be_t2024, public_health_mandate_authority__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t1900, public_health_mandate_authority__proportionality_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(publ_su_t1950, public_health_mandate_authority__proportionality_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(publ_su_t2000, public_health_mandate_authority__proportionality_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(publ_su_t2010, public_health_mandate_authority__proportionality_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(publ_su_t2020, public_health_mandate_authority__proportionality_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(publ_su_t2024, public_health_mandate_authority__proportionality_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'public_health_mandate_authority' kernel. It attempts to mediate between the 'public_health_primary' and 'bodily_autonomy_primary' readings by introducing a dynamic proportionality assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
