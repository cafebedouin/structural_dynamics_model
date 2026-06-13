% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance Only Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the Kodashim
 *   commandment status, which holds that sacrificial laws are contingent on
 *   the Temple's existence and are suspended without an altar. This reading
 *   leads to a focus on the theoretical study of these laws rather than their
 *   practical application or active preparation for their restoration. The
 *   constraint is classified as a Piton due to its high theater ratio and the
 *   inertial persistence of scholarly investment in a non-performable domain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.65).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.4).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '64a04ba8-27ba-4064-90e1-5e2ce7a0997b').
narrative_ontology:cs_kernel_codification('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', fixed_text).
narrative_ontology:cs_authority_grounding('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', lineage).
narrative_ontology:cs_interpretation_layer_present('64a04ba8-27ba-4064-90e1-5e2ce7a0997b').
narrative_ontology:cs_reading_relation('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', foundational, commandment_contingent_on_temple).
narrative_ontology:cs_axiom_status(commandment_contingent_on_temple, holdable).
narrative_ontology:cs_axiom_grounding('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', commandment_contingent_on_temple, conventional).
narrative_ontology:cs_axiom('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', foundational, study_is_not_performance).
narrative_ontology:cs_axiom_status(study_is_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', study_is_not_performance, deontological).
narrative_ontology:cs_reference_frame('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('64a04ba8-27ba-4064-90e1-5e2ce7a0997b', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, scholarly_community).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, community_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, laity).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Jewish law. They continue to study and teach the Kodashim (sacrificial) laws, even though the Temple, where these laws would be performed, has not existed for nearly 2000 years. Their professional identity and scholarly careers are deeply intertwined with this tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Invest significant time and intellectual effort in studying the Kodashim laws, often at the expense of other areas of Jewish law or secular studies. Their educational path and future roles within the religious community are shaped by this curriculum.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    moderate, biographical, identity_locked, local).

% Financial and intellectual resources of the broader religious community are allocated to maintaining institutions and curricula focused on the study of Kodashim, diverting them from potentially more pressing contemporary needs or other areas of religious development.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, community_resources, payer,
    powerless, generational, trapped, local).

% Benefit from the preservation of ancient texts and the intellectual rigor of the scholarly tradition, even if they do not directly engage with the Kodashim laws. They may also feel a sense of continuity with tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, laity, beneficiary,
    powerless, biographical, mobile, local).

% Advocate for the rebuilding of the Temple and the restoration of sacrificial worship. From their perspective, the current study of Kodashim is insufficient and defers the true commandment. They are excluded from the mainstream halakhic discourse that treats the laws as currently non-performable.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, messianic_activists, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous scholarly tradition and intellectual engagement with foundational religious texts, ensuring the preservation of knowledge related to sacrificial laws even in their non-performable state.
% TRANSFER_FUNCTION: Transfers scholarly attention, educational resources, and communal prestige towards the study of non-performable sacrificial laws, from the broader community and students to the established halakhic institutions and scholars.
% ABSENT_VOICES: Those who advocate for a more pragmatic allocation of scholarly resources towards contemporary ethical or social issues, or those who believe the focus on non-performable laws detracts from active religious practice, are largely absent from the core discourse. Messianic activists, who believe the laws should be performed, are also marginalized.
% DISAPPEARANCE_RATIONALE: If the constraint (that Kodashim laws are only for study, not performance, due to Temple absence) vanished, the scholarly community would likely continue studying them, but the intensity and resource allocation might shift. The core religious practice of the laity would remain largely unchanged, as these laws are not currently part of their active observance. The world would not 'rearrange' in a fundamental way, but scholarly priorities might rebalance.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central commandments of sacrificial worship impossible to perform, creating a crisis of religious practice and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple destruction is historically attested and universally acknowledged. However, the 'performance_only' reading asserts that the problem of non-performance is 'dead' in the sense that the commandment is suspended, not merely deferred. This status is corroborated by the long-standing practice of the mainstream halakhic community, which has not attempted to perform sacrifices for nearly two millennia, and by the consensus of most non-messianic scholars.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) comes from the diversion of significant intellectual and communal resources into the study of laws that cannot be performed. Suppression (0.40) is moderate, as there isn't active coercion against alternative scholarly pursuits, but strong social and identity pressures exist. The theater ratio (0.80) is high because the primary activity (study) is a performance of engagement with a commandment that is functionally inert. Accessibility collapse (0.70) is high because the physical absence of the Temple makes actual performance impossible, and the dominant interpretive tradition makes alternative 'performance' (e.g., study as fulfillment) less accessible within this specific reading. Resistance (0.15) is low because the mainstream community largely accepts this interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars, as agenda-setters, benefit from the perpetuation of their field of study, even if it's theoretical. Yeshiva students and community resources are payers, as they invest heavily in this study. The laity are diffuse beneficiaries, gaining a sense of tradition without direct cost. Messianic activists are excluded, as their desire for active performance is outside the scope of this reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the original mandate (performing sacrifices) is impossible, but the structure (studying the laws) persists due to institutional inertia and identity-locked scholarly careers. It's a Piton because no single party benefits enough from the *extraction* to actively maintain it as a Snare, nor is any party hurt enough to force its dismantling. The diffuse costs are borne by many, but the concentrated benefit of 'preserving tradition' is enough to keep the system running performatively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of scholarly and communal resources to Kodashim study an efficient use of intellectual capital, given the non-performable status of the laws?',
    'Comparative analysis of resource allocation in other areas of Jewish law or contemporary ethical challenges, measuring impact and community benefit.',
    'If inefficient, it would strengthen the ''extraction'' component of the constraint, highlighting the opportunity cost borne by the community. If deemed efficient (e.g., for intellectual rigor or historical preservation), it would slightly reduce the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for non-performable laws.').

omega_variable(
    identity_lock_vs_scholarly_autonomy,
    'To what extent is the continued focus on Kodashim study driven by genuine scholarly interest and to what extent by identity-lock and career path dependence within the halakhic community?',
    'Sociological study of scholarly career paths, incentives, and the perceived prestige of different areas of study within the yeshiva system. Counterfactual analysis of alternative curricula.',
    'If identity-lock is the dominant factor, it reinforces the Piton classification by emphasizing the inertial, self-perpetuating nature of the constraint, rather than its functional utility. If genuine scholarly autonomy is higher, it suggests a more Rope-like coordination of intellectual pursuit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_scholarly_autonomy, empirical, 'Drivers of scholarly focus: identity-lock vs. autonomy.').

omega_variable(
    kernel_reading_divergence,
    'What are the precise structural implications of adopting the ''messianic_deferral'' or ''study_as_performance'' readings instead of ''performance_only''?',
    'Detailed comparative analysis of the three readings, mapping their distinct beneficiary/victim sets, resource flows, and implied actions (e.g., active preparation for Temple rebuilding vs. purely theoretical engagement).',
    'The ''messianic_deferral'' reading would likely reduce the theater ratio and increase the ''live'' status of the founding problem, potentially shifting the classification towards a Scaffold (preparation for future performance). The ''study_as_performance'' reading would reframe the study as a direct fulfillment, potentially reducing extractiveness and shifting towards a Rope (coordination of spiritual practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.7).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__performance_only, theater_ratio, 500, 0.75).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__performance_only, theater_ratio, 1000, 0.78).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__performance_only, theater_ratio, 1500, 0.79).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__performance_only, theater_ratio, 2000, 0.8).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__performance_only, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__performance_only, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__performance_only, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__performance_only, base_extractiveness, 2000, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. The other readings are 'messianic_deferral' and 'study_as_performance'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
