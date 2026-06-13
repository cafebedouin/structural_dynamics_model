% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Ontological Substrate (Incoherent Bundle Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the 'incoherent bundle' reading of the
 *   Shinbutsu ontological substrate, arguing that the historical syncretism
 *   between Shinto and Buddhism in Japan was not a coherent theological
 *   fusion but an accumulated institutional drift, often enforced by state
 *   authorities for political ends. This reading views the resulting
 *   religious landscape as a snare, extracting cognitive and institutional
 *   coherence from practitioners and scholars while benefiting state and
 *   established religious institutions. The claimed type is 'snare' because
 *   the coordination story (religious unity) is cover for extraction and
 *   suppression, with identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.85).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.9).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Ontological Substrate (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '0df3d18e-3265-4772-bcfd-8a7105a9dbb1').
narrative_ontology:cs_kernel_codification('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', distributed).
narrative_ontology:cs_authority_grounding('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', extraction).
narrative_ontology:cs_interpretation_layer_present('0df3d18e-3265-4772-bcfd-8a7105a9dbb1').
narrative_ontology:cs_reading_relation('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', foundational, no_inherent_ontological_unity).
narrative_ontology:cs_axiom_status(no_inherent_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', no_inherent_ontological_unity, empirically_contingent).
narrative_ontology:cs_axiom('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', foundational, syncretism_as_institutional_drift).
narrative_ontology:cs_axiom_status(syncretism_as_institutional_drift, holdable).
narrative_ontology:cs_axiom_grounding('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', syncretism_as_institutional_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', pre_syncretic_diversity).
narrative_ontology:cs_drift_state('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', contemporary_religious_landscape, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0df3d18e-3265-4772-bcfd-8a7105a9dbb1', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, theological_scholars).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, new_religious_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically enforced the fusion of Shinto and Buddhist elements for political control and national unity, benefiting from a unified, state-sanctioned religious landscape. They continue to benefit from the administrative simplicity and historical legitimacy derived from this enforced syncretism, even if the explicit enforcement has changed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the historical endowments, land, and social status accrued during periods of enforced syncretism. They maintain the 'incoherent bundle' as a de facto operating model, even if it lacks theological coherence, because it preserves their institutional position and avoids disruptive internal debates.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, established_religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Bear the cognitive burden of holding contradictory beliefs and practices without a coherent theological framework. Their identity is often deeply intertwined with local traditions that blend Shinto and Buddhist elements, making it difficult to disentangle or question the underlying incoherence without challenging their own cultural and spiritual heritage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Struggle to construct coherent theological or philosophical accounts of the shinbutsu relationship, often facing resistance from institutions that prefer the ambiguity. Their careers and intellectual integrity are constrained by the need to navigate an officially sanctioned but intellectually unsatisfying framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, theological_scholars, payer,
    moderate, generational, constrained, global).

% Often emerge by rejecting the historical syncretism and attempting to establish 'pure' forms of Shinto or Buddhism. They are excluded from mainstream legitimacy and resources, facing institutional and social pressure to conform to the established, albeit incoherent, religious landscape.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, new_religious_movements, excluded,
    moderate, biographical, constrained, national).

% Analyze the historical processes of syncretism and separation, often concluding that the 'fusion' was more a political and institutional imposition than a genuine theological development. They provide an external, critical perspective on the constraint's origins and persistence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it coordinated diverse local religious practices under a unified state-sanctioned framework, reducing inter-sectarian conflict and facilitating administrative control over religious institutions and populations.
% TRANSFER_FUNCTION: Transfers legitimacy, resources, and administrative control from local, diverse religious expressions to centralized state and established religious institutions, in exchange for a nominally unified religious landscape.
% ABSENT_VOICES: Early proponents of 'pure' Shinto or Buddhism, as well as modern new religious movements, are excluded. They would argue for a clear separation or a different, coherent theological framework, but their voices have historically been suppressed or marginalized by the state-enforced syncretism.
% DISAPPEARANCE_RATIONALE: If the 'incoherent bundle' vanished, the administrative and institutional structures of Japanese religion would undergo significant reorganization. Many temples and shrines would lose their dual identities, leading to disputes over property, lineage, and ritual practice. New, more coherent theological frameworks would likely emerge, but the immediate effect would be widespread institutional and cultural disruption.
% FOUNDING_PROBLEM: The problem of integrating diverse indigenous kami worship with imported Buddhism, and later, the state's desire for a unified religious ideology to support political authority.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysts and theological scholars corroborate that the original problem of integrating diverse religious practices has been superseded by modern secular governance and the explicit separation of Shinto and Buddhism post-Meiji. The 'incoherent bundle' persists due to institutional inertia and historical endowments, not because it solves a live problem of religious integration.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system demands adherence to an internally contradictory framework, imposing cognitive costs on practitioners and intellectual costs on scholars, while funneling resources and legitimacy to established institutions. Suppression is very high (0.9) due to historical state enforcement (e.g., Shinbutsu-bunri policies post-Meiji, which paradoxically enforced a 'separation' that still left an incoherent legacy) and ongoing institutional pressure against alternative, more coherent framings. Theater ratio is high (0.6) because much of the 'unity' is performative, masking underlying contradictions and institutional inertia rather than reflecting genuine theological synthesis. The peak in extractiveness and suppression around 1870 reflects the Meiji government's aggressive Shinbutsu-bunri policies, which, while ostensibly separating the two, often resulted in violent suppression of Buddhist elements and a re-assertion of state-controlled Shinto, further entrenching an incoherent, politically driven religious landscape.
 *
 * PERSPECTIVAL GAP:
 *   State authorities and established religious institutions experience this as a stable, legitimate, and beneficial arrangement, even if intellectually ambiguous. Local practitioners and scholars, however, experience it as a source of cognitive dissonance, institutional constraint, and intellectual frustration. The 'incoherent bundle' is a feature for the beneficiaries (maintaining power/status) and a bug for the victims (bearing the contradictions).
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities are the primary agenda-setters and beneficiaries, historically leveraging religious unity for political control. Established religious institutions are beneficiaries, maintaining their historical endowments and social status. Local practitioners and theological scholars are victims, bearing the cognitive and intellectual costs of the incoherent framework. New religious movements are excluded, as their attempts at 'purity' challenge the established, ambiguous order.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (integrating diverse religious practices for social cohesion) has atrophied. The 'incoherent bundle' now persists not because it genuinely solves a live coordination problem, but because its maintenance benefits identifiable state and religious institutions, while the costs are borne by diffuse practitioners and marginalized scholars. This prevents mislabeling it as a 'rope' (genuine coordination) or 'piton' (inertial decay without concentrated benefit) by highlighting the active extraction and suppression involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_utility,
    'Is the ''incoherent bundle'' maintained due to its historical utility for social control, or is there an underlying, unarticulated coherence that scholars have yet to fully grasp?',
    'Comparative analysis of other syncretic traditions and their theological justifications, alongside further historical research into the motivations of state actors and religious institutions during periods of enforced syncretism.',
    'If primarily utility-driven, the snare classification is strengthened. If a hidden coherence is found, the constraint might reclassify towards a ''tangled rope'' or even ''rope'' if the benefits of this coherence are widely distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_utility, conceptual, 'Whether the shinbutsu syncretism is a pragmatic construct or has latent theological coherence.').

omega_variable(
    identity_lock_strength,
    'To what extent are local practitioners ''identity-locked'' into the incoherent bundle, such that disentangling Shinto and Buddhist elements would cause profound personal and communal identity crises?',
    'Sociological studies of communities undergoing religious reform or separation, examining the psychological and social costs of abandoning syncretic practices.',
    'If identity-lock is very strong, the effective suppression and extractiveness on practitioners are higher than structural measures suggest, as the cost of exit is existential. If weaker, the potential for resistance and alternative framings is greater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which practitioners'' identities are fused with the shinbutsu syncretism.').

omega_variable(
    state_enforcement_legacy,
    'What is the residual impact of historical state enforcement on the persistence of the ''incoherent bundle'' today, even in the absence of overt coercion?',
    'Analysis of institutional structures, property laws, and educational curricula to identify lingering effects of past state policies that favored or enforced syncretism.',
    'A strong residual impact would reinforce the ''snare'' classification, indicating that the constraint''s persistence is still rooted in past coercion. A weak impact might suggest a drift towards ''piton'' if inertia is the primary driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_legacy, empirical, 'Lingering effects of historical state enforcement on shinbutsu syncretism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 1600, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1800, 0.5).
narrative_ontology:measurement(shin_tr_t1870, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1870, 0.7).
narrative_ontology:measurement(shin_tr_t1950, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1950, 0.65).
narrative_ontology:measurement(shin_tr_t2020, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1700, 0.75).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1800, 0.8).
narrative_ontology:measurement(shin_be_t1870, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1870, 0.9).
narrative_ontology:measurement(shin_be_t1950, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement(shin_be_t2020, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1700, 0.8).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(shin_su_t1870, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1870, 0.95).
narrative_ontology:measurement(shin_su_t1950, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement(shin_su_t2020, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu ontological substrate' kernel. It posits that the syncretism is an incoherent, state-enforced bundle, contrasting with readings that claim genuine fusion or functional partition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
