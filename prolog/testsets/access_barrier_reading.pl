% ============================================================================
% CONSTRAINT STORY: access_barrier_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_access_barrier_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: access_barrier_reading
 *   human_readable: Learning Difficulty as Access Barrier (Resource-Differential Reading)
 *   domain: educational_psychology/learning_theory/epistemology
 *
 * SUMMARY:
 *   This constraint is the access-barrier reading of the learning-difficulty
 *   kernel. It holds that when a learner struggles, the struggle primarily
 *   indexes differential access to material resources—time, tutoring,
 *   institutional quality—rather than intrinsic cognitive limits. The reading
 *   names credentialing gatekeepers and well-resourced families as
 *   beneficiaries and under-resourced learners as victims. The constraint
 *   coordinates (shared standards enable credential portability) and extracts
 *   (unequal resource access is laundered as individual merit, legitimating
 *   stratification). The claim is tangled_rope; the metrics describe
 *   substantially extractive operation with rising theater as equity rhetoric
 *   grows while resource gaps persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(access_barrier_reading, 0.68).
domain_priors:suppression_score(access_barrier_reading, 0.72).
domain_priors:theater_ratio(access_barrier_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(access_barrier_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(access_barrier_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(access_barrier_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(access_barrier_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(access_barrier_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(access_barrier_reading, tangled_rope).
narrative_ontology:human_readable(access_barrier_reading, "Learning Difficulty as Access Barrier (Resource-Differential Reading)").
narrative_ontology:topic_domain(access_barrier_reading, "educational_psychology/learning_theory/epistemology").

domain_priors:requires_active_enforcement(access_barrier_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(access_barrier_reading, '5cb84df3-9090-440b-a2d4-67e7f934a36c').
narrative_ontology:cs_kernel_codification('5cb84df3-9090-440b-a2d4-67e7f934a36c', distributed).
narrative_ontology:cs_authority_grounding('5cb84df3-9090-440b-a2d4-67e7f934a36c', distributed).
narrative_ontology:cs_reading_relation('5cb84df3-9090-440b-a2d4-67e7f934a36c', learning_difficulty_substrate__ability_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cb84df3-9090-440b-a2d4-67e7f934a36c', learning_difficulty_substrate__prerequisite_debt_reading, coexists_with).
narrative_ontology:cs_axiom('5cb84df3-9090-440b-a2d4-67e7f934a36c', foundational, struggle_indexes_material_conditions).
narrative_ontology:cs_axiom_status(struggle_indexes_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('5cb84df3-9090-440b-a2d4-67e7f934a36c', struggle_indexes_material_conditions, empirically_contingent).
narrative_ontology:cs_axiom('5cb84df3-9090-440b-a2d4-67e7f934a36c', secondary, resource_access_determines_mastery_probability).
narrative_ontology:cs_axiom_status(resource_access_determines_mastery_probability, holdable).
narrative_ontology:cs_axiom_grounding('5cb84df3-9090-440b-a2d4-67e7f934a36c', resource_access_determines_mastery_probability, empirically_contingent).
narrative_ontology:cs_reference_frame('5cb84df3-9090-440b-a2d4-67e7f934a36c', equal_access_counterfactual).
narrative_ontology:cs_drift_state('5cb84df3-9090-440b-a2d4-67e7f934a36c', contemporary_inequality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5cb84df3-9090-440b-a2d4-67e7f934a36c', '').
narrative_ontology:cs_kernel_id(access_barrier_reading, learning_difficulty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(access_barrier_reading, credentialing_gatekeepers).
narrative_ontology:constraint_beneficiary(access_barrier_reading, tutoring_industry).
narrative_ontology:constraint_beneficiary(access_barrier_reading, well_resourced_families).
narrative_ontology:constraint_victim(access_barrier_reading, under_resourced_learners).
narrative_ontology:constraint_victim(access_barrier_reading, time_poor_students).
narrative_ontology:constraint_victim(access_barrier_reading, institutional_periphery_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(access_barrier_reading, public_school_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the standards for what counts as mastery and administer the assessments that sort learners into credential tiers. Frame difficulty as revealing individual capacity rather than resource access. Benefit from maintaining scarcity in credentialed positions—if struggle indexed only material conditions, the legitimacy of selective sorting would collapse.
narrative_ontology:constraint_stakeholder(access_barrier_reading, credentialing_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell supplementary instruction, test preparation, and enrichment services to families who can pay. Revenue depends on the gap between institutional provision and what mastery actually requires—if schools provided sufficient time and support, the market would shrink. The constraint's coordination story (standards ensure quality) covers the extraction (unequal access to mastery resources).
narrative_ontology:constraint_stakeholder(access_barrier_reading, tutoring_industry, beneficiary,
    organized, biographical, mobile, national).

% Purchase tutoring, enrichment, and institutional quality (via residential sorting or private school tuition). Their children clear the difficulty threshold not by greater inherent capacity but by access to more learning time, better-trained instructors, and lower competing demands. The system's failure to name this advantage as structural preserves it.
narrative_ontology:constraint_stakeholder(access_barrier_reading, well_resourced_families, beneficiary,
    powerful, generational, arbitrage, local).

% Attend under-funded schools with larger classes, fewer materials, and less experienced teachers. Work outside school to support family income, reducing study time. Lack access to tutoring or test prep. When they struggle, the difficulty is attributed to individual deficit rather than resource scarcity, which forecloses claims for redistribution and cements their position in the credential hierarchy.
narrative_ontology:constraint_stakeholder(access_barrier_reading, under_resourced_learners, payer,
    powerless, biographical, trapped, local).

% Juggle school with caregiving, employment, or long commutes. Have less time for homework, review, and practice than peers without these demands. The system treats time as if it were equally distributed; when time-poor students fall behind, the lag is read as lower ability rather than constrained hours.
narrative_ontology:constraint_stakeholder(access_barrier_reading, time_poor_students, payer,
    moderate, biographical, constrained, local).

% Attend schools in rural areas or under-served urban neighborhoods where teacher turnover is high, advanced courses are unavailable, and infrastructure is degraded. The institutional quality gap is large but invisible in the assessment—they are tested against the same standards as students in well-funded districts, and their lower scores are attributed to individual shortcomings.
narrative_ontology:constraint_stakeholder(access_barrier_reading, institutional_periphery_students, payer,
    powerless, biographical, trapped, regional).

% Study the correlation between socioeconomic status and learning outcomes. Produce evidence that resource access predicts performance more strongly than measured cognitive traits. Their findings challenge the individual-deficit framing but are often ignored in policy because accepting them would require redistributive remedies.
narrative_ontology:constraint_stakeholder(access_barrier_reading, educational_researchers, observer,
    analytical, generational, analytical, global).

% Work in under-resourced schools and see the material constraints firsthand—too many students per class, insufficient materials, students arriving hungry or exhausted. Advocate for smaller classes and more support staff but are told the standards are fixed and difficulty is a student trait. Their structural knowledge is excluded from the policy conversation.
narrative_ontology:constraint_stakeholder(access_barrier_reading, public_school_teachers, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(access_barrier_reading, public_school_teachers, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared standards for what counts as educational mastery, enabling credential portability and signaling to employers and institutions. Coordinates expectations across a distributed system of schools and assessments.
% TRANSFER_FUNCTION: Moves credentialed access and social position from under-resourced learners to well-resourced learners, mediated by differential access to learning time, tutoring, and institutional quality. The transfer is laundered through the language of individual merit.
% ABSENT_VOICES: Under-resourced learners and their families are structurally excluded from standard-setting bodies. Public school teachers in high-poverty schools have direct knowledge of resource constraints but are excluded from policy design. Both groups would argue for resource redistribution and time-adjusted standards if they held agenda-setting power.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if learning difficulty were universally understood as indexing material conditions rather than individual deficit—resource allocation would shift dramatically. Credentialing institutions would lose legitimacy for selective sorting without first equalizing access. The tutoring industry would contract. Well-resourced families would face pressure to support redistribution rather than purchase advantage. The entire structure of educational stratification would require renegotiation.
% FOUNDING_PROBLEM: Early mass education systems needed portable, legible standards to coordinate instruction and credential recognition across geographically distributed schools with varying local resources.
% FOUNDING_PROBLEM_CORROBORATION: Credentialing gatekeepers attest the founding problem is still live and standards remain necessary for coordination. Educational researchers and equity advocates attest the founding problem has been substantially solved by information technology and that the current arrangement persists primarily to legitimate stratification. Independent sociological analysis from outside the credentialing system supports the shifted-function reading.
narrative_ontology:disappearance_verdict(access_barrier_reading, world_rearranges).
narrative_ontology:founding_problem_status(access_barrier_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(access_barrier_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-27',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(access_barrier_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(access_barrier_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(access_barrier_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(access_barrier_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the constraint's operation transfers credentialed access from resource-poor to resource-rich learners while attributing the transfer to individual capacity. Suppression is higher still (0.72) because the arrangement depends on actively suppressing the resource-access framing—if struggle were widely understood as material rather than individual, redistributive claims would gain legitimacy and the sorting function would lose it. Theater rises over the interval (0.22 to 0.41) as equity language proliferates in policy documents while funding gaps and tutoring markets expand, indicating performative commitment displacing structural remedy. Accessibility collapse is moderate (0.58)—alternative framings exist and circulate in research communities, but they are excluded from standard-setting. Resistance is substantial (0.64)—teachers, families, and students in under-resourced contexts contest the individual-deficit framing, though their resistance is marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialing gatekeeper seat, the constraint is coordination—standards ensure quality and portability, and difficulty reveals who has mastered the material. From the under-resourced learner seat, the same structure operates as extraction—difficulty reveals who had access to mastery resources, and the individual-deficit framing forecloses claims for redistribution. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialing gatekeepers are structural beneficiaries (set the standards, benefit from scarcity in credentialed positions, d near beneficiary end). Well-resourced families and the tutoring industry are beneficiaries (purchase advantage, collect revenue from the resource gap). Under-resourced learners, time-poor students, and institutional-periphery students are targets (bear the costs of unequal access, attributed to individual deficit, trapped or constrained exit, d near target end). Educational researchers are analytical observers (see the full structure, no stake in its persistence).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to coordinate distributed instruction and credential recognition. That function persists but is now layered with extraction—the standards are enforced without equalizing the resources required to meet them, which converts resource inequality into credentialed hierarchy. The founding problem (coordination across distributed schools) is contested: credentialing bodies say it remains live; researchers say information technology has solved it and the arrangement now primarily legitimates stratification. The six-questions interview records this as founding_problem_status: contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_access_vs_cognitive_trait,
    'What proportion of observed learning difficulty is attributable to differential resource access (time, tutoring, institutional quality) versus stable cognitive traits?',
    'Randomized controlled trials equalizing resource access across learners and measuring residual performance variance. Natural experiments from policy changes that dramatically shift resource distribution.',
    'If resource access explains most variance, the individual-deficit framing is false and the constraint operates primarily as extraction. If cognitive traits explain most variance, the coordination framing is more accurate and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_access_vs_cognitive_trait, empirical, 'Whether learning difficulty primarily indexes material conditions or intrinsic traits.').

omega_variable(
    committer_frame_kernel_ambiguity,
    'Is the learning-difficulty substrate a single contested kernel with multiple readings, or are the readings describing structurally distinct phenomena that happen to share a label?',
    'Conceptual analysis of whether the readings share a common object (the substrate of difficulty) or whether ''learning difficulty'' is a natural-language umbrella covering unrelated constraints. Empirical test: do interventions that resolve difficulty under one reading also resolve it under the others?',
    'If the readings share a kernel, they are alternative interpretations of one commitment and the committer frame applies. If they describe distinct phenomena, they are separate constraints that happen to use the same term, and the kernel structure is a labeling artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_ambiguity, conceptual, 'Whether the sibling readings are alternative interpretations of one kernel or distinct constraints sharing a label.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (shared standards for credential portability) structurally separable from the extraction function (legitimating stratification via individual-deficit framing)?',
    'Policy experiments that maintain shared standards while explicitly adjusting for resource access in assessment or credentialing. If coordination holds while extraction drops, the functions are separable.',
    'If separable, the extraction is riding on genuine coordination and could be removed without losing the coordination benefit. If inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be structurally decoupled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(access_barrier_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, access_barrier_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(acce_tr_t8, access_barrier_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(acce_tr_t16, access_barrier_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(acce_tr_t24, access_barrier_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(acce_tr_t32, access_barrier_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(acce_tr_t40, access_barrier_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, access_barrier_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(acce_be_t8, access_barrier_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(acce_be_t16, access_barrier_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(acce_be_t24, access_barrier_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(acce_be_t32, access_barrier_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(acce_be_t40, access_barrier_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, access_barrier_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(acce_su_t8, access_barrier_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(acce_su_t16, access_barrier_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(acce_su_t24, access_barrier_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(acce_su_t32, access_barrier_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(acce_su_t40, access_barrier_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(access_barrier_reading, ability_ceiling_reading).
narrative_ontology:affects_constraint(access_barrier_reading, prerequisite_debt_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the learning_difficulty_substrate kernel. The access_barrier_reading (this file) holds that difficulty indexes material resource access and names under-resourced learners as victims. The ability_ceiling_reading holds that difficulty reveals intrinsic cognitive limits and has near-zero extraction (mountain claim). The prerequisite_debt_reading holds that difficulty reveals accumulated gaps in prior mastery and has moderate extraction (rope or scaffold). The readings have different ε values, different beneficiary/victim structures, and different policy implications. They are linked here via network.affects_constraints because they compete for legitimacy in the same policy and institutional spaces—adoption of one reading structurally influences the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
