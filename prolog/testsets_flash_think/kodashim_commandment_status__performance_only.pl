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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance-Only Reading
 *   domain: religious/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the Kodashim
 *   commandment status, which asserts that sacrifice laws are contingent on
 *   the Temple's existence and are suspended without an altar. From this
 *   perspective, the continued emphasis on the study of these laws,
 *   particularly their performance aspects, becomes a 'husk' – a practice
 *   devoid of its original function. The constraint is classified as a Snare
 *   because the coordination story (preserving knowledge) is argued to be a
 *   cover for the extraction of resources and intellectual effort, benefiting
 *   specific institutions and scholars while diverting from more relevant
 *   halakhic pursuits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.78).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.65).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba').
narrative_ontology:cs_kernel_codification('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', fixed_text).
narrative_ontology:cs_authority_grounding('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', lineage).
narrative_ontology:cs_interpretation_layer_present('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba').
narrative_ontology:cs_reading_relation('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_axiom('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', foundational, commandment_contingent_on_conditions).
narrative_ontology:cs_axiom_status(commandment_contingent_on_conditions, holdable).
narrative_ontology:cs_axiom_grounding('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', commandment_contingent_on_conditions, deontological).
narrative_ontology:cs_axiom('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', foundational, performance_without_altar_is_void).
narrative_ontology:cs_axiom_status(performance_without_altar_is_void, holdable).
narrative_ontology:cs_axiom_grounding('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', performance_without_altar_is_void, conventional).
narrative_ontology:cs_reference_frame('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', halakhic_contingency_principle).
narrative_ontology:cs_drift_state('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', post_second_temple_destruction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98b16b94-52ef-4bc3-ad81-6fd5fd7f16ba', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, traditional_halakhic_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, scholars_of_kodashim).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, students_of_halakha).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, community_resources).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, reform_minded_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions set the curriculum and allocate resources within the traditional Jewish legal system. They benefit from maintaining the prestige and continuity of the study of Kodashim, even if its practical application is suspended, as it reinforces their authority and historical lineage.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, traditional_halakhic_institutions, agenda_setter,
    institutional, generational, constrained, global).

% These scholars dedicate their careers to the intricate study of sacrifice laws. Their professional identity, academic prestige, and access to funding are tied to the continued emphasis on this field, even if its direct performance is impossible. Exiting this field would mean abandoning their specialized expertise and status.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, scholars_of_kodashim, beneficiary,
    powerful, biographical, identity_locked, global).

% Students invest significant time and intellectual effort in mastering the complex laws of Kodashim, often at the expense of other areas of Halakha with more immediate practical relevance. They bear the cost of this diverted intellectual labor, which this reading deems largely obsolete for performance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, students_of_halakha, payer,
    moderate, biographical, constrained, global).

% Financial and communal resources (e.g., funding for yeshivas, scholarly publications, endowed chairs) are allocated to support the study of Kodashim. This reading argues these resources are diverted from areas that could yield more tangible benefits for the community, such as social welfare or contemporary halakhic challenges.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, community_resources, payer,
    institutional, generational, trapped, global).

% Scholars who advocate for a re-prioritization of halakhic study towards currently applicable laws and away from obsolete performance-focused areas. They are often marginalized or excluded from mainstream traditional institutions, limiting their ability to influence curriculum and resource allocation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, reform_minded_scholars, excluded,
    moderate, biographical, mobile, global).

% Academics and independent thinkers who analyze the structural dynamics of religious legal systems, including resource allocation and the persistence of traditions. They observe the constraint from an external, critical perspective.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous scholarly tradition around the entirety of Jewish law, including the laws of Temple sacrifices, ensuring that knowledge is preserved across generations and is theoretically available for future restoration.
% TRANSFER_FUNCTION: Transfers scholarly prestige, institutional funding, and student intellectual effort towards the study of sacrifice laws, away from other areas of Halakha with more immediate practical application.
% ABSENT_VOICES: Reform-minded scholars and community leaders who advocate for a re-prioritization of halakhic study towards currently applicable laws are often structurally excluded from the decision-making bodies of traditional institutions.
% DISAPPEARANCE_RATIONALE: If the constraint (the emphasis on performance-only study of Kodashim) vanished overnight, scholarly focus and institutional funding would rapidly shift to other areas of Halakha, leading to a significant reorganization of religious education, curriculum, and resource allocation within traditional Jewish legal institutions.
% FOUNDING_PROBLEM: To preserve the entirety of the Oral Law, including laws pertaining to the Temple sacrifices, even in its absence, ensuring continuity of knowledge and theoretical readiness for a future restoration of the Temple.
% FOUNDING_PROBLEM_CORROBORATION: Traditional institutions and scholars attest the problem of knowledge preservation is still live. However, this 'performance_only' reading argues that the *performance* aspect of the founding problem is dead without the Temple, and independent critical analyses of resource allocation in religious education support the view that the emphasis on Kodashim study is disproportionate to its current applicability, suggesting the original problem's *performance* dimension is no longer relevant.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because significant resources (time, funding, prestige) are diverted to a field whose practical application is suspended, creating a net cost for students and the community. Suppression is moderate (0.65) due to strong institutional norms, career path dependencies for scholars, and the marginalization of dissenting voices. The theater ratio is moderate-high (0.55) as the 'performance' of studying these laws is largely symbolic without the actual Temple, yet it maintains the appearance of continuity and adherence to tradition. Accessibility collapse is moderate (0.45) as alternative scholarly pursuits exist but are less prestigious within the traditional framework, and resistance is low (0.30) due to the difficulty of challenging established religious academic structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, the constraint is a vital mechanism for preserving tradition and ensuring continuity. From the perspective of the victims and excluded parties, it operates as an extractive mechanism, maintaining an obsolete focus at significant cost. The engine's classification as a Snare reflects the latter, highlighting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional halakhic institutions and scholars of Kodashim are beneficiaries (low d) as they gain prestige, funding, and career stability from the continued emphasis on this study. Students of Halakha and community resources are victims (high d) as they bear the costs of diverted intellectual and financial capital. Reform-minded scholars are excluded, their voices suppressed by the dominant institutional framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_of_obsolete_study,
    'Does the continued study of Kodashim, despite its lack of immediate practical application, serve an unacknowledged but genuine function (e.g., identity formation, spiritual discipline, abstract legal reasoning training) that offsets its extractive costs?',
    'Qualitative sociological and pedagogical studies on the impact of Kodashim study on student development and community identity, compared against alternative curricula.',
    'If significant unacknowledged benefits are found, the constraint''s effective extractiveness might be lower, potentially reclassifying it as a Tangled Rope (genuine coordination + extraction) rather than a Snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_of_obsolete_study, empirical, 'Assessing the latent functions of studying obsolete laws.').

omega_variable(
    resource_allocation_efficiency,
    'What is the actual opportunity cost of diverting communal and intellectual resources to the study of Kodashim compared to investing in other areas of Halakha or community needs?',
    'Detailed economic analysis of resource flows within traditional Jewish legal institutions, comparing investment in Kodashim study to other fields and their societal impact.',
    'A high opportunity cost would strengthen the Snare classification by demonstrating clear, measurable extraction. A lower cost might suggest the extraction is less severe, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Measuring the efficiency of resource allocation in religious education.').

omega_variable(
    kernel_interpretation_ambiguity,
    'Is the commandment truly suspended (performance_only), merely deferred (messianic_deferral), or transformed into study (study_as_performance)?',
    'This is a conceptual ambiguity inherent to the kernel, resolvable only through a choice of interpretive framework or a shift in theological consensus.',
    'The classification of this constraint (and its siblings) depends entirely on which reading of the kernel is adopted. A shift to ''study_as_performance'' would likely lower extractiveness for students, while ''messianic_deferral'' might maintain a higher sense of future utility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'The fundamental interpretive disagreement over the status of Kodashim commandments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.45).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.5).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__performance_only, theater_ratio, 60, 0.52).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__performance_only, theater_ratio, 80, 0.54).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__performance_only, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__performance_only, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__performance_only, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__performance_only, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__performance_only, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__performance_only, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__performance_only, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
