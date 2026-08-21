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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Halakhic Suspension of Temple Sacrifice Laws (Performance-Only Reading)
 *   domain: religious/halakhic_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the status
 *   of Kodashim (Temple sacrifice laws) in the absence of the Temple. It
 *   asserts that these laws are contingent on the Temple's physical
 *   existence, and without an altar, the commandment for actual performance
 *   is suspended. While this reading provides a halakhic framework for the
 *   diaspora, it leads to a situation where significant intellectual and
 *   institutional resources are dedicated to the theoretical study of
 *   non-performable laws, creating a 'husk' of a commandment that extracts
 *   from its adherents without a corresponding functional output.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.8).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.6).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.8).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Halakhic Suspension of Temple Sacrifice Laws (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '13c33fe5-b805-4fc9-9c0a-76fa94eab97e').
narrative_ontology:cs_kernel_codification('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', fixed_text).
narrative_ontology:cs_authority_grounding('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', lineage).
narrative_ontology:cs_interpretation_layer_present('13c33fe5-b805-4fc9-9c0a-76fa94eab97e').
narrative_ontology:cs_reading_relation('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', foundational, temple_contingency_of_sacrifice).
narrative_ontology:cs_axiom_status(temple_contingency_of_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', temple_contingency_of_sacrifice, conventional).
narrative_ontology:cs_axiom('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', foundational, performance_is_literal).
narrative_ontology:cs_axiom_status(performance_is_literal, holdable).
narrative_ontology:cs_axiom_grounding('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', performance_is_literal, conventional).
narrative_ontology:cs_reference_frame('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', halakhic_continuity_in_exile).
narrative_ontology:cs_drift_state('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13c33fe5-b805-4fc9-9c0a-76fa94eab97e', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, kodashim_scholars).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, students_of_halakha).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, broader_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the academic and interpretive tradition of Kodashim (laws of sacrifices), even in the absence of the Temple. Their careers, institutional funding, and scholarly prestige are often built upon this continued focus, despite the laws being non-performable.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, kodashim_scholars, beneficiary,
    institutional, generational, constrained, global).

% Invest significant time and intellectual effort into studying sacrifice laws, which this reading deems currently non-performable. Their intellectual resources are diverted from other areas of halakhic study that might have more immediate practical application.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, students_of_halakha, payer,
    moderate, biographical, constrained, global).

% Indirectly supports institutions and scholars focused on Kodashim, potentially at the expense of other areas of Jewish law, social action, or spiritual development, based on the perceived importance of this study within the tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, broader_jewish_community, payer,
    organized, generational, constrained, global).

% Advocate for immediate rebuilding of the Temple and resumption of sacrifices, viewing the current state as a temporary deferral, not a suspension of the commandment's *performance* aspect. This reading directly contradicts their active pursuit and is often marginalized in traditional discourse.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, messianic_activists, excluded,
    organized, generational, identity_locked, global).

% Analyze the practical and ethical implications of continued focus on Kodashim in the absence of the Temple, advocating for re-prioritization of halakhic study towards currently applicable commandments. They are often outside the mainstream institutions that benefit from the current focus.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_reformers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, kodashim_scholars).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading asserts that the actual performance of sacrifice laws is suspended due to the absence of the Temple, thus preventing coordination around actual sacrificial rituals. The constraint itself is about the *status* of these laws, not their active coordination.
% TRANSFER_FUNCTION: Diverts intellectual and institutional resources (time, funding, scholarly prestige) from other areas of halakhic study or communal needs towards the theoretical study of sacrifice laws, which are currently non-performable according to this reading.
% ABSENT_VOICES: Messianic activists (who believe in immediate restoration of Temple service) and halakhic reformers (who advocate for re-prioritization of study) are often marginalized or excluded from the mainstream discourse that perpetuates the focus on Kodashim, as their views challenge the underlying premises of this reading.
% DISAPPEARANCE_RATIONALE: If the interpretation that sacrifice laws are *performance-only* and thus suspended without the Temple vanished, it would lead to a massive re-evaluation of scholarly priorities within Jewish institutions. Intellectual and financial resources would likely be redirected, and the landscape of halakhic study would shift significantly, either towards other areas of law or towards active preparation for Temple rebuilding.
% FOUNDING_PROBLEM: To provide a halakhic framework for Jewish life and worship in the absence of the Temple, specifically regarding the status of commandments related to Temple service that cannot be performed.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream rabbinic authorities and historical halakhic texts attest to the ongoing need to define the status of these laws in the absence of the Temple. While the specific 'performance-only' interpretation is debated, the underlying problem of how to relate to these non-performable laws is universally acknowledged across different Jewish movements.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because resources (time, intellectual effort, funding) are diverted to a practice that, by this reading's own logic, cannot be performed. The 'husk' nature means the investment yields no direct halakhic performance. Theater ratio is high because the 'performance' of the commandment is reduced to theoretical study or ritualistic contemplation, rather than actual sacrificial acts. Suppression is moderate, as it's primarily cultural and institutional pressure to maintain a traditional focus, rather than overt coercion. Resistance is low because this reading itself is a widely accepted halakhic position, and the 'victims' are often deeply committed to the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Kodashim scholars, this reading is a vital act of preserving tradition and ensuring halakhic continuity. From the perspective of students or the broader community, the same structure can be experienced as a diversion of valuable resources and intellectual energy towards an obsolete practice, potentially hindering engagement with more practically relevant areas of Jewish law or social needs.
 *
 * DIRECTIONALITY LOGIC:
 *   Kodashim scholars are the primary beneficiaries, as their careers and institutional structures are supported by the continued emphasis on this area of study. Students of Halakha and the broader Jewish community are the payers, as their intellectual and financial resources are directed towards this focus. Messianic activists and halakhic reformers are excluded or marginalized, as their alternative interpretations challenge the status quo that benefits the scholars.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading itself emerged as a response to the mandatrophy of the Temple's destruction. However, by maintaining a strong focus on the theoretical aspects of non-performable laws, it creates a secondary mandatrophy: the mandate to study Kodashim persists, but its original function (preparation for actual performance) is suspended, leading to a diversion of resources that could otherwise address live problems. The constraint is a 'husk' that continues to extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''kodashim_commandment_status'' kernel. What are the structural implications of this specific ''performance_only'' reading compared to its siblings?',
    'Comparative analysis of the ''performance_only'' reading with ''study_as_performance'' and ''messianic_deferral'' readings, focusing on resource allocation and perceived halakhic obligation.',
    'If a sibling reading were adopted, the extractiveness and theater ratio of the constraint would shift significantly, as the perceived function and value of Kodashim study would change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading within a contested kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative halakhic priorities structural (institutional funding, academic prestige, curriculum design) or internalized (a deeply held belief within the community that Kodashim study is inherently superior or more meritorious)?',
    'Post-exit suppression trajectory: if scholars or students who shift focus to other halakhic areas continue to experience internal or external pressure, it indicates a mix of structural and internalized suppression. Analysis of institutional funding patterns and academic hiring practices.',
    'If internalized suppression is a significant factor, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint''s influence with them even when external barriers are reduced. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in halakhic prioritization.').

omega_variable(
    resource_diversion_quantification,
    'What is the quantifiable amount of intellectual and financial resources diverted to the theoretical study of Kodashim compared to other areas of halakhic study or communal needs?',
    'Detailed analysis of rabbinic academy curricula, academic publications, institutional budgets, and philanthropic allocations over time.',
    'A high quantifiable diversion would strengthen the argument for high extractiveness and the ''snare'' classification, demonstrating a significant cost borne by the community for a non-performable practice. A lower diversion might suggest a more balanced allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_quantification, empirical, 'Quantifies the resource diversion inherent in the ''performance-only'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1970, kodashim_commandment_status__performance_only, theater_ratio, 1970, 0.5).
narrative_ontology:measurement(koda_tr_t1980, kodashim_commandment_status__performance_only, theater_ratio, 1980, 0.58).
narrative_ontology:measurement(koda_tr_t1990, kodashim_commandment_status__performance_only, theater_ratio, 1990, 0.65).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__performance_only, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(koda_tr_t2010, kodashim_commandment_status__performance_only, theater_ratio, 2010, 0.69).
narrative_ontology:measurement(koda_tr_t2020, kodashim_commandment_status__performance_only, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(koda_be_t1970, kodashim_commandment_status__performance_only, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(koda_be_t1980, kodashim_commandment_status__performance_only, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(koda_be_t1990, kodashim_commandment_status__performance_only, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__performance_only, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(koda_be_t2010, kodashim_commandment_status__performance_only, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(koda_be_t2020, kodashim_commandment_status__performance_only, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1970, kodashim_commandment_status__performance_only, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(koda_su_t1980, kodashim_commandment_status__performance_only, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(koda_su_t1990, kodashim_commandment_status__performance_only, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__performance_only, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(koda_su_t2010, kodashim_commandment_status__performance_only, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(koda_su_t2020, kodashim_commandment_status__performance_only, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, halakhic_curriculum_prioritization).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, jewish_communal_resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. The 'performance_only' reading focuses on the suspension of actual sacrifice, leading to resource diversion. Sibling readings ('study_as_performance' and 'messianic_deferral') offer alternative interpretations of the commandment's status and implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
