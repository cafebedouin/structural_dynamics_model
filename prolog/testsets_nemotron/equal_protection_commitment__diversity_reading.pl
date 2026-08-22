% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection Diversity Reading: Race as One Factor for Educational Diversity
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint story models the 'diversity reading' of the Equal
 *   Protection Clause — the doctrinal position that race may be considered as
 *   one factor among many in university admissions to achieve the educational
 *   benefits of diversity, as articulated in Bakke (Powell), Grutter, and
 *   Fisher. It is one reading of the contested kernel
 *   'equal_protection_commitment,' alongside the colorblind_reading and
 *   remedial_reading. The diversity reading has been the controlling
 *   framework since 1978 but faces mounting challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.35).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Reading: Race as One Factor for Educational Diversity").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'f411f393-f1c3-47f8-8314-d5c209c18788').
narrative_ontology:cs_kernel_codification('f411f393-f1c3-47f8-8314-d5c209c18788', formalized).
narrative_ontology:cs_authority_grounding('f411f393-f1c3-47f8-8314-d5c209c18788', lineage).
narrative_ontology:cs_interpretation_layer_present('f411f393-f1c3-47f8-8314-d5c209c18788').
narrative_ontology:cs_reading_relation('f411f393-f1c3-47f8-8314-d5c209c18788', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('f411f393-f1c3-47f8-8314-d5c209c18788', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('f411f393-f1c3-47f8-8314-d5c209c18788', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('f411f393-f1c3-47f8-8314-d5c209c18788', educational_diversity_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('f411f393-f1c3-47f8-8314-d5c209c18788', foundational, race_as_one_factor_permissible).
narrative_ontology:cs_axiom_status(race_as_one_factor_permissible, holdable).
narrative_ontology:cs_axiom_grounding('f411f393-f1c3-47f8-8314-d5c209c18788', race_as_one_factor_permissible, conventional).
narrative_ontology:cs_reference_frame('f411f393-f1c3-47f8-8314-d5c209c18788', bakke_powell_framework).
narrative_ontology:cs_drift_state('f411f393-f1c3-47f8-8314-d5c209c18788', post_sffa_v_harvard, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f411f393-f1c3-47f8-8314-d5c209c18788', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, diversity_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, high_achieving_underrepresented_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, high_achieving_underrepresented_groups).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, institutional_autonomy_in_admissions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer holistic admissions programs that consider race as one factor among many. Gain discretion to shape student bodies for educational mission. Bear compliance costs and litigation risk. Exit options constrained by mission commitments and accreditation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, universities, beneficiary).

% Litigate, lobby, and organize to defend the diversity framework. Collect donations and membership from constituents who value racially diverse campuses. Can redirect advocacy to other frameworks if doctrine shifts.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, diversity_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Subject to holistic review where race may be a factor; individual claims are obscured by multi-factor analysis. Cannot know whether race helped or hurt their specific application. Exit is constrained: alternative institutions may use similar frameworks, or may not admit them regardless.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants, payer,
    powerless, immediate, constrained, national).

% Experience dual position: may benefit from diversity considerations at some institutions, but face stereotype threat and mismatch hypotheses at others. Holistic review makes individual impact unknowable. Exit options constrained by same admissions landscape.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, high_achieving_underrepresented_groups, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, high_achieving_underrepresented_groups, beneficiary).

% Argue that any racial classification violates equal protection. Their view has been rejected by controlling precedent but persists in dissent and state-level bans. Would object to any race-conscious framework but are structurally excluded from the diversity regime's operation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_constitutionalists, excluded,
    organized, generational, trapped, national).

% Argue diversity framework is too weak — equal protection requires race-conscious measures to dismantle subordination, not just achieve educational benefits. Their preferred framework is not on offer in current doctrine. Can pivot to state-level advocacy or litigation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, remedial_justice_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate challenges to admissions programs, articulate standards, and enforce compliance. Composition shifts alter doctrinal trajectory. Neither collects nor pays; their rulings structure the constraint's effective extraction.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally defensible framework for universities to pursue diverse student bodies through individualized, holistic review — coordinating institutional autonomy with constitutional limits, avoiding rigid quotas while permitting race-consciousness.
% TRANSFER_FUNCTION: Moves admissions discretion from a colorblind baseline (where race is categorically forbidden) to universities, allowing them to shape class composition. The transfer is procedural: universities gain process rights; applicants bear the opacity of holistic review.
% ABSENT_VOICES: Applicants themselves — especially those denied admission — are structurally absent from the doctrinal conversation. Their individual experiences of the constraint are filtered through institutional litigants. Future students who would have been admitted under alternative frameworks are also absent.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight, universities would lose their primary constitutional basis for race-conscious admissions. Many would shift to race-neutral alternatives (percentage plans, socioeconomic factors) or face immediate litigation. The admissions landscape would reorganize around colorblind or remedial frameworks, altering student body composition nationally.
% FOUNDING_PROBLEM: After formal segregation ended, universities remained overwhelmingly white. The diversity reading was built to solve the problem of how to achieve meaningful integration without violating equal protection — a middle path between colorblindness (which freezes existing disparities) and remedial quotas (which the Court rejected in Bakke).
% FOUNDING_PROBLEM_CORROBORATION: University administrators and diversity advocates attest the problem persists: racial isolation remains, diversity benefits are empirically documented. Colorblind constitutionalists and some remedial advocates attest the problem is either solved (formal barriers gone) or mischaracterized (diversity is not the constitutional mandate — remedying subordination is). Independent social science research on diversity outcomes provides partial external corroboration for the continuing problem.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the constraint is procedural — it governs how decisions are made, not outcomes directly. Universities gain admissions discretion; applicants face opaque holistic review. Suppression (0.35) reflects active enforcement of doctrinal boundaries (narrow tailoring, no quotas, individualized review) rather than coercion of individuals. Theater ratio (0.22) captures the growing gap between the diversity rationale and actual admissions practices (e.g., index-based systems, racial balancing in practice). Accessibility collapse (0.45) is moderate: race-neutral alternatives exist but are less effective at achieving racial diversity. Resistance (0.58) is substantial: decades of litigation, state bans, and shifting Court composition.
 *
 * PERSPECTIVAL GAP:
 *   From the university seat, the constraint is genuine coordination — a workable framework for mission-driven admissions. From the applicant seat, especially rejected applicants, the same structure operates as enforced opacity — they cannot challenge what they cannot see. From the colorblind seat, it is illicit classification. The engine computes these divergent seat types from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are agenda_setters and beneficiaries (d near 0.15): they design the programs, collect the educational benefits, and bear manageable compliance costs. Diversity advocates are beneficiaries (d ~0.2): they gain a defensible framework but don't administer it. All applicants are payers (d ~0.75): they bear the opacity and potential disadvantage of race-conscious review with constrained exit. High-achieving underrepresented groups are dual-positioned (d ~0.5): potential benefit at some schools, potential harm from mismatch/stereotype at others. Colorblind constitutionalists are excluded (d ~0.9): their preferred framework is doctrinally foreclosed. Remedial advocates are excluded but less trapped (d ~0.6): their framework is not foreclosed, just not adopted. Courts are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-segregation integration) remains contested — not dead, not fully live. The diversity reading has drifted from its Bakke-era justification (remedying societal discrimination was rejected; educational diversity was the compromise). Current doctrine sustains a framework whose original compromising logic is contested from both sides. This is not pure mandatrophy — the constraint still coordinates a real function — but the coordination-extraction balance has shifted toward extraction as narrow tailoring requirements tighten while diversity benefits become harder to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_vs_practice_gap,
    'Does the operational reality of holistic review (index systems, racial balancing, mechanical plus-factors) match the doctrinal ideal of individualized consideration?',
    'Discovery in litigation (SFFA v. Harvard/UNC), empirical studies of admissions algorithms, internal university documents.',
    'If practice systematically deviates from doctrine, the constraint''s theater_ratio is higher than measured and its coordination function is degraded — moving classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_vs_practice_gap, empirical, 'Gap between doctrinal ideal and admissions practice.').

omega_variable(
    educational_benefits_measurement,
    'Are the educational benefits of diversity empirically established to the degree required by strict scrutiny''s ''compelling interest'' standard?',
    'Social science research on diversity outcomes, Court''s evolving evidentiary standards, amicus briefs in pending cases.',
    'If benefits are not sufficiently established, the compelling interest predicate collapses — the constraint loses its coordination justification and becomes pure extraction (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(educational_benefits_measurement, conceptual, 'Empirical foundation of the compelling interest.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the equal_protection_commitment kernel differ from its siblings?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, ε values, and doctrinal trajectories.',
    'Clarifies whether the kernel is a single constraint with variable readings or a family of distinct constraints. Affects network contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel-reading structural delta for diversity_reading vs. colorblind_reading and remedial_reading.').

omega_variable(
    applicant_exit_options_ambiguity,
    'Are applicants truly constrained in exit, or do race-neutral alternatives (percentage plans, socioeconomic affirmative action) provide meaningful alternatives?',
    'Comparative analysis of student body outcomes under race-neutral regimes (California, Michigan, Washington post-ban).',
    'If exit is more mobile than modeled, applicant directionality shifts toward symmetric, reducing effective extraction. If exit is trapped, extraction amplifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(applicant_exit_options_ambiguity, empirical, 'Whether applicant exit options are meaningfully constrained or functionally mobile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_commitment__diversity_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__diversity_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_commitment__diversity_reading, theater_ratio, 2016, 0.21).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.18).
narrative_ontology:measurement(equa_be_t1996, equal_protection_commitment__diversity_reading, base_extractiveness, 1996, 0.22).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.25).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__diversity_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement(equa_be_t2016, equal_protection_commitment__diversity_reading, base_extractiveness, 2016, 0.26).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(equa_su_t1996, equal_protection_commitment__diversity_reading, suppression_requirement, 1996, 0.3).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__diversity_reading, suppression_requirement, 2013, 0.34).
narrative_ontology:measurement(equa_su_t2016, equal_protection_commitment__diversity_reading, suppression_requirement, 2016, 0.35).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, university_admissions_governance).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, strict_scrutiny_framework).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_commitment constraint family. The three readings (diversity, colorblind, remedial) have different ε values (diversity ~0.28, colorblind ~0.05, remedial ~0.45), different beneficiary/victim structures, and different claimed types. They are linked because they compete for the same doctrinal space and the Court's choice among them restructures the admissions constraint landscape. The diversity reading was the controlling precedent from 1978-2023; its displacement by colorblind_reading in SFFA v. Harvard (2023) constitutes a constraint regime change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, powerless, 0.78).
constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, moderate, 0.5).
constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
