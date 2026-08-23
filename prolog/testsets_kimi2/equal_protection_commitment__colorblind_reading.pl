% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Colorblind Reading of Equal Protection: State Racial Classification as Constitutional Violation
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the colorblind reading of the equal
 *   protection kernel: the constitutional commitment that the state may never
 *   classify individuals by race. From this reading's perspective, the
 *   standing arrangement under contest is the regime of race-conscious state
 *   programs (affirmative action, diversity admissions) that classify
 *   applicants by race and distribute opportunities differentially. The
 *   reading assesses this arrangement as a snare: racial classification is
 *   inherently extractive, imposing costs on asian_white_applicants who are
 *   denied admission or disadvantaged because of their race, while
 *   race_conscious_institutions administer the classification scheme. The
 *   coordination story (diversity, remediation) is cover for a structure that
 *   depends on active enforcement and the suppression of race-neutral
 *   alternatives. The claim/metric independence is maintained: the reading
 *   claims the arrangement is a snare, and the metrics are authored to
 *   describe the actual operation of race-conscious classification as
 *   moderate-high extraction with substantial suppression and resistance.
 *
 * KEY AGENTS:
 *   - asian_white_applicants (payer/organized/constrained) â bear the direct cost of racial classification in admissions and contracting
 *   - race_conscious_institutions (agenda_setter/institutional/constrained) â design and enforce the race-conscious classification scheme
 *   - underrepresented_minority_applicants (beneficiary/organized/constrained) â receive differential access through the classification regime
 *   - colorblind_advocacy_groups (observer/organized/analytical) â litigate to dismantle the regime from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.45).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.72).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Colorblind Reading of Equal Protection: State Racial Classification as Constitutional Violation").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '54660459-98b2-4746-87ec-12092bf20416').
narrative_ontology:cs_kernel_codification('54660459-98b2-4746-87ec-12092bf20416', fixed_text).
narrative_ontology:cs_authority_grounding('54660459-98b2-4746-87ec-12092bf20416', lineage).
narrative_ontology:cs_interpretation_layer_present('54660459-98b2-4746-87ec-12092bf20416').
narrative_ontology:cs_reading_relation('54660459-98b2-4746-87ec-12092bf20416', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_reading_relation('54660459-98b2-4746-87ec-12092bf20416', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_axiom('54660459-98b2-4746-87ec-12092bf20416', foundational, state_racial_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_racial_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('54660459-98b2-4746-87ec-12092bf20416', state_racial_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('54660459-98b2-4746-87ec-12092bf20416', secondary, racial_preferences_constitute_invidious_discrimination).
narrative_ontology:cs_axiom_status(racial_preferences_constitute_invidious_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('54660459-98b2-4746-87ec-12092bf20416', racial_preferences_constitute_invidious_discrimination, deontological).
narrative_ontology:cs_reference_frame('54660459-98b2-4746-87ec-12092bf20416', colorblind_constitutional_order).
narrative_ontology:cs_drift_state('54660459-98b2-4746-87ec-12092bf20416', post_sffa_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('54660459-98b2-4746-87ec-12092bf20416', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, asian_white_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applicants to selective public and private universities who are denied admission or placed at a structural disadvantage because admissions offices classify them by race and apply differential standards. They bear the direct cost of the state's racial classification system, with exit options limited to applying elsewhere or engaging in lengthy constitutional litigation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_white_applicants, payer,
    organized, biographical, constrained, national).

% Universities and state agencies that design, administer, and defend race-conscious admissions and contracting programs. They set the racial classification criteria, monitor compliance with diversity targets, and enforce the differential treatment regime through internal policy, admissions practice, and legal defense.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Applicants who receive preference in admissions or contracting because they fall within the institution's racial classification categories. They are the intended beneficiaries of the race-conscious regime, though the colorblind reading construes this benefit as extracted from other applicants through an unconstitutional classification scheme.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants, beneficiary,
    organized, biographical, constrained, national).

% Legal advocacy organizations that challenge race-conscious programs on equal protection grounds. They observe and litigate against the constraint from an analytical seat, seeking to dismantle the racial classification system through judicial review.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_advocacy_groups, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the distribution of educational and economic opportunities across racial lines to achieve demographic representation or remedy historical exclusion.
% TRANSFER_FUNCTION: Moves admission slots, contracting opportunities, and institutional resources from asian_white_applicants to underrepresented_minority_applicants, mediated by race_conscious_institutions' racial classification schemes.
% ABSENT_VOICES: Asian_white_applicants and colorblind_advocacy_groups are structurally excluded from the policy-design processes within race_conscious_institutions; their objections are treated as legally settled or morally illegitimate within diversity-framework deliberations.
% DISAPPEARANCE_RATIONALE: If race-conscious classification vanished, university admissions and state contracting would reorganize around race-neutral criteria; the current demographic distribution of selective-admissions seats would shift, and institutional diversity frameworks would collapse.
% FOUNDING_PROBLEM: Historical racial subordination and systematic exclusion of Black Americans and other minorities from higher education and public contracting.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil rights organizations attest to the founding problem, but colorblind_advocacy_groups and independent social scientists outside the beneficiary set contest that the current arrangement continues to address that problem rather than perpetuating racial classification for its own sake.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.45) because the colorblind reading treats every instance of racial classification as a harm regardless of compensatory intent; the cost is borne by applicants excluded on racial grounds. Suppression is high (0.72) because race-conscious programs require active legal defense, institutional compliance machinery, and the marginalization of race-neutral alternatives (class-based, socioeconomic) that would achieve similar coordination without classification. Theater_ratio is moderate (0.50) because the diversity rationale functions as a publicly acceptable cover for a structure that the reading treats as raw extraction. Resistance is high (0.72) reflecting sustained constitutional litigation (Bakke, Grutter, Fisher, SFFA) that challenges the regime's legitimacy. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The race_conscious_institutions seat and the asian_white_applicant seat compute radically different types. From the institutional seat, the arrangement is legitimate coordination achieving educational and social objectives; from the applicant seat, it is state-mandated racial discrimination. The engine computes this divergence from the structural data: same scope, different power/exit, and opposed beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Asian_white_applicants are declared victims (payer role) because they bear the cost of exclusion under racial classification; their directionality is near full target. Race_conscious_institutions are agenda_setters because they administer and enforce the classification; their directionality is ambiguous â they are not the ultimate beneficiaries of the extraction (they do not personally collect the admission slots) but they control the machinery. Underrepresented_minority_applicants are beneficiaries (low directionality) because the constraint transfers value to them. Colorblind_advocacy_groups are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification is not a scaffold (no sunset clause) and not a rope (the coordination story is cover, not genuine coordination from this reading's perspective). The founding problem â historical racial subordination â is contested as to whether it remains live; if dead, the constraint would be a piton. However, the reading views the arrangement as actively harmful (not merely inertial), so snare is the structurally true claim despite the contested founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_inherent_vs_instrumental_harm,
    'Does the colorblind reading treat racial classification as inherently wrongful regardless of outcomes, or as instrumentally harmful based on distributive effects?',
    'Examination of doctrinal premises and judicial opinions; if inherent, epsilon is driven by deontological status; if instrumental, epsilon depends on empirical outcome measures.',
    'Would reclassify the grounding of the extraction from deontological to empirically contingent, potentially altering the axiom status in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_inherent_vs_instrumental_harm, conceptual, 'Whether the harm of racial classification is deontological or instrumental.').

omega_variable(
    committer_kernel_contest,
    'This constraint is the colorblind reading of the equal protection kernel; the remedial and diversity readings instantiate mutually exclusive constraints with different victim/beneficiary structures.',
    'Comparative analysis of all three reading files; no single resolution within this file.',
    'The epsilon and victim/beneficiary sets are reading-indexed; a different reading would invert or reassign the directionalities entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Reading-indexed nature of the constraint within the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of race-neutral alternatives structural (legal mandates and judicial precedent requiring race-conscious programs) or internalized (institutional belief that diversity requires racial classification)?',
    'Post-SFFA institutional responses: if universities abandon race-consciousness quickly, suppression was primarily legal; if they resist through workaround policies (essays, proxies), suppression was internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the institutions carry the suppression with them after legal removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__colorblind_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__colorblind_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(equa_tr_t50, equal_protection_commitment__colorblind_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__colorblind_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__colorblind_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(equa_be_t50, equal_protection_commitment__colorblind_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__colorblind_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__colorblind_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(equa_su_t50, equal_protection_commitment__colorblind_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is the colorblind reading of the equal_protection_commitment kernel, decomposed from the diversity and remedial readings per the epsilon-invariance principle. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
