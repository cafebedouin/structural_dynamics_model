% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Race-Conscious Action
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'remedial' reading of the Equal Protection
 *   Clause, which permits race-conscious state action when narrowly tailored
 *   to remedy documented historical exclusion or achieve a compelling
 *   diversity interest. This reading allows universities to consider race as
 *   a 'plus factor' in admissions. It is one of several competing
 *   interpretations of the Equal Protection Clause, each with distinct
 *   implications for civil rights and education policy. The metrics reflect
 *   the ongoing contestation and the administrative burden of maintaining
 *   such policies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.45).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.6).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause: Remedial Race-Conscious Action").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '979f0e28-bc25-4fe2-a5de-0df72707b82b').
narrative_ontology:cs_kernel_codification('979f0e28-bc25-4fe2-a5de-0df72707b82b', fixed_text).
narrative_ontology:cs_authority_grounding('979f0e28-bc25-4fe2-a5de-0df72707b82b', lineage).
narrative_ontology:cs_interpretation_layer_present('979f0e28-bc25-4fe2-a5de-0df72707b82b').
narrative_ontology:cs_reading_relation('979f0e28-bc25-4fe2-a5de-0df72707b82b', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('979f0e28-bc25-4fe2-a5de-0df72707b82b', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('979f0e28-bc25-4fe2-a5de-0df72707b82b', foundational, race_conscious_action_permissible_for_remedy_or_diversity).
narrative_ontology:cs_axiom_status(race_conscious_action_permissible_for_remedy_or_diversity, holdable).
narrative_ontology:cs_axiom_grounding('979f0e28-bc25-4fe2-a5de-0df72707b82b', race_conscious_action_permissible_for_remedy_or_diversity, conventional).
narrative_ontology:cs_axiom('979f0e28-bc25-4fe2-a5de-0df72707b82b', secondary, narrow_tailoring_and_compelling_interest_required).
narrative_ontology:cs_axiom_status(narrow_tailoring_and_compelling_interest_required, holdable).
narrative_ontology:cs_axiom_grounding('979f0e28-bc25-4fe2-a5de-0df72707b82b', narrow_tailoring_and_compelling_interest_required, conventional).
narrative_ontology:cs_reference_frame('979f0e28-bc25-4fe2-a5de-0df72707b82b', post_bakke_framework).
narrative_ontology:cs_drift_state('979f0e28-bc25-4fe2-a5de-0df72707b82b', contemporary_sfafa_ruling, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('979f0e28-bc25-4fe2-a5de-0df72707b82b', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_seeking_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, state_actors_facing_litigation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from state action that remedies past discrimination or promotes diversity, gaining access to opportunities that might otherwise be denied. Their ability to exit systemic disadvantage is constrained by historical factors.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Administer admissions policies that consider race as a 'plus factor' to achieve a compelling diversity interest, or to remedy documented past discrimination. They face legal challenges and must demonstrate narrow tailoring and compelling interest.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of race-conscious policies when they are denied admission to institutions they might have entered under a strictly race-blind process. Their exit options are limited to other institutions or legal challenge.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process, payer,
    moderate, biographical, constrained, national).

% Bear the legal and administrative costs of defending race-conscious policies in court, including documenting remedial purpose and narrow tailoring. Their options are to cease race-conscious action or continue to litigate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_actors_facing_litigation, payer,
    institutional, immediate, constrained, national).

% Advocate for a strictly colorblind interpretation of the Equal Protection Clause, arguing that any racial classification is unconstitutional. They are excluded from the direct administration of remedial policies but actively challenge them in courts and public discourse.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Observe and critique the remedial reading, arguing that it does not go far enough to dismantle systemic racial hierarchy and often focuses on individual harm rather than structural inequality. They seek a broader interpretation of the Equal Protection Clause.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, antisubordination_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state efforts to address historical racial inequality and achieve educational diversity, providing a legal framework for race-conscious policies that might otherwise be challenged as discriminatory.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., university admissions slots) from individuals who might have received them under a race-blind system to individuals from historically excluded groups, in pursuit of remedial or diversity goals.
% ABSENT_VOICES: Those who advocate for a strictly colorblind interpretation of the Constitution are structurally excluded from the policy-making process that implements remedial race-conscious action, though they are active in litigation. Their arguments for a categorical ban on racial classifications are not accommodated by this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, universities and other state actors would likely cease race-conscious programs to avoid legal challenge, leading to a significant shift in the racial composition of student bodies and other institutions. Efforts to address historical exclusion would need to find entirely race-neutral means, fundamentally altering the landscape of civil rights enforcement.
% FOUNDING_PROBLEM: The problem of persistent racial inequality and the legacy of de jure segregation, which race-neutral policies alone were deemed insufficient to overcome.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, educational researchers, and many legal scholars attest that the problem of racial inequality remains live, citing ongoing disparities in educational attainment and socioeconomic status. Opponents argue the problem is largely solved or that race-conscious remedies exacerbate division.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).
:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as it reallocates opportunities, creating identifiable victims among those who would benefit from a strictly race-blind process. Suppression (0.6) is present due to the active legal enforcement required to defend these policies against challenges and the suppression of alternative, race-neutral approaches. Theater ratio (0.2) is low, as the policies are genuinely intended to achieve their stated goals, though the administrative burden of 'narrow tailoring' can sometimes feel performative. Resistance (0.7) is high, reflecting persistent legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically excluded groups, this reading is a necessary (though perhaps insufficient) tool for justice. From the perspective of rejected applicants, it is an unfair imposition. The institutional actors (universities, state actors) experience it as a complex legal tightrope walk. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and universities seeking diversity are beneficiaries, as the constraint enables their goals. Rejected applicants and state actors defending policies are payers, bearing the direct costs. Colorblind advocates are excluded, as their core premise is not accommodated by this reading. Antisubordination advocates are observers, critiquing its scope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_definition,
    'What constitutes a ''compelling diversity interest'' and how is it empirically demonstrated?',
    'Further Supreme Court rulings or legislative action providing clearer, more objective criteria for what qualifies as ''compelling'' and how its achievement is measured.',
    'A narrower definition would increase extractiveness for beneficiaries and reduce the scope of permissible race-conscious action; a broader definition would have the opposite effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_definition, conceptual, 'Ambiguity in the legal standard for ''compelling interest''.').

omega_variable(
    narrow_tailoring_burden,
    'Is the burden of demonstrating ''narrow tailoring'' genuinely achievable without making race-conscious policies practically impossible to implement?',
    'Empirical study of administrative costs and success rates of narrowly tailored programs across various institutions and jurisdictions.',
    'If the burden is found to be prohibitive, the constraint effectively becomes a snare for universities, forcing them to abandon diversity goals or engage in extensive, costly litigation for minimal gain. If manageable, it remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_burden, empirical, 'The practical feasibility and cost of meeting the ''narrow tailoring'' requirement.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint best framed as a ''remedial'' reading, or is it better understood as a ''diversity'' reading, with distinct justifications and implications?',
    'Analysis of judicial opinions and legal scholarship to determine whether the ''remedial'' and ''diversity'' rationales are treated as distinct or overlapping, and which predominates in practice.',
    'If primarily a ''diversity'' reading, the focus shifts from past harm to future benefit, potentially altering the scope of beneficiaries and victims. If primarily ''remedial'', the emphasis on documented historical exclusion becomes paramount.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Conceptual distinction between remedial and diversity rationales within race-conscious action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__remedial_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__remedial_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__remedial_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__remedial_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__remedial_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__remedial_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
