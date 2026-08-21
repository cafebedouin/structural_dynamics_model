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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Race-Conscious Action
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'remedial' reading of the Equal Protection
 *   Clause, which permits race-conscious state action when narrowly tailored
 *   to remedy documented historical exclusion or achieve a compelling
 *   diversity interest. This reading allows universities to consider race as
 *   a 'plus factor' in admissions. Historically excluded groups are
 *   beneficiaries, while rejected applicants and state actors facing
 *   litigation are victims. The state has an affirmative obligation to
 *   document the remedial purpose and narrow tailoring of such policies. This
 *   is one reading of the broader 'equal_protection_kernel', which also
 *   includes 'colorblind_reading' and 'antisubordination_reading'.
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
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, 'eb75d2be-cba3-4bb7-8540-81f21f48c637').
narrative_ontology:cs_kernel_codification('eb75d2be-cba3-4bb7-8540-81f21f48c637', fixed_text).
narrative_ontology:cs_authority_grounding('eb75d2be-cba3-4bb7-8540-81f21f48c637', lineage).
narrative_ontology:cs_interpretation_layer_present('eb75d2be-cba3-4bb7-8540-81f21f48c637').
narrative_ontology:cs_reading_relation('eb75d2be-cba3-4bb7-8540-81f21f48c637', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb75d2be-cba3-4bb7-8540-81f21f48c637', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('eb75d2be-cba3-4bb7-8540-81f21f48c637', foundational, race_conscious_action_permissible_for_remedy_or_diversity).
narrative_ontology:cs_axiom_status(race_conscious_action_permissible_for_remedy_or_diversity, holdable).
narrative_ontology:cs_axiom_grounding('eb75d2be-cba3-4bb7-8540-81f21f48c637', race_conscious_action_permissible_for_remedy_or_diversity, conventional).
narrative_ontology:cs_axiom('eb75d2be-cba3-4bb7-8540-81f21f48c637', secondary, strict_scrutiny_applies_to_racial_classifications).
narrative_ontology:cs_axiom_status(strict_scrutiny_applies_to_racial_classifications, holdable).
narrative_ontology:cs_axiom_grounding('eb75d2be-cba3-4bb7-8540-81f21f48c637', strict_scrutiny_applies_to_racial_classifications, conventional).
narrative_ontology:cs_reference_frame('eb75d2be-cba3-4bb7-8540-81f21f48c637', bakke_grutter_framework).
narrative_ontology:cs_drift_state('eb75d2be-cba3-4bb7-8540-81f21f48c637', sfaf_harvard_unc_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('eb75d2be-cba3-4bb7-8540-81f21f48c637', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_seeking_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, state_actors_facing_litigation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from state actions designed to remedy past discrimination, such as affirmative action in university admissions. Their access to opportunities is enhanced, but they remain dependent on the state's willingness and ability to implement such policies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Utilize race-conscious policies to achieve a diverse student body, which they view as essential for educational enrichment. They bear the administrative burden and legal risk of designing and defending these narrowly tailored programs.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Are denied admission to institutions where race is considered a 'plus factor,' even if they might have been admitted under a strictly race-blind process. They bear the direct cost of missed opportunities and often resort to litigation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process, payer,
    powerless, immediate, trapped, national).

% Bear the legal and political costs of defending race-conscious policies against challenges. They must meticulously document remedial purposes and demonstrate narrow tailoring, diverting resources from other priorities.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_actors_facing_litigation, payer,
    institutional, biographical, constrained, national).

% The ultimate arbiter of Equal Protection Clause interpretation, setting the legal boundaries for race-conscious state action. Its rulings shape the constraint's application and enforcement.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Advocate for policies that address historical discrimination and promote diversity. They benefit from the legal framework that permits remedial race-conscious action, aligning with their mission.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocate for strictly race-neutral policies, believing that any racial classification is discriminatory. They are often excluded from the policy-making process for race-conscious programs, but actively challenge them in court.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, equal_opportunity_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state efforts to address the lingering effects of historical racial discrimination and achieve the educational benefits of diversity, providing a legal framework for race-conscious policies.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., university admissions slots) from individuals who might otherwise have received them to members of historically excluded groups, based on race as a 'plus factor.' It also transfers legal and administrative burdens to state actors.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Equal Protection Clause are often absent from the design phase of remedial policies, only engaging through litigation. Their perspective, if fully integrated, would fundamentally alter the constraint's structure.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state actors would likely cease all race-conscious programs to avoid legal challenges, leading to a significant decrease in diversity in higher education and other sectors. The legal landscape for civil rights would fundamentally shift, and the mechanisms for addressing historical discrimination would be severely curtailed.
% FOUNDING_PROBLEM: The problem of persistent racial inequality and the legacy of systemic discrimination, which race-neutral policies alone were deemed insufficient to overcome.
% FOUNDING_PROBLEM_CORROBORATION: Sociological studies, economic data on wealth gaps, and ongoing disparities in educational outcomes corroborate the persistence of racial inequality. Legal scholars and civil rights organizations outside the direct beneficiaries attest to the ongoing need for remedial measures.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the transfer of opportunities and the administrative burden on state actors, balanced against the perceived benefits of diversity. Suppression (0.6) is moderate, as the constraint is actively enforced through litigation and judicial review, but not absolute, as challenges can succeed. The theater ratio (0.2) indicates that while there is genuine effort to achieve remedial goals, some aspects of 'narrow tailoring' can become performative to withstand legal scrutiny. The values reflect the period from Regents of the University of California v. Bakke (1978) to Students for Fair Admissions v. Harvard/UNC (2023).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically excluded groups, this constraint is a necessary (though imperfect) mechanism for justice and equity. From the perspective of rejected applicants, it is an unfair imposition that denies them opportunities based on race. The Supreme Court's perspective is one of balancing competing constitutional interests, often leading to a 'tangled rope' outcome where some coordination occurs alongside extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and universities seeking diversity are beneficiaries, as the constraint enables policies that serve their interests. Rejected applicants and state actors defending policies are payers, bearing the direct costs. The Supreme Court acts as an agenda-setter, defining the legal boundaries. Civil rights advocates are beneficiaries, while equal opportunity advocates are excluded from the policy-making process but actively resist through legal challenges.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_tailoring_efficacy,
    'Is ''narrow tailoring'' an effective mechanism for achieving remedial or diversity goals without undue burden on other groups, or is it primarily a legal fiction to permit race-conscious action?',
    'Empirical studies comparing outcomes of narrowly tailored programs with race-neutral alternatives, assessing both diversity metrics and impact on non-beneficiary groups.',
    'If found ineffective or overly burdensome, the constraint''s legitimacy would erode, potentially leading to a reclassification towards a Snare or Piton. If highly effective, it would strengthen the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_efficacy, empirical, 'Assesses the practical effectiveness and fairness of the ''narrow tailoring'' requirement.').

omega_variable(
    compelling_interest_definition,
    'What constitutes a ''compelling diversity interest'' that justifies race-conscious state action, and is this definition stable or subject to political and judicial reinterpretation?',
    'Analysis of judicial opinions and legislative debates over time, tracking shifts in the definition and application of ''compelling interest'' across different contexts.',
    'If the definition is unstable or easily manipulated, the constraint''s predictability and fairness would decrease, increasing its extractiveness for those caught in shifting legal sands. If stable, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_definition, conceptual, 'Examines the stability and scope of the ''compelling interest'' justification for race-conscious policies.').

omega_variable(
    remedial_purpose_documentation,
    'Is the state''s documentation of historical exclusion genuinely robust and causally linked to current disparities, or is it often a pro forma exercise to justify desired outcomes?',
    'Independent audits of state-level documentation processes and their evidentiary basis, comparing documented claims to historical and sociological research.',
    'If documentation is weak or performative, the constraint''s theater_ratio would increase, and its legitimacy as a remedial tool would be undermined, pushing it towards a Piton or Snare. Strong documentation reinforces its stated purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_purpose_documentation, empirical, 'Evaluates the rigor and sincerity of the state''s justification for remedial action.').


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
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__remedial_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__remedial_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__remedial_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__remedial_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__remedial_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__remedial_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, affirmative_action_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equal_protection_kernel', each representing a distinct interpretation of the Equal Protection Clause. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
