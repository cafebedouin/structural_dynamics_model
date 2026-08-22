% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Remedial Reading — Race-Conscious Remediation
 *   domain: constitutional/education/civil_rights
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause authorizes
 *   race-conscious state action — specifically university admissions — when
 *   narrowly tailored to remedy documented historical exclusion or achieve
 *   the compelling interest of student body diversity. This reading emerged
 *   in Bakke (1978), was refined in Grutter (2003) and Fisher (2013, 2016),
 *   and was effectively overruled in Students for Fair Admissions v. Harvard
 *   (2023). The constraint operates as a tangled rope: it solves a genuine
 *   coordination problem (how to integrate elite institutions after formal
 *   exclusion ended) while extracting admissions opportunities from
 *   applicants who would prevail under race-neutral criteria. The state (via
 *   courts) actively enforces the boundaries of permissible
 *   race-consciousness — programs must be narrowly tailored, time-limited,
 *   and subject to strict scrutiny.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.38).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.42).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Remedial Reading — Race-Conscious Remediation").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional/education/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '24620a68-4dd7-4450-b550-445144523aed').
narrative_ontology:cs_kernel_codification('24620a68-4dd7-4450-b550-445144523aed', fixed_text).
narrative_ontology:cs_authority_grounding('24620a68-4dd7-4450-b550-445144523aed', lineage).
narrative_ontology:cs_interpretation_layer_present('24620a68-4dd7-4450-b550-445144523aed').
narrative_ontology:cs_reading_relation('24620a68-4dd7-4450-b550-445144523aed', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('24620a68-4dd7-4450-b550-445144523aed', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('24620a68-4dd7-4450-b550-445144523aed', foundational, race_conscious_remediation_permitted).
narrative_ontology:cs_axiom_status(race_conscious_remediation_permitted, overridden).
narrative_ontology:cs_axiom_grounding('24620a68-4dd7-4450-b550-445144523aed', race_conscious_remediation_permitted, conventional).
narrative_ontology:cs_axiom('24620a68-4dd7-4450-b550-445144523aed', foundational, diversity_as_compelling_interest).
narrative_ontology:cs_axiom_status(diversity_as_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('24620a68-4dd7-4450-b550-445144523aed', diversity_as_compelling_interest, conventional).
narrative_ontology:cs_axiom('24620a68-4dd7-4450-b550-445144523aed', secondary, narrow_tailoring_requirement).
narrative_ontology:cs_axiom_status(narrow_tailoring_requirement, overridden).
narrative_ontology:cs_axiom_grounding('24620a68-4dd7-4450-b550-445144523aed', narrow_tailoring_requirement, conventional).
narrative_ontology:cs_reference_frame('24620a68-4dd7-4450-b550-445144523aed', bakke_grutter_framework).
narrative_ontology:cs_drift_state('24620a68-4dd7-4450-b550-445144523aed', post_sffa_2023, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('24620a68-4dd7-4450-b550-445144523aed', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, diversity_seeking_institutions).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, race_neutral_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, state_legislatures).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, conservative_legal_groups).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, strict_scrutiny_narrow_tailoring).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_interest_diversity).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, remedial_justification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the constitutional boundaries of race-conscious admissions. Sets the narrow-tailoring and compelling-interest standards through precedent. Its rulings bind all lower courts and state actors; the Court itself is not subject to the constraint it authorizes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Design and operate race-conscious admissions programs within Court-defined boundaries. Gain institutional diversity and legitimacy; bear compliance costs and litigation risk. Can modify programs but cannot exit the constitutional framework — must comply or abandon race-conscious tools.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, selective_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, selective_universities, beneficiary).

% Receive admissions advantage under race-conscious policies. Benefit from remedial justification; their inclusion is the constraint's stated purpose. Exit options are limited — alternative pathways (less selective institutions, non-degree credentials) carry substantial opportunity cost.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Compete for fixed seats under a process that weights race. Those who would be admitted under race-blind criteria but are displaced bear the cost. Cannot exit the competitive framework without foregoing selective higher education; litigation is the primary resistance channel.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, race_neutral_applicants, payer,
    moderate, biographical, constrained, national).

% Fund public universities and set complementary policies (e.g., top-percent plans). Bear fiscal and political costs of compliance or defiance. Can ban race-conscious admissions via referendum or statute (as in CA, MI, WA) but cannot override federal constitutional floor.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_legislatures, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, state_legislatures, agenda_setter).

% Litigate to defend and expand race-conscious remedies. Organizationally invested in the remedial framework; gain membership, funding, and political capital from its maintenance. Can shift strategy to alternative frameworks (e.g., class-based, pipeline programs) — not trapped.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_organizations, beneficiary,
    organized, generational, mobile, national).

% Litigate to eliminate race-conscious admissions. Frame the constraint as unconstitutional racial classification. Organizationally invested in colorblind reading; gain fundraising and judicial appointments from the contest. Not trapped — can pivot to other constitutional fronts.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, conservative_legal_groups, payer,
    organized, generational, mobile, national).

% Produces the doctrinal scholarship, empirical studies, and theoretical frameworks that shape judicial and public understanding. Does not directly collect or pay; its analyses feed all other seats.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutionally authorized mechanism for institutions to remedy documented historical exclusion and pursue the educational benefits of diversity through race-conscious admissions, replacing the prior regime of explicit racial exclusion.
% TRANSFER_FUNCTION: Moves admission slots at selective institutions from applicants who would prevail under race-neutral criteria to historically excluded applicants, with the state (via courts) defining the permissible scope and the universities executing the transfer.
% ABSENT_VOICES: Applicants from historically excluded groups who do not benefit from elite-institution remediation (community college students, non-college-bound youth) — the constraint's benefits concentrate at the selective-admissions margin. Also absent: future cohorts whose remedial claim may weaken as historical distance grows.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, selective universities would immediately shift to race-neutral alternatives (top-percent plans, socioeconomic preferences, pipeline programs) or abandon diversity goals; the admission profile of every selective campus would change within one cycle; the constitutional authorization that currently shields these programs from strict scrutiny elimination would disappear.
% FOUNDING_PROBLEM: After formal desegregation, persistent racial stratification in elite educational access remained — the constraint was built to authorize race-conscious tools that could dismantle the continuing effects of prior de jure and de facto exclusion.
% FOUNDING_PROBLEM_CORROBORATION: The remedial reading's beneficiaries (universities, civil rights groups) attest the founding problem persists — citation to ongoing disparities in K-12 preparation, wealth gaps, and campus climate. Colorblind and antisubordination readings' proponents (conservative legal groups, critical race theorists outside the institutional beneficiary set) attest the problem is either solved (formal barriers gone) or misdiagnosed (remediation entrenches classification rather than dismantling hierarchy).
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects that the constraint transfers a modest but nonzero share of selective seats — the magnitude is bounded by narrow-tailoring requirements. Suppression (0.42) reflects that the constraint's persistence depends on judicial enforcement of the remedial framework against colorblind challenges; the 2023 overruling demonstrates the constraint could not survive without active Court maintenance. Theater ratio (0.28) captures the growing gap between the diversity rationale as stated and the mechanical 'plus factor' operation of many programs. Accessibility collapse (0.35) is moderate — race-neutral alternatives exist but are less effective at achieving racial diversity. Resistance (0.55) is high — sustained litigation, state bans, and the 2023 terminus show the constraint faced organized opposition from inception.
 *
 * PERSPECTIVAL GAP:
 *   From the university/admitted-student seat, the constraint is genuine coordination — it enables the diversity the institution values and the Court (until 2023) authorized. From the displaced-applicant seat, it is extraction — a racial classification that costs them admission. From the Court's seat, it is a managed transition — the narrow-tailoring and sunset expectations (Grutter's '25 years') signal the constraint was never meant to be permanent. The engine computes these divergences from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and selective universities are agenda-setters — the Court authorizes the framework; universities implement it. Historically excluded applicants are primary beneficiaries — the constraint exists for them. Race-neutral applicants are payers — they bear the competitive displacement. State legislatures both pay (fund compliance) and set agendas (can ban within their jurisdiction). Civil rights groups and conservative legal groups are organized beneficiaries/payers respectively, but with mobile exit — they are institutional entrepreneurs of the contest, not trapped by it. The legal academy observes analytically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persistent stratification after formal desegregation) remains contested — not clearly live (formal barriers gone, disparities persist) nor clearly dead (race-neutral alternatives show partial success). The constraint's 2023 termination via SFFA suggests the Court concluded the mandate had atrophied: the remedial justification no longer justified the classification. But the constraint was not a piton — it was actively litigated, actively defended, and actively overruled. Theatricality grew over time (theater_ratio 0.12→0.28) but enforcement remained real until the end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_diversity_boundary,
    'Is the diversity rationale a genuine continuation of the remedial purpose, or a doctrinal pivot that sustains the constraint after its original justification faded?',
    'Doctrinal history analysis: trace whether ''diversity'' in Bakke/Grutter operates as a proxy for remediation or as an independent institutional interest. Empirical: compare outcomes of remedial-targeted vs. diversity-targeted programs.',
    'If diversity is a pivot, the constraint''s extraction continued after its coordination function (remediation) weakened — strengthening the tangled_rope classification. If continuous, the coordination function persists and extraction is the price of a live remedial project.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_diversity_boundary, conceptual, 'Whether the diversity rationale extends or replaces the remedial founding problem.').

omega_variable(
    narrow_tailoring_operationalization,
    'Does narrow tailoring meaningfully limit extraction, or does it function as a procedural ritual that rubber-stamps whatever program the university designs?',
    'Compare admission outcomes under ''narrowly tailored'' programs vs. hypothetical race-blind baselines across institutions. Measure the variance in racial composition explained by the plus factor vs. other factors.',
    'If narrow tailoring is ritual, suppression is higher than measured (the constraint extracts more than its authorization permits). If binding, the 0.38 extractiveness reflects a genuine structural limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_tailoring_operationalization, empirical, 'Whether the Court''s limiting doctrine actually constrains extraction.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the remedial reading''s structural relationship to the equal_protection_kernel differ from its siblings, and what does that imply for classification stability?',
    'Map the structural deltas: remedial adds beneficiaries (historically excluded) and victims (displaced applicants) where colorblind adds neither; antisubordination adds beneficiaries but defines them by subordination status not historical exclusion. Trace how each reading''s ε and stakeholder set would shift if adopted as binding law.',
    'The remedial reading is the only one that creates a clear victim set (race_neutral_applicants) — this is why it computes as tangled_rope while colorblind would compute as mountain (no extraction, no victims) and antisubordination might compute as rope (beneficiaries without clear victims). The kernel''s contested structure means no single reading is stable; classification depends on which reading is authoritative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: how this reading''s beneficiary/victim structure differs from sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_kernel__remedial_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__remedial_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__remedial_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1995, equal_protection_kernel__remedial_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.32).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__remedial_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__remedial_reading, base_extractiveness, 2016, 0.36).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(equa_su_t1995, equal_protection_kernel__remedial_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__remedial_reading, suppression_requirement, 2013, 0.41).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__remedial_reading, suppression_requirement, 2016, 0.41).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__remedial_reading, 0.15).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, state_affirmative_action_bans).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, top_percent_plans).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three constraint stories: remedial_reading (this file), colorblind_reading, and antisubordination_reading. Each has distinct ε, stakeholder sets, and claimed types. The remedial reading was the binding precedent 1978–2023; the colorblind reading became binding in 2023; the antisubordination reading remains a live academic/advocacy position. The ε values diverge because each reading instantiates a different constraint: remedial authorizes extraction for remediation (ε~0.38), colorblind forbids the classification entirely (ε~0.05), antisubordination authorizes only hierarchy-dismantling action (ε~0.15 estimated).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, moderate, 0.7).
constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
