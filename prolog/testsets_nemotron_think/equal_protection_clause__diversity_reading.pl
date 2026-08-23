% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Diversity Reading (Race-Conscious Admissions for Educational Diversity)
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   The diversity reading of the Equal Protection Clause, originating in
 *   Justice Powell's Bakke opinion and elaborated in Grutter v. Bollinger,
 *   holds that race-conscious university admissions are constitutionally
 *   permissible when narrowly tailored to serve the compelling interest in
 *   educational diversity. This reading frames diversity as a benefit to ALL
 *   students — not merely remedial justice for historically subordinated
 *   groups — and thus positions the constraint as ongoing educational
 *   coordination rather than temporary remediation. The constraint requires
 *   active judicial enforcement (strict scrutiny, narrow tailoring) and
 *   extracts moderate costs from applicants disadvantaged by race-conscious
 *   policies (particularly Asian American applicants in recent litigation),
 *   while delivering diffuse educational benefits to the entire student body.
 *   The reading was substantially constrained but not formally overruled by
 *   Students for Fair Admissions v. Harvard/UNC (2023).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.38).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.42).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading (Race-Conscious Admissions for Educational Diversity)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'ef14c651-06fe-4795-ac6f-e20f609e3bcd').
narrative_ontology:cs_kernel_codification('ef14c651-06fe-4795-ac6f-e20f609e3bcd', formalized).
narrative_ontology:cs_authority_grounding('ef14c651-06fe-4795-ac6f-e20f609e3bcd', lineage).
narrative_ontology:cs_interpretation_layer_present('ef14c651-06fe-4795-ac6f-e20f609e3bcd').
narrative_ontology:cs_reading_relation('ef14c651-06fe-4795-ac6f-e20f609e3bcd', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef14c651-06fe-4795-ac6f-e20f609e3bcd', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_axiom('ef14c651-06fe-4795-ac6f-e20f609e3bcd', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('ef14c651-06fe-4795-ac6f-e20f609e3bcd', educational_diversity_is_compelling_state_interest, empirically_contingent).
narrative_ontology:cs_axiom('ef14c651-06fe-4795-ac6f-e20f609e3bcd', secondary, narrow_tailoring_limits_extraction_to_moderate_band).
narrative_ontology:cs_axiom_status(narrow_tailoring_limits_extraction_to_moderate_band, holdable).
narrative_ontology:cs_axiom_grounding('ef14c651-06fe-4795-ac6f-e20f609e3bcd', narrow_tailoring_limits_extraction_to_moderate_band, conventional).
narrative_ontology:cs_reference_frame('ef14c651-06fe-4795-ac6f-e20f609e3bcd', bakke_powell_opinion).
narrative_ontology:cs_drift_state('ef14c651-06fe-4795-ac6f-e20f609e3bcd', contemporary_sffa_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef14c651-06fe-4795-ac6f-e20f609e3bcd', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, disadvantaged_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_students_admitted).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_students_admitted).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, educational_diversity_is_compelling_state_interest).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, academic_freedom_includes_admissions_discretion).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, diversity_benefits_all_students_not_just_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All enrolled students (regardless of race) receive the educational benefits of a diverse learning environment: cross-racial understanding, reduced stereotyping, enhanced classroom discourse, preparation for diverse workplaces. They cannot individually opt out of the diversity environment once enrolled; exit would mean leaving the institution.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    organized, biographical, constrained, national).

% Universities design and administer race-conscious admissions policies, justifying them as essential to academic freedom and educational mission. They bear compliance costs (litigation, administrative burden) but gain institutional autonomy in composing student bodies. They can partially exit by adopting race-neutral alternatives, but claim these are insufficient for their educational objectives.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, educational_institutions, beneficiary).

% Applicants (disproportionately Asian American in recent litigation) who face reduced admission probability due to race-conscious policies. They bear the extraction directly: lower admission chances at selective institutions. Their exit options are constrained — they can apply to less selective institutions, attend institutions in states with affirmative action bans, or forgo higher education, but cannot avoid the constraint's effect on the most selective tier.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, disadvantaged_applicants, payer,
    moderate, biographical, constrained, national).

% Students admitted under race-conscious policies receive individual admission benefits but are doctrinally framed as instrumental to the diversity interest of all students. They may bear stigma costs (mismatch hypothesis, stereotype threat) and the burden of being the 'diversity' the institution seeks. Their exit is constrained by the same institutional dynamics as other students.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students_admitted, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_students_admitted, payer).

% Federal courts (ultimately the Supreme Court) enforce strict scrutiny: they determine whether diversity is compelling, whether the policy is narrowly tailored, and whether race-neutral alternatives suffice. They do not collect or pay from the constraint but structure its enforcement. Their composition changes the constraint's effective extractiveness over time.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, courts, observer,
    institutional, generational, analytical, national).

% Advocates and litigants who argue the Equal Protection Clause forbids all racial classifications. They are structurally excluded from the diversity reading's framework — their position is the colorblind_reading, a sibling constraint. They cannot exit the diversity reading's regime except through judicial overturning (achieved partially in SFFA) or state-level bans.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of assembling educationally diverse student bodies: no single institution can unilaterally produce the cross-racial learning benefits that arise from critical-mass diversity across the system; race-conscious admissions coordinate this outcome across selective institutions.
% TRANSFER_FUNCTION: Moves admission probability from applicants who would be admitted under race-neutral criteria (disproportionately Asian American and white applicants at the margin) to underrepresented minority applicants, with the justification that the resulting diversity generates educational benefits for all enrolled students.
% ABSENT_VOICES: Colorblind constitutionalists (excluded role) who would argue all racial classification is forbidden; future students who would experience the long-term institutional effects; K-12 systems whose pipeline inequalities make selective admissions a late-stage intervention.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight, selective universities would immediately shift to race-neutral alternatives (percentage plans, socioeconomic preferences, expanded recruitment), admission probabilities would change for identifiable applicant groups, the educational composition of elite campuses would shift, and the constitutional doctrine governing race-conscious policy would be fundamentally altered — the SFFA decision approximates this disappearance.
% FOUNDING_PROBLEM: How to justify race-conscious admissions after Bakke rejected remedial justifications for private universities and the Court moved away from societal-discrimination remediation. The diversity reading was constructed to provide a forward-looking, educationally grounded compelling interest that could survive strict scrutiny without requiring findings of past institutional discrimination.
% FOUNDING_PROBLEM_CORROBORATION: The diversity reading's founding problem is attested by Justice Powell's Bakke opinion (itself a single-Justice controlling opinion), the Grutter majority, and decades of amicus briefs from universities and social scientists. Critics (colorblind_reading proponents, SFFA majority) attest the problem was a post-hoc rationalization — that the true founding problem was preserving racial preferences after remedial justifications collapsed, and that the diversity rationale was constructed to fit the doctrinal opening. No neutral corroboration exists outside the benefiting institutional coalition.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).
:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because narrow tailoring requirements genuinely limit the magnitude of racial preferences, though the constraint still transfers admission probability from some applicants to others. Suppression (0.42) reflects the active enforcement machinery: strict scrutiny review, individualized holistic review requirements, prohibition of quotas/mechanical bonuses, and periodic rejustification. Theater ratio (0.28) captures the gap between the 'holistic review' formalism and the mechanical weight race often carries in practice — the constraint's coordination function is real but the enforcement rituals partially mask the extraction. Accessibility collapse (0.35) is moderate: race-neutral alternatives exist but are treated as insufficient by the doctrine. Resistance (0.55) is high: sustained litigation, state bans, and the SFFA decision show active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the institution/student beneficiary seats, the constraint appears as genuine coordination (rope-like): a mechanism that solves the collective-action problem of assembling diverse learning environments. From the disadvantaged applicant seat, it appears as extraction (snare-like): a racial classification that reduces their admission chances. The engine computes this divergence from the structural data — the diversity reading's claim of universal benefit does not eliminate the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   All students (including white students) are primary beneficiaries (d ≈ 0.2-0.3) — they receive the educational benefits of diverse learning environments. Educational institutions are agenda_setters and beneficiaries (d ≈ 0.15) — they gain academic freedom and institutional autonomy. Disadvantaged applicants (particularly Asian Americans in recent cases) are payers (d ≈ 0.7-0.8) — they bear admission probability reductions. Minority students admitted under the policy are instrumental means to diversity (d ≈ 0.4-0.5) — they benefit individually but the doctrine's justification is systemic, not individual. Courts are observers (d = 0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids mandatrophy by tethering its justification to an ongoing educational value (diversity benefits) rather than a completed remedial project. However, the SFFA decision suggests the constraint may be entering mandatrophy: the founding problem (educational diversity as compelling interest) remains live per the reading's logic, but the authority structure has severely restricted the mechanism. The constraint persists in degraded form — some institutions maintain race-conscious policies through proxy variables — suggesting piton dynamics may emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the diversity reading a distinct constraint from the remedial and colorblind readings of the Equal Protection Clause, or merely a rhetorical variant?',
    'Compare beneficiary/victim structures, extractiveness profiles, and sunset logic across the three readings. If each produces a stable, different ε and different stakeholder map, they are distinct constraints.',
    'If distinct, each reading gets its own classification; if not, the kernel is a single constraint with observer-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the diversity reading instantiates a structurally distinct constraint from its sibling readings.').

omega_variable(
    diversity_benefit_measurement,
    'Can the educational benefits of diversity for all students (including white students) be measured independently of the racial classification mechanism?',
    'Natural experiments from race-neutral alternatives (Texas Top 10%, California post-Prop 209) showing whether equivalent diversity benefits obtain without racial classification.',
    'If benefits are measurable without classification, the coordination function is separable from the extractive mechanism; if not, the classification is structurally necessary to the benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefit_measurement, empirical, 'Whether the diversity benefit is causally dependent on the race-conscious mechanism.').

omega_variable(
    narrow_tailoring_effectiveness,
    'Does narrow tailoring actually limit extraction to a moderate band, or does it function as theatrical compliance masking substantial de facto quotas?',
    'Compare stated holistic review practices against statistical outcomes (admission rates by race at given academic indices) across institutions under strict scrutiny.',
    'If narrow tailoring is effective, ε stays moderate (~0.3-0.4); if theatrical, ε rises toward snare territory and theater_ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_effectiveness, empirical, 'Whether the constraint''s enforcement mechanism genuinely bounds extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_diversity_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement_basis(ep_diversity_tr_t1978, observed).
narrative_ontology:measurement(ep_diversity_tr_t1995, equal_protection_clause__diversity_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(ep_diversity_tr_t1995, observed).
narrative_ontology:measurement(ep_diversity_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement_basis(ep_diversity_tr_t2003, observed).
narrative_ontology:measurement(ep_diversity_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement_basis(ep_diversity_tr_t2013, observed).
narrative_ontology:measurement(ep_diversity_tr_t2016, equal_protection_clause__diversity_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement_basis(ep_diversity_tr_t2016, observed).
narrative_ontology:measurement(ep_diversity_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.28).
narrative_ontology:measurement_basis(ep_diversity_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(ep_diversity_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement_basis(ep_diversity_be_t1978, observed).
narrative_ontology:measurement(ep_diversity_be_t1995, equal_protection_clause__diversity_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(ep_diversity_be_t1995, observed).
narrative_ontology:measurement(ep_diversity_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement_basis(ep_diversity_be_t2003, observed).
narrative_ontology:measurement(ep_diversity_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement_basis(ep_diversity_be_t2013, observed).
narrative_ontology:measurement(ep_diversity_be_t2016, equal_protection_clause__diversity_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement_basis(ep_diversity_be_t2016, observed).
narrative_ontology:measurement(ep_diversity_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement_basis(ep_diversity_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_diversity_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement_basis(ep_diversity_su_t1978, observed).
narrative_ontology:measurement(ep_diversity_su_t1995, equal_protection_clause__diversity_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(ep_diversity_su_t1995, observed).
narrative_ontology:measurement(ep_diversity_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.45).
narrative_ontology:measurement_basis(ep_diversity_su_t2003, observed).
narrative_ontology:measurement(ep_diversity_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.48).
narrative_ontology:measurement_basis(ep_diversity_su_t2013, observed).
narrative_ontology:measurement(ep_diversity_su_t2016, equal_protection_clause__diversity_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement_basis(ep_diversity_su_t2016, observed).
narrative_ontology:measurement(ep_diversity_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.42).
narrative_ontology:measurement_basis(ep_diversity_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.08).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, title_vi_statutory_framework).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, state_affirmative_action_bans).

% DUAL FORMULATION NOTE:
% Part of the equal_protection_clause constraint family (three readings). The diversity reading instantiates identity_coordination (educational diversity as identity-boundary maintenance) with moderate extraction. The remedial_reading instantiates resource_allocation (reparative transfers) with higher extraction and sunset logic. The colorblind_reading instantiates information_standard (colorblindness as procedural rule) with near-zero extraction. All three share the same constitutional text kernel but produce distinct ε, stakeholder maps, and temporal profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, organized, 0.25).
constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
