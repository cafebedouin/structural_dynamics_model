% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Equal Protection Remedial Reading: Narrowly Tailored Race-Conscious Action
 *   domain: constitutional law / education policy / civil rights
 *
 * SUMMARY:
 *   This constraint story instantiates the remedial reading of the
 *   equal_protection_kernel: the interpretation that the Equal Protection
 *   Clause permits race-conscious state action when narrowly tailored to
 *   remedy documented historical exclusion or to achieve a compelling
 *   educational diversity interest. Under this reading, public universities
 *   may treat race as a 'plus factor' in holistic admissions. The constraint
 *   coordinates access for historically excluded groups while extracting
 *   admissions opportunities from applicants who would have been admitted
 *   under a race-blind baseline. It requires active judicial enforcement to
 *   maintain the boundary between permissible individualized consideration
 *   and impermissible quotas. This is one reading of a contested kernel;
 *   sibling readings (colorblind and antisubordination) are structurally
 *   distinct constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - public_universities: Primary agenda-setter (institutional/constrained) â designs and administers narrowly tailored admissions plans under judicial oversight
 *   - historically_excluded_groups: Primary beneficiary (organized/constrained) â gains enhanced admissions access relative to race-blind baseline
 *   - rejected_applicants: Primary payer (powerless/constrained) â bears the opportunity cost of displaced admission slots
 *   - federal_judiciary: Agenda-setter/enforcer (institutional/analytical) â articulates the narrow tailoring standard and strikes down non-compliant plans
 *   - class_based_advocates: Excluded voice (moderate/constrained) â advocates for socioeconomic alternatives crowded out by the racial framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.62).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.58).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Remedial Reading: Narrowly Tailored Race-Conscious Action").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional law / education policy / civil rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '82287c01-a76e-4746-8a29-c07cd6be48ea').
narrative_ontology:cs_kernel_codification('82287c01-a76e-4746-8a29-c07cd6be48ea', formalized).
narrative_ontology:cs_authority_grounding('82287c01-a76e-4746-8a29-c07cd6be48ea', lineage).
narrative_ontology:cs_interpretation_layer_present('82287c01-a76e-4746-8a29-c07cd6be48ea').
narrative_ontology:cs_reading_relation('82287c01-a76e-4746-8a29-c07cd6be48ea', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('82287c01-a76e-4746-8a29-c07cd6be48ea', equal_protection_kernel__antisubordination_reading, influences).
narrative_ontology:cs_axiom('82287c01-a76e-4746-8a29-c07cd6be48ea', foundational, race_as_plus_factor_permissible).
narrative_ontology:cs_axiom_status(race_as_plus_factor_permissible, holdable).
narrative_ontology:cs_axiom_grounding('82287c01-a76e-4746-8a29-c07cd6be48ea', race_as_plus_factor_permissible, conventional).
narrative_ontology:cs_axiom('82287c01-a76e-4746-8a29-c07cd6be48ea', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('82287c01-a76e-4746-8a29-c07cd6be48ea', educational_diversity_compelling_interest, empirically_contingent).
narrative_ontology:cs_reference_frame('82287c01-a76e-4746-8a29-c07cd6be48ea', remedial_constitutional_order).
narrative_ontology:cs_drift_state('82287c01-a76e-4746-8a29-c07cd6be48ea', post_sffa_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('82287c01-a76e-4746-8a29-c07cd6be48ea', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, public_universities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, diversity_as_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, remedial_state_purpose).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer race-conscious admissions programs under active judicial supervision. They must document that their plans are narrowly tailored to achieve educational diversity or remedy documented historical exclusion, undergoing repeated litigation and policy revision to maintain constitutional compliance. They gain mission fulfillment and doctrinal space to pursue diversity goals.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, public_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, public_universities, beneficiary).

% Gain enhanced access to selective public universities under admissions plans that treat race as a plus factor in holistic review. Their likelihood of admission is improved relative to a race-blind baseline, though they remain subject to competitive academic and extracurricular evaluation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Are denied admission to selective public universities despite academic credentials that would have secured admission under a race-blind process. They bear the direct opportunity cost of displaced admissions slots, often without knowledge of the specific role race played in the holistic review.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants, payer,
    powerless, biographical, constrained, national).

% Articulates and enforces the constitutional standard through case-by-case review of university admissions plans. It polices the boundary between permissible individualized consideration and impermissible quotas, generating extensive doctrinal tests and periodically revising the intensity of scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocate for socioeconomic affirmative action as an alternative to race-conscious preferences. Their preferred policy approach lacks constitutional stature under the remedial reading and is crowded out of the doctrinal framework, despite empirical support for class-based mechanisms.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, class_based_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse student bodies in selective public higher education and remedies documented historical exclusion by permitting universities to treat race as one factor among many in holistic admissions, subject to strict judicial review.
% TRANSFER_FUNCTION: Moves selective admissions slots from applicants who would have been admitted under race-blind criteria to historically excluded racial groups, and moves institutional legitimacy, compliance burden, and litigation risk to public universities.
% ABSENT_VOICES: Class-based affirmative action advocates, who would argue for socioeconomic rather than racial criteria, are crowded out of the constitutional framework; individual rejected applicants often lack standing to challenge holistic plans; colorblind constitutional advocates are formally present in litigation but their preferred reading is excluded from the operative doctrine.
% DISAPPEARANCE_RATIONALE: If the remedial reading disappeared overnight, public universities would immediately redesign admissions to exclude explicit racial preferences; the composition of selective incoming classes would shift in the short term; litigation strategies would reorient around alternative diversity mechanisms; and the beneficiary group's access to selective institutions would decline absent compensatory policies.
% FOUNDING_PROBLEM: Persistent historical exclusion of Black, Latino, and other minority groups from public higher education, and the resulting lack of meaningful educational diversity in selective institutions, documented in legislative findings and social science evidence.
% FOUNDING_PROBLEM_CORROBORATION: Historical exclusion is corroborated by legislative records and academic historians outside the directly benefiting universities. However, the current necessity of race-conscious remedies in contemporary admissions is contested by dissenting Supreme Court justices, plaintiff advocacy organizations, and empirical researchers who argue that socioeconomic factors have superseded race as primary barriers; no unanimous corroboration exists for the live-status claim.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores high on resistance (0.75) because it has faced continuous constitutional challenge since its inception. Extractiveness (0.62) reflects the zero-sum nature of selective admissions: every slot allocated via race-conscious criteria displaces an applicant who would have prevailed under race-blind review. Theater_ratio (0.60) is elevated because 'holistic review' and 'narrow tailoring' generate extensive performative compliance documentation that often obscures the underlying allocation mechanics. Suppression (0.58) is moderate: the constraint does not suppress all alternatives (class-based plans, race-blind processes still exist) but actively suppresses quota systems and pure racial balancing. Accessibility_collapse (0.45) is moderate because alternatives are legally disadvantaged but not eliminated. The temporal series show extraction and theater rising through Grutter (2003) and remaining elevated until the constraint's effective enforcement collapsed in 2023 (SFFA), producing the late-interval divergence between extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of historically_excluded_groups, the constraint operates as remedial coordination correcting historical injustice. From the seat of rejected_applicants, it operates as asymmetric extraction displacing merit-based outcomes. The public_universities seat experiences both coordination (mission fulfillment) and extraction (compliance costs, litigation risk). The federal_judiciary seat perceives a constitutional balancing test; the engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   public_universities and historically_excluded_groups are declared beneficiaries: they gain mission legitimacy and access, respectively, placing their directionality toward the beneficiary pole. rejected_applicants are declared victims: they bear the concentrated cost of displacement, placing their directionality toward the target pole. The federal_judiciary is neither beneficiary nor victim; its directionality is analytical. class_based_advocates are excluded from the doctrinal framework entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy analysis prevents mislabeling this constraint as a pure rope (coordination) or pure snare (extraction). Without the declared victim set (rejected_applicants), the narrow tailoring framework could be read as a scaffold or rope: it solves a coordination problem (diverse classrooms) with judicial oversight. However, the zero-sum admissions context creates an identifiable victim set â applicants displaced by race-conscious preferences â which triggers the tangled_rope classification. The constraint is not a snare because the coordination function (remedying documented exclusion, educational diversity) is genuine and not merely cover; but it is not a rope because the coordination is asymmetrically distributed across stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the remedial reading of the equal_protection_kernel. What structural elements change if the colorblind reading or antisubordination reading were adopted instead?',
    'Comparative doctrinal analysis: the colorblind reading would eliminate the beneficiary set (historically_excluded_groups) and the victim set (rejected_applicants) under race-conscious plans, shifting the constraint toward a rope or mountain; the antisubordination reading would reframe the victim set as groups experiencing caste-like subordination rather than individual rejected applicants.',
    'Adoption of a sibling reading would dissolve the current beneficiary/victim structure and recompute directionality across all seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural delta between sibling readings of the equal protection kernel').

omega_variable(
    diversity_compelling_empirical_basis,
    'Is educational diversity a compelling interest because the empirical benefits to learning and civic formation are real and substantial, or is the compelling interest designation a doctrinal construct without robust empirical support?',
    'Meta-analysis of educational outcome studies comparing diverse and non-diverse classroom environments, controlling for selection effects; also tracking whether courts rely on actual social science or rhetorical assertions.',
    'If the empirical basis is weak, the coordination function (diversity) is largely performative, increasing theater_ratio and shifting the constraint toward snare; if strong, the coordination function is genuine and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_compelling_empirical_basis, empirical, 'Empirical grounding of the diversity compelling interest').

omega_variable(
    historical_exclusion_contemporaneous_relevance,
    'Does documented historical exclusion of minority groups from public higher education remain a live, operative harm that justifies race-conscious remedies in contemporary admissions, or has the problem attenuated to the point where the remedial rationale has become theatrical?',
    'Longitudinal analysis of admissions data, segregation indices, and socioeconomic mobility by race; comparison of selective university demographics against relevant applicant pools over multi-decade intervals.',
    'If the harm has attenuated, the founding_problem_status shifts toward dead, mandatrophy flags rise, and the constraint''s coordination component collapses into performance; if the harm remains live, the remedial rationale sustains the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_exclusion_contemporaneous_relevance, empirical, 'Whether historical exclusion remains a live justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t0, equal_protection_kernel__remedial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ep_remedial_tr_t9, equal_protection_kernel__remedial_reading, theater_ratio, 9, 0.32).
narrative_ontology:measurement(ep_remedial_tr_t18, equal_protection_kernel__remedial_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement(ep_remedial_tr_t27, equal_protection_kernel__remedial_reading, theater_ratio, 27, 0.56).
narrative_ontology:measurement(ep_remedial_tr_t36, equal_protection_kernel__remedial_reading, theater_ratio, 36, 0.6).
narrative_ontology:measurement(ep_remedial_tr_t45, equal_protection_kernel__remedial_reading, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t0, equal_protection_kernel__remedial_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ep_remedial_be_t9, equal_protection_kernel__remedial_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement(ep_remedial_be_t18, equal_protection_kernel__remedial_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(ep_remedial_be_t27, equal_protection_kernel__remedial_reading, base_extractiveness, 27, 0.6).
narrative_ontology:measurement(ep_remedial_be_t36, equal_protection_kernel__remedial_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(ep_remedial_be_t45, equal_protection_kernel__remedial_reading, base_extractiveness, 45, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t0, equal_protection_kernel__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ep_remedial_su_t9, equal_protection_kernel__remedial_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(ep_remedial_su_t18, equal_protection_kernel__remedial_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(ep_remedial_su_t27, equal_protection_kernel__remedial_reading, suppression_requirement, 27, 0.7).
narrative_ontology:measurement(ep_remedial_su_t36, equal_protection_kernel__remedial_reading, suppression_requirement, 36, 0.72).
narrative_ontology:measurement(ep_remedial_su_t45, equal_protection_kernel__remedial_reading, suppression_requirement, 45, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three structurally distinct readings. The remedial reading (this file) permits narrowly tailored race-conscious action; the colorblind reading forbids all racial classifications; the antisubordination reading evaluates classifications by their effect on caste-like hierarchy. Each reading has a different beneficiary/victim structure and extractiveness profile. They are linked as a constraint family because they share the kernel (the Equal Protection Clause text) but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
