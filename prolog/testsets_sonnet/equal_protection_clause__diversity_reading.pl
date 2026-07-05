% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection — Educational Diversity Reading (Grutter/Fisher/Students for Fair Admissions Lineage)
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This story instantiates the diversity reading of the equal protection
 *   kernel: the doctrinal lineage from Bakke through Grutter, Fisher, and
 *   (its narrowing endpoint) Students for Fair Admissions, under which
 *   race-conscious admissions survive strict scrutiny only insofar as they
 *   serve a compelling interest in the educational benefits of a diverse
 *   student body, benefiting the entire enrolled cohort rather than remedying
 *   group-specific historical injury. This is a distinct constraint from the
 *   remedial reading (which grounds race-consciousness in redress for group
 *   subordination, carries an implicit sunset tied to remediation's
 *   completion, and names minority students as primary rights-bearing
 *   beneficiaries) and from the colorblind reading (which treats any racial
 *   classification as per se suspect regardless of asserted benefit). The
 *   three readings have different beneficiary structures, different ε
 *   profiles, and different persistence logics — they are linked here via
 *   network edges, not merged into one constraint.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda_setter (institutional/arbitrage) — designs and administers the race-conscious admissions apparatus
 *   - white_and_asian_majority_applicant_cohort: primary beneficiary (organized/mobile) — officially named recipient of the diversity pedagogical benefit
 *   - high_achieving_asian_american_applicants: primary payer (moderate/constrained) — bears disproportionate admissions cost under holistic review
 *   - minority_students_used_as_pedagogical_instruments: instrumentalized payer/beneficiary (powerless/identity_locked) — admitted partly as means to majority students' educational experience
 *   - federal_courts: analytical observer (institutional/analytical) — adjudicates narrow tailoring over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.42).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.38).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection — Educational Diversity Reading (Grutter/Fisher/Students for Fair Admissions Lineage)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '97e48a17-082f-4917-8c54-205dd60612bb').
narrative_ontology:cs_kernel_codification('97e48a17-082f-4917-8c54-205dd60612bb', fixed_text).
narrative_ontology:cs_authority_grounding('97e48a17-082f-4917-8c54-205dd60612bb', lineage).
narrative_ontology:cs_interpretation_layer_present('97e48a17-082f-4917-8c54-205dd60612bb').
narrative_ontology:cs_reading_relation('97e48a17-082f-4917-8c54-205dd60612bb', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('97e48a17-082f-4917-8c54-205dd60612bb', equal_protection_clause__colorblind_reading, influences).
narrative_ontology:cs_axiom('97e48a17-082f-4917-8c54-205dd60612bb', foundational, diversity_is_compelling_pedagogical_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_pedagogical_interest, holdable).
narrative_ontology:cs_axiom_grounding('97e48a17-082f-4917-8c54-205dd60612bb', diversity_is_compelling_pedagogical_interest, instrumental).
narrative_ontology:cs_axiom('97e48a17-082f-4917-8c54-205dd60612bb', foundational, race_conscious_means_permissible_when_narrowly_tailored_to_nonremedial_end).
narrative_ontology:cs_axiom_status(race_conscious_means_permissible_when_narrowly_tailored_to_nonremedial_end, holdable).
narrative_ontology:cs_axiom_grounding('97e48a17-082f-4917-8c54-205dd60612bb', race_conscious_means_permissible_when_narrowly_tailored_to_nonremedial_end, conventional).
narrative_ontology:cs_reference_frame('97e48a17-082f-4917-8c54-205dd60612bb', post_bakke_compelling_interest_framework).
narrative_ontology:cs_drift_state('97e48a17-082f-4917-8c54-205dd60612bb', post_sffa_harvard_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('97e48a17-082f-4917-8c54-205dd60612bb', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, white_and_asian_majority_applicant_cohort).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, diversity_administration_apparatus).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_students_used_as_pedagogical_instruments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_students_used_as_pedagogical_instruments).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, compelling_interest_in_educational_diversity).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, critical_mass_theory_of_classroom_benefit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer admissions processes that weigh race as a 'plus factor' among many, justified by the pedagogical claim that a racially diverse classroom improves learning outcomes for everyone. They set the definition of 'critical mass,' control the holistic review process that makes race-weighting hard to audit from outside, and defend the practice in litigation. They bear compliance costs but retain full discretion over how the diversity rationale is operationalized.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The doctrine's official rationale names this group as primary beneficiaries: they receive the pedagogical value of a diverse learning environment without personally bearing any admissions cost, since the racial preference is directed toward other applicants, not extracted from their credentials as a formal quota. Their exit options (private institutions, other selective schools, geographic mobility) are broad.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, white_and_asian_majority_applicant_cohort, beneficiary,
    organized, biographical, mobile, national).

% Statistical and litigation evidence (Harvard/UNC record) shows this group bears a disproportionate admissions penalty under holistic review that incorporates race, without being named as either the diversity's intended beneficiary or its designated remedial target. They have limited practical exit — a small number of elite institutions confer the credentialing and network effects that matter for the careers they are pursuing — and they cannot litigate the underlying rationale itself without attacking the doctrine's continued existence.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Admitted in part because their presence produces an educational benefit officially attributed to the classroom's other occupants; the diversity rationale frames their contribution as instrumental to majority students' learning rather than as their own entitlement to redress or opportunity. They gain admission access but carry the 'diversity admit' stigma the doctrine's own framing invites, and cannot exit the identity category the rationale assigns them without abandoning the admissions pathway itself.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students_used_as_pedagogical_instruments, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_students_used_as_pedagogical_instruments, beneficiary).

% University diversity offices, admissions consultants, and litigation-support institutions whose professional existence depends on the diversity rationale remaining a live, contestable, compliance-intensive doctrine requiring ongoing holistic-review infrastructure, narrow-tailoring documentation, and periodic re-justification in court.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, diversity_administration_apparatus, beneficiary,
    organized, generational, arbitrage, national).

% Argue equal protection forbids any racial classification regardless of asserted benefit; this reading's continued doctrinal life (until SFFA v. Harvard narrowed it) foreclosed their preferred colorblind rule from governing admissions practice. They litigate against the diversity rationale but do not control university admissions design.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_constitutionalists, excluded,
    organized, generational, constrained, national).

% Adjudicate strict scrutiny challenges, requiring universities to show the racial classification is narrowly tailored to the compelling diversity interest. Their evolving tolerance (Bakke through Grutter through Fisher through SFFA) is itself the mechanism by which this reading's scope has been permitted, then constrained.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, federal_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates admissions practice around a shared educational theory: that cross-racial classroom interaction produces cognitive and civic benefits (reduced stereotyping, broadened perspective-taking, preparation for a diverse workforce) that a race-blind admissions process would under-produce relative to its stated pedagogical goals.
% TRANSFER_FUNCTION: Moves admissions slots at the margin from applicants who would have been admitted under race-neutral criteria (disproportionately high-achieving Asian American applicants) to applicants who benefit from the race-conscious plus factor, while officially routing the justification through the benefit accruing to the entire cohort rather than through corrective entitlement to the admitted minority applicants themselves.
% ABSENT_VOICES: High-achieving Asian American applicants were structurally underrepresented in the doctrinal record until SFFA v. Harvard surfaced their statistical evidence directly; minority students themselves are rarely asked whether they consent to being framed as instruments of majority-student pedagogical benefit rather than as rights-bearing beneficiaries of remediation in their own right — that framing question is almost never posed to the students it assigns.
% DISAPPEARANCE_RATIONALE: Selective universities and the diversity administration apparatus would need to substantially reconstruct admissions processes and lose a compliance-defensible legal rationale (evidenced by post-SFFA scrambling toward proxy variables). Colorblind constitutionalists and many Asian American applicant advocates would say almost nothing changes for actual educational quality, only for the legal cover used to justify preference. The parties dispute which world we are describing.
% FOUNDING_PROBLEM: Following Bakke (1978), universities needed a constitutionally survivable rationale for considering race in admissions after quota systems and pure remediation rationales were rejected or politically vulnerable; the diversity rationale offered a compelling-interest theory (pedagogical benefit to all) that could survive strict scrutiny where an explicit remedial or reparative rationale could not.
% FOUNDING_PROBLEM_CORROBORATION: Universities and diversity-office professional associations attest the pedagogical benefit is real and ongoing, citing social-science literature on cross-racial interaction. Independent sources outside the beneficiary set — the Students for Fair Admissions litigation record, dissenting Supreme Court opinions (Thomas in Grutter and Fisher), and empirical critiques questioning the strength of the underlying social-science evidence — corroborate that the doctrine functions substantially as durable legal cover for race-conscious admissions rather than as a demonstrated, measurable pedagogical mechanism; no fully independent corroborator affirms the founding problem is straightforwardly 'live' in the form originally asserted.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at endpoint) rather than high because narrow-tailoring doctrine genuinely constrains how much weight race can carry relative to a pure quota system — this is the structural feature that most differentiates the diversity reading from a cruder racial-preference regime. Suppression is moderate (0.38): the constraint does not forbid alternatives outright, but the compelling-interest doctrine forecloses the colorblind alternative as a matter of governing law for the interval it holds, and forecloses direct discussion of remedial rationale (which courts have treated as constitutionally weaker) as the operative justification. Theater ratio rises across the interval (0.15 to 0.31) tracking the growing gap between the doctrine's officially asserted rationale (pedagogical benefit to all) and the increasingly well-documented practice of race-conscious weighting functioning as a proxy for group-balancing that the doctrine's own language forbids — this is the drift the SFFA majority ultimately cited.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (selective universities), the arrangement reads as principled compliance with a demanding constitutional standard in service of genuine pedagogical goals. From the payer seat (high-achieving Asian American applicants), the identical admissions process reads as an opaque, unaccountable racial penalty dressed in pedagogical language that shields the practice from the strict scrutiny a bare group-balancing rationale would fail. The engine computes these as structurally different seat classifications from the same base data; the doctrine's own 'benefits everyone' framing is precisely the surface that produces this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The white and Asian majority applicant cohort is the doctrine's declared beneficiary class — they receive the asserted pedagogical value without bearing the admissions cost, giving them low derived directionality (near-beneficiary). High-achieving Asian American applicants are declared victims: statistical evidence shows they bear a concentrated admissions penalty while the doctrine's rationale is not built around compensating them for anything, and their exit options are constrained by the concentration of elite credentialing value in a small set of institutions — this produces high derived directionality (near-target). Minority students admitted under the policy occupy a genuinely split position: they benefit from admission access (lower d on that axis) while simultaneously being instrumentalized as the mechanism that produces benefit for others (higher d on the dignitary axis) — this is exactly the asymmetry that distinguishes the diversity reading from the remedial reading, where minority students would be the doctrine's primary named beneficiaries rather than its instrumental means.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (finding a constitutionally survivable rationale for race-conscious admissions after Bakke rejected quotas) has arguably been resolved or superseded by subsequent doctrinal narrowing (SFFA), yet the diversity-administration apparatus and the broader admissions infrastructure built around it persist through inertia and continued institutional investment — a classic mandatrophy signature. Declaring this tangled_rope rather than snare or piton reflects that a genuine coordination function (the pedagogical diversity theory) is not fabricated wholesale; it coexists with asymmetric extraction from a specific, identifiable payer class. Classifying it as pure extraction would erase the real (if contested) educational-benefit literature; classifying it as pure coordination would erase the well-documented statistical penalty borne by Asian American applicants that motivated SFFA.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_reading_kernel_instantiation,
    'Is the diversity rationale a distinct, independently defensible constitutional theory, or is it a strategic doctrinal vehicle constructed specifically because the remedial rationale could not survive strict scrutiny after Bakke?',
    'Doctrinal history and internal Court deliberation records (where available) showing whether the diversity rationale was selected on its own merits or as the maximally survivable framing given anticipated judicial resistance to remediation-based rationales.',
    'If constructed as a survival strategy, the diversity reading''s claimed independence from the remedial reading is weaker than declared, and the instrumentalization of minority students becomes a feature of the doctrine''s design rather than an incidental byproduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_reading_kernel_instantiation, conceptual, 'Whether the diversity reading is an independent theory or a strategic substitute for the foreclosed remedial rationale.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'During the interval this reading governed admissions law (Bakke to SFFA), did the diversity reading merely coexist with the remedial and colorblind readings as competing arguments, or did it operationally foreclose the remedial reading from ever becoming the controlling doctrine?',
    'Track whether any Supreme Court majority ever adopted the remedial rationale as the sole or primary basis for upholding race-conscious admissions during this interval, versus always routing through diversity language even when remedial arguments were briefed.',
    'If the diversity reading systematically foreclosed the remedial reading from governing-law status, the two readings are not merely coexisting alternatives but exist in an asymmetric relationship where the diversity reading structurally suppressed remedial framing as a legal strategy — this would sharpen the network relationship declared between the two stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Whether the diversity reading''s dominance operationally foreclosed the remedial reading during the governing interval.').

omega_variable(
    pedagogical_benefit_evidentiary_status,
    'Is the social-science evidence for cross-racial-interaction pedagogical benefit strong enough to sustain ''compelling interest'' status independent of its litigation utility, or is it primarily litigation-driven post-hoc justification?',
    'Independent meta-analysis of the underlying social-science literature by researchers with no stake in admissions litigation outcomes, compared against the selective citation patterns visible in amicus briefs supporting the diversity rationale.',
    'Weak independent evidentiary support would strengthen the reading of this constraint as extraction-dominant (tangled_rope trending toward snare); strong independent support would strengthen the genuine-coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pedagogical_benefit_evidentiary_status, empirical, 'Whether the pedagogical diversity benefit is independently well-evidenced or primarily litigation-constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_clause__diversity_reading, theater_ratio, 1988, 0.19).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_clause__diversity_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.31).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1988, equal_protection_clause__diversity_reading, base_extractiveness, 1988, 0.28).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.34).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(equa_be_t2018, equal_protection_clause__diversity_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1988, equal_protection_clause__diversity_reading, suppression_requirement, 1988, 0.24).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.34).
narrative_ontology:measurement(equa_su_t2018, equal_protection_clause__diversity_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.08).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_clause kernel, decomposed per the ε-invariance principle: colorblind_reading (ε near-mountain, forbids all racial classification), diversity_reading (this story, moderate ε, tangled_rope, permanent/ongoing pedagogical justification, minority students instrumentalized), and remedial_reading (higher ε where enforced without sunset, names minority students as primary rights-bearing beneficiaries, structurally implies a remediation-completion sunset). Each carries its own beneficiary/victim structure and its own classification; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
