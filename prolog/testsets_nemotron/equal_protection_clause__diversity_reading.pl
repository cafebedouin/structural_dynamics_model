% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Equal Protection Diversity Reading (Race-Conscious Educational Admissions)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint story captures the 'diversity reading' of the Equal
 *   Protection Clause — the position that race-conscious university
 *   admissions are constitutionally permissible when narrowly tailored to
 *   serve the compelling educational interest of student body diversity.
 *   Originating in Justice Powell's Bakke opinion (1978), refined in Grutter
 *   v. Bollinger (2003), and substantially restricted in Students for Fair
 *   Admissions v. Harvard (2023), this reading structures higher education
 *   admissions for nearly five decades. The constraint coordinates
 *   educational institutions' pursuit of diverse learning environments while
 *   extracting admission opportunities from applicants who would be admitted
 *   under race-neutral criteria. It is a tangled rope: it solves a genuine
 *   coordination problem (how to assemble educationally valuable diverse
 *   cohorts) but does so through asymmetric extraction (rejected applicants
 *   from overrepresented groups bear concentrated costs; the colorblind
 *   principle is doctrinally suppressed). Active enforcement is required —
 *   courts must continuously police the narrow tailoring boundary,
 *   institutions must administer complex holistic review, and the doctrinal
 *   edifice requires constant judicial maintenance.
 *
 * KEY AGENTS:
 *   - all_students: Primary beneficiaries (diverse learning environment) — organized/constrained
 *   - educational_institutions: Agenda setters (design/administer admissions) — institutional/arbitrage
 *   - rejected_applicants_from_overrepresented_groups: Primary payers (lost admission slots) — moderate/trapped
 *   - minority_students: Instrumental beneficiaries / identity-taxed participants — organized/identity_locked
 *   - employers: Secondary beneficiaries (diverse workforce pipeline) — institutional/mobile
 *   - democratic_institutions: Tertiary beneficiaries (legitimate leadership pipeline) — institutional/analytical
 *   - colorblind_constitutional_principle: Doctrinal payer (suppressed principle) — analytical/analytical
 *   - courts: Enforcers (narrow tailoring review) — institutional/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.22).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.35).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading (Race-Conscious Educational Admissions)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'b347c19d-fd43-4f97-9c1e-69f718ee21a7').
narrative_ontology:cs_kernel_codification('b347c19d-fd43-4f97-9c1e-69f718ee21a7', fixed_text).
narrative_ontology:cs_authority_grounding('b347c19d-fd43-4f97-9c1e-69f718ee21a7', lineage).
narrative_ontology:cs_interpretation_layer_present('b347c19d-fd43-4f97-9c1e-69f718ee21a7').
narrative_ontology:cs_reading_relation('b347c19d-fd43-4f97-9c1e-69f718ee21a7', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('b347c19d-fd43-4f97-9c1e-69f718ee21a7', equal_protection_clause__remedial_reading, influences).
narrative_ontology:cs_axiom('b347c19d-fd43-4f97-9c1e-69f718ee21a7', foundational, diversity_is_compelling_educational_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_educational_interest, holdable).
narrative_ontology:cs_axiom_grounding('b347c19d-fd43-4f97-9c1e-69f718ee21a7', diversity_is_compelling_educational_interest, empirically_contingent).
narrative_ontology:cs_axiom('b347c19d-fd43-4f97-9c1e-69f718ee21a7', foundational, all_students_benefit_from_diverse_learning_environment).
narrative_ontology:cs_axiom_status(all_students_benefit_from_diverse_learning_environment, holdable).
narrative_ontology:cs_axiom_grounding('b347c19d-fd43-4f97-9c1e-69f718ee21a7', all_students_benefit_from_diverse_learning_environment, empirically_contingent).
narrative_ontology:cs_axiom('b347c19d-fd43-4f97-9c1e-69f718ee21a7', secondary, narrow_tailoring_permits_race_as_one_factor).
narrative_ontology:cs_axiom_status(narrow_tailoring_permits_race_as_one_factor, holdable).
narrative_ontology:cs_axiom_grounding('b347c19d-fd43-4f97-9c1e-69f718ee21a7', narrow_tailoring_permits_race_as_one_factor, conventional).
narrative_ontology:cs_reference_frame('b347c19d-fd43-4f97-9c1e-69f718ee21a7', bakke_powell_opinion_1978).
narrative_ontology:cs_drift_state('b347c19d-fd43-4f97-9c1e-69f718ee21a7', post_sffa_2023, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b347c19d-fd43-4f97-9c1e-69f718ee21a7', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, employers).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, democratic_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, rejected_applicants_from_overrepresented_groups).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, colorblind_constitutional_principle).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_students).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_students).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, diversity_compelling_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, educational_benefits_of_diversity_thesis).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, narrow_tailoring_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All enrolled students experience the diverse learning environment as an educational benefit — cross-racial understanding, reduced prejudice, cognitive complexity. Their exit is constrained: they cannot individually opt out of the diversity rationale without leaving the institution, and the benefit is collective (cannot be individually captured). White students and Asian American students are included as beneficiaries in this reading's own logic.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    organized, biographical, constrained, national).

% Design and administer holistic admissions programs; defend them in litigation; set diversity goals and metrics. They have arbitrage-grade exit: they can modify admissions policies, shift to race-neutral alternatives, or (for private institutions) adjust mission. They collect institutional benefits: mission fulfillment, rankings, alumni diversity, donor expectations, regulatory compliance.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Applicants (disproportionately white and Asian American) who would be admitted under race-neutral criteria but are rejected to make room for diversity admits. They bear concentrated, individualized costs: lost admission to preferred institution, cascading effects on career trajectory. Exit is trapped — they cannot avoid the constraint by choosing another selective institution (all similarly constrained), and litigation is costly and uncertain. Their power is moderate: they can sue, generate political pressure, but individually lack institutional leverage.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, rejected_applicants_from_overrepresented_groups, payer,
    moderate, immediate, trapped, national).

% Admitted under race-conscious policies, they gain access to elite institutions. But they also bear identity taxation: pressure to represent their race, stereotype threat, tokenism, assumptions of mismatch. Their exit is identity-locked — the constraint's rationale makes their racial identity the mechanism of their inclusion, making it psychologically and structurally difficult to disidentify from the arrangement. They are both beneficiaries (access) and payers (identity costs), reflecting the instrumentalization ambiguity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_students, payer).

% Receive a diverse graduate pipeline without bearing admissions costs. They benefit from cross-cultural competence in workforce. Exit is mobile — they can recruit from any institution, advocate for race-neutral hiring, or develop internal diversity pipelines. They do not administer the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, employers, beneficiary,
    institutional, generational, mobile, national).

% Gain legitimacy from diverse leadership pathways (courts, legislatures, agencies). The constraint feeds a representative leadership pipeline. Exit is analytical — they observe the constraint's systemic effects but do not directly participate in admissions.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, democratic_institutions, beneficiary,
    institutional, generational, analytical, national).

% The doctrinal principle that equal protection forbids racial classification by government. This reading suppresses it by carving a diversity exception. The principle does not 'act' but its doctrinal vitality is a structural casualty — courts must distinguish, limit, and justify the exception, weakening the principle's clarity and force. It is a non-agent payer (vindicated proposition in reverse).
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_constitutional_principle, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(equal_protection_clause__diversity_reading, colorblind_constitutional_principle).

% Enforce narrow tailoring through strict scrutiny review. They administer the constraint's limiting principle. Their power is institutional (judicial review); their horizon is generational (precedent); their exit is analytical (they interpret, not experience). They bear enforcement costs (docket burden, legitimacy risk) but also gain institutional role as diversity's gatekeepers.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of assembling educationally valuable diverse student bodies in a society where K-12 segregation and socioeconomic stratification would otherwise produce homogeneous elite cohorts. Provides a single, constitutionally authorized mechanism for institutions to pursue diversity without each litigating its own justification.
% TRANSFER_FUNCTION: Transfers admission slots at selective institutions from applicants who would prevail under race-neutral criteria (disproportionately white and Asian American) to underrepresented minority applicants, mediated by holistic review that treats race as one factor among many. The transfer is not monetary but positional — access to elite credentials, networks, and career pathways.
% ABSENT_VOICES: K-12 students in segregated schools who never reach the selective admissions pipeline — the diversity reading addresses elite composition, not pipeline inequality. Socioeconomically disadvantaged students of all races who might benefit from class-based alternatives but are not centered in the diversity rationale. Future generations who inherit either a colorblind or race-conscious constitutional order — they are not in the courtroom.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight, selective institutions would immediately shift to race-neutral admissions (percentage plans, socioeconomic preferences, expanded outreach). Racial composition of elite cohorts would change substantially. The colorblind_reading would become the sole operative interpretation. The coordination machinery (holistic review infrastructure, diversity offices, pipeline programs) would persist but reorient. The world of higher education admissions would rearrange.
% FOUNDING_PROBLEM: In 1978, the problem was how to integrate elite professional schools (law, medicine) after formal segregation ended but de facto exclusion persisted. The diversity rationale offered a forward-looking, educationally grounded justification that avoided the backward-looking remedial framework (which required identifiable discrimination) and the colorblind framework (which would freeze existing disparities).
% FOUNDING_PROBLEM_CORROBORATION: The diversity rationale's proponents (universities, employers, military) attest the problem remains live: persistent segregation means race-neutral admissions would not produce educationally meaningful diversity. The colorblind_reading proponents (SFFA, state bans, conservative jurists) attest the problem is dead: formal barriers are gone, disparities reflect non-discriminatory factors, and race-consciousness now creates new injustice. The remedial_reading proponents (critical race theorists, some civil rights organizations) attest the problem is misdiagnosed: diversity instrumentalizes minorities rather than remedying subordination. No single corroborator outside the beneficiary set exists — the dispute is structural.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.22) because the constraint transfers a relatively small number of admission slots from overrepresented to underrepresented groups, but does so through a permanent, non-sunsetted mechanism that has expanded in scope (from law/medicine to all selective admissions). Suppression (0.35) reflects that the colorblind principle is not eliminated but is substantially constrained — courts permit racial classification only under strict scrutiny, and alternatives are not suppressed (race-neutral means remain available but are treated as insufficient). Theater ratio (0.15) is low but rising: the 'holistic review' machinery increasingly performs diversity rationales while operating against fixed demographic targets. Accessibility collapse (0.45) is moderate — race-neutral alternatives exist (percentage plans, socioeconomic preferences) but are treated as inadequate by institutions. Resistance (0.55) is substantial — sustained litigation, state bans, and the 2023 SFFA decision show the constraint meets active opposition.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different types for different seats: from the all_students seat, the constraint appears as genuine coordination (rope-like) — diverse learning environments benefit everyone. From the rejected_applicants seat, it appears as extraction (snare-like) — their individual merit is subordinated to group-based diversity metrics. From the minority_students seat, the experience is mixed: admission access is a benefit, but identity taxation (representing one's race, stereotype threat, tokenism) and instrumentalization extract real costs. From the courts seat, the constraint appears as active enforcement burden (narrow tailoring review is resource-intensive and doctrinally unstable). The colorblind_principle seat experiences doctrinal suppression — a foundational constitutional commitment is held in abeyance for diversity's sake.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: all_students (primary — diverse learning environment), educational_institutions (mission fulfillment, institutional autonomy), employers (workforce pipeline), democratic_institutions (leadership legitimacy). Victims declared: rejected_applicants_from_overrepresented_groups (concentrated admission losses), colorblind_constitutional_principle (doctrinal suppression). Minority students are NOT listed as victims or beneficiaries in base_properties — the omega 'beneficiary_instrumentalization_ambiguity' captures their ambiguous position. The diversity reading's core structural claim is that ALL students benefit, making the coordination function universal; the extraction falls on a narrow, identifiable group (overrepresented applicants) while the benefits are diffuse and collective. This asymmetry is the tangled rope signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assembling educationally valuable diverse cohorts in a segregated society) was live in 1978. Its status is now contested: formal segregation is gone, but persistent segregation by neighborhood and school district means the diversity rationale still addresses real educational deficits. However, the constraint has outlived its remedial framing — it was never explicitly remedial (that is the remedial_reading's domain) but has become permanent infrastructure. No sunset clause exists. The mandatrophy risk is that the constraint persists as theatrical diversity management (checking demographic boxes) rather than genuine educational coordination. Theater ratio rising from 0.08 to 0.15 over 45 years suggests this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this reading of the Equal Protection Clause structurally distinct from its sibling readings (colorblind_reading, remedial_reading), or does it collapse into one of them under scrutiny?',
    'Compare beneficiary/victim structures, extractiveness profiles, and founding problems across the three readings. If all three produce identical structural profiles, they are one constraint under different labels.',
    'If not distinct, the kernel is not genuinely contested at the structural level — the disagreement is rhetorical, not constitutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the diversity reading from its kernel siblings').

omega_variable(
    beneficiary_instrumentalization_ambiguity,
    'Are minority students genuine co-beneficiaries of the diversity rationale, or are they instrumentalized as means to white students'' educational benefit?',
    'Examine whether the constraint''s enforcement mechanisms and doctrinal evolution treat minority student outcomes as independently weighty or solely derivative of majority educational gains. Track post-admission support, retention investment, and curricular integration.',
    'If instrumental, the constraint extracts from minority students (identity taxation, stereotype threat) while claiming to benefit all — raising extractiveness and shifting toward snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_instrumentalization_ambiguity, conceptual, 'Whether minority students are beneficiaries or instruments in the diversity rationale').

omega_variable(
    narrow_tailoring_enforceability,
    'Does ''narrow tailoring'' impose genuine limiting principles on race-conscious admissions, or does it function as a performative check that permits de facto racial balancing?',
    'Empirical study of admissions outcomes pre/post strict scrutiny standard; comparison of racial composition targets vs. actual admits across institutions; analysis of whether race-neutral alternatives are seriously pursued.',
    'If narrow tailoring is performative, suppression is higher than authored and the constraint drifts toward snare. If genuinely limiting, the moderate extractiveness score holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_tailoring_enforceability, empirical, 'Whether the doctrinal limiting principle constrains practice or rationalizes it').

omega_variable(
    permanence_vs_sunset_ambiguity,
    'Is the diversity reading''s lack of sunset clause a feature (ongoing educational value) or a bug (institutionalized racial classification without endpoint)?',
    'Track whether diversity justifications evolve toward colorblind alternatives over time, or whether institutions treat race-consciousness as permanent infrastructure. Compare to remedial_reading''s explicit sunset logic.',
    'If permanent without evolution toward race-neutral means, the constraint accumulates extraction over time (T17 drift risk) and may reclassify toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_sunset_ambiguity, preference, 'Whether the constraint''s indefinite duration is structurally justified or a covert entrenchment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_clause__diversity_reading, theater_ratio, 1996, 0.1).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.14).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.15).
narrative_ontology:measurement(equa_be_t1996, equal_protection_clause__diversity_reading, base_extractiveness, 1996, 0.18).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.2).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.21).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(equa_su_t1996, equal_protection_clause__diversity_reading, suppression_requirement, 1996, 0.3).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.33).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.34).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.08).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, title_vi_statutory_framework).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, state_affirmative_action_bans).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_clause kernel. The diversity_reading occupies the doctrinal center (1978-2023) with all_students as beneficiaries and moderate extraction. The colorblind_reading treats any racial classification as extraction (higher ε for institutions, lower for applicants). The remedial_reading treats extraction as justified remediation (different victim/beneficiary structure, explicit sunset logic). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, organized, 0.35).
constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
