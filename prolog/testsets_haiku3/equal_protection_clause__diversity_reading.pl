% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Equal Protection: Diversity Reading of Race-Conscious Educational Policy
 *   domain: constitutional/political/educational
 *
 * SUMMARY:
 *   The diversity reading of equal protection treats race-conscious
 *   educational policies (primarily university admissions) as
 *   constitutionally permissible when they serve the compelling interest in
 *   diverse learning environments from which all students benefit. This
 *   reading frames the equal protection clause not as colorblind (forbidding
 *   any racial classification) but as permitting careful, narrowly tailored
 *   consideration of race when justified by institutional educational
 *   mission. The constraint is CLAIMED as tangled_rope (genuine coordination
 *   function—managing tension between non-discrimination norms and diversity
 *   aspirations—alongside asymmetric extraction of admissions opportunity
 *   from non-selected applicants) while the authored metrics describe
 *   moderate extractiveness (the cost to payers is concentrated and
 *   measurable) and modest suppression (applicants retain exit options and
 *   litigation voice, though constrained). The diversity reading is one of
 *   three live interpretations of the equal protection clause kernel; the
 *   others are the colorblind reading (race-classification per se forbidden)
 *   and the remedial reading (race-conscious policy justified by remedying
 *   historical subordination, not diversity instrumental value).
 *
 * KEY AGENTS:
 *   - All students (powerless/constrained): framed as beneficiaries of diverse learning environments; gain theoretical educational and social goods from classroom diversity
 *   - Applicants not selected on diversity grounds (moderate/mobile): payers; bear concentrated, measurable cost of race-conscious admissions; typically white but not exclusively; exit via alternative institutions
 *   - Underrepresented minority applicants (moderate/mobile): dual-positioned beneficiaries and payers; gain admission access but are instrumentally selected to serve diversity function for others, not as remedy for group subordination
 *   - Institutional education sector (institutional/arbitrage): agenda-setter and beneficiary; sets and enforces admissions policy; frames diversity as core educational mission; benefits by retaining autonomy against colorblind mandates and remedial mandates
 *   - Courts and constitutional interpreters (institutional/analytical): agenda-setter; interpret equal protection to permit race-conscious admissions when narrowly tailored to compelling interest; set legitimacy conditions for policy
 *   - Colorblind reading advocates (powerful/constrained): excluded from institutional policy justification; contend equal protection forbids all racial classification; litigate against race-conscious policies
 *   - Remedial reading advocates (powerful/constrained): excluded from institutional policy justification; contend race-conscious policy should be justified by historical remediation, not diversity; contest the adequacy of diversity rationale
 *   - Legislative authority (institutional/analytical): observer; could override via statute but has largely deferred to judicial interpretation and institutional autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.42).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.28).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection: Diversity Reading of Race-Conscious Educational Policy").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional/political/educational").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '8a60aa73-f994-4158-9eea-885f1e01446e').
narrative_ontology:cs_kernel_codification('8a60aa73-f994-4158-9eea-885f1e01446e', fixed_text).
narrative_ontology:cs_authority_grounding('8a60aa73-f994-4158-9eea-885f1e01446e', lineage).
narrative_ontology:cs_interpretation_layer_present('8a60aa73-f994-4158-9eea-885f1e01446e').
narrative_ontology:cs_reading_relation('8a60aa73-f994-4158-9eea-885f1e01446e', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a60aa73-f994-4158-9eea-885f1e01446e', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('8a60aa73-f994-4158-9eea-885f1e01446e', foundational, all_students_beneficiary_of_diversity).
narrative_ontology:cs_axiom_status(all_students_beneficiary_of_diversity, holdable).
narrative_ontology:cs_axiom_grounding('8a60aa73-f994-4158-9eea-885f1e01446e', all_students_beneficiary_of_diversity, empirically_contingent).
narrative_ontology:cs_axiom('8a60aa73-f994-4158-9eea-885f1e01446e', foundational, narrow_tailoring_constrains_permissible_race_consciousness).
narrative_ontology:cs_axiom_status(narrow_tailoring_constrains_permissible_race_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('8a60aa73-f994-4158-9eea-885f1e01446e', narrow_tailoring_constrains_permissible_race_consciousness, deontological).
narrative_ontology:cs_reference_frame('8a60aa73-f994-4158-9eea-885f1e01446e', equal_protection_as_permitting_compelling_interest_race_consciousness).
narrative_ontology:cs_drift_state('8a60aa73-f994-4158-9eea-885f1e01446e', contemporary_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a60aa73-f994-4158-9eea-885f1e01446e', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, institutional_education_sector).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, applicants_not_selected_on_diversity_grounds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, underrepresented_minority_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend institutions adopting diversity-weighted admissions. The diversity reading asserts that exposure to racially diverse peer groups produces educational gains (enhanced critical thinking, reduced prejudice, broader civic understanding) and social goods (intergroup competence, cross-group relationships). Students do not choose their peers directly; institutional admissions policy determines classroom composition. The benefit is asserted to inhere in the classroom experience itself; students are framed as passive beneficiaries of institutional choice.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    powerless, biographical, constrained, national).

% Applicants (disproportionately white, though not exclusively) rejected or waitlisted when diversity-weighted decisions favor underrepresented-group applicants. They bear the measurable cost of lost admission opportunity. The constraint treats their race (majority status) as a legitimate criterion for weighted consideration; race becomes a targeting mechanism for allocation of admissions slots. They can exit by applying to alternative institutions; the constraint does not foreclose education but constrains entry to specific institutions. The cost is concentrated and uncompensated—no stated benefit accrues to them from the policy (unlike the diversity-framed benefit to all students).
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, applicants_not_selected_on_diversity_grounds, payer,
    moderate, biographical, mobile, national).

% Applicants from racial groups designated as underrepresented in higher education (Black, Latino, Native American applicants in US context). They gain admission access through diversity-weighted consideration—they are beneficiaries of the policy in the tangible sense of increased admissions likelihood. However, the diversity reading justifies their selection not as remedy for group subordination but as means to the collective (all-students) educational good: they are selected to serve diversity function for others. This creates a dual position: they benefit (admission access) but are instrumentalized (their presence justified by utility to others, not by their own needs). The reading does not treat them as remedy-recipients or as primary beneficiaries; it treats them as diversity-serving tools.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, underrepresented_minority_applicants, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, underrepresented_minority_applicants, payer).

% Universities and colleges adopt and enforce race-conscious admissions policies, framing them as advancing educational mission and institutional values. They set criteria, conduct reviews, litigate in defense, and issue policy statements. They benefit by retaining autonomy over admissions (resisting colorblind mandates that would eliminate race-consciousness) and by avoiding explicit remedial justifications (which might require different metrics and accountability structures). The diversity reading aligns race-conscious policy with educational philosophy and mission rather than with historical-injustice remediation. Institutions position themselves as pursuing intrinsic educational goods, not compensating for past wrongs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, institutional_education_sector, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, institutional_education_sector, beneficiary).

% The judiciary and constitutional scholars interpret the equal protection clause. Under the diversity reading, courts determine whether race-conscious admissions survive constitutional scrutiny via the compelling-interest / narrow-tailoring test. Judges and scholars holding this reading set the legitimacy conditions for institutional policy. They interpret equal protection not as colorblind (reading 1) but as permitting careful consideration of race when justified by institutional interests. Interpreters set doctrine; institutions implement within doctrine's bounds.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, courts_and_constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, jurists, and policy advocates who read equal protection to forbid all governmental racial classification. They contend the clause demands race-neutral decision-making and that the diversity reading violates equal protection by treating applicants as members of racial groups rather than as individuals. They are excluded from institutional admissions policy-setting but retain power through litigation (challenging race-conscious policies in court), legislative advocacy (proposing colorblind mandates), and constitutional interpretation (competing doctrinal positions). They view the diversity reading as a deviation from proper equal protection that instrumentalizes both applicants (by race-targeting) and benefited groups (by reducing them to diversity-serving units).
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_reading_advocates, excluded,
    powerful, generational, constrained, national).

% Legal scholars, civil rights advocates, and judges who read equal protection to require race-conscious remediation of historical group subordination to achieve substantive equality. They view the diversity reading as inadequate: diversity instrumental value does not address the structural injustice remedial policy would correct. They would justify race-conscious policy through necessity (remedying systemic subordination) rather than through institutional interest (diverse learning environments). They are excluded from the institutional policy-justification regime the diversity reading establishes. They contest that the diversity reading, by de-coupling race-consciousness from remedial purpose, actually enables institutions to treat race-conscious admissions as optional institutional preferences rather than as corrective justice.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, remedial_reading_advocates, excluded,
    powerful, generational, constrained, national).

% State and federal legislators could amend education law, restrict institutional autonomy, or override judicial interpretation via statute. Some states have enacted colorblind mandates (banning race-conscious admissions) that override the diversity reading. Most have deferred to judicial interpretation and institutional autonomy. Legislatures observe the constraint from a position of potential override authority but have largely chosen not to exercise it, indicating either deference or political equilibrium where colorblind and diversity/remedial coalitions balance.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, legislative_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, institutional_education_sector).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages three potentially conflicting goods: equal protection norms (forbidding state discrimination), institutional autonomy in admissions (retaining discretion over student selection), and educational quality (defined by institutional mission including diverse learning environments). Without the diversity reading, institutions face a forced choice: abandon race-consciousness entirely (colorblind), or justify it through remedial grounds (addressing historical injustice). The diversity reading creates a third institutional option: race-consciousness is permissible when narrowly tailored to serve a compelling institutional interest (diverse learning environment). This coordinates institutional autonomy with non-discrimination norms by reframing racial consideration as instrumental to educational mission, not compensatory for historical wrong.
% TRANSFER_FUNCTION: Moves admissions opportunity from applicants not selected on diversity grounds (typically majority applicants, especially white applicants) to applicants from designated underrepresented racial groups. The reading frames this as transferring a policy-allocation tool (race-conscious weighting) from majority applicants to minority applicants in service of the collective good (diverse classrooms). Secondary transfer: moves justificatory authority from applicants (who might claim individual merit-desert or colorblind right) to institutions (who assert educational mission justifies race-conscious means). Tertiary transfer: moves burden of litigation/challenge from institutions (defending the constraint) to applicants and civil rights groups (challenging it, incurring legal costs).
% ABSENT_VOICES: Applicants from underrepresented groups whose selection is purely merit-based (benefiting from diversity consideration without awareness or consent to being instrumentalized) are not absent but are structurally invisible in the diversity reading's justification. White applicants and other non-selected applicants have voice through litigation but limited voice in institutional policy deliberation—courts adjudicate, but institutions retain policy setting. Colorblind reading advocates and remedial reading advocates are excluded from the institutional justification regime: their framing of the constraint (colorblind: race classification per se forbidden; remedial: race-consciousness as historical corrective) would redefine what the constraint is. Future generations of applicants are absent from current policy deliberation, yet face the constraints set today.
% DISAPPEARANCE_RATIONALE: If the diversity reading disappeared, institutions would reorganize around the colorblind or remedial readings. Colorblind disappearance path: institutions adopt race-blind admissions, losing claimed diversity benefits and facing homogenization pressures; applicants currently excluded on race-conscious grounds would be readmitted under purely merit criteria (raising questions about what merit means absent racial context). Remedial disappearance path: institutions reframe race-conscious policy as remedying historical subordination rather than achieving diversity; this shifts the political and legal battlefield from institutional-interest-in-diversity to historical-justice-and-structural-remedy. The constraint does not literally prevent race-conscious admissions; it permits a particular justificatory frame. Disappearing it would reorganize the frame, not eliminate the practice—institutions might adopt remedial justifications or courts might impose colorblind mandates. The verdict is contested because beneficiaries (institutions, all-students) dispute whether race-conscious admissions would survive without this reading; payers (non-selected applicants) dispute whether disappearance would end the practice or merely rename it.
% FOUNDING_PROBLEM: A perceived gap between constitutional non-discrimination norms and educational institutions' aspirations for racially diverse student bodies. Civil rights movement and early equal protection doctrine focused on eliminating segregation and discrimination; contemporary institutions assert that educational quality includes exposure to racial diversity. The constraint's founding problem is: how can institutions pursue diversity while respecting equal protection? The colorblind reading answers: they cannot (race-consciousness violates equal protection). The remedial reading answers: through historical remediation of subordination. The diversity reading answers: through narrow tailoring of race-conscious means to the compelling interest in diverse learning environments.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions and institutional scholars corroborate the founding problem as live: diversity aspirations and equal protection norms remain in tension, requiring ongoing doctrinal management. Colorblind advocates corroborate that the problem is a false framing—institutions should abandon race-consciousness, not seek a permission frame for it. Remedial advocates corroborate that the problem is mislabeled: the real ongoing problem is unremitted historical injustice; diversity rationales sidestep it. No authority outside the institutional beneficiary sector attests that the diversity reading's framing of the problem is correct. Legislative and public opinion data are contested: some jurisdictions have voted for colorblind mandates (treating the diversity reading as illegitimate), others have affirmed institutional autonomy (implicitly accepting the diversity reading). The corroboration is one-sided (from the benefiting institutional seat) and contested by alternative readings.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) rather than high because the diversity reading includes genuine narrow-tailoring requirements—institutions must justify race consideration by specific educational mission and cannot use race as a proxy for unrelated goals. This constrains the extraction: not every racial classification passes scrutiny, only those serving compelling educational interests. Suppression is low-moderate (0.28) because applicants retain legal voice through litigation and can exit to alternative institutions; the constraint does not foreclose their education, only their entry to particular institutions. Theater is low (0.22) because the policy's justification (diversity educational benefits) is substantively contested—colorblind and remedial advocates dispute its sufficiency—so the institutional narrative cannot fully suppress the alternative readings. The measurement series shows extractiveness and suppression rising modestly (0.35→0.42, 0.18→0.28) from interval start to midpoint as litigation intensifies and institutions harden their policy rationales, then stabilizing (slight decline in suppression at end) as the political economy settles into a quasi-stable contestation. This pattern reflects increasing institutional commitment to the reading while applicant litigation persists without dislodging the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat (universities, educational mission interpreters), the diversity reading solves a genuine coordination problem and is experienced as legitimate educational policy. From the payer seat (applicants not selected), the same policy structure operates as coercive race-based allocation: their race is weighted against them in a binary (admitted/rejected) decision. The diversity reading asserts this asymmetry is justified by the collective good; the payer seat experiences the assertion as cover story. The colorblind reading sees the policy as violating individual rights (treating applicants as members of racial groups rather than individuals); the remedial reading sees it as insufficient (failing to address historical injustice directly). Each reading produces a different seat-to-seat directionality profile. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (all students, institutional sector) have directionality near 0.0-0.2 (the constraint subsidizes them—they benefit from diversity framing and institutional autonomy). Payers (applicants not selected on diversity grounds) have directionality near 0.75-0.9 (the constraint extracts admissions opportunity, their race is the targeting mechanism). Underrepresented minority applicants sit near 0.4-0.5 (dual-positioned: gain access but are instrumentalized). This asymmetry is the core structural fact the tangled_rope classification captures: genuine coordination function (managing non-discrimination + diversity aspirations) coupled with asymmetric extraction of admissions opportunity. No directionality_overrides are authored here; the derivation from beneficiary/victim declarations + exit options + power atoms produces the observed spread accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids pure mandatrophy (founding problem solved but constraint persists unchanged) by framing diversity as an ongoing institutional value, not a remedial sunset goal. However, the founding problem (tension between non-discrimination norms and diversity aspirations) shows signs of PARTIAL mandatrophy: if the founding problem were fully solved, the high litigation rate and continued policy contestation would be absent. The measurement series shows litigation and policy rationale-hardening accelerating (suppression rising), suggesting the constraint persists not because the coordination problem remains live but because institutional interests (captured beneficiary sector) defend it against colorblind and remedial challengers. The mandatrophy risk is moderate: the reading maintains a plausible ongoing-coordination narrative (diversity is intrinsically valuable to education), but if litigation eventually forces clarification that diversity benefits are contested/unproven, the constraint could tip from tangled_rope (legitimate asymmetry) to snare (coercive extraction defended by spurious narrative). The theater_ratio increase (0.12→0.22) signals growing performative activity (institutional defenses, counter-litigation, policy statements) relative to substantive coordination—a classic mandatrophy symptom. The constraint does not yet meet the threshold for declaring mandatrophy_resolved: the founding problem (diversity-non-discrimination tension) has not demonstrably been solved, only institutionalized as permanently contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_benefits_empirical_validity,
    'Do diverse learning environments actually produce the educational and social benefits (critical thinking, reduced prejudice, civic understanding, cross-group competence) the diversity reading asserts?',
    'Longitudinal educational outcome studies comparing diverse vs. homogeneous cohorts, controlling for selection effects; psychological research on prejudice reduction and intergroup contact; labor market follow-up studies tracking collaboration and civic participation across diverse peer networks.',
    'If diversity benefits are robust, the constraint''s coordination function is real and mandatrophy risk declines. If benefits are marginal, small, or group-specific, the constraint tips toward snare (coercive racial allocation dressed as educational mission). If benefits accrue only to non-selected applicants (not to all students as the reading claims), the reading''s core axiom (all-students-beneficiary) is empirically overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefits_empirical_validity, empirical, 'Whether the diversity reading''s claimed educational benefits of racial diversity are empirically substantiated.').

omega_variable(
    narrow_tailoring_specification_gap,
    'What constitutes ''narrow tailoring'' for race-conscious admissions in practice? How much race-conscious weighting does the constraint actually permit, and does institutional practice stay within constitutional limits?',
    'Doctrinal analysis of court decisions defining narrow tailoring in admissions context; audit of institutional admissions practices against stated narrow-tailoring doctrine; litigation outcomes testing whether institutions'' actual weighting of race survives constitutional scrutiny.',
    'If narrow tailoring is concretely specified and enforced, extractiveness is constrained and the constraint remains tangled_rope. If narrow tailoring is vague and permissive, extractiveness rises and the constraint approaches snare (race-conscious allocation with minimal doctrinal constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_tailoring_specification_gap, empirical, 'Whether the narrow tailoring requirement meaningfully constrains institutional race-conscious weighting in practice.').

omega_variable(
    instrumental_vs_remedial_boundary,
    'Can the diversity reading''s instrumentalization of minority applicants (selecting them as means to all-students'' benefit) be distinguished from the remedial reading''s treatment of them as remedy-beneficiaries? Or does the diversity framing suppress awareness of historical subordination in a way that functions identically to coercive racial classification?',
    'Philosophical/conceptual analysis of whether instrumental selection vs. remedial selection can be coherently distinguished from the applicant''s standpoint; sociological study of how institutions and applicants describe and understand the diversity reading''s justification (does the all-students-benefit framing suppress awareness of remedial context, or does it add a distinct justification layer?).',
    'If the readings are genuinely distinct in their logical structure and institutional function, the diversity reading remains a live, structurally defensible alternative to colorblind and remedial readings. If the readings collapse into functional equivalence (both directing admissions toward minority applicants, with different narrative justifications), the diversity reading becomes a cover story and mandatrophy risk rises—the founding problem (remedying subordination or managing diversity) persists unsolved beneath the institutional framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumental_vs_remedial_boundary, conceptual, 'Whether the diversity reading''s instrumentalization of minority applicants can be coherently distinguished from remedial justifications, or whether it is a functional variant with different narrative framing.').

omega_variable(
    reading_kernel_authority_source,
    'What legitimate authority grounds the diversity reading''s interpretation of the equal protection clause kernel? Is it judicial precedent (doctrine), institutional practice (de facto authority), constitutional text (original meaning), or evolving societal understanding?',
    'Trace the doctrine''s developmental history; audit which authority seat (courts, institutions, scholarly consensus, public opinion) has advanced each reading; test whether courts and institutions would abandon the diversity reading if one authority source (e.g., SCOTUS precedent) shifted.',
    'If authority is distributed (courts, institutions, and scholarship align), the reading is robust. If authority is narrowly held (courts only, or institutions only against judicial skepticism), the reading is fragile to authority shift. This determines whether the constraint persists as enduring coordination or degrades to performative maintenance (piton) when authority erodes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_authority_source, empirical, 'What authority structure sustains the diversity reading''s interpretation of equal protection, and how robust is that authority to institutional shifts.').

omega_variable(
    committer_reading_contest_structure,
    'The three readings (colorblind, diversity, remedial) of the equal protection kernel represent a genuine constitutional contest or a provisional unstable equilibrium? Which reading''s framing will ultimately dominate institutional practice and constitutional doctrine?',
    'Long-term litigation trajectory (which reading''s doctrine courts favor over decades); institutional choice analysis (do institutions adopt diversity reading out of conviction or strategic calculation to navigate colorblind and remedial pressures?); comparative constitutional law (how other democracies resolve this contest, and do their solutions suggest any reading is epistemically superior).',
    'This omega documents the irreducible political/constitutional contestation the kernel embodies. The diversity reading is one live position in an ongoing dispute; its persistence as a structurally valid constraint depends on neither colorblind nor remedial readings achieving hegemonic authority. If one reading forecloses another (which the schema rules permit but require explicit documentation), this omega would be resolved by that foreclosure. Until then, the diversity reading remains one of three coexisting live interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_contest_structure, preference, 'Which reading of the equal protection kernel will ultimately dominate, or whether the contest remains permanently unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__diversity_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__diversity_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__diversity_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__diversity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__diversity_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__diversity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__diversity_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__diversity_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__diversity_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__diversity_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__diversity_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__diversity_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__diversity_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__diversity_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__diversity_reading, suppression_requirement, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.18).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal protection clause kernel admits three structurally distinct readings (constraint_family): colorblind_reading (all racial classification forbidden), diversity_reading (race-conscious policy permitted when serving compelling educational interests), remedial_reading (race-conscious remediation required to achieve substantive equality). Each reading instantiates a different constraint with different ε, beneficiary/victim sets, and legitimacy narratives. The readings are linked as coexisting live interpretations of the same constitutional text. See commentary.kernel_context for framing details and omega variables documenting the reading contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, powerless, 0.15).
constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
