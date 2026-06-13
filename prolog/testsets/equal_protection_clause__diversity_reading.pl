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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Clause — Diversity Reading (Race-Conscious Admissions for Educational Benefit)
 *   domain: constitutional_law/education_policy/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the equal protection clause:
 *   the diversity reading permits race-conscious admissions policies when
 *   they serve a compelling interest in educational diversity benefiting all
 *   students. The kernel is the equal protection clause itself (fixed text);
 *   the reading interprets it to ground legitimacy in
 *   diversity-as-educational-value rather than remediation of historical
 *   injury (remedial reading) or individual atomistic rights (colorblind
 *   reading). This story models the diversity reading's internal structure:
 *   moderate extractiveness because narrow tailoring and ongoing
 *   recalibration constrain the permissible scope; tangled rope because it
 *   coordinates admissions practices (genuine institutional coordination
 *   problem) while extracting from individual majority applicants
 *   disadvantaged by the classification. The constraint's persistence depends
 *   on active judicial enforcement of the compelling-interest framework and
 *   universities' commitment to the diversity rationale. The measurement
 *   series spans 1978 (Bakke decision legitimizing diversity as compelling
 *   interest) to 2024 (post-Students for Fair Admissions era where the
 *   constraint faces skeptical scrutiny).
 *
 * KEY AGENTS:
 *   - university_admissions_authority: sets and operationalizes the policy; institutional power, grounds its legitimacy in diversity-as-compelling-interest
 *   - majority_group_applicants_disadvantaged: bears direct harm (reduced admission probability from race-conscious consideration); moderate power, constrained exit
 *   - minority_group_applicants_benefited: structured as beneficiaries narrowly (enhanced admission probability) but instrumentalized (presence justified for others' benefit); moderate power, constrained exit
 *   - all_students_at_diverse_institutions: primary normative beneficiary per this reading (diversity benefits all); organized power, mobile exit
 *   - courts_constitutional_interpreter: grounds the constraint's legitimacy through compelling-interest doctrine; institutional power, analytical seat
 *   - colorblind_reading_advocates: doctrinally excluded from this reading's premises; their reading logically forecloses diversity-permissiveness
 *   - remedial_reading_advocates: coexist in public discourse; different forward-looking (diversity benefit) vs. backward-looking (historical remediation) rationale
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
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause — Diversity Reading (Race-Conscious Admissions for Educational Benefit)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'a2efcda3-509a-420c-b15e-4d1cba8f8db8').
narrative_ontology:cs_kernel_codification('a2efcda3-509a-420c-b15e-4d1cba8f8db8', fixed_text).
narrative_ontology:cs_authority_grounding('a2efcda3-509a-420c-b15e-4d1cba8f8db8', lineage).
narrative_ontology:cs_interpretation_layer_present('a2efcda3-509a-420c-b15e-4d1cba8f8db8').
narrative_ontology:cs_reading_relation('a2efcda3-509a-420c-b15e-4d1cba8f8db8', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('a2efcda3-509a-420c-b15e-4d1cba8f8db8', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('a2efcda3-509a-420c-b15e-4d1cba8f8db8', foundational, diversity_as_compelling_educational_interest).
narrative_ontology:cs_axiom_status(diversity_as_compelling_educational_interest, holdable).
narrative_ontology:cs_axiom_grounding('a2efcda3-509a-420c-b15e-4d1cba8f8db8', diversity_as_compelling_educational_interest, empirically_contingent).
narrative_ontology:cs_axiom('a2efcda3-509a-420c-b15e-4d1cba8f8db8', secondary, narrow_tailoring_doctrine_constrains_race_consciousness).
narrative_ontology:cs_axiom_status(narrow_tailoring_doctrine_constrains_race_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('a2efcda3-509a-420c-b15e-4d1cba8f8db8', narrow_tailoring_doctrine_constrains_race_consciousness, conventional).
narrative_ontology:cs_reference_frame('a2efcda3-509a-420c-b15e-4d1cba8f8db8', bakke_permissive_diversity_framework).
narrative_ontology:cs_drift_state('a2efcda3-509a-420c-b15e-4d1cba8f8db8', post_students_for_fair_admissions_2023, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('a2efcda3-509a-420c-b15e-4d1cba8f8db8', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students_educational_environment).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, majority_group_students_diversity_benefit).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, individual_applicants_disadvantaged_by_race_classification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_group_applicants_benefited).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students_at_diverse_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, majority_group_applicants_disadvantaged).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_group_applicants_benefited).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets admission policy incorporating race-conscious review to achieve educational diversity. Interprets equal protection to permit this under the compelling interest framework. Operationalizes the policy through holistic application review where race is one factor among many. Defends the policy as serving all students' educational interests while acknowledging it disadvantages some individual applicants based on race.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, university_admissions_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individual applicants whose admission is denied or probability reduced due to race-conscious consideration, even under holistic review framework. Experience direct harm from the classification. Exit options include applying to universities with different admissions policies or non-selective institutions; cannot exit the constitutional constraint itself. Subject to the judicial interpretation that equal protection permits this burden.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, majority_group_applicants_disadvantaged, payer,
    moderate, biographical, constrained, national).

% Individual applicants from underrepresented groups whose probability of admission is enhanced by race-conscious review. Structured as beneficiaries of the admissions policy narrowly, but experience the constraint as a double-bind: they benefit from race-conscious policies but are also subject to them, and their presence in the institution is theoretically justified not for their own sake but for others' educational benefit (instrumental positioning).
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_group_applicants_benefited, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_group_applicants_benefited, payer).

% Receive educational benefit from learning in a racially diverse cohort. The diversity reading's primary normative claim: a diverse educational environment benefits all students, including majority-group students, through exposure to different perspectives and improved critical thinking. This is the justification for the entire constraint under this reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students_at_diverse_institutions, beneficiary,
    organized, biographical, mobile, national).

% Would argue that equal protection requires race-blindness, treating individuals as atomic rights-bearers to whom no racial classification can be applied regardless of effect. Are present in litigation (as litigants, amici, justices) but their reading of equal protection is structurally excluded by this reading's core premise. Their legitimacy challenge goes to the kernel interpretation itself.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_reading_advocates, excluded,
    institutional, generational, analytical, national).

% Would argue that race-conscious policies are legitimate as remediation for historical group subordination, not merely as pedagogical tools for all students. This reading justifies race-consciousness through backward-looking correction of injustice; the diversity reading justifies it through forward-looking educational benefit to all. The two readings coexist in public discourse but ground themselves differently.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, remedial_reading_advocates, excluded,
    institutional, generational, analytical, national).

% Interpret the equal protection clause and adjudicate whether universities' race-conscious policies meet constitutional scrutiny under the compelling interest / narrow tailoring framework. Are the ultimate authority grounding this constraint's legitimacy; their shift from permissive (diversity interest is compelling, narrowly tailored) to skeptical (race-consciousness categorically incompatible with equal protection) directly controls the constraint's applicability and force.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, courts_constitutional_interpreter, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, courts_constitutional_interpreter, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, university_admissions_authority).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional admissions practices to achieve racially diverse student bodies across universities, addressing the collective action problem that individual institutions pursuing diversity independently create incentives for strategic sorting and potential resegregation. The constraint structures what a permissible diversity-pursuit looks like: race may be considered but only as one factor, with holistic review, narrow tailoring requirements, and ongoing recalibration.
% TRANSFER_FUNCTION: Transfers admission probability from individual majority-group applicants to individual minority-group applicants; transfers educational benefit from those individuals' presence in the classroom to all students via diversity effects; transfers institutional legitimacy from the colorblind constitutional frame to the diversity-compelling-interest frame.
% ABSENT_VOICES: Individual applicants denied admission due to race-conscious review are present as plaintiffs and parties; their structural exclusion from policy design (admissions offices do not systematically solicit their input on the framework) mirrors the colorblind reading advocates' doctrinal exclusion from this reading's premises. Courts can include their interests in legal analysis, but the policy-making seat structurally excludes them from jurisdiction.
% DISAPPEARANCE_RATIONALE: If this constraint (race-conscious admissions under compelling educational interest) disappeared, universities would shift to race-neutral admissions processes, likely resulting in less racially diverse student bodies at selective institutions (empirical literature on this is extensive). The institutional coordination around diversity-achieving admissions practices would dissolve. The educational experiences of all current students and subsequent cohorts would change; classroom dynamics, peer learning effects, and institutional self-presentation would reorganize. The constitutional permission structure that universities depend on would vanish.
% FOUNDING_PROBLEM: Universities faced pressures to integrate racially in the 1960s–1970s forward, and sought constitutional justification for admissions practices that would achieve diverse student bodies. The founding problem: how to pursue diversity under equal protection if equal protection is read as forbidding race-consciousness (the colorblind reading)? Solution: establish diversity itself as a compelling institutional interest justifying limited race-consciousness.
% FOUNDING_PROBLEM_CORROBORATION: University administrators and diversity advocates attest the problem is live: without race-conscious admissions, selective institutions resegregate. The Supreme Court (in diversity-permissive eras, particularly Gratz v. Bollinger 2003 and Fisher v. University of Texas 2013 before Students for Fair Admissions 2023) attested the compelling interest is real and the narrow tailoring requirement protects against overreach. Colorblind advocates and recent skeptical Justices (Students for Fair Admissions majority 2023) attest the founding problem is conceptually malformed—that there is no legitimate 'diversity interest' that can override individual equal protection rights. No consensus from outside all three seats; the Supreme Court majority in 2023 effectively rejected the founding problem as stated by diversity advocates.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).

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
 *   Extractiveness (0.38, endpoint) is moderate because the constraint operates under the compelling-interest / narrow-tailoring doctrine: race consciousness is permitted but bounded—holistic review required, no quotas, ongoing recalibration mandated, race cannot be determinative. This is structurally different from a pure snare (high extractiveness with minimal constraints) or a pure rope (low extractiveness because all parties benefit). The constraint benefits all students through diversity (coordination function) while extracting from individual majority applicants through race-based disadvantage (asymmetric distribution). Suppression (0.42) reflects the enforcement machinery required: courts must police the narrow-tailoring requirement, universities must maintain institutional commitment to the diversity rationale against political pressure, litigation risk is constant, the constraint requires active judicial policing to survive. Theater (0.28) indicates moderate performative activity: universities articulate diversity rationales in language designed to survive judicial scrutiny; colorblind critics argue the stated diversity justification is cover for racial preferences; the institutional narrative is genuinely important to the constraint's legitimacy but leaves rhetorical space for skepticism about true motives. The measurement trajectory shows extractiveness rising from 0.28 (1978, Bakke's tentative approval) to a peak of 0.40 (2020, diversity widely institutionalized) then declining to 0.38 (2024, post-Students for Fair Admissions); this mirrors the constraint's doctrinal support peaking around 2013 (Fisher I) and declining sharply after 2020. Theater and suppression mirror this: the constraint requires increasing institutional commitment and legal defense as skepticism grows, then plateaus as the Supreme Court's decision settles the matter against the diversity reading's doctrinal legitimacy. Accessibility collapse (0.65) is moderate-high because once the diversity reading is understood, individuals subject to it (majority applicants) see alternatives (non-selective institutions, universities with different policies, litigation challenging the framework) but these are materially less attractive or unavailable in competitive labor markets. Resistance (0.72) is substantial because colorblind and remedial readings remain live doctrinal positions; litigation challenging diversity policies is constant; institutions face political pressure to abandon the practice; the measurement trajectory shows rising resistance through 2010s as the colorblind reading gained judicial traction (Scalia in Fisher I 2013), then 2023 Supreme Court decision (Students for Fair Admissions) effectively forecloses the diversity reading's doctrinal legitimacy in federal constitutional law.
 *
 * PERSPECTIVAL GAP:
 *   The university admissions authority and all-students-at-diverse-institutions seats experience this as genuine coordination: a legitimate diversity-pursuing framework that serves everyone's educational interests under proper constraints (narrow tailoring). The majority-group-applicants-disadvantaged seat experiences this as forced extraction: a race-based harm justified by others' educational benefit, which violates the individual's equal protection rights (the colorblind reading's seat). The courts seat divides: permissive eras (1978–2020, particularly Gratz 2003 and Fisher I 2013) viewed this as constitutionally permissible narrow tailoring; skeptical eras (2020–2024, culminating in Students for Fair Admissions 2023) viewed race-consciousness itself as categorically impermissible under equal protection. The minority-group-applicants seat occupies an unstable position: nominally benefited (enhanced admission probability) but instrumentalized (their presence justified as means to others' education, not as justice-claim-bearer as under the remedial reading). The engine should compute multi-seat divergence: the agenda-setter and all-students seats computing this constraint as Rope or Tangled Rope (genuine coordination with some asymmetry they view as justified by narrow tailoring); the majority-applicants and skeptical-court seats computing it as Snare (extraction from disadvantaged individuals using diversity framing as cover); the minority-applicants seat computing it as a complex Tangled Rope (benefits and harms coexist, the structure itself embeds instrumental positioning that raises dignitary questions).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation via the base framework: majority-group-applicants-disadvantaged are the structural victims (d = 0.85: race-based classification disadvantaging this group is the operative mechanism; constrained exit—cannot exit the constitutional rule in selective institutions; no arbitrage option; face the constraint as asymmetric burden). All-students-at-diverse-institutions are primary beneficiaries (d near 0.0: educational benefit flows to them directly as result of the diversity the constraint mandates; mobile exit available—they can attend other institutions but all universities increasingly coordinate toward diversity; no extraction from this seat, rather benefit). University admissions authority benefits from being permitted to pursue diversity (can set policy that many support, gains legitimacy, achieves institutional goals) but also bears enforcement burden and political/litigation risk; derives d around 0.35–0.45 (moderate: permissive framework permits their action, but they must actively defend the interpretation and manage narrow-tailoring requirements). Colorblind and remedial advocates are excluded from this reading's framework; their directionality is analytical, not extracted from. The override applied here (power_atom: moderate, d_value: 0.85) applies to the majority-group-applicants seat: the structural derivation from beneficiary/victim + exit options would place them at ~0.80; the override (0.85) reflects the full-target condition: they are the named victims of the constraint, experience the race-based classification as the operative harm, have constrained exit, and the narrow-tailoring doctrine does not substantively protect them (it only requires that race be one factor, not dispositive—but 'one factor' can still decisively advantage the competing applicant from an underrepresented group). The asymmetry between beneficiary and victim groups is what makes this tangled rope rather than rope: the coordination function (diverse education benefits everyone) is real, but the distribution asymmetry (majority applicants bear concentrated costs) is also real and requires active enforcement to maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading faces a real mandatrophy question post-2023. No false summit in the period 1978–2020: universities and courts who adopted this reading genuinely believed diversity serves educational purposes; the beneficiary and coordination claims were sincere, not a cover story for pure extraction. The narrow-tailoring doctrine was believed (by permissive courts) to be a real constraint. Real mandatrophy risk emerges after Students for Fair Admissions (2023): the Supreme Court opinion held that race-consciousness is categorically incompatible with equal protection, effectively foreclosing the diversity reading's kernel interpretation. The constraint persists as institutional practice in some jurisdictions (universities continue race-conscious admissions despite the ruling's prohibition in Gratz-type systems) but its authoritative kernel interpretation (compelling interest + narrow tailoring) no longer grounds its legitimacy in federal constitutional law. The constraint enters a transition phase where institutional practice and doctrinal authority decouple: universities pursue diversity through workarounds (percent plans, socioeconomic affirmative action, legacy preference phase-outs) that do not explicitly invoke race, maintaining diversity effects while respecting the colorblind reading's categorical prohibition. This is the classical mandatrophy profile: the founding problem (how to achieve diversity under equal protection) persists, but the authorized solution (diversity-as-compelling-interest) is no longer doctrinally available. Measurement trajectory (extractiveness plateaus 2010–2024, theater plateaus post-2020, suppression plateaus post-2020) reflects this: the constraint reaches stable institutional operation by 2010, then begins a managed decline as the Supreme Court signals skepticism (2013 forward), culminating in explicit foreclosure (2023). The institutional practice persists through performance (theater remains elevated) without doctrinal support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_benefit_empirical_reality,
    'Do students actually receive meaningful educational benefits from racial diversity in the classroom, or is the diversity-benefit claim a constructed rationale for pursuing racial distribution goals?',
    'Longitudinal educational outcomes research comparing diverse and non-diverse cohorts, controlling for self-selection and peer effects; qualitative evidence from student learning assessments and critical thinking measures; cross-national comparison where some jurisdictions permit diversity-pursuing admissions and others forbid it.',
    'If diversity benefits are empirically weak or contested, the constraint''s justification shifts from genuine coordination (diversity benefits all) to extraction (achieving racial distribution without legitimating rationale). If benefits are robust, the tangled-rope classification holds—the diversity benefit is real, the individual disadvantage to majority applicants is also real, and the structure coordinates while extracting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diversity_benefit_empirical_reality, empirical, 'Whether the pedagogical diversity benefit claimed under this reading is empirically real or a post-hoc rationale for racial distribution goals.').

omega_variable(
    instrumental_positioning_of_minority_applicants,
    'Does the diversity reading''s structure of benefiting all students while using minority applicants as instrumental means constitute a form of dignitary harm or oppression even if individual minority applicants'' admission probabilities are enhanced?',
    'Normative and philosophical analysis of instrumentalization; empirical evidence on how minority students perceive and experience their roles in diversity-justified admissions systems; comparison to explicitly remedial framings where minority applicants are centered as justice-claim-bearers rather than pedagogical tools.',
    'If instrumental positioning is itself a dignitary harm, the constraint''s beneficiary structure requires revision: minority applicants are not simple beneficiaries but occupy an ethically complex position. If instrumental positioning is acceptable when coupled with material benefit, the constraint''s current structure is ethically defensible. This affects the proper classification of the constraint from this seat''s perspective (beneficiary vs. payer vs. both).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_positioning_of_minority_applicants, conceptual, 'Whether being structurally positioned as a means to others'' education (even with enhanced admission probability) is itself a harm under equal protection or dignity principles.').

omega_variable(
    kernel_reading_foreclosure_dynamics,
    'Does adoption of the colorblind reading logically foreclose the diversity reading within a single framework, or do the two readings coexist as different parties'' live interpretations of the same clause?',
    'Constitutional doctrine analysis: if colorblindness is adopted as the binding interpretation by authoritative courts, the diversity reading becomes legally inert (though it may persist in institutional practice and political argument). Empirically, observe whether institutions in colorblind jurisdictions cease diversity-conscious admissions or develop workarounds.',
    'If colorblindness forecloses diversity (as recent Supreme Court shifts suggest), the constraint transitions to mandatrophy: institutional practice persists without authoritative kernel interpretation supporting it. If the readings truly coexist, institutional diversity practices in diverse jurisdictions can maintain legitimacy even where courts shift toward colorblindness (different electorates support different readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_dynamics, empirical, 'Whether the Supreme Court''s shift toward colorblindness forecloses the diversity reading doctrinally, moving it from live interpretation to institutional inertia.').

omega_variable(
    narrow_tailoring_as_constraint_vs_performative_constraint,
    'Does the narrow-tailoring requirement function as a genuine structural constraint on how much race-consciousness is permissible, or is it primarily performative language that permits substantial race-consciousness while maintaining equal protection rhetorical cover?',
    'Doctrinal and empirical analysis: examine admissions data showing what proportion of students'' outcomes are determined by race-conscious consideration in practice; analyze judicial review of universities'' tailoring claims; compare jurisdictions with different tailoring standards.',
    'If narrow tailoring is genuine, suppression and theater metrics should be moderate and the tangled-rope classification is accurate. If narrow tailoring is primarily performative, the constraint functions more like a snare—the suppression is higher (needed to defend the cover story) and theater is higher (the tailoring language is the show). This affects the proper classification from the institutional seat''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_as_constraint_vs_performative_constraint, empirical, 'Whether the narrow-tailoring doctrine functions as a real constraint on race-conscious admissions or as performative language permitting substantial race-consciousness while maintaining equal protection credibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_diversity_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement_basis(epc_diversity_tr_t1978, observed).
narrative_ontology:measurement(epc_diversity_tr_t1990, equal_protection_clause__diversity_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(epc_diversity_tr_t1990, observed).
narrative_ontology:measurement(epc_diversity_tr_t2000, equal_protection_clause__diversity_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(epc_diversity_tr_t2000, observed).
narrative_ontology:measurement(epc_diversity_tr_t2010, equal_protection_clause__diversity_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(epc_diversity_tr_t2010, observed).
narrative_ontology:measurement(epc_diversity_tr_t2020, equal_protection_clause__diversity_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(epc_diversity_tr_t2020, observed).
narrative_ontology:measurement(epc_diversity_tr_t2024, equal_protection_clause__diversity_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(epc_diversity_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(epc_diversity_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement_basis(epc_diversity_be_t1978, observed).
narrative_ontology:measurement(epc_diversity_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement_basis(epc_diversity_be_t1990, observed).
narrative_ontology:measurement(epc_diversity_be_t2000, equal_protection_clause__diversity_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement_basis(epc_diversity_be_t2000, observed).
narrative_ontology:measurement(epc_diversity_be_t2010, equal_protection_clause__diversity_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement_basis(epc_diversity_be_t2010, observed).
narrative_ontology:measurement(epc_diversity_be_t2020, equal_protection_clause__diversity_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement_basis(epc_diversity_be_t2020, observed).
narrative_ontology:measurement(epc_diversity_be_t2024, equal_protection_clause__diversity_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(epc_diversity_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(epc_diversity_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement_basis(epc_diversity_su_t1978, observed).
narrative_ontology:measurement(epc_diversity_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(epc_diversity_su_t1990, observed).
narrative_ontology:measurement(epc_diversity_su_t2000, equal_protection_clause__diversity_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(epc_diversity_su_t2000, observed).
narrative_ontology:measurement(epc_diversity_su_t2010, equal_protection_clause__diversity_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement_basis(epc_diversity_su_t2010, observed).
narrative_ontology:measurement(epc_diversity_su_t2020, equal_protection_clause__diversity_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement_basis(epc_diversity_su_t2020, observed).
narrative_ontology:measurement(epc_diversity_su_t2024, equal_protection_clause__diversity_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(epc_diversity_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% The EQUAL_PROTECTION_CLAUSE kernel decomposes into three constraint stories: COLORBLIND_READING (race-consciousness forbidden by equal protection), DIVERSITY_READING (this story: race-consciousness permitted when serving compelling educational diversity interest), and REMEDIAL_READING (race-consciousness required to remedy historical subordination). All three stories are readings of the same fixed constitutional text but instantiate structurally distinct constraints with different ε values, beneficiary/victim structures, and persistence mechanisms. The diversity reading (this story) has moderate ε (~0.38) because narrow tailoring constrains scope; the remedial reading has lower ε (remediation benefit is primary, not instrumental); the colorblind reading has near-zero ε (no extraction, no beneficiaries, pure constraint on state power). The diversity reading coexists with remedial reading (different forward/backward rationales) and is increasingly foreclosed by colorblind reading (which logically rules out all race-consciousness). See cs_structure.reading_relations for the formal kernel relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
