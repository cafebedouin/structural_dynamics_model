% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection as Diversity-Permitting Commitment (Diversity Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The diversity reading of the equal protection commitment interprets the
 *   Constitution to permit universities to use race as one factor among many
 *   in holistic admissions review to achieve the compelling state interest of
 *   educational diversity. Under this reading, equal protection does not
 *   forbid all state consideration of race; it forbids only racial
 *   classifications that are not narrowly tailored to achieve a compelling
 *   interest. The diversity reading licenses race-consciousness in admissions
 *   as long as it remains procedural (factor among many, not a quota),
 *   justified (toward educational mission), and reviewable (subject to
 *   periodic scrutiny). The constraint's extractiveness is moderate-to-low
 *   (0.28) because it is primarily procedural and permissive rather than
 *   coercive: it grants discretion to universities rather than commanding
 *   specific outcomes. Suppression is similarly moderate (0.31) because the
 *   constraint persists through judicial enforcement and institutional
 *   adoption rather than through high coercive intensity. The reading is
 *   claimed as rope—genuine coordination enabling universities to solve the
 *   institutional bind of pursuing diversity within constitutional limits—but
 *   the cost to rejected applicants is real and asymmetric, creating the
 *   rope-vs-tangled-rope tension.
 *
 * KEY AGENTS:
 *   - Universities and colleges: institutional beneficiaries, gain discretion to use race as admissions factor
 *   - Applicants excluded holistically: moderate-power payers, rejected under diversity-weighted admissions
 *   - Beneficiary racial groups: dual-positioned (beneficiaries of enhanced chances, payers of stigma cost)
 *   - Majority-group applicants rejected: powerless payers, no institutional voice
 *   - Courts and judges: agenda-setters, interpret and enforce the constraint
 *   - Colorblind reading adherents: excluded, would contest the constraint's legitimacy
 *   - Remedial reading adherents: excluded, view the reading as insufficiently radical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.31).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection as Diversity-Permitting Commitment (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '6dff2028-9500-401d-9176-c56b98d5e9cf').
narrative_ontology:cs_kernel_codification('6dff2028-9500-401d-9176-c56b98d5e9cf', fixed_text).
narrative_ontology:cs_authority_grounding('6dff2028-9500-401d-9176-c56b98d5e9cf', lineage).
narrative_ontology:cs_interpretation_layer_present('6dff2028-9500-401d-9176-c56b98d5e9cf').
narrative_ontology:cs_reading_relation('6dff2028-9500-401d-9176-c56b98d5e9cf', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dff2028-9500-401d-9176-c56b98d5e9cf', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('6dff2028-9500-401d-9176-c56b98d5e9cf', foundational, race_permissible_as_one_factor_in_holistic_review).
narrative_ontology:cs_axiom_status(race_permissible_as_one_factor_in_holistic_review, holdable).
narrative_ontology:cs_axiom_grounding('6dff2028-9500-401d-9176-c56b98d5e9cf', race_permissible_as_one_factor_in_holistic_review, deontological).
narrative_ontology:cs_axiom('6dff2028-9500-401d-9176-c56b98d5e9cf', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('6dff2028-9500-401d-9176-c56b98d5e9cf', educational_diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('6dff2028-9500-401d-9176-c56b98d5e9cf', secondary, narrow_tailoring_requirement_enforces_constraint).
narrative_ontology:cs_axiom_status(narrow_tailoring_requirement_enforces_constraint, holdable).
narrative_ontology:cs_axiom_grounding('6dff2028-9500-401d-9176-c56b98d5e9cf', narrow_tailoring_requirement_enforces_constraint, conventional).
narrative_ontology:cs_reference_frame('6dff2028-9500-401d-9176-c56b98d5e9cf', equal_protection_permits_race_consciousness_toward_diversity).
narrative_ontology:cs_drift_state('6dff2028-9500-401d-9176-c56b98d5e9cf', contemporary_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6dff2028-9500-401d-9176-c56b98d5e9cf', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_and_colleges).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants_excluded_holistically).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, beneficiary_racial_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, beneficiary_racial_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, majority_group_rejected).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain the constitutional permission and institutional discretion to use race as one factor in holistic admissions review to build diverse student bodies aligned with their educational missions. This reading licenses them to weigh racial composition without needing to prove remediation of past institutional discrimination. They retain authority to set admissions policy subject to narrow-tailoring review (race as one factor, not a quota, subject to periodic scrutiny). Universities experience the constraint as enabling rather than restricting: it solves the institutional bind of pursuing diversity within constitutional limits.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_and_colleges, beneficiary,
    institutional, generational, constrained, national).

% Include applicants (disproportionately white and Asian-American applicants in highly selective contexts) whose qualifications would have secured admission under a race-neutral metric but are rejected under holistic review that factors race toward diversity. They bear the direct cost of the constraint through rejection from their preferred institutions. They experience the constraint as restricting their opportunities. Their exit option is applying to other universities or non-selective institutions, but they cannot exit the constraint's applicability to selective universities they wish to attend.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, applicants_excluded_holistically, payer,
    moderate, immediate, mobile, national).

% Applicants from groups historically underrepresented in selective higher education (Black, Latino/Latina/Latinx, Native American applicants, and sometimes first-generation and low-income applicants) gain enhanced chances of admission under the diversity reading. However, they also bear a secondary cost: the potential stigma burden of being perceived as admitted 'for diversity' rather than merit, and the identity-fusion pressure of absorbing that reading. The constraint is ambiguous for them: procedurally permissive and beneficent at the admissions stage, but socially extractive at the identity and integration stage within the university.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, beneficiary_racial_groups, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, beneficiary_racial_groups, payer).

% Applicants from majority groups (white applicants, and sometimes Asian-American applicants in highly selective institutional contexts) who are rejected from selective universities where race factors into holistic review. They have no institutional voice in the constraint-setting process and cannot collectively challenge individual admissions decisions. Their exit is applying to alternative institutions or non-selective universities. At the institutional level they are trapped; at the system level they are mobile.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, majority_group_rejected, payer,
    powerless, immediate, mobile, national).

% Interpret and enforce the equal protection commitment and determine the boundaries of the diversity reading. Under this reading, they hold that race may factor into admissions to achieve the compelling state interest of educational diversity, subject to narrow tailoring (race as one factor, periodic review required, quotas forbidden). They adjudicate disputes about whether universities' use of race stays within the constraint bounds or violates equal protection. They author the constraint through case law.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Parties (litigants, advocacy groups, judicial dissenters, legislators) who hold that equal protection forbids any state use of race and believe race-blind admissions is the constitutionally correct approach. They are excluded from the diversity reading's legitimating circle and actively contest it through litigation. They have legal standing and political power to challenge the constraint, and their position has gained Supreme Court dominance as of 2023 (Students for Fair Admissions v. Harvard/UNC). They would be the beneficiaries if the colorblind reading displaced the diversity reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_reading_adherents, excluded,
    organized, generational, trapped, national).

% Parties (civil rights scholars, advocacy groups, some judicial dissenters, institutional reformers) who hold that equal protection demands affirmative race-conscious remedies to dismantle caste subordination and ongoing structural racism. They view the diversity reading as insufficiently radical: permitting race-consciousness only as educational philosophy, not as structural repair or reparation. They are absent from the constraint-setting process but present in academic and advocacy discourse. They contest the diversity reading as anemic.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, remedial_reading_adherents, excluded,
    organized, generational, trapped, national).

% May amend the equal protection commitment through constitutional amendment (rare) or create statutory frameworks that interact with and narrow/expand the diversity reading (e.g., state bans on race-conscious admissions, federal civil rights statute amendments). They observe the constraint's operation and can reshape it through legislative action. They are analytical because they can alter the kernel itself.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, universities_and_colleges).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of permitting universities to pursue educational diversity in student bodies while respecting constitutional equal protection doctrine: universities need a constitutional reading that permits them to consider race without violating the Constitution. The diversity reading provides that: race may be used as one factor among many toward the compelling state interest of educational diversity, subject to narrow tailoring and periodic review. Without this coordination function, universities would face an irresolvable tension between their diversity goals and equal protection constraints, forcing them to either abandon diversity or violate constitutional law. The constraint enables the coordination of institutional mission with constitutional legitimacy.
% TRANSFER_FUNCTION: Transfers admissions discretion to universities (the institutional power to use race as an admissions factor without violating equal protection) and transfers rejection risk to applicants (those whose qualifications are sufficient but whose racial identity is not favored by the diversity-weighted holistic review). The constraint moves the cost of diversity-building from the abstract level (universities' institutional obligation) to the individual level (applicant rejection). It also transfers psychological/identity cost to beneficiary racial groups in the form of potential stigma and identity-fusion burden (the uncertain knowledge of being admitted 'for diversity'). The net flow is discretion to universities, rejection risk and identity cost to applicants.
% ABSENT_VOICES: Rejected applicants (especially majority-group applicants rejected under diversity-weighted review) have no voice in the admissions process and cannot collectively contest the constraint. They contest individual decisions through litigation or appeals, but cannot shape the constraint-setting process. Colorblind reading adherents and remedial reading adherents are present in advocacy and litigation but excluded from the institutional consensus that the diversity reading articulates. Applicants from beneficiary racial groups have voice as beneficiaries but not as payers of stigma and identity cost. Legislative bodies and civil society organizations outside universities have limited voice in institutional admissions policy, though they can shape statutory frameworks that narrow or expand the constraint.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight and were replaced by a strict race-neutral equal protection doctrine (the colorblind reading), universities would be required to remove explicit race consideration from admissions procedures. Diversity metrics would change markedly; the racial/ethnic composition of selective university student bodies would shift (decreasing Black, Latino, Native American enrollment; increasing Asian-American and white enrollment, depending on institutional context). Institutional decisions about mission, student body composition, and admissions criteria would reorganize around alternative factors (socioeconomic status, geographic origin, first-generation status, legacy status, test scores). The educational landscape, university cultures, and career pipelines would rearrange in response. The constraint's removal would produce measurable institutional reorganization.
% FOUNDING_PROBLEM: Selective universities in the 1970s–1980s faced institutional pressure to diversify student bodies in response to the civil rights movement and demands for racial justice, but strict colorblind equal protection jurisprudence (interpreting the Constitution as forbidding any state consideration of race) appeared to prohibit race-conscious admissions. The founding problem was the institutional bind: how to pursue diversity as a pedagogically and morally justified institutional goal while remaining constitutionally compliant under equal protection. Universities needed a constitutional reading that permitted race-consciousness without violating the equal protection clause. The diversity reading solves this bind by reinterpreting equal protection to permit race as one factor toward a compelling state interest in educational diversity.
% FOUNDING_PROBLEM_CORROBORATION: Universities and affirmative action proponents attest the founding problem is live: diversity remains an institutional goal, institutional benefits from diversity continue to be documented in research, and the constraint from a strict colorblind reading persists (even after the 2023 Supreme Court shift, universities remain subject to equal protection review). Opponents of affirmative action and colorblind reading adherents attest the founding problem is misframed: they argue the real problem is constitutional error (the diversity reading's permitting of race is itself a violation of equal protection), and the solution is race-neutral admissions without exception. The remedial reading adherents attest the founding problem is incompletely defined: the real problem is structural racism and caste subordination, which the diversity reading does not adequately address. Courts divided on the issue: the Grutter v. Bollinger (2003) line endorsed the diversity reading for 20 years; the Students for Fair Admissions v. Harvard/UNC (2023) Supreme Court decision shifted back toward colorblind jurisprudence. Legislative bodies and civil rights organizations outside the university beneficiary set present diverse framings. No corroborating source outside the university and diversity-advocacy beneficiary set affirms the founding problem as stated by universities; external corroboration is from institutional research on diversity benefits and from civil rights history, not from the constraint's primary beneficiary set.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28) because the constraint is procedural rather than substantive: it permits universities to consider race, but does not command diversity or punish failure to achieve it. The core extraction is the transfer of admissions discretion to universities and the correlated transfer of rejection risk to individual applicants. The measurement series shows slight upward drift from 0.18 to 0.29 over the interval, reflecting increasing litigation intensity and institutional hardening around the constraint's bounds (narrow tailoring scrutiny became more exacting from 1970s through 2000s, then shifted back toward colorblind readings in 2020s). Theater ratio is consistently low (0.08–0.13) because the constraint is genuinely functional: universities do use holistic admissions with race as a factor, and the institutional and legal apparatus really operates this way, not merely performatively. Suppression is moderate (0.25–0.32) because the constraint persists through judicial enforcement and the costs to rejected applicants are not highly visible or collectively organized. The drift toward slightly higher suppression in the middle period reflects the litigation ratchet (more lawsuits, more appellate scrutiny, more institutional defensive positioning), then stabilizes in the recent period as the constraint faced existential challenge from colorblind jurisprudence. The measurement series is authored on one shared time grid to avoid the misalignment problem: every metric is valued at every time point, allowing the engine to detect co-movement and type transitions across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The university agenda-setter seat and the rejected-applicant payer seat should compute as sharply divergent. From the university's position, the constraint is genuine coordination: it licenses their mission-driven pursuit of diversity within constitutional bounds, solving the institutional bind. From the rejected applicant's position (majority-group applicant denied admission they would have secured under race-neutral criteria), the constraint operates as enforced extraction: they bear the cost of the university's diversity goal and have no voice in the decision-making. Courts interpret the constraint as balancing competing equal protection values (individual race-neutral treatment vs. systemic diversity as educational interest); they see it as a boundary-setting rule. Colorblind reading adherents see it as a constitutional error; remedial reading adherents see it as insufficiently attentive to structural racism. The engine computes per-seat type from the structural data: universities get low directionality (beneficiary, institutional power, high exit options → d ≈ 0.1–0.2 → low/negative χ); rejected applicants get moderate-to-high directionality (payer, moderate power, mobile exit → d ≈ 0.5–0.7 → moderate χ); beneficiary racial groups get near-symmetric directionality (dual costs and benefits → d ≈ 0.45–0.55) reflecting the ambiguous position.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are the structural beneficiaries: they gain institutional discretion to use race without violating equal protection and can build diverse student bodies aligned with their mission. Their power is institutional, their time horizon is generational (institutional continuity), their exit options are arbitrage-grade (they can adjust admissions procedures within the bounds the constraint sets or challenge the constraint through litigation). This places them near the beneficiary end of the directionality spectrum (d ≈ 0.15). Applicants excluded holistically are the structural payers: they bear rejection where race factors into the decision. Their power is moderate (some have resources to litigate, most do not; they can appeal individually but not collectively organize against universities), their time horizon is immediate (the admissions cycle), their exit options are mobile (they can apply to other universities or non-selective institutions). This places them near d ≈ 0.6–0.65. Beneficiary racial groups sit ambiguously: they gain enhanced admissions chances (beneficiary side) but carry the stigma cost and identity-fusion burden of being admitted 'for diversity' (payer side). Their directionality is near-symmetric (d ≈ 0.45–0.55). Courts are analytical (d ≈ 0.5 by construction). Colorblind and remedial reading adherents are excluded from the constraint-setting process, so their directionality is trapped at the boundary between powerless and moderate (d ≈ 0.65–0.75, targets of the constraint they reject).
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids the mandatrophy trap by remaining genuinely functional: universities DO use holistic admissions with race as a factor, and the constraint's bounds (narrow tailoring, periodic review, race as one factor not a quota) are meaningfully enforced through litigation and institutional practice. The constraint is not a zombie (mandate divorced from function). However, the constraint faces existential pressure from colorblind jurisprudence: the recent Supreme Court shift (2023, Students for Fair Admissions v. Harvard/UNC) moved back toward a colorblind reading, making the diversity reading's mandate increasingly difficult to operate under (universities must now show that they are using race as a factor only in response to applicant-initiated discourse, not as an active institutional goal). This pressure is not yet mandatrophy (the constraint is still being enforced, still shaping admissions), but it is trajectory toward mandatrophy: if colorblind jurisprudence fully displaces the diversity reading, universities would retain the formal permission to use race only in vestigial forms, and the constraint would persist as institutional theater (appearing race-conscious while actually operating race-neutral). The founding problem status (contested) reflects this: universities attest the founding problem (integrating selective institutions while respecting equal protection) is still live; colorblind adherents attest it is a misconceived problem rooted in constitutional error; remedial adherents attest the real problem (structural racism) is not addressed by the diversity reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_tailoring_boundary_ambiguity,
    'What counts as race being used as ''one factor among many'' versus becoming a determinative factor or de facto quota? Where exactly is the narrow-tailoring boundary?',
    'Appellate litigation testing specific admissions practices: courts review evidence of how race is weighted relative to other factors (grades, test scores, essays, extracurriculars). Cases like Fisher v. University of Texas (2016, 2013) and Students for Fair Admissions v. Harvard/UNC (2023) explicitly adjudicate the boundary.',
    'If the boundary is deemed strictly enforced and narrow (race weighted at <5% relative to other factors), the constraint remains stable and permissive (low extraction). If the boundary is deemed loose or unenforced (race weighted as a primary determinant despite being one of many factors), universities extract more discretion than the reading permits, and the type shifts toward snare. If the boundary is deemed impossible to enforce (it is impossible to say how much race ''really'' mattered in a holistic review), the constraint becomes theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_tailoring_boundary_ambiguity, empirical, 'The precise content of narrow tailoring and the enforceability of the one-factor-among-many requirement.').

omega_variable(
    educational_diversity_as_genuine_compelling_interest,
    'Is educational diversity in student bodies a genuine compelling state interest (strong enough to justify race-conscious measures), or is it a cover story for institutional goals that could be achieved race-neutrally?',
    'Social science research on the causal mechanisms of diversity benefit: does racial diversity in the student body produce educational gains (critical race theory, perspective-taking, reduced stereotype threat) or is diversity pursued as a cultural signal? Compare diversity-driven admissions outcomes to outcomes under alternative criteria (socioeconomic status, geography, holistic review without race weighting).',
    'If diversity produces genuine educational benefits beyond institutional status, the constraint is real coordination (rope). If diversity is a euphemism for institutional prestige-seeking that could be met other ways, the constraint is snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_diversity_as_genuine_compelling_interest, empirical, 'Whether educational diversity is an intrinsic institutional good or a proxy for other institutional interests.').

omega_variable(
    reading_foreclosure_by_colorblind_jurisprudence,
    'Is the diversity reading logically foreclosed by the colorblind reading, or do they coexist as live positions held by different parties and courts?',
    'Meta-constitutional analysis: does the colorblind reading''s core premise (equal protection forbids ANY state consideration of race) logically entail that the diversity reading is false and cannot be held in the same framework? Or are they different interpretations of the same text that can coexist until one is adjudicated as the dominant reading by the Supreme Court?',
    'If the readings logically foreclose each other, the diversity reading will eventually be displaced by whichever reading gains dominance (as the 2023 Students for Fair Admissions decision suggests). If they coexist as live positions, the diversity reading may persist as the reading of lower courts and universities until overruled. This determines the reading_relations classification: forecloses vs. coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_colorblind_jurisprudence, conceptual, 'Whether the colorblind and diversity readings are logically incompatible or can coexist as live constitutional positions.').

omega_variable(
    stigma_cost_internalization_vs_structural,
    'Is the suppression/stigma cost borne by beneficiary racial groups (admitted under diversity considerations) structural (imposed from outside, from majority groups'' skepticism of affirmative action) or internalized (the applicants absorb the constraint''s logic and fuse their identity with ''admitted for diversity'')?',
    'Post-admission trajectory studies: measure whether stigma persists after admission, whether applicants'' self-perception of legitimacy recovers, whether institutional framing (e.g., reframing diversity as benefit to all students) changes the internalization. Compare to similar constraints in other domains (workplace diversity initiatives, scholarship programs) to identify structural vs. internalized suppression patterns.',
    'If stigma is purely structural, universities could mitigate it through framing and institutional messaging. If stigma is internalized, the constraint''s suppressive power travels with the applicant post-admission and is harder to remedy. High internalization increases effective suppression and pushes the constraint toward snare (extraction of psychological cost, not just procedural discretion). Identity-locked exit would apply to beneficiary-group applicants if internalization is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_cost_internalization_vs_structural, empirical, 'The structural vs. internalized nature of suppression for beneficiary racial groups.').

omega_variable(
    sibling_reading_kernel_contest_ontology,
    'Are the three readings (diversity, colorblind, remedial) three different constraints sharing one kernel, or are they three framings of one constraint?',
    'Structural analysis: if ε differs markedly across readings (as predicted: diversity ~0.28, colorblind ~0.05–0.10, remedial ~0.45–0.55), then they are structurally distinct constraints, not framings. If ε is similar and only the narrative differs, they are different reads of one constraint.',
    'If distinct constraints, the corpus should author three separate JSON files linked by network.affects_constraints, and the kernel context merely notes the common textual referent. If one constraint with three reads, the framework needs a different mechanism (currently unsupported). This omega documents the meta-structural ambiguity about how the constraint family is carved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest_ontology, conceptual, 'Whether the three readings are three constraints or three framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_diversity_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(epc_diversity_tr_t7, equal_protection_commitment__diversity_reading, theater_ratio, 7, 0.09).
narrative_ontology:measurement(epc_diversity_tr_t14, equal_protection_commitment__diversity_reading, theater_ratio, 14, 0.1).
narrative_ontology:measurement(epc_diversity_tr_t21, equal_protection_commitment__diversity_reading, theater_ratio, 21, 0.11).
narrative_ontology:measurement(epc_diversity_tr_t28, equal_protection_commitment__diversity_reading, theater_ratio, 28, 0.12).
narrative_ontology:measurement(epc_diversity_tr_t35, equal_protection_commitment__diversity_reading, theater_ratio, 35, 0.13).
narrative_ontology:measurement(epc_diversity_tr_t42, equal_protection_commitment__diversity_reading, theater_ratio, 42, 0.12).
narrative_ontology:measurement(epc_diversity_tr_t50, equal_protection_commitment__diversity_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(epc_diversity_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(epc_diversity_be_t7, equal_protection_commitment__diversity_reading, base_extractiveness, 7, 0.22).
narrative_ontology:measurement(epc_diversity_be_t14, equal_protection_commitment__diversity_reading, base_extractiveness, 14, 0.25).
narrative_ontology:measurement(epc_diversity_be_t21, equal_protection_commitment__diversity_reading, base_extractiveness, 21, 0.27).
narrative_ontology:measurement(epc_diversity_be_t28, equal_protection_commitment__diversity_reading, base_extractiveness, 28, 0.28).
narrative_ontology:measurement(epc_diversity_be_t35, equal_protection_commitment__diversity_reading, base_extractiveness, 35, 0.29).
narrative_ontology:measurement(epc_diversity_be_t42, equal_protection_commitment__diversity_reading, base_extractiveness, 42, 0.29).
narrative_ontology:measurement(epc_diversity_be_t50, equal_protection_commitment__diversity_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(epc_diversity_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(epc_diversity_su_t7, equal_protection_commitment__diversity_reading, suppression_requirement, 7, 0.27).
narrative_ontology:measurement(epc_diversity_su_t14, equal_protection_commitment__diversity_reading, suppression_requirement, 14, 0.29).
narrative_ontology:measurement(epc_diversity_su_t21, equal_protection_commitment__diversity_reading, suppression_requirement, 21, 0.3).
narrative_ontology:measurement(epc_diversity_su_t28, equal_protection_commitment__diversity_reading, suppression_requirement, 28, 0.31).
narrative_ontology:measurement(epc_diversity_su_t35, equal_protection_commitment__diversity_reading, suppression_requirement, 35, 0.32).
narrative_ontology:measurement(epc_diversity_su_t42, equal_protection_commitment__diversity_reading, suppression_requirement, 42, 0.31).
narrative_ontology:measurement(epc_diversity_su_t50, equal_protection_commitment__diversity_reading, suppression_requirement, 50, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three structurally distinct constraint stories: the DIVERSITY READING (this file) interprets equal protection as permitting race-consciousness to achieve educational diversity (low-moderate extraction, procedural rather than substantive, universities gain discretion); the COLORBLIND READING interprets equal protection as forbidding all state use of race (near-zero extraction, boundary-setting principle, strong constraint on state action); the REMEDIAL READING interprets equal protection as requiring race-conscious measures to dismantle caste subordination (high extraction, demands affirmative institutional action, targets mainstream institutions). All three readings reference the same constitutional text but instantiate different ε values, different beneficiary/victim sets, and different genealogies. The diversity reading is neither more nor less legitimate than its siblings; it is a live position in ongoing constitutional contest. The three are linked by network.affects_constraints; sibling relationships are declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
