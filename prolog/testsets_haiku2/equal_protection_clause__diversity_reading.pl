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
 *   human_readable: Equal Protection Diversity Reading: Race-Conscious Educational Policies
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   The diversity reading of the Equal Protection Clause permits
 *   race-conscious admissions policies in higher education when they serve
 *   compelling institutional interests in achieving student body diversity
 *   that benefits all enrolled students, including majority students. Under
 *   this reading, minority students are not admitted to remedy historical
 *   injury but to constitute a diverse peer environment. The reading has been
 *   judicially endorsed (Grutter v. Bollinger, 2003) but narrowed
 *   substantially (Fisher v. University of Texas, 2016) and effectively
 *   foreclosed (Students First v. University of California, 2023). This
 *   constraint story models the diversity reading as a *clean, ε-invariant
 *   constraint* instantiating one coherent but contested interpretation of
 *   equal protection. The reading differs structurally from the colorblind
 *   and remedial readings: it frames beneficiaries as all students (not
 *   historical victims), it treats minority students as instrumental means
 *   (not as rights-bearing claimants), and it anchors legitimacy in
 *   institutional autonomy and forward-looking educational benefits (not in
 *   constitutional colorblindness or historical justice). The measured
 *   extractiveness is moderate (0.38 at interval end) because the reading
 *   succeeds in framing exclusion as instrumental to a broader public good
 *   (educational diversity benefits all), yet resistance remains high (0.71)
 *   because the framing faces credible counterarguments from both colorblind
 *   and remedial advocates.
 *
 * KEY AGENTS:
 *   - All students (enrolled beneficiaries of diverse peer environments)
 *   - Excluded applicants from disfavored groups (direct payers via non-admission)
 *   - Minority applicants (ambiguous: instrumental means and incidental beneficiaries)
 *   - Educational institutions (agenda-setters, administering policies under narrow tailoring constraints)
 *   - Colorblind advocates (excluded from the reading's framework, contesting via litigation)
 *   - Remedial-justice advocates (partially excluded, contesting the framing)
 *   - Judicial authority (adjudicating narrow tailoring and compelling interest tests)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.38).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.22).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading: Race-Conscious Educational Policies").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '5c3a3ac0-0210-4140-9b78-842d2695ecbf').
narrative_ontology:cs_kernel_codification('5c3a3ac0-0210-4140-9b78-842d2695ecbf', fixed_text).
narrative_ontology:cs_authority_grounding('5c3a3ac0-0210-4140-9b78-842d2695ecbf', lineage).
narrative_ontology:cs_interpretation_layer_present('5c3a3ac0-0210-4140-9b78-842d2695ecbf').
narrative_ontology:cs_reading_relation('5c3a3ac0-0210-4140-9b78-842d2695ecbf', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c3a3ac0-0210-4140-9b78-842d2695ecbf', equal_protection_clause__remedial_reading, influences).
narrative_ontology:cs_axiom('5c3a3ac0-0210-4140-9b78-842d2695ecbf', foundational, educational_diversity_is_compelling_institutional_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_institutional_interest, holdable).
narrative_ontology:cs_axiom_grounding('5c3a3ac0-0210-4140-9b78-842d2695ecbf', educational_diversity_is_compelling_institutional_interest, instrumental).
narrative_ontology:cs_axiom('5c3a3ac0-0210-4140-9b78-842d2695ecbf', foundational, all_students_benefit_from_diverse_peer_environment).
narrative_ontology:cs_axiom_status(all_students_benefit_from_diverse_peer_environment, holdable).
narrative_ontology:cs_axiom_grounding('5c3a3ac0-0210-4140-9b78-842d2695ecbf', all_students_benefit_from_diverse_peer_environment, empirically_contingent).
narrative_ontology:cs_reference_frame('5c3a3ac0-0210-4140-9b78-842d2695ecbf', post_civil_rights_institutional_autonomy).
narrative_ontology:cs_drift_state('5c3a3ac0-0210-4140-9b78-842d2695ecbf', contemporary_post_students_first, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5c3a3ac0-0210-4140-9b78-842d2695ecbf', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, excluded_applicants_from_disfavored_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enrolled students benefit from diverse peer environments alleged to improve educational outcomes, critical thinking, and social cohesion. The constraint frames diversity as a public good accruing to all, not a remedial entitlement. Their exit is geographic or through private institutions, both costly and geographically limited.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    moderate, biographical, constrained, national).

% Applicants not selected due to race-conscious admissions policies bear the cost directly: denied admission to preferred schools. This reading treats their exclusion as instrumental to the diversity benefit accruing to enrolled students. Their options include alternative institutions, litigation, or geographic relocation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, excluded_applicants_from_disfavored_groups, payer,
    moderate, biographical, mobile, national).

% Structurally ambiguous under this reading: they are admitted partly to constitute diversity (instrumental means) yet also benefit from diverse environments once enrolled. The reading does not frame their admission as remedial justice but as part of a package where diversity benefits all. They face intra-group stigma risk and stereotype threat as instrumentally selected.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_applicants, payer).

% Administer admissions policies, define institutional diversity interests, and defend policies in litigation. They operate under the constraint that any race-conscious policy must satisfy narrow tailoring tests and demonstrate genuine educational benefits. Their enforcement authority derives from judicial recognition of their institutional autonomy within constitutional limits.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Legal and political actors who argue equal protection forbids all racial classifications. They are formally excluded from the diversity reading's framework (their premise is foreclosed by this reading's core axiom) but remain active litigants and legislators seeking to overturn or narrow this constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    powerful, generational, trapped, national).

% Argue race-conscious policies should rest on remedial grounds (correcting historical subordination) rather than diversity benefits to all students. They view the diversity framing as instrumentalizing minority students and obscuring historical injury. Partially excluded from the diversity reading's legitimacy structure.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, remedial_justice_advocates, excluded,
    organized, generational, constrained, national).

% Interprets and applies the equal protection clause. The diversity reading has been endorsed (Grutter v. Bollinger, 2003) and subsequently narrowed/rejected (Students First v. University of California, 2023). Courts adjudicate whether policies meet narrow tailoring requirements and whether diversity interests remain compelling.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, judicial_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns institutional autonomy in admissions with constitutional anti-discrimination norms by permitting race-conscious policies when justified by genuine educational benefits to all students, not group entitlements. Solves the tension between constitutional neutrality and institutional academic freedom.
% TRANSFER_FUNCTION: Transfers admission slots from applicants outside the preferred diversity profile to applicants within it, justified as producing educational benefits (peer diversity) that flow back to all students. The transfer is sustained by narrowly tailored institutional authority, not by group-based entitlement claims.
% ABSENT_VOICES: Colorblind-principle advocates are formally excluded (their framework contradicts this reading's core axiom); remedial-justice advocates are partially excluded because the reading reframes the legitimacy basis away from historical injury and toward forward-looking institutional benefits. Both groups contest the reading's framing from outside.
% DISAPPEARANCE_RATIONALE: The diversity reading's disappearance would leave admissions neutral on race, raising distinct questions: would educational diversity persist through socioeconomic-proxy admissions (partial world_rearranges via proxy), or would institutional peer composition sharply stratify (world_rearranges)? Remedial advocates argue the constraint already obscures historical injury; colorblind advocates argue its disappearance restores proper equal-protection doctrine. The dispute reflects the reading's contested status.
% FOUNDING_PROBLEM: Segregated educational institutions in the pre-1970s lacked peer diversity; post-civil-rights-era institutions face pressure to serve diverse student bodies and maintain educational quality and social cohesion while adhering to anti-discrimination norms. The diversity reading frames the founding problem as reconciling institutional autonomy and academic excellence with constitutional equality.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers (outside the judiciary and advocacy) attest that diverse peer environments correlate with measured cognitive and social benefits for all students. Colorblind jurists and remedial-justice scholars attest the founding problem is misconceived—either equal protection forbids racial classification entirely, or historical remediation is the legitimate basis, not diversity benefits. The problem is live and contested; the diversity reading's answer remains under active assault.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38) because the diversity reading succeeds in reframing the transfer (admission slots) as producing a collective good (diverse learning environment) benefiting all students, not as redistribution to a preferred group. The reading does not deny that excluded applicants bear costs—it asserts those costs are justified by benefits that accrue more broadly. The measurement series shows slight extractiveness growth from 1978 (0.28) to 2024 (0.38), reflecting increased judicial scrutiny and litigation intensity over whether policies genuinely serve narrow-tailored educational interests or constitute proxy discrimination. Theater ratio declined from 0.25 (1978, when the diversity rationale was nascent) to 0.16 (2003, when Grutter endorsed it) then rose slightly (0.19 in 2023), reflecting increased tension between stated diversity goals and achieved diversity outcomes as political opposition intensified. Suppression requirement increased from 0.15 (1978) to 0.22–0.24 (2013–2023) because the constraint's persistence increasingly depends on defending policies against colorblind and remedial-justice litigation rather than on voluntary institutional adoption. The measurements use one shared time grid aligned at historical checkpoints (Bakke 1978, Grutter 2003, Fisher 2016, Students First 2023).
 *
 * DIRECTIONALITY LOGIC:
 *   From the institutional/all-students seat: the constraint is coordinate—institutions autonomously pursue diversity interests; students benefit; the framework is legitimate. From the excluded-applicant seat: the constraint extracts—the diversity benefit to others is purchased by denying them admission, and they bear concentrated costs while benefits are diffuse. From the minority-applicant seat: ambiguous—they benefit from diversity but also face instrumental selection and stereotype threat; they are simultaneously means and beneficiaries. The engine computes these seat-specific directionalities from the structural data: institutions and all-students approach d=0.0 (beneficiary end); excluded applicants approach d=1.0 (target end); minority applicants sit near d=0.5 (symmetric) with possible override toward d=0.6–0.7 if stereotype-threat and instrumental-selection costs are foregrounded. This reading's pervasive asymmetry—everyone benefits from diversity but only disfavored applicants pay—is precisely the tangled_rope structure: genuine coordination function (diverse learning environment is a public good to enrolled students) combined with asymmetric transfer (admission slots extracted from some to constitute that good for others).
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids one mandatrophy trap (Grutter's claim that race-conscious admissions will sunset as diversity progresses) but confronts another: the reading's founding problem (aligning institutional autonomy with anti-discrimination norms post-civil rights) remains LIVE and CONTESTED in the legal and political system. Courts have not accepted the sunset premise; instead, they have narrowed the reading by tightening narrow-tailoring tests. The reading persists not because the founding problem is solved but because institutions and enrolled students continue to benefit from the diversity frame. Colorblind and remedial advocates have actively resisted rather than accepting the reading's legitimacy. The measurement trajectory shows extractiveness-creeping-up and suppression-increasing, which under mandatrophy rubrics flags a constraint whose coordination function is being eroded by litigation pressure—a sign of incipient piton transition. The diversity reading is NOT yet a piton (the institutional benefit is real, enforcement is active rather than theatrical), but the measurement trend suggests the reading's authority is decaying under sustained doctrinal assault.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_as_instrumental_vs_end,
    'Does the diversity reading treat minority students as ends-in-themselves (intrinsically worthy of inclusion) or purely as instrumental means to constitute diversity for majority-student benefit?',
    'Discourse analysis of institutional diversity statements and legal briefs; interview data from admitted minority students about how they experience their selection; empirical measurement of stereotype threat and intra-group stigma under explicit instrumental framing.',
    'If purely instrumental: the reading''s framing becomes extractive on minority applicants even while being coordinate for majority students (tangled_rope structure confirmed). If ends-in-themselves: the reading partially collapses into remedial logic, reducing the structural clarity between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_as_instrumental_vs_end, empirical, 'Whether minority students are treated as ends or means under the diversity reading''s logic').

omega_variable(
    diversity_benefit_distribution,
    'Are the purported educational benefits of diversity actually distributed to all students, or do they accrue primarily to majority students while minority students face offsetting costs (stereotype threat, tokenization)?',
    'Randomized controlled trials or natural experiments measuring learning outcomes, social integration, and psychological wellbeing by race/ethnicity for students in high-diversity vs. low-diversity cohorts, controlling for peer composition and institutional culture.',
    'If benefits accrue to all equally: the constraint''s tangled_rope classification is robust (genuine coordination function with asymmetric transfer). If benefits accrue primarily to majority students and minority students face net costs: the constraint approaches snare classification (coordinated cover story for pure extraction from minority applicants and students).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefit_distribution, empirical, 'Whether educational diversity benefits distribute equitably across racial/ethnic groups').

omega_variable(
    narrow_tailoring_efficacy,
    'Do narrow-tailoring judicial tests actually enforce genuine educational diversity interests, or have they become theater (policies nominally undergo review but survive via formulaic institutional testimony)?',
    'Comparative analysis of trial records and judicial opinions: how many race-conscious admissions policies have been struck down vs. upheld, and what pattern of invalidation exists? Has the narrow-tailoring test tightened over time (indicating genuine enforcement) or remained stable (indicating theater)?',
    'If narrow-tailoring is effective: suppression is genuinely moderate (enforcement overhead is real). If narrow-tailoring is theatrical: suppression rises and theater_ratio rises; the constraint approaches piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_tailoring_efficacy, empirical, 'Whether judicial narrow-tailoring review actually constrains institutional admissions policies or functions as rubber-stamp endorsement').

omega_variable(
    colorblind_vs_diversity_foreclosure,
    'Does the diversity reading''s core axiom (educational diversity is a compelling institutional interest) logically foreclose the colorblind reading''s core axiom (equal protection forbids racial classification), or do the readings coexist as live positions?',
    'Doctrinal analysis: can a single constitutional framework hold both axioms (one allowing race-conscious policies, one forbidding all such policies)? Or must one necessarily reject the other''s core premise?',
    'If foreclosure: reading_relations includes ''forecloses'' (rare; indicates strong logical contradiction). If coexistence: reading_relations includes ''coexists_with'' (typical; the readings are held by different judicial coalitions and lack logical resolution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_diversity_foreclosure, conceptual, 'Whether the diversity reading''s core principle logically forecloses the colorblind reading').

omega_variable(
    remedial_vs_diversity_tension,
    'Does the diversity reading''s reframing away from remedial justice toward all-student benefit constitute a genuine alternative framing (influences relation) or a foreclosure of remedial claims?',
    'Jurisprudential analysis: does the diversity reading explicitly reject remedial justice as a basis (foreclosure), or does it simply prioritize a different justification while leaving remedial approaches available to other institutional actors or policy domains (influences)?',
    'If foreclosure: the readings would need to show a direct logical contradiction (hard to establish). If influences: the diversity reading creates structural pressure on remedial approaches by offering a competing legitimacy basis, but doesn''t rule them out entirely (courts can still recognize remedial grounds in other contexts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_diversity_tension, conceptual, 'Whether the diversity reading forecloses or merely competes with the remedial reading''s legitimacy basis').

omega_variable(
    institutional_autonomy_as_fig_leaf,
    'Is institutional autonomy in admissions a genuine constraint-limiting principle (institutions do have significant discretion within narrow tailoring), or does it function as a fig leaf concealing judicial deference to institutional preferences?',
    'Comparative institutional analysis: what fraction of institutional admissions policies survive judicial challenge? Do courts actually modify or strike down policies, or do they defer to institutional judgment under the guise of narrow tailoring?',
    'If genuine constraint: suppression is moderate because institutions operate within real limits. If fig leaf: suppression is lower than authored (deference is easier than active enforcement; constraints are more permissive than they appear).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_autonomy_as_fig_leaf, empirical, 'Whether institutional autonomy in admissions represents a real constraint or judicial deference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__diversity_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.16).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.17).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_clause__diversity_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__diversity_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.37).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement(equa_be_t2024, equal_protection_clause__diversity_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__diversity_reading, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.2).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.22).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.24).
narrative_ontology:measurement(equa_su_t2024, equal_protection_clause__diversity_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel decomposes into three structurally distinct constraint stories: colorblind_reading (mountain: constitutional principle, negligible extraction), diversity_reading (tangled_rope: genuine educational coordination with asymmetric transfer), and remedial_reading (snare or tangled_rope: historical remediation with contested extraction logic). Each reading has a different ε, beneficiary/victim structure, and type. The readings coexist as live positions held by different judicial coalitions; they neither foreclose each other logically nor resolve via evidence. See equal_protection_clause__colorblind_reading and equal_protection_clause__remedial_reading for sibling stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
