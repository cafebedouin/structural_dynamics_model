% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Constitutional Color-Blindness: Classification as Prohibited Harm
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The color-blind reading of equal protection holds that the Constitution
 *   prohibits any state use of racial classification, regardless of intent or
 *   remedial purpose. This reading originates in Justice Harlan's dissent in
 *   Plessy v. Ferguson (1896) and became dominant Supreme Court doctrine in
 *   the late 20th century (Regents v. Bakke, 1978; Parents Involved v.
 *   Seattle, 2007; Students for Fair Admissions v. Harvard, 2023). Under this
 *   reading, applicants from groups favored by affirmative action and members
 *   of historically subordinated groups become victims of state-sponsored
 *   racial harm; institutions that administer race-conscious policies become
 *   agenda-setters enforcing the constraint; and applicants not selected by
 *   racial preference benefit structurally. The constraint is claimed as
 *   Tangled Rope—it coordinates a uniform principle (color-blindness) while
 *   asymmetrically extracting opportunity from one group of applicants. The
 *   measurement series track the evolution of the reading's institutional
 *   enforcement intensity over three decades.
 *
 * KEY AGENTS:
 *   - applicants_from_preferred_racial_groups: Victims of the color-blind reading; lose opportunities if institutions use race-conscious remedies, moderate power, constrained exit (cannot apply outside US jurisdiction without forgoing opportunities)
 *   - applicants_from_non_racial_preference_groups: Structural beneficiaries; gain admissions/hiring slots if institutions adopt race-blind criteria, moderate power, constrained exit (facing same constraint across jurisdictions)
 *   - educational_institutions and employers: Agenda-setters; enforce the reading by adopting facially neutral criteria and are liable for violating it, institutional power, constrained exit (must obey constitutional doctrine)
 *   - federal courts: Primary institutional enforcer; interpret and apply the color-blind reading through doctrine, institutional power, analytical position
 *   - civil rights advocates (remedial reading) and diversity advocates: Excluded from applying this reading; would argue it ignores structural racism or forecloses legitimate diversity goals; organized power, constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Constitutional Color-Blindness: Classification as Prohibited Harm").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'a2b6e7eb-e9ba-44ef-a789-a21b99d93067').
narrative_ontology:cs_kernel_codification('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', fixed_text).
narrative_ontology:cs_authority_grounding('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', lineage).
narrative_ontology:cs_interpretation_layer_present('a2b6e7eb-e9ba-44ef-a789-a21b99d93067').
narrative_ontology:cs_reading_relation('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', foundational, constitution_is_color_blind).
narrative_ontology:cs_axiom_status(constitution_is_color_blind, holdable).
narrative_ontology:cs_axiom_grounding('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', constitution_is_color_blind, deontological).
narrative_ontology:cs_axiom('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', foundational, racial_classification_violates_individual_dignity).
narrative_ontology:cs_axiom_status(racial_classification_violates_individual_dignity, holdable).
narrative_ontology:cs_axiom_grounding('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', racial_classification_violates_individual_dignity, deontological).
narrative_ontology:cs_reference_frame('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', racial_classification_constitutionally_forbidden).
narrative_ontology:cs_drift_state('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', contemporary_political_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a2b6e7eb-e9ba-44ef-a789-a21b99d93067', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, applicants_from_non_racial_preference_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, applicants_from_preferred_racial_groups).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, constitutional_individualism_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, formal_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reading that treats any racial classification (including affirmative action) as constitutionally prohibited. When applying to schools, employers, or public programs, they argue their exclusion is based on merit, not on racial preference granted to others. Their legal position is strengthened when institutions cannot use race as a factor; exit would mean applying to institutions outside the jurisdiction, which is possible but costly.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, applicants_from_non_racial_preference_groups, beneficiary,
    moderate, biographical, constrained, national).

% Pay the cost of the color-blind reading: rejected applications despite qualifications, exclusion from opportunities they would otherwise access. Under this reading, any consideration of their race in admissions or hiring is per se harm, not remedy. Their exit options are limited—they face the same constraint across all jurisdictions that adopt the color-blind reading, and legal challenge requires overturning the established interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, applicants_from_preferred_racial_groups, payer,
    moderate, biographical, constrained, national).

% Enforce and administer the color-blind reading by adopting facially race-neutral admissions policies. They must defend against challenges that their ostensibly neutral criteria perpetuate racial subordination. Under the color-blind reading, they are prohibited from considering race even to remedy past discrimination or achieve student-body diversity. Institutions that attempt race-conscious remedies face legal liability and override by courts applying this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Enforce the color-blind reading by structuring hiring and contracting on formally neutral criteria. They absorb the legal cost of defending against disparate-impact claims and the operational cost of designing systems that appear race-blind while managing demographic outcomes. They cannot use race-conscious remedies even where workforce composition dramatically diverges from applicant pool diversity.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, employers_and_public_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Enforce the color-blind reading through constitutional doctrine and statutory construction. They issue opinions that treat race-conscious state action as inherently suspect and subject to strict scrutiny. This reading's institutional carrier is the judiciary; courts act as the primary enforcement mechanism, striking down policies that use racial classification even for remedial purposes.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, federal_courts_interpreting_civil_rights, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that equal protection forbids the perpetuation of caste-based subordination and permits race-conscious measures to dismantle it. They are not in the room when the color-blind reading is applied; their objection—that the reading ignores structural racism and forecloses remedies—is theoretically available but practically excluded from the decisions that apply this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, civil_rights_advocates_remedial_reading, excluded,
    organized, generational, constrained, national).

% Would argue that equal protection permits race as one factor in achieving compelling state interests like educational diversity. They occupy a different institutional position (academia, some courts) and their reading gains traction in some jurisdictions; they are excluded from the decisions that apply the color-blind reading and must advocate for a different constitutional understanding.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, constitutional_theorists_diversity_reading, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, applicants_from_non_racial_preference_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform principle for evaluating state use of race: any classification by race triggers heightened constitutional scrutiny and is presumptively forbidden. This answers the coordination question: what standard governs whether a state may classify by race? The color-blind reading supplies one answer—no, the Constitution forbids it—as opposed to permitting race-conscious remedies (remedial reading) or race as one factor among many (diversity reading).
% TRANSFER_FUNCTION: Moves opportunity and institutional access from applicants and communities whose race was the target of state-sponsored preference policies to those not selected by such policies. The constraint redirects admissions and hiring decisions from criterion-sets that include race to criterion-sets that exclude it; applicants disadvantaged by their race under the former lose access they would have gained.
% ABSENT_VOICES: Civil rights advocates holding the remedial reading are excluded from the decisions that instantiate the color-blind reading. They would argue that equal protection forbids perpetuating caste-based subordination and permits race-conscious remedies to dismantle it. Theorists holding the diversity reading are also excluded; they would argue equal protection permits race as one factor in achieving compelling state interests. Both absent positions are structurally opposite to the color-blind reading and could reshape the constraint's boundaries if granted standing.
% DISAPPEARANCE_RATIONALE: If the color-blind reading disappeared—replaced by the remedial reading or a pluralist reading permitting race-conscious remedies—institutions would shift back to race-conscious admissions and hiring in many domains. Applicant outcomes would change dramatically: admissions profiles would reorder to reflect demographic representation and remedial goals rather than ostensibly race-neutral metrics. Legal obligations on employers and schools would transform. The constitutional constraint is not natural law; its disappearance would reorganize institutional decision-making.
% FOUNDING_PROBLEM: Harlan's dissent (1896) identified the founding problem as the risk that constitutional color-blindness would be honored in principle but violated in practice—that the state would use formal equality to legitimize substantive inequality ('the real substance and meaning of the provision may be phased away by construction'). The later color-blind reading (1990s onward in Supreme Court doctrine) reframed the founding problem as the risk that the state would use racial classification itself to perpetuate harmful stereotypes and violate individual dignity, even when nominally remedial.
% FOUNDING_PROBLEM_CORROBORATION: The color-blind reading itself (via Supreme Court majority opinions: Students for Fair Admissions v. Harvard, 2023; Parents Involved in Community Schools v. Seattle, 2007; Regents v. Bakke, 1978) attests that the founding problem is live: states continue to classify by race in ways that the reading treats as inherently harmful. Civil rights organizations and legal scholars outside the beneficiary set (remedial reading advocates) attest that Harlan's original founding problem—that formal color-blindness masks and permits structural racism—persists and is NOT solved by the color-blind reading itself. Empirical research on educational outcomes and disparities is cited by both sides as evidence for their reading of the founding problem.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.42 at interval end, rising from 0.35) reflects that the constraint transfers opportunity based on state classification, independent of merit or need—the classification itself is treated as the harm. Suppression (0.48) tracks the institutional enforcement required to maintain the reading against competing interpretations; it is not as high as a pure snare because the constraint appeals to formal equality principles and is defended through legitimate legal argument, not solely through coercion. Theater ratio (0.29, rising to 0.30) reflects that institutions must perform compliance (advertising race-blind criteria, designing facially neutral systems) while the actual sorting continues to produce demographic outcomes—some portion of activity is theatrical adherence to the principle rather than substantive change. Accessibility collapse (0.61) reflects that applicants cannot escape the constraint by seeking alternative educational or employment channels within the US; once the color-blind reading applies nationally, alternatives are unavailable. Resistance (0.72) is substantial because the reading is actively contested: remedial reading advocates and diversity theorists mount continuous legal and scholarly challenges, and some communities organize political opposition. The measurements show extractiveness and suppression rising slightly from 2000–2015 (Courts solidifying the doctrine) and plateauing thereafter (doctrine entrenched), with theater ratio rising as institutions invest in ever-more-elaborate race-neutral framing.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a beneficiary (applicants in non-preferred groups), the color-blind reading is a protection against state-sponsored discrimination—equal treatment under law. From the perspective of a payer (applicants whose race is a target of state preference), the same reading is a mechanism that extracts opportunity through the prohibition on remedying past discrimination. The agenda-setter (institution) sits between: it benefits from the clarity the reading provides (one clear rule) but pays the cost of continuous legal compliance and manages the reality that ostensibly race-neutral criteria still produce demographic sorting. The engine computes these different d values (beneficiary d near 0.2, payer d near 0.8, agenda-setter d near 0.5) from the structural data—the reading itself does not adjudicate which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Applicants from preferred groups (victims) face a high directionality (d near 0.75–0.85) because the constraint systematically extracts opportunity from them; their exclusion is the constraint's primary functional purpose. Applicants from non-preferred groups (beneficiaries) face low directionality (d near 0.15–0.25) because the constraint subsidizes their access. Institutional actors (agenda-setters) face moderate directionality (d near 0.45–0.55) because they gain clarity and legal compliance by applying the rule, but pay the cost of managing the political and moral challenge of doing so. Courts face analytical directionality (d = 0.5) because their role is to interpret the constraint, not to collect from it. No overrides are needed; the structural beneficiary/victim declarations and exit constraints generate the correct d values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The color-blind reading avoids mandatrophy—the original coordination problem (establishing a uniform principle for race-based classifications) remains live. The founding problem (preventing formal equality from masking structural inequality) is contested, not solved, but the reading does not claim to solve it; the reading's own proponents argue the mandate (no racial classification) persists as necessary. A mandatrophy signal would appear if the founding problem disappeared (i.e., racial inequality was eliminated or no longer relevant) but institutions continued enforcing color-blindness by theatrical compliance—that is not the case here. Resistance is substantial because the remedial reading and diversity reading directly contest the mandate itself, not merely its application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_status,
    'Is the color-blind reading a natural principle of justice (as the reading claims—''neither knows nor tolerates classes''), or is it a constructed constraint that benefits some applicants while harming others?',
    'Comparative constitutional analysis: do other constitutional systems treating equal protection identically to the US reach the same color-blind reading? Does the reading''s empirical outcomes match what a ''natural'' principle would produce, or do they align with the interests of specific constituencies?',
    'If the reading is genuinely natural, it should be independent of who benefits; if constructed, it should be reclassifiable as a snare or piton masquerading as a mountain. High correlation with constituency interests would indicate construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_status, conceptual, 'Whether color-blindness is intrinsic to equal protection or a constructed reading that serves identifiable beneficiaries.').

omega_variable(
    remedy_vs_harm_interpretation,
    'Does racial classification in admissions/hiring constitute harm to individual applicants (the color-blind reading''s core claim), or does it constitute a remedy for prior harm to groups that experienced racial subordination?',
    'Empirical: long-term educational outcomes, earning trajectories, and sense of belonging for applicants admitted under race-conscious versus race-blind regimes. Normative: whether constitutional harm is measured at individual classification or group subordination levels.',
    'If race-conscious admissions produces better individual and group outcomes and addresses prior harm without new individual harm, the reading''s ε would be revised upward (more extractive). If it produces worse outcomes or harms without remedying subordination, the reading''s ε would be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_vs_harm_interpretation, empirical, 'Whether racial classification in remedial policies constitutes individual harm or group remedy.').

omega_variable(
    reading_kernel_boundary_contestation,
    'Is the color-blind reading the authoritative interpretation of the Fourteenth Amendment''s equal protection clause, or is it one contestable reading among others with equal constitutional standing?',
    'Constitutional history: textual analysis of the Fourteenth Amendment''s language and original public meaning; institutional analysis of how many major constitutional democracies read equal protection differently; political-alignment analysis showing whether the reading''s adoption correlates with shifts in institutional power rather than interpretive breakthrough.',
    'If the reading''s authority is contingent rather than foundational, the constraint should be reclassified to reflect that interpretive contestation; if foundational, it should remain as is. The committer axis routes this question through the cs_structure; the outcome determines whether the reading''s grounding is legitimately ''lineage'' or more accurately ''extraction'' (institutional actors enforcing the reading to prevent alternative readings from gaining traction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary_contestation, conceptual, 'Whether the color-blind reading is the definitive or one contested interpretation of equal protection.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.48) the result of external legal enforcement (courts striking down programs, liability for violation), or is it internalized—institutional actors believing color-blindness is the right principle and voluntarily complying?',
    'Institutional behavior analysis: would institutions adopt race-neutral policies in the absence of legal liability? Do they minimize compliance or exceed it? Do leaders express genuine commitment to the reading or reluctant obedience?',
    'If suppression is purely external, the constraint could dissolve if enforcement weakened. If internalized, the constraint persists because institutions have adopted the reading''s legitimacy frame; exit from the reading would require ideological reframing of equality itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether institutional compliance with the color-blind reading is coerced or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t5, equal_protection_commitment__colorblind_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(equa_tr_t5, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t15, equal_protection_commitment__colorblind_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(equa_tr_t15, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__colorblind_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__colorblind_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(equa_tr_t25, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(equa_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t5, equal_protection_commitment__colorblind_reading, base_extractiveness, 5, 0.37).
narrative_ontology:measurement_basis(equa_be_t5, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t15, equal_protection_commitment__colorblind_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(equa_be_t15, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__colorblind_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__colorblind_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(equa_be_t25, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(equa_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t5, equal_protection_commitment__colorblind_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(equa_su_t5, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t15, equal_protection_commitment__colorblind_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(equa_su_t15, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__colorblind_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__colorblind_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(equa_su_t25, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(equa_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel is instantiated by three distinct constraint stories: colorblind_reading (no racial classification permitted), remedial_reading (race-conscious remedies permitted to dismantle subordination), and diversity_reading (race as one factor permitted for compelling diversity interests). Each reading has its own ε value, victim/beneficiary structure, and enforcement mechanism. They are linked as a constraint family because they compete to define the same constitutional text. Decomposition is necessary because ε differs substantially: the color-blind reading treats any classification as harm (moderate ε, 0.42); the remedial reading treats perpetuation of subordination as harm (potentially lower ε if remedies work); the diversity reading treats exclusion from diversity benefits as harm (different victim set). The network edges enable contamination analysis: if one reading's purity degrades, competing readings gain institutional traction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
