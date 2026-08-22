% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Equal Protection — Diversity Rationale for Race-Conscious Admissions
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This story instantiates the DIVERSITY reading of the Equal Protection
 *   Clause's treatment of race-conscious admissions — the line running from
 *   Justice Powell's Bakke opinion through Grutter's 'critical mass'
 *   framework to its ultimate rejection in Students for Fair Admissions v.
 *   Harvard/UNC (2023). Under this reading, race-conscious admissions are
 *   constitutionally permissible ONLY when justified by the educational
 *   benefits diversity confers on the whole student body — not as remediation
 *   for past discrimination (the remedial reading) and not forbidden outright
 *   (the colorblind reading). This framing structurally centers non-minority
 *   students as the doctrine's beneficiary class (they get the pedagogical
 *   benefit) while minority students function as the instrumental means by
 *   which that benefit is produced — a structural asymmetry the doctrine
 *   itself has been repeatedly criticized for, including by some of its own
 *   defenders. This is a single, specific claim among the kernel's readings;
 *   the remedial and colorblind readings are separate constraints with their
 *   own ε, stakeholders, and classification, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda_setter/beneficiary (institutional/arbitrage) — designs and administers the admissions rationale, captures reputational and mission legitimacy
 *   - white_and_nonminority_students: beneficiary (moderate/constrained) — doctrine's named primary beneficiary of the diversity rationale
 *   - high_achieving_asian_american_applicants: payer (moderate/constrained) — bears the clearest measurable statistical cost
 *   - displaced_qualified_nonpreferred_applicants: payer (powerless/trapped) — diffuse, unidentifiable, cannot organize
 *   - underrepresented_minority_students: beneficiary/payer (moderate/identity_locked) — admitted but doctrinally instrumentalized and stigmatized
 *   - supreme_court: agenda_setter (institutional/analytical) — sets and narrows the doctrinal frame across decades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.42).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.38).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection — Diversity Rationale for Race-Conscious Admissions").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'e6f595aa-7233-4485-bfda-c6be25cd1ced').
narrative_ontology:cs_kernel_codification('e6f595aa-7233-4485-bfda-c6be25cd1ced', fixed_text).
narrative_ontology:cs_authority_grounding('e6f595aa-7233-4485-bfda-c6be25cd1ced', lineage).
narrative_ontology:cs_interpretation_layer_present('e6f595aa-7233-4485-bfda-c6be25cd1ced').
narrative_ontology:cs_reading_relation('e6f595aa-7233-4485-bfda-c6be25cd1ced', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6f595aa-7233-4485-bfda-c6be25cd1ced', equal_protection_clause__colorblind_reading, influences).
narrative_ontology:cs_axiom('e6f595aa-7233-4485-bfda-c6be25cd1ced', foundational, diversity_is_compelling_educational_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_educational_interest, holdable).
narrative_ontology:cs_axiom_grounding('e6f595aa-7233-4485-bfda-c6be25cd1ced', diversity_is_compelling_educational_interest, instrumental).
narrative_ontology:cs_axiom('e6f595aa-7233-4485-bfda-c6be25cd1ced', foundational, benefit_to_all_students_not_remediation_of_harm_justifies_race_consciousness).
narrative_ontology:cs_axiom_status(benefit_to_all_students_not_remediation_of_harm_justifies_race_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('e6f595aa-7233-4485-bfda-c6be25cd1ced', benefit_to_all_students_not_remediation_of_harm_justifies_race_consciousness, conventional).
narrative_ontology:cs_reference_frame('e6f595aa-7233-4485-bfda-c6be25cd1ced', grutter_critical_mass_framework).
narrative_ontology:cs_drift_state('e6f595aa-7233-4485-bfda-c6be25cd1ced', post_sffa_harvard_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e6f595aa-7233-4485-bfda-c6be25cd1ced', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, white_and_nonminority_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, diversity_credentialed_graduates).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, displaced_qualified_nonpreferred_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, underrepresented_minority_students).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, underrepresented_minority_students).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, critical_mass_educational_benefit_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, narrow_tailoring_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer holistic admissions processes that weigh race as one factor among many, justified by the claim that a diverse student body produces pedagogical benefits for everyone. Retains near-total discretion over how 'critical mass' and 'holistic review' are defined and measured, and captures the reputational and mission-legitimacy benefits of being seen as diverse and inclusive institutions.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, selective_universities, beneficiary).

% Attend the institution and are told, and largely accept, that exposure to a racially diverse cohort improves their own classroom experience, cross-cultural competence, and preparation for a diverse workforce. They bear essentially none of the admissions cost of the policy and are the doctrine's named primary beneficiary class under the diversity rationale.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, white_and_nonminority_students, beneficiary,
    moderate, biographical, constrained, national).

% Face admissions personal-rating and holistic-review practices that, according to plaintiffs and internal institutional data introduced in litigation, systematically score this group lower on subjective traits, offsetting otherwise superior academic and test-score profiles. Their statistically lower admit rates at equivalent qualification levels are the clearest measurable transfer the constraint produces; their only exit is applying elsewhere or foregoing selective institutions entirely.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Individual applicants of any background who are academically qualified but not admitted to a seat that, absent the race-conscious weighting, would statistically have gone to them. They are diffuse, unidentifiable ex ante, cannot organize, and have no standing or visibility into why any specific decision went against them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, displaced_qualified_nonpreferred_applicants, payer,
    powerless, biographical, trapped, national).

% Admitted in part through race-conscious review and gain access to selective institutions and their downstream opportunities. Under the diversity rationale, however, their doctrinal role is instrumental — their presence is valued for the educational benefit it confers on classmates rather than as remediation owed to them directly — which produces stigma costs (assumptions of lesser qualification) and ties their admission's legal survival to a rationale about others' experience rather than their own claims to redress.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, underrepresented_minority_students, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, underrepresented_minority_students, payer).

% Announced and then substantially narrowed the diversity rationale across Bakke, Grutter, Fisher, and Students for Fair Admissions v. Harvard/UNC, setting and re-setting the narrow-tailoring requirements that determine whether particular admissions practices survive strict scrutiny. Controls the doctrinal frame entirely and is not itself a party to any admissions transaction.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations (on both the remedial and colorblind sides) that argue the diversity rationale is a doctrinal workaround avoiding the harder constitutional question of remedying historical subordination or, alternatively, avoiding the simpler rule of pure colorblindness. Their preferred framings are litigated but not adopted as the operative rule; they remain outside the diversity rationale's own logic even as they shape the case law around it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_litigants, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows selective institutions to assemble a student body whose racial composition is treated as pedagogically valuable — enabling cross-racial interaction, perspective diversity in classroom discussion, and preparation for a heterogeneous workforce and civic life — without requiring proof of any individual applicant's or institution's own discriminatory history.
% TRANSFER_FUNCTION: Moves admission seats, and the downstream credentialing and opportunity attached to them, from higher-scoring applicants in disfavored groups (measured most starkly among Asian American applicants) toward applicants whose group membership is treated as contributing to diversity, while distributing the claimed pedagogical benefit to the entire enrolled student body, especially the racial majority.
% ABSENT_VOICES: Individual displaced applicants never appear as identified parties — no one can show which specific denial was caused by the policy versus ordinary competition, so their objection has no forum. Historically subordinated groups seeking remediation as an entitlement in its own right are also structurally absent from this rationale's own logic, since the diversity reading affirmatively declines to ground the policy in remedying past discrimination.
% DISAPPEARANCE_RATIONALE: Universities dispute this sharply: institutions and diversity's defenders argue that without race-conscious admissions, underrepresented minority enrollment at the most selective schools would drop substantially (as observed post-SFFA and in states that banned the practice via ballot initiative), meaningfully rearranging both classroom composition and downstream elite-institution demographics. Colorblind-reading proponents argue enrollment would merely reallocate seats among qualified applicants without changing any underlying social arrangement, so the world is essentially unchanged except for who specifically sits in which seat.
% FOUNDING_PROBLEM: Selective universities wanted a constitutionally durable justification for considering race in admissions after Bakke rejected quota-based remediation and strict racial balancing as compelling state interests, needing a rationale that survived strict scrutiny without requiring proof of the university's own past discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Universities and their amici (educational associations, some employers) attest the pedagogical-diversity problem remains live and empirically supported by social-science literature on cross-racial contact. Independent of the benefiting institutions, the Supreme Court majority in SFFA v. Harvard (2023) itself concluded the rationale's goals were not measurable or judicially administrable and that the practices as implemented had drifted from the stated coordination purpose into disguised racial balancing — an assessment from the adjudicating authority, not a beneficiary.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42 by 2023) rather than severe because narrow tailoring requirements genuinely constrain how race can be used — it must be one factor among many, subject to periodic review, and cannot function as a quota. This keeps ε well below what a remedial reading with explicit group entitlements or a colorblind-violating quota system would register. Suppression (0.38) and theater ratio (0.30) both rose modestly across the measured interval as 'holistic review' language increasingly served to shield practices (personal ratings, opaque weighting) from scrutiny — the internal Harvard admissions data introduced in SFFA litigation is the clearest evidence the theater component grew over time relative to the stated pedagogical-benefit rationale. Accessibility collapse (0.35) and resistance (0.55) reflect that alternatives to race-conscious review (socioeconomic-only approaches, top-percent plans) were live and litigated throughout, and organized resistance from multiple directions (colorblind advocates, remedial advocates, and displaced-applicant plaintiffs) was substantial and ultimately successful in SFFA.
 *
 * PERSPECTIVAL GAP:
 *   From the university's seat, this is a Rope: a coordination mechanism producing genuine, evidence-supported pedagogical value for an entire cohort, held within tight constitutional bounds. From the excluded/displaced applicant's seat, the same structure is a Tangled Rope shading toward Snare: a coordination story (diversity benefits everyone) providing cover for admissions decisions that systematically transfer opportunity away from specific disfavored groups. The engine computing both seats' types from the same structural data is the point — the doctrine's own instability (three major re-litigations in 45 years, ending in reversal) is consistent with a structure whose coordination and extraction functions never fully separated.
 *
 * DIRECTIONALITY LOGIC:
 *   White and nonminority students are declared primary beneficiaries under this specific reading's own logic (the diversity rationale is framed AS a benefit to them, not to minority students) — this yields low d for that group despite them not being direct parties to any admissions decision. High-achieving Asian American applicants and displaced qualified nonpreferred applicants are the measurable targets — d is pushed toward the full-target end by their constrained/trapped exit options and by the statistical admit-rate evidence. Underrepresented minority students occupy a genuinely mixed position: real access benefit, but real instrumentalization and stigma cost, captured via the secondary_role and the identity_locked exit option (their admission is doctrinally tied to a rationale about others).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested, not resolved, because the doctrine's core empirical premise (measurable, judicially administrable educational benefits from critical-mass diversity) was directly challenged by the adjudicating authority itself in SFFA — the Supreme Court majority found the goals not sufficiently measurable to survive strict scrutiny, an assessment from outside the beneficiary set. This is the R5 corroboration this story requires: not university self-reports, but the institution empowered to end the arrangement concluding its own coordination rationale had become undoable to verify. That is exactly the mismatch (status=contested/dead-leaning, disappearance_verdict=contested) the framework's mandatrophy detection is built to surface, distinguishing a genuine but time-limited coordination function from a rationale that outlived its administrability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_survives_as_live_doctrine,
    'After SFFA v. Harvard (2023) rejected race-based admissions as practiced, does the diversity rationale survive as a live constitutional theory in any remaining domain (e.g., military academies, K-12, employment), or has it been effectively foreclosed as a workable basis for race-conscious action?',
    'Track subsequent litigation and lower-court application of SFFA''s reasoning outside undergraduate admissions; observe whether any institution successfully defends a race-conscious policy on pure educational-diversity grounds post-2023.',
    'If the rationale is functionally dead outside narrow carve-outs, this constraint should be understood as a historical constraint (1978-2023) rather than an ongoing one, and ''permanent constraint'' framing in the expected structural delta would need revision to ''formerly permanent, now foreclosed.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_survives_as_live_doctrine, empirical, 'Whether the diversity rationale has continuing doctrinal life after SFFA v. Harvard.').

omega_variable(
    instrumentalization_versus_genuine_benefit,
    'Is the instrumentalization of underrepresented minority students under the diversity rationale (their presence valued for others'' educational benefit rather than as remediation owed to them) a necessary structural feature of any diversity-based justification, or an avoidable framing choice universities could have made differently while still defending race-conscious admissions?',
    'Comparative analysis of alternative doctrinal formulations (e.g., a hybrid rationale citing both diversity benefit and remedial justice) and whether such hybrids have survived strict scrutiny anywhere.',
    'If instrumentalization is unavoidable given the diversity-only framing, the doctrine''s asymmetric beneficiary structure (majority students as primary beneficiary) is intrinsic to this specific reading, not an implementation flaw — strengthening the tangled_rope classification. If avoidable, the extraction from minority students'' dignity interest is more contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_versus_genuine_benefit, conceptual, 'Whether instrumentalizing minority students is intrinsic to the diversity rationale or a contingent framing choice.').

omega_variable(
    measurability_of_educational_diversity_benefit,
    'Is the claimed educational benefit of racial diversity (improved cross-racial understanding, classroom discussion quality, workforce preparation) genuinely measurable and attributable to racial composition specifically, or is it a plausible-sounding but empirically underdetermined justification that could not survive rigorous scrutiny?',
    'Review of the social-science literature record introduced across Grutter, Fisher, and SFFA litigation, including methodological critiques offered by both sides'' expert witnesses and the Court''s own assessment of administrability.',
    'If the benefit is genuinely measurable, ε should be understood as covering real coordination value plus extraction; if the benefit is largely unmeasurable, the coordination story is weaker and the classification should sit closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurability_of_educational_diversity_benefit, empirical, 'Whether the diversity rationale''s core empirical claim is well-supported or underdetermined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__diversity_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__diversity_reading, theater_ratio, 2016, 0.29).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.36).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__diversity_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__diversity_reading, suppression_requirement, 2016, 0.37).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the equal_protection_clause kernel. The colorblind_reading treats any racial classification as the violation itself (ε near-total on the government's use of race, near-zero coordination function claimed). The remedial_reading treats race-conscious action as required to redress group subordination, with minority claimants as direct rights-holders rather than instrumental means, and typically a sunset tied to remediation of the underlying disparity rather than a permanent pedagogical rationale. This diversity_reading occupies the doctrinal middle: it permits race-consciousness (unlike colorblind_reading) but grounds it in benefit-to-all rather than remedy-owed-to-harmed-groups (unlike remedial_reading), producing a moderate ε, a majority-centered beneficiary structure, and — per SFFA v. Harvard — an eventually foreclosed practical application even though the doctrine was never formally overruled as a theory until that decision substantially displaced it in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
