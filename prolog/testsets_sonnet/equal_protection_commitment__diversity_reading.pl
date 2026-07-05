% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection — Diversity-as-Compelling-Interest Reading
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This story instantiates the diversity reading of the equal protection
 *   kernel: race may be considered as one factor among many in university
 *   admissions because a diverse student body serves a compelling educational
 *   interest — a doctrinal path opened by Justice Powell's solo Bakke
 *   opinion, consolidated by a majority in Grutter v. Bollinger (2003), and
 *   terminated by Students for Fair Admissions v. Harvard/UNC (2023). This is
 *   a distinct constraint from the remedial reading (which grounds
 *   race-conscious measures in dismantling caste subordination rather than
 *   pedagogical benefit) and the colorblind reading (which forbids racial
 *   classification outright). The three readings are not the same constraint
 *   measured differently — they have different beneficiary/victim structures,
 *   different doctrinal lineages, and different fates before the Court. This
 *   story's ε (0.28) reflects a procedural, individualized-consideration
 *   constraint rather than a quota or set-aside; the theater_ratio rising
 *   through the 2000s-2010s reflects the doctrine's own drift toward numeric
 *   proxies for diversity dressed in holistic-review language, which is
 *   precisely what SFFA found unreviewable and unmoored from a logical
 *   endpoint.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda_setter (institutional/arbitrage) — designs and administers the admissions process
 *   - underrepresented_minority_applicants: beneficiary (moderate/constrained) — gains admissions consideration
 *   - diversity_rationale_advocates: beneficiary (organized/mobile) — defends the doctrine's legitimacy
 *   - unsuccessful_applicants_across_groups: payer (powerless/constrained) — bears an unreviewable individual cost
 *   - asian_american_applicants: payer (moderate/constrained) — statistically disadvantaged litigants
 *   - federal_courts: observer (institutional/analytical) — adjudicates strict scrutiny
 *   - k12_and_pipeline_institutions: excluded (powerless/trapped) — upstream cause, no doctrinal voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.32).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection — Diversity-as-Compelling-Interest Reading").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '32f06101-4c40-4b8c-96d0-591484f2b6ab').
narrative_ontology:cs_kernel_codification('32f06101-4c40-4b8c-96d0-591484f2b6ab', fixed_text).
narrative_ontology:cs_authority_grounding('32f06101-4c40-4b8c-96d0-591484f2b6ab', lineage).
narrative_ontology:cs_interpretation_layer_present('32f06101-4c40-4b8c-96d0-591484f2b6ab').
narrative_ontology:cs_reading_relation('32f06101-4c40-4b8c-96d0-591484f2b6ab', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('32f06101-4c40-4b8c-96d0-591484f2b6ab', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_axiom('32f06101-4c40-4b8c-96d0-591484f2b6ab', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('32f06101-4c40-4b8c-96d0-591484f2b6ab', educational_diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('32f06101-4c40-4b8c-96d0-591484f2b6ab', secondary, race_may_be_one_factor_among_many_in_individualized_review).
narrative_ontology:cs_axiom_status(race_may_be_one_factor_among_many_in_individualized_review, overridden).
narrative_ontology:cs_axiom_grounding('32f06101-4c40-4b8c-96d0-591484f2b6ab', race_may_be_one_factor_among_many_in_individualized_review, conventional).
narrative_ontology:cs_reference_frame('32f06101-4c40-4b8c-96d0-591484f2b6ab', powell_diversity_compelling_interest).
narrative_ontology:cs_drift_state('32f06101-4c40-4b8c-96d0-591484f2b6ab', post_sffa_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('32f06101-4c40-4b8c-96d0-591484f2b6ab', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, diversity_rationale_advocates).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, unsuccessful_applicants_across_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, asian_american_applicants).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer holistic admissions processes that weigh race as one factor among many in service of a stated pedagogical mission (the educational benefits of a diverse student body). They control the weighting, the file review, and the narrative justification, and they bear no direct cost from denied applicants; they gain broad discretion to shape class composition while insulating individual decisions from outside scrutiny under 'holistic' review.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain admissions consideration that accounts for the ways race has shaped their opportunities and experiences, increasing access to selective institutions relative to a race-blind baseline. Their exit options are constrained by the small number of comparably resourced institutions; they benefit from the constraint's operation without controlling its administration.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Legal scholars, amici, and university associations who built and defend the diversity rationale as the surviving doctrinal basis for race-conscious admissions after remedial and compensatory rationales were rejected by the Court. They benefit reputationally and institutionally from the rationale's continued vitality but bear none of the individual admissions costs.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, diversity_rationale_advocates, beneficiary,
    organized, generational, mobile, national).

% Applicants of any background denied admission cannot know whether or how race entered their file's holistic evaluation; the individualized, non-mechanical review that the diversity rationale requires is also what makes any single denial legally and practically unreviewable. Their recourse is largely limited to applying elsewhere or, rarely, litigation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, unsuccessful_applicants_across_groups, payer,
    powerless, biographical, constrained, national).

% Statistically and in litigation records shown to be disadvantaged in personal-rating and overall-admission components relative to academic-index performance at some institutions applying the diversity rationale. They organize litigation and public advocacy but face the same opacity barrier as other unsuccessful applicants in proving individual causation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Adjudicate strict-scrutiny challenges to admissions programs claiming the diversity rationale, requiring narrow tailoring and periodic reassessment (e.g., the 25-year expectation floated in Grutter). They can uphold, strike, or narrow the doctrine and did so decisively in 2023.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Underfunded schools and pipeline programs whose structural disadvantages are the deeper cause of underrepresentation are not parties to the admissions litigation or doctrine at all; the diversity rationale addresses composition at the point of selective admission rather than the upstream disparities, and no seat in this constraint speaks for them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, k12_and_pipeline_institutions, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows selective universities to pursue a stated pedagogical goal — a student body diverse enough to produce the cross-racial understanding, reduced stereotyping, and preparation for a diverse workforce that the Court accepted as compelling — without a race-blind admissions system, while nominally cabining the practice through strict scrutiny and narrow-tailoring requirements.
% TRANSFER_FUNCTION: Moves admissions probability at a fixed number of selective seats among applicant groups according to race-conscious holistic weighting; also moves reputational and legal legitimacy toward universities and diversity-rationale advocates who administer and defend the doctrine.
% ABSENT_VOICES: K-12 and pipeline institutions whose upstream disparities are the deeper driver of underrepresentation have no seat in the admissions-doctrine conversation at all; individual denied applicants across all groups cannot see inside their own file review to know what happened to their claim.
% DISAPPEARANCE_RATIONALE: If this reading vanished, selective universities would need to rely on race-neutral proxies (socioeconomic status, geography, first-generation status) or abandon numeric diversity targets; underrepresented-minority admission rates at the most selective institutions would likely fall in the near term (as observed in states that banned the practice by referendum before Students for Fair Admissions), and litigation over what counts as an impermissible proxy would intensify.
% FOUNDING_PROBLEM: Following Bakke (1978), the Court needed a rationale for race-conscious admissions that could survive strict scrutiny after rejecting quotas and social-remediation justifications; Powell's solo diversity rationale, later adopted by a majority in Grutter (2003), supplied one grounded in academic freedom and pedagogical benefit rather than compensatory justice.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority in Students for Fair Admissions v. Harvard (2023) itself concluded the diversity rationale had become unmoored from measurable, judicially reviewable ends and that the programs before it lacked the logical endpoint Grutter had promised — an assessment from the adjudicating body itself, not from the universities or advocates who administered and benefited from the doctrine. Justice O'Connor's own 25-year sunset expectation in Grutter is a rare instance of a doctrine's own architect anticipating its obsolescence.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is low-moderate (0.28) because the diversity reading is explicitly procedural — race is one factor, not a determinative one, and the doctrine requires periodic reassessment rather than permanent entitlement. Suppression (0.32) is moderate: the mechanism that makes the doctrine legally survivable (individualized, holistic, non-formulaic review) is the same mechanism that makes any single denial unreviewable, so the suppression is baked into the coordination structure itself rather than externally imposed. Theater ratio climbs through the doctrine's middle period (peaking near 0.38 around 2013-2018) as institutions increasingly used 'holistic' language to pursue what the Court ultimately found were de facto numeric targets — precisely the substitution SFFA identified. Accessibility collapse (0.35) is moderate-low: race-neutral alternatives (socioeconomic proxies, percentage plans, geographic diversity) existed and were litigated as alternatives throughout, so alternatives were never fully foreclosed. Resistance (0.55) is substantial and organized, culminating in successful litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities sit at the beneficiary end: they gain broad, largely unreviewable discretion over class composition and bear no individual cost from any denial. Underrepresented minority applicants and diversity-rationale advocates are structural beneficiaries of the doctrine's operation without controlling its administration. Unsuccessful applicants generally, and Asian American applicants in particular given the empirical admissions-component disparities documented in litigation, sit toward the target end: they are constrained (selective institutions are scarce and reapplication does not cure the underlying opacity) and cannot observe how the factor operated in their own file. K-12 institutions are excluded entirely from the constraint's operation despite being the deeper structural cause the doctrine gestures toward addressing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — finding a constitutionally survivable rationale for race-conscious admissions after Bakke foreclosed quotas — was itself an act of doctrinal improvisation, and Justice O'Connor's own 25-year sunset expectation in Grutter is unusually explicit architect-level acknowledgment that the arrangement was meant to be temporary. Classifying this as tangled_rope rather than scaffold reflects that no binding sunset clause was ever enacted (the 25-year window was aspirational dicta, not a legal has_sunset_clause commitment) and that active enforcement (strict scrutiny litigation) was required throughout rather than a declared wind-down. The 2023 status shift to founding_problem_status=dead, corroborated by the adjudicating court itself rather than by the universities or advocacy groups who benefited from the doctrine, is the mismatch signal the R5 interview is built to surface: a genealogy corroborated only by beneficiaries would read status=live indefinitely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_vs_remedial_grounding_ambiguity,
    'Was the diversity rationale ever a stable, independent justification for race-conscious admissions, or was it always a legally survivable proxy for remedial goals that Bakke had foreclosed from being stated directly?',
    'Close comparison of internal university admissions-committee deliberations and mission statements (where discoverable in litigation) against the public diversity rationale — do institutions'' actual justifications track pedagogical diversity benefits or track compensatory/remedial goals dressed in diversity language?',
    'If the diversity rationale was substantially a proxy for the remedial rationale, this constraint and the remedial_reading constraint are less structurally distinct in practice than the doctrine claims, even though they remain formally separate holdings; this would suggest higher suppression (the true justification was itself suppressed to survive scrutiny) than the metrics above assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_vs_remedial_grounding_ambiguity, conceptual, 'Whether the diversity rationale functioned as an independent justification or a scrutiny-survivable proxy for the remedial rationale.').

omega_variable(
    holistic_review_opacity_omega,
    'Is the individualized, non-formulaic nature of holistic review a genuine requirement of narrow tailoring, or was it partly a strategic opacity mechanism that made the constraint''s actual operation unreviewable by design?',
    'Comparative analysis of admissions systems that disclosed internal scoring rubrics under litigation discovery (as occurred in SFFA v. Harvard) against the doctrine''s stated requirement that race not be used mechanically — did disclosed practice reveal quantifiable, near-formulaic racial weighting despite formally holistic framing?',
    'If largely strategic, the accessibility_collapse and suppression scores understate how effectively the doctrine foreclosed individual-level accountability; the theater_ratio trajectory would be read as evidence of this drift rather than a benign administrative evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_review_opacity_omega, empirical, 'Whether holistic-review opacity was doctrinally necessary or strategically extractive.').

omega_variable(
    compelling_interest_naturalness_omega,
    'Is ''the educational benefits of diversity'' a discoverable pedagogical fact the Court deferred to, or a constructed legal category whose primary function was to make race-conscious admissions survivable rather than to describe a genuine, measured educational outcome?',
    'Review of the social-science record the Court relied on (e.g. amicus briefs citing educational-outcome studies) against subsequent independent replication of claimed cross-racial-understanding and reduced-stereotyping benefits.',
    'If the compelling-interest finding was weakly empirically grounded, the vindicated_propositions entry (educational_diversity_compelling_interest_doctrine) is itself a constructed justification rather than a discovered fact, strengthening the case that beneficiaries (universities, advocates) shaped the doctrine''s own epistemic foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compelling_interest_naturalness_omega, empirical, 'Whether the compelling educational-diversity interest is empirically grounded or doctrinally constructed to survive scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_commitment__diversity_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__diversity_reading, theater_ratio, 2013, 0.34).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_commitment__diversity_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.18).
narrative_ontology:measurement(equa_be_t1990, equal_protection_commitment__diversity_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.24).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__diversity_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement(equa_be_t2018, equal_protection_commitment__diversity_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_commitment__diversity_reading, suppression_requirement, 1990, 0.24).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.28).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__diversity_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(equa_su_t2018, equal_protection_commitment__diversity_reading, suppression_requirement, 2018, 0.32).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'equal protection and race-conscious admissions' per the ε-invariance principle: diversity_reading (this file, ε≈0.28, procedural/tangled_rope), remedial_reading (grounded in anti-caste/anti-subordination purpose, distinct beneficiary/victim framing), and colorblind_reading (forbids racial classification outright, ultimately prevailing doctrine post-2023). Each carries its own ε, stakeholders, and classification; they are linked here rather than merged because measuring 'equal protection and race' under any single observable conflates three structurally distinct legal claims with different histories and different winners.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
