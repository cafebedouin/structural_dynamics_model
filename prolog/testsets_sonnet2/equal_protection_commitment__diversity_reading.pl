% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Equal Protection Diversity Rationale — Race as Plus-Factor in Holistic Admissions
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This story instantiates the diversity reading of the equal protection
 *   kernel: the doctrine, running from Bakke (1978) through Grutter (2003) to
 *   its substantial curtailment in SFFA v. Harvard/UNC (2023), that race may
 *   be considered as one factor among many in holistic university admissions
 *   to achieve the compelling state interest of educational diversity. It is
 *   distinct from the colorblind reading (which forbids racial classification
 *   outright) and the remedial reading (which grounds race-consciousness in
 *   dismantling caste subordination rather than pedagogical benefit). This
 *   story authors ONLY the diversity rationale as its own constraint — the
 *   sibling readings are separate constraints with their own ε and their own
 *   stakeholder structures, linked here only by network reference and
 *   cs_structure.reading_relations, not folded into this one.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda_setter/beneficiary (institutional/arbitrage) — designs and administers holistic review, protected discretion
 *   - underrepresented_minority_applicants_admitted: beneficiary (moderate/constrained) — gains admission, bears stigma cost
 *   - rejected_applicants_all_backgrounds: payer (moderate/trapped) — displaced, no individualized remedy
 *   - asian_american_applicants: payer (moderate/constrained) — statistically concentrated cost per SFFA litigation record
 *   - federal_judiciary: observer (institutional/analytical) — applies and eventually narrows strict scrutiny
 *   - future_applicant_cohorts: excluded (powerless/trapped) — governed by calibration they had no voice in setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.32).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Rationale — Race as Plus-Factor in Holistic Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'ebf8e807-acc0-4fcc-94a5-ca5389870570').
narrative_ontology:cs_kernel_codification('ebf8e807-acc0-4fcc-94a5-ca5389870570', fixed_text).
narrative_ontology:cs_authority_grounding('ebf8e807-acc0-4fcc-94a5-ca5389870570', lineage).
narrative_ontology:cs_interpretation_layer_present('ebf8e807-acc0-4fcc-94a5-ca5389870570').
narrative_ontology:cs_reading_relation('ebf8e807-acc0-4fcc-94a5-ca5389870570', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('ebf8e807-acc0-4fcc-94a5-ca5389870570', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('ebf8e807-acc0-4fcc-94a5-ca5389870570', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('ebf8e807-acc0-4fcc-94a5-ca5389870570', educational_diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('ebf8e807-acc0-4fcc-94a5-ca5389870570', foundational, race_may_be_one_factor_among_many_in_individualized_review).
narrative_ontology:cs_axiom_status(race_may_be_one_factor_among_many_in_individualized_review, overridden).
narrative_ontology:cs_axiom_grounding('ebf8e807-acc0-4fcc-94a5-ca5389870570', race_may_be_one_factor_among_many_in_individualized_review, conventional).
narrative_ontology:cs_reference_frame('ebf8e807-acc0-4fcc-94a5-ca5389870570', grutter_holistic_diversity_framework).
narrative_ontology:cs_drift_state('ebf8e807-acc0-4fcc-94a5-ca5389870570', post_sffa_harvard_2023, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('ebf8e807-acc0-4fcc-94a5-ca5389870570', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants_admitted).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, rejected_applicants_all_backgrounds).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, asian_american_applicants).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, critical_mass_pedagogical_benefit_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer holistic admissions processes, deciding how much weight race receives as one factor among many. Justify the practice by citing pedagogical benefits of a diverse student body and institutional mission. Retain broad discretion over criteria, weighting, and process opacity, and are the party whose admissions autonomy the doctrine directly protects from strict judicial second-guessing (subject to narrow tailoring review).
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, selective_universities, beneficiary).

% Gain admission in part because race was considered as a favorable factor within a holistic file review. Benefit from the doctrine's existence but bear the secondary cost of stigma — the suspicion, sometimes voiced by peers or critics, that their qualifications were discounted rather than genuinely evaluated.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants_admitted, beneficiary,
    moderate, biographical, constrained, national).

% Denied admission at a selective institution; cannot know whether race-consciousness in someone else's file changed their own outcome because holistic review does not disclose comparative weighting. Have essentially no individual remedy — the diversity rationale is evaluated at the institutional-policy level, not applicant by applicant, foreclosing any single rejected applicant's ability to show they specifically were displaced by the practice.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, rejected_applicants_all_backgrounds, payer,
    moderate, biographical, trapped, national).

% As a group, statistically the most consistently disadvantaged by holistic personal-rating components correlated with race-conscious diversity goals, per litigation record (SFFA v. Harvard). Bear a concentrated, group-differentiated cost within a doctrine framed as diffuse and individualized, without a clean path to challenge the aggregate pattern because each file is reviewed 'holistically.'
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, asian_american_applicants, payer,
    moderate, generational, constrained, national).

% Applies strict scrutiny to race-conscious admissions, requiring narrow tailoring to the compelling interest of diversity. Historically deferred to university judgments about educational benefits (Grutter) before substantially curtailing the doctrine (SFFA v. Harvard/UNC, 2023), which shifted the practical viability of this reading even where its formal logic persists in dissent and in K-12/employment contexts.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Not yet in the applicant pool when admissions policy is set; have no voice in how race-consciousness is calibrated for the cohorts that will apply years later, yet will be governed by whatever the doctrine's boundaries are when they apply.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, future_applicant_cohorts, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows selective universities to pursue the pedagogical and civic benefits of a racially diverse student body — cross-racial understanding, breaking down stereotypes, preparing students for a diverse workforce and citizenry — by considering race as one factor among many in individualized review, without imposing quotas or fixed set-asides.
% TRANSFER_FUNCTION: Moves admission slots at capacity-constrained selective institutions from some applicants to others based partly on race-conscious weighting within holistic files; also moves institutional legal and reputational risk (from strict-scrutiny litigation exposure) onto universities that adopt the practice.
% ABSENT_VOICES: Rejected applicants of all backgrounds have no individualized mechanism to contest whether race specifically changed their own outcome, since holistic review is evaluated as an institutional policy rather than file-by-file; future applicant cohorts have no voice in setting the calibration that will govern them.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, selective universities would lose the primary doctrinal basis for considering race in admissions, admissions criteria and diversity-officer functions would be restructured around race-neutral proxies (as observed rapidly after SFFA v. Harvard in 2023), and the political and legal battle would shift entirely to the colorblind and remedial readings as competing frameworks.
% FOUNDING_PROBLEM: Selective institutions sought a constitutionally durable rationale for considering race in admissions after Bakke (1978) foreclosed quota systems and strict racial balancing, needing a framework that survived strict scrutiny while still permitting some race-consciousness.
% FOUNDING_PROBLEM_CORROBORATION: Universities and diversity-research scholars attest the pedagogical-benefits problem remains live. The Supreme Court majority in SFFA v. Harvard/UNC (2023), an institution external to and adversarial toward the beneficiary universities, held that the diversity rationale as implemented was not susceptible to meaningful judicial review and had not been shown to be narrowly tailored — an outside corroboration that the doctrinal solution had drifted from a defensible remedy for its stated problem, even as it did not resolve the underlying question of whether diversity itself remains a live compelling interest.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   ε is authored low-moderate (0.28 at interval end) because the constraint is procedural — it governs HOW race may be weighed, not a substantive quota or fixed transfer — and because the harm to any individual rejected applicant is diffuse and largely unprovable given holistic review's opacity. Suppression sits moderate (0.32): the constraint does not coerce compliance from applicants in any direct sense, but it does foreclose individualized judicial remedy for rejected applicants (the holistic-file opacity functions as structural suppression of contestability, not of behavior). Theater ratio rises over the interval (0.20 to 0.40) reflecting increasing gap between the doctrine's stated pedagogical-benefit rationale and its actual operation as documented in litigation discovery (SFFA v. Harvard revealed personal-rating patterns correlated with race that the university could not adequately explain in pedagogical terms), consistent with a rationale whose proxy metrics (diversity statistics, mission statements) increasingly substituted for demonstrated compelling-interest fit.
 *
 * DIRECTIONALITY LOGIC:
 *   Selective universities sit near the beneficiary end: they gain admissions discretion and reputational/mission benefits, and their exit options are effectively arbitrage-grade (they can adjust proxies, essay prompts, and holistic weighting to preserve diversity outcomes even under judicial constraint, as post-2023 adaptation has shown). Underrepresented minority applicants admitted under the doctrine are also beneficiaries, though the stigma externality is a real secondary cost not captured by the primary metric. Rejected applicants generally, and Asian American applicants specifically, sit toward the target end: they bear the transfer with no visibility into whether or how race affected their specific outcome, and their exit options are trapped/constrained because selective higher education access has no substitute market at that tier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — finding a constitutionally survivable rationale for race-consciousness after Bakke foreclosed quotas — was itself a doctrinal compromise from the outset, not a fix for genuine unmet coordination need. Its status is contested rather than cleanly dead: the pedagogical-diversity interest as an educational proposition may remain sociologically live even after SFFA curtailed its constitutional vehicle. This story treats the founding_problem_status as contested precisely because the mismatch (status=contested + disappearance_verdict=world_rearranges) signals a doctrine whose formal justification survived past the point where courts found its operational fit demonstrable — worth flagging for the mismatch-only consumer rather than resolving here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_versus_remedial_rationale_convergence,
    'Does the diversity rationale function, in practice, as a disguised remedial rationale (redressing historical exclusion) wearing pedagogical-benefit language because the remedial framing was constitutionally riskier post-Croson/Adarand?',
    'Compare admitted-cohort composition and university internal deliberations (where discoverable via litigation, as in SFFA v. Harvard) against publicly stated pedagogical rationale; substantial divergence would support the disguised-remedial-purpose reading.',
    'If the diversity rationale is substantially a proxy for remedial purpose, this constraint''s classification should more closely track the remedial_reading sibling''s structure (different beneficiary framing, different victim theory) rather than standing as an independent procedural doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_versus_remedial_rationale_convergence, conceptual, 'Whether diversity rationale is a genuine independent interest or a doctrinal proxy for the remedial reading.').

omega_variable(
    holistic_review_opacity_as_structural_versus_incidental,
    'Is the opacity of holistic review (which prevents any individual rejected applicant from proving race changed their specific outcome) a necessary feature of individualized assessment, or a structural choice that shields the practice from the individualized-injury standing doctrine typically required for equal protection challenges?',
    'Compare admissions systems that publish detailed scoring rubrics and race-weight disclosures (if any exist) against opaque holistic systems for litigation outcomes and applicant challengeability.',
    'If opacity is a structural choice rather than a necessity, the suppression metric authored here (0.32) understates the degree to which the doctrine''s persistence depends on foreclosing individual-level accountability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(holistic_review_opacity_as_structural_versus_incidental, empirical, 'Whether holistic-review opacity is inherent to individualized assessment or a deliberate accountability shield.').

omega_variable(
    compelling_interest_naturalness_ambiguity,
    'Is ''educational diversity as compelling state interest'' a genuine, judicially discoverable constitutional value, or a constructed doctrinal category whose primary function is to preserve university admissions discretion against stricter colorblind or remedial alternatives?',
    'Track whether the compelling-interest finding is ever revisited or narrowed independent of political composition of the reviewing court, versus tracking strictly with appointments — the latter would support the constructed-category reading.',
    'If constructed primarily to preserve institutional discretion, the beneficiary declaration for selective_universities is even more central than authored, and the tangled_rope classification is more clearly warranted over any rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_naturalness_ambiguity, conceptual, 'Whether the compelling-interest doctrine is a discovered constitutional value or an institutionally-serving construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_commitment__diversity_reading, theater_ratio, 1988, 0.25).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__diversity_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_commitment__diversity_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.18).
narrative_ontology:measurement(equa_be_t1988, equal_protection_commitment__diversity_reading, base_extractiveness, 1988, 0.2).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__diversity_reading, base_extractiveness, 2013, 0.25).
narrative_ontology:measurement(equa_be_t2018, equal_protection_commitment__diversity_reading, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1988, equal_protection_commitment__diversity_reading, suppression_requirement, 1988, 0.22).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.24).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__diversity_reading, suppression_requirement, 2013, 0.28).
narrative_ontology:measurement(equa_su_t2018, equal_protection_commitment__diversity_reading, suppression_requirement, 2018, 0.3).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_commitment kernel, decomposed per the ε-invariance principle: colorblind_reading (near-mountain procedural rule forbidding racial classification, low ε, high accessibility_collapse if genuinely originalist), diversity_reading (this story — procedural, low-moderate ε, tangled_rope), and remedial_reading (substantive anti-subordination rationale, likely higher ε given explicit victim-group targeting of remedy, tangled_rope or snare depending on implementation). Each carries its own ε, beneficiary/victim structure, and classification; they are linked here by network reference and by cs_structure.reading_relations rather than merged into one constraint with an averaged or observer-dependent ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
