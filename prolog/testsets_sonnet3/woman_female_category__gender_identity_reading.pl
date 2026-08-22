% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Woman/Female Category Membership — Gender Identity Reading
 *   domain: political philosophy / bioethics / gender studies / law
 *
 * SUMMARY:
 *   Under this reading, whether someone counts as a woman (or a man, or
 *   belongs to any gender category) for legal, social, and institutional
 *   purposes is settled by that person's self-declared internal identity, not
 *   by chromosomal sex, reproductive anatomy, or developmental biology. The
 *   reading has been substantially codified into law, employment policy,
 *   sporting-body rules, and facility-access policy across many jurisdictions
 *   since roughly the 2010s, replacing biology-based tests in domains that
 *   had previously used them (prisons, shelters, changing rooms, sport, legal
 *   sex-marker changes). Advocacy organizations for transgender recognition
 *   are the primary agenda-setters pushing adoption; courts and regulators
 *   are the adjudicating institutions; natal women in sex-segregated safety
 *   and competitive contexts bear the structural cost when the category
 *   boundary they relied on for exclusion of males is redefined to admit
 *   anyone who self-identifies as female.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.52).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Woman/Female Category Membership — Gender Identity Reading").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political philosophy / bioethics / gender studies / law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '413d8889-2ee0-48de-a764-722535eb952e').
narrative_ontology:cs_kernel_codification('413d8889-2ee0-48de-a764-722535eb952e', distributed).
narrative_ontology:cs_authority_grounding('413d8889-2ee0-48de-a764-722535eb952e', distributed).
narrative_ontology:cs_reading_relation('413d8889-2ee0-48de-a764-722535eb952e', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('413d8889-2ee0-48de-a764-722535eb952e', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('413d8889-2ee0-48de-a764-722535eb952e', foundational, self_attested_identity_constitutes_category_membership).
narrative_ontology:cs_axiom_status(self_attested_identity_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('413d8889-2ee0-48de-a764-722535eb952e', self_attested_identity_constitutes_category_membership, deontological).
narrative_ontology:cs_axiom('413d8889-2ee0-48de-a764-722535eb952e', secondary, biological_criteria_for_sex_category_are_illegitimate_grounds_for_exclusion).
narrative_ontology:cs_axiom_status(biological_criteria_for_sex_category_are_illegitimate_grounds_for_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('413d8889-2ee0-48de-a764-722535eb952e', biological_criteria_for_sex_category_are_illegitimate_grounds_for_exclusion, conventional).
narrative_ontology:cs_reference_frame('413d8889-2ee0-48de-a764-722535eb952e', identity_self_attestation_as_sufficient_criterion).
narrative_ontology:cs_drift_state('413d8889-2ee0-48de-a764-722535eb952e', post_2020_institutional_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('413d8889-2ee0-48de-a764-722535eb952e', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_men_and_nonbinary_people_seeking_recognition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, female_athletes_in_sex_segregated_sport).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, detained_and_incarcerated_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, domestic_violence_shelter_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal, social, and institutional recognition as women on the basis of self-identification rather than surgical or hormonal status. Gain access to female-designated categories, spaces, and protections that align with their identity. Face severe harm — exclusion, harassment, violence — where recognition is denied; the reading is what makes their claim to the category legible in policy and law.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition, beneficiary,
    moderate, biographical, identity_locked, national).

% Benefit from the same self-identification principle applied in reverse or laterally — exit from a natally-assigned category, or recognition of a category outside the binary, without requiring medical gatekeeping. Their standing under this reading rises and falls together with the trans women's claim, since both depend on identity, not biology, as the sorting criterion.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_men_and_nonbinary_people_seeking_recognition, beneficiary,
    moderate, biographical, identity_locked, national).

% Use changing rooms, shelters, prisons, and other sex-segregated facilities on the premise that co-occupants share their reproductive biology and vulnerability profile. Under this reading, admission is governed by self-identification, so they cannot exclude a self-identified woman regardless of birth sex without being cast as discriminatory. Exit means forgoing the service or space entirely — there is no alternative single-sex facility organized on the sex-biology criterion in most jurisdictions where this reading is codified.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces, payer,
    powerless, biographical, trapped, national).

% Compete in categories created to offset average male performance advantages accrued through puberty. Under this reading, competitors who identify as women qualify for the female category regardless of retained post-pubertal physiological advantage. Athletes can appeal to federations or exit the sport, but cannot alter the category rule from within competition; many report feeling unable to voice objection without professional and social penalty.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, female_athletes_in_sex_segregated_sport, payer,
    moderate, biographical, constrained, national).

% Housed in women's facilities that under this reading admit any self-identified woman, including those convicted of violent or sexual offenses against women, without a biological-sex screen. They have no capacity to select their housing conditions and no mechanism to exit the facility; incidents of assault by transferred inmates have driven several high-profile policy reversals.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, detained_and_incarcerated_women, payer,
    powerless, immediate, trapped, regional).

% Seek refuge from male violence in shelters premised on excluding males from the physical space. Under this reading, exclusion criteria based on birth sex are treated as impermissible discrimination against self-identified women, so shelters admitting on identity cannot categorically screen by natal sex. Residents in crisis have essentially no capacity to shop among alternative shelters.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, domestic_violence_shelter_residents, payer,
    powerless, immediate, trapped, local).

% Lobby legislatures, courts, professional associations, and employers to codify self-identification as the operative criterion for category membership, and litigate or campaign against institutions that retain biological criteria. They set the interpretive agenda for what counts as discrimination under this reading and can shift strategy or venue if one avenue closes.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Adjudicate disputes between the competing readings, issue rulings and regulations that either adopt or reject self-identification as the legal test, and bear responsibility for reconciling anti-discrimination statutes with single-sex exemptions. Their rulings determine which reading has legal force in a given jurisdiction at a given time.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, courts_and_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, courts_and_regulatory_agencies, observer).

% Argue that sex-based protections require a biological criterion and that self-identification erodes the basis for single-sex provision. Frequently characterized as bigoted in institutional and media discourse operating under this reading, which limits their access to the platforms and processes where category rules are set, even though they are directly affected parties.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_and_womens_rights_groups, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple test for category membership — declared identity — that avoids requiring institutions to conduct medical or biological verification, and extends recognition and protection to people whose lived identity does not match natal sex.
% TRANSFER_FUNCTION: Moves the benefit of unambiguous, protected access to a chosen category to individuals who self-identify into it, and moves the cost of maintaining sex-based exclusion criteria (safety, fairness, privacy premised on shared biology) onto natal members of the category who cannot re-exclude on that basis without being cast as discriminating.
% ABSENT_VOICES: Gender critical and women's rights groups, and many rank-and-file natal women in affected facilities and sports, are frequently excluded from policy-setting venues or face reputational and professional consequences for raising the biological-criterion objection inside institutions that have adopted this reading.
% DISAPPEARANCE_RATIONALE: If self-identification were dropped as the operative test overnight, admission to single-sex facilities, sports categories, and legal sex markers would revert to a biological-sex criterion (the sibling reading), and the recognition currently extended to trans individuals under this reading would lapse, requiring institutions to build new accommodation pathways or lose them.
% FOUNDING_PROBLEM: Trans individuals faced exclusion, violence, and denial of legal recognition when category membership was tested strictly against birth-assigned sex, with no route to recognition short of invasive medical requirements many could not or would not meet.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and many public-health and legal bodies attest the founding problem remains live and that self-identification is the appropriate remedy. Independent of the beneficiary set, several government reviews, sporting federations' own commissioned physiological studies, and prison-safety audits in multiple jurisdictions have found that self-identification as the sole criterion creates unaddressed safety and fairness costs for natal women — corroboration that the remedy's scope is contested even where the underlying exclusion problem is acknowledged as real.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-not-extreme 0.58 because the reading confers a genuine, real benefit (recognition, reduced exclusion and violence risk for trans people) while imposing serious, well-documented costs on natal women in safety-critical single-sex contexts (shelters, prisons, changing rooms) and competitive-fairness contexts (sport) — this is not a pure extraction story, hence tangled_rope rather than snare. Suppression is authored at 0.52 and rising over the interval: as the reading has been codified, institutions and individuals raising the biological-criterion objection have faced increasing professional, legal, and reputational consequences (employment tribunal findings against gender-critical belief in some early years, later partially reversed; social-media deplatforming; loss of platform access), which is the suppression mechanism proper to a tangled_rope rather than incidental friction. Theater ratio is kept low (0.22, rising modestly) because the underlying coordination and extraction functions are both real and operative, not primarily performative — this is a substantive dispute over a real category boundary, not a symbolic gesture.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals seeking recognition are the structural beneficiaries: the reading is the mechanism by which their identity claim becomes legally and institutionally cognizable, and its withdrawal would materially worsen their position (d near the beneficiary end). Natal women in single-sex safety and competitive contexts are the structural targets: the reading redefines the boundary they relied on for exclusion, and their exit options are near-zero once the reading is institutionally codified in the facilities and leagues they must use (d near the full-target end, especially for the trapped detained and shelter populations). Advocacy organizations and courts are agenda-setters with mobile/analytical exit — they can shift venues, strategies, or jurisdictions; the payer populations, particularly incarcerated women and shelter residents in crisis, cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusion and violence against trans individuals under a strict natal-sex test — was real and, per the corroboration record, remains at least partially live; this blocks a simple 'pure mandatrophy' read where the arrangement has lost all function and now only extracts. But the reading's expansion into safety-critical single-sex spaces (detention, shelters) without an accompanying risk-differentiated accommodation has generated a second, independently corroborated problem (documented assaults, competitive unfairness) that the reading's own institutional defenders have been slow to acknowledge — this is the signature the tangled_rope classification is built to catch: genuine coordination function coexisting with, not replaced by, asymmetric extraction that requires active enforcement (deplatforming, tribunal findings, institutional policy mandates) to sustain against resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is ''woman/female category membership'' correctly modeled as a single contested fact-of-the-matter question, or as three structurally distinct social arrangements (identity-based, biology-based, context-split) that happen to share a natural-language label?',
    'This ambiguity is resolved at the corpus level, not within this story: the kernel is decomposed into three sibling constraint files (gender_identity_reading, sex_biology_reading, hybrid_contextual_reading), each with its own ε, beneficiary/victim structure, and classification, linked via network.affects_constraints. This story speaks only for the identity-reading.',
    'If treated as one constraint with one ε, the sharply different extraction profiles of the three readings would be averaged into an uninformative middle value; decomposition preserves the fact that each reading has genuinely different victims, beneficiaries, and structural extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel requires decomposition into sibling readings (resolved: yes, via this family).').

omega_variable(
    safety_versus_dignity_tradeoff_weighting,
    'How should the framework weigh the dignity/recognition harm to trans individuals from exclusion against the physical-safety and competitive-fairness harm to natal women from unqualified self-identification admission, when both are simultaneously real?',
    'No empirical resolution exists for a value-weighting question; the hybrid_contextual_reading represents one attempted institutional resolution (splitting the criterion by context) whose adoption or rejection by courts and legislatures over time will reveal which weighting prevails in practice, though not which is correct.',
    'A reading that weights dignity harm decisively higher will classify the arrangement closer to Rope (genuine, undominated coordination gain); a reading that weights safety/fairness harm decisively higher will classify it closer to Snare (the coordination story is cover for extraction). The tangled_rope claim here reflects an authored judgment that neither harm dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_versus_dignity_tradeoff_weighting, preference, 'Irreducible value disagreement over relative weight of dignity harms versus safety/fairness harms.').

omega_variable(
    reversal_trajectory_uncertainty,
    'Will the rising suppression trend (tribunal and reputational consequences for the biological-criterion objection) continue, or is the trajectory already reversing as several jurisdictions revisit self-identification policies in prisons and sport following documented incidents?',
    'Track policy reversal rate across jurisdictions and sporting federations over the next several years; a sustained reversal would indicate the suppression measurements plateau or fall rather than continuing to rise past the interval end.',
    'If reversal is sustained, the correct model may be closer to a scaffold (an interim arrangement being actively renegotiated) than a stable tangled_rope; continued codification without reversal supports the tangled_rope classification standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_trajectory_uncertainty, empirical, 'Whether the measured suppression/enforcement trend will continue or is already reversing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__gender_identity_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__gender_identity_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__gender_identity_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_female_category__gender_identity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_female_category__gender_identity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_female_category__gender_identity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(woma_su_t4, woman_female_category__gender_identity_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(woma_su_t8, woman_female_category__gender_identity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(woma_su_t12, woman_female_category__gender_identity_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(woma_su_t16, woman_female_category__gender_identity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the woman_female_category kernel, decomposed per the ε-invariance principle because the natural-language question ('who counts as a woman') resolves to structurally distinct claims depending on the operative criterion (identity vs. biology vs. context-split). Each sibling authors its own ε, beneficiaries, victims, and claimed_type. This file (gender_identity_reading) claims tangled_rope with ε=0.58; expect the sex_biology_reading to author a different beneficiary/victim inversion and a different ε, and the hybrid_contextual_reading to author a lower ε reflecting its attempt to capture both coordination functions with reduced asymmetric extraction. All three must cross-link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
