% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biology Reading of the 'Woman' Category (Chromosomal/Anatomical Membership Rule)
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the sex-biology reading of the contested 'woman'
 *   category kernel: category membership determined by
 *   chromosomal/anatomical/reproductive biology, with 'woman' defined as
 *   adult human female with XX chromosomes and typical female reproductive
 *   anatomy. This reading anchors sex-segregated sport eligibility, some
 *   shelter and prison admission criteria, and sex-based public health and
 *   crime data collection. It is presented by its advocates as a coordination
 *   mechanism — a stable, verifiable category that lets institutions
 *   administer protections without relying on contestable self-report. But
 *   the same fixed test extracts recognition and access from transgender
 *   women (who cannot satisfy it regardless of social role or duration of
 *   transition) and imposes invasive, stigmatizing verification burdens on
 *   intersex people whose biology falls outside the 'typical case' the
 *   definition names. Enforcement is active: sport federations run
 *   chromosomal or hormonal testing regimes, and shelters/legislatures
 *   periodically litigate eligibility. The sibling readings —
 *   gender_identity_reading and intersex_accommodation_reading — are NOT part
 *   of this story; they are separate constraints with their own ε,
 *   beneficiary/victim sets, and classifications, linked only via the kernel
 *   and cross-referenced in the omegas below.
 *
 * KEY AGENTS:
 *   - female_athletes_in_sex_segregated_sport: beneficiary of the eligibility category (organized/constrained)
 *   - transgender_women: primary target excluded from protections and categories under this reading (powerless/trapped)
 *   - intersex_people: ambiguously handled, bear invasive verification burden (powerless/trapped)
 *   - sports_governing_bodies: administers and enforces the biological test (institutional/constrained)
 *   - biology_based_policy_advocates: sets the political agenda for codifying this reading (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.58).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.52).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Reading of the 'Woman' Category (Chromosomal/Anatomical Membership Rule)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ddf04f21-3643-4048-96ee-594b71368f3e').
narrative_ontology:cs_kernel_codification('ddf04f21-3643-4048-96ee-594b71368f3e', distributed).
narrative_ontology:cs_authority_grounding('ddf04f21-3643-4048-96ee-594b71368f3e', distributed).
narrative_ontology:cs_reading_relation('ddf04f21-3643-4048-96ee-594b71368f3e', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('ddf04f21-3643-4048-96ee-594b71368f3e', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('ddf04f21-3643-4048-96ee-594b71368f3e', foundational, chromosomal_anatomical_sex_is_the_sole_membership_criterion).
narrative_ontology:cs_axiom_status(chromosomal_anatomical_sex_is_the_sole_membership_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ddf04f21-3643-4048-96ee-594b71368f3e', chromosomal_anatomical_sex_is_the_sole_membership_criterion, empirically_contingent).
narrative_ontology:cs_axiom('ddf04f21-3643-4048-96ee-594b71368f3e', secondary, self_reported_identity_is_administratively_unverifiable).
narrative_ontology:cs_axiom_status(self_reported_identity_is_administratively_unverifiable, holdable).
narrative_ontology:cs_axiom_grounding('ddf04f21-3643-4048-96ee-594b71368f3e', self_reported_identity_is_administratively_unverifiable, instrumental).
narrative_ontology:cs_reference_frame('ddf04f21-3643-4048-96ee-594b71368f3e', biological_essentialist_sex_category).
narrative_ontology:cs_drift_state('ddf04f21-3643-4048-96ee-594b71368f3e', contemporary_gender_recognition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ddf04f21-3643-4048-96ee-594b71368f3e', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_athletes_in_sex_segregated_sport).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_violence_data_collectors).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, womens_shelter_operators).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, biology_based_policy_advocates).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, detransitioned_individuals_seeking_flexible_recognition).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_dimorphism_is_a_real_biological_category).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_based_data_collection_has_policy_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compete in categories reserved for people with typical female biology on the premise that average post-pubertal male physiology confers performance advantages that identity alone does not neutralize. This reading's eligibility rule is what preserves the competitive category they train within; without it they would compete against a wider physiological range.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_athletes_in_sex_segregated_sport, beneficiary,
    organized, biographical, constrained, national).

% Compile crime, health, and demographic statistics using chromosomal/anatomical sex as the recorded variable, arguing that patterns of sex-based violence (rates, perpetration, victimization) require a stable biological category to be legible across time and jurisdictions. Their institutional output depends on the category remaining fixed and not redefined by self-identification.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_violence_data_collectors, beneficiary,
    institutional, generational, arbitrage, national).

% Operate single-sex spaces (shelters, changing rooms, prisons) justified by the trauma histories of residents who have experienced male violence; they administer admission criteria using the biological definition and can choose, within legal limits, how strictly to apply it. They bear reputational and legal costs when the criteria are challenged.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, womens_shelter_operators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, womens_shelter_operators, agenda_setter).

% Live and are socially recognized as women in most daily contexts but are excluded from the specific sex-segregated protections and categories this reading reserves for those with typical female biology — sport eligibility, some shelters, some data categories. They cannot change their chromosomal/anatomical status and have no legal or medical route that satisfies this reading's membership test; exit from the constraint means exit from the category entirely, which is not available to them.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, trapped, national).

% Possess biological variation that does not cleanly satisfy 'typical XX chromosomes and female reproductive anatomy' or the male equivalent. This reading handles them ambiguously — sometimes folded into the female category by administrative default, sometimes flagged as exceptions requiring case-by-case biological testing (hormone panels, karyotyping) that is itself invasive and stigmatizing. They did not choose their variation and have no voice in how the ambiguity is resolved.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, intersex_people, excluded).

% Individuals whose relationship to sex/gender categories has changed over their lives and who want recognition frameworks that accommodate biographical complexity; the fixed biological test offers clarity but no room for their particular histories, which do not map cleanly onto either 'always was' framing used to defend the category.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, detransitioned_individuals_seeking_flexible_recognition, payer,
    powerless, biographical, trapped, national).

% Campaign for legal codification of the biological definition in statute, sport governance, and record-keeping; they draft model legislation, fund litigation, and testify before regulatory bodies. They do not personally bear the exclusionary costs and can shift focus to other policy fights if this one stalls.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, biology_based_policy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Set and enforce eligibility criteria for competition categories, choosing whether to adopt chromosomal/anatomical tests, hormone thresholds, or identity self-report. They face litigation and reputational pressure from all sides and must administer whatever rule they adopt, including invasive verification procedures.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, sports_governing_bodies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, biologically anchored membership rule that sex-segregated systems (sport categories, shelters, some legal protections, epidemiological data) can administer consistently without relying on self-report, which those systems argue is harder to verify and easier to contest case-by-case.
% TRANSFER_FUNCTION: Moves access to sex-segregated spaces, competitive categories, and 'female' data classification toward people with typical female biology and away from transgender women; moves administrative burden and invasive scrutiny onto intersex people whose biology does not cleanly satisfy the test.
% ABSENT_VOICES: Transgender women and intersex people are frequently not seated at the bodies that draft eligibility criteria (sports federations, legislatures); their testimony is taken as advocacy input rather than as co-authorship of the rule that governs them. Detransitioned individuals with non-linear biographies are almost never consulted at all.
% DISAPPEARANCE_RATIONALE: Advocates for this reading say its disappearance would collapse verifiable sex-based protections and data categories, forcing shelters and sport bodies to rely on self-identification alone. Critics say the underlying protective functions (safety from male violence, fair competition) could be served by alternative criteria (hormone levels, case-by-case risk assessment) without a fixed biological test, so the world would rearrange around a different but still workable arrangement rather than becoming ungovernable.
% FOUNDING_PROBLEM: Historically, 'woman' as a legal and social category tracked observable reproductive/anatomical sex because that was the available and stable marker for allocating sex-differentiated protections, roles, and (later) anti-discrimination remedies targeting sex-based disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: Biology-based advocates and many sex-based-violence researchers attest the founding problem (verifiable sex-based protection and data integrity) remains live. Clinicians treating intersex patients and legal scholars studying trans rights attest, from outside the advocacy coalition that benefits from the strict reading, that the same protective functions can be served by less rigid criteria, and that the strict biological test now functions partly to exclude rather than merely to protect.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, contested).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects real, non-trivial costs imposed on transgender women and intersex people through exclusion and invasive scrutiny, but is moderated because the reading does deliver a genuine coordination good (stable data categories, a workable competition framework) to a real beneficiary set. Suppression (0.52) reflects active enforcement machinery — chromosomal/hormonal testing regimes, litigation, statutory codification efforts — required to hold the boundary once challenged; it is not passive natural fact. Accessibility collapse is moderate (0.4): alternative membership criteria (identity-based, spectrum-based) are visibly available and actively argued in the same institutions, so alternatives have not collapsed, they are being actively suppressed/contested. Resistance is high (0.78): this reading faces sustained organized opposition from trans rights advocacy, professional medical bodies, and international sport governance reform efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of female athletes or shelter operators, this reading is functional coordination solving a real safety/fairness problem. From the seat of transgender women, the identical rule is experienced as categorical exclusion enforced by testing regimes and litigation — the engine should compute divergent per-seat types from this same structural data, and that divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Female athletes, shelter operators, and sex-based-data institutions are declared beneficiaries: the biological test gives them a category they can administer and defend, so their directionality sits toward the subsidized end. Transgender women are declared victims with trapped exit — no biographical or legal route satisfies the chromosomal/anatomical test, so their directionality sits at the full-target end regardless of social integration elsewhere. Intersex people are also victims but their situation is structurally distinct from transgender women's: their exclusion is not exclusion by category mismatch but exclusion by category failure — the test itself does not resolve cleanly for their biology, producing case-by-case invasive determination rather than a clean no. This asymmetry is why they are listed separately rather than folded into a single victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifiable sex-based protection when observable anatomy was the only available marker) is contested as live: sex-based violence data collection and single-sex space safety arguments plausibly remain functionally necessary in some form, but the specific instrument — a fixed chromosomal/anatomical test with no accommodation pathway — may have outlived the absence of alternatives (hormone-panel-based frameworks, risk-based case assessment) that did not exist when the category was first legally codified. This is exactly the tangled_rope signature: real coordination function persists alongside asymmetric extraction that requires active enforcement to maintain, rather than either a pure Rope (no victims) or pure Snare (no genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_woman_category,
    'This story instantiates the sex_biology_reading of the woman_category kernel. The kernel also has a gender_identity_reading (membership by internal identity, victim set shifts to gender-nonconforming/questioning people who face identity policing) and an intersex_accommodation_reading (membership includes typical female biology plus intersex variation, victim set narrows and beneficiary framing shifts toward accommodation rather than exclusion). Which reading a jurisdiction or institution adopts determines who is structurally a victim and who is a beneficiary — the disagreement is located in what counts as the relevant membership-determining property (chromosome/anatomy vs. identity vs. spectrum-inclusive biology), not in any shared metric.',
    'Not empirically resolvable — this is a normative/definitional dispute about which property should ground legal and social category membership. Comparative institutional analysis (which reading produces better-corroborated outcomes against independently-measurable goals like safety, fairness, and non-discrimination) can inform but not settle it.',
    'Adopting a different reading changes the beneficiary/victim declarations entirely, which changes the computed classification. This story''s tangled_rope classification is specific to the sex_biology_reading''s structural data; the gender_identity_reading and intersex_accommodation_reading are separate constraint stories with their own ε and their own classification, not alternate measurements of this same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_woman_category, conceptual, 'Kernel contest: three sibling readings of woman_category produce three structurally distinct constraints, not one constraint measured three ways.').

omega_variable(
    intersex_handling_ambiguity,
    'Does the sex_biology_reading, as actually administered, treat intersex people as included-by-default, excluded-by-default, or subject to invasive case-by-case determination — and does this vary by institution (sport vs. shelter vs. legal statute)?',
    'Institutional audit of actual admission/eligibility determinations for intersex applicants across sport governing bodies, shelters, and jurisdictions applying this reading, coded for outcome and process (invasive testing required vs. not).',
    'If administration is predominantly invasive case-by-case testing, the effective extraction on intersex people is higher than a simple binary victim declaration captures, and suppression should be weighted toward the intersex seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_handling_ambiguity, empirical, 'Whether intersex accommodation under this reading is inclusion, exclusion, or invasive gatekeeping in practice.').

omega_variable(
    sport_performance_advantage_evidence_base,
    'How robust is the empirical evidence that post-pubertal male physiology confers a performance advantage that persists after hormone therapy, in the specific sports where this reading is used to justify eligibility exclusion?',
    'Systematic review of sport-specific physiological studies on hormone therapy duration and performance metrics, ideally pre-registered and conducted by researchers without a stake in either eligibility outcome.',
    'A strong, sport-specific evidence base would support the coordination-function claim in this reading''s beneficiary declarations for that sport; a weak or absent evidence base would suggest the extraction (exclusion of transgender women) is not adequately justified by the stated coordination rationale even within this reading''s own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sport_performance_advantage_evidence_base, empirical, 'Whether the sports performance-advantage justification is evidentially load-bearing or a post-hoc rationale for exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_category__sex_biology_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(woma_tr_t8, woman_category__sex_biology_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(woma_tr_t12, woman_category__sex_biology_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(woma_tr_t16, woman_category__sex_biology_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_category__sex_biology_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(woma_be_t8, woman_category__sex_biology_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(woma_be_t12, woman_category__sex_biology_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(woma_be_t16, woman_category__sex_biology_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_category__sex_biology_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_category__sex_biology_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(woma_su_t12, woman_category__sex_biology_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(woma_su_t16, woman_category__sex_biology_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the woman category.' Each sibling instantiates a different membership-determining property (biology, identity, biology-plus-intersex-spectrum) and carries its own ε, beneficiary/victim set, and classification. sex_biology_reading and its siblings are linked via network edges per the ε-invariance principle; none is a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
