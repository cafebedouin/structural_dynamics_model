% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Membership by Reproductive Biology (Biology Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   Under the biology reading, legal and institutional sex categories (for
 *   prisons, shelters, sports, anti-discrimination protections, and vital
 *   records) are anchored to chromosomal and anatomical facts fixed at birth,
 *   irrespective of subsequent social transition, hormone therapy, or
 *   surgical intervention. This reading is advanced primarily to secure a
 *   stable, verifiable basis for sex-segregated protections that its
 *   advocates argue serve cis women's safety and fairness interests. Its
 *   operation excludes trans women from 'woman' as a legal and social
 *   category, excludes trans men from 'man', and forces intersex individuals
 *   into a binary assignment that may not track their actual physiology. The
 *   reading solves a genuine coordination problem (a workable sorting
 *   criterion for sex-segregated space) but does so by imposing a categorical
 *   exclusion on three distinct populations who bear that exclusion as a
 *   structural cost regardless of their individual circumstances.
 *
 * KEY AGENTS:
 *   - cis_women_seeking_sex_based_protections: Primary beneficiary (organized/constrained) — gains a stable, legally secure basis for sex-segregated claims
 *   - trans_women: Primary target (powerless/trapped) — excluded from 'woman' category regardless of transition status
 *   - intersex_individuals: Secondary target (powerless/trapped) — forced into a binary that does not match their physiology
 *   - trans_men_seeking_male_recognition: Secondary target (powerless/trapped) — retained in 'female' category despite transition
 *   - sports_governing_bodies_using_biological_eligibility: Institutional agenda-setter/beneficiary (institutional/arbitrage) — adopts the reading for administrative and fairness cover
 *   - legislators_and_courts: Analytical/agenda-setting observer (institutional/analytical) — adjudicates which reading has legal force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.62).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership by Reproductive Biology (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, 'be2c66e0-b430-48c2-bdc6-d2d496de969c').
narrative_ontology:cs_kernel_codification('be2c66e0-b430-48c2-bdc6-d2d496de969c', distributed).
narrative_ontology:cs_authority_grounding('be2c66e0-b430-48c2-bdc6-d2d496de969c', distributed).
narrative_ontology:cs_reading_relation('be2c66e0-b430-48c2-bdc6-d2d496de969c', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('be2c66e0-b430-48c2-bdc6-d2d496de969c', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('be2c66e0-b430-48c2-bdc6-d2d496de969c', foundational, reproductive_biology_is_the_sole_categorical_ground).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_sole_categorical_ground, holdable).
narrative_ontology:cs_axiom_grounding('be2c66e0-b430-48c2-bdc6-d2d496de969c', reproductive_biology_is_the_sole_categorical_ground, empirically_contingent).
narrative_ontology:cs_axiom('be2c66e0-b430-48c2-bdc6-d2d496de969c', foundational, category_membership_is_fixed_at_birth_and_immutable).
narrative_ontology:cs_axiom_status(category_membership_is_fixed_at_birth_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('be2c66e0-b430-48c2-bdc6-d2d496de969c', category_membership_is_fixed_at_birth_and_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('be2c66e0-b430-48c2-bdc6-d2d496de969c', reproductive_biology_as_categorical_ground).
narrative_ontology:cs_drift_state('be2c66e0-b430-48c2-bdc6-d2d496de969c', contemporary_legal_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('be2c66e0-b430-48c2-bdc6-d2d496de969c', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sports_governing_bodies_using_biological_eligibility).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, biology_reading_advocacy_organizations).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men_seeking_male_recognition).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_dimorphism_grounds_legal_category).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, reproductive_capacity_determines_protected_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for single-sex spaces (shelters, prisons, sports categories, changing rooms) on the premise that biological sex, not identity, determines who shares vulnerable spaces with them. Under the biology reading their category membership is unambiguous and their claims to sex-segregated resources are legally and socially secure without needing to litigate individual cases.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Have transitioned socially and often medically but are categorized as male under the biology reading regardless of hormone levels, surgical status, or lived presentation. Excluded from women's shelters, sports categories, prisons, and legal 'woman' status. Exit requires either abandoning the category claim entirely or relocating to a jurisdiction with a different kernel reading — an option most cannot exercise.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, trapped, national).

% Born with chromosomal or anatomical variation that does not map cleanly onto either binary category the biology reading requires. Forced into an administrative assignment (often made at birth without their consent) that may not match their later anatomy, hormone profile, or self-understanding. The reading's insistence on a clean biological binary erases their actual physiological reality rather than accommodating it.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Categorized as female under the biology reading despite years of testosterone therapy, surgical transition, and social presentation as men. Retain formal classification as female for legal and administrative purposes such as prison assignment or sex-segregated sports, which can place them in contexts inconsistent with their transition and expose them to specific safety risks.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men_seeking_male_recognition, payer,
    powerless, biographical, trapped, national).

% Administer eligibility rules for competitive categories, adopting the biology reading to justify chromosomal or endogenous-testosterone screening as the basis for the women's category. Benefit from a bright-line rule that is easier to defend administratively than a case-by-case identity assessment, and can invoke 'fairness to female athletes' as institutional cover.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies_using_biological_eligibility, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, sports_governing_bodies_using_biological_eligibility, beneficiary).

% Lobby legislatures and courts to codify reproductive biology as the sole determinant of sex category in law, litigation strategy, and public messaging. Fund legal challenges to identity-based policies and draft model legislation. Their institutional survival and funding are tied to the reading's continued political salience.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biology_reading_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Adjudicate which kernel reading governs statutory sex categories, birth certificate amendment, prison placement, and anti-discrimination law. Their rulings determine which reading has legal force in a given jurisdiction and are subject to appeal, referendum, and legislative override as the contest continues.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, legislators_and_courts, agenda_setter).

% Argue the biology reading is not a neutral factual description but a policy choice that erases lived identity and imposes disproportionate costs on trans and intersex people. Present alternative evidence and testimony in litigation and legislative hearings but are structurally positioned as challengers to, rather than authors of, the biology reading's institutional footholds.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively low-cost, verifiable criterion (chromosomes/anatomy at birth) for sorting people into sex categories across law, sport, and single-sex institutional space — solving the real coordination problem of who shares vulnerable, sex-segregated resources (shelters, prisons, changing rooms, competitive sports divisions).
% TRANSFER_FUNCTION: Moves access to sex-segregated spaces, competitive sports categories, legal recognition, and safety accommodations toward those whose biology matches their claimed category at birth, and away from trans women, trans men, and intersex individuals whose biology and lived identity diverge from the binary the reading enforces.
% ABSENT_VOICES: Trans and intersex individuals directly affected by boundary enforcement are frequently not parties to the legislative and judicial processes that codify the biology reading; medical and endocrinological expertise on intersex variation is often excluded from binary-framed legal drafting. Trans rights organizations participate as external challengers, not as co-authors of the standard.
% DISAPPEARANCE_RATIONALE: If the biology reading were displaced overnight by a competing reading, sex-segregated institutions (prisons, shelters, sports federations) would need to re-derive eligibility criteria, cis women's organizations would lose an already-secured bright-line basis for their claims, and trans/intersex individuals currently excluded would gain access to categories currently closed to them — a substantial reallocation of institutional access, not a cosmetic change.
% FOUNDING_PROBLEM: Historically, sex categories were used to organize protections (maternity provisions, single-sex spaces, sport divisions) understood to track reproductive biology and its associated vulnerabilities (pregnancy, physical strength differentials, sexual violence risk) — a problem legislators and institutions needed a workable, verifiable sorting criterion to address.
% FOUNDING_PROBLEM_CORROBORATION: Sports scientists and some endocrinologists attest that biological factors (testosterone exposure, skeletal structure) retain relevance for competitive fairness in some contexts, corroborating a live residual problem from outside the advocacy organizations. Conversely, human rights bodies, some medical associations, and intersex advocacy groups (an audience with no stake in either advocacy camp's litigation funding) attest that the founding problem as originally framed — protecting cis women from specific harms — does not require an immutable biology test and can be addressed through narrower, context-specific criteria; they read the current broad application as exceeding the original problem's scope.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the reading transfers concrete institutional access (shelters, sports categories, legal recognition, prison placement) away from three defined populations toward a defined beneficiary population, via an enforced categorical rule rather than individualized assessment. Suppression is authored at 0.62, reflecting the active legal, legislative, and administrative machinery (statutory definitions, birth-certificate rules, sports-federation testing regimes) required to maintain the boundary against contestation. Theater ratio is low (0.2) because the enforcement machinery performs real sorting work, not mere performance — this is a genuinely operative boundary, not a vestigial one. Accessibility collapse is high (0.7): once codified in statute or federation rule, alternative sorting criteria become very difficult to invoke case-by-case. Resistance is high (0.72), reflecting sustained, organized litigation and advocacy contesting the reading from trans and intersex advocacy organizations.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (cis women's advocacy organizations, sports bodies), the biology reading looks like Rope — a clean, low-cost coordination solution to a genuine sorting problem. From the payer seats (trans women, trans men, intersex individuals), the same rule computes as enforced extraction: their institutional access is foreclosed by a categorical criterion imposed without regard to their individual circumstances. The engine should register this as seat divergence within a single Tangled Rope classification — the coordination function is real (a workable eligibility rule exists) and the extraction is real and asymmetric (falls entirely on non-beneficiary populations), which is exactly the Tangled Rope signature rather than a pure Rope or pure Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women's organizations and sports/legal bodies that adopt the biology reading sit near the beneficiary end: the reading secures their claims without requiring them to individually justify exclusion. Trans women, trans men, and intersex individuals sit near the full-target end: the reading imposes a categorical determination on them that they did not choose and largely cannot exit — jurisdictional relocation is the only exit option and is unavailable to most. This maps cleanly onto the declared beneficiary/victim structure; no override was needed because the powerless/trapped combination for the victim groups already derives a high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a workable basis for sex-segregated protections tied to specific vulnerabilities (pregnancy, physical differentials, assault risk) — retains partial contemporary relevance in narrow contexts (some sports categories), which is why founding_problem_status is authored as contested rather than dead. Declaring this a Snare would understate the genuine coordination function claimed by sports bodies and legislators; declaring it a pure Rope would erase the concentrated, asymmetric cost borne by trans and intersex populations who have no path to reclassification. Tangled Rope is the structurally honest label: real coordination function, real and asymmetric extraction, maintained by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_reading_kernel_indexation,
    'Is the biology reading the historically original and thus ''default'' reading of the sex_gender_category kernel, or is it one of three co-equal contemporary readings with no privileged claim to priority?',
    'Historical-legal analysis of when and how sex categories were first codified in the relevant jurisdictions, cross-referenced with whether those original codifications anticipated or excluded the trans/intersex cases now contested.',
    'If the biology reading is genuinely the historically original default, its claim to represent ''the'' sex category (rather than ''a'' reading of a contested category) is stronger, which affects how courts should weigh burden-of-proof in litigation between readings. If it is not privileged, treating it as default rather than contested unfairly forecloses the sibling readings in practice even where they coexist in principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biology_reading_kernel_indexation, conceptual, 'Whether the biology reading holds a privileged historical-default status relative to the identity and hybrid readings.').

omega_variable(
    intersex_forced_binary_naturalness,
    'Is the binary sex classification the biology reading enforces itself a natural fact about human biology, or a constructed simplification that the reading imposes on a biologically non-binary reality (intersex variation)?',
    'Endocrinological and developmental-biology literature on the frequency and diversity of intersex variation, assessed against whether a strict binary can be derived from biological data alone versus requires a policy choice about where to draw administrative lines.',
    'If intersex variation shows sex is not cleanly binary at the biological level, the biology reading''s claim to derive category membership directly and unambiguously from biology is undermined — the boundary-drawing for intersex cases is itself a policy choice, not a biological fact, which would reduce the reading''s claim to naturalness and support recharacterizing part of its operation as constructed rather than discovered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_forced_binary_naturalness, empirical, 'Whether the reading''s binary sorting is dictated by biology itself or requires an additional policy decision for non-binary biological cases.').

omega_variable(
    cost_benefit_distribution_across_readings,
    'Does the biology reading''s asymmetric cost distribution (concentrated on trans/intersex populations, diffused as security benefit to cis women) represent a defensible tradeoff given the coordination problem, or does it externalize costs disproportionately relative to the safety benefit achieved?',
    'Comparative empirical study of safety outcomes in jurisdictions using biology-reading criteria versus identity-reading or hybrid-reading criteria for shelters, prisons, and sports, controlling for confounds.',
    'If safety outcomes do not differ meaningfully across reading regimes, the extraction imposed on trans/intersex populations under the biology reading would be difficult to justify as necessary to the coordination function, strengthening the case that a substantial share of the reading''s operation is extraction beyond what coordination requires.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_benefit_distribution_across_readings, empirical, 'Whether the biology reading''s asymmetric costs are proportionate to a demonstrated safety benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t4, sex_gender_category__biology_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__biology_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__biology_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__biology_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sex__be_t4, sex_gender_category__biology_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__biology_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__biology_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__biology_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__biology_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__biology_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__biology_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__biology_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sex_gender_category kernel. sex_gender_category__identity_reading authors category membership by self-identification (excludes cis women's sex-based-harm claims as a distinct victim set, includes trans people as beneficiaries). sex_gender_category__hybrid_reading authors a medical-gatekeeping model combining biological and social-transition criteria (produces a third, distinct victim/beneficiary structure centered on those who cannot or will not complete formal transition requirements). Each story has its own epsilon, beneficiaries, victims, and classification per the epsilon-invariance principle; none is a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
