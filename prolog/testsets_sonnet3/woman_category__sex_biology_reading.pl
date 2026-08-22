% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Sex-Biology Reading of the Category 'Woman' (Chromosomal/Anatomical/Reproductive Definition)
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This story authors the sex-biology reading of the contested 'woman'
 *   category kernel: category membership is fixed by chromosomal, anatomical,
 *   and reproductive facts, with 'adult human female with XX chromosomes and
 *   female reproductive anatomy' as the typical case. This is one of three
 *   sibling readings of the same kernel (the others being
 *   gender-identity-based and intersex-accommodation-based readings, authored
 *   as separate constraints). Under this reading's own lights, the standing
 *   arrangement is the current sex-segregation regime in sports, shelters,
 *   prisons, and sex-based data collection as adjudicated by this biological
 *   test — not the rights-respecting alternative any sibling reading would
 *   install. Extraction rises over the measured interval as the reading's
 *   boundary has been extended from its original sports/safety justification
 *   into a wider set of legal and administrative contexts (identification
 *   documents, broader anti-discrimination carve-outs), increasing the
 *   population for whom the boundary is binding and increasing the
 *   enforcement apparatus (medical testing panels, legal challenges,
 *   litigation) required to hold the line.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.58).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.62).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Reading of the Category 'Woman' (Chromosomal/Anatomical/Reproductive Definition)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'a14b9680-b59b-4c87-a31a-f9c9deedb2aa').
narrative_ontology:cs_kernel_codification('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', distributed).
narrative_ontology:cs_authority_grounding('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', distributed).
narrative_ontology:cs_reading_relation('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', foundational, natal_reproductive_biology_is_immutable_and_controlling).
narrative_ontology:cs_axiom_status(natal_reproductive_biology_is_immutable_and_controlling, holdable).
narrative_ontology:cs_axiom_grounding('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', natal_reproductive_biology_is_immutable_and_controlling, empirically_contingent).
narrative_ontology:cs_axiom('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', secondary, average_sex_dimorphism_justifies_categorical_not_individualized_sorting).
narrative_ontology:cs_axiom_status(average_sex_dimorphism_justifies_categorical_not_individualized_sorting, holdable).
narrative_ontology:cs_axiom_grounding('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', average_sex_dimorphism_justifies_categorical_not_individualized_sorting, instrumental).
narrative_ontology:cs_reference_frame('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', pre_identity_politics_biomedical_sex_classification).
narrative_ontology:cs_drift_state('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', contemporary_gender_identity_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a14b9680-b59b-4c87-a31a-f9c9deedb2aa', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, womens_sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_athletes_in_contact_and_strength_sports).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_violence_data_collectors).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, single_sex_shelter_and_prison_administrators).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people_with_ambiguous_classification).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_dimorphism_grounds_categorical_boundary).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, reproductive_biology_is_the_relevant_predicate_for_sex_segregation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set eligibility criteria for women's competitive categories using chromosomal or hormonal testing tied to this reading's definition. They administer the boundary, defend it as necessary for competitive fairness given average performance differences from puberty-driven androgenization, and face litigation and reputational pressure from both directions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, womens_sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Compete in categories reserved for people who developed under typical female puberty. Under this reading they retain access to podiums, scholarships, and safety protections in categories they say would be foreclosed to them if performance-relevant biological categories were replaced by self-identification. Their exit option if the category dissolved is to compete in an open category against a wider performance distribution.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_athletes_in_contact_and_strength_sports, beneficiary,
    moderate, biographical, constrained, national).

% Live socially, legally, and often medically as women but are excluded from the category as this reading defines it. They are barred from women's sports categories, sex-segregated shelters, prisons, and some single-sex spaces, and are the explicit subject of the boundary-drawing exercise. They cannot change their chromosomal or natal anatomical history, which this reading treats as the controlling fact; their only route to inclusion is a redefinition of the category itself, which is exactly what this reading forecloses.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, trapped, national).

% Have chromosomal, gonadal, or anatomical configurations that do not cleanly fit the 'typical case' the definition names as canonical. Sports and legal bodies applying this reading must adjudicate borderline cases (e.g. certain DSD conditions) ad hoc, often through invasive testing, and intersex people bear the burden of proving membership in a category built around a modal case that excludes their bodies by construction.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people_with_ambiguous_classification, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, intersex_people_with_ambiguous_classification, excluded).

% Compile crime statistics, health research, and epidemiological data using natal-sex categories because reproductive and anatomical variables (pregnancy, cervical cancer risk, sexual violence patterns tied to anatomy) are the relevant predicates for the research question. This reading gives them a stable, non-self-report-dependent variable to collect against.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_violence_data_collectors, beneficiary,
    institutional, generational, analytical, national).

% Administer housing and safety arrangements premised on anatomical vulnerability and single-sex privacy norms (rape crisis shelters, women's prisons). They apply this reading's boundary to determine placement, citing safety of natal-female residents as the justification, and face litigation from excluded transgender women and advocacy pressure from both directions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, single_sex_shelter_and_prison_administrators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, single_sex_shelter_and_prison_administrators, agenda_setter).

% Argue the relevant predicate for 'woman' should be self-identified gender, not natal biology. Under this reading's framework their claim is not merely disagreed with but structurally foreclosed — the category is defined in terms this reading treats as immutable and non-elective, so identity-based membership claims cannot be accommodated without abandoning the reading's foundational premise.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_reading_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate disputes between this reading and rival readings in specific statutory contexts (sports eligibility statutes, anti-discrimination law, prison placement policy, single-sex service exemptions). Their rulings determine which reading has legal force in a given domain and can shift the boundary jurisdiction by jurisdiction.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable (non-self-report) predicate for allocating goods and protections where average biological sex differences are held to be causally relevant: competitive fairness in sports with performance-relevant sexual dimorphism, physical safety in intimate single-sex spaces, and consistent variables for reproductive and violence-related public health data.
% TRANSFER_FUNCTION: Moves competitive slots, safety-premised placements, and category-based legal protections toward people whose natal biology matches the 'typical case' definition, and away from transgender women (categorically) and ambiguously from intersex people, who must either seek case-by-case exceptions or accept exclusion from the sex-segregated goods the category gates.
% ABSENT_VOICES: Transgender women and gender_identity_reading advocates are present as objecting parties but structurally cannot be accommodated within this reading without abandoning its foundational premise; intersex people are formally covered by the 'typical case' hedge but their actual treatment is adjudicated ad hoc, often without their voice in the criteria-setting process (sports federations' medical panels, legislative drafting).
% DISAPPEARANCE_RATIONALE: If this reading's boundary vanished and were replaced by the gender_identity_reading tomorrow, women's sports categories as currently constituted would need to reorganize around a different eligibility test, single-sex shelter and prison placement policy would change intake criteria, and sex-based public health and crime data collection would need a new stable variable. The rearrangement would be immediate and structural, not cosmetic.
% FOUNDING_PROBLEM: Historically, this reading's boundary was built to track real average biological differences (musculoskeletal, cardiovascular, reproductive) that were treated as relevant to fairness in physical competition, safety in intimate spaces, and epidemiological research — problems that predate and are independent of the contemporary contest over gender identity.
% FOUNDING_PROBLEM_CORROBORATION: Sports physiologists and some public health researchers outside the advocacy coalitions on either side attest that average post-pubertal sex differences in athletic performance and certain health risks are empirically real and were the original basis for sex-segregated categories, predating the current identity dispute. Transgender rights advocates and some clinicians dispute that this residual empirical fact justifies the reading's current scope (e.g. applying it to non-contact sports, or to shelter placement where safety concerns are not clearly biology-dependent), arguing the boundary has been extended well past its original justification.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58 at interval end) reflects that transgender women are categorically excluded from protections and spaces this category gates, and intersex people face an ad hoc, medicalized adjudication process the 'typical case' framing does not resolve. Suppression (0.62) is driven by the active enforcement machinery required to police the boundary — eligibility testing in sports, documentation requirements in legal contexts, and litigation defending exclusions. Accessibility collapse is moderate (0.5): the category itself is not fully closed to contest, since courts, legislatures, and sports bodies actively revisit boundary cases, so alternatives have not fully foreclosed. Resistance is high (0.72) because transgender advocacy organizations, some medical bodies, and civil rights litigation actively contest the boundary in nearly every domain it is applied.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (sports federations, shelter administrators) the arrangement reads as a rope: a workable, defensible coordination solution to a real fairness/safety problem. From the transgender women seat it reads as a snare: total, non-negotiable exclusion from goods gated by an attribute they cannot change, defended by active enforcement. From the intersex seat it reads as something closer to a tangled rope with unresolved edges: coordination exists but leaves them structurally unaccommodated by the category's own admission. The engine computes these per-seat divergences from the structural (power/exit) data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Womens' sports governing bodies, single-sex shelter/prison administrators, and sex-based data collectors are beneficiaries: the reading gives them a stable, litigation-defensible administrative test and, in the sports case, preserves competitive categories valued by female athletes. Transgender women are the clearest target: the category is defined in terms of the exact attribute (natal biology) they cannot alter, and their exclusion is total rather than partial. Intersex people are victims by a different mechanism — not total exclusion but structural ambiguity, since the 'typical case' hedge in the definition itself concedes their bodies fall outside the modal case, requiring case-by-case medicalized adjudication that is itself a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (real average biological differences relevant to athletic performance and certain safety contexts) remains partially live — sports physiology data on post-pubertal performance differences is not seriously disputed even by many who reject this reading's broader application. But the reading's scope has expanded well past sports and safety into a general definitional claim used in identification documents, employment contexts, and general anti-discrimination carve-outs where the biological predicate's relevance is much more contested. This is a partial-mandatrophy signature: the classification should not treat 'the founding problem is dead' as settled, but it should also not treat every current application of the boundary as equally justified by that founding problem — the tangled_rope classification (real coordination function bundled with extraction beyond that function) captures this better than either pure rope or pure snare would.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_predicate_scope_ambiguity,
    'Is chromosomal/anatomical sex the relevant predicate across ALL the domains this reading is currently applied to (sports, shelters, prisons, legal identification, general anti-discrimination law), or only in the narrower subset (contact/strength sports, anatomically-premised safety contexts) where the founding empirical justification actually holds?',
    'Domain-by-domain empirical and legal analysis: does the specific good being gated (competitive fairness, physical safety, data validity) depend on the biological predicate, or has the category been extended to domains (e.g. general legal identification) where the predicate''s relevance is asserted rather than demonstrated?',
    'If the predicate is relevant only in the narrow domains, this reading''s high extraction and suppression scores in the broader domains reflect scope-creep beyond the founding coordination function, strengthening a tangled_rope reading with the extraction concentrated in the extended domains; if relevant broadly, the classification shifts toward a cleaner rope with lower extraction across the board.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_predicate_scope_ambiguity, empirical, 'Whether the biological predicate''s relevance is domain-general or domain-specific.').

omega_variable(
    intersex_typical_case_hedge,
    'Does the ''typical case'' qualifier in this reading''s own definition function as a genuine accommodation of biological complexity, or as a structural admission that the category''s core test does not actually resolve membership for a known population?',
    'Track how sports federations and legal bodies applying this reading actually adjudicate intersex cases: consistent, transparent criteria would support the hedge-as-accommodation reading; ad hoc, medically invasive, or outcome-driven adjudication would support the hedge-as-admission reading.',
    'If adjudication is ad hoc, intersex people''s classification as victims (rather than merely ''ambiguous'') strengthens, and the tangled_rope''s asymmetric-extraction gate is more clearly satisfied for this group specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_typical_case_hedge, conceptual, 'Whether the intersex hedge is genuine accommodation or a structural gap.').

omega_variable(
    sports_fairness_generalization,
    'Does the performance-advantage rationale that justifies this reading in contact and strength sports generalize to sports without a clear strength/power performance gap, or to non-sports domains (shelters, IDs) where a different rationale (safety, privacy) is doing the actual work?',
    'Comparative performance-gap data across sport types, and separate causal analysis of whether safety/privacy rationales in shelters and prisons are empirically about anatomy specifically or about a broader (and more contestable) categorical claim.',
    'Determines whether the high ε this story authors for sports and violence-data domains should be understood as the reading''s core, well-grounded application, with weaker domains representing extension beyond the founding problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sports_fairness_generalization, empirical, 'Whether the sports performance rationale generalizes across sport types and non-sports domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t4, woman_category__sex_biology_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(woma_tr_t8, woman_category__sex_biology_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(woma_tr_t12, woman_category__sex_biology_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(woma_tr_t16, woman_category__sex_biology_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_category__sex_biology_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(woma_be_t8, woman_category__sex_biology_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(woma_be_t12, woman_category__sex_biology_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(woma_be_t16, woman_category__sex_biology_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t4, woman_category__sex_biology_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(woma_su_t8, woman_category__sex_biology_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(woma_su_t12, woman_category__sex_biology_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(woma_su_t16, woman_category__sex_biology_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the woman_category kernel (BGS-style decomposition). The sex_biology_reading, gender_identity_reading, and intersex_accommodation_reading each author a distinct victim set, distinct ε, and distinct claimed_type from the same contested category label. This reading's victim set (transgender women, ambiguously intersex people) is largely the beneficiary set the gender_identity_reading is built to protect, and vice versa — the two readings' extraction profiles are structurally mirrored, not merely differently weighted. The intersex_accommodation_reading sits partially orthogonal to both, proposing a spectrum model that could in principle reduce the intersex-specific extraction present in this reading without resolving the transgender-specific dispute with the gender_identity_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
