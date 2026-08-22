% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biology Reading of the Woman/Female Category Kernel
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the sex-biology reading of the contested
 *   woman/female category kernel: category membership is fixed by chromosomal
 *   sex, reproductive anatomy, and developmental biology, independent of
 *   self-identification. Under this reading, access to female-designated
 *   spaces (prisons, shelters, sports categories, certain legal recognitions)
 *   tracks natal sex rather than gender identity. This is deliberately ONE of
 *   three readings of the same kernel — the gender_identity_reading and
 *   hybrid_contextual_reading are separate constraint files with their own
 *   beneficiary/victim sets and their own ε; this file does not average over
 *   them, hedge between them, or describe their contest internally. The
 *   extraction this story measures is specific to this reading's own
 *   operation: the cost borne by trans women excluded from spaces and
 *   categories they seek access to, as assessed from within a framework that
 *   holds the sex-biology criterion as correct. The rising
 *   extraction/suppression trajectory reflects the reading's institutional
 *   entrenchment (more jurisdictions and federations formally codifying
 *   sex-based tests) over the interval, not a claim about which reading is
 *   normatively correct.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: beneficiary class, organized/national, advocates for sex-based category boundaries
 *   - trans_women_seeking_female_space_access: primary target of exclusion under this reading, powerless/trapped
 *   - womens_sports_governing_bodies: institutional beneficiary and agenda-setter, administers eligibility testing
 *   - domestic_violence_shelter_operators: institutional agenda-setter, administers admission policy under safety rationale
 *   - prison_administrators: institutional agenda-setter, administers facility placement
 *   - medical_and_developmental_biology_researchers: analytical observer, does not adjudicate the normative boundary question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.42).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.38).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biology Reading of the Woman/Female Category Kernel").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '9af33eea-59cc-43c1-8006-cc79a04bbafe').
narrative_ontology:cs_kernel_codification('9af33eea-59cc-43c1-8006-cc79a04bbafe', distributed).
narrative_ontology:cs_authority_grounding('9af33eea-59cc-43c1-8006-cc79a04bbafe', distributed).
narrative_ontology:cs_reading_relation('9af33eea-59cc-43c1-8006-cc79a04bbafe', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('9af33eea-59cc-43c1-8006-cc79a04bbafe', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('9af33eea-59cc-43c1-8006-cc79a04bbafe', foundational, reproductive_biology_is_the_correct_category_criterion).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_correct_category_criterion, holdable).
narrative_ontology:cs_axiom_grounding('9af33eea-59cc-43c1-8006-cc79a04bbafe', reproductive_biology_is_the_correct_category_criterion, empirically_contingent).
narrative_ontology:cs_axiom('9af33eea-59cc-43c1-8006-cc79a04bbafe', secondary, physical_safety_differentials_justify_sex_based_exclusion).
narrative_ontology:cs_axiom_status(physical_safety_differentials_justify_sex_based_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('9af33eea-59cc-43c1-8006-cc79a04bbafe', physical_safety_differentials_justify_sex_based_exclusion, instrumental).
narrative_ontology:cs_reference_frame('9af33eea-59cc-43c1-8006-cc79a04bbafe', biological_sex_dimorphism_baseline).
narrative_ontology:cs_drift_state('9af33eea-59cc-43c1-8006-cc79a04bbafe', post_gender_identity_legal_recognition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9af33eea-59cc-43c1-8006-cc79a04bbafe', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, womens_sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, domestic_violence_shelter_operators).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_seeking_female_space_access).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sexual_dimorphism_grounds_legal_sex_category).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, reproductive_biology_determines_protected_class_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for categories (prisons, shelters, changing rooms, sports divisions) to be defined by chromosomal/reproductive sex rather than self-identification, on the ground that physical strength differentials and histories of male violence make sex-based screening a genuine safety and fairness mechanism. Their exit option is limited: they cannot personally re-engineer institutional admission rules, only lobby and litigate for the reading to be adopted.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Live as women in every social respect but are excluded from female-designated spaces, sports categories, and legal recognition under this reading because they do not meet the chromosomal/gamete-production definition. Bear the direct cost of exclusion: housing in male facilities under detention, exclusion from women's shelters, exclusion from women's sports, denial of updated legal sex markers. Exit from the category dispute is not available to them short of abandoning the claim to womanhood altogether.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_seeking_female_space_access, payer,
    powerless, biographical, trapped, national).

% Administer eligibility rules for women's competitive categories and adopt chromosomal/developmental criteria to preserve what they describe as competitive fairness given average post-pubertal performance differentials. They both benefit from the reading (it protects the commercial and competitive integrity of the women's category they administer) and set its enforcement terms (testing protocols, eligibility panels).
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, womens_sports_governing_bodies, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, womens_sports_governing_bodies, agenda_setter).

% Operate single-sex shelters serving women fleeing male violence and set admission policy under this reading using natal-sex criteria, citing survivor trauma responses to male physiology and presence. They administer the boundary directly and could in principle adopt a different admission standard, but describe the cost of doing so (survivor attrition, funder requirements, legal liability) as high.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, domestic_violence_shelter_operators, agenda_setter,
    institutional, biographical, constrained, regional).

% Decide facility placement for incarcerated people and, under this reading, house individuals according to chromosomal/anatomical sex rather than gender identity, citing documented assault-risk statistics from mixed housing arrangements. They hold direct administrative power over the boundary and bear political and legal liability for whichever standard they adopt.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_administrators, agenda_setter,
    institutional, immediate, constrained, national).

% Study sexual dimorphism, gamete production, and developmental pathways (including intersex variation) without adjudicating the legal or social question of category membership; their findings are cited by advocates on this side of the kernel but do not themselves settle the contested normative question of what a category boundary should track.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_and_developmental_biology_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, biologically verifiable criterion for allocating access to sex-segregated spaces and categories (prisons, shelters, sports, some medical contexts) where the underlying rationale for segregation is physical sex characteristics (strength, anatomy, reproductive capacity) rather than gender identity or presentation.
% TRANSFER_FUNCTION: Moves access to female-designated spaces, competitive categories, and certain legal recognitions toward natal females and away from trans women, on the premise that the relevant safety/fairness rationale for the space tracks chromosomal/developmental sex rather than identity.
% ABSENT_VOICES: Trans women excluded under this reading are structurally present as the named payer class but their own account of the harm of exclusion (psychological, social, safety-in-male-facilities) is frequently absent from the venues where the sex-biology reading is adopted as policy — sports federations and legislatures dominated by advocacy from the beneficiary side. Intersex people, whose bodies do not cleanly satisfy either chromosomal or gamete-based criteria, are also largely unheard in the framing of the boundary itself.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned entirely (i.e., no institution anywhere used sex-biology criteria for any category boundary), single-sex sports categories would be reorganized, prison and shelter placement policy would shift to identity-based standards, and the safety and fairness arguments currently mobilized by the beneficiary class would lose their institutional footing — a substantial rearrangement of concrete admission and eligibility rules across multiple domains.
% FOUNDING_PROBLEM: Physical sex differences (average strength, anatomy, and the vulnerability differentials that follow from them) created a documented safety and fairness problem in contexts of forced proximity (incarceration, shelter, competitive sport) between men and women; sex-segregated spaces and categories were built to solve that specific problem using observable, verifiable sex characteristics.
% FOUNDING_PROBLEM_CORROBORATION: Domestic violence researchers and correctional-safety auditors outside the direct advocacy coalition corroborate that sex-based physical safety concerns in shelters and prisons remain empirically live; sports physiologists studying post-pubertal performance differentials (independent of advocacy organizations) corroborate a live basis for competitive-category concerns. Trans-inclusive advocates and some legal scholars dispute that chromosomal/anatomical criteria are the necessary or least-harmful way to address the underlying safety problem, arguing individualized risk assessment could serve the same function with less exclusionary cost — this is the locus of the contest, not a settled fact on either side.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).
:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.42) reflects real, concrete costs imposed on trans women — exclusion from female facilities, sports categories, and legal recognition — but is authored at a moderate rather than extreme level because the reading's proponents offer a genuine, non-pretextual coordination rationale (documented physical safety and competitive fairness concerns) rather than pure animus; the coordination function is real by this reading's own lights, which is why this is authored as tangled_rope rather than snare. Suppression (0.38) is moderate: enforcement exists (eligibility testing, facility assignment policy, legal sex-marker requirements) but is not maximally coercive — trans women retain other social contexts and are not physically confined by this mechanism alone. Resistance (0.72) is high because the exclusion is actively and visibly contested by trans advocacy, some legal scholars, and international human rights bodies — this is not a settled, unresisted arrangement. Accessibility collapse (0.5) is moderate: alternative frameworks (hybrid contextual criteria, individualized risk assessment) are visibly proposed and adopted in some jurisdictions, so alternatives have not fully collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats (shelter operators, sports bodies, prison administrators, natal-female advocates), the arrangement is read as protective coordination solving a genuine safety/fairness problem using a verifiable criterion. From the payer seat (trans women), the identical rule structure is read as exclusionary extraction — denial of a claimed identity's practical consequences imposed via administrative machinery they cannot appeal to on the terms that matter to them (their self-understanding is not the criterion the rule recognizes). The engine computes these as structurally different experiences of the same authored data; this story does not resolve which perspective is 'correct' — that adjudication belongs to the kernel-level contest, not to this single reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females and the institutions administering sex-based criteria sit near the beneficiary end: the reading validates their preferred safety/fairness framework and gives them institutional standing to exclude on sex grounds. Trans women sit near the full-target end: they bear the cost of exclusion with no meaningful exit — abandoning the claim to womanhood is not a genuine alternative from their standpoint, and their exit_options are authored as trapped rather than merely constrained, reflecting that leaving the disputed category is not equivalent to leaving a market or jurisdiction. Institutional agenda-setters (shelters, prisons, sports bodies) are dual-positioned: they benefit from having a workable administrable rule and simultaneously administer its enforcement costs and liability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (physical safety/fairness differentials in forced-proximity or competitive contexts) is authored as contested rather than flatly dead or live: corroborating evidence from correctional-safety and sports-physiology research outside the advocacy coalition supports that the underlying physical differential problem remains empirically real in at least some domains (contact sport, custodial settings), which blocks a simple 'zombie mandate' reading. At the same time, critics dispute that chromosomal/anatomical criteria are the narrowest or least-harmful way to address that problem, which is exactly the locus of the sibling readings' disagreement — this story does not resolve that dispute, only names it via the R5 corroboration field so the mismatch-only consumer can flag it appropriately rather than accepting either side's self-report.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_as_natural_kind_vs_constructed_boundary,
    'Is the chromosomal/developmental-biology criterion a discovery of a pre-existing natural kind that categories should track, or a constructed policy choice among several defensible ways of drawing the woman/female boundary?',
    'This is not fully empirically resolvable — it depends partly on philosophy of biological classification (whether ''sex'' itself is a clean natural kind given intersex variation) and partly on normative theory about what categories are FOR (safety, fairness, recognition, or some combination). Partial resolution: sustained cross-jurisdictional outcome data comparing safety/fairness outcomes under sex-biology vs. hybrid vs. identity-based regimes.',
    'If treated as a discovered natural kind, this reading''s classification is a Mountain and the tangled_rope framing overstates its extractive character. If treated as a constructed policy choice among several live options (as the corroboration field suggests, given genuine dispute even among safety-focused analysts about the narrowest-tailored solution), the tangled_rope framing is the correct read: real coordination function, but also concentrated cost imposed on a non-consenting class via active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biology_as_natural_kind_vs_constructed_boundary, conceptual, 'Whether the sex-biology boundary is a natural-kind discovery or a constructed, contestable policy choice.').

omega_variable(
    sibling_reading_delta_gender_identity,
    'How would the constraint''s structure change if the gender_identity_reading were adopted instead of this reading?',
    'Comparative institutional analysis: under gender_identity_reading, the beneficiary set becomes trans women (and natal females who support self-ID access), the victim set shifts toward natal females who report safety/dignity concerns about mixed-sex facilities, and enforcement mechanisms shift from biological verification (chromosomal/anatomical testing) to self-attestation review. This is a distinct constraint file (gender_identity_reading), not a variant of this one.',
    'Confirms per DP-001 that the two readings are not the same constraint measured differently — they have different ε, different beneficiaries, different victims, and would be authored as separate stories. This omega documents the committer-frame relationship rather than altering this story''s own classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_gender_identity, conceptual, 'Documents the structural delta to the gender_identity_reading sibling constraint.').

omega_variable(
    sibling_reading_delta_hybrid_contextual,
    'How would the constraint''s structure change if the hybrid_contextual_reading were adopted instead of this reading?',
    'Under hybrid_contextual_reading, the criterion this story authors would apply ONLY to medical/sports/safety contexts, while a separate identity-based criterion would govern social/legal recognition contexts. This narrows this reading''s scope of application and correspondingly narrows both its beneficiary reach and its victim set (trans women would retain legal/social recognition even while excluded from a narrower category of sex-segregated safety/sports contexts).',
    'The hybrid reading is expected to show lower ε than this reading on the legal/social recognition axis (that axis is conceded to identity) but potentially similar ε on the safety/sports axis where the criteria converge. This is documented as a sibling relationship, not resolved within this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_hybrid_contextual, conceptual, 'Documents the structural delta to the hybrid_contextual_reading sibling constraint.').

omega_variable(
    intersex_boundary_ambiguity,
    'How does this reading''s chromosomal/gamete-based criterion handle intersex individuals whose biology does not cleanly satisfy either XX/XY or gamete-production tests?',
    'Case-by-case administrative and medical practice under this reading''s own institutions; systematic review of how sports federations and other administering bodies have actually resolved intersex eligibility disputes (e.g., DSD athlete cases) would show whether the criterion is applied as a bright line or with ad hoc exceptions.',
    'If intersex cases are routinely handled by ad hoc exception rather than principled extension of the stated criteria, this suggests the ''biological bright line'' claimed by this reading is less clean in practice than advertised, which would raise the effective accessibility_collapse and suppression figures for that adjacent population — a population not directly modeled as a stakeholder in this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_boundary_ambiguity, empirical, 'Whether the chromosomal/gamete criterion is a clean bright line or requires ad hoc exception-handling for intersex cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__sex_biology_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__sex_biology_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__sex_biology_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(woma_be_t5, woman_female_category__sex_biology_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(woma_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(woma_be_t15, woman_female_category__sex_biology_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(woma_be_t25, woman_female_category__sex_biology_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(woma_su_t5, woman_female_category__sex_biology_reading, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(woma_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(woma_su_t15, woman_female_category__sex_biology_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(woma_su_t25, woman_female_category__sex_biology_reading, suppression_requirement, 25, 0.36).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the woman_female_category kernel. sex_biology_reading (this file) authors ε=0.42 with trans women as the victim class and natal females/sex-based-protection institutions as beneficiaries. gender_identity_reading and hybrid_contextual_reading are separate files with their own ε, beneficiary, and victim declarations per the ε-invariance principle — they are not variants of this file and this file's classification does not average with theirs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
