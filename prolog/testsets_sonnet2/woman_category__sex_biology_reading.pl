% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Sex-Biology Reading of 'Woman' Category (Chromosomal/Anatomical/Reproductive Definition)
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This story authors the sex-biology reading of the 'woman' category
 *   kernel: category membership is determined by chromosomal, anatomical, and
 *   reproductive characteristics, with the typical case (XX chromosomes,
 *   female reproductive anatomy) as the paradigm. This reading has become
 *   substantially more actively litigated and legislated over the past two
 *   decades as gender-identity-based readings gained legal and social
 *   traction; what was once an unstated background assumption has become an
 *   explicitly defended, actively enforced position requiring statute,
 *   litigation, and eligibility-committee rulings to maintain against
 *   contestation. The rising suppression_requirement and extractiveness
 *   trajectory reflects this: the reading now requires active institutional
 *   defense (court rulings, sport federation eligibility panels, legislative
 *   carve-outs) where it previously required none.
 *
 * KEY AGENTS:
 *   - female_athletes_in_sex_segregated_sport: beneficiary of a protected competitive category
 *   - womens_shelter_operators: beneficiary and administrator of admission criteria for single-sex crisis services
 *   - sex_based_data_collection_advocates: beneficiary of a stable statistical variable
 *   - biological_essentialist_advocacy_groups: agenda-setter, drafts and litigates model policy
 *   - transgender_women: primary payer, excluded from sex-segregated protections and legal recognition matching lived identity
 *   - intersex_people_with_ambiguous_classification: secondary payer, poorly served by a 'typical case' framework built around a binary they do not fit
 *   - legislatures_and_courts: institutional observer/adjudicator, determines which reading has legal force in a given domain
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
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Reading of 'Woman' Category (Chromosomal/Anatomical/Reproductive Definition)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'e333431e-0381-476f-9a25-3d9223e77524').
narrative_ontology:cs_kernel_codification('e333431e-0381-476f-9a25-3d9223e77524', distributed).
narrative_ontology:cs_authority_grounding('e333431e-0381-476f-9a25-3d9223e77524', distributed).
narrative_ontology:cs_reading_relation('e333431e-0381-476f-9a25-3d9223e77524', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('e333431e-0381-476f-9a25-3d9223e77524', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('e333431e-0381-476f-9a25-3d9223e77524', foundational, reproductive_biology_is_the_operative_criterion).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_operative_criterion, holdable).
narrative_ontology:cs_axiom_grounding('e333431e-0381-476f-9a25-3d9223e77524', reproductive_biology_is_the_operative_criterion, empirically_contingent).
narrative_ontology:cs_axiom('e333431e-0381-476f-9a25-3d9223e77524', secondary, self_identification_is_not_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_is_not_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('e333431e-0381-476f-9a25-3d9223e77524', self_identification_is_not_sufficient_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('e333431e-0381-476f-9a25-3d9223e77524', unstated_biological_default_pre_dispute).
narrative_ontology:cs_drift_state('e333431e-0381-476f-9a25-3d9223e77524', post_gender_identity_legal_recognition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e333431e-0381-476f-9a25-3d9223e77524', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_athletes_in_sex_segregated_sport).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_data_collection_advocates).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, womens_shelter_operators).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, biological_essentialist_advocacy_groups).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people_with_ambiguous_classification).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, biological_sex_dimorphism_as_regulatory_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compete in categories reserved for people with typical female biology, citing average performance differentials from testosterone-mediated puberty. Benefit from the sex-biology reading being codified into eligibility rules, gaining a protected competitive category. Their exit option if the category dissolved would be competing in a mixed or male-advantaged pool, which most view as effectively foreclosing competitive opportunity.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_athletes_in_sex_segregated_sport, beneficiary,
    moderate, biographical, constrained, national).

% Operate sex-segregated shelters and crisis services for survivors of male violence, using the sex-biology definition to determine admission and staffing. Benefit from a bright-line biological category that they argue is necessary for trauma-informed single-sex space and for collecting sex-disaggregated violence statistics. Administer intake criteria and can set policy but are constrained by funding bodies and litigation risk from either direction.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, womens_shelter_operators, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, womens_shelter_operators, agenda_setter).

% Researchers and policy advocates who argue that maternal health, medical research, crime statistics, and violence-against-women reporting require a stable biological sex variable uncontaminated by self-identification. Benefit from the reading's persistence in statistical and legal categories, and can shift venues (academic publishing, policy advocacy) if any single jurisdiction adopts a different standard.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_data_collection_advocates, beneficiary,
    organized, generational, mobile, national).

% Campaign for legal codification of the sex-biology definition in statute, litigation, and regulatory guidance. Set the agenda by drafting model legislation and funding litigation. Not personally at stake in the outcome the way affected individuals are — their exit option (shifting jurisdictions or campaigns) is comparatively unconstrained.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, biological_essentialist_advocacy_groups, agenda_setter,
    organized, generational, mobile, national).

% Live as women in social, legal, and often medical respects but are excluded from sex-segregated categories, protections, and services under this reading because they do not have XX chromosomes and typical female reproductive anatomy. Bear exclusion from sport, shelters, and legal recognition as women; cannot exit the classification because their identity is not a preference but a lived, often medically affirmed, self-understanding. The category assignment directly determines their access to spaces core to daily functioning.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, national).

% Have variations in sex characteristics (chromosomal, gonadal, or anatomical) that do not cleanly fit the 'typical case' the reading is built around. Classification decisions are often made by institutions (sport federations, medical boards) without their input, and outcomes vary unpredictably by context. They have essentially no exit — their bodies exist prior to and independent of the classification dispute, yet the dispute's resolution directly governs their access to categories.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people_with_ambiguous_classification, payer,
    powerless, biographical, trapped, national).

% Adjudicate disputes over which reading of 'woman' governs a given statute (sports eligibility law, anti-discrimination law, prison placement, shelter funding conditions). Hear testimony and evidence from all sides and can shift the legally operative definition, which changes which reading has state backing in a given domain.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable, low-ambiguity criterion for allocating access to sex-segregated spaces, competitive categories, and statistical categories where biological differences (average physical performance, reproductive health risk, patterns of sex-based violence) are argued to be materially relevant to the purpose of the segregation.
% TRANSFER_FUNCTION: Allocates access to competitive sport categories, shelter and crisis-service admission, and legal/statistical categorization according to chromosomal and anatomical criteria; moves the benefit of a protected/legible category to those who meet the biological criterion and moves the cost of exclusion (from sport, from single-sex spaces, from legal recognition matching lived identity) onto transgender women and ambiguously onto intersex people.
% ABSENT_VOICES: Transgender women and intersex people are frequently not the ones drafting the eligibility criteria or shelter admission policies that govern them; medical and sporting bodies often set criteria in consultation with biological-essentialist advocacy groups and sex-based-data advocates without comparable structural input from those excluded. Where present, their testimony is often treated as advocacy rather than as expertise on lived classification harms.
% DISAPPEARANCE_RATIONALE: If the sex-biology reading were displaced entirely (e.g., by uniform adoption of self-identification as the legal standard), sex-segregated sport categories, shelter admission criteria, and sex-disaggregated statistical collection would need to be rebuilt on a different basis; conversely, if this reading is the one that is entrenched, its removal would immediately reopen currently-settled eligibility disputes in sport and single-sex services, showing that real arrangements depend on which reading holds.
% FOUNDING_PROBLEM: Prior to any explicit definitional dispute, sex-segregated categories in sport, health data, and violence-response services were built on an unstated assumption that 'woman' tracked biological sex; the sex-biology reading makes that assumption explicit and defends it once the assumption was contested by advocates for gender-identity-based recognition.
% FOUNDING_PROBLEM_CORROBORATION: Sport governing bodies and shelter-sector researchers (outside the advocacy groups that campaign for the reading) corroborate that average post-pubertal physical differences and patterns of male-perpetrated violence are real phenomena the categories were built to track; however, human-rights bodies and some medical associations, also outside the direct beneficiary set, corroborate that the same underlying phenomena can be addressed via case-specific or hormonal criteria rather than a categorical chromosomal rule, so the 'is the founding problem still best solved this way' question remains genuinely contested by outside parties on both sides.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the reading materially and predictably excludes an identifiable group (transgender women) from protections and categories central to social participation, and treats intersex people's actual bodies as an edge case to the 'typical case' framing rather than as a first-class category — this is real cost imposed on real people, not merely a definitional preference. Suppression (0.52) is moderate: enforcement now runs through statute, sport-federation eligibility panels, and litigation, but genuine countervailing legal and social pressure (from the sibling readings) prevents suppression from being total — the category is actively contested, not settled by force alone. Accessibility collapse is moderate-low (0.4) because sibling readings remain live, litigated, and adopted in some jurisdictions — alternatives to this reading are visibly available, not foreclosed. Resistance is high (0.75) precisely because this reading is the most actively challenged of the three siblings in current discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats this reading functions as protective coordination — a necessary bright line defending a legible, defensible resource allocation. From the payer seats (transgender women, intersex people) the identical structure functions as exclusionary extraction: a rule that determines access to core social participation, applied to their bodies or identities without their meaningful input, defended increasingly by active institutional enforcement rather than by unquestioned consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (athletes in the protected category, shelter operators, data advocates, essentialist advocacy groups) hold roles that either collect a protected resource (the category itself) or administer/campaign for the definitional rule with mobile exit options — none are trapped by the classification the way payers are. Transgender women are identity_locked: their exclusion from the category is not a preference they could exit by choosing differently, it is a structural consequence of a biological criterion applied to a lived identity that does not vary with legal reclassification pressure. Intersex people are trapped rather than identity_locked: unlike transgender women, whose exclusion follows from identity, intersex people's exclusion or ambiguous inclusion follows from bodies that predate and are independent of any identity claim, and institutions frequently resolve their cases without consulting them at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview shows genuine contest, not simple mandatrophy: the underlying phenomena the reading was built to track (average post-pubertal physical differences, patterns of male-perpetrated violence) are corroborated as real by observers outside the direct beneficiary set, which prevents a straightforward 'dead mandate propped up by inertia' reading. But the corroboration also shows the resolution mechanism itself is contested — outside parties on the other side argue the same phenomena could be addressed by case-specific or hormonal criteria rather than a categorical chromosomal rule. This story does not resolve that contest; it authors ONE reading's structure honestly, which is why the claimed type (tangled_rope) sits with real coordination function (sport fairness, service-provision logic, epidemiological data) alongside a named, non-trivial victim set experiencing exclusion as extraction, both simultaneously true of this specific reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_functional_criterion_for_segregation,
    'Is chromosomal/anatomical sex the ethically and functionally correct criterion for the purposes sex-segregated categories actually serve (competitive fairness, trauma-informed care, epidemiological accuracy), or would a functional criterion (e.g., circulating testosterone levels, time since transition, specific medical history) better track the same underlying purposes while including transgender women and clarifying intersex cases?',
    'Longitudinal sport-performance studies comparing outcomes under chromosomal versus hormonal eligibility criteria; trauma-informed-care outcome studies comparing shelter models with different admission criteria; direct comparison of predictive validity between biological-sex and self-identification variables in violence and health statistics.',
    'If functional criteria track the underlying purposes as well as or better than the chromosomal criterion, this reading''s exclusionary structure is extraction disguised as a coordination necessity — the coordination function could be preserved with a different, less exclusionary criterion, meaning ε for this reading is higher than the coordination story alone would justify. If chromosomal criteria are shown to be uniquely predictive and functional substitutes fail, the coordination function is closer to load-bearing and the reading''s extraction is closer to an unavoidable byproduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_vs_functional_criterion_for_segregation, empirical, 'Whether the reading''s exclusionary criterion is functionally necessary to its stated coordination purposes or a stricter-than-needed proxy.').

omega_variable(
    kernel_reading_relative_naturalness,
    'Which reading of the woman_category kernel represents the ''default'' or ''natural'' framing against which the others are measured as departures — or is there no defensible default and all three readings are equally constructed responses to genuine, partially incompatible normative and empirical considerations?',
    'This is a conceptual/normative question not resolvable by data alone; it depends on background commitments about whether legal categories should track biological classification, psychological self-understanding, or spectrum-inclusive biological reality, and on which purpose (sport fairness, trauma care, legal recognition, epidemiology) is treated as primary when they conflict.',
    'If the sex-biology reading is treated as the default/natural baseline, sibling readings appear as revisions requiring justification, which shifts legitimacy burden and litigation posture toward gender-identity and intersex-accommodation readings. If no reading is treated as default, all three compete on equal normative footing and the classification here (tangled_rope, with a real coordination function and a real victim set) should be read as one contested reading among structurally co-equal alternatives, not as the ground truth the siblings deviate from.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relative_naturalness, conceptual, 'Whether this reading should be treated as the baseline framing of the kernel or as one contested option among structurally co-equal readings.').

omega_variable(
    intersex_inclusion_ambiguity_within_reading,
    'Within the sex-biology reading itself, are intersex people with atypical chromosomal/anatomical configurations included in ''woman'' when their configuration is female-typical in relevant respects, or does the ''typical case'' framing effectively exclude or destabilize their categorization regardless of which specific variation they have?',
    'Case-law and policy analysis of how sport federations and legal bodies applying a chromosomal/anatomical standard have actually resolved specific intersex cases (e.g., XY DSD conditions with female phenotype, androgen insensitivity syndrome cases) to see whether resolution is principled and consistent or ad hoc and outcome-driven.',
    'If intersex cases are resolved inconsistently or in a manner driven by convenience (e.g., barring an intersex athlete whenever performance is a concern regardless of the specific mechanism), the ambiguity documented in the structural delta is not incidental but is a second, distinct extraction channel within this reading, independent of the transgender exclusion question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity_within_reading, empirical, 'Whether intersex people are treated consistently under this reading''s own stated logic or are excluded/included opportunistically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_category__sex_biology_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(woma_tr_t8, woman_category__sex_biology_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(woma_tr_t12, woman_category__sex_biology_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(woma_tr_t16, woman_category__sex_biology_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(woma_tr_t24, woman_category__sex_biology_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t4, woman_category__sex_biology_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(woma_be_t8, woman_category__sex_biology_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(woma_be_t12, woman_category__sex_biology_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(woma_be_t16, woman_category__sex_biology_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(woma_be_t24, woman_category__sex_biology_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_category__sex_biology_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(woma_su_t8, woman_category__sex_biology_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(woma_su_t12, woman_category__sex_biology_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(woma_su_t16, woman_category__sex_biology_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(woma_su_t24, woman_category__sex_biology_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the woman_category kernel. gender_identity_reading and intersex_accommodation_reading are separate constraint stories with their own ε values, beneficiary/victim sets, and claimed types; they are not alternative measurements of this same constraint. All three should be treated as a constraint family for contamination-propagation purposes: a legal or institutional shift that increases enforcement pressure on one reading structurally decreases the operative scope of the others in the same jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
