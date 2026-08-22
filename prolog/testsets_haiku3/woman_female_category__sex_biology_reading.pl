% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Based Category Membership: Biological Female Determination
 *   domain: political_philosophy/bioethics/gender_studies
 *
 * SUMMARY:
 *   This constraint operationalizes one reading of the contested kernel
 *   'woman'/'female' category: membership is determined by chromosomal sex
 *   (XX), reproductive anatomy (ovaries, uterus, vagina), and
 *   sex-hormone-driven development (estrogen-dominant puberty, absence of
 *   testis-derived androgens during fetal development). Under this reading,
 *   the category boundary is objective, verifiable, and stable across
 *   lifespan and context. The constraint produces real institutional effects:
 *   trans women are excluded from female-designated prisons, shelters,
 *   sports, bathrooms, and intimate-care settings; trans men are retained in
 *   female spaces despite male identity; non-binary individuals are forced
 *   into binary classification. The reading frames sex-based protections (for
 *   natal females vulnerable to male-pattern violence) as a legitimate
 *   beneficiary function and the exclusion of trans women as a necessary cost
 *   of maintaining that protection. This is ONE reading of the kernel, not
 *   the reading; sibling readings (gender_identity_reading,
 *   hybrid_contextual_reading) instantiate competing constraints with
 *   different victim sets, beneficiary structures, and ε values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.62).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.71).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Based Category Membership: Biological Female Determination").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '0540d713-e43b-4eda-b5be-a5497801cbe1').
narrative_ontology:cs_kernel_codification('0540d713-e43b-4eda-b5be-a5497801cbe1', formalized).
narrative_ontology:cs_authority_grounding('0540d713-e43b-4eda-b5be-a5497801cbe1', lineage).
narrative_ontology:cs_interpretation_layer_present('0540d713-e43b-4eda-b5be-a5497801cbe1').
narrative_ontology:cs_reading_relation('0540d713-e43b-4eda-b5be-a5497801cbe1', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('0540d713-e43b-4eda-b5be-a5497801cbe1', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('0540d713-e43b-4eda-b5be-a5497801cbe1', foundational, chromosomal_reproductive_anatomy_determines_category).
narrative_ontology:cs_axiom_status(chromosomal_reproductive_anatomy_determines_category, holdable).
narrative_ontology:cs_axiom_grounding('0540d713-e43b-4eda-b5be-a5497801cbe1', chromosomal_reproductive_anatomy_determines_category, empirically_contingent).
narrative_ontology:cs_axiom('0540d713-e43b-4eda-b5be-a5497801cbe1', secondary, sex_based_institutional_segregation_justified_by_vulnerability_asymmetry).
narrative_ontology:cs_axiom_status(sex_based_institutional_segregation_justified_by_vulnerability_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('0540d713-e43b-4eda-b5be-a5497801cbe1', sex_based_institutional_segregation_justified_by_vulnerability_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('0540d713-e43b-4eda-b5be-a5497801cbe1', biological_sex_as_category_referent).
narrative_ontology:cs_drift_state('0540d713-e43b-4eda-b5be-a5497801cbe1', contemporary_trans_rights_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0540d713-e43b-4eda-b5be-a5497801cbe1', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_safety_advocates).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, non_binary_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek sex-based protections in contexts where reproductive anatomy or chromosomal sex determines vulnerability patterns: prison housing, domestic violence shelters, sexual assault support services, intimate-search procedures in law enforcement. They frame biological sex as an irreducible category for safety risk assessment and argue that erosion of sex-based boundaries undermines their claim on institutional resources and protection protocols built on sex-segregation principles.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear exclusion from female-designated spaces (prisons, shelters, bathrooms, sports, healthcare settings) under a regime that makes female category membership conditional on natal sex characteristics. They cannot exit the constraint without abandoning their gender identity claim or migration, and their exclusion from sex-segregated safety/recovery resources creates concrete harm: denial of appropriate facility placement, institutional coercion to inhabit spaces that invalidate their gender identity, absence of trauma-informed shelter access for trans women survivors of gender-based violence.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Face categorical exclusion or forced misclassification where institutional space assignment requires binary sex coding (M/F checkboxes). They are treated as either male or female regardless of their actual gender identity or biological characteristics, with no institutional accommodation for non-binary positioning. The constraint renders their gender identity administratively invisible and compels binary assignment against their self-understanding.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, non_binary_individuals, payer,
    powerless, biographical, identity_locked, national).

% Administer sex-segregated institutions (prisons, shelters, sports bodies, military units) and face regulatory pressure from two opposed constituencies: demands for sex-based boundaries from natal females and civil rights advocates, and demands for gender-identity-based inclusion from trans advocates. They set the rules that determine who belongs in female-designated spaces, with ongoing litigation and legislative contestation. Their constraint stems from their regulatory authority to define institutional categories, not from any direct stake in the category boundary itself.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, institutional_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Apply biological sex determination in clinical contexts (reproductive health, bone density screening, pharmacokinetics, disease prevalence patterns). They observe that chromosomal sex and anatomy carry medical relevance in some contexts but that gender identity carries psychological and social relevance in others. They are positioned as technical experts but their authority is increasingly contested by both natal-female advocates and trans advocates claiming to speak for medical truth.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_practitioners, observer,
    institutional, biographical, constrained, national).

% Retain female classification under a sex-biology regime despite male gender identity. They are often retained in female-designated spaces despite identifying as men, creating dysphoria and safety risks where sex-segregation is justified by sex-based vulnerability patterns that do not apply uniformly to trans men. Their situation inverts the trans-woman exclusion but produces parallel institutional invalidation of gender identity.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_men, observer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, trans_men, payer).

% Interpret the sex-category boundary through competing human-rights frameworks: some frame sex-based protections as necessary to prevent male-pattern violence; others frame sex-category exclusion of trans women as a civil rights violation. They produce competing legal theories, litigation strategies, and policy advocacy, making the category boundary a focal point for competing rights claims.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, civil_rights_organizations, observer,
    organized, generational, mobile, national).

% Legislate and enforce the legal definition of 'woman' or 'female' in statutes, regulations, and court doctrine. They face political pressure from multiple constituencies and court challenges over whether sex-based category membership should rest on biology, identity, or context. Their regulatory choices determine whether the sex-biology reading is institutionalized or undermined by statutory redefinition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates sex-segregated institutional space (prison housing, shelters, bathrooms, intimate-care settings) using biological sex as a verifiable, stable proxy for vulnerability patterns: female vulnerability to male-pattern sexual violence, requiring segregation to reduce institutional sexual assault. The coordination claim is that biological sex-based sorting reduces the institutional burden of case-by-case vulnerability assessment and makes protection policies predictable and scalable.
% TRANSFER_FUNCTION: Allocates access to female-designated institutional resources and protection protocols to those with XX chromosomes, reproductive anatomy, and sex-hormone-typical development; excludes or reroutes those classified as male at birth despite gender identity claims. The transfer is institutional space, institutional recognition of category membership, and access to sex-segregated protection services. The cost is borne by trans women (forced into male housing and spaces despite identity) and non-binary individuals (forced binary classification).
% ABSENT_VOICES: Trans women survivors of sexual violence who need shelter are absent from shelter policy design; their testimony is routed through civil rights organizations rather than incorporated into institutional decision-making. Trans men's safety needs in female spaces are rarely centered in policy debates. Institutional staff who manage mixed-gender housing and report that identity-based approaches work (or fail) are absent from legislative debates dominated by abstract category disputes.
% DISAPPEARANCE_RATIONALE: Removal of the sex-biology reading would require institutional redesign: prison housing would shift to gender-identity-based assignment or individualized-assessment protocols; shelter intake would change; sports competition would reorganize; legal status categories would be rewritten. Medical institutions might retain sex-based screening for reproductive/hormonal health but would decouple identity from institutional space assignment. The world would not accommodate this constraint's disappearance; it would restructure around an alternative category boundary.
% FOUNDING_PROBLEM: Male-pattern sexual violence in sex-mixed institutional settings (prisons, shelters, military units). Early institutional sex-segregation was premised on the vulnerability asymmetry: individuals with female reproductive anatomy and socialization face distinct sexual-assault risk from individuals with male socialization and reproductive anatomy. Sex-based segregation reduced institutional assault caseload relative to unsegregated approaches.
% FOUNDING_PROBLEM_CORROBORATION: Domestic violence practitioners, sexual assault survivors, and prison reform advocates testify that sex-based segregation remains necessary to prevent male-perpetrated sexual violence in shelters and prisons (external corroboration: epidemiological data on assault rates in sex-segregated vs. mixed facilities shows segregation correlates with lower assault rates, though causality is contested). Trans advocates and civil rights organizations testify that the founding problem has been substantially mitigated by institutional design improvements (better staff training, segregated units for vulnerable populations, grievance procedures) and that sex-category-based solutions now extract more than they coordinate (external corroboration: reports from jurisdictions using gender-identity-based housing show comparable or improved safety outcomes; trans women in female facilities report lower assault risk than trans women in male facilities). Medical practitioners confirm male-pattern violence exists as an aggregate-level phenomenon but contest whether sex categories capture it precisely enough for institutional policy (some trans women pose no elevated assault risk; some cis men in shelters for homeless also face sexual assault risk; mixed-gender housing with improved oversight achieves similar outcomes to segregation in some populations).
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the reading's core feature: coordination function (sex-segregation reduces institutional vulnerability assessment burden) is genuine, but asymmetric: natal females benefit from a stable category they occupy by default; trans women and non-binary individuals bear exclusion cost with no offsetting benefit. The constraint is Tangled Rope rather than Snare because a real coordination problem (institutional vulnerability sorting) motivated it; it became extractive when the category boundary solidified around biology despite growing evidence that gender identity matters for some institutional decisions (e.g., trans women's relative safety risk in prison is empirically contested; some studies show trans women pose no higher threat than cis women; others suggest case-by-case assessment is needed). Suppression is high (0.71) because institutional enforcement is active: intake procedures require biological sex documentation; trans women's petitions for female housing face legal barriers; judicial deference to biological-sex criteria in sports and intimate-care policies persists despite trans advocacy. Theater is low-moderate (0.28): the institutional machinery genuinely sorts people by biology in many cases, but an increasing share of maintenance activity (litigation, legislative debate, policy justification) defends the category boundary against trans-identity claims rather than against actual vulnerability. Accessibility collapse is high (0.78): once someone is assigned male sex at birth based on genital anatomy and chromosomal test, the category assignment is nearly irreversible; alternatives (self-identification, gender-affirming legal change) are actively suppressed by law, policy, and institutional practice in many jurisdictions. Resistance is high (0.82): trans advocates, civil rights organizations, and some medical professionals mount active resistance to the sex-biology reading through litigation, legislation, institutional policy change, and cultural argument — no quiet acceptance. Measurement series shows extractiveness and suppression rising over the interval as political contestation intensifies and institutional enforcement hardens in response to trans-rights advocacy: as legal/social pressure mounted, institutions doubled down on sex-documentation requirements rather than opening to gender-identity-based admission.
 *
 * PERSPECTIVAL GAP:
 *   The natal-female-beneficiary seat and the trans-woman-target seat experience radically different types: from the natal-female perspective, the constraint is legitimate coordination (biological sex is an objective, stable basis for vulnerability sorting and protection allocation; the engine should compute Rope or Tangled Rope with low extractiveness). From the trans-woman perspective, the constraint is coercive extraction (identity invalidation, forced segregation, denial of access to appropriate institutional support; the engine should compute Snare or high-extractiveness Tangled Rope). Both perspectives are consistent with the authored metrics and structural data: a genuine coordination function does exist (sex-based vulnerability patterns), but it is being leveraged by the institutional regime to extract identity compliance from trans women. The per-seat computation will show this gap: beneficiary seats compute lower extractiveness/higher coordination weight; target seats compute high extractiveness/lower coordination weight. This gap IS the substantive finding the constraint story exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections sit near the beneficiary end (d ~ 0.2–0.3): the constraint allocates resources to them, validates their vulnerability claim, and requires no identity-abandonment for institutional access. Trans women sit near the full-target end (d ~ 0.85–0.95): they are excluded from female spaces despite identifying as female, are forced into male housing in prisons/shelters, and face institutional invalidation of their gender identity; their exit would require abandoning either their gender identity (identity_locked exit is trapped) or migrating to jurisdictions with different category boundaries (mobile exit exists but is socially/economically costly). Non-binary individuals sit near targets (d ~ 0.80): forced binary classification with no institutional accommodation. Institutional administrators occupy intermediate d (~0.5): they administer the constraint and face pressure from both sides but do not directly collect from the arrangement — they are structurally symmetric between conflicting stakeholder demands. Trans men face a peculiar position: they occupy the female category by virtue of natal sex but are harmed by retention in female spaces (dysphoria, safety risks where sex-segregation is premised on female vulnerability); their d is ambiguous (~ 0.6): they benefit from institutional inclusion but are harmed by category misalignment with identity. No directionality overrides are needed; the structural derivation from beneficiary/victim/exit captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (male-pattern violence risk in institutional settings) is live and real, but contested. Natal females attest the problem persists; trans women and civil rights advocates attest the founding problem has been substantially solved by institutional design improvements (better staff training, monitoring, grievance procedures, trauma-informed care) and is no longer the primary driver of sex-segregation policy — the boundary now persists partly by institutional inertia and partly by active defense against trans-rights claims. The measurement series supports the contested reading: as institutional alternatives emerged (gender-responsive housing pilots, segregated trans units, individualized assessment protocols), the sex-biology boundary did NOT relax in response; instead, institutional suppression hardened and theater rose. This suggests the constraint has partially transitioned from coordination (solving the founding problem) to extraction (defending a category boundary against identity claims). The Tangled Rope classification captures this: a real coordination function (vulnerability sorting) remains, but it is now nested inside an asymmetric extraction mechanism (trans women's forced exclusion and identity invalidation). No mandatrophy flag is warranted yet — the founding problem is contested-live, not dead — but the hardening suppression and rising theater over the interval suggest monitoring for mandatrophy emergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_stability_and_relevance,
    'Across what set of institutional decisions is biological sex (XX/XY chromosomes, reproductive anatomy, sex-hormone development) a legitimate basis for differential treatment, and across what set is it not?',
    'Systematic empirical study of institutional outcomes (prison safety, shelter outcomes, sports fairness, medical care quality) segregated by sex-based vs. gender-identity-based vs. individualized-assessment grouping regimes; comparison of jurisdictions with different category boundaries; meta-analysis of sex-based risk and capability differences across contexts.',
    'A narrow empirical set (e.g., reproductive medicine and prison housing only) would reclassify the constraint as extractive in non-foundational contexts; a broad empirical set would support the reading''s coordination claim across domains. The reading''s viability depends on establishing that sex-biology sorting produces net beneficial outcomes relative to alternatives in the institutional contexts where it is enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_stability_and_relevance, empirical, 'Scope of legitimate sex-based institutional differentiation.').

omega_variable(
    category_boundary_vs_identity_invalidation_decoupling,
    'Can institutional sex-segregation for protection purposes be maintained WITHOUT actively invalidating trans women''s gender identity claims through law, policy, and cultural reinforcement?',
    'Natural experiments: jurisdictions that permit legal gender recognition for trans women while maintaining sex-segregated institutional policies (female housing for legal females by choice but sex-typical body assignment by admin function); pilot programs with explicit identity-affirming language in sex-segregated policies; comparative analysis of outcomes where identity validation is decoupled from category access.',
    'If decoupling is empirically possible, the constraint''s extractiveness is not intrinsic to sex-based coordination but is instead a choice by institutional actors to leverage category boundaries for identity suppression. Reclassification would shift from Tangled Rope (necessary extraction embedded in coordination) to Snare (extraction separated from coordination function by institutional choice). If decoupling produces worse institutional outcomes, the identity invalidation is revealed as necessary to the coordination, pushing the reading back toward justified extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_boundary_vs_identity_invalidation_decoupling, empirical, 'Whether sex-segregation and trans-identity validity are structurally decoupled or entangled.').

omega_variable(
    reading_logical_structure_foreclosure,
    'Do the sex_biology_reading and gender_identity_reading logically foreclose each other within a single institutional framework, or can they coexist through context-switching?',
    'Formal analysis of the two readings'' core premises: if one reading''s law of category membership is true, is the sibling reading''s law necessarily false? Or do they describe different aspects of the same entity (e.g., chromosomal sex describes physiology; gender identity describes psychology) that could both be true simultaneously? Can an institution simultaneously recognize both facts without contradiction (sex-based medical screening, gender-identity-based social recognition)?',
    'If the readings foreclose each other, this constraint''s type is fixed independent of empirical outcomes — it is one pole of a binary choice about category definition. If they coexist (context-dependent switching is logically possible), the reading''s extraction rises because the sex-biology boundary is not justified by logical necessity but by institutional choice to exclude alternatives. This omega determines whether to classify the reading''s foreclosure relation to siblings as ''forecloses'' or ''coexists_with'' in the cs_structure.reading_relations block.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_structure_foreclosure, conceptual, 'Logical relationship of competing category definitions.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (external barriers, legal exclusion, institutional policy) or internalized (trans women''s acceptance of male classification, identity fusion with male role despite gender identity claim)?',
    'Post-exit suppression trajectory: if institutional barriers were removed (trans women legally permitted to access female facilities, gender-affirming identity recognition), would suppression persist? If suppression collapses, it was structural; if it persists among previously excluded individuals, the constraint successfully internalized identity invalidation. Qualitative interviews with trans women about identity claims post-institutional-exit; comparison of identity stability across jurisdictions with different institutional regimes.',
    'If suppression is primarily structural, barrier removal might resolve the constraint without deep identity reformation. If suppression is substantially internalized, the constraint has embedded identity-invalidation into its targets'' self-concept, requiring longer therapeutic/cultural recovery after exit. Higher internalized proportion would indicate more severe extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Proportion of suppression that is structural vs. internalized.').

omega_variable(
    kernel_contest_committer_frame,
    'Which reading of the woman/female category kernel is the ''true'' reading, and by what epistemology should truth be adjudicated?',
    'This omega has no technical resolution — it is a preference/values question routed through political and philosophical contestation. It remains permanently open in the framework. The three readings (sex_biology, gender_identity, hybrid_contextual) cannot all be simultaneously instantiated in one institutional framework; one must be chosen. The choice is not empirically determined but politically negotiated.',
    'The framework treats this as open because the three readings are incommensurable: they rest on different axioms about what grounds category membership (biology, phenomenology, context-dependence) and no empirical fact adjudicates between axiom choices. Different jurisdictions, institutions, and communities will author different ε values for different readings of the same kernel based on which reading they adopt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_committer_frame, preference, 'Meta-question: which reading represents the ''true'' category boundary?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__sex_biology_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__sex_biology_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(woma_be_t5, woman_female_category__sex_biology_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(woma_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(woma_be_t15, woman_female_category__sex_biology_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(woma_su_t5, woman_female_category__sex_biology_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(woma_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(woma_su_t15, woman_female_category__sex_biology_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel woman_female_category. The readings decompose into separate constraints because their ε values, victim sets, and beneficiary structures differ substantially (ε-invariance principle). The sex_biology_reading and gender_identity_reading foreclose each other logically within a single institutional framework; the hybrid_contextual_reading attempts to coexist through context-switching but faces feasibility constraints. All three readings link to each other via network.affects_constraints because institutional choice of one reading directly determines the applicability and configuration of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
