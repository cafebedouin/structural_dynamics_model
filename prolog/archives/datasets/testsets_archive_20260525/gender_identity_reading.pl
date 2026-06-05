% ============================================================================
% CONSTRAINT STORY: gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gender_identity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gender_identity_reading
 *   human_readable: Woman/Female Defined by Gender Identity Self-Identification
 *   domain: social_ontology/medical_classification/rights_frameworks
 *
 * SUMMARY:
 *   The definition of 'woman' as grounded in gender identity
 *   (self-identification as central to personhood) represents one reading of
 *   a contested kernel — the category 'woman/female' itself. This reading
 *   instantiates a specific structural constraint with distinct beneficiaries
 *   (transgender women, gender-nonconforming persons, self-identification
 *   advocates) and victims (sex-based rights claimants, biological sex
 *   category maintainers, feminist sex-based analysis frameworks). The
 *   constraint exhibits tangled coordination and extraction: it solves a
 *   genuine collective action problem (enabling those with non-normative
 *   gender identity to achieve social recognition and access) while
 *   simultaneously extracting from those whose rights, medical protocols, or
 *   analytical frameworks depend on sex-based categorization. The theater
 *   ratio (0.41) reflects that much institutional activity around this
 *   definition involves performative commitment rather than functional
 *   redefinition — institutions declare commitment to self-identification
 *   while maintaining underlying biological sex tracking for medical and
 *   statistical purposes. The suppression metric (0.52) captures
 *   institutional pressure on competing definitional frameworks and on agents
 *   who maintain sex-based analysis. This is not the only reading of the
 *   'woman' category — the sex-biology reading and intersectional-coexistence
 *   reading are live alternative institutional positions — but it is the one
 *   this constraint story instantiates.
 *
 * KEY AGENTS:
 *   - Transgender Women and Self-ID Advocates: Primary beneficiaries (institutional/arbitrage) — gain access to legal recognition, spaces, and protections by expanding category 'woman' to include self-identification
 *   - Sex-Based Rights Claimants: Primary victims (powerless/trapped) — lose definitional security of the sex-based category that grounds claims to sex-specific protections (reproductive health, domestic violence services)
 *   - Biological Sex Category Maintainers: Secondary victims (moderate/constrained) — face institutional pressure and delegitimation for maintaining sex-based medical and administrative systems; constrained by eroding institutional support
 *   - Feminist Sex-Based Rights Movements: Organized victims (organized/constrained) — provide genuine coordination function (sex-based analysis of patriarchal oppression) while experiencing suppression through delegitimation and institutional pressure
 *   - Traditional Medical Ontology: Institutional actor (institutional/arbitrage) — maintains sex-based protocols through functional necessity while experiencing performative pressure to subordinate biological sex to gender identity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent definitional choice (gender identity as the essential criterion) as an immutable feature of personhood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_identity_reading, 0.38).
domain_priors:suppression_score(gender_identity_reading, 0.52).
domain_priors:theater_ratio(gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_identity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gender_identity_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gender_identity_reading, "Woman/Female Defined by Gender Identity Self-Identification").
narrative_ontology:topic_domain(gender_identity_reading, "social_ontology/medical_classification/rights_frameworks").

domain_priors:requires_active_enforcement(gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(gender_identity_reading, fixed_text).
narrative_ontology:cs_authority_grounding(gender_identity_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(gender_identity_reading).
narrative_ontology:cs_kernel_id(gender_identity_reading, woman_female_category).
narrative_ontology:cs_reading_relation(gender_identity_reading, sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation(gender_identity_reading, intersectional_coexistence_reading, influences).
narrative_ontology:cs_axiom(gender_identity_reading, foundational, gender_identity_constitutive_of_personhood).
narrative_ontology:cs_axiom_status(gender_identity_constitutive_of_personhood, holdable).
narrative_ontology:cs_axiom_grounding(gender_identity_reading, gender_identity_constitutive_of_personhood, deontological).
narrative_ontology:cs_axiom(gender_identity_reading, foundational, self_identification_boundary_criterion).
narrative_ontology:cs_axiom_status(self_identification_boundary_criterion, holdable).
narrative_ontology:cs_axiom_grounding(gender_identity_reading, self_identification_boundary_criterion, conventional).
narrative_ontology:cs_reference_frame(gender_identity_reading, identity_first_personhood).
narrative_ontology:cs_drift_state(gender_identity_reading, contemporary_gender_recognition_movement, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(gender_identity_reading, gender_nonconforming_persons).
narrative_ontology:constraint_beneficiary(gender_identity_reading, self_identification_advocates).
narrative_ontology:constraint_victim(gender_identity_reading, sex_based_rights_claimants).
narrative_ontology:constraint_victim(gender_identity_reading, biological_sex_boundary_enforcers).
narrative_ontology:constraint_victim(gender_identity_reading, female_sex_category_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-BASED RIGHTS CLAIMANTS (SNARE) — Women claiming legal protections rooted in biological sex (reproductive health access, domestic violence shelters, single-sex spaces) face erosion of those protections as the category 'woman' decouples from biological sex. Trapped: cannot exit the administrative category that defines their access to sex-specific services. Bears extraction through redefinition of the category that grounds their rights claims.
constraint_indexing:constraint_classification(gender_identity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BIOLOGICAL SEX CATEGORY MAINTAINERS (TANGLED_ROPE) — Medical professionals, sports regulators, and policy makers attempting to maintain sex-based classification systems for health or fairness purposes face both genuine coordination benefits (sex-based medicine requires biological distinction for treatment protocols) and asymmetric extraction (forced to absorb administrative burden of defending biological categories as their institutional legitimacy erodes). Constrained by institutional inertia and legal/professional pressure; some coordination function genuine.
constraint_indexing:constraint_classification(gender_identity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSGENDER WOMEN AND SELF-ID ADVOCATES (ROPE) — Primary beneficiaries. Self-identification as the defining criterion for the category 'woman' expands access to spaces, legal protections, and social recognition. Institutional and legal support grows for this reading. Experience the constraint as pure coordination: enabling the social category to match internal identity resolves a collective action problem (misalignment of identity and recognition). Low suppression from this position; high mobility and institutional backing.
constraint_indexing:constraint_classification(gender_identity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL MEDICAL ONTOLOGY (PITON) — The sex-based biological classification system in medicine (used for hormonal treatment, reproductive health, disease epidemiology, surgical protocols) persists through institutional inertia despite theoretical challenge from gender-identity-first framing. The traditional system still functions (clinical outcomes depend on biological sex differentiation) but is increasingly treated as theater — as if the categories are purely social constructs rather than medically salient. Theater ratio high because the performance of defending 'sex is real' in medical contexts coexists with actual clinical reliance on sex-based protocols. Piton classification derives from degraded institutional legitimacy, not from high extraction.
constraint_indexing:constraint_classification(gender_identity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FEMINIST SEX-BASED RIGHTS MOVEMENTS (TANGLED_ROPE) — Organized agents claiming that sex-based categories are necessary for addressing gendered subordination face extraction through institutional pressure and social stigmatization (labeled transphobic for maintaining sex-based analysis) while providing genuine coordination function (analysis of patriarchal systems requires sex-based framework). Constrained by institutional power imbalances; high suppression through delegitimation. Both coordination (sex-based oppression analysis) and extraction (delegitimation of their theoretical framework) present.
constraint_indexing:constraint_classification(gender_identity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, this perspective risks classifying biological sex as a natural law: 'the category woman is grounded in immutable biological fact; self-identification cannot redefine biological categories.' However, the structural data reveals this as a false summit: the claim that 'woman' has a single, immutable, biologically-determined essence is itself a contested reading, not a discovered natural law. The categorical structure is under legal and social control, not biological determination alone.
constraint_indexing:constraint_classification(gender_identity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_identity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_identity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_identity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_identity_reading, TR),
    TR >= 0.70.

:- end_tests(gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The gender-identity reading benefits transgender women and self-ID advocates by expanding access to the 'woman' category, but the extraction imposed on sex-based rights claimants is real. The institutional advantage flows to those with power to redefine categories (self-ID advocates with institutional backing) while costs flow to those whose claims depend on the old boundary (sex-based rights frameworks). The extractiveness is not total because some institutional dual-tracking preserves elements of sex-based protections, and because the reading provides genuine coordination benefit (enabling identity-recognition alignment). Suppression (0.52): Moderate-high. Institutional enforcement of gender-identity-based definition includes pressure against competing frameworks (sex-based analysis increasingly classified as 'transphobic'), but suppression is not absolute — sex-based rights movements, biological sex categories, and feminist analysis remain live institutional positions. The suppression operates primarily through delegitimation and institutional policy rather than legal prohibition. Theater ratio (0.41): Moderate. Significant institutional activity is performative — declarations of commitment to self-identification while maintaining underlying biological sex tracking for medical purposes, statistical reporting, and single-sex space administration. But the theater is not total; some institutions are functionally restructuring around self-identification (legal sex change, institutional records, workplace gender marker change). The ratio reflects the gap between normative commitment (self-ID is the real criterion) and operational reality (biological sex still tracked for medical and administrative purposes).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival division. From the beneficiary position (transgender women, self-ID advocates), the constraint is pure coordination (rope) — enabling identity-recognition alignment solves a genuine collective action problem with low suppression. From the victim positions (sex-based rights claimants, biological sex maintainers), the constraint is extraction (snare/tangled rope) — their category is being appropriated and redefined without consent, imposing costs on sex-specific protections and institutional clarity. The medical system sees piton dynamics — maintaining biological sex categories through institutional inertia despite theoretical pressure to treat them as merely performative. The analytical observer risks mountain classification (treating gender identity as an immutable feature of personhood) but structural data reveals this as a false summit: the boundary between 'woman' and 'not-woman' is under legal and institutional control, and the choice to define it by self-identification is a contingent social decision, not a discovered natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness depends on their structural relationship to the constraint. Transgender women (institutional/arbitrage) experience low or negative effective extraction — they benefit from the expanded category and have institutional allies. Sex-based rights claimants (powerless/trapped) experience high extraction — their category is being redefined without their consent or input, and they have no exit. Biological sex category maintainers (moderate/constrained) experience moderate extraction — they provide genuine coordination function (sex-based medicine) but face institutional pressure and delegitimation. Feminist movements (organized/constrained) provide analytical and activist coordination while being suppressed through institutional pressure. The medical system maintains arbitrage options (can track both self-identified gender and biological sex simultaneously) despite performative commitment to self-ID-only frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how a tangled-rope reading contains real coordination benefits (enabling identity recognition, solving genuine alignment problem) alongside real extraction (redefining a category in ways that harm those dependent on the old boundary). Mandatrophy resolution requires acknowledging that both the coordination function and the extraction mechanism are real. The beneficiary perspective is not wrong about the coordination problem being solved; the victim perspective is not wrong about real costs to sex-based protections. The constraint is legitimately tangled — active enforcement (institutional policy, workplace/institutional gender marker changes, legal sex recognition) creates both benefits and extraction simultaneously. No single type captures this; tangled_rope is the accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_kernel_status,
    'Is ''woman'' a natural kind with immutable boundaries grounded in biology, or a social category that can be defined by different criteria (self-identification vs. reproductive anatomy vs. social position)?',
    'Ontological analysis of category stability across cultures and historical periods; distinction between biological facts (chromosomes, reproductive anatomy) and category boundaries (who counts as woman, for what purposes). Investigation of whether category boundaries are determined by biological properties or by social/legal decision.',
    'If natural kind: sex-based reading is mountain (immutable). If social category: both readings are contingent institutional choices, neither immutable. This determines whether the constraint is false summit or genuine natural law dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_kernel_status, conceptual, 'Whether ''woman'' category is natural kind or social construction').

omega_variable(
    institutional_authority_grounding,
    'What grounds the authority to define ''woman'' — biological discovery, legal/administrative convention, cultural tradition, or the self-identification of those claiming the category?',
    'Historical genealogy of the ''woman'' category; analysis of which institutions currently hold definitional authority and how they justify it; comparison across jurisdictions with different definitional standards.',
    'If grounded in biological discovery: sex-based reading retains institutional authority. If grounded in convention/self-identification: gender-identity reading gains legitimacy. This determines whose definition wins in cases of conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_grounding, conceptual, 'Authority grounding for defining woman category').

omega_variable(
    sex_based_rights_redefinability,
    'Can sex-specific legal protections and rights (reproductive health access, domestic violence shelter eligibility, single-sex sport categories) be preserved if the legal category ''woman'' is defined by self-identification rather than biological sex?',
    'Policy analysis: mapping how jurisdictions redefine sex-based eligibility criteria when legal sex category is self-identification-based; measurement of protected outcomes for each group under both definitional systems; longitudinal tracking of women''s access to sex-specific services across different legal frameworks.',
    'If yes: sex-based rights survive under new definition; victims (sex-based rights claimants) experience lower extraction. If no: sex-specific protections erode; extraction from biological-sex-category-dependent persons increases. This determines the real cost structure of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_rights_redefinability, empirical, 'Whether sex-based rights can survive gender-identity definition of woman').

omega_variable(
    trans_inclusion_institutional_resistance,
    'To what extent does institutional enforcement of gender-identity-based definition require active suppression of competing definitional frameworks (sex-based, intersectional, or biological)?',
    'Documentation of institutional policies mandating use of self-identified gender in official contexts; measurement of penalties for maintaining sex-based language or analysis; analysis of whether institutional pressure is coercive or persuasive; comparison of enforcement intensity across different institutional domains (healthcare, law, education, employment).',
    'High suppression: the reading requires active enforcement against resistance, confirming tangled_rope classification (coordination + extraction + coercion). Low suppression: the reading achieves institutional dominance through consensus, suggesting rope or scaffold. This determines whether the constraint is stable or requires ongoing force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_inclusion_institutional_resistance, empirical, 'Institutional suppression required for gender-identity definition enforcement').

omega_variable(
    sex_specific_medical_protocols_functionality,
    'Do sex-based medical protocols (hormone treatment, reproductive health, disease epidemiology) lose clinical efficacy or safety if the administrative category ''female patient'' is defined by self-identification rather than biological sex?',
    'Clinical outcome analysis comparing protocols that use self-identified gender vs. biological sex for treatment selection; pharmacokinetic studies on hormone-based treatments; epidemiological comparison of disease prevalence and treatment response across sex categories defined different ways.',
    'If protocols remain clinically sound: medical ontology can adapt without functional loss; piton classification confirmed (theater ratio high, functional compression). If clinical outcomes degrade: piton classification false; actual functional coordination present, confirming tangled_rope. This determines whether the medical system is genuinely degraded or adapting functionally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sex_specific_medical_protocols_functionality, empirical, 'Whether sex-based medical protocols retain clinical efficacy under gender-identity category definition').

omega_variable(
    reading_foreclosure_logic,
    'Does the gender-identity reading logically foreclose the sex-biology reading within a single institutional framework, or can both coexist with institutional separation (gender identity for social/legal purposes, biological sex for medical/safety purposes)?',
    'Analysis of whether institutional dual-tracking is theoretically coherent; case studies of jurisdictions attempting to maintain both (self-identified gender for legal status, biological sex for medical protocols); examination of whether conflicts at boundaries reveal incompatibility.',
    'If foreclosure: the readings are mutually exclusive; one must win. If coexistence: both can remain live within different institutional domains; the constraint is not zero-sum. This determines the fundamental structure of the reading relationship (forecloses vs. coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_logic, conceptual, 'Whether gender-identity reading logically forecloses sex-biology reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_identity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t5, gender_identity_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(gend_tr_t10, gender_identity_reading, theater_ratio, 10, 0.41).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gender_identity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gend_be_t5, gender_identity_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(gend_be_t10, gender_identity_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(gender_identity_reading, intersectional_coexistence_reading).
narrative_ontology:affects_constraint(gender_identity_reading, single_sex_space_access).
narrative_ontology:affects_constraint(gender_identity_reading, reproductive_health_policy).
narrative_ontology:affects_constraint(gender_identity_reading, sports_eligibility_frameworks).

% DUAL FORMULATION NOTE:
% Gender-identity reading of woman/female category is one of three structurally distinct constraint stories sharing the kernel 'woman/female.' Each reading has distinct ε, distinct beneficiary/victim sets, and distinct institutional implications. The readings are linked by network.affects_constraints rather than described within a single story. See sex_biology_reading and intersectional_coexistence_reading for alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gender_identity_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
