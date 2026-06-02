% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership via Gender Identity Self-Declaration
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   The gender_identity_reading instantiates one interpretation of how
 *   gendered categories should function: membership is constituted by
 *   self-declared gender identity rather than biological sex characteristics
 *   or social role assignment. This reading emerged into legal prominence in
 *   the 1990s–2010s as medical and advocacy communities recognized that
 *   denying gender-identity-based category membership created measurable
 *   harms (suicide risk, medical erasure, legal invisibility) for transgender
 *   people. The constraint exhibits the full structure of a tangled-rope
 *   hybrid: it solves genuine coordination problems (bringing legal
 *   categories into alignment with lived gender experience, removing medical
 *   gatekeeping from self-identification) while simultaneously creating
 *   asymmetric extraction (redefining categories such that cis women's
 *   interests in sex-segregated services become suspect, positioning those
 *   who defend sex-based boundaries as exclusionary, degrading the empirical
 *   measurement capacity of the sex category). The extractiveness has risen
 *   over 10 years as enforcement mechanisms have tightened (institutional
 *   gatekeeping, legal precedent, professional guidelines) and as the
 *   performance theater of maintaining 'sex' as a separate analytical
 *   category has increased while actual administrative reliance on biological
 *   measurement has declined. Suppression has risen as questioning this
 *   reading becomes professionally and socially costly. This reading coexists
 *   with the biological_sex_reading (which grounds categories in observable
 *   sex characteristics) and the social_role_reading (which grounds
 *   categories in gender expression and social positioning), but the three
 *   readings occupy structurally incompatible frameworks about what makes
 *   category membership legitimate.
 *
 * KEY AGENTS:
 *   - Transgender people seeking legal recognition (moderate/mobile) — Primary beneficiary of coordination function; gain access to legal documents, institutional recognition, and sex-segregated spaces aligned with identity
 *   - Cis women defending category boundaries (moderate/constrained) — Experience both coordination gains (shared interests in sex-based legal protections) and extraction losses (gatekeeping authority relinquished; boundary concerns delegitimized); constrained exit due to professional/reputational consequences of dissent
 *   - Sex-segregated space users (powerless/trapped) — Victim group; lose basis for excluding male-bodied people from domestic violence shelters, gynecology clinics, correctional housing; no exit from dependency on sex-segregated services
 *   - Legal and administrative institutions (institutional/constrained) — Enforce the reading through legal precedent, administrative guidelines, institutional gatekeeping; constrained by statutory frameworks and constitutional law
 *   - Gender identity advocacy institutions (organized/mobile) — Organizational beneficiary; gain permanent authority over gender-identity recognition mechanisms; see current rules as temporary scaffolding toward full gender diversity accommodation
 *   - Sex-category measurement infrastructure (institutional/arbitrage) — Piton; administrative systems (medical, statistical, legal) that once relied on sex classification now operate with degraded function as category membership depends on identity claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.48).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.58).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership via Gender Identity Self-Declaration").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, 'a9879459-7967-46cb-acb6-50a2dc9d6be1').
narrative_ontology:cs_kernel_codification('a9879459-7967-46cb-acb6-50a2dc9d6be1', formalized).
narrative_ontology:cs_authority_grounding('a9879459-7967-46cb-acb6-50a2dc9d6be1', extraction).
narrative_ontology:cs_interpretation_layer_present('a9879459-7967-46cb-acb6-50a2dc9d6be1').
narrative_ontology:cs_reading_relation('a9879459-7967-46cb-acb6-50a2dc9d6be1', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9879459-7967-46cb-acb6-50a2dc9d6be1', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('a9879459-7967-46cb-acb6-50a2dc9d6be1', foundational, gender_identity_constitutes_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('a9879459-7967-46cb-acb6-50a2dc9d6be1', gender_identity_constitutes_category_membership, deontological).
narrative_ontology:cs_axiom('a9879459-7967-46cb-acb6-50a2dc9d6be1', secondary, categorical_membership_independent_of_biological_substrate).
narrative_ontology:cs_axiom_status(categorical_membership_independent_of_biological_substrate, holdable).
narrative_ontology:cs_axiom_grounding('a9879459-7967-46cb-acb6-50a2dc9d6be1', categorical_membership_independent_of_biological_substrate, deontological).
narrative_ontology:cs_reference_frame('a9879459-7967-46cb-acb6-50a2dc9d6be1', identity_recognition_without_gatekeeping).
narrative_ontology:cs_drift_state('a9879459-7967-46cb-acb6-50a2dc9d6be1', contemporary_institutional_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9879459-7967-46cb-acb6-50a2dc9d6be1', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_people_seeking_legal_recognition).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_facing_category_boundary_dispute).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sex_segregated_space_integrity).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, empirical_sex_category_measurement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSGENDER PEOPLE SEEKING LEGAL RECOGNITION (ROPE) — Self-identification as a path to legal recognition solves a genuine coordination problem: how to align legal categories with lived gender identity without requiring medical gatekeeping or state surveillance of private medical history. The mechanism is primarily coordinative (establishing mutual recognition), with modest extraction (access to sex-segregated spaces and legal documents). Mobile exit options reflect that trans people can pursue alternative legal pathways or live without formal recognition, though at significant social cost.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CIS WOMEN DEFENDING CATEGORY BOUNDARIES (TANGLED ROPE) — Cis women experience this constraint as both coordination and extraction. Genuine coordination: shared interests in combating sex-based discrimination, reproductive justice, and access to female-specific services. Extraction: gatekeeping power over 'woman' category is redistributed; those who defend sex-based categories face social sanction and are positioned as exclusionary; concerns about sex-segregated space integrity (domestic violence shelters, locker rooms) are treated as suspect. Constrained exit: speaking concerns about category boundaries carries reputational risk and professional consequences.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN IN SEX-SEGREGATED SPACES — EMPIRICAL REALITY AS VICTIM (SNARE) — The category 'woman' becomes dependent on subjective identity declaration rather than observable sex characteristics. Women seeking sex-segregated spaces for vulnerability (domestic violence shelters, gynecology services, correctional housing) lose basis for excluding male-bodied people who self-identify as women. The empirical category is neither consulted nor represented in this reading. Suppression is high: questioning the boundary is treated as transphobic; alternative mechanisms (ability testing in sports, different privacy provisions in prisons) are resisted. No exit: the biological base is immutable; those dependent on sex-segregated services cannot opt out of the category dispute.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGAL AND ADMINISTRATIVE INSTITUTIONS (TANGLED ROPE) — Courts, legislatures, and administrative agencies face genuine coordination demands: how to update legal categories to reflect current understanding of gender without destabilizing sex-segregated statutory protections. Active enforcement is required to suppress alternative readings (the biological_sex_reading, the social_role_reading). Extraction: institutions gain authority to define membership; those challenging institutional gatekeeping are subject to legal sanctions; institutional consistency becomes more important than either coordinate goals or empirical accuracy. Constrained exit: institutions are bound by statutory frameworks and constitutional constraints that limit how radically they can revise category definitions.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENDER IDENTITY ADVOCACY INSTITUTIONS (SCAFFOLD) — Human rights organizations, LGBTQ+ advocacy groups, and allied institutions see self-identification as a temporary scaffolding mechanism solving the urgent problem of trans legal erasure and medical gatekeeping. The sunset logic: as broader culture recognizes gender diversity, sex-segregated spaces can be replaced with genuine privacy accommodations and ability-based access rules (in sports, prisons, shelters) that don't depend on binary category membership. Low extraction because the advocacy sees itself as solving a humanitarian crisis, not claiming permanent authority. Mobile exit because the institutions can and do revise strategy when evidence surfaces.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SEX-CATEGORY ADMINISTRATIVE INFRASTRUCTURE (PITON) — Decades of legal and medical infrastructure (sex chromosomes, anatomical classification, reproductive capacity, endocrine systems) became the basis for statutory protections and resource allocation. Under the gender_identity_reading, this entire apparatus degrades to theater: sex-based statistics become unreliable (if trans men are counted as men and trans women as women, reproductive health data fractures); medical history becomes optional (hormone replacement therapy is relevant to cardiovascular risk, but if 'woman' is a subjective category, medical providers cannot reliably query pertinent history); athletic competition data becomes incoherent (performance variation by sex becomes unmentionable). The infrastructure persists because no complete alternative has replaced it, not because it functions. Theater ratio reflects that much institutional talk about 'sex' persists while the actual categorization mechanism has shifted to identity claim.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, gender is an irreducible feature of human social organization and experience; membership in gendered categories is therefore inevitable and unchosen. Gender identity is a fundamental aspect of human consciousness; self-identification is a natural expression of this irreducibility. From this view, the constraint appears as an immutable law: people's gender identity cannot be overridden by external classification; social recognition of this law is not optional. However, this mountain classification is a false summit — beneficiaries exist (advocacy institutions benefit from permanent authority; cis women experience social sanction for boundary defense), indicating that the 'natural law' framing obscures a contingent institutional arrangement grounded in specific policy choices about category definition and enforcement mechanisms.
constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gendered_category_membership__gender_identity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, TR),
    TR >= 0.70.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The reading creates both genuine coordination value (aligning legal recognition with identity experience, reducing medical gatekeeping) and significant extraction (gatekeeping power over 'woman' category is redistributed; sex-based statutory protections lose salience; questioning the boundary incurs social cost). The 0.48 reflects that the coordination function is substantial but the asymmetry is real: advocacy institutions and trans people gain recognition authority; cis women lose category control; vulnerable populations lose sex-segregation basis. Suppression (0.58): Moderate-high. Significant barriers to expressing concerns about category boundaries include professional consequences (educators, healthcare providers cannot voice concerns without discipline risk), social sanction, reputational damage, and legal exposure to discrimination suits. However, suppression is not total — dissenting views persist in academic circles, among some feminist scholars, and in policy critiques. Theater ratio (0.64): Moderately high. Much institutional performance around 'sex' persists (medical forms still ask sex; legal documents still list sex at birth) while actual decision-making on category membership has shifted to identity claim. The continued collection and publication of 'sex' data serves little administrative purpose if sex is not the basis for category assignment; the theater reflects institutional inertia and legal complexity rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives span from snare (sex-segregated space users trapped in category dispute) through tangled_rope (cis women and institutional actors experiencing both coordination and extraction) to rope (trans people experiencing primarily coordination with modest extraction) to scaffold (advocacy institutions seeing a sunset) to piton (sex-category infrastructure degraded but persisting) to mountain (analytical view that sees gender identity as an inviolable law). The perspectival gap reveals the constraint's complexity: there is no single type that captures the structure. The same reading (gender_identity_reading) appears as beneficial coordination from the trans perspective, extractive redistribution from the cis women's perspective, total loss from the sex-segregated-space perspective, necessary enforcement from the institutional perspective, and temporary support from the advocacy perspective. This distribution shows that the reading's apparent simplicity ('gender identity determines category membership') masks profound institutional and material asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from beneficiary/victim status plus exit options. Transgender people benefit from the reading (low d) and have mobile exit options (can pursue alternative legal pathways, can live without formal recognition); this produces low d → negative or low χ from their position. Cis women are both beneficiaries (of sex-based statutory protections) and victims (of category-boundary gatekeeping redistribution); their constrained exit (professional/reputational cost of dissent) produces moderate d → moderate χ. Sex-segregated space users are victims with trapped exit (cannot opt out of dependency on sex-segregated services); this produces high d → high χ. Institutional actors benefit from enforcement authority but are constrained by statutory frameworks; overrides may be needed to capture that the constraint's real extraction flows to advocacy institutions, not to cis women (whose power is diminished, not enhanced). Advocacy institutions benefit from permanent authority over recognition mechanisms and have mobile exit (can revise strategy); this produces low d despite their organized power. The analytical observer derives d from the modal agent structure, which is mixed; the mountain classification is false-summit territory because identifiable beneficiaries exist.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sex_category_measurability,
    'What constitutes the empirical basis for ''sex'' category assignment in a system where legal membership depends on identity claim rather than observable characteristic?',
    'Specification of legal standards: does ''sex'' retain meaning as a biological category for medical, statistical, or legal purposes? If yes, how is biological sex determined when category membership is self-declared? If no, what replaces sex-based statutory protections (reproductive healthcare, domestic violence services)?',
    'If empirical measurability is preserved: sex and gender are distinct categories; tangled_rope classification holds. If measurability collapses: empirical category becomes piton; snare classification for those dependent on sex-segregated services; mountain classification is false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sex_category_measurability, conceptual, 'Empirical measurability of biological sex category when legal category depends on identity').

omega_variable(
    category_boundary_enforcement_mechanism,
    'Who enforces the boundary of ''woman'' category, and by what authority? If self-identification is sufficient, what prevents category collapse (all humans self-identifying as women)?',
    'Analysis of actual enforcement practice: courts, administrative agencies, institutional gatekeeping. Identification of where enforcement operates (legal documents, institutions, everyday speech) and where it fails (informal social contexts, private belief). Specification of penalties for misclassification or boundary violation.',
    'If enforcement is state monopoly: institutional_power perspective is accurate; extraction concentration is high. If enforcement is distributed (social sanction, institutional norms, legal documents separately): extraction is more diffuse; multiple perspectives capture different enforcement regimes. If enforcement fails (category boundary unenforceable): piton classification for formal institutions; snare for those dependent on category stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_enforcement_mechanism, empirical, 'Enforcement mechanism for gender category boundaries under self-identification rule').

omega_variable(
    sex_segregated_space_functional_necessity,
    'For what purposes are sex-segregated spaces functionally necessary, and can those purposes be satisfied by alternative mechanisms (privacy, ability-based access, vulnerability-based services)?',
    'Comparative case analysis: performance in sports (hormone-suppression standards vs. ability testing); safety in detention (body search protocols vs. staff-assignment protocols); healthcare (gynecology vs. gender-affirming care provision); intimate shelters (sexual assault trauma vs. generic vulnerability assessment). Empirical comparison of access equity and safety outcomes.',
    'If alternative mechanisms fully satisfy purposes: scaffold classification is correct — transition to new mechanisms has a real sunset. If some purposes require sex-segregation (reproductive healthcare, detection of reproductive trauma): constraint remains indefinitely; snare classification for those dependent on those services; piton for transition institutions. If purposes are partially sexed, partially gendered: tangled_rope holds; multiple institutions operate in parallel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_segregated_space_functional_necessity, empirical, 'Functional necessity of biological sex segregation vs. alternative accommodations').

omega_variable(
    category_identity_fusion_mechanism,
    'Is the binding between gender identity and category membership tight (identity is constituted through category membership) or loose (identity is personal; category is administrative)?',
    'Phenomenological and philosophical analysis: Does affirming one''s gender identity require legal recognition? Can gender identity exist without category membership? For cis women, does defending sex category boundaries express genuine material interest or identity investment? Do advocacy institutions frame category change as humanitarian necessity or as identity vindication?',
    'If fusion is tight: identity_locked exit option accurate for those invested in category stability or identity expression; mountain and rope classifications reflect genuine identity-constituting mechanisms. If loose: identity_locked exit overstates the mechanism; constrained or mobile exits are more accurate; extraction is more clearly administrative than existential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_identity_fusion_mechanism, conceptual, 'Whether gender identity is constituted through category membership or orthogonal to it').

omega_variable(
    kernel_reading_committer_ambiguity,
    'This constraint is the gender_identity_reading of the gendered_category_membership kernel. What is the underlying commitment (kernel) that these readings compete to interpret?',
    'Identification of the stabilized claim that both readings treat as binding: Is it ''categories must reflect lived experience''? ''Categories must rest on empirical bases''? ''Gender and sex are relevant to justice''? The kernel is not the reading but the thing the readings disagree about interpreting. Different kernels support different reading relationships (coexist vs. foreclose vs. influence).',
    'If kernel is ''gender identity is socially real'': this reading and social_role_reading coexist; both interpret the same kernel. If kernel is ''empirical sex category is inviolable'': this reading forecloses biological_sex_reading within a single framework. If kernel is ''gendered categories serve justice'': all three readings influence each other but no single reading rules others out.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Specification of the gendered_category_membership kernel that reading_relations interpret').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gendcat_id_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gendcat_id_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(gendcat_id_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(gendcat_id_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gendcat_id_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(gendcat_id_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gendcat_id_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(gendcat_id_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(gendcat_id_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_space_access).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, medical_category_measurement).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, athletic_competition_classification).

% DUAL FORMULATION NOTE:
% The gendered_category_membership kernel decomposes into three constraint stories, one per reading. The gender_identity_reading (this constraint) has ε=0.48 (tangled rope). The biological_sex_reading will show different ε reflecting different empirical status and different beneficiary/victim structure. The social_role_reading will show different ε reflecting emphasis on enacted expression rather than identity or biology. Each reading has its own network of downstream constraints (sex-segregated spaces, medical measurement, athletic competition); the network edges show which downstream constraints are affected by which upstream reading. This is not decomposition of one constraint via ε-invariance; these are genuinely different readings of a single kernel, making the network structure essential to understand the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
