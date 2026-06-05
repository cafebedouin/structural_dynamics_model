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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
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
 *   human_readable: Gender Identity Self-Identification as Category Membership Criterion
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The gender identity reading of the woman/female category resolves the
 *   allocation problem of space, legal recognition, and rights by making
 *   self-identification the sole criterion for membership in gender-based
 *   categories. This reading instantiates one specific answer to the
 *   contested kernel: what does it mean to be a woman or female in law? The
 *   gender identity reading claims: category membership follows the
 *   individual's own gender self-identification, independent of biological
 *   sex, medical transition status, or past legal designation. This reading
 *   coordinates genuine recognition and dignity for transgender individuals
 *   and resolves allocation conflicts in gender-diverse populations by
 *   deferring to internal self-knowledge. Simultaneously, it imposes
 *   extraction on agents whose interests in sex-segregated space, athletic
 *   fairness, or legal protections for biological women are affected by trans
 *   inclusion. The constraint exhibits different classifications depending on
 *   the observer: pure extraction (Snare) from the perspective of cisgender
 *   women in vulnerable spaces; coordination (Rope) from the perspective of
 *   trans individuals seeking recognition; mixed coordination-extraction
 *   (Tangled Rope) from institutional and athletic perspectives; degraded
 *   ritual (Piton) from jurisdictions with parallel systems; and naturalized
 *   law (false-summit Mountain) from civilizational analytical positions that
 *   treat gender identity as ontologically self-evident.
 *
 * KEY AGENTS:
 *   - Transgender individuals seeking identity protection (organized/mobile): Primary beneficiary — gains legal recognition, space access, dignity. Benefits flow toward this agent.
 *   - Cisgender women in sex-segregated spaces (powerless/trapped): Primary victim — loses gatekeeping mechanism protecting sex-segregated spaces; cannot exit.
 *   - Female athletes (moderate/constrained): Secondary victim — experience competitive disadvantage if hormonal advantages persist; can organize but face resource barriers.
 *   - Healthcare and custodial institutions (institutional/constrained): Mixed — must coordinate care for diverse populations but manage extraction of liability exposure and staffing complexity.
 *   - Sex-biology reading advocates (varies): Contesters — identity-locked or constrained resistance; see this reading as foreclosing their foundational ontology.
 *   - Jurisdictions with self-identification policies (institutional/arbitrage): Implementers — capture administrative simplicity but manage parallel system degradation.
 *   - Analytical observer (analytical/analytical): Third-position perspective risking naturalization of what is contingent institutional design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_identity_reading, 0.58).
domain_priors:suppression_score(gender_identity_reading, 0.62).
domain_priors:theater_ratio(gender_identity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gender_identity_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gender_identity_reading, "Gender Identity Self-Identification as Category Membership Criterion").
narrative_ontology:topic_domain(gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(gender_identity_reading, formalized).
narrative_ontology:cs_authority_grounding(gender_identity_reading, lineage).
narrative_ontology:cs_kernel_id(gender_identity_reading, woman_female_category).
narrative_ontology:cs_reading_relation(gender_identity_reading, sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation(gender_identity_reading, hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom(gender_identity_reading, foundational, gender_identity_ontologically_primary).
narrative_ontology:cs_axiom_status(gender_identity_ontologically_primary, holdable).
narrative_ontology:cs_axiom(gender_identity_reading, foundational, self_identification_sufficient_for_legal_category).
narrative_ontology:cs_axiom_status(self_identification_sufficient_for_legal_category, holdable).
narrative_ontology:cs_reference_frame(gender_identity_reading, gender_identity_legal_recognition).
narrative_ontology:cs_drift_state(gender_identity_reading, post_implementation_institutional_conflict, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_identity_reading, transgender_individuals_seeking_identity_protection).
narrative_ontology:constraint_beneficiary(gender_identity_reading, gender_nonconforming_individuals).
narrative_ontology:constraint_victim(gender_identity_reading, cisgender_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(gender_identity_reading, female_athlete_categories).
narrative_ontology:constraint_victim(gender_identity_reading, sex_based_rights_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CISGENDER WOMEN (SNARE) — From the position of women claiming sex-based rights and privacy in spaces historically segregated by biological sex, this reading imposes a constraint with no exit option. The self-identification criterion removes the gatekeeping mechanism that historically protected these spaces. Exit is impossible (one cannot unsex oneself, cannot refuse shared spaces), and the constraint operates through institutional enforcement of inclusion. Maximum extraction experienced because the agent is trapped.
constraint_indexing:constraint_classification(gender_identity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEMALE ATHLETES (TANGLED ROPE) — Female athletes experience both coordination and extraction. The identity-based reading coordinates legitimate recognition of trans athletes' identities and enables participation. Simultaneously, it extracts a performance advantage where hormonal advantages persist despite transition protocols. The athlete bears significant cost (competitive disadvantage) but retains some agency (advocacy, rule-setting participation). Suppression is high (career-dependent, cannot exit without abandoning sport).
constraint_indexing:constraint_classification(gender_identity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSGENDER INDIVIDUALS SEEKING IDENTITY PROTECTION (ROPE) — From the perspective of trans individuals, this reading coordinates a genuine collective action problem: enabling recognition, dignity, legal status, and safety in spaces matching identity. The identity-based criterion solves a coordination problem (how to allocate space-access rights in a gender-diverse population) with minimal coercive overhead relative to the beneficiary group's own preferences. This perspective experiences the constraint as low-extraction coordination.
constraint_indexing:constraint_classification(gender_identity_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE AND CUSTODIAL INSTITUTIONS (TANGLED ROPE) — Institutional administrators coordinate genuine care challenges (dignified treatment, safety, privacy for a gender-diverse population) while managing extraction: liability exposure, staffing complexity, facility redesign costs. The identity-based reading simplifies some coordination (trans individuals' preferred pronouns and spaces) but creates new asymmetries (trans women in female prisons/shelters, staffing across gendered roles). Institutions have constrained exit (duty of care, regulatory mandate).
constraint_indexing:constraint_classification(gender_identity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JURISDICTIONS USING LEGAL SELF-IDENTIFICATION (PITON) — Many jurisdictions have adopted self-identification as the legal criterion for gender status (Argentina, Denmark, Portugal, Ireland, Belgium), yet the policy has not fully replaced underlying sex-based legal structures (criminal sentencing, sports categories, medical research cohorts continue using biological sex). The identity-based reading coexists with a degraded parallel system of sex-based administration. Theater ratio is moderate (formal compliance with self-identification law alongside persistent sex-based data collection) because the underlying sex-based infrastructure persists through inertia rather than elimination.
constraint_indexing:constraint_classification(gender_identity_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical position, one might argue that self-identification as the sole criterion for legal category membership is an irreducible feature of human agency and dignity: a person's gender identity is internal, self-constituted, and inaccessible to external verification. Therefore, any institutional category that requires external verification (biological markers, past documents, medical certification) necessarily extracts from those whose internal identity diverges from external markers. The reading appears mountain-like — grounded in an inviolable principle about human self-knowledge. However, the structural data reveals this as a false summit: the constraint's extraction flows depend on institutional design choices (space allocation, athletic categories, incarceration protocols), not on immutable principles.
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

test(extraction_signature) :-
    domain_priors:base_extractiveness(gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_identity_reading, TR),
    TR >= 0.70.

:- end_tests(gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the magnitude of extraction imposed on multiple victim groups but tempered by genuine coordination gains for beneficiaries and the reading's institutional legitimacy in multiple jurisdictions. The metric reflects that the identity-based reading is not pure extraction (unlike a predatory Snare) but rather a hybrid mechanism where beneficiary gains are genuine but asymmetrically distributed. The value has increased over the interval (0.35 → 0.58) as implementation has surfaced specific harms in athletics, custodial settings, and vulnerable women's spaces — the reading's extraction became more visible and measurable as practice matured. Suppression (0.62): High. The enforcement of self-identification criteria requires institutional override of biological sex documentation, medical gatekeeping, and sex-based governance structures. Institutions must suppress alternative criteria (biological sex, medical transition status) to enforce the reading's logic. Suppression is active (requires continuous enforcement against institutional inertia and counter-pressure) rather than passive. Theater ratio (0.45): Moderate-low. The reading has relatively low theatrical content — the institutional implementation is relatively straightforward (accept self-identification documents, update legal status). The theater that does exist emerges in maintenance of parallel sex-based systems (medical research cohorts, sport categories, criminal sentencing protocols continue using biological sex despite identity-based legal status). Claimed type (Tangled Rope): Justified by presence of both genuine coordination function (solving allocation problems for trans individuals and diverse populations) AND asymmetric extraction (from women in vulnerable spaces, female athletes, sex-based rights advocates). Requires active enforcement (institutional mandate to override sex-based governance). Satisfies all Tangled Rope gates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and represents the core mandatrophy of this constraint. The beneficiary (trans individuals seeking recognition) experiences Rope — the reading solves their coordination problem with minimal coercive overhead. The victim (cisgender women in segregated spaces) experiences Snare — they are trapped with no exit. The athletic institution experiences Tangled Rope — coordination of trans participation alongside extraction of competitive disadvantage. The custodial institution experiences Tangled Rope — coordination of dignified treatment alongside extraction of safety and privacy management costs. The jurisdictional level experiences Piton — self-identification is formally mandated but parallel sex-based systems persist through inertia. The analytical observer risks Mountain — treating gender identity as an inviolable law of human agency rather than as a contingent institutional design choice. These are not minor differences in emphasis; they are fundamental divergences in how the same institutional arrangement is experienced and classified.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the constraint. Transgender individuals seeking identity protection are beneficiaries with exit option 'mobile' (they can relocate to jurisdictions with more favorable policies, exit is possible but costly), yielding d ≈ 0.15–0.25 (low extraction experienced). Cisgender women in vulnerable spaces are victims with exit option 'trapped' (cannot exit, cannot abandon space-needs), yielding d ≈ 0.95 (maximum extraction experienced). Female athletes are victims with exit option 'constrained' (can exit sport but at career cost), yielding d ≈ 0.70–0.85 (high extraction experienced). Institutions are mixed (beneficiary of simplified administrative logic for trans individuals, victim of increased liability and staffing complexity), with exit option 'constrained' (regulatory mandate prevents full exit), yielding d ≈ 0.50–0.65 (moderate extraction experienced). The high suppression value (0.62) reflects that enforcement is active and constrains alternative criteria; this suppression is unscaled by d — it applies globally to all agents. Effective extractiveness (χ) is computed as ε × f(d) × σ(S) for each agent, which explains why the same constraint is experienced differently: beneficiaries experience negative or low χ; victims experience high χ; institutions experience moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED KERNEL CONTEXT: This constraint does not resolve its mandatrophy because it represents an active, three-way dispute over what the category 'woman'/'female' means in law. The gender identity reading (this story) coexists with the sex-biology reading and the hybrid contextual reading. The mandatrophy is not 'which type is correct?' but 'which reading resolves the kernel correctly?' The extractiveness value (0.58) and the perspectival gap (Rope for beneficiary, Snare for victim, Tangled Rope at analytical level) are correct under the gender identity reading. Under the sex-biology reading, the extractiveness would redistribute (toward sex-biology advocates as victims, trans individuals as extractors), and the classifications would invert. The engine's mandatrophy resolution depends on which reading's legitimacy is asserted — and that is not a technical question but a question of political authority, which the constraint system exposes but does not resolve. The measurement trajectory (extractiveness rising from 0.35 to 0.58) reflects implementation maturation: as the reading's implications surfaced in practice (athletic competition, custodial settings, vulnerable women's spaces), the extraction became more visible and measurable. The theater ratio stayed relatively low (0.30 → 0.45) because the reading's institutional logic is straightforward; the increasing theater that does exist reflects growing institutional dissonance (parallel systems, conflicting mandates) as implementation revealed conflicts the reading did not anticipate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hormone_level_threshold_for_fairness,
    'At what hormone levels, post-transition protocols, or timeframes do residual athletic advantages from pre-transition development diminish to levels compatible with competitive fairness?',
    'Longitudinal biomechanical data on trans athletes'' performance relative to cisgender athletes across sports; meta-analysis of muscle retention, bone density, aerobic capacity changes post-transition; statistical modeling of fair category boundaries',
    'If residual advantage is negligible (<2% performance gap): identity-based athletics classification shifts toward Rope (coordination with minimal extraction). If substantial (>5%): remains Snare from female athletes'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hormone_level_threshold_for_fairness, empirical, 'Hormone thresholds and residual athletic advantages post-transition').

omega_variable(
    space_sharing_safety_empirics,
    'In sex-segregated spaces (shelters, prisons, healthcare wards) allocated by self-identification, what is the actual empirical rate of harm, harassment, or safety violations relative to the baseline rate in single-sex spaces?',
    'Comparative incident data from jurisdictions with and without self-identification criteria; controlled analysis of space types, security protocols, and demographic composition; qualitative data from residents/staff in mixed-gender spaces',
    'If harm rates are equivalent to cis-only cohorts: privacy/safety extraction is minimal (approaches Rope). If substantially elevated: extraction is substantive (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(space_sharing_safety_empirics, empirical, 'Empirical safety outcomes in sex-segregated spaces under identity-based allocation').

omega_variable(
    legal_category_kernel_definition,
    'Is legal category membership in ''woman''/''female'' (for criminal sentencing, rights protection, historical injustice remedies) ontologically grounded in biological sex, social gender, legal recognition, or is the category itself ambiguous across these axes?',
    'Historical legal doctrine analysis; comparative jurisdiction review of how ''woman'' is defined in different statutes (employment law, criminal law, sex-based rights frameworks); philosophical analysis of category persistence and the coherence of ''woman'' as a legal kind',
    'If ''woman'' is fundamentally sex-based: identity-based reading FORECLOSES sex-biology reading (they cannot coexist in law). If ''woman'' is fundamentally social-gender-based: identity-based reading is the canonical reading. If ambiguous/contextual: both coexist or context determines which applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_category_kernel_definition, conceptual, 'Ontological grounding of legal category ''woman'' / ''female'' across sex, gender, recognition').

omega_variable(
    institutional_capacity_for_parallel_systems,
    'Can institutions practically maintain parallel identity-based and sex-based administrative systems (e.g., self-identified gender for pronouns/social spaces; biological sex for medical research cohorts, sport categories, custodial housing) without creating degradation, inconsistency, or extractive workarounds?',
    'Audit of jurisdictions maintaining dual systems; documentation of errors, inconsistencies, workarounds staff develop; cost analysis of parallel data collection and space management; qualitative data on which system ''wins'' in conflict cases',
    'If parallel systems are stable: Piton classification confirmed (theater, not extraction, dominates). If parallel systems degrade and institutional pressure toward single system: the constraint force increases and the system becomes Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_for_parallel_systems, empirical, 'Institutional feasibility of maintaining parallel identity-based and sex-based systems').

omega_variable(
    reading_identity_locked_agents,
    'Are agents who identify with the sex-biology reading (rejecting self-identification as the sole criterion) bound by constrained material barriers to exit, or are they bound by identity-locked commitment to a foundational ontological premise about sex and category membership?',
    'Qualitative analysis of sex-biology advocates'' framing; identify whether opposition centers on material harms, institutional feasibility, or on a foundational commitment to sex as ontologically primitive to gender/legal category. Test whether advocates would revise their position if empirical data on harms/benefits changed, or whether the opposition is grounded in identity commitment to a reading of ''woman'' as inherently sex-based.',
    'If constrained (material): the reading''s suppression is externally imposed (high extraction of sex-biology adherents). If identity_locked: the reading''s enforcement relies on institutional mandate against identity-committed resistance (higher theater, lower pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_locked_agents, conceptual, 'Whether sex-biology reading adherents are constrained or identity-locked').

omega_variable(
    committer_kernel_contest,
    'This constraint is one reading of the kernel ''woman_female_category.'' The sibling readings (sex_biology_reading, hybrid_contextual_reading) derive from incompatible premises about what ''woman'' and ''female'' mean. Is this a genuine three-way logical contest where only one reading can be legally true at a time, or are these three readings describing different contexts where all three meanings coexist?',
    'Examine whether legal systems must globally choose one reading or whether they can adopt context-dependent readings (identity-based for pronouns/social spaces; sex-based for criminal sentencing; hybrid for athletics). Analyze whether the sibling readings FORECLOSE each other in principle or merely COEXIST in different jurisdictions.',
    'If context-dependent coexistence is possible: all three readings are live (coexists_with relations). If one reading must be canonical: the relations shift toward foreclosure and hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Whether the gender identity, sex-biology, and hybrid readings are mutually exclusive or context-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_identity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gir_tr_t0, gender_identity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gir_tr_t5, gender_identity_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(gir_tr_t10, gender_identity_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(gir_tr_t8, gender_identity_reading, theater_ratio, 8, 0.42).

% Extraction over time
narrative_ontology:measurement(gir_be_t0, gender_identity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gir_be_t5, gender_identity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gir_be_t10, gender_identity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gir_be_t8, gender_identity_reading, base_extractiveness, 8, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(gender_identity_reading, hybrid_contextual_reading).
narrative_ontology:affects_constraint(gender_identity_reading, athletic_category_fairness).
narrative_ontology:affects_constraint(gender_identity_reading, custodial_space_allocation).

% DUAL FORMULATION NOTE:
% The woman_female_category kernel admits at least three structurally distinct readings: gender_identity_reading (this story), sex_biology_reading, and hybrid_contextual_reading. These are NOT alternative observables of one constraint but distinct constraints grounded in incompatible premises about category membership. Each reading has its own extractiveness value, its own beneficiary/victim structure, and its own classification landscape. They are linked by network.affects_constraints because each reading's institutional adoption affects the empirical coherence and authority of the others. The gender_identity_reading affects sex_biology_reading by providing an alternative legal framework that can displace it; it affects hybrid_contextual_reading by simplifying what the hybrid approach must manage. Downstream constraints (athletic_category_fairness, custodial_space_allocation) are affected by which reading is institutionally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gender_identity_reading, powerless, 0.95).
constraint_indexing:directionality_override(gender_identity_reading, organized, 0.18).
constraint_indexing:directionality_override(gender_identity_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
