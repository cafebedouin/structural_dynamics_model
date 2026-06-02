% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA §702 Foreign Target Statutory Text (Strict Reading)
 *   domain: constitutional_law/national_security/surveillance
 *
 * SUMMARY:
 *   FISA §702 is a statutory authorization for bulk collection of foreign
 *   communications. The statute contains contested language about the
 *   permissible scope of collection, incidental collection of U.S. person
 *   communications, minimization procedures, and access restrictions. This
 *   story instantiates ONE reading of the statute: the strict foreign-target
 *   reading, where the statutory text is read to require that collection be
 *   directed at foreign persons abroad, U.S. person incidental data must be
 *   minimized (operationalized as deletion rather than mere access
 *   restriction), and FBI access to incidentally collected data for domestic
 *   purposes is prohibited. This reading preserves Fourth Amendment
 *   protections by excluding U.S. persons from the collection target set
 *   entirely — they are not victims of the constraint as written. The
 *   constraint functions as pure coordination (Rope) for the intelligence
 *   community and foreign governments, and as either rope or tangled rope
 *   from the U.S. person incidental-exposure perspective, depending on
 *   whether minimization is operationalized as deletion or access
 *   restriction. The analytical observer risks misclassifying the statutory
 *   text as a natural law (Mountain) when it is actually a contested kernel
 *   with multiple defensible readings and accumulated practice variance.
 *
 * KEY AGENTS:
 *   - Intelligence Community (NSA, CIA, FBI): Primary beneficiary (institutional/arbitrage) — benefits from §702 collection authority; has arbitrage options (can modify procedures, seek additional authorities)
 *   - Foreign Governments: Secondary target (powerful/mobile) — their communications are lawfully collected; have mobile options (encryption, changed communication patterns) but cannot exit collection entirely
 *   - U.S. Persons Incidentally Exposed: Potential victim depending on minimization operational definition (moderate/constrained) — may bear extraction if incidentally collected data is retained and accessed; have constrained exit options
 *   - Congress: Co-authorizer (organized/mobile) — authorized §702 and can revoke or modify; has mobile exit options
 *   - FISA Court: Reviewer/Certifier (institutional/mobile) — reviews §702 certifications and can restrict collection; has mobile options to impose restrictions or revoke authorization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — must assess whether the statutory text is a stable legal constraint or a contested kernel with multiple readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.18).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.35).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA §702 Foreign Target Statutory Text (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '2da72149-ff43-487b-bdb7-71d1468874a1').
narrative_ontology:cs_kernel_codification('2da72149-ff43-487b-bdb7-71d1468874a1', formalized).
narrative_ontology:cs_authority_grounding('2da72149-ff43-487b-bdb7-71d1468874a1', lineage).
narrative_ontology:cs_interpretation_layer_present('2da72149-ff43-487b-bdb7-71d1468874a1').
narrative_ontology:cs_reading_relation('2da72149-ff43-487b-bdb7-71d1468874a1', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('2da72149-ff43-487b-bdb7-71d1468874a1', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('2da72149-ff43-487b-bdb7-71d1468874a1', foundational, us_persons_fourth_amendment_protected).
narrative_ontology:cs_axiom_status(us_persons_fourth_amendment_protected, holdable).
narrative_ontology:cs_axiom_grounding('2da72149-ff43-487b-bdb7-71d1468874a1', us_persons_fourth_amendment_protected, deontological).
narrative_ontology:cs_axiom('2da72149-ff43-487b-bdb7-71d1468874a1', foundational, foreign_target_collection_authorized).
narrative_ontology:cs_axiom_status(foreign_target_collection_authorized, holdable).
narrative_ontology:cs_axiom_grounding('2da72149-ff43-487b-bdb7-71d1468874a1', foreign_target_collection_authorized, deontological).
narrative_ontology:cs_reference_frame('2da72149-ff43-487b-bdb7-71d1468874a1', statutory_foreign_target_regime).
narrative_ontology:cs_drift_state('2da72149-ff43-487b-bdb7-71d1468874a1', contemporary_post_snowden_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2da72149-ff43-487b-bdb7-71d1468874a1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_operations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTELLIGENCE COMMUNITY / AUTHORIZED COLLECTION (ROPE) — FISA §702 in strict statutory reading provides pure coordination: the statutory text constrains collection to foreign targets abroad with U.S. person data minimized and inaccessible for domestic purposes. The intelligence community coordinates through this constraint to achieve foreign intelligence objectives while respecting Fourth Amendment floors for U.S. persons. Low extraction because the constraint aligns with legitimate foreign intelligence authority.
constraint_indexing:constraint_classification(fisa_702_statutory_text__foreign_target_strict_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: FOREIGN GOVERNMENTS / COMMUNICATIONS TARGETS (ROPE) — From the perspective of foreign governments whose communications are lawfully targeted under §702 strict reading (foreign persons abroad, legitimate foreign intelligence interests), the constraint functions as a coordination mechanism: the statutory text defines a transparent regime that foreign actors can model and plan within. They cannot exit collection (no arbitrage option available), but they experience the constraint as stable coordination rather than deceptive extraction. Low effective extraction derives from the clarity and predictability of the foreign-targets-only rule.
constraint_indexing:constraint_classification(fisa_702_statutory_text__foreign_target_strict_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (NATURAL LAW VIEW / MOUNTAIN) — The §702 statutory text in strict reading declares a clear, enforceable boundary: foreign targets abroad vs. U.S. persons. From a civilizational analytical view, this boundary appears as an immutable legal-constitutional limit — Fourth Amendment protections for U.S. persons are non-negotiable, and the statutory text encodes this floor directly. The constraint emerges naturally from the constitutional structure (Fourth Amendment), not from contingent policy choice. This perspective is subject to false summit detection if structural beneficiaries can be identified within the surveillance apparatus itself.
constraint_indexing:constraint_classification(fisa_702_statutory_text__foreign_target_strict_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: U.S. PERSONS / INCIDENTAL EXPOSURE (TANGLED ROPE) — U.S. persons whose communications are incidentally collected while monitoring foreign targets face a mixed constraint. The §702 statutory text provides coordination value: minimization rules and access restrictions theoretically protect their data. But they bear real extraction: their communications are collected without individualized warrant, retained in databases potentially queryable by law enforcement (even under strict reading, some access pathways exist), and subject to derivative use where incidental collection informs foreign intelligence that downstream enables domestic law enforcement prosecution. They have constrained exit options — they can change communication patterns or use encryption, but cannot fully exit exposure in a globally networked world.
constraint_indexing:constraint_classification(fisa_702_statutory_text__foreign_target_strict_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL OVERSIGHT & JUDICIARY (ROPE) — The FISA §702 statutory text in strict reading coordinates relationships between the executive (authorized to conduct collection), Congress (authorized to mandate minimization and access restrictions), and the judiciary (authorized to review certifications and approve guidelines). Each branch has exit options (Congress can revoke, judiciary can restrict), but the constraint functions primarily as coordination of checks and balances. Low extraction derives from explicit shared authority and clear statutory boundaries.
constraint_indexing:constraint_classification(fisa_702_statutory_text__foreign_target_strict_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Under the strict foreign-target reading, the statutory text constrains collection to foreign persons abroad and requires minimization/deletion of U.S. person data. U.S. persons are not in the victim set — they retain Fourth Amendment protections and are excluded from targeting. The modest extractiveness value (not zero) reflects: (1) residual risk that incidental collection may occur and may not be fully deleted in practice, (2) derivative uses where incidentally collected data informs foreign intelligence that downstream enables domestic law enforcement, and (3) practice variance where actual implementation may not conform to the strict statutory reading. The value is lowest possible while acknowledging that no constraint is perfectly frictionless. Suppression (0.35): Moderate. The statute does impose barriers to lawful challenge — the state action is statutory authorization, the primary challenge vector is Congressional/judicial oversight rather than individual right-holders, and the statute's legitimacy depends partly on classified intelligence justifications. But suppression is not high because Congress retains revision authority and the FISA Court can restrict collection. Theater ratio (0.42): Moderate-Low. The §702 statutory certification process requires that the government file certifications with the FISA Court, but the FISA Court's review is on a deferential standard and classified materials prevent public verification. There is genuine coordination function (the statute coordinates between executive collection authority and judicial oversight), but also theatrical elements (certification language is standardized and formulaic, actual compliance is opaque to the public, Congress receives only aggregate briefings). The theater value reflects that §702 includes both real checks (FISA Court review, Congressional briefing) and performative elements (the secrecy regime itself prevents independent verification of compliance).
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives derives from different structural relationships to the foreign-target constraint. The intelligence community sees a coordination mechanism (Rope) — the statute authorizes foreign collection while establishing procedures. Foreign governments see a stable regime they can model (Rope) — lawful foreign targets abroad, transparent rules. U.S. persons incidentally exposed see either rope (if minimization means deletion) or tangled rope (if minimization means only access restriction) — they are not victims under the strict reading but may bear residual extraction if practice deviates. Congress and FISA Court see coordination and oversight (Rope) — shared authority and checks. The analytical observer risks seeing a natural law (Mountain) when the statutory text is actually a contested kernel where different readings produce different victim sets and different extraction mechanisms. The false summit detection critical for this constraint: if practice has drifted from the statutory text, the mountain perspective becomes misplaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position. The intelligence community as beneficiary with arbitrage options has d≈0.05 (full beneficiary position — they can choose collection parameters, seek additional authorities, modify procedures). Foreign governments as targets with mobile exit options have d≈0.50 (symmetric — they cannot exit collection but have some capacity to evade through encryption/pattern changes). U.S. persons incidentally exposed have higher d depending on whether their data is actually deleted or merely restricted in access — if deleted, d≈0.15 (minimal exposure); if retained, d≈0.60 (significant exposure). Congress and FISA Court as reviewers have d≈0.40 (able to impose restrictions but not fully controlling collection). The strict reading produces lower d values across all perspectives because U.S. persons are not in the victim set — they are excluded from the extraction mechanism by the statute's plain language.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the §702 statutory text in strict reading is NOT pure extraction (Snare) — it includes genuine coordination function (authorization + oversight) and excludes U.S. persons from the victim set entirely. The constraint is Rope because it coordinates between executive authority (foreign collection) and legislative/judicial oversight (minimization, access restriction, compliance review). The false summit risk (analytical observer perspective as Mountain) must be addressed through omega variables documenting: (1) whether the strict reading is actually operative or has been superseded by executive reinterpretation, (2) whether practice conforms to statute, (3) what the actual victim set is under current implementation. The mandatrophy is resolved by keeping U.S. persons out of the victim set (per the strict reading) while acknowledging omega ambiguity about whether that statutory protection is actually enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incidental_collection_scope_ambiguity,
    'Does §702 strict statutory reading prohibit FBI queries into the 702 database for domestic crime information when the query result is foreign intelligence derived from lawful foreign target collection?',
    'Statutory interpretation analysis of §702(h)(1) minimization requirements vs. (b)(4) certification requirements; Inspector General audits documenting actual query practices; comparison of statutory text to executive guidance and DOJ interpretations',
    'If strict reading: FBI has zero domestic query access even to incidentally collected U.S. person data. If moderate reading: FBI can query if the query nexus is foreign intelligence purpose, even if result includes derivative domestic intelligence. The distinction determines whether U.S. persons in tangled_rope perspective experience rope (coordination with access gates) or snare (warrantless access via foreign intelligence pretext).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidental_collection_scope_ambiguity, conceptual, 'Whether §702 strict reading permits FBI domestic queries via foreign intelligence pretext').

omega_variable(
    minimization_operational_definition,
    'What operational definition of ''minimization'' satisfies §702 statutory text? Is minimization deletion, access restriction, use restriction, or all three?',
    'NSA/FBI/DOJ guidance documents on minimization procedures; FISA Court orders establishing minimization standards; Inspector General reports on compliance with minimization rules; comparison to Fourth Amendment reasonable-expectation-of-privacy standards',
    'If minimization = deletion: incidentally collected U.S. person data must be removed from database, reducing extraction to zero. If minimization = access restriction only: U.S. person data remains but is behind access gates, maintaining exposure risk and tangled_rope classification. If minimization = use restriction only: data is accessible but restricted in how it can be used, maintaining high extraction for U.S. persons via metadata or derivative uses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimization_operational_definition, conceptual, 'Whether minimization means deletion, access restriction, or use restriction').

omega_variable(
    foreign_person_definition_boundary,
    'What determines foreign person status under §702 strict reading? Is it citizenship, residence, nationality, intent, or contact patterns?',
    'FISA Court precedent on foreign person definitions; NSA targeting procedures; DOJ guidance on nationality and citizenship determinations; analysis of cases where foreign person status was contested (dual nationals, permanent residents, persons abroad with U.S. connections)',
    'If status is objective (citizenship/nationality): boundary is stable and enables confident targeting decisions; constraint operates as mountain from IC perspective. If status is subjective or contextual (contact patterns, suspected foreign allegiance): boundary becomes porous and judgment-dependent; constraint degrades toward tangled_rope or scaffold (sunset on dual nationals, periodic re-certification). The strictness of the reading depends critically on whether the foreign/domestic boundary is sharp or fuzzy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_person_definition_boundary, empirical, 'Definition of ''foreign person'' under §702 targeting requirements').

omega_variable(
    statutory_reading_versus_practice_drift,
    'Does the actual implementation of §702 conform to the strict statutory reading, or has executive practice accumulated deviations that constitute de facto reinterpretation?',
    'Inspector General audits comparing statutory requirements to actual NSA/FBI procedures; FISA Court orders and opinions addressing compliance gaps; Congressional testimony on implementation practices; comparison of statutory text to published DOJ guidance and NSA targeting/minimization procedures',
    'If practice conforms: this reading is stable and constrains behavior as intended. If practice has drifted: the constraint has been eroded (or reinterpreted) through administrative action, and the statutory text''s protective force is weaker than this reading assumes. Major implication: if practice diverges from strict reading, the mountain perspective becomes a false summit — the statutory text appears as natural law but is actually contested through implementation variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_reading_versus_practice_drift, empirical, 'Conformity of actual §702 implementation to strict statutory reading').

omega_variable(
    reading_instantiation_versus_sibling_readings,
    'Which reading (strict foreign-target, incidental-collection permissive, constitutional-floor) governs the actual legal regime at any given moment? Can all three coexist in parallel interpretations, or does one foreclose the others?',
    'FISA Court precedent establishing which reading the judiciary adopts; Congressional explicit legislative language (absent explicit language, the reading that dominates practice); DOJ Office of Legal Counsel opinions; comparative analysis of statutory text language across all three readings to identify logical contradictions vs. complementary areas',
    'If readings foreclose each other: only one can be correct, and the constraint''s classification is indeterminate pending resolution. If readings coexist: different audiences (Congress, judiciary, executive, civil liberties groups) hold different readings simultaneously, and the constraint has structural ambiguity built in. This is an omega variable documenting the committer ambiguity itself — what this story (strict reading) assumes about its relationship to siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_versus_sibling_readings, conceptual, 'Logical relationship between strict foreign-target reading and sibling readings of §702').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa702_ft_theater_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fisa702_ft_theater_t7, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 7, 0.42).
narrative_ontology:measurement(fisa702_ft_theater_t13, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 13, 0.48).

% Extraction over time
narrative_ontology:measurement(fisa702_ft_extractiveness_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(fisa702_ft_extractiveness_t7, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 7, 0.18).
narrative_ontology:measurement(fisa702_ft_extractiveness_t13, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 13, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_warrant_requirement).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_exception_to_warrant).

% DUAL FORMULATION NOTE:
% FISA §702 instantiates three structurally distinct constraints (strict, permissive, floor readings). Each has different ε, different victim sets, different classifications. They are linked as a kernel family because they all interpret the same statutory text, but they are separate constraints with separate stories. Upstream constraints (Fourth Amendment warrant requirement, foreign intelligence exception) establish the constitutional context that all three readings operate within. The strict reading most closely respects the Fourth Amendment floor; the permissive and floor-only readings tolerate greater deviation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
