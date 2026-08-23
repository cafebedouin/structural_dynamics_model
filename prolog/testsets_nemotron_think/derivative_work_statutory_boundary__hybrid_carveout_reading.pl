% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Hybrid Carveout: Commercial/Non-Commercial Derivative Work Boundary
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'hybrid carveout' reading of the
 *   statutory derivative work boundary (17 U.S.C. § 101, § 106(2)) as
 *   developed through U.S. case law from Sony through Google Books. The
 *   reading holds that the derivative work right is triggered categorically
 *   by commercial exploitation, while non-commercial transformative use is
 *   presumptively non-infringing. This creates a two-tier system: commercial
 *   developers face a licensing requirement (extraction), while
 *   non-commercial transformative users enjoy a categorical exemption
 *   (coordination). The constraint is a tangled rope because it performs a
 *   genuine coordination function (clearing the space for non-commercial
 *   transformative culture) while simultaneously extracting from commercial
 *   developers through the licensing gate. The commercial/non-commercial line
 *   is actively enforced by platform intermediaries and rights-holders,
 *   requiring ongoing suppression machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.45).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.48).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Hybrid Carveout: Commercial/Non-Commercial Derivative Work Boundary").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '74518ea1-53ca-4d1c-9973-74d1869e6b34').
narrative_ontology:cs_kernel_codification('74518ea1-53ca-4d1c-9973-74d1869e6b34', formalized).
narrative_ontology:cs_authority_grounding('74518ea1-53ca-4d1c-9973-74d1869e6b34', lineage).
narrative_ontology:cs_interpretation_layer_present('74518ea1-53ca-4d1c-9973-74d1869e6b34').
narrative_ontology:cs_reading_relation('74518ea1-53ca-4d1c-9973-74d1869e6b34', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('74518ea1-53ca-4d1c-9973-74d1869e6b34', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('74518ea1-53ca-4d1c-9973-74d1869e6b34', foundational, commercial_exploitation_triggers_derivative_right).
narrative_ontology:cs_axiom_status(commercial_exploitation_triggers_derivative_right, holdable).
narrative_ontology:cs_axiom_grounding('74518ea1-53ca-4d1c-9973-74d1869e6b34', commercial_exploitation_triggers_derivative_right, conventional).
narrative_ontology:cs_axiom('74518ea1-53ca-4d1c-9973-74d1869e6b34', foundational, non_commercial_transformative_exempt).
narrative_ontology:cs_axiom_status(non_commercial_transformative_exempt, holdable).
narrative_ontology:cs_axiom_grounding('74518ea1-53ca-4d1c-9973-74d1869e6b34', non_commercial_transformative_exempt, conventional).
narrative_ontology:cs_reference_frame('74518ea1-53ca-4d1c-9973-74d1869e6b34', statutory_derivative_work_boundary).
narrative_ontology:cs_drift_state('74518ea1-53ca-4d1c-9973-74d1869e6b34', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74518ea1-53ca-4d1c-9973-74d1869e6b34', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_exploitation_justifies_derivative_control).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_use_is_non_infringing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold statutory derivative work rights and enforce them through licensing regimes. They lobbied for and benefit from the commercial exploitation trigger, collecting license fees from commercial developers while the carveout limits their control over non-commercial transformative uses. They can shift enforcement strategies across jurisdictions and media formats.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_copyright_holders, beneficiary).

% Must obtain licenses for any commercial use that incorporates copyrighted expression, including transformative commercial works. The licensing cost is a direct extraction; their exit options are limited to licensing, litigating fair use (uncertain), or abandoning projects. They cannot easily reach audiences without using established platforms that enforce the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers, payer,
    organized, biographical, constrained, national).

% Create transformative works (fan fiction, remixes, commentary, educational adaptations) without commercial intent. The carveout grants them a categorical safe harbor — they do not need licenses and face minimal suppression. Their exit is mobile: they can create freely within the non-commercial sphere, though platform policies may impose additional constraints.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    moderate, biographical, mobile, national).

% Operate the distribution platforms (YouTube, GitHub, app stores, social media) where the boundary is enforced. They implement automated systems (Content ID, DMCA tooling) that operationalize the commercial/non-commercial distinction. They bear compliance costs and liability risk, but also benefit from the safe harbor provisions that the same statutory framework provides.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, payer).

% Adjudicate boundary disputes: what counts as 'commercial exploitation,' what qualifies as 'transformative,' and where the line falls for mixed-use cases. Their rulings shape the operational boundary. They do not directly extract or pay but their interpretive authority determines the constraint's effective shape.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% Make non-commercial verbatim copies or minimally altered uses (personal archives, private sharing). They fall outside the carveout — the statutory language does not clearly protect them — but are rarely targeted due to enforcement economics. They would object to being treated as infringers but have no organized voice in the policy process.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_non_transformative_users, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, categorical rule that resolves the derivative work boundary for the vast majority of non-commercial transformative activity, eliminating transaction costs and chilling effects for that class of users while preserving a licensing market for commercial exploitation.
% TRANSFER_FUNCTION: Moves licensing revenue from commercial developers to commercial copyright holders for any commercial use incorporating protected expression. Non-commercial transformative users pay zero; the transfer is categorically blocked for that class.
% ABSENT_VOICES: Non-commercial non-transformative users (personal copiers, archivists) are structurally excluded — the carveout does not clearly cover them, but enforcement economics makes them invisible. Small commercial developers who cannot afford licensing but don't qualify for the carveout are also absent from the legislative record.
% DISAPPEARANCE_RATIONALE: If the hybrid carveout vanished, non-commercial transformative creators would face infringement liability for works currently considered safe, chilling a massive volume of cultural production. Commercial licensing markets would expand to cover formerly exempt uses. The copyright system would lose its primary cultural legitimacy mechanism (the 'breathing space' for transformative non-commercial use).
% FOUNDING_PROBLEM: The pre-1976 Copyright Act provided no explicit derivative work right for non-dramatic works; courts struggled with the boundary between infringing adaptation and permissible use. The 1976 Act created the derivative work right but left the transformative/non-transformative and commercial/non-commercial lines to judicial development. The hybrid carveout emerged from case law (Sony, Campbell, Google Books) to prevent the derivative right from swallowing fair use entirely.
% FOUNDING_PROBLEM_CORROBORATION: Legislative history of the 1976 Act (House Report 94-1476) attests the derivative right was meant to be broad but subject to fair use. Supreme Court in Campbell v. Acuff-Rose (1994) and Authors Guild v. Google (2015) established the transformative use doctrine that the carveout codifies in practice. Commercial copyright holders contest that the carveout has expanded beyond the founding problem, arguing it now covers commercial-adjacent uses (Patreon-supported fan works, ad-supported transformative content).
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).
:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: commercial developers pay substantial licensing fees, but the carveout shields a large volume of activity. Suppression (0.48) is moderate: enforcement is real (DMCA, Content ID, litigation) but targeted at commercial uses; non-commercial users face minimal direct suppression. Theater ratio (0.25) reflects that the carveout is functionally real — it's not a sham — but platform over-enforcement (automated takedowns hitting non-commercial uses) introduces performative compliance. Accessibility collapse (0.52) is moderate: commercial developers have no practical alternative to licensing; non-commercial users have full alternatives. Resistance (0.42) comes mainly from commercial developers challenging scope and from platforms resisting compliance costs.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (commercial developers) experiences this as a snare-like extraction: licensing fees with no meaningful alternative. The beneficiary seat (non-commercial transformative users) experiences it as a rope: a clear, low-friction coordination rule. The agenda-setter seat (copyright holders) experiences it as a rope with acceptable leakage — they get the commercial market they wanted, the carveout is the price of legislative legitimacy. The engine will compute these divergent seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial copyright holders are agenda_setters and beneficiaries (d near 0.0) — they set the licensing terms and collect the revenue. Commercial developers are payers (d near 1.0) — they bear the full licensing cost with constrained exit. Non-commercial transformative users are beneficiaries (d near 0.0) — they receive a categorical safe harbor. Platform intermediaries are dual-positioned: agenda_setters in operationalizing the boundary, payers in compliance costs. Courts are analytical observers. Non-commercial non-transformative users are excluded — they fall in a doctrinal gap but enforcement economics makes them de facto unregulated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing the derivative right from swallowing fair use) remains live but contested. The carveout has expanded beyond its original justification (personal/educational use) to cover large-scale non-commercial platforms (Wikipedia, fan fiction archives, open source). Copyright holders argue this is mandatrophy — the arrangement now serves a function (mass non-commercial distribution) the founders didn't anticipate. Non-commercial users argue the founding problem is live because commercial copyright holders would eliminate the carveout entirely if they could. The constraint is not a piton — it has active beneficiaries on both sides and active enforcement — but the commercial/non-commercial line is under pressure from platform-mediated monetization (Patreon, YouTube ads, Substack) that blurs the categorical distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_exploitation_definition,
    'What constitutes ''commercial exploitation'' in the platform era — does indirect monetization (ads, Patreon, merchandise) trigger the derivative work right?',
    'Circuit court rulings on mixed-use cases (e.g., fan works on ad-supported platforms, open source with commercial support contracts), or legislative clarification.',
    'If indirect monetization counts as commercial, the carveout shrinks dramatically and extraction expands to currently exempt creators. If only direct sales count, the carveout remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_exploitation_definition, conceptual, 'Boundary of the commercial/non-commercial distinction in platform-mediated creation').

omega_variable(
    transformative_use_scope,
    'How transformative must a non-commercial use be to qualify for the carveout — does the Supreme Court''s ''new expression, meaning, or message'' test (Campbell) apply categorically?',
    'Empirical study of district court fair use rulings on non-commercial transformative works; Supreme Court guidance on the transformative use standard.',
    'A narrow transformative standard would exclude many fan works and remixes from the carveout, increasing extraction. A broad standard maintains the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformative_use_scope, empirical, 'Operational scope of the transformative use exemption for non-commercial works').

omega_variable(
    kernel_reading_ambiguity,
    'Does the statutory text (17 U.S.C. § 101 definition of ''derivative work'') structurally support a commercial/non-commercial split, or is the hybrid reading a judicial construction with no textual anchor?',
    'Textualist analysis of the 1976 Act''s definition and legislative history; comparison with the enclosure_reading''s textual argument.',
    'If the hybrid reading lacks textual support, it is vulnerable to Supreme Court reversal under a textualist majority — the constraint would shift toward enclosure_reading, increasing extraction substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the hybrid carveout is textually grounded or judicially constructed').

omega_variable(
    platform_enforcement_as_suppression,
    'Do automated enforcement systems (Content ID, copyright strikes) constitute structural suppression of non-commercial transformative uses that nominally fall within the carveout?',
    'Empirical measurement of false positive rates on transformative non-commercial content; analysis of chilling effects on creator behavior.',
    'If platform over-enforcement effectively nullifies the carveout for many users, the constraint''s effective suppression is higher than the statutory text suggests, and the coordination function is degraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_enforcement_as_suppression, empirical, 'Gap between statutory carveout and platform-enforced boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 1976, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t1976, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(deri_tr_t1990, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(deri_tr_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(deri_tr_t2005, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(deri_tr_t2025, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(deri_be_t1976, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 1976, 0.25).
narrative_ontology:measurement(deri_be_t1990, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(deri_be_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(deri_be_t2005, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(deri_be_t2025, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t1976, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement(deri_su_t1990, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(deri_su_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement(deri_su_t2005, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement(deri_su_t2025, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.03).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, dmca_safe_harbor).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_copyright_policy).

% DUAL FORMULATION NOTE:
% Part of the derivative_work_statutory_boundary constraint family. The enclosure_reading (constraint_id: derivative_work_statutory_boundary__enclosure_reading) treats any incorporation as derivative; the coordination_reading (constraint_id: derivative_work_statutory_boundary__coordination_reading) treats only fixed substantial recastings as derivative. This hybrid reading creates a commercial/non-commercial split. The three readings compete for judicial adoption; the hybrid reading currently dominates in the 2nd and 9th Circuits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, institutional, 0.15).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, organized, 0.85).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, moderate, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
