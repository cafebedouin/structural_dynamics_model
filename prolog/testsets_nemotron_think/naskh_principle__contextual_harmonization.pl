% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__contextual_harmonization, []).

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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Quranic Contextual Harmonization Reading (Naskh Principle)
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The naskh (abrogation) principle is a contested kernel in Islamic legal
 *   theory. This story instantiates the contextual_harmonization reading: all
 *   Quranic verses remain legally valid within their specific revelatory and
 *   situational contexts; apparent contradictions are resolved by specifying
 *   the context of application for each verse, not by declaring earlier
 *   verses abrogated by later ones. This reading coordinates theological
 *   coherence (no verse is invalid) with legal adaptability (context
 *   determines applicability). It extracts from legal predictability (no
 *   bright-line chronological rule) and from traditionalist jurist authority
 *   (the power to definitively close questions via naskh declarations). The
 *   constraint requires active enforcement through scholarly
 *   consensus-building, institutional curriculum reform, and fatwa
 *   methodology shifts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.42).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.38).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.42).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Quranic Contextual Harmonization Reading (Naskh Principle)").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, 'ecee723d-a724-4858-98c7-2fa61bacf1e6').
narrative_ontology:cs_kernel_codification('ecee723d-a724-4858-98c7-2fa61bacf1e6', fixed_text).
narrative_ontology:cs_authority_grounding('ecee723d-a724-4858-98c7-2fa61bacf1e6', lineage).
narrative_ontology:cs_interpretation_layer_present('ecee723d-a724-4858-98c7-2fa61bacf1e6').
narrative_ontology:cs_reading_relation('ecee723d-a724-4858-98c7-2fa61bacf1e6', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('ecee723d-a724-4858-98c7-2fa61bacf1e6', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('ecee723d-a724-4858-98c7-2fa61bacf1e6', foundational, all_verses_perpetually_valid).
narrative_ontology:cs_axiom_status(all_verses_perpetually_valid, holdable).
narrative_ontology:cs_axiom_grounding('ecee723d-a724-4858-98c7-2fa61bacf1e6', all_verses_perpetually_valid, deontological).
narrative_ontology:cs_axiom('ecee723d-a724-4858-98c7-2fa61bacf1e6', foundational, context_specifies_not_abrogates).
narrative_ontology:cs_axiom_status(context_specifies_not_abrogates, holdable).
narrative_ontology:cs_axiom_grounding('ecee723d-a724-4858-98c7-2fa61bacf1e6', context_specifies_not_abrogates, conventional).
narrative_ontology:cs_reference_frame('ecee723d-a724-4858-98c7-2fa61bacf1e6', classical_naskh_framework).
narrative_ontology:cs_drift_state('ecee723d-a724-4858-98c7-2fa61bacf1e6', modern_reformist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ecee723d-a724-4858-98c7-2fa61bacf1e6', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, reformist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, theologians_seeking_coherence).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, adaptive_legal_systems).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, lay_muslims_seeking_contextual_guidance).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, traditionalist_jurists).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, legal_predictability_seekers).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, definitive_authority_claimants).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, classical_abrogation_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, lay_muslims_seeking_guidance).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, lay_muslims_seeking_guidance).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, islamic_legal_institutions).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, contextual_specification_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, theological_coherence_principle).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, perpetual_validity_of_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance contextual harmonization as methodology; gain interpretive authority and relevance in modern contexts; their career advancement depends on institutional acceptance of this reading. Exit to classical positions is professionally costly but possible.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, reformist_jurists, agenda_setter,
    organized, generational, mobile, global).

% Hold authority through classical naskh doctrine; lose definitive closure power when all verses remain potentially applicable; their institutional position (madrasa appointments, fatwa authority) is fused with the classical framework. Exit means surrendering professional identity.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, traditionalist_jurists, payer,
    institutional, generational, identity_locked, global).

% Receive flexible guidance adaptable to modern circumstances (benefit); lose clear bright-line rules for daily practice (cost); constrained by local scholarly environment — cannot easily choose interpretive framework independently.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, lay_muslims_seeking_guidance, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, lay_muslims_seeking_guidance, payer).

% Some institutions (e.g., Al-Azhar, IIUM) selectively adopt contextual methods for family law while retaining classical naskh for worship; they bear transition costs and legitimacy challenges from both reformist and traditionalist constituencies.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, islamic_legal_institutions, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, islamic_legal_institutions, payer).

% Their reading is structurally marginalized in reformist academic and some institutional spaces; they would object that contextual harmonization dissolves legal certainty, but their voice is filtered out of key deliberative forums (international conferences, reform-oriented journals).
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_abrogation_adherents, excluded,
    organized, generational, trapped, global).

% Hold a distinct middle reading (revelation progressively restricts); excluded from both classical and contextual-harmonization coalitions; would argue contextual harmonization ignores historical trajectory of revelation.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, progressive_restriction_adherents, excluded,
    organized, generational, constrained, global).

% Study the interpretive contest from outside the tradition; no material stake in any reading's victory; provide historical-philological data that all sides selectively cite.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent Quranic contradictions while preserving all verses' perpetual validity through contextual specification rather than chronological invalidation, maintaining theological coherence across changing circumstances.
% TRANSFER_FUNCTION: Moves interpretive authority from chronological hierarchy (later verse overrides earlier) to contextual analysis (each verse applies in its proper situation); shifts closure power from jurists who declare abrogation to context-analysts who specify applicability.
% ABSENT_VOICES: Classical abrogation scholars who argue this reading undermines legal certainty and opens the door to subjective interpretation; progressive restriction scholars who argue it ignores the demonstrable historical trajectory from permission to restriction in Quranic legislation; both are excluded from reformist-dominated deliberative spaces.
% DISAPPEARANCE_RATIONALE: If contextual harmonization vanished overnight, Islamic legal methodology would revert to classical naskh as dominant paradigm; verses currently applied contextually (e.g., warfare verses, inheritance modifications) would be declared abrogated or restricted; reformist jurisprudence would lose its primary methodological tool; madrasa curricula and fatwa institutions would reorganize around classical chronology.
% FOUNDING_PROBLEM: How to reconcile apparently contradictory Quranic verses (e.g., 'no compulsion in religion' vs. 'fight the polytheists'; 'inherit by fixed shares' vs. 'bequest to parents') without declaring any divine verse invalid or inoperative.
% FOUNDING_PROBLEM_CORROBORATION: Early jurists Ibn al-Qayyim (d. 751/1350) and Shah Waliullah (d. 1176/1762) argued for contextual harmonization over abrogation; modern scholars Fazlur Rahman, Abdullahi An-Na'im, and Khaled Abou El Fadl corroborate from outside the classical beneficiary set. Classical majority tradition (Shafi'i, Tabari, Ibn Kathir, Suyuti) corroborates abrogation as the solution. No consensus exists.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__contextual_harmonization, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__contextual_harmonization, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__contextual_harmonization_tests).
:- end_tests(naskh_principle__contextual_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the genuine cost to legal predictability and jurist closure-power — not material extraction but epistemic-institutional extraction. Suppression (0.38) measures the constraint's dependence on marginalizing classical abrogation discourse in reformist spaces. Theater is low (0.18): contextual harmonization is a genuine interpretive methodology with substantive philological and usuli arguments, not performative. Accessibility collapse is moderate (0.35): classical naskh remains a live, taught, and practiced alternative. Resistance (0.55) captures the organized traditionalist pushback. Measurements show gradual rise from early formative period (150 AH) through classical crystallization (350-550 AH) to modern reformist ascendancy (950+ AH).
 *
 * PERSPECTIVAL GAP:
 *   From the reformist seat, this is a rope (pure coordination: solves contradiction problem with minimal coercion). From the traditionalist seat, it is a snare (extraction of their closure-authority under cover of coherence). From the lay Muslim seat, it is a tangled rope (genuine coordination benefit + real predictability cost). The engine will compute this divergence; the authored claim (tangled_rope) reflects the structural reality that both coordination and asymmetric extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist jurists are structural beneficiaries (d ≈ 0.15): they gain interpretive authority, institutional positions, and relevance. Traditionalist jurists are structural targets (d ≈ 0.85): their identity-fused authority erodes. Lay Muslims sit near symmetric (d ≈ 0.5): genuine coordination benefit (adaptable guidance) vs. genuine cost (lost predictability). Islamic legal institutions are dual-positioned: agenda_setters where they adopt the reading, payers where they bear transition costs. Classical abrogation adherents are excluded (trapped exit): their reading is filtered from key forums. The engine computes per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling verses without invalidation) remains contested — classical school says naskh solves it; this reading says naskh was misunderstood. The constraint is NOT mandatrophy-resolved: the arrangement persists because the founding problem is live for reformists, not because the original solution atrophied. Classical naskh doctrine shows mandatrophy signals (persists by inertia in some institutions despite reformist critique), but this reading is the active challenger, not the atrophied remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_specification_operationalizability,
    'Can contextual specification be operationalized into determinate legal rules without collapsing into juristic subjectivity?',
    'Comparative analysis of fatwa outputs across contexts using this methodology; test whether different jurists reach convergent context-specifications for the same verses.',
    'If operationalization fails, the reading''s coordination function collapses — it becomes a cover for unconstrained interpretation (snare). If it succeeds, the reading sustains as genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_specification_operationalizability, empirical, 'Whether contextual harmonization yields determinate law or subjective discretion.').

omega_variable(
    implicit_verse_privileging,
    'Does contextual harmonization implicitly privilege Meccan (universal/ethical) verses over Medinan (legal/specific) verses in practice?',
    'Corpus analysis of which verses are treated as ''contextually universal'' vs. ''contextually restricted'' in reformist jurisprudence; check for systematic bias toward ethical-verses-over-legal-verses.',
    'If privileging is systematic, the reading extracts from Medinan legal verses'' authority while claiming to preserve all verses equally — a hidden extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_verse_privileging, conceptual, 'Whether the reading''s neutrality claim masks asymmetric treatment of verse categories.').

omega_variable(
    committer_structure_naskh_kernel,
    'How does the contested kernel structure (three readings of naskh_principle) affect this reading''s classification stability?',
    'Track classification shifts if sibling readings gain/lose institutional dominance; measure cross-reading contamination via shared terminology and citation networks.',
    'If classical_abrogation loses institutional dominance, this reading''s extraction profile changes (fewer traditionalist jurists to extract from). Kernel-level dynamics are part of this constraint''s structural environment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_naskh_kernel, conceptual, 'Kernel-level committer structure: this reading''s extraction and coordination are relational to sibling readings'' institutional fortunes.').

omega_variable(
    suppression_mechanism_in_scholarly_spaces,
    'Is the marginalization of classical abrogation discourse in reformist spaces structural (institutional gatekeeping) or internalized (self-censorship by traditionalists)?',
    'Survey traditionalist junior scholars on whether they avoid contextual-harmonization venues due to explicit exclusion or anticipated hostility; compare publication acceptance rates.',
    'If internalized, suppression is higher than structural metrics capture — traditionalists carry the exclusion with them. If structural, suppression is institutionally addressable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_scholarly_spaces, empirical, 'Structural vs. internalized suppression in the interpretive contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 150, 1450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t150, naskh_principle__contextual_harmonization, theater_ratio, 150, 0.05).
narrative_ontology:measurement(nask_tr_t350, naskh_principle__contextual_harmonization, theater_ratio, 350, 0.08).
narrative_ontology:measurement(nask_tr_t550, naskh_principle__contextual_harmonization, theater_ratio, 550, 0.12).
narrative_ontology:measurement(nask_tr_t750, naskh_principle__contextual_harmonization, theater_ratio, 750, 0.15).
narrative_ontology:measurement(nask_tr_t950, naskh_principle__contextual_harmonization, theater_ratio, 950, 0.17).
narrative_ontology:measurement(nask_tr_t1150, naskh_principle__contextual_harmonization, theater_ratio, 1150, 0.18).
narrative_ontology:measurement(nask_tr_t1350, naskh_principle__contextual_harmonization, theater_ratio, 1350, 0.18).
narrative_ontology:measurement(nask_tr_t1450, naskh_principle__contextual_harmonization, theater_ratio, 1450, 0.18).

% Extraction over time
narrative_ontology:measurement(nask_be_t150, naskh_principle__contextual_harmonization, base_extractiveness, 150, 0.15).
narrative_ontology:measurement(nask_be_t350, naskh_principle__contextual_harmonization, base_extractiveness, 350, 0.22).
narrative_ontology:measurement(nask_be_t550, naskh_principle__contextual_harmonization, base_extractiveness, 550, 0.28).
narrative_ontology:measurement(nask_be_t750, naskh_principle__contextual_harmonization, base_extractiveness, 750, 0.33).
narrative_ontology:measurement(nask_be_t950, naskh_principle__contextual_harmonization, base_extractiveness, 950, 0.38).
narrative_ontology:measurement(nask_be_t1150, naskh_principle__contextual_harmonization, base_extractiveness, 1150, 0.4).
narrative_ontology:measurement(nask_be_t1350, naskh_principle__contextual_harmonization, base_extractiveness, 1350, 0.42).
narrative_ontology:measurement(nask_be_t1450, naskh_principle__contextual_harmonization, base_extractiveness, 1450, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t150, naskh_principle__contextual_harmonization, suppression_requirement, 150, 0.1).
narrative_ontology:measurement(nask_su_t350, naskh_principle__contextual_harmonization, suppression_requirement, 350, 0.18).
narrative_ontology:measurement(nask_su_t550, naskh_principle__contextual_harmonization, suppression_requirement, 550, 0.25).
narrative_ontology:measurement(nask_su_t750, naskh_principle__contextual_harmonization, suppression_requirement, 750, 0.3).
narrative_ontology:measurement(nask_su_t950, naskh_principle__contextual_harmonization, suppression_requirement, 950, 0.35).
narrative_ontology:measurement(nask_su_t1150, naskh_principle__contextual_harmonization, suppression_requirement, 1150, 0.37).
narrative_ontology:measurement(nask_su_t1350, naskh_principle__contextual_harmonization, suppression_requirement, 1350, 0.38).
narrative_ontology:measurement(nask_su_t1450, naskh_principle__contextual_harmonization, suppression_requirement, 1450, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__contextual_harmonization, 0.08).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, maqasid_shariah_methodology).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, istihsan_juristic_preference).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, maslaha_public_interest).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, quranic_hermeneutic_circularity).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, classical_naskh_doctrine).

% DUAL FORMULATION NOTE:
% Part of naskh_principle constraint family: classical_abrogation (naskh_principle__classical_abrogation), progressive_restriction (naskh_principle__progressive_restriction). All three share the kernel 'how to reconcile apparently contradictory Quranic verses' but instantiate different constraints with different ε, beneficiary/victim structures, and coordination/extraction profiles. This reading (contextual_harmonization) coordinates theological coherence; classical_abrogation coordinates legal certainty; progressive_restriction coordinates historical pedagogy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, institutional, 0.75).
constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
