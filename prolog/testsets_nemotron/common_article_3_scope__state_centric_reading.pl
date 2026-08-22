% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope — State-Centric Threshold Reading
 *   domain: international_law/humanitarian_law
 *
 * SUMMARY:
 *   The state-centric reading of Common Article 3 scope asserts that the
 *   provision applies only when a non-international armed conflict meets
 *   specific intensity and organization thresholds — sustained and concerted
 *   military operations by a non-state group under responsible command. Below
 *   these thresholds, situations are classified as internal disturbances,
 *   tensions, or law enforcement matters, excluding CA3 protections. This
 *   reading has been the dominant state practice since 1949 but has expanded
 *   significantly post-2001 to cover counter-terrorism operations and
 *   low-intensity conflicts where states deny the existence of armed
 *   conflict. The constraint operates as a snare: the threshold
 *   classification is presented as a legal/technical determination but
 *   structurally functions to deny IHL protections to identifiable victim
 *   groups while preserving state operational discretion. The coordination
 *   function (legal certainty, law enforcement primacy) is real but the
 *   extraction (denial of protections to powerless groups) is asymmetric and
 *   actively enforced through classification authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.85).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, snare).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope — State-Centric Threshold Reading").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_law/humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '36623c81-96e5-4edc-b543-01f12eea3042').
narrative_ontology:cs_kernel_codification('36623c81-96e5-4edc-b543-01f12eea3042', formalized).
narrative_ontology:cs_authority_grounding('36623c81-96e5-4edc-b543-01f12eea3042', lineage).
narrative_ontology:cs_interpretation_layer_present('36623c81-96e5-4edc-b543-01f12eea3042').
narrative_ontology:cs_reading_relation('36623c81-96e5-4edc-b543-01f12eea3042', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('36623c81-96e5-4edc-b543-01f12eea3042', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('36623c81-96e5-4edc-b543-01f12eea3042', foundational, threshold_determination_is_state_prerogative).
narrative_ontology:cs_axiom_status(threshold_determination_is_state_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('36623c81-96e5-4edc-b543-01f12eea3042', threshold_determination_is_state_prerogative, conventional).
narrative_ontology:cs_axiom('36623c81-96e5-4edc-b543-01f12eea3042', foundational, law_enforcement_paradigm_primacy_below_threshold).
narrative_ontology:cs_axiom_status(law_enforcement_paradigm_primacy_below_threshold, holdable).
narrative_ontology:cs_axiom_grounding('36623c81-96e5-4edc-b543-01f12eea3042', law_enforcement_paradigm_primacy_below_threshold, conventional).
narrative_ontology:cs_reference_frame('36623c81-96e5-4edc-b543-01f12eea3042', diplomatic_conference_1949_threshold_compromise).
narrative_ontology:cs_drift_state('36623c81-96e5-4edc-b543-01f12eea3042', post_2001_counterterrorism_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36623c81-96e5-4edc-b543-01f12eea3042', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_armed_forces).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, government_legal_advisors).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_zones).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, non_state_armed_groups_below_threshold).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_sovereignty_in_conflict_classification).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, threshold_based_ihl_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author the threshold criteria through military doctrine and legal opinions; apply CA3 only when conflict crosses intensity/organization thresholds. Retain full operational discretion below thresholds including use of domestic law enforcement paradigms. Benefit from the legal space the threshold creates for counter-insurgency and counter-terrorism operations without IHL constraints.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_armed_forces, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce the legal interpretations that sustain the threshold reading; their professional standing and career advancement depend on maintaining the state's classification authority. They do not directly extract but their institutional position is constituted by the reading's persistence.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, government_legal_advisors, beneficiary,
    institutional, biographical, constrained, national).

% Fighters in non-international armed conflicts below the intensity/organization threshold (e.g. sporadic violence, localized uprisings, criminalized insurgency). Denied combatant status, prisoner-of-war protections, and CA3 minimum guarantees. Subject to domestic criminal law with no IHL floor. Often face execution, disappearance, or indefinite detention without the procedural protections CA3 would require if the threshold were met.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Civilians living in areas of sustained low-level violence that does not meet the CA3 threshold. Denied the protective framework CA3 would provide (humane treatment guarantees, prohibition on violence to life and person, judicial guarantees). State security operations in these zones operate under law enforcement rules that permit lethal force with less restrictive proportionality analysis than IHL.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_zones, payer,
    powerless, biographical, constrained, regional).

% Organized armed groups that fail the 'responsible command' or 'sustained and concerted military operations' criteria. Their members are treated as criminals rather than combatants. The groups themselves are excluded from any legal personality under IHL. Their identity as 'fighters' is fused with criminal status — exit from the group does not remove the legal taint; the classification follows the person.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, non_state_armed_groups_below_threshold, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, non_state_armed_groups_below_threshold, excluded).

% Monitors state practice and advocates for the customary reading that would expand CA3 scope. Produces the Commentary and customary law studies that constitute the main alternative reading. Their authority derives from treaty mandate and historical role, but they lack enforcement power — they observe, document, and persuade.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_legal_division, observer,
    institutional, generational, analytical, global).

% Advocate for the expansive reading that would apply CA3 to any organized armed violence. Document violations in threshold-excluded zones. Their voice is excluded from the classification decision — states control the threshold determination. They can shame but not compel reclassification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_ngos, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, state-controlled classification boundary that determines when IHL obligations activate, preventing legal uncertainty in low-level violence and preserving domestic law enforcement primacy.
% TRANSFER_FUNCTION: Moves legal protections (humane treatment guarantees, judicial guarantees, prohibition on adverse distinction) away from irregular combatants and civilians in low-intensity zones, toward state operational discretion and domestic legal frameworks that favor state power.
% ABSENT_VOICES: Irregular combatants and affected civilians in threshold-excluded zones are structurally absent from the classification process — they have no standing to contest whether a conflict meets the threshold. Their voices appear only posthumously in NGO reports or ICJ proceedings years later.
% DISAPPEARANCE_RATIONALE: If the threshold reading vanished, states would lose the legal basis for denying IHL protections in low-intensity conflicts. Domestic criminal law would no longer be the exclusive framework for counter-insurgency operations. Irregular combatants would gain minimum CA3 protections immediately. The legal architecture of the 'war on terror' and similar low-intensity counter-insurgency frameworks would collapse.
% FOUNDING_PROBLEM: States needed a clear boundary to prevent IHL from swallowing domestic law enforcement in situations of internal disturbances, riots, and isolated acts of violence — the 'threshold problem' identified during the 1949 Diplomatic Conference.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 Diplomatic Conference travaux préparatoires show states explicitly sought to exclude 'internal disturbances and tensions' from CA3. However, the ICRC Commentary (2016) and customary law study argue the threshold has been eroded by state practice — the founding problem of 'clear boundary' is contested by the very actors who benefit from the boundary's flexibility. No non-state actor corroborates the threshold's continuing necessity; only states and their legal advisors do.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the threshold determination transfers substantial legal protections from powerless groups to state discretion. Suppression is very high (0.85) because the constraint's persistence depends on states' exclusive authority to classify conflicts — no alternative classification mechanism exists, and challenges are suppressed through sovereignty arguments and non-justiciability doctrines. Theater ratio (0.42) reflects genuine legal debate at the margins (ICTY Tadić, ICRC Commentary) but the core threshold apparatus operates as functional extraction machinery. Accessibility collapse (0.72) is high because once the threshold framework is accepted, alternatives (expansive reading, customary reading) appear as legal errors rather than policy choices. Resistance (0.58) is moderate — ICRC and NGOs contest but lack enforcement leverage; some domestic courts have pushed back (e.g., Colombian Constitutional Court, Israeli HCJ) but state practice remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the threshold is a necessary coordination mechanism preventing legal chaos; from the irregular combatant seat, it is a denial of humanity encoded in law; from the ICRC seat, it is an outdated formalism that customary law has overtaken. The engine computes this divergence from the structural data — the same threshold produces different χ values because directionality differs radically across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State armed forces and legal advisors are beneficiaries (d near 0.0) — they control the classification and gain operational freedom. Irregular combatants and civilians in low-intensity zones are full targets (d near 1.0) — trapped or constrained exit, identity-locked for fighters, bear the full cost of denied protections. Non-state armed groups below threshold are payers with identity_locked exit — their organizational identity is fused with criminal status. ICRC and NGOs are observers/excluded with analytical/mobile exit — they see the structure but cannot alter the classification authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear boundary for IHL activation) is contested — states claim it remains live, but the threshold has become a tool for denying protections in conflicts that factually meet any reasonable intensity/organization test. The arrangement persists not because the founding problem requires it, but because the classification authority it grants is valuable to states. This is mandatrophy: the mandate (legal certainty) has atrophied into a discretion-maximizing tool. The snare classification captures this — the coordination story is cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_factual_vs_legal,
    'Is the intensity/organization threshold a factual determination (does the conflict objectively meet criteria?) or a legal/political determination (does the state acknowledge the conflict?)',
    'Comparative analysis of state practice: when states deny armed conflict existence despite factual indicators meeting threshold criteria (e.g., Colombia 1990s, Sri Lanka 2000s, Sahel 2010s), the determination is political. When independent fact-finders (ICRC, UN commissions) consistently find thresholds met but states deny, the legal/factual gap is confirmed.',
    'If political, the threshold is a discretion-granting mechanism, not a legal test — strengthening the snare classification. If factual, the threshold has genuine coordination function and extraction varies with good-faith application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_factual_vs_legal, empirical, 'Whether threshold determination is factual or political').

omega_variable(
    customary_law_displacement,
    'Has customary international law displaced the treaty threshold, making CA3 applicable to all organized armed violence regardless of state classification?',
    'ICJ or ICC jurisprudence on CA3 scope in threshold-contested conflicts; convergence of state practice toward lower thresholds; opinio juris evidence from military manuals and UN resolutions.',
    'If customary law has displaced the threshold, the state-centric reading becomes a violation of binding law rather than a permissible interpretation — the constraint shifts from snare (extraction within law) to something closer to unlawful denial of protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_displacement, conceptual, 'Whether customary IHL has overtaken the treaty threshold').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of CA3 application structural (states control classification authority) or internalized (non-state actors and civilians accept the threshold as legitimate)?',
    'Post-conflict legal claims: if irregular combatants and affected populations continue to invoke CA3 protections despite state denial, suppression is primarily structural. If they adopt the state''s classification and seek redress only through domestic law, internalization is present.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — victims carry the denial of protections as legitimate. This would increase effective extraction for identity-locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for threshold-excluded populations').

omega_variable(
    committer_structure_kernel_reading,
    'How does the state-centric reading''s structural relationship to the common_article_3_scope kernel differ from its sibling readings, and what does this imply for classification stability?',
    'Track whether the kernel''s authority structure (the 1949 treaty text) remains stable while readings diverge, or whether the kernel itself is fracturing into distinct constraint families. Monitor ICJ/ICC treatment of the threshold question as a proxy for kernel coherence.',
    'If the kernel is fracturing, each reading becomes a separate constraint with its own ε — the state-centric reading''s snare classification would be stable. If the kernel holds, the readings are in active contest and classification may shift as authoritative interpretation evolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel stability and reading relations for common_article_3_scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__state_centric_reading, theater_ratio, 1977, 0.22).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__state_centric_reading, theater_ratio, 1995, 0.31).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__state_centric_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__state_centric_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__state_centric_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__state_centric_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__state_centric_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__state_centric_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__state_centric_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__state_centric_reading, suppression_requirement, 1995, 0.71).
narrative_ontology:measurement(comm_su_t2001, common_article_3_scope__state_centric_reading, suppression_requirement, 2001, 0.88).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__state_centric_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, non_international_armed_conflict_classification).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, combatant_status_determination).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, detention_in_non_international_armed_conflict).

% DUAL FORMULATION NOTE:
% The common_article_3_scope kernel decomposes into three constraint stories: this state-centric reading (snare, high extraction), the expansive_human_rights_reading (claimed rope, lower extraction), and the icrc_customary_reading (claimed mountain, near-zero extraction). Their ε values differ structurally: the state reading extracts protections; the expansive reading coordinates protections; the customary reading asserts settled law. They are linked through the kernel's authority structure and the threshold determination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, institutional, 0.1).
constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, powerless, 0.95).
constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
