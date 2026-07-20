% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Doctrine-Practice Ambiguity (Section 132 Dual-Track)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   This constraint is the practice_doctrine_gap reading of the
 *   marriage_commitment_reversal kernel. The kernel is the contested status
 *   of Section 132 (the plural marriage revelation) after the 1890 Manifesto.
 *   This reading instantiates the structural ambiguity in which the principle
 *   is preserved as binding doctrine while public practice is suspended,
 *   creating dual-track legitimation that allows the institution to survive
 *   federal coercion without textual repudiation. Sibling readings are
 *   exogenous_override_reading (external coercion caused reversal without
 *   doctrinal revision) and endogenous_reinterpretation_reading (internal
 *   divine vision reinterpreted the principle). This reading is distinguished
 *   by its focus on the functional extraction from membership clarity in
 *   service of institutional survival.
 *
 * KEY AGENTS:
 *   - central_leadership: Institutional agenda-setter (institutional/global/constrained) â benefits from flexibility to negotiate federal legitimacy
 *   - general_membership: Primary target (powerless/global/identity_locked) â bears doctrinal bewilderment and betrayal
 *   - fundamentalist_dissenters: Secondary target (moderate/national/constrained) â bears schism, excommunication, and suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.68).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Doctrine-Practice Ambiguity (Section 132 Dual-Track)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '9c2b902d-627d-48c4-96c6-940c7ed816ec').
narrative_ontology:cs_kernel_codification('9c2b902d-627d-48c4-96c6-940c7ed816ec', fixed_text).
narrative_ontology:cs_authority_grounding('9c2b902d-627d-48c4-96c6-940c7ed816ec', lineage).
narrative_ontology:cs_interpretation_layer_present('9c2b902d-627d-48c4-96c6-940c7ed816ec').
narrative_ontology:cs_reading_relation('9c2b902d-627d-48c4-96c6-940c7ed816ec', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c2b902d-627d-48c4-96c6-940c7ed816ec', marriage_commitment_reversal__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_axiom('9c2b902d-627d-48c4-96c6-940c7ed816ec', foundational, preserved_doctrine_binding_irrespective_of_practice).
narrative_ontology:cs_axiom_status(preserved_doctrine_binding_irrespective_of_practice, holdable).
narrative_ontology:cs_axiom_grounding('9c2b902d-627d-48c4-96c6-940c7ed816ec', preserved_doctrine_binding_irrespective_of_practice, theological).
narrative_ontology:cs_axiom('9c2b902d-627d-48c4-96c6-940c7ed816ec', foundational, institutional_survival_validates_tactical_ambiguity).
narrative_ontology:cs_axiom_status(institutional_survival_validates_tactical_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('9c2b902d-627d-48c4-96c6-940c7ed816ec', institutional_survival_validates_tactical_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('9c2b902d-627d-48c4-96c6-940c7ed816ec', section_132_eternal_covenant).
narrative_ontology:cs_drift_state('9c2b902d-627d-48c4-96c6-940c7ed816ec', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9c2b902d-627d-48c4-96c6-940c7ed816ec', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, central_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dual-track system after the 1890 Manifesto: publicly enforces monogamous compliance to secure federal legitimacy and property, while preserving Section 132 as binding scripture and authorizing continued plural marriages in claimed-legal jurisdictions. Bears the burden of maintaining doctrinal coherence without explicit textual revision.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, central_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, central_leadership, beneficiary).

% Receives instruction that plural marriage is an eternal covenant and core identity marker, while observing its public disappearance. Experiences doctrinal bewilderment, status anxiety, and community fracture as the explicit principle is preserved but the practice is suspended. Exit is bounded by kinship networks, cosmological identity, and the threat of social death.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, identity_locked, global).

% Continues to regard Section 132 as immediately binding and public suspension as apostasy. Bears costs of excommunication, legal jeopardy, and forced underground practice or schism. Acts as the visible resistance that the central leadership must actively manage to prevent narrative collapse.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters, payer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, central_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional continuity, federal legitimacy, and collective property ownership by decoupling public practice from doctrinal text, allowing a single community to simultaneously satisfy coercive state demands and maintain a sacred canon.
% TRANSFER_FUNCTION: Moves doctrinal clarity and identity stability from general membership and fundamentalist dissenters to central leadership, in exchange for institutional survival and legal protection; leadership gains temporal flexibility while members absorb cosmological uncertainty.
% ABSENT_VOICES: Federal prosecutors and anti-polygamy legislators who sought explicit statutory repudiation of the doctrine, not merely practice suspension; future generations of members who inherit the canonical text without the living practice context; women and children in unacknowledged unions whose legal standing was erased by the public silence.
% DISAPPEARANCE_RATIONALE: If the ambiguity vanishedâeither by explicit doctrinal repudiation of Section 132 or by restored public practiceâthe church could no longer maintain its dual-track legitimation. Either path would force a single coherent stance, triggering mass schism, federal suppression, or loss of prophetic authority, fundamentally rearranging the institutional landscape.
% FOUNDING_PROBLEM: Existential federal threat to institutional survival, property, and leadership liberty posed by anti-polygamy legislation and territorial disincorporation (Edmunds-Tucker Act, Late Corp of the Church of Jesus Christ v. United States).
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional record and Supreme Court jurisprudence corroborate the external threat from outside the beneficiary set. Fundamentalist dissenters and non-affiliated historians (e.g., Sarah Barringer Gordon, Kathryn Daynes) attest that the founding problem persisted in altered form and that the ambiguity outlived its immediate survival function. Central leadership self-attestation is discounted as beneficiary-corroborated.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically sacrifices membership clarity and identity coherence to preserve institutional optionality; suppression (0.68) reflects the active disciplinary machinery required to silence dissent, manage schism, and prevent public practice from surfacing. Theater ratio (0.62) captures the widening gap between the preserved doctrinal text and the performed public monogamy. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The central_leadership seat computes the constraint as coordination (necessary institutional adaptation to existential threat), while the general_membership and fundamentalist_dissenter seats compute it as extraction (clarity and loyalty are harvested to buy institutional survival). The engine computes this divergence from the structural data: beneficiaries with constrained institutional exit versus victims with identity_locked or trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Central_leadership is the structural beneficiary (collects institutional survival and federal legitimacy â d near the beneficiary end). General_membership and fundamentalist_dissenters are the structural victims (pay in clarity, identity coherence, and disciplinary costs â d near the target end, amplified by identity_locked and constrained exit respectively). The asymmetry is stark: the same arrangement is subsidy for the institution and extraction for the individual believer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal threat to existence) was acute in 1890, but by 1896 Utah statehood had largely resolved the immediate existential danger. The ambiguity persistedâand extraction deepenedâbecause it served ongoing institutional goals (property retention, political respectability, leadership authority) beyond the original survival mandate. This prevents mislabeling the constraint as a Scaffold (no sunset clause) or a Piton (the leadership actively benefits and could not maintain authority without the dual-track). The arrangement is coordination layered with extraction, not expired coordination maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_threat_continuation,
    'Was the federal threat to institutional survival still live after Utah statehood (1896), or had it passed, making the doctrine-practice gap pure extraction rather than survival coordination?',
    'Historical analysis of federal enforcement intensity post-1896, prosecution rates for plural marriage, and congressional anti-polygamy rhetoric through 1904.',
    'If the threat had substantially passed, the constraint''s coordination function is weaker and its classification shifts toward snare; if the threat remained latent, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_threat_continuation, empirical, 'Whether the founding problem persisted beyond the interval midpoint').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is member compliance with the doctrine-practice gap driven primarily by internalized prophetic authority (identity-locked deference) or by structural discipline (excommunication, kinship expulsion, legal threat)?',
    'Post-schism trajectory analysis: if compliance persists among members who have exited the institutional jurisdiction, suppression is partially internalized; if compliance collapses upon exit, suppression was structural.',
    'If internalized, effective suppression exceeds the structural measure and the victim seats experience higher extraction; this would raise the computed severity for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in doctrinal ambiguity').

omega_variable(
    dual_track_intentionality,
    'Was the doctrine-practice gap deliberately engineered as a strategic dual-track system, or did it emerge as an unplanned byproduct of contradictory institutional pressures?',
    'Archival evidence of leadership deliberations (e.g., Woodruff and Snow private correspondence, Quorum of the Twelve minutes) regarding the management of Section 132 post-Manifesto.',
    'If deliberate, the high extractiveness is intentionally designed; if emergent, the constraint may compute as less extractive and more inertial, altering the balance between tangled_rope and piton dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_track_intentionality, empirical, 'Whether the ambiguity was strategic or emergent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1892, 0.45).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1896, 0.54).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1900, 0.6).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.61).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1892, 0.66).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.7).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1896, 0.74).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.78).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.81).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1892, 0.56).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.62).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1896, 0.66).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.7).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.72).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
