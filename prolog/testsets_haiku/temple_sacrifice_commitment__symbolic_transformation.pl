% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment via Symbolic Transformation Reading
 *   domain: religious_law/commitment_system
 *
 * SUMMARY:
 *   In the symbolic-transformation reading of the temple-sacrifice
 *   commitment, the destruction of the Second Temple in 70 CE triggered an
 *   authorized reinterpretation: the commitment to sacrifice was not
 *   suspended but transformed. Prayer (particularly the Amidah service) and
 *   study of sacrifice law (in yeshivas and community learning) became the
 *   authorized instantiation of the original commitment. The rabbinic
 *   interpretive authority claimed and exercised the power to redefine what
 *   performance of a divine command means when material conditions prevent
 *   literal execution. This is not a temporary holding pattern (as in the
 *   preparatory/suspension reading) nor a claim that study was always what
 *   the law intended (as in the exercise reading), but an assertion that the
 *   transformation itself is authorized and constitutes the commitment's
 *   current, valid instantiation. The claim/metric divergence is intentional:
 *   the constraint is claimed as Tangled Rope (it coordinates covenant
 *   continuity while extracting authority from the literalist reading) and
 *   the metrics describe substantial extractiveness and high suppression (the
 *   authority structure defends the reinterpretation against competing
 *   readings with institutional power).
 *
 * KEY AGENTS:
 *   - Rabbinic interpretive authority: institutional agenda-setter, controls the reading's enforcement across diaspora communities
 *   - Literalist halakhic community: identity-locked payer, bears the cost of accepting a reading they argue violates divine specificity
 *   - Messianic restoration advocates: constrained payer-beneficiary, argue suspension is appropriate but pay the cost of a reading that treats suspension as permanent transformation
 *   - Diaspora Jewish communities: beneficiary, enabled to maintain full halakhic participation in absence of Temple
 *   - Competing scriptural readings: excluded from enforcement machinery, would argue their frame deserves equal recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.68).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.72).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment via Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '5394380e-5296-4dff-a1c7-bf9f75f0eb7c').
narrative_ontology:cs_kernel_codification('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', fixed_text).
narrative_ontology:cs_authority_grounding('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', lineage).
narrative_ontology:cs_interpretation_layer_present('5394380e-5296-4dff-a1c7-bf9f75f0eb7c').
narrative_ontology:cs_reading_relation('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_axiom('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', foundational, rabbinic_authority_to_redefine_instantiation).
narrative_ontology:cs_axiom_status(rabbinic_authority_to_redefine_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', rabbinic_authority_to_redefine_instantiation, conventional).
narrative_ontology:cs_axiom('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', foundational, prayer_and_study_constitute_occupancy).
narrative_ontology:cs_axiom_status(prayer_and_study_constitute_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', prayer_and_study_constitute_occupancy, deontological).
narrative_ontology:cs_axiom('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', secondary, material_performance_no_longer_required).
narrative_ontology:cs_axiom_status(material_performance_no_longer_required, holdable).
narrative_ontology:cs_axiom_grounding('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', material_performance_no_longer_required, empirically_contingent).
narrative_ontology:cs_reference_frame('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', authorized_rabbinic_reinterpretation).
narrative_ontology:cs_drift_state('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', contemporary_diaspora_judaism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5394380e-5296-4dff-a1c7-bf9f75f0eb7c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, messianic_restoration_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 to 0.68 across the 2000-year interval because the symbolic-transformation reading accumulates institutional entrenchment: what begins as a contested reinterpretation becomes canonical doctrine, and the cost to literalist and restoration communities of dissenting from this frame grows as it becomes assumed. Suppression is high (0.72) because the rabbinic authority actively excludes competing readings from the machinery that enforces halakhic practice — Karaite literalism, restoration movements, and alternative interpretations are not simply disagreed with, they are structurally prevented from controlling how the commitment is taught and practiced. Theater ratio (0.41) reflects that much of what appears to be performance of the commitment (daily prayer recitation, study-hall debate) is theatrical in the sense that it does not instantiate the original material practice and its explanatory purpose (atonement, connection to Temple service) is performed around rather than through. The measurements are authored on a single shared time grid: every metric is stated at every time point, enabling the engine to detect the coordination-to-extraction drift (rising extractiveness, rising theater) over civilizational timescale.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority and the diaspora beneficiaries experience this constraint as genuine coordination and protective transformation — it enabled a dispersed people to maintain their covenant commitment despite historical disruption. The literalist and restoration communities experience the same constraint as unauthorized drift and authority overreach — they argue that prayer and study cannot substitute for the material practice and that the transformation violates the specificity of divine command. The engine should compute radically different seated types: from the authority's and diaspora's position the constraint is protective rope coordinating around disruption; from the literalist position it is extractive snare, using institutional power to foreclose their reading of what the law requires. The directionality for literalist victims should be high (full target: they pay the cost of accepting an interpretive frame they reject, with constrained exit options). The directionality for the rabbinic authority should be low (beneficiary: it collects interpretive power and validates its own authority structure). These divergences emerge from the structural data, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (rabbinic authority): Low d (near 0.0-0.2). Controls interpretation, enforces the frame, collects institutional legitimacy and the authority to redefine divine commands. Exit options are analytical (can always reinterpret further). Power is institutional (embedded in transmission structures, yeshivas, halakhic courts). Victims (literalist and restoration communities): High d (near 0.8-1.0). Identity-locked and constrained exit options (abandoning the reading means abandoning their theological framework or accepting minority status in a majority-controlled tradition). They must either accept the transformation or become dissenting voices. Diaspora beneficiaries (moderate d, near 0.3-0.4): Benefit materially and spiritually from the transformation but are also constrained by the same authority that imposes it. They have moderate exit (can adopt literalist or restoration readings but absorb social cost), moderate power (organized but not institutional), generational time horizon (the commitment matters across generations but individual choice is real).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids misclassification as pure rope by declaring victims explicitly. A reading that treated symbolic transformation as merely coordination (prayer and study are genuinely meaningful practices that maintain the commitment's depth and relevance) would hide the extraction of authority and the cost imposed on literalist and restoration communities. By naming them as victims, the constraint's structure becomes clear: it is coordination (diaspora community benefit, covenant continuity achieved) AND extraction (authority concentrated in one interpretive body, competing readings excluded and suppressed). The Tangled Rope classification holds both truths simultaneously. The mandatrophy check asks: has the founding problem (how to maintain the commitment after Temple loss) outlived its function? The answer is contested: the rabbinic authority argues the founding problem remains live (the commitment must be maintained in diaspora, prayer and study provide the means); the restoration community argues the founding problem is in suspended state (restoration is pending, so permanent transformation is premature); the literalist community argues the founding problem was wrongly diagnosed (the commitment requires material performance, period, and transformation violates it). These contested statuses are exactly what the omega variables should capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_to_transform_divine_command,
    'Does the rabbinic interpretive authority possess the structural right to redefine a divine command when material conditions prevent its original performance, or does such reinterpretation violate the specificity of divine utterance?',
    'Examination of foundational texts (Talmud, Midrash) and their interpretations across different schools; attestation from Karaite and literalist sources that reject the transformation; analysis of the hermeneutical principles the authority claims grant it this power (gezeirah shavah, kal vachomer, etc.) and whether these principles are treated as discoverable from text or as authorizing discretion.',
    'If transformation authority is not justified, the constraint becomes a pure snare (authority overreach suppressing the true reading). If transformation authority is justified, the constraint remains a tangled rope (legitimate coordination with asymmetric enforcement). The victim set and extraction severity both depend on whether the authority''s power is legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_to_transform_divine_command, conceptual, 'Whether rabbinic authority legitimately extends to reinterpreting divine commands or is limited to applying them as given.').

omega_variable(
    substitution_vs_transformation,
    'Are prayer and study semantically substitutes for sacrifice (performing a similar social/spiritual function through different means) or are they a true transformation of what the commitment now requires (redefining the commitment itself)?',
    'Textual analysis of how sacrifice and prayer/study are described in rabbinic sources — do they justify the move as ''equivalent'' (substitution) or as ''redefining the practice'' (transformation)? Cross-cultural examination of how other religious traditions handle loss of material practice (Hindu puja without proper ritual context, Christian communion theology post-Reformation). Contemporary theological attestation from different Jewish movements about whether the commitment has been transformed or merely accommodated.',
    'If substitution, the commitment might be in suspended state (the hybrid_preparatory reading''s claim). If transformation, the commitment has genuinely changed form and restoration would not restore the original (symbolic_transformation claim holds). The difference affects whether messianic restoration advocates are correctly reading the tradition or making a category error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_transformation, conceptual, 'Whether prayer/study are equivalent to sacrifice or constitute a redefined commitment.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the suppression of competing readings (literalist, preparatory, exercise) primarily structural (institutional exclusion from halakhic authority and transmission machinery) or partly internalized (individuals have absorbed the symbolic-transformation reading as correct)?',
    'Post-exit trajectory: if literalist or other competing communities were to establish fully independent halakhic institutions (separate yeshivas, courts, liturgical practice), would the suppression persist or erode? High persistence (competing reading communities remain unable to organize) would indicate structural dominance; erosion (alternative communities thrive once separated from the rabbinic apparatus) would indicate structural suppression with internalized elements present but not necessary.',
    'If suppression is primarily structural, the constraint''s extracted power lies in institutional control — fixing it requires decentralizing authority. If internalized, fixing it requires cultural re-education and restoration of confidence in alternative readings. The measurement of true extraction severity depends on which mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether the suppression of competing readings is structural or internalized.').

omega_variable(
    contested_founding_problem_status,
    'Does the founding problem (how to maintain the commitment after Temple loss) remain live, or has the 2000-year interval resolved it such that the commitment is now simply instantiated differently?',
    'Temporal analysis of teaching, prayer, and study practices: is sacrifice still taught as a law waiting for restoration (founding problem live in suspension) or as a law whose instantiation has fundamentally changed (founding problem resolved by transformation)? Documentary evidence from different periods showing whether the sacrifice laws were treated as temporarily unobserved or as transformed. Contemporary Jewish theological positions on whether a rebuilt Temple would restore sacrifice or would be unnecessary.',
    'If the founding problem is live, the constraint is genuinely solving an ongoing coordination problem (rope component justified). If the founding problem is dead, the constraint may be extracting authority in a context where coordination is no longer needed (snare component dominant). If contested, different communities experience the constraint asymmetrically: some see urgent coordination, others see irrelevant authority defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_founding_problem_status, empirical, 'Whether the Temple-loss founding problem remains active or has been resolved by the transformation.').

omega_variable(
    kernel_contest_underdetermination,
    'Is the symbolic_transformation reading the only coherent instantiation of how the commitment could be preserved across Temple loss, or do the other readings (performance_only, study_as_exercise, hybrid_preparatory) represent genuinely defensible alternative resolutions of the same founding problem?',
    'Philosophical reconstruction of each reading''s internal consistency and its relationship to the foundational sources. Analysis of why a coherent tradition (Karaite Judaism, hasidic sects, modern Orthodox literalists) could organize around different readings if all readings were equally irrational. Examination of whether the dominance of the symbolic-transformation reading is justified by superior reasoning or by institutional power and historical contingency.',
    'If symbolic transformation is the only coherent reading, the constraint is classifying rightly and victims'' objections are misinformed. If other readings are coherent alternatives, the constraint''s extraction lies in suppressing legitimate competing interpretations, making it more purely extractive (snare-like) than tangled-rope. This affects how the constraint should be reformed or resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_underdetermination, conceptual, 'Whether the symbolic-transformation reading is uniquely justified or one coherent alternative among several.').

omega_variable(
    performance_vs_intellectual_commitment,
    'Can intellectual engagement with sacrifice law (study, contemplation, theorization) genuinely constitute ''performing'' a commitment, or does performance require material instantiation and intention directed at practical effect?',
    'Examination of how the rabbinic sources theorize the relationship between study and practice (la''asot ve''la''shomor, thought and deed). Philosophical analysis of what ''performing a commitment'' means — does it require external action, or can internal/intellectual states constitute performance? Comparison with other domains where commitment is maintained without material practice (vows of poverty in absence of material poverty to renounce, commitment to pacifism in absence of conflict, commitment to truthfulness in absence of external verification).',
    'If intellectual engagement can constitute performance, the transformation is legitimate and the constraint''s extraction is bounded. If performance requires material instantiation, then prayer and study are at best proxies or preparation, and the transformation is unauthorized drift (extraction is higher, constraint is more snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_intellectual_commitment, conceptual, 'Whether intellectual engagement constitutes performance of a commitment or requires material instantiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_symbolic_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sacrifice_symbolic_tr_t200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 200, 0.24).
narrative_ontology:measurement(sacrifice_symbolic_tr_t600, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 600, 0.31).
narrative_ontology:measurement(sacrifice_symbolic_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.36).
narrative_ontology:measurement(sacrifice_symbolic_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.39).
narrative_ontology:measurement(sacrifice_symbolic_tr_t2000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2000, 0.41).

% Extraction over time
narrative_ontology:measurement(sacrifice_symbolic_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(sacrifice_symbolic_be_t200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(sacrifice_symbolic_be_t600, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(sacrifice_symbolic_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(sacrifice_symbolic_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(sacrifice_symbolic_be_t2000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_symbolic_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(sacrifice_symbolic_su_t200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 200, 0.52).
narrative_ontology:measurement(sacrifice_symbolic_su_t600, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 600, 0.61).
narrative_ontology:measurement(sacrifice_symbolic_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.67).
narrative_ontology:measurement(sacrifice_symbolic_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(sacrifice_symbolic_su_t2000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.25).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_commitment kernel is instantiated by four distinct constraint stories, each representing a reading with different structural properties. The symbolic_transformation reading claims the commitment has been permanently reinterpreted; the performance_only reading claims the commitment requires material practice and study is archival; the study_as_exercise reading claims intellectual engagement was always the performance; the hybrid_preparatory reading claims suspension is appropriate pending restoration. Each reading has different ε (extractiveness), different victim sets, different beneficiary structures. The symbolic_transformation reading is upstream in the institutional sense (it controls the halakhic machinery) and influences the others by preventing them from being taught as equal live options. However, the performance_only and hybrid_preparatory readings have lower ε and would represent different constraint types if authorized. The four stories together form a constraint family documenting how a single kernel (the commitment itself) supports multiple structurally distinct instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
