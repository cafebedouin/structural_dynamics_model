% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Direct Performance of Divine Command (Talmud Torah Reading)
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the
 *   temple_sacrifice_commitment kernel: that Torah study of the sacrificial
 *   legal corpus is not preparatory, symbolic, or archival, but is itself the
 *   commanded act — intellectual engagement occupies the commandment fully in
 *   the absence of the Temple and priesthood. Under this reading there is no
 *   suspended commitment waiting for restoration and no substitution
 *   requiring separate justification; the commandment's performance-mode has
 *   always included, and in the current era consists entirely of, sustained
 *   study. This is generated as a clean, ε-invariant constraint for THIS
 *   reading only. The sibling readings (performance_only, hybrid_preparatory,
 *   symbolic_transformation) are separate constraints with their own ε values
 *   and structural data; they are not represented, hedged, or averaged into
 *   this file.
 *
 * KEY AGENTS:
 *   - studying_community: Primary practitioner and beneficiary (organized/mobile) — performs the commandment through study
 *   - halakhic_scholars: Interpretive authority and beneficiary (organized/mobile) — establishes and transmits the study-as-performance framework
 *   - sibling_reading_adherents: Analytical/doctrinal observer (organized/analytical) — contests the reading's adequacy without being extracted from by it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.03).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Direct Performance of Divine Command (Talmud Torah Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '1329dd2b-cfc3-4082-b2c5-007c07b2b25e').
narrative_ontology:cs_kernel_codification('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', fixed_text).
narrative_ontology:cs_authority_grounding('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', practice).
narrative_ontology:cs_interpretation_layer_present('1329dd2b-cfc3-4082-b2c5-007c07b2b25e').
narrative_ontology:cs_reading_relation('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', foundational, study_constitutes_full_performance).
narrative_ontology:cs_axiom_status(study_constitutes_full_performance, holdable).
narrative_ontology:cs_axiom_grounding('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', study_constitutes_full_performance, conventional).
narrative_ontology:cs_axiom('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', secondary, material_instantiation_not_required_for_occupation).
narrative_ontology:cs_axiom_status(material_instantiation_not_required_for_occupation, holdable).
narrative_ontology:cs_axiom_grounding('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', material_instantiation_not_required_for_occupation, conventional).
narrative_ontology:cs_reference_frame('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', second_temple_active_performance_era).
narrative_ontology:cs_drift_state('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1329dd2b-cfc3-4082-b2c5-007c07b2b25e', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, halakhic_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, torah_study_as_mitzvah_fulfillment).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, intellectual_engagement_occupies_commandment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in sustained textual study of the sacrificial order (Seder Kodashim, Zevachim, Menachot) as a communal and individual practice. Understands this study, under this reading, to itself constitute fulfillment of the divine command to occupy the sacrificial commitment, not merely preparation for or memory of it. Chooses this practice voluntarily; nothing coerces engagement beyond internal religious motivation and communal expectation. Can disengage from intensive study of this tractate set without loss of standing in most communities, though within study-intensive communities the expectation carries real but non-coercive social weight.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, studying_community, agenda_setter).

% Produces and transmits the interpretive apparatus (Talmudic sugyot, later commentaries, contemporary yeshiva curricula) that establishes study as commensurate performance. Their institutional and intellectual investment is real, but no material transfer or extraction runs from any other party to them under this reading — they administer a practice whose value is claimed to be intrinsic, not diverted from anyone.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, halakhic_scholars, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, halakhic_scholars, agenda_setter).

% Communities and scholars holding the performance_only, hybrid_preparatory, or symbolic_transformation readings of the same kernel. They observe this reading's practice without being subject to it; they dispute its adequacy on theological and legal grounds but are not extracted from or coerced by it. Their disagreement is doctrinal contest, not victimhood.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, sibling_reading_adherents, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible way for a dispersed community to maintain unbroken engagement with a body of divine command whose material conditions (a standing Temple, an active priesthood, ritual purity infrastructure) do not currently exist, without declaring the commandment abandoned or in permanent suspension.
% TRANSFER_FUNCTION: No material transfer occurs between distinct parties. Time and intellectual effort are invested by the studying community and returned to that same community in the form of continued covenantal standing and communal identity; nothing is extracted from an outside party.
% ABSENT_VOICES: Adherents of performance_only and hybrid_preparatory readings would object that study alone cannot occupy a commandment whose essence is material sacrificial action, and that this reading risks permanently normalizing the absence of restoration efforts. They are present in the broader tradition's discourse (this is an active, canvassed disagreement within halakhic literature) but are not parties this specific reading extracts from or silences.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, communities currently organizing serious study of Kodashim as commandment-fulfillment would lose the framework that makes that specific practice religiously load-bearing; study might continue as scholarship or as the hybrid_preparatory framing, but the claim that intellectual engagement itself discharges the divine command would need to be re-argued or abandoned, changing what the practice is understood to accomplish.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial commandments could no longer be performed materially, creating a genealogical crisis: is the commitment simply void, permanently suspended, or transformed? This reading answers that Torah study of the sacrificial laws IS the commandment's performance, closing the gap without declaring the commandment dormant or symbolically replaced.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (e.g., Megillah 3b, Menachot 110a) are cited by adherents as internal corroboration that study substitutes for sacrifice, but this reading is contested from within the same tradition by scholars holding hybrid_preparatory and performance_only positions, who are not beneficiaries of this specific reading and attest the founding problem remains live pending restoration, not resolved by study alone. No corroboration exists from a source entirely outside the halakhic tradition, since this is an internal doctrinal question without external material stakes to adjudicate it.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because no party's costs subsidize another party's benefit under this reading: the studying community invests effort and receives the claimed spiritual/covenantal return directly, with no third party paying for it. Suppression is low (0.05) because engagement with this reading is not coercively maintained — communities that instead hold the hybrid_preparatory or performance_only view face no penalty enforced by this reading's adherents. Theater ratio is low (0.1) because the study practice this reading describes is substantive intellectual labor (sustained textual analysis of Zevachim, Menachot, and related tractates), not a hollowed-out performative gesture standing in for a defunct function — though a small and stable theatrical residue is honestly acknowledged (ceremonial recitation without engagement occurs at the margins). Accessibility collapse is moderate (0.2): once a community accepts this reading, alternative framings of what 'occupying the commandment' means become less psychologically available to it, but they remain doctrinally accessible and are actively held by other communities, so collapse is far from mountain-level.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community and the scholars who elaborate this reading are both beneficiaries and agenda-setters: they set the terms of what counts as fulfillment and they collect the covenantal/spiritual benefit the reading describes. There is no victim group because no one's material position is worsened by other communities holding this reading — it is a voluntary interpretive commitment, not an extraction mechanism. Sibling-reading adherents are observers, not payers: their disagreement is doctrinal, and the disagreement itself does not cost them anything this reading imposes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is explicitly NOT a case of mandatrophy: the founding problem (how to discharge a commandment whose material conditions vanished) is treated by this reading as a problem that study genuinely and fully resolves, not as a problem that has been forgotten while the practice persists as empty ritual. Because the studying community and scholars are candid that no material sacrifice occurs and defend study as adequate performance on its own terms, there is no gap between claimed function and operative function to mislabel as pure extraction — the practice is coordination-shaped (sustaining covenantal continuity across a community) without a hidden extraction layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_vs_preparation,
    'Does textual study of sacrificial law constitute actual performance of the divine command, or does it merely maintain readiness for a performance that requires material conditions this reading treats as unnecessary?',
    'Internal halakhic argument only — this is a first-order doctrinal dispute within the tradition with no external empirical test. Resolution (if any) would come from rabbinic consensus-formation processes, not from data external to the tradition.',
    'If the hybrid_preparatory framing is correct rather than this reading, the studying community''s practice would be recharacterized as incomplete occupation of the commandment, which could shift emphasis toward restoration-oriented activity and away from treating study as sufficient in itself — this would not change ε for this reading (which remains about the study_as_exercise claim) but would affect which constraint the community''s actual practice should be filed under.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_performance_vs_preparation, conceptual, 'Whether study is performance or merely preparatory maintenance of the commandment.').

omega_variable(
    which_framing_is_the_true_kernel_state,
    'Among the four declared readings of the temple_sacrifice_commitment kernel, is there a single historically or textually privileged framing, or do all four remain permanently coexisting live options with no adjudicating authority?',
    'Would require either (a) a recognized pan-communal halakhic authority ruling definitively among the readings, which does not currently exist and may never exist given the distributed nature of post-Temple rabbinic authority, or (b) restoration of the Temple itself, which would make performance_only and hybrid_preparatory readings moot by resolving the underlying material absence.',
    'If a privileged framing were established, the losing readings would either be reclassified as historically superseded (moving their axioms from holdable to overridden) or would persist as minority positions; this reading''s zero-extraction, coordination-only classification would only be threatened if a future authority reclassified the study practice itself as either extractive (e.g., discovered to fund an exploitative institutional structure) or as illegitimately substituting for a still-live material obligation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_framing_is_the_true_kernel_state, conceptual, 'Whether one kernel reading is authoritative or all four remain permanently coexisting.').

omega_variable(
    natural_vs_constructed_covenantal_benefit,
    'Is the claimed benefit to the studying community (covenantal fidelity, spiritual merit) a genuine feature of religious reality this reading correctly identifies, or a constructed benefit that primarily serves to justify continued communal investment in study institutions regardless of the framework''s ultimate correctness?',
    'Not empirically resolvable; this is a theological question about the reality of covenantal merit. The closest available signal is longitudinal community health and continuity data, which is suggestive but not dispositive of the underlying theological claim.',
    'If the benefit is substantially constructed to serve institutional self-perpetuation of scholarly authority (halakhic_scholars) rather than genuinely accruing to the studying community, this reading would begin to resemble a false-summit pattern despite currently authoring beneficiaries without extraction — this omega documents that possibility even though the current metrics do not indicate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_covenantal_benefit, preference, 'Whether the claimed covenantal benefit is a real feature this reading identifies or a constructed justification for institutional continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 400, 0.08).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 800, 0.09).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 800, 0.03).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1600, 0.03).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1900, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
