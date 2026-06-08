% ============================================================================
% CONSTRAINT STORY: performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only_reading, []).

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
 *   constraint_id: performance_only_reading
 *   human_readable: Performance-Only Reading of Sacrifice Obligation
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of sacrifice obligation holds that physical
 *   performance in the Temple is the sole fulfillment mechanism for the
 *   commanded sacrifices. Study of sacrifice laws is preparatory and
 *   meritorious but does not discharge the obligation. This reading creates a
 *   1,900-year structural impossibility: the Jewish people remain commanded
 *   to perform sacrifices but lack the Temple, priesthood, and ritual purity
 *   system required for performance. The constraint's extractiveness (0.78)
 *   reflects the gap between persistent obligation and structural incapacity.
 *   Suppression (0.85) reflects the identity-lock: exit from the obligation
 *   requires exit from halakhic Judaism itself under this reading. Theater
 *   ratio (0.15) is low because the reading is not performative — it
 *   genuinely maintains textual fidelity and does not substitute symbolic
 *   performance for the commanded act. The constraint demonstrates
 *   commitment-system dynamics: a fixed textual kernel (Torah sacrifice
 *   commands) interpreted through an authority structure (rabbinic
 *   textualism) that maintains obligation despite impossibility of
 *   fulfillment.
 *
 * KEY AGENTS:
 *   - Commanded Jewish People: Primary victim (powerless/identity_locked) — bears unfulfilled obligation for 1,900 years with no exit preserving Jewish identity under this reading
 *   - Halakhic Practitioners: Secondary victim (moderate/constrained) — benefit from coordination function (clear textual standard) but bear cost of unfulfillable command
 *   - Textualist Rabbinic Authority: Primary beneficiary (institutional/arbitrage) — reading vindicates literal textual interpretation and maintains boundary between study and fulfillment
 *   - Messianic Restorationists: Organized agents (organized/constrained) — see constraint as temporary with sunset at Temple restoration
 *   - Analytical Observer: Cross-positional view (analytical/analytical) — recognizes both coordination (textual fidelity) and extraction (impossible obligation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only_reading, 0.78).
domain_priors:suppression_score(performance_only_reading, 0.85).
domain_priors:theater_ratio(performance_only_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_only_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_only_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only_reading, snare).
narrative_ontology:human_readable(performance_only_reading, "Performance-Only Reading of Sacrifice Obligation").
narrative_ontology:topic_domain(performance_only_reading, "religious_law/halakhic_authority/commitment_system").

domain_priors:requires_active_enforcement(performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only_reading, '4f29aa07-feb4-4cec-a392-f672a91ff384').
narrative_ontology:cs_kernel_codification('4f29aa07-feb4-4cec-a392-f672a91ff384', fixed_text).
narrative_ontology:cs_authority_grounding('4f29aa07-feb4-4cec-a392-f672a91ff384', lineage).
narrative_ontology:cs_interpretation_layer_present('4f29aa07-feb4-4cec-a392-f672a91ff384').
narrative_ontology:cs_reading_relation('4f29aa07-feb4-4cec-a392-f672a91ff384', performance_only_reading__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f29aa07-feb4-4cec-a392-f672a91ff384', performance_only_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f29aa07-feb4-4cec-a392-f672a91ff384', performance_only_reading__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('4f29aa07-feb4-4cec-a392-f672a91ff384', foundational, physical_performance_irreplaceable).
narrative_ontology:cs_axiom_status(physical_performance_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('4f29aa07-feb4-4cec-a392-f672a91ff384', physical_performance_irreplaceable, deontological).
narrative_ontology:cs_axiom('4f29aa07-feb4-4cec-a392-f672a91ff384', foundational, study_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('4f29aa07-feb4-4cec-a392-f672a91ff384', study_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('4f29aa07-feb4-4cec-a392-f672a91ff384', temple_service_operational).
narrative_ontology:cs_drift_state('4f29aa07-feb4-4cec-a392-f672a91ff384', post_destruction_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4f29aa07-feb4-4cec-a392-f672a91ff384', '').
narrative_ontology:cs_kernel_id(performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(performance_only_reading, commanded_jewish_people).
narrative_ontology:constraint_victim(performance_only_reading, halakhic_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMANDED INDIVIDUAL (SNARE) — Identity-locked within halakhic commitment; commanded to perform sacrifices but structurally unable to do so for 1,900 years. The obligation persists without fulfillment mechanism. Maximum extraction: bears guilt of unfulfilled mitzvah with no exit that preserves Jewish identity under this reading.
constraint_indexing:constraint_classification(performance_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: HALAKHIC PRACTITIONER (TANGLED ROPE) — Constrained by textual authority and communal expectation. Benefits from the coordination function (clear textual standard, transmitted tradition) but bears cost of unfulfillable obligation. Study of sacrifice laws maintains connection to the command while acknowledging impossibility of performance.
constraint_indexing:constraint_classification(performance_only_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TEXTUALIST RABBINIC AUTHORITY (ROPE) — Benefits from literal reading's clarity and textual supremacy. Experiences the constraint as coordination: the performance-only standard preserves textual integrity and prevents substitution drift. Low extraction because this reading vindicates their interpretive authority and maintains the boundary between study and fulfillment.
constraint_indexing:constraint_classification(performance_only_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MESSIANIC RESTORATIONIST (SCAFFOLD) — Organized movement anticipating Temple restoration. Sees the performance-only constraint as temporary: the obligation's unfulfillability is a transitional state resolved by messianic redemption. Study during exile is preparatory for future performance. Sunset clause: restoration of Temple service.
constraint_indexing:constraint_classification(performance_only_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both coordination function (textual fidelity, transmitted tradition) and extraction mechanism (1,900 years of structural impossibility). The reading coordinates halakhic discourse around a clear standard while extracting compliance with an unfulfillable command. The gap between obligation and capacity is the extraction vector.
constraint_indexing:constraint_classification(performance_only_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_only_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_only_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The performance-only reading maintains a commanded obligation that has been structurally impossible to fulfill for 1,900 years. The extraction is not captured by any human beneficiary — it is structural impossibility presented as textual fidelity. The commanded individual bears guilt of unfulfilled mitzvah across biographical timescales. The trajectory shows increasing extraction as the temporal gap between command and capacity widens: at T=0 (Second Temple period) extraction was moderate (0.45) because performance was possible; by T=1900 extraction is high (0.78) because impossibility is entrenched. Suppression (0.85): Very high. Exit from the obligation requires exit from halakhic commitment itself — the constraint is identity-locked. Alternative readings (study-as-fulfillment) are available but require abandoning the performance-only interpretation, which for textualist authorities would constitute abandoning textual supremacy. Suppression increases over time as the reading becomes more entrenched and alternatives are marginalized. Theater ratio (0.15): Low. The reading is not performative — it does not substitute symbolic acts for commanded performance. Study of sacrifice laws is explicitly preparatory, not fulfillment. The low theater ratio distinguishes this from piton constraints where ritual persists as performance after function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The commanded individual sees snare — trapped in unfulfillable obligation with identity-locked exit. The halakhic practitioner sees tangled rope — coordination and extraction intertwined. The textualist authority sees rope — the reading coordinates halakhic discourse and vindicates textual supremacy. The messianic restorationist sees scaffold — temporary impossibility with sunset at redemption. The analytical observer sees tangled rope — genuine textual fidelity (coordination) combined with 1,900 years of structural impossibility (extraction). The gap reveals how the same constraint appears as pure extraction to those trapped by it, as coordination to those whose authority it vindicates, and as temporary to those who see an exit path.
 *
 * DIRECTIONALITY LOGIC:
 *   The commanded individual (powerless/identity_locked) is the primary victim — bears maximum extraction because the obligation persists without fulfillment mechanism and exit requires abandoning Jewish identity under this reading. The halakhic practitioner (moderate/constrained) experiences mixed extraction: benefits from coordination function (clear textual standard, transmitted tradition) but bears cost of unfulfillable obligation. The textualist rabbinic authority (institutional/arbitrage) is the structural beneficiary — the performance-only reading vindicates literal textual interpretation and maintains their authority to define the boundary between study and fulfillment. The messianic restorationist (organized/constrained) sees the constraint as temporary and experiences lower extraction because they have an exit path (Temple restoration). The analytical observer recognizes the dual structure: genuine coordination around textual fidelity combined with extraction through impossible obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading demonstrates how a constraint can be simultaneously high-extraction (snare from victim perspective) and low-theater (not performative). The mandate (perform sacrifices) has outlived its function (Temple destroyed, priesthood dispersed, ritual purity system collapsed) but the obligation persists. This is not mandatrophy in the classic sense (function atrophied, performance remains) — it is structural impossibility maintained as textual fidelity. The constraint resolves the mandatrophy question by showing that high extraction does not require high theater: the reading extracts through impossible obligation, not through performative substitution. The low theater ratio confirms that study is genuinely preparatory, not a symbolic replacement for performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the performance_only_reading of the sacrifice_obligation_kernel. Sibling readings (study_as_exercise_reading, messianic_suspension_reading, symbolic_archive_reading) resolve the obligation-capacity gap differently. What structural element distinguishes this reading?',
    'Cross-reading comparison: performance_only maintains obligation despite impossibility; study_as_exercise treats study as fulfillment; messianic_suspension defers obligation to future restoration; symbolic_archive treats the command as historical record. The distinguishing element is whether study substitutes for performance or remains preparatory.',
    'If study substitutes: extractiveness drops to ~0.25 (coordination with low overhead). If study is preparatory only: extractiveness remains high (~0.78) because obligation persists unfulfilled. The performance_only reading holds the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame structural delta: study as preparatory vs substitutive').

omega_variable(
    obligation_persistence_mechanism,
    'Does the obligation''s persistence despite 1,900 years of impossibility reflect textual fidelity (mountain) or institutional extraction (snare)?',
    'Historical analysis: trace whether the performance-only reading was contested during Second Temple period vs post-destruction. If contested early: extraction mechanism. If uncontested until modern period: closer to textual fidelity. Examine whether alternative readings (study-as-fulfillment) were suppressed or simply not developed.',
    'If textual fidelity: the constraint is closer to mountain (immutable command). If institutional extraction: the constraint is snare (authority structure benefits from maintaining unfulfillable obligation). Current classification assumes snare based on victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_persistence_mechanism, empirical, 'Whether obligation persistence is textual fidelity or extraction').

omega_variable(
    study_preparatory_boundary,
    'What distinguishes preparatory study (does not fulfill) from substitutive study (does fulfill) within halakhic discourse?',
    'Textual analysis of Talmudic and post-Talmudic sources on study of sacrifice laws. Identify explicit statements about study''s status. Cross-reference with other unfulfillable mitzvot (e.g., agricultural laws outside Israel) to see if study-as-fulfillment pattern exists elsewhere.',
    'If boundary is clear and textually grounded: performance_only reading is coordination around textual standard. If boundary is ambiguous or post-hoc: performance_only reading may be extractive maintenance of impossible obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_preparatory_boundary, empirical, 'Textual grounding of preparatory vs substitutive study distinction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_second_temple, performance_only_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theater_talmudic, performance_only_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(theater_medieval, performance_only_reading, theater_ratio, 1000, 0.13).
narrative_ontology:measurement(theater_early_modern, performance_only_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement(theater_contemporary, performance_only_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(extract_second_temple, performance_only_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(extract_talmudic, performance_only_reading, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(extract_medieval, performance_only_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(extract_early_modern, performance_only_reading, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement(extract_contemporary, performance_only_reading, base_extractiveness, 1900, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(suppress_second_temple, performance_only_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(suppress_talmudic, performance_only_reading, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(suppress_medieval, performance_only_reading, suppression_requirement, 1000, 0.8).
narrative_ontology:measurement(suppress_early_modern, performance_only_reading, suppression_requirement, 1500, 0.83).
narrative_ontology:measurement(suppress_contemporary, performance_only_reading, suppression_requirement, 1900, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The performance_only_reading is one of four readings of the sacrifice_obligation_kernel. Each reading resolves the obligation-capacity gap differently. This reading maintains the gap as structural impossibility; siblings resolve it through substitution, suspension, or archival reframing. All four readings share the same textual kernel but produce different victim sets and extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
