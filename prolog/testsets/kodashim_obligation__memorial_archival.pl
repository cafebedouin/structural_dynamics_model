% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__memorial_archival
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__memorial_archival, []).

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
 *   constraint_id: kodashim_obligation__memorial_archival
 *   human_readable: Kodashim Study as Memorial Archival (Post-70 CE Reading)
 *   domain: religious_studies/jewish_law/commitment_systems
 *
 * SUMMARY:
 *   The destruction of the Second Temple in 70 CE severed the institutional
 *   practice of sacrificial worship in Jerusalem. Yet Jewish law maintained
 *   an obligation to study the detailed procedures and architecture of the
 *   Temple sacrificial system (kodashim, 'holy things'). This constraint
 *   models the memorial-archival reading: the obligation to study kodashim
 *   persists as a mechanism for preserving collective Jewish memory of an
 *   extinct practice. No performance is expected or possible. The study is
 *   purely archival — maintaining textual knowledge of procedures that will
 *   never again be enacted under present conditions. The kernel
 *   (kodashim_obligation) is acknowledged as historically binding — the
 *   obligation originated in post-70 CE Rabbinic law and remains formally
 *   binding in traditional Jewish practice. But the constraint is presently
 *   inactive in the performative sense: no actual sacrifices are prepared, no
 *   Temple service is conducted. The constraint's function collapses to
 *   ceremonial preservation of cultural memory. This reading competes with
 *   sibling readings that ground the obligation in different axioms:
 *   study_as_occupation (the obligation exists because continuous engagement
 *   with Jewish law is meritorious in itself, regardless of the material's
 *   practical relevance) and performance_prerequisite (the obligation
 *   persists because Jewish law anticipates messianic restoration of Temple
 *   worship, for which the knowledge must be preserved and ready). Each
 *   reading instantiates a different structural constraint on how the
 *   kodashim obligation functions in post-70 CE Jewish practice.
 *
 * KEY AGENTS:
 *   - Individual Student: (powerless/mobile) — Freely enters memorial study; can exit without structural penalty; participates in coordination of collective memory.
 *   - Rabbinical Authority: (institutional/arbitrage) — Maintains the obligation in law; coordinates students' participation; benefits from institutional continuity of legal tradition.
 *   - Study Community: (institutional/constrained) — Maintains the ceremonial practice; invested in the performative transmission of the obligation; cannot abandon without institutional cost to identity.
 *   - Jewish Collective Memory: (abstract/universal) — Beneficiary of the memorial archival function; preserved through continuous study and textual transmission.
 *   - Analytical Observer: (analytical/analytical) — Sees the constraint as pure coordination of memory work; perceives the memorial function as genuine and valuable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__memorial_archival, 0.08).
domain_priors:suppression_score(kodashim_obligation__memorial_archival, 0.02).
domain_priors:theater_ratio(kodashim_obligation__memorial_archival, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__memorial_archival, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_obligation__memorial_archival, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(kodashim_obligation__memorial_archival, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__memorial_archival, rope).
narrative_ontology:human_readable(kodashim_obligation__memorial_archival, "Kodashim Study as Memorial Archival (Post-70 CE Reading)").
narrative_ontology:topic_domain(kodashim_obligation__memorial_archival, "religious_studies/jewish_law/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__memorial_archival, '6253b1f5-4346-4b49-ad2f-ec11d78818de').
narrative_ontology:cs_kernel_codification('6253b1f5-4346-4b49-ad2f-ec11d78818de', formalized).
narrative_ontology:cs_authority_grounding('6253b1f5-4346-4b49-ad2f-ec11d78818de', lineage).
narrative_ontology:cs_interpretation_layer_present('6253b1f5-4346-4b49-ad2f-ec11d78818de').
narrative_ontology:cs_reading_relation('6253b1f5-4346-4b49-ad2f-ec11d78818de', kodashim_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('6253b1f5-4346-4b49-ad2f-ec11d78818de', kodashim_obligation__performance_prerequisite, coexists_with).
narrative_ontology:cs_axiom('6253b1f5-4346-4b49-ad2f-ec11d78818de', foundational, extinct_practice_requires_memorial_preservation).
narrative_ontology:cs_axiom_status(extinct_practice_requires_memorial_preservation, holdable).
narrative_ontology:cs_axiom_grounding('6253b1f5-4346-4b49-ad2f-ec11d78818de', extinct_practice_requires_memorial_preservation, conventional).
narrative_ontology:cs_axiom('6253b1f5-4346-4b49-ad2f-ec11d78818de', foundational, archival_study_constitutes_valid_obligation_fulfillment).
narrative_ontology:cs_axiom_status(archival_study_constitutes_valid_obligation_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6253b1f5-4346-4b49-ad2f-ec11d78818de', archival_study_constitutes_valid_obligation_fulfillment, conventional).
narrative_ontology:cs_reference_frame('6253b1f5-4346-4b49-ad2f-ec11d78818de', post_70_ce_memorial_preservation).
narrative_ontology:cs_drift_state('6253b1f5-4346-4b49-ad2f-ec11d78818de', contemporary_diaspora_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6253b1f5-4346-4b49-ad2f-ec11d78818de', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__memorial_archival, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__memorial_archival, jewish_collective_memory).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL STUDENT (ROPE) — The student who studies kodashim texts is engaged in a pure coordination mechanism: preserving knowledge of an extinct practice for collective memory. No extraction occurs. The student enters freely, can exit freely (mobile), and the constraint is coordinative only — transmitting the memorial archive across generations. Powerless in the nominal sense (no coercive authority) but culturally significant. At civilizational scale, this is rope: coordination of memory work without asymmetric extraction.
constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, rope,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 2: RABBINICAL AUTHORITY (ROPE) — The institutional body that maintains the kodashim study obligation in post-70 CE law sees this as coordination: preserving the textual archive ensures continuity of Jewish practice-memory and supports the broader institutional function of law study as identity maintenance. The authority does not extract from students; it coordinates their participation in memorial labor. This is arbitrage-level exit for institutional actors (they can choose whether to maintain the obligation, and they do), making the effective extractiveness near zero. The constraint is pure coordination of memory.
constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: STUDY COMMUNITY / INSTITUTIONAL PRACTICE (PITON) — At biographical timescale and regional scope, the ongoing study practice appears as inertial maintenance. The community continues the ritual of kodashim study not primarily because it serves the stated memorial function (which it does, archivally) but because the practice has institutional weight. Theater ratio 0.85 indicates that much of the study activity is performative: the ritual of studying extinct sacrifice procedures, the reverence for the texts, the institutional signaling that 'we preserve this knowledge' — all this performs cultural continuity more than it functionally preserves it. The constraint persists through habit and institutional identity, not because the memorial coordination breaks down without it. Constrained exit: communities cannot simply abandon the practice without institutional cost.
constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — At analytical remove and civilizational scope, this constraint is pure coordination: the mechanism by which a dispersed Jewish diaspora preserves collective memory of extinct institutional practice. The coordination function is genuine — without such memorial study, the archive would degrade and knowledge of sacrificial practice would be lost. No extraction occurs; no beneficiary captures asymmetric gain. This is the lowest-theater perspective: the constraint's function and form align. The coordination is real and valuable.
constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__memorial_archival_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_obligation__memorial_archival, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_obligation__memorial_archival, TR),
    TR >= 0.70.

:- end_tests(kodashim_obligation__memorial_archival_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The memorial-archival reading produces essentially zero extraction. Students enter the study voluntarily (mobile exit). The authority gains no asymmetric benefit — maintaining the obligation serves the collective memorial function, not institutional extraction. No victim group exists: the study does not extract from anyone. The low extractiveness reflects that this is pure coordination. Suppression (0.02): Minimal. No coercive mechanisms enforce participation in kodashim study. No barriers prevent exit. The constraint is held in place by voluntary transmission of cultural practice, not by suppression. Theater ratio (0.85): High. Most of the observable activity in kodashim study is performative rather than functionally necessary for preservation. The reverence for the texts, the ritual of studying procedures that cannot be enacted, the institutional weight given to the practice — all this performs cultural continuity. But the theater does not indicate extraction; it indicates inertial maintenance through ceremonial practice. The constraint is a rope (coordination) that appears piton-like (performative) when observed at biographical timescale. From civilizational scope, the theater is the only available mechanism for preserving memory of extinct practice, and it works.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap appears between timescales and scopes. At civilizational/universal scope, the constraint is pure rope: coordination of memory. At biographical/regional scope, it appears as piton: performative maintenance through institutional inertia. The same structural constraint exhibits different classifications depending on whether the observer assumes the memorial function is a genuine coordination goal (rope) or a rationalization for institutional momentum (piton). The gap is not an error; it is diagnostic. It reveals that the constraint's legitimacy in the memorial-archival reading depends on the observer accepting that preserving cultural memory is a genuine coordination value. If that value is questioned, the constraint degrades into ceremonial theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: powerless students with mobile exit experience near-zero extraction (d ≈ 0.15, beneficiary status without exit cost); institutional authority with arbitrage exit experiences negative extraction (d ≈ 0.05, beneficiary status with full escape option). The analytical observer derives d from the absence of coercive mechanisms and asymmetric extraction (d ≈ 0.50, symmetric). No override is needed — the structural data produces low directionality naturally. This reflects the constraint's genuine coordinativeness.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by reducing extractiveness nearly to zero: the constraint functions as pure coordination with no asymmetric extraction. The beneficiary (collective memory) is genuine and non-extractive. The cost-bearer is not a victim but a voluntary participant in the coordination. This is NOT mislabeling extraction as coordination — it is recognizing that the constraint genuinely lacks extraction. The piton perspective (performative maintenance at biographical scope) is real but subordinate to the rope perspective (genuine coordination at civilizational scope). The theater is high because it is the only available preservation mechanism for extinct practice, not because it masks extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_function_vs_obligation_drift,
    'Is the stated obligation to study kodashim grounded in the memorial function (preserve cultural memory of extinct practice) or in a different axiom (e.g., spiritual merit of study itself, or restoration-readiness for messianic redemption)?',
    'Textual analysis of Talmudic rationales for maintaining kodashim study post-70 CE; comparison of justifications in Mishnah, Gemara, and medieval responsa; historical tracing of why the obligation persisted vs. why it could have been abandoned.',
    'If memorial function is primary: this reading''s rope classification is correct — pure coordination of memory work. If restoration-readiness or spiritual merit is primary: the constraint should reclassify as a different reading (study_as_occupation or performance_prerequisite). The kernel itself may have multiple live readings with different legitimacy grounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(memorial_function_vs_obligation_drift, empirical, 'Whether kodashim study obligation is grounded in memorial preservation vs. other axioms').

omega_variable(
    this_reading_vs_sibling_reading_foreclosure,
    'Does the memorial_archival reading logically foreclose the performance_prerequisite reading (obligation as spiritual readiness for redemptive restoration)? Or do they coexist as different interpretations held by different Jewish communities?',
    'Survey of contemporary Jewish movements'' approaches to kodashim study: do they treat it as memorial archival only, or do some maintain elements of restoration-readiness doctrine? Can a single community coherently hold both readings simultaneously?',
    'If foreclosure: the engine computes one reading as overridden within the other''s framework. If coexistence: both readings remain live and the kernel exhibits genuine indeterminacy. This determines reading_relations taxonomy (forecloses vs. coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_sibling_reading_foreclosure, conceptual, 'Logical relationship between memorial_archival and performance_prerequisite readings').

omega_variable(
    archive_degradation_mechanism,
    'What mechanism prevents the memorial archive of kodashim knowledge from degrading if the obligation to study is removed? Is continuous study essential, or could the knowledge be preserved through written texts alone without the ceremonial obligation?',
    'Historical analysis: did kodashim knowledge actually degrade in communities that abandoned the obligation? Or is the degradation risk more epistemic (institutional loss of expertise) than textual (archive loss)? Comparison with other extinct practices whose archives survived without continuous obligatory study.',
    'If study is essential to preservation: the coordination function is genuine and high-value. If texts alone suffice: the constraint''s memorial function is ceremonial and could be replaced by lower-theater archival mechanisms. This affects whether rope classification accurately captures the constraint''s functional value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_degradation_mechanism, empirical, 'Whether continuous obligatory study is essential to preserving kodashim knowledge').

omega_variable(
    reading_kernel_identity_ambiguity,
    'Is this constraint one reading of a single kernel (kodashim_obligation), or do the memorial_archival reading and the performance_prerequisite reading operate on separate kernels (one grounded in post-70 CE continuity, one grounded in restoration theology)? If separate kernels, the constraint stories should be decomposed differently.',
    'Textual archaeology: does the Talmudic discourse that established the post-70 CE obligation explicitly ground it in memorial function, or did memorial justification emerge as a post-hoc reading to explain an obligation that originated from different axioms? Chain-of-transmission analysis of when each reading became dominant.',
    'If single kernel with multiple readings: this story correctly models one reading of kodashim_obligation. If separate kernels: the constraint family should decompose into separate kernel-groups, each with its own constraint_id (kodashim_obligation__restoration vs. kodashim_obligation__archival as separate kernels, not readings).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_identity_ambiguity, conceptual, 'Whether memorial_archival and performance_prerequisite are readings of one kernel or separate kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__memorial_archival, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__memorial_archival, attachment_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__memorial_archival, 0.02).
narrative_ontology:affects_constraint(kodashim_obligation__memorial_archival, kodashim_obligation__study_as_occupation).
narrative_ontology:affects_constraint(kodashim_obligation__memorial_archival, kodashim_obligation__performance_prerequisite).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraint stories with different epsilon values and different structural relationships. The memorial_archival reading models the constraint as archival coordination (ε≈0.08, rope). The study_as_occupation reading models it as labor-institution creation (ε≈0.45, tangled_rope). The performance_prerequisite reading models it as restoration theology (ε≈0.30, scaffold). Each reading grounds the obligation in a different axiom. All three readings remain live in contemporary Jewish law; they represent different interpretations of the same kernel held by different communities. The network edges document the semantic and structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
