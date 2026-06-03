% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Exercise of Commandment (Study-as-Performance Reading)
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The study-as-performance reading emerges from classical rabbinic theology
 *   (Talmud Bavli, Menachot 110a: 'one who studies the laws of the sacrifice
 *   as if one brought it') and is elaborated in medieval and modern halakhic
 *   philosophy. It resolves the structural problem created by the Temple's
 *   destruction (70 CE) and the diaspora condition: How can Jews fulfill a
 *   commandment whose primary execution modality (bringing a physical
 *   sacrifice in the Temple) is materially inaccessible? The reading answers:
 *   the commandment's essence is intellectual mastery of its laws, not the
 *   physical act. Study IS the commandment. This is not a substitute or a
 *   preparation for future Temple practice — it is the intrinsic fulfillment
 *   of obligation. The constraint exhibits zero extractiveness because the
 *   relationship between scholar and obligation, between scholar and halakhic
 *   authority, contains no asymmetric extraction. The scholar voluntarily
 *   engages in study as authentic worship; the halakhic authority transmits
 *   the corpus without gatekeeping or coercive enforcement. This is a pure
 *   coordination mechanism: it enables the commandment to persist as
 *   meaningful across conditions where its original form is unavailable.
 *
 * KEY AGENTS:
 *   - Scholar-Worshipper (moderate/mobile): Primary beneficiary — engages in study as intrinsically valuable worship; fulfills obligation through intellectual participation
 *   - Halakhic Authority (institutional/arbitrage): Secondary beneficiary and coordinator — transmits sacrifice law intact; maintains tradition across generations; enables scholar's engagement
 *   - The Diaspora Condition (analytical/analytical): Structural context — Temple destruction creates the logical problem (how to fulfill a commandment without Temple); the reading solves it coordinatively
 *   - Analytical Observer (analytical/analytical): Sees the three-element coordination: obligation (normative), absence of Temple (material), sufficiency of study (theological)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Exercise of Commandment (Study-as-Performance Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '39204957-f5af-42a6-ac93-9c247aaa25a8').
narrative_ontology:cs_kernel_codification('39204957-f5af-42a6-ac93-9c247aaa25a8', fixed_text).
narrative_ontology:cs_authority_grounding('39204957-f5af-42a6-ac93-9c247aaa25a8', lineage).
narrative_ontology:cs_interpretation_layer_present('39204957-f5af-42a6-ac93-9c247aaa25a8').
narrative_ontology:cs_reading_relation('39204957-f5af-42a6-ac93-9c247aaa25a8', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('39204957-f5af-42a6-ac93-9c247aaa25a8', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('39204957-f5af-42a6-ac93-9c247aaa25a8', foundational, intellectual_engagement_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('39204957-f5af-42a6-ac93-9c247aaa25a8', intellectual_engagement_constitutes_fulfillment, deontological).
narrative_ontology:cs_axiom('39204957-f5af-42a6-ac93-9c247aaa25a8', foundational, diaspora_study_as_intrinsic_worship).
narrative_ontology:cs_axiom_status(diaspora_study_as_intrinsic_worship, holdable).
narrative_ontology:cs_axiom_grounding('39204957-f5af-42a6-ac93-9c247aaa25a8', diaspora_study_as_intrinsic_worship, theological).
narrative_ontology:cs_reference_frame('39204957-f5af-42a6-ac93-9c247aaa25a8', study_fulfills_obligation_diaspora).
narrative_ontology:cs_drift_state('39204957-f5af-42a6-ac93-9c247aaa25a8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('39204957-f5af-42a6-ac93-9c247aaa25a8', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHOLAR-WORSHIPPER (ROPE) — The scholar engaged in halakhic study perceives the constraint as pure coordination. Study fulfills the commandment directly through intellectual engagement; worship and learning are fused. No extraction occurs — the scholar experiences only the coordination benefit of fulfilling obligation through the intrinsic act of understanding. Exit options are mobile because one can choose different forms of devotional engagement, yet the study constraint is selected voluntarily as authentic worship. This is the canonical beneficiary perspective under the study-as-performance reading.
constraint_indexing:constraint_classification(sacrifice_commandment__study_as_performance, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — The halakhic authority that codifies and transmits sacrifice law sees the constraint as coordinating three functions: (1) preserving the legal corpus intact across generations, (2) maintaining the scholar's connection to obligation in the absence of Temple practice, (3) preparing the interpretive framework for eventual Temple restoration. No extraction is imposed — the authority's role is stewardship of the tradition. The relationship between scholar and authority is coordinative: both benefit from the transmission mechanism.
constraint_indexing:constraint_classification(sacrifice_commandment__study_as_performance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, this constraint coordinates three logically distinct elements: (1) the imperative of commandment fulfillment (normative obligation), (2) the absence of Temple infrastructure (material constraint), and (3) the sufficiency of intellectual engagement (theological claim). The constraint solves the logical problem: 'How can an obligation be fulfilled when its physical conditions do not exist?' The solution is coordinative — it reframes fulfillment from physical execution to cognitive participation. Zero suppression; zero extraction; pure coordination enabling obligation to persist as meaningful across a diaspora condition.
constraint_indexing:constraint_classification(sacrifice_commandment__study_as_performance, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. The reading declares that study fulfills obligation intrinsically, not instrumentally. No agent bears costs while another extracts benefits. The scholar gains fulfillment; the tradition gains continuity; no victim set exists. The small non-zero value (0.05 vs 0.00) reflects minimal coordination overhead: transmitting the legal corpus requires some institutional labor, which can be read as a tiny extraction cost on the tradition. But the primary dynamic is coordinative. Suppression (0.00): Zero. No alternatives are suppressed; scholars freely choose study as their form of devotion. The reading does not coerce participation or foreclose other forms of engagement. Theater ratio (0.15): Very low. Study of halakhic law is substantive intellectual work, not performative. The scholar engages with complex logical chains, interpretive debates, and textual detail. Some ceremonial framing surrounds study (liturgical context in prayer services, formal study partnerships), but the core activity is functional, not theatrical. The low theater ratio distinguishes this from the piton (degraded) perspective and from ritual performance that is primarily display.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a maximal perspectival coherence — all three perspectives (scholar, authority, analytical) converge on the same classification (Rope) and experience minimal extractiveness. There is no gap because the reading eliminates the structural conditions for asymmetric extraction: study is voluntary, obligation is intrinsic, transmission is non-coercive. This differs dramatically from the performance_only reading (which would see Snare for those ordered to believe an obligation is suspended) and from archive_maintenance (which would see Tangled Rope — coordination with embedded institutional gatekeeping of esoteric knowledge). The study-as-performance reading is unique in producing perspectival unanimity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the agent's structural position relative to extraction flow. In this reading, d ≈ 0.5 for all agents because all are symmetric participants in a coordination mechanism. The scholar and authority both benefit from the transmission; neither extracts from the other. The analytical observer has d ≈ 0.73 (canonical for analytical), but perceives the constraint as pure coordination, not extraction, so the high d does not produce high chi. The formula χ = ε × f(d) × σ(S) yields χ ≈ 0.05 × f(0.5) × σ(local) ≈ 0.05 × 0.65 × 0.8 ≈ 0.026 for the scholar perspective. This is functionally Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading structurally resolves mandatrophy by eliminating the ambiguity that would create it. In the performance_only reading, mandatrophy would arise: is the obligation still binding (Snare — commanded to do the impossible) or suspended (Rope — coordinated postponement)? In archive_maintenance, mandatrophy would arise: is study coordination with genuine worship function, or instrumental gatekeeping? In study-as-performance, there is no mandatrophy because the reading declares unambiguously that study IS the commandment. The structure is transparent: fulfillment = intellectual engagement; obligation is satisfied; coordination mechanism is non-coercive. The constraint classifies as Rope without remainder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_intrinsic_vs_instrumental_value,
    'Is study''s value as commandment-fulfillment intrinsic (study IS worship) or instrumental (study prepares for future Temple practice)?',
    'Rabbinic textual analysis: does the Talmud describe study as fulfilling obligation in itself (Menachot 110a: ''one who studies the sacrifice law is as if they brought it''), or as preparation for Temple restoration (Vayikra Rabbah, messianic commentary)? Both appear in the same tradition.',
    'If intrinsic: study-as-performance reading is structurally stable and truly coordinative (Rope). If instrumental: study is archive maintenance (different constraint, different epsilon). If both: the reading coexists with archive_maintenance but they have different primary functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_intrinsic_vs_instrumental_value, conceptual, 'Whether study''s fulfillment value is intrinsic or instrumental').

omega_variable(
    performance_only_foreclosure_strength,
    'Does the performance_only reading logically foreclose the study-as-performance reading, or can both coexist in a single halakhic framework?',
    'Normative analysis of medieval and modern halakhic debate. Sources that argue performance suspension (Sefer haHinuch on Temple-dependent commandments) versus sources that argue study maintains obligation (Talmudic framework of studying as substitute). Assessment: do these occupy incompatible normative spaces or do they represent different emphases within compatible frameworks?',
    'If foreclosed: the two readings cannot coexist and one must resolve via authority decision. If coexist: both remain live positions and can be held by different communities simultaneously. This determines whether the reading_relations entry is ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_foreclosure_strength, conceptual, 'Whether performance-only reading forecloses study-as-performance').

omega_variable(
    extraction_concealment_risk,
    'Does framing study as intrinsic worship conceal any extractive structure — for instance, institutional gatekeeping of interpretation, or unequal access to advanced study?',
    'Sociological analysis of who has access to full halakhic study across Jewish communities historically. Assessment of whether institutional control over interpretation counts as extraction within this reading''s framework.',
    'If no structural extraction: epsilon correctly remains at 0.05. If extraction exists: the reading''s beneficiary set may include gatekeeping institutions, not only scholar-worshippers. This would shift the constraint from pure Rope toward Tangled Rope or false-summit territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_concealment_risk, empirical, 'Whether institutional study gatekeeping creates hidden extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints corresponding to three readings. Each reading has different epsilon, different beneficiary/victim structure, and different primary function. study_as_performance (THIS CONSTRAINT): ε ≈ 0.05, pure coordination, identity_coordination type. performance_only (SIBLING): ε ≈ 0.65, asymmetric extraction (scholar ordered to believe suspended obligation), Snare type. archive_maintenance (SIBLING): ε ≈ 0.35, mixed coordination and gatekeeping, Tangled Rope type. All three readings reference the same halakhic kernel (sacrifice commandment); each reading instantiates a different constraint because they carry different obligations, different victim sets, and different extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
