% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment Archive Maintenance Reading: Technical Knowledge Preservation for Future Temple Restoration
 *   domain: religious_studies/halakhic_theory/commitment_systems
 *
 * SUMMARY:
 *   The archive-maintenance reading of the sacrifice commandment represents
 *   one coherent interpretation of how the commandment remains binding after
 *   the destruction of the Second Temple and loss of the physical locus
 *   (Temple building) where sacrifice could be performed. Under this reading,
 *   the commandment is not suspended but transformed: its present fulfillment
 *   takes the form of preserving technical knowledge about sacrifice
 *   procedures for future restoration during the messianic age. Study becomes
 *   the mechanism of commandment fulfillment, justified by future utility
 *   rather than present worship. This reading exemplifies a commitment-system
 *   constraint because it grounds its legitimacy in a fixed kernel (the
 *   Torah's commandment to offer sacrifices) and resolves the impossible
 *   performance problem through an interpretive layer (the
 *   archive-maintenance framework). The constraint exhibits moderate
 *   extractiveness because the present generation bears real costs (mandatory
 *   study, intellectual burden, sustained engagement with inaccessible
 *   performance) while the primary beneficiary is a future generation whose
 *   restoration possibility is indefinitely deferred. The theater ratio
 *   increases over time (0.35 → 0.58) as the messianic timeline recedes and
 *   institutional enforcement becomes more obviously ceremonial. The
 *   suppression requirement increases as the gap between commandment and
 *   execution widens, necessitating stronger enforcement of the study
 *   obligation to maintain cohesion.
 *
 * KEY AGENTS:
 *   - Present-day Worshippers: Primary victims (powerless/trapped) — obligated to maintain knowledge of impossible-to-perform sacrifice procedures; no present worship experience accrues
 *   - Studying Community (Intermediate Generations): Secondary victims and partial beneficiaries (moderate/constrained) — bear study burden but benefit from knowledge transmission role; constrained exit due to identity/social cost
 *   - Future Generations / Messianic Readiness: Primary beneficiaries (institutional/arbitrage) — positioned to benefit from accumulated technical knowledge if Temple restoration occurs; no present cost
 *   - Halakhic Authority Structure: Institutional enforcer (institutional/arbitrage) — maintains and enforces the study mandate; benefits from preserving interpretive framework that sustains its authority during exile period
 *   - Competing Interpretive Traditions: Secondary victims (institutional/constrained) — alternative readings (study-as-performance, performance-only) are marginalized or suppressed by the archive-maintenance framework
 *   - Analytical Observer: Sees the constraint as a coherent commitment-system dynamic, not a false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.5).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment Archive Maintenance Reading: Technical Knowledge Preservation for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory/commitment_systems").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '04442be6-7fb2-4a4d-9fb6-16578a6841c2').
narrative_ontology:cs_kernel_codification('04442be6-7fb2-4a4d-9fb6-16578a6841c2', fixed_text).
narrative_ontology:cs_authority_grounding('04442be6-7fb2-4a4d-9fb6-16578a6841c2', lineage).
narrative_ontology:cs_interpretation_layer_present('04442be6-7fb2-4a4d-9fb6-16578a6841c2').
narrative_ontology:cs_reading_relation('04442be6-7fb2-4a4d-9fb6-16578a6841c2', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('04442be6-7fb2-4a4d-9fb6-16578a6841c2', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('04442be6-7fb2-4a4d-9fb6-16578a6841c2', foundational, study_is_valid_fulfillment_in_exile).
narrative_ontology:cs_axiom_status(study_is_valid_fulfillment_in_exile, holdable).
narrative_ontology:cs_axiom_grounding('04442be6-7fb2-4a4d-9fb6-16578a6841c2', study_is_valid_fulfillment_in_exile, deontological).
narrative_ontology:cs_axiom('04442be6-7fb2-4a4d-9fb6-16578a6841c2', foundational, future_restoration_justifies_present_cost).
narrative_ontology:cs_axiom_status(future_restoration_justifies_present_cost, holdable).
narrative_ontology:cs_axiom_grounding('04442be6-7fb2-4a4d-9fb6-16578a6841c2', future_restoration_justifies_present_cost, deontological).
narrative_ontology:cs_reference_frame('04442be6-7fb2-4a4d-9fb6-16578a6841c2', torah_commandment_continuous_fulfillment_across_exile).
narrative_ontology:cs_drift_state('04442be6-7fb2-4a4d-9fb6-16578a6841c2', contemporary_indefinite_deferral, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04442be6-7fb2-4a4d-9fb6-16578a6841c2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, messianic_age_preparedness).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_generation_worshippers).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, alternative_commandment_interpretations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT-DAY WORSHIPPER (SNARE) — Obligated to maintain technical knowledge of Temple sacrifice procedures with no possibility of actual performance. Trapped in an archive function that produces no present worship experience. The commandment appears suspended but enforcement of study obligation remains active. Extraction is maximal for this agent: costs of study (time, intellectual burden, sustained engagement with inaccessible performance) yield no present-moment coordination benefit.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STUDYING COMMUNITY INTERMEDIATE GENERATION (TANGLED ROPE) — Bears the burden of study maintenance (constrained exit: departure from halakhic obligation carries social/identity cost) but also benefits from genuine coordination function: transmitting accumulated technical knowledge across generations preserves the possibility of restoration. Real coordination (knowledge transmission) coexists with real extraction (mandated unpaid scholarly labor).
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE GENERATIONS / MESSIANIC READINESS (ROPE) — Primary beneficiary. Arbitrary access to accumulated technical knowledge if Temple restoration occurs. This perspective experiences the constraint as pure coordination: the archive function solves a real collective problem (preserving restoration-preparedness across the long gap between Second Temple destruction and messianic age). No extraction perceived because the benefit flow is toward this agent; the present-day cost is external to their timeline.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HALAKHIC AUTHORITY STRUCTURE (SCAFFOLD) — Enforces study mandate as temporary expedient during the exile period. Sees the constraint as having a sunset clause: when the Temple is rebuilt (messianic age), the archive function concludes and performance resumes. Low effective extraction because the authority maintains active enforcement contingent on the stated temporal boundary.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COMPETING INTERPRETIVE TRADITIONS (TANGLED ROPE) — The archive-maintenance reading creates asymmetric benefits and costs across different interpretive frameworks. Study-as-performance traditions experience extraction (their framework is marginalized); performance-only traditions experience extraction (commandment is suspended entirely). This reading benefits the messianically-oriented authority structure that uses archive maintenance to justify continued enforcement. Both coordination (preserving technical knowledge) and extraction (suppression of alternative readings) are present.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL ENFORCEMENT RITUAL (PITON) — The ongoing study mandate persists through institutional inertia despite uncertainty about messianic timeline and restoration probability. Theater ratio high (0.58) because study fulfills a commitment-system obligation (maintaining archive) whose present-moment function is largely ceremonial. The actual restoration event to which this archive points is indefinitely deferred. Institutional enforcement maintains the appearance of preparedness without testing whether the archived knowledge is adequate or current.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — This reading instantiates a commitment-system constraint: the halakhic authority grounds legitimacy in the Torah kernel (the commandment to offer sacrifices) and resolves the tension between commandment and impossible execution by reframing execution as 'archive for future performance.' The reading exhibits both genuine coordination (knowledge preservation) and genuine extraction (present-day obligation with deferred benefit). The analytical view sees this as a coherent CS dynamic, not a false summit, because the commitment structure is formally maintained and the interpretive reading is sustained within the tradition's own authority.
constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sacrifice_commandment__archive_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, TR),
    TR >= 0.70.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate, increasing over time. Initial extractiveness (0.28) reflects early Second Temple period when messianic expectations were acute and archive function was genuinely tied to preparation for near-term restoration. As timeline recedes (t=500→1000), extractiveness increases to 0.42 because the deferred benefit becomes more speculative and institutional enforcement on present generation becomes less justified. The archive-maintenance reading is not pure extraction (snare) because genuine coordination function exists — knowledge transmission is real and necessary — but the increasing gap between cost and benefit realization produces asymmetric distribution of burden. Suppression (0.50): Moderate-high, stable. The reading does not employ physical coercion but maintains enforcement through authority structure, social identity alignment, and suppression of alternatives (study-as-performance and performance-only readings). Present-generation practitioners have structural mobility (can adopt alternative readings) but face high identity/social cost. Theater ratio (0.58, increasing): Moderate-high. The study obligation increasingly appears ceremonial because its justification (preparation for restoration) rests on deferred timeline. The institutional enforcement maintains the appearance of active preparedness without testing whether archived knowledge remains adequate or coherent. Early theater ratio (0.35) reflects genuine knowledge-preservation function when messianic timeline was compressed.
 *
 * PERSPECTIVAL GAP:
 *   The archive-maintenance reading generates sharp perspectival gaps across temporal horizons and power contexts. The present-day powerless worshipper sees a snare (no exit, no present benefit, mandated study). The studying community sees tangled rope (real coordination function coexisting with real extraction). Future generations see rope (pure coordination; the constraint solves their collective problem). The halakhic authority sees scaffold (temporary expedient with messianic sunset). Competing traditions see extraction (their frameworks are marginalized). The institutional perspective from within the authority structure sees rope (coordination for future restoration). The analytical observer sees a coherent commitment-system dynamic: the authority grounds its legitimacy in the Torah kernel, uses the archive-maintenance reading to resolve the performance-impossibility problem, and maintains enforcement through the interpretive layer. The gap is not between correct and incorrect classifications but between structural positions: each perspective correctly observes its own experienced extractiveness, and the constraint is legitimately classified as snare/rope/tangled_rope/scaffold simultaneously from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim structure and exit options. Future generations (arbitrage exit + beneficiary status) experience low directionality (d ≈ 0.10) because they extract benefit with high exit capacity; the study obligation is external to their timeline. Present-generation worshippers (trapped exit + victim status) experience high directionality (d ≈ 0.90) because they bear costs with no exit. The halakhic authority (institutional power + arbitrage exit + beneficiary status) experiences low directionality (d ≈ 0.15) because enforcement is voluntary and the authority benefits from maintaining the framework. Competing traditions (institutional power + constrained exit + victim status) experience moderate directionality (d ≈ 0.55) because their exit from the framework carries identity and social cost. The studying community (moderate power + constrained exit + mixed beneficiary-victim status) experiences moderate directionality (d ≈ 0.50) because they bear study burden but benefit from transmission role. The analytical observer (analytical power + analytical exit) observes the directionality structure without occupying a position within it (d ≈ 0.72 canonical fallback).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_contingency,
    'Is the archive-maintenance reading''s justification contingent on belief in imminent messianic arrival, or can it justify study obligation indefinitely without temporal expectations?',
    'Historical analysis of halakhic authorities'' statements about messianic timeline; correlation between declining messianic expectations and continued study mandate enforcement; contemporary rabbinic consensus on whether indefinite deferral remains coherent justification',
    'If contingent on imminent expectation: the reading''s legitimacy erodes as timeline recedes (archive becomes vestigial). If indefinite deferral is accepted: the reading remains stable but extraction on present generation increases (no foreseeable benefit realization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timeline_contingency, conceptual, 'Whether archive-maintenance reading requires imminent messianic expectation').

omega_variable(
    knowledge_degradation_rate,
    'At what rate does technical knowledge of Temple sacrifice procedures degrade or become obsolete across generations of study-only transmission (without practice)?',
    'Comparative analysis of detailed sacrifice procedures in early halakhic sources vs. contemporary understanding; assessment of accumulated errors or interpretive drift in the studied tradition; historical instances of rediscovery or correction of lost procedural details',
    'If degradation is rapid: archive function may fail (future generation cannot restore from degraded knowledge). If degradation is slow: archive function is viable but its success becomes empirical gamble rather than guaranteed coordination. Either case affects whether Rope classification (pure coordination, minimal extraction) is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_degradation_rate, empirical, 'Rate of technical knowledge degradation in study-only transmission').

omega_variable(
    reading_foreclosure_structure,
    'Does the archive-maintenance reading logically foreclose the study-as-performance reading within a single halakhic framework, or do the two readings represent coexistent legitimate positions?',
    'Analysis of foundational premises: does archive-maintenance REQUIRE that study is NOT performance (foreclosure), or does it merely prioritize future utility while study-as-performance prioritizes present fulfillment (coexistence)? Examination of whether contemporary authorities hold both positions simultaneously or treat them as mutually exclusive.',
    'If foreclosure: the reading produces asymmetric suppression of alternatives (strengthens extraction classification). If coexistence: the reading competes but does not eliminate alternatives (maintains tangled_rope classification from institutional perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether archive-maintenance forecloses study-as-performance reading').

omega_variable(
    present_vs_future_benefit_weighting,
    'On what basis does the halakhic authority weight present-generation costs against future-generation benefits? Is there an implicit discount rate, or is the benefit to future restoration treated as equally valuable to present performance?',
    'Examination of halakhic authorities'' explicit statements about temporal weighting; comparison of enforcement intensity for archive maintenance vs. other commandments; analysis of whether any competing present-commandment obligations are subordinated to archive maintenance',
    'If equal temporal weighting: the tangled_rope classification holds (genuine coordination justifies real extraction). If strong present-bias: extraction component increases (present costs unjustifiably burdened). If strong future-bias: coordination function becomes more purely future-directed (scaffold classification becomes more viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_vs_future_benefit_weighting, preference, 'Temporal weighting of present costs vs. future benefits in archive-maintenance justification').

omega_variable(
    alternative_interpretations_suppression,
    'How actively does the authority structure suppress or marginalize the study-as-performance and performance-only readings? Is suppression enforcement (active exclusion) or merely under-representation (passive marginalization)?',
    'Historical analysis of rabbinic treatment of alternative readings; examination of whether authorities explicitly forbid or merely deemphasize alternatives; assessment of whether practitioners who adopt alternative readings face sanctions or merely social pressure',
    'If active enforcement suppression: reading contributes to snare or tangled_rope classification of victim traditions. If passive marginalization: victim traditions have more exit capacity, reducing extraction from their perspective. Determines whether second tangled_rope perspective (competing traditions) should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_interpretations_suppression, empirical, 'Degree of active vs. passive suppression of alternative readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_arch_theater_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sac_arch_theater_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.52).
narrative_ontology:measurement(sac_arch_theater_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(sac_arch_extract_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sac_arch_extract_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.4).
narrative_ontology:measurement(sac_arch_extract_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sac_arch_suppress_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sac_arch_suppress_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(sac_arch_suppress_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, enforcement_mechanism).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, temple_restoration_preparedness).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, halakhic_authority_during_exile).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints corresponding to three interpretive readings. Each reading has its own ε value reflecting different balance of benefit-to-future vs. cost-to-present. Archive-maintenance (this story): ε=0.42, tangled_rope. Study-as-performance: ε ≈ 0.15-0.25, rope (study fulfills present obligation). Performance-only: ε ≈ 0.25-0.35, scaffold or piton (commandment is suspended during exile). All three remain live positions within contemporary halakhic discourse. Network links represent structural influence: archive-maintenance creates pressure on the other readings by redefining present-moment fulfillment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
