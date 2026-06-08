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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_only_reading
 *   human_readable: Sacrifice Obligation: Performance-Only Reading (Halakhic Authority)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice obligation kernel asserts
 *   that the mitzvah (commandment) to perform sacrificial service requires
 *   physical performance at the Temple altar; study, prayer, and other
 *   substitute practices are preparatory and spiritually valuable but do not
 *   fulfill the obligation itself. This reading has been maintained by
 *   halakhic authorities for nearly 1,900 years despite the structural
 *   impossibility of performance — the Second Temple was destroyed in 70 CE,
 *   and no Jewish authority has claimed the capacity to rebuild it or resume
 *   sacrificial service. The constraint is a snare: the Jewish people are
 *   commanded to perform an act they cannot perform, and the obligation
 *   persists unfulfilled. The performance-only reading naturalizes this gap
 *   as a feature of halakhic law rather than a problem to be solved. The
 *   extractiveness value (0.95) reflects the severity of the gap: an entire
 *   people commanded to an unfulfillable obligation, with no exit and no
 *   beneficiary. The theater ratio (0.65) reflects that the reading is
 *   maintained through interpretive discipline and formal legal reasoning,
 *   but the actual practice of Jewish life has long since migrated to
 *   substitute practices. The suppression (0.88) reflects that the obligation
 *   cannot be renounced — it persists as a binding commandment even though
 *   performance is impossible.
 *
 * KEY AGENTS:
 *   - Jewish People: Primary victim (powerless/trapped) — commanded to perform sacrificial service; structurally unable to perform for 1,900+ years; no exit from the obligation
 *   - Halakhic Authority: Institutional maintainer (institutional/constrained) — maintains the performance-only reading through interpretive discipline; enforces the principle that study does not fulfill the obligation
 *   - Rabbinic Interpretive Community: Organized mediator (organized/constrained) — develops substitute practices (study, prayer, commemoration) that acknowledge the gap while maintaining fidelity to the obligation
 *   - Sibling Readings: Competing interpretive frameworks (analytical/analytical) — study-as-exercise reading, messianic-suspension reading, symbolic-archive reading each offer different resolutions to the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only_reading, 0.95).
domain_priors:suppression_score(performance_only_reading, 0.88).
domain_priors:theater_ratio(performance_only_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(performance_only_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(performance_only_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only_reading, snare).
narrative_ontology:human_readable(performance_only_reading, "Sacrifice Obligation: Performance-Only Reading (Halakhic Authority)").
narrative_ontology:topic_domain(performance_only_reading, "religious_law/halakhic_authority/commitment_system").

domain_priors:requires_active_enforcement(performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only_reading, 'd31878ec-766c-4803-a9e6-c5fa51d9f724').
narrative_ontology:cs_kernel_codification('d31878ec-766c-4803-a9e6-c5fa51d9f724', fixed_text).
narrative_ontology:cs_authority_grounding('d31878ec-766c-4803-a9e6-c5fa51d9f724', lineage).
narrative_ontology:cs_interpretation_layer_present('d31878ec-766c-4803-a9e6-c5fa51d9f724').
narrative_ontology:cs_reading_relation('d31878ec-766c-4803-a9e6-c5fa51d9f724', performance_only_reading__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('d31878ec-766c-4803-a9e6-c5fa51d9f724', performance_only_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('d31878ec-766c-4803-a9e6-c5fa51d9f724', performance_only_reading__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('d31878ec-766c-4803-a9e6-c5fa51d9f724', foundational, physical_performance_required).
narrative_ontology:cs_axiom_status(physical_performance_required, holdable).
narrative_ontology:cs_axiom_grounding('d31878ec-766c-4803-a9e6-c5fa51d9f724', physical_performance_required, deontological).
narrative_ontology:cs_axiom('d31878ec-766c-4803-a9e6-c5fa51d9f724', foundational, study_preparatory_not_fulfilling).
narrative_ontology:cs_axiom_status(study_preparatory_not_fulfilling, holdable).
narrative_ontology:cs_axiom_grounding('d31878ec-766c-4803-a9e6-c5fa51d9f724', study_preparatory_not_fulfilling, deontological).
narrative_ontology:cs_reference_frame('d31878ec-766c-4803-a9e6-c5fa51d9f724', temple_sacrificial_service_required).
narrative_ontology:cs_drift_state('d31878ec-766c-4803-a9e6-c5fa51d9f724', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d31878ec-766c-4803-a9e6-c5fa51d9f724', '').
narrative_ontology:cs_kernel_id(performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(performance_only_reading, jewish_people_commanded_unable_to_perform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_only_reading, rabbinic_interpretive_community).
narrative_ontology:constraint_victim(performance_only_reading, jewish_people_commanded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded to perform sacrificial service at the Temple altar. The Temple was destroyed in 70 CE. For 1,900+ years, the Jewish people have been unable to perform the obligation. The performance-only reading asserts that study, prayer, and other substitute practices do not fulfill the mitzvah. The people bear the full weight of an unfulfillable obligation. They cannot renounce the commandment; they cannot reinterpret it away (within this reading's framework); they cannot perform it. The obligation persists as a binding legal principle despite its structural impossibility.
narrative_ontology:constraint_stakeholder(performance_only_reading, jewish_people_commanded, payer,
    powerless, civilizational, trapped, universal).

% Maintains the performance-only reading through interpretive discipline and formal legal reasoning. The authority enforces the principle that study does not fulfill the obligation. The authority's power derives from its role as the authoritative interpreter of Jewish law. The authority benefits from the structural dependence created by the gap — the people depend on rabbinic guidance to navigate the unfulfillable obligation. The authority maintains the reading because it is part of the authoritative legal corpus and because it preserves the coherence of the halakhic system.
narrative_ontology:constraint_stakeholder(performance_only_reading, halakhic_authority, agenda_setter,
    institutional, civilizational, constrained, universal).

% Develops substitute practices (study, prayer, commemoration) that acknowledge the gap between the obligation and the capacity to perform. The community coordinates the Jewish people's relationship to the unfulfillable obligation through interpretive work. The community benefits from the structural dependence created by the gap — they become indispensable mediators between the obligation and the people. The community's interpretive authority is grounded in the halakhic tradition and the community's role as transmitters of Jewish law.
narrative_ontology:constraint_stakeholder(performance_only_reading, rabbinic_interpretive_community, agenda_setter,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(performance_only_reading, rabbinic_interpretive_community, beneficiary).

% Alternative interpretive frameworks that offer different resolutions to the gap between command and capacity. The study-as-exercise reading asserts that study fulfills the obligation. The messianic-suspension reading asserts that the obligation is suspended until the Temple is rebuilt. The symbolic-archive reading asserts that the obligation is transformed into a symbolic practice. These readings are not present in the room when the performance-only reading is maintained, but they represent live alternatives that could be adopted if the halakhic authority's interpretive power were challenged.
narrative_ontology:constraint_stakeholder(performance_only_reading, sibling_readings, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(performance_only_reading, sibling_readings).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How does a community maintain fidelity to a binding commandment it cannot fulfill? The performance-only reading's answer: maintain the obligation as stated while developing substitute practices that acknowledge the gap. This is genuine coordination — the community needs a shared framework for relating to the unfulfillable.
% TRANSFER_FUNCTION: The reading transfers interpretive authority from the people to the halakhic authority. The people are commanded to perform; the authority interprets what the commandment means and whether substitutes are legitimate. The reading also transfers the burden of the unfulfillable obligation from the authority to the people — the people bear the weight of the gap, while the authority maintains the principle.
% ABSENT_VOICES: The voices of those who would adopt sibling readings are absent from the performance-only framework. Those who would argue that study fulfills the obligation, or that the obligation is suspended, or that it is transformed into a symbolic practice — these voices are excluded from the room where the performance-only reading is maintained. The exclusion is not physical but interpretive: the performance-only reading forecloses or constrains these alternatives within a single halakhic framework.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared, the Jewish people would need to adopt a sibling reading or develop a new interpretation of the obligation. The world would not rearrange itself — the Temple would still be destroyed, and performance would still be impossible. But the community's relationship to the obligation would change. The disappearance of the performance-only reading would open space for alternative interpretations that make the obligation fulfillable or suspend it. The contest is over whether the obligation persists unfulfilled (performance-only) or is transformed by alternative readings.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE created a structural impossibility: the Jewish people were commanded to perform sacrificial service, but the Temple was destroyed and no authority claimed the capacity to rebuild it. The performance-only reading was developed to maintain the obligation as a binding legal principle despite this impossibility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (the Temple's destruction) is attested by historical sources and is accepted by all readings of the sacrifice obligation kernel. The performance-only reading's response to the dead founding problem is to maintain the obligation as unfulfilled. The study-as-exercise reading's response is to reinterpret the obligation as fulfillable through study. The messianic-suspension reading's response is to suspend the obligation pending the Temple's rebuilding. The symbolic-archive reading's response is to transform the obligation into a symbolic practice. The corroboration comes from the fact that all readings acknowledge the Temple's destruction; they differ in their interpretation of what this means for the obligation.
narrative_ontology:disappearance_verdict(performance_only_reading, contested).
narrative_ontology:founding_problem_status(performance_only_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JEWISH PEOPLE (SNARE) — Commanded to perform sacrificial service; structurally unable to perform for 1,900+ years (no Temple, no altar, no priestly service). The obligation persists; the capacity to fulfill it does not. Exit is impossible — the commandment cannot be renounced, only suspended or reinterpreted. Maximum extraction: the gap between command and capacity is unbridgeable within this reading's framework. The people bear the full weight of an unfulfillable obligation.
constraint_indexing:constraint_classification(performance_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: HALAKHIC AUTHORITY (MOUNTAIN) — From the institutional seat, the performance-only reading appears as an immutable legal principle: the mitzvah requires physical performance; study and prayer are preparatory but do not fulfill the obligation. This reading naturalizes the gap between command and capacity as a structural feature of halakhic law itself — not something to be resolved, but something to be maintained as written. The authority enforces the reading's coherence through interpretive discipline, treating the unfulfillable obligation as a permanent feature of the legal landscape.
constraint_indexing:constraint_classification(performance_only_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE COMMUNITY (TANGLED ROPE) — The rabbis coordinate the Jewish people's relationship to an unfulfillable obligation through interpretive work: they develop the halakhic framework that acknowledges the obligation while creating substitute practices (study, prayer, ritual commemoration). This is genuine coordination — the community needs a way to relate to the commandment despite its unfulfillability. But it is also extraction: the rabbis' interpretive authority becomes indispensable precisely because the obligation cannot be fulfilled. The people depend on rabbinic guidance to navigate the gap. The rabbis benefit from this structural dependence through institutional authority and interpretive power.
constraint_indexing:constraint_classification(performance_only_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: PERFORMANCE-ONLY READING AS PITON (INSTITUTIONAL THEATER) — The reading itself has become largely performative. Halakhic authorities maintain the principle that 'study does not fulfill the obligation' through interpretive discipline and formal legal reasoning, but the actual practice of Jewish life has long since migrated to substitute practices (study, prayer, commemoration). The performance-only reading persists through institutional inertia — it is maintained because it is part of the authoritative legal corpus, not because it functions as a guide to actual practice. The theater ratio (0.65) reflects this: significant performative content (maintaining the principle) alongside residual functional content (the principle still constrains how substitutes are framed).
constraint_indexing:constraint_classification(performance_only_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a purely structural standpoint, the performance-only reading solves a genuine coordination problem: how does a community maintain fidelity to a commandment it cannot fulfill? The reading's answer is: maintain the obligation as stated while developing substitute practices that acknowledge the gap. This is coordination — the community needs a shared framework for relating to the unfulfillable. But the engine will compute this as a false summit (rope misclassified as mountain from the institutional perspective) because the reading naturalizes what is actually a contingent interpretive choice.
constraint_indexing:constraint_classification(performance_only_reading, rope,
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
    constraint_indexing:constraint_classification(performance_only_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_only_reading, TR),
    TR >= 0.70.

:- end_tests(performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.95): Extreme. The performance-only reading creates a structural impossibility: a binding obligation that cannot be fulfilled. The Jewish people bear the full weight of this gap. There is no beneficiary — no agent collects from the unfulfillability. The extraction is pure structural impossibility, not extraction by an agent. The value reflects the severity of the gap and the fact that the obligation persists unfulfilled across 1,900 years. Suppression (0.88): Very high. The obligation cannot be renounced or reinterpreted away (within this reading's framework). The only exit would be to adopt a sibling reading (study-as-exercise, messianic suspension, symbolic archive), but the performance-only reading forecloses or constrains these alternatives. The suppression reflects the binding nature of the commandment and the halakhic authority's power to maintain the reading. Theater ratio (0.65): Moderate-high. The reading is maintained through formal legal reasoning and interpretive discipline, but the actual practice of Jewish life has long since migrated to substitute practices. The performance-only principle persists as a formal legal position while being functionally displaced by substitutes. The theater has increased over time as the gap between the principle and practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The performance-only reading produces a stark perspectival gap. From the Jewish people's perspective, the obligation is a snare — unfulfillable and inescapable. From the halakhic authority's perspective, the reading is a mountain — an immutable legal principle that naturalizes the gap. From the rabbinic interpretive community's perspective, the reading is a tangled rope — they coordinate the people's relationship to the obligation through substitute practices, but this coordination depends on the people's structural inability to perform. From the analytical perspective, the reading is a rope — it solves the coordination problem of how to relate to an unfulfillable obligation — but the engine will compute it as a false summit because the reading naturalizes a contingent interpretive choice. The piton perspective reveals that the reading is maintained through institutional inertia: the principle persists because it is part of the authoritative legal corpus, not because it functions as a guide to practice.
 *
 * DIRECTIONALITY LOGIC:
 *   The performance-only reading creates a unique directionality structure: there is no beneficiary. The Jewish people (powerless/trapped) experience maximum extraction because they are commanded to an unfulfillable obligation. The halakhic authority (institutional/constrained) maintains the reading through interpretive power, but does not collect rents from the unfulfillability — the authority's benefit is institutional (maintaining the coherence of the legal system), not extractive. The rabbinic community (organized/constrained) benefits from the structural dependence created by the gap — they become indispensable mediators between the obligation and the people — but this is a secondary effect of the reading, not its primary function. The directionality is unusual because the constraint is not extraction by an agent but structural impossibility maintained as a legal principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading exhibits mandatrophy: the mandate (perform sacrificial service) has outlived its function (the Temple is destroyed; performance is impossible). The reading resolves the mandatrophy by maintaining the obligation as a binding legal principle despite its unfulfillability. This is not a resolution in the sense of solving the problem — it is a resolution in the sense of accepting the gap as permanent. The reading's persistence depends on the halakhic authority's power to maintain it and the community's acceptance of the obligation's unfulfillability. The sibling readings (study-as-exercise, messianic suspension, symbolic archive) offer alternative resolutions: they either reinterpret the obligation to make it fulfillable, or suspend it pending the messianic age, or transform it into a symbolic practice. The performance-only reading forecloses or constrains these alternatives, maintaining the gap as irreducible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the performance-only reading a genuine halakhic principle or a contingent interpretive choice that could be otherwise?',
    'Historical analysis of rabbinic sources: do early authorities present performance-only as a necessary logical principle, or as one interpretive option among others? Comparison with sibling readings'' textual grounding.',
    'If necessary principle: the reading is structurally sound and the gap is irreducible. If contingent choice: the reading is one framing among others, and the performance-only constraint is a product of interpretive authority, not halakhic logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether performance-only is a necessary halakhic principle or contingent interpretive choice').

omega_variable(
    substitute_practice_legitimacy,
    'Do substitute practices (study, prayer, commemoration) genuinely fulfill the obligation''s intent, or do they merely manage the gap between command and capacity?',
    'Textual analysis of rabbinic justifications for substitutes; comparison of the obligation''s stated purpose with the substitutes'' actual function; examination of whether authorities claim substitutes fulfill or merely replace the obligation.',
    'If substitutes fulfill intent: the performance-only reading is false — study does fulfill the mitzvah, just not through physical performance. If substitutes merely manage the gap: the performance-only reading is correct, and the obligation remains unfulfilled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitute_practice_legitimacy, conceptual, 'Whether substitutes fulfill the obligation''s intent or merely manage the gap').

omega_variable(
    authority_grounding_source,
    'What grounds the halakhic authority''s power to maintain the performance-only reading despite 1,900 years of non-performance?',
    'Genealogy of the reading''s authority: does it derive from textual interpretation (lineage), institutional practice (practice-based authority), or the authority''s own interpretive power (extraction)? Examination of whether the reading is defended through logical argument or through assertion of interpretive jurisdiction.',
    'If textual/logical: the reading''s authority is grounded in the kernel itself. If institutional/practice-based: the reading''s authority derives from community acceptance. If extraction-based: the reading''s persistence depends on the authority''s power to enforce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_source, empirical, 'Source of halakhic authority''s power to maintain the performance-only reading').

omega_variable(
    messianic_suspension_relationship,
    'Is the performance-only reading compatible with messianic suspension, or does the reading foreclose the possibility that the obligation could be suspended pending the messianic age?',
    'Textual analysis: do authorities who hold the performance-only reading also hold that the obligation is suspended until the Temple is rebuilt? Or do they hold that the obligation persists unfulfilled? Examination of whether the two readings can coexist in a single framework.',
    'If compatible: the readings coexist. If incompatible: the performance-only reading forecloses messianic suspension within a single halakhic framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_suspension_relationship, conceptual, 'Compatibility of performance-only reading with messianic suspension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_theater_t0_temple_destruction, performance_only_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(perf_theater_t500_talmudic_consolidation, performance_only_reading, theater_ratio, 500, 0.45).
narrative_ontology:measurement(perf_theater_t1000_medieval_codification, performance_only_reading, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(perf_theater_t1500_early_modern, performance_only_reading, theater_ratio, 1500, 0.63).
narrative_ontology:measurement(perf_theater_t1900_contemporary, performance_only_reading, theater_ratio, 1900, 0.65).

% Extraction over time
narrative_ontology:measurement(perf_extract_t0_temple_destruction, performance_only_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(perf_extract_t500_talmudic_consolidation, performance_only_reading, base_extractiveness, 500, 0.94).
narrative_ontology:measurement(perf_extract_t1000_medieval_codification, performance_only_reading, base_extractiveness, 1000, 0.95).
narrative_ontology:measurement(perf_extract_t1500_early_modern, performance_only_reading, base_extractiveness, 1500, 0.95).
narrative_ontology:measurement(perf_extract_t1900_contemporary, performance_only_reading, base_extractiveness, 1900, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(perf_suppress_t0_temple_destruction, performance_only_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(perf_suppress_t500_talmudic_consolidation, performance_only_reading, suppression_requirement, 500, 0.87).
narrative_ontology:measurement(perf_suppress_t1000_medieval_codification, performance_only_reading, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(perf_suppress_t1500_early_modern, performance_only_reading, suppression_requirement, 1500, 0.88).
narrative_ontology:measurement(perf_suppress_t1900_contemporary, performance_only_reading, suppression_requirement, 1900, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice obligation kernel decomposes into four structurally distinct constraint stories, each representing a different reading of the same binding commandment. The performance-only reading asserts that physical performance is required; the other readings offer alternative interpretations. Each reading has its own extractiveness value, its own beneficiary/victim structure, and its own classification. The readings are linked through the kernel: they compete for interpretive authority over the same binding commandment. The performance-only reading forecloses or constrains the alternatives within a single halakhic framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
