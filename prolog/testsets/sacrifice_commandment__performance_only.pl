% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment (Performance-Only Reading): Extraction Through Suspended Obligation
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice commandment instantiates a
 *   specific halakhic interpretation: the commandment to sacrifice animals in
 *   the Temple is obligatory and literal; without a standing Temple, the
 *   commandment is suspended (not fulfilled, not nullified, but dormant and
 *   binding). This reading emerged as authoritative in rabbinic law following
 *   the Temple's destruction in 70 CE. For nearly 1,900 years, Jewish
 *   scholarship has devoted enormous intellectual labor to codifying
 *   sacrifice law — the detailed procedures, the specific animals, the
 *   priestly protocols, the states of ritual purity — in texts that cannot be
 *   executed. The performance-only reading creates a structural extraction:
 *   scholarly attention is directed toward unperformable acts, victims are
 *   the community members whose obligation persists without satisfaction
 *   mechanism and the living law framework that receives less interpretive
 *   attention, and beneficiaries are the rabbinic authorities who maintain
 *   exclusive control over the interpretive apparatus for an obligation that
 *   has no performance outlet. This constraint is one reading of a contested
 *   kernel. The kernel is the sacrifice commandment itself — a foundational
 *   obligation in Jewish law grounded in Torah. Three readings compete:
 *   performance_only (this constraint) holds that the commandment requires
 *   physical execution and is suspended without Temple; study_as_performance
 *   holds that intellectual engagement with the law constitutes obedience;
 *   archive_maintenance holds that study preserves technical knowledge for
 *   future restoration without claiming the study itself is obedience. The
 *   performance-only reading is presented here as a snare with high
 *   extractiveness and theater, highlighting how the reading choice (not
 *   inevitable halakhic logic) extracts labor toward an obligation that
 *   cannot be satisfied.
 *
 * KEY AGENTS:
 *   - The Obligated Individual: Primary victim (powerless/trapped) — bears perpetual unsatisfiable obligation. Obligated to obey the commandment yet the sole performance mechanism is unavailable.
 *   - Scholarly Community: Secondary victim (moderate/constrained) — directed to preserve and codify unperformable law. Scholarship is obligatory (failure to preserve the commandment is itself a violation) yet produces no living application.
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — controls the interpretation and adjudication of the suspended commandment. The performance-only reading secures their epistemic gatekeeping role.
 *   - Messianic Restoration Framework: Tertiary agent (organized/arbitrage) — frames the suspended commandment as preparation for future Temple. Genuinely coordinates preparation while simultaneously extracting present resources for speculative future.
 *   - Living Law Framework: Implicit victim — receives less scholarly attention and institutional development because resources are diverted toward unperformable sacrifice law.
 *   - Analytical Observer: Sees the naturalizing move — the reading presents a contingent institutional arrangement (suspension following Temple destruction) as an immutable structural feature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.68).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.72).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment (Performance-Only Reading): Extraction Through Suspended Obligation").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '5b916318-2c86-4e42-98dd-51a65630fd84').
narrative_ontology:cs_kernel_codification('5b916318-2c86-4e42-98dd-51a65630fd84', fixed_text).
narrative_ontology:cs_authority_grounding('5b916318-2c86-4e42-98dd-51a65630fd84', lineage).
narrative_ontology:cs_interpretation_layer_present('5b916318-2c86-4e42-98dd-51a65630fd84').
narrative_ontology:cs_reading_relation('5b916318-2c86-4e42-98dd-51a65630fd84', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('5b916318-2c86-4e42-98dd-51a65630fd84', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('5b916318-2c86-4e42-98dd-51a65630fd84', foundational, physical_execution_required).
narrative_ontology:cs_axiom_status(physical_execution_required, holdable).
narrative_ontology:cs_axiom_grounding('5b916318-2c86-4e42-98dd-51a65630fd84', physical_execution_required, empirically_contingent).
narrative_ontology:cs_axiom('5b916318-2c86-4e42-98dd-51a65630fd84', foundational, suspension_not_nullification).
narrative_ontology:cs_axiom_status(suspension_not_nullification, holdable).
narrative_ontology:cs_axiom_grounding('5b916318-2c86-4e42-98dd-51a65630fd84', suspension_not_nullification, deontological).
narrative_ontology:cs_reference_frame('5b916318-2c86-4e42-98dd-51a65630fd84', bound_obligation_pending_restoration).
narrative_ontology:cs_drift_state('5b916318-2c86-4e42-98dd-51a65630fd84', contemporary_post_1900_years, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b916318-2c86-4e42-98dd-51a65630fd84', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention_pool).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, living_commandment_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OBLIGATED INDIVIDUAL (SNARE) — A Jewish person today encounters the sacrifice commandment as suspended, not fulfilled. They are obligated to obey the commandment, yet the sole means of obedience (Temple sacrifice) is removed by circumstance beyond their control. No exit from the obligation; no path to satisfaction. The commandment persists as binding law while its performance mechanism is structurally unavailable. Maximum extraction — the individual bears perpetual obligation without resolution.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SCHOLARLY COMMUNITY (SNARE) — Scholars of Jewish law are constrained by the performance-only reading to devote extensive intellectual labor to codifying, interpreting, and transmitting laws that cannot be executed. For 1,900 years, the field's resources (attention, time, institutional capacity) have been directed toward unperformable acts. The constraint extracts scholarly labor while suppressing alternative uses of that attention. The study is obligatory (failure to preserve the commandment is itself a violation), yet the output has no living application.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — From the institutional perspective of rabbinic authorities, the performance-only reading secures their coordinate function: they adjudicate the boundary between suspended and living law, control the interpretive apparatus for an unperformable commandment, and maintain the authority to determine when (if ever) the commandment reverts to performable status. The constraint coordinates the community around the interpretive hierarchy. Net beneficiary — rabbinic authority experiences this as a coordination mechanism that secures their epistemic role.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MESSIANIC RESTORATION FRAMEWORK (TANGLED ROPE) — The eschatological reading sees the suspended commandment as part of a genuine coordination structure: preparing the community for eventual restoration. The study of sacrifice law is coordinating effort toward a future state. However, this reading is also complicit in extracting present scholarly labor for a speculative future. The framework genuinely coordinates preparation activities while simultaneously extracting resources for activities (Temple construction, priestly training) that may never occur. Mixed coordination and extraction.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE RITUAL PRESERVATION SYSTEM (PITON) — The elaborate system of sacrifice law codification, transmission, and study appears increasingly performative from a civilizational perspective. The system maintains itself through institutional inertia and reverence for the ancient obligation. Yet its core function (enabling obedience to the commandment) is structurally disabled. The theater ratio is very high — the codification, commentary, and study ritual persists while the performance mechanism remains absent. The piton classification reflects that the system's primary function has atrophied, but the constraint survives due to institutional memory and cultural authority.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From the analytical position, the performance-only reading appears to describe an immutable structural limit: a commandment whose performance mechanism is permanently severed by historical circumstance. The reading naturalizes this as 'the way Jewish law works' — commandments can be suspended by external events beyond human control. Yet the structural data reveals beneficiaries (rabbinic authority) and victims (scholarly attention), indicating that what appears as natural constraint is actually a contingent institutional arrangement. The false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sacrifice_commandment__performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sacrifice_commandment__performance_only, TR),
    TR >= 0.70.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The performance-only reading directs scholarly labor toward codifying an obligation that cannot be executed. This is not a trivial coordination task — it is 1,900 years of intellectual effort directed at an unperformable commandment. The extractiveness is high because the reading choice (not inherent halakhic logic) sustains the extraction. The study-as-performance reading would preserve the scholarly function while eliminating the suspended-obligation dynamic; archive-maintenance would clarify that study is preservation, not obedience. That the performance-only reading persists despite these alternatives suggests the reading is sustained by beneficiaries who profit from the extraction (rabbinic authority). The value 0.68 reflects that the extraction is significant but not maximal — the scholarly community does genuinely believe in the obligation's validity and does not uniformly resist the labor. Suppression (0.72): High. The performance-only reading suppresses the study-as-performance alternative, which is halakhically coherent and would reduce the extraction dynamic. Historical evidence shows that study-as-performance readings existed in medieval Jewish philosophy and Kabbalistic traditions but were subordinated to the rabbinic-institutional performance-only reading. The suppression is both active (alternative readings are labeled heretical or intellectually deficient) and structural (rabbinic institutions control the resources for what counts as legitimate scholarship). Theater ratio (0.85): Very high and rising over time. The measurement trajectory shows theater increasing from 0.60 (early period, when sacrifice law was still practiced by some Samaritans and fragments of the Jewish diaspora) to 0.85 (contemporary, when the performance mechanism is completely unavailable and the study has become purely commemorative and preparatory). The high theater reflects that the scholarly activity is sustained by reverence for the ancient obligation and messianic hope rather than by functional performance. The system performs obedience through study even though study cannot satisfy the obligation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same halakhic rule produces radically different classification experiences. The obligated individual is trapped in an unsatisfiable obligation — snare. Rabbinic authority sees the constraint as coordinating the scholarly hierarchy and their interpretive role — rope. The scholarly community experiences extraction with some agency (they could adopt alternative readings) — tangled rope. The messianic framework sees coordination toward restoration — tangled rope. The preservation system sees its own atrophy but maintains itself through institutional inertia — piton. The analytical observer at civilizational scope risks naturalizing the reading as 'how Jewish law necessarily works' — mountain (false summit). The perspectival gaps reveal that the performance-only reading is not inevitable. Different readings would produce different classification structures. The gap is not an artifact of the observer's knowledge state but a feature of the reading choice itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The performance-only reading's directionality derives from the structural relationship of each agent to the extraction mechanism. The obligated individual experiences full d=1.0 (victim/trapped): they bear the obligation with no exit. The scholarly community experiences d=0.85 (victim/constrained): they are directed to preserve the law but could theoretically exit through adopting alternative readings; the exit cost is high (professional stigma, institutional retaliation). Rabbinic authority experiences d=0.05 (beneficiary/arbitrage): they control the interpretation apparatus and can exit by reinterpreting the reading; the exit cost is low because alternative readings are already available in the tradition. The messianic framework experiences d=0.50 (mixed): it both coordinates genuine preparation and extracts resources; beneficiaries and victims are distributed. These directionalities feed the sigmoid f(d) to produce effective extractiveness chi: higher d → higher f(d) → higher chi for victims; lower d → negative f(d) → lower chi for beneficiaries. The perspectival gap is large: the obligated individual experiences a snare (chi near 1.0); rabbinic authority experiences a rope (chi near 0.0); the analytical observer sees a snare being maintained by beneficiary choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading avoids mandatrophy (confusion between coordination and extraction) by being explicit about the extraction mechanism. The reading acknowledges the snare structure: the obligation persists (coordination feature) but cannot be satisfied (extraction feature). The messianic framing provides partial cover for the extraction by claiming the study is preparation for future restoration. However, the theater ratio (0.85) reveals that the preparation function is largely performative — most of the scholarly labor is devoted to transmission and reverence rather than to logistics of actual restoration. The mandatrophy is resolved by recognizing that this constraint is a *reading choice*, not an inevitable feature of Jewish law. The alternative readings (study-as-performance, archive-maintenance) would eliminate or reduce the snare structure. That the performance-only reading persists suggests it serves the interests of beneficiaries (rabbinic authority) rather than the obligated individuals or scholarly community. The engine's mandatrophy resolution here is: acknowledge the mixed coordination-extraction character, identify who benefits from the reading choice, and flag the extraction as partly contingent rather than inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_nullification,
    'Is the sacrifice commandment genuinely suspended (dormant, awaiting Temple restoration) or effectively nullified (permanently inapplicable)?',
    'Historical analysis of halakhic treatment: (a) does rabbinic law preserve the commandment for future performance, or (b) does it treat the commandment as permanently non-binding? Empirical test: would restoration of a Temple automatically restore the obligation, or would new legislation be required?',
    'If suspended: the performance-only reading is correct, and the snare classification holds (obligation persists without satisfaction mechanism). If nullified: the obligation is discharged by circumstance, and the extraction is less severe (archival victims are lower because the obligation itself is discharged). If ambiguous: the performance-only reading exploits the ambiguity to extract sustained scholarly labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_nullification, empirical, 'Whether the sacrifice commandment is suspended or nullified').

omega_variable(
    performance_only_vs_study_equivalence,
    'Is the performance-only reading''s strict requirement for physical execution the only coherent interpretation of the obligation, or is it one reading competing against the study-as-performance reading?',
    'Textual analysis: do the foundational sources (Torah, early rabbinic sources) logically require physical performance, or do they permit interpretive readings where study constitutes performance? Historical-sociological analysis: what historical conditions led each reading to prominence?',
    'If performance-only is logically necessary: the reading correctly captures the commandment''s meaning, and the snare classification reflects a genuine structural feature (not constructed extraction). If study-as-performance is coherent: the performance-only reading is a *choice* to extract, not a constraint imposed by the law itself, and the extractiveness value should be higher (0.75+) with an omega documenting the deliberate suppression of the alternative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_only_vs_study_equivalence, conceptual, 'Whether performance-only reading is logically necessary or one among competing interpretations').

omega_variable(
    rabbinic_authority_maintenance,
    'Does the rabbinic interpretive apparatus genuinely require the performance-only reading to maintain authority, or would the study-as-performance reading preserve or enhance rabbinic control?',
    'Institutional analysis: under study-as-performance, would rabbinic interpretation maintain the same gatekeeping function? Would individual study replace institutional authority? Historical precedent: have periods of rabbinic authority decline or growth correlated with acceptance of study-as-performance?',
    'If rabbinic authority requires performance-only: the beneficiary declaration is accurate. If study-as-performance would maintain or enhance rabbinic authority: the choice of performance-only reveals deliberate extraction — the reading is selected to maintain hierarchical control over scholarship. This would move the constraint toward a higher snare classification (0.72+) with explicit documentation that the reading choice is extractive, not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_maintenance, empirical, 'Whether rabbinic authority depends on the performance-only reading').

omega_variable(
    false_summit_candidate,
    'Is the performance-only reading presenting a contingent institutional arrangement (suspension of sacrifice law due to Temple destruction) as though it were a natural limit (permanent impossibility of obedience)?',
    'Structural analysis: the performance-only reading declares rabbinic authority as beneficiary and scholarly attention as victim. If these declarations are accurate, the reading is naturalizing a constructed constraint. Historical-counterfactual: if the Temple were restored tomorrow, would the obligation revert to performance? If yes, the constraint is contingent, not immutable. If no, the obligation is genuinely nullified (not suspended), and the framing is misleading.',
    'If the reading is a false summit: the engine''s FSM detector will reclassify from mountain to tangled_rope or snare. The performance-only reading would be revealed as extraction through naturalization — presenting an institutional choice (to suspend the commandment and extract scholarly labor toward its preservation) as though it were an inevitable natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether performance-only reading naturalizes a contingent institutional arrangement').

omega_variable(
    messianic_timeline_extraction,
    'Does the messianic framing of the suspended commandment extract scholarly labor for a speculative future, or does it represent genuine coordination toward a shared eschatological goal?',
    'Content analysis of halakhic and theological sources: what fraction of sacrifice law codification is framed as preparation for restoration vs. preservation for its own sake? Empirical measurement: do communities that de-emphasize messianism (secular, reform movements) reduce scholarly labor on sacrifice law? Do communities that emphasize restoration (certain haredi, Kabbalistic traditions) increase it?',
    'If messianic framing is central to extraction: the snare classification is accurate, and the theater ratio (0.85) reflects that the study is sustained by eschatological hope rather than by functional obligation. If messianic framing is genuine coordination: the constraint transitions toward tangled_rope (mixed coordination-extraction), and the extraction is more justified as a legitimate cost of preparation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timeline_extraction, empirical, 'Whether messianic framing extracts or coordinates').

omega_variable(
    archive_maintenance_alternative,
    'Would the archive-maintenance reading (study preserves technical knowledge without claiming the study IS obedience) reduce extractiveness by clarifying the non-performance status?',
    'Comparative analysis: communities adopting archive-maintenance framing vs. communities using study-as-performance or performance-only. Do archive-maintenance communities experience lower theater ratios? Do they redirect resources toward living commandments more readily?',
    'If archive-maintenance reduces extraction: the performance-only reading''s high extractiveness (0.68) is partially a choice to extract rather than a necessary feature of halakhic law. If archive-maintenance produces the same extractiveness: the reading is constrained by deeper structural features.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_maintenance_alternative, empirical, 'Whether archive-maintenance reading reduces extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_perf_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.6).
narrative_ontology:measurement(sac_perf_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.72).
narrative_ontology:measurement(sac_perf_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.8).
narrative_ontology:measurement(sac_perf_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.85).

% Extraction over time
narrative_ontology:measurement(sac_perf_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sac_perf_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(sac_perf_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(sac_perf_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, enforcement_mechanism).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, rabbinic_interpretive_authority_preservation).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, temple_restoration_eschatology).

% DUAL FORMULATION NOTE:
% The sacrifice commandment kernel decomposes into three distinct constraint stories, each with a different ε value and classification. performance_only (this story, ε=0.68, snare) frames the obligation as suspended and unperformable, extracting scholarly labor. study_as_performance (ε=0.25, rope) treats intellectual engagement as obedience, eliminating the extraction. archive_maintenance (ε=0.35, tangled_rope) clarifies that study is preservation without claiming obedience, reducing but not eliminating extraction. These three constraints are linked by the kernel: the same foundational text (Torah sacrifice commandment) is read three different ways, producing three different structural profiles. The network relationships document how adoption of performance_only affects the others: performance_only's authority suppresses study_as_performance and archive_maintenance as valid halakhic readings, maintaining extractive control over the scholarly apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
