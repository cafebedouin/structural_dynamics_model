% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment Archive Maintenance (Archive Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The archive-maintenance reading of the sacrifice commandment frames
 *   post-Temple study of sacrifice law as a form of obligation fulfillment
 *   justified by preservation of technical knowledge for eventual messianic
 *   Temple restoration. Under this reading, present scholars engage in
 *   detailed study (animal anatomy, procedural sequences, Temple
 *   architecture, halachic reasoning) not because they expect to perform
 *   sacrifice imminently, but because a future Jewish community under
 *   messianic conditions will need this knowledge intact and undistorted. The
 *   reading posits a deferred beneficiary (the future community) and a
 *   present cost (scholars' time and effort, community support structure).
 *   This creates a tangled-rope structure: genuine coordination function
 *   (preserving knowledge against loss) combined with asymmetric extraction
 *   (present practitioners bear cost, future beneficiaries collect value,
 *   present community bears maintenance cost). The present value of the
 *   archive is uncertain—messianic restoration is indefinitely projected,
 *   probability undefined—making the extractiveness moderate rather than
 *   negligible.
 *
 * KEY AGENTS:
 *   - Halakhic scholars: dedicated practitioners bearing the study obligation; identity-locked to the knowledge-transmission role
 *   - Rabbinical authority: frames and enforces the archive-maintenance rationale; benefits indirectly from institutional continuity
 *   - Present Jewish community: sustains scholars and accepts the commandment as obligatory; bears indirect maintenance cost
 *   - Future messianic community: projected beneficiary; non-agent, indefinitely deferred
 *   - Competing reading holders (performance-only, study-as-performance): structurally excluded; would contest the archive rationale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.58).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.41).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment Archive Maintenance (Archive Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '50630046-978c-4347-9987-64b597f04b7f').
narrative_ontology:cs_kernel_codification('50630046-978c-4347-9987-64b597f04b7f', formalized).
narrative_ontology:cs_authority_grounding('50630046-978c-4347-9987-64b597f04b7f', lineage).
narrative_ontology:cs_interpretation_layer_present('50630046-978c-4347-9987-64b597f04b7f').
narrative_ontology:cs_reading_relation('50630046-978c-4347-9987-64b597f04b7f', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('50630046-978c-4347-9987-64b597f04b7f', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('50630046-978c-4347-9987-64b597f04b7f', foundational, archive_preservation_fulfills_commandment).
narrative_ontology:cs_axiom_status(archive_preservation_fulfills_commandment, holdable).
narrative_ontology:cs_axiom_grounding('50630046-978c-4347-9987-64b597f04b7f', archive_preservation_fulfills_commandment, instrumental).
narrative_ontology:cs_axiom('50630046-978c-4347-9987-64b597f04b7f', secondary, messianic_restoration_probable).
narrative_ontology:cs_axiom_status(messianic_restoration_probable, holdable).
narrative_ontology:cs_axiom_grounding('50630046-978c-4347-9987-64b597f04b7f', messianic_restoration_probable, deontological).
narrative_ontology:cs_reference_frame('50630046-978c-4347-9987-64b597f04b7f', temple_absence_with_knowledge_preservation).
narrative_ontology:cs_drift_state('50630046-978c-4347-9987-64b597f04b7f', contemporary_two_millennia_post_destruction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50630046-978c-4347-9987-64b597f04b7f', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_messianic_community).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_study_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, present_jewish_community).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dedicated to precise transmission and study of sacrifice law, spending decades mastering ritual procedures, animal classifications, and Temple geometry that have no immediate practical application. Their scholarly identity is constituted through this knowledge transmission; departure from the study obligation would dissolve their vocational role and community standing. They bear the opportunity cost of study time that could address present halachic problems or pastoral needs.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, payer,
    organized, generational, identity_locked, global).

% A non-agent collective future entity. If and when the Temple is rebuilt under messianic conditions, this community will inherit precise knowledge of sacrifice procedures, animal requirements, and ritual specifications. The archive reading posits that present study preserves this knowledge against loss or corruption.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_messianic_community, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__archive_maintenance, future_messianic_community).

% Frames and enforces the interpretive rule that study of sacrifice law counts as fulfillment of the commandment, with the explicit rationale of preserving knowledge for future Temple restoration. Maintains the halachic and social structures that make this study obligatory and prestigious. Benefits indirectly through institutional continuity and doctrinal coherence.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinical_authority, agenda_setter,
    institutional, generational, mobile, global).

% Sustains the scholars through institutional support (yeshiva funding, communal prestige, social scaffolding) and accepts the commandment as obligatory on qualified practitioners. They receive continuity of tradition and textual expertise applied to present halachic questions, but also bear the maintenance cost of supporting scholarship whose direct utility is deferred or uncertain.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_jewish_community, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, present_jewish_community, beneficiary).

% Scholars and communities who hold competing readings (performance-only or study-as-performance) are structurally absent from the authority structures that enforce the archive-maintenance reading. Their objection that study without present physical preparation for sacrifice is inauthentic or unfulfilled is excluded from the framework that legitimates current practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, performance_reading_holders, excluded,
    moderate, biographical, constrained, regional).

% External analysis of the constraint structure, examining whether the archive-maintenance reading represents genuine coordination for future restoration or extraction of study obligation from present practitioners justified by deferred benefit.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, rabbinical_authority).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complete, precise knowledge of sacrifice ritual—animal types, anatomical specifications, procedural sequences, Temple architecture, and halachic reasoning—in written form and embodied scholarly practice, against loss, textual corruption, or forgetting across generations without Temple operation. Solves the coordination problem: how does a religious community maintain technical knowledge for an obligation it cannot perform, ensuring that knowledge is available and undistorted if future circumstances permit performance?
% TRANSFER_FUNCTION: Transfers the obligation of study from direct performance (impossible without Temple) to intellectual engagement and textual transmission. The cost is scholar's time and cognitive capacity; the purported beneficiary is a future messianic community. Present community bears indirect cost through supporting scholar infrastructure.
% ABSENT_VOICES: Scholars holding the performance-only reading (commandment is suspended, not fulfilled, without Temple) and the study-as-performance reading (study itself IS complete fulfillment, not archive) are structurally excluded from the framework that legitimates current practice. They would argue the archive rationale is a post-hoc justification for an obligation whose real present function is institutional self-perpetuation or intellectual satisfaction, not future preparation.
% DISAPPEARANCE_RATIONALE: From the archive reading's framing: if the study obligation vanished, knowledge would degrade within one generation, Temple restoration would face technical barriers, and messianic preparation would fail. From competing readings: if the study obligation vanished, scholars would redirect effort to present halachic problems, community funding would increase for current welfare, and the commandment's actual present function (institutional identity, textual learning as spiritual practice) would reorganize under different framing. The contest concerns whether the future beneficiary is real or whether present benefit is being attributed to a deferred, possibly illusory future.
% FOUNDING_PROBLEM: After Temple destruction (70 CE), the Jewish religious community faced a structural problem: how to maintain a commanded practice (sacrifice) that could not be performed, against textual forgetting and interpretive drift. The archive reading posits that study preserves the knowledge for eventual restoration under messianic conditions.
% FOUNDING_PROBLEM_CORROBORATION: Medieval and early-modern halakhic authorities attest the founding problem: texts repeatedly invoke preservation of knowledge against loss as a rationale for study. However, historians and scholars from outside the benefiting authority structures note that no Jewish community has attempted large-scale Temple reconstruction in 2000 years despite possession of this precise knowledge, and that the messianic restoration is presented as a future contingency of undefined probability. The Talmudic record shows the study obligation emerging gradually from practical need (determining permitted foods, advising on hypothetical cases) rather than explicitly as archive preservation. The founding problem's relationship to its stated purpose is under-corroborated by present-day behavioral evidence.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is moderate rather than low because the present utility of the study obligation is uncertain and deferred. A genuine coordination function exists (preserving technical knowledge), but the beneficiary is future and contingent, not present and guaranteed. The measurement series shows extractiveness rising from 0.35 (early period, when restoration seemed nearer and more plausible) to 0.58 (contemporary, when 2000 years of non-occurrence have made the benefit increasingly hypothetical). Theater ratio rises sharply (0.25 to 0.62) because over time, the archive function becomes disproportionate to the actual activity—the knowledge IS preserved (books exist), yet study continues with intensified performative weight. The study obligation persists partly as archive, partly as institutional identity-maintenance. Suppression requirement is moderate (0.41) because the constraint persists through institutional authority and identity fusion, not through coercive exclusion of alternatives—scholars could reject the study obligation and would not face violent suppression, but would lose social position and scholarly community. One shared time grid: every metric is authored at every time point (0, 500, 1000, 1500, 2000 years post-destruction).
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical authority and scholar seats: the archive-maintenance reading is the true account of the commandment's present function—it preserves knowledge for future restoration and maintains continuity with the pre-destruction tradition. From external analysis and competing reading holders: the reading is post-hoc justification for an obligation whose real present function is institutional self-perpetuation, textual learning as spiritual practice, or identity maintenance—the messianic benefit is formally stated but not empirically pursued. The engine should compute this divergence: rabbinical authority and internal scholars should classify the constraint as closer to rope (coordination with future benefit), while external analysts and competing reading holders should compute closer to tangled_rope or snare (deferred benefit justifying present extraction). The structural gap is between seated actors (who benefit from the present institutional configuration) and external observers (who question whether the deferred benefit is real or merely stated).
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are the structural targets (d near 1.0): they bear the primary cost of time and identity commitment; they are identity-locked to the study obligation such that exit means professional and social death. The future messianic community is structurally beneficiary (d near 0.0), but as a non-agent entity, its 'd' is analytical only. Rabbinical authority sits between beneficiary and agenda-setter: it frames and enforces the obligation (agenda-setter power) but benefits indirectly from institutional continuity rather than directly collecting extraction (beneficiary modulation). The present Jewish community is dual-positioned: they benefit from scholarly expertise applied to present halachic problems and from cultural continuity, but also bear indirect cost through supporting scholar infrastructure. The performance-only and study-as-performance reading holders are excluded rather than coordinated; their exclusion is structural because the archive-maintenance rationale forecloses (in the sense of logical inconsistency, not operational suppression) their competing claims: if study is archive-only, then performance-only and study-as-performance framings are false by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The archive-maintenance reading resolves mandatrophy through temporal deferral: the founding problem (how to maintain commanded practice without Temple) is declared 'live' (restoration is still possible, even if indefinitely projected), preventing mandatrophy classification. However, the behavioral evidence—no serious attempt at Temple reconstruction despite 2000 years of maintained knowledge—raises the question: is the founding problem actually live, or is the constraint performing mandatrophy (persisting as institutional ritual after its stated function has become obsolete)? The six_questions corroboration rule is key here: rabbinical authorities attest the founding problem is live; external historians and scholars dispute this, noting that the probability of restoration is not empirically pursued. The mandatrophy resolution turns on whether deferred messianic redemption counts as a live founding problem—the reading asserts yes, external analysis notes the assertion is unverified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_contingency_probability,
    'What is the implicit or explicit probability that Temple restoration will occur within a time horizon relevant to knowledge preservation? Does this probability affect whether the archive function is a genuine coordination function or a rationalization?',
    'Empirical measurement of how Jewish communities and authorities treat messianic restoration: do they invest resources in preparation proportional to probability? Do they update the archive obligation based on changing messianic expectations? Textual analysis of authority statements about messianic timing and its bearing on present practice.',
    'If probability is genuinely high (>50% within a civilization-scale horizon), the archive function is real coordination justified by deferred benefit. If probability is asserted but not empirically pursued (approaches 0% in revealed preference), the archive rationale is post-hoc justification and extractiveness should be reclassified upward (toward snare). If probability is genuinely uncertain (Knightian uncertainty rather than calculable risk), the classification should reflect ambiguity in the foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_contingency_probability, empirical, 'Messianic restoration probability and its bearing on archive justification').

omega_variable(
    identity_versus_obligation_source,
    'Is the study obligation maintained primarily because the reading community believes it is halakhically commanded, or primarily because it is constitutive of scholarly and communal identity?',
    'Structural variation: communities that maintain the obligation while questioning its halakhic basis (cultural Judaism, secular-learning frameworks) would show that identity maintenance is sufficient; communities that abandon the obligation while maintaining rabbinical authority (Conservative/Reform transitions) would show that halakhic belief is necessary. Survey of contemporary scholars about their reasons for continuing study.',
    'If identity is the primary driver, the constraint is more extractive than the halakhic-commandment framing suggests—the obligation persists because scholars need the role, not because the archive is necessary. The theater_ratio rise supports this. If halakhic belief is primary, the constraint is more rope-like (genuine obligation justified by law, even if the future-benefit framing is speculative). This affects whether the constraint should be classified as tangled_rope (as authored) or snare (if identity-maintenance is primary and halakhic justification is cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_versus_obligation_source, empirical, 'Whether study obligation is driven by halakhic command or identity maintenance').

omega_variable(
    archive_medium_adequacy,
    'If the archive function is genuine, is embodied scholarly practice (present-day study and transmission) necessary to the archive, or would written texts alone be sufficient? Is the constraint preserving knowledge or scholarly community?',
    'Comparative analysis: Jewish communities that maintain written archives without living study obligation (Karaite traditions, secular scholarly communities) show archive adequacy without practice obligation. Examination of whether textual traditions degrade without live scholarly transmission, or whether written texts are stable across generations of non-practice.',
    'If texts alone preserve knowledge adequately, the present study obligation is not necessary for archive function; it persists for other reasons (identity, institutional continuity, spiritual practice). The constraint would be reclassified from genuine-but-deferred-benefit coordination toward identity-locked extraction. If live transmission is necessary (knowledge decays without active scholarly engagement), the obligation has more real justification, and the constraint is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_medium_adequacy, empirical, 'Whether archive function requires live scholarly practice or is adequately served by written texts').

omega_variable(
    kernel_reading_frame_ambiguity,
    'Is the archive-maintenance reading internally coherent, or does it implicitly rest on unstated assumptions about what counts as fulfilling the sacrifice commandment?',
    'Textual genealogy of the archive rationale in Talmudic and medieval sources; examination of whether early sources explicitly frame study as archive vs. whether this framing is a retrospective coherence project. Comparison with how other obligations whose performance is impossible (e.g., some Temple-service commandments) are handled in halakhic literature.',
    'If archive-maintenance is a coherent ancient reading, the constraint reflects a stable interpretive tradition and the classification holds. If archive-maintenance is a modern retrospective reframing applied to an obligation that arose for different reasons (practical problem-solving, intellectual engagement), the reading is less well-grounded and the theatrical components suggest classification toward snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_ambiguity, conceptual, 'Genealogy and internal coherence of the archive-maintenance reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.45).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__archive_maintenance, theater_ratio, 1500, 0.62).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.62).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__archive_maintenance, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.35).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__archive_maintenance, suppression_requirement, 1500, 0.41).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.25).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel admits three structurally distinct readings: archive-maintenance, performance-only, and study-as-performance. Each reading instantiates a different constraint with different beneficiary structures, different temporal horizons, and different extractiveness profiles. Archive-maintenance posits deferred benefit (future restoration); performance-only posits obligation suspension (commandment in abeyance); study-as-performance posits intellectual fulfillment (study=commandment). The three stories are linked as a constraint family addressing the same kernel under different framings. Archive-maintenance influences both siblings by establishing the scholarly infrastructure and interpretive authority that frame what 'sacrifice' means; performance-only and study-as-performance coexist as competing readings held by different parties within Jewish tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
