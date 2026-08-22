% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Study-as-Commandment Fulfillment
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   kodashim_commandment_status. The kernel is the question: what is the
 *   halakhic status of commandments to perform Temple sacrifice after the
 *   Temple's destruction in 70 CE? Three readings compete: (1)
 *   study_as_performance (this constraint)—studying sacrifice laws fulfills
 *   the commandment in full; the tradition remains occupied through
 *   intellectual engagement; (2) messianic_deferral—the commandment persists
 *   in latency, suspended but not obsolete, and study maintains readiness for
 *   future restoration; (3) performance_only—sacrifice laws are contingent on
 *   Temple existence and the commandment is suspended without performance
 *   conditions. This JSON instantiates reading (1) as a clean, ε-invariant
 *   constraint. The constraint claims that study of sacrifice laws genuinely
 *   fulfills the commandment—not as substitute, not as preparation, but as
 *   the authentic performance modality in post-Temple reality. Zero
 *   extractiveness: no one is harmed by the absence of literal sacrifice, no
 *   one benefits from suppressing literal performance, no transfer of goods
 *   occurs. The tradition is bound together by intellectual coherence, not by
 *   extraction. Accessibility collapse is near-universal (0.92): once the
 *   reading's logic is understood—that the commandment's essence is legal
 *   obligation and study engages that obligation intellectually—alternatives
 *   collapse. A scholar who comprehends the argument converges. Suppression
 *   is minimal (0.15): the constraint holds through conviction, not coercion;
 *   the modest residual suppression reflects only the historical barriers (no
 *   Temple, no animal stock) and the intellectual pressure of competing
 *   readings.
 *
 * KEY AGENTS:
 *   - talmudic_scholar: engages in study, experiences fulfillment through intellectual engagement (no performance cost)
 *   - jewish_community: maintains continuity without Temple infrastructure; commanded to learn rather than sacrifice
 *   - messianic_restoration_advocates: excluded voices arguing for deferral reading; would maintain study as readiness, not fulfillment
 *   - literal_performance_advocates: excluded voices arguing commandment is suspended; would treat study as evasion
 *   - intellectual_tradition (non-agent): the doctrine itself, that study is generative and fulfilling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.15).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Study-as-Commandment Fulfillment").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment-system").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '5c5bbbf6-6744-4454-8b0f-db23f297c905').
narrative_ontology:cs_kernel_codification('5c5bbbf6-6744-4454-8b0f-db23f297c905', fixed_text).
narrative_ontology:cs_authority_grounding('5c5bbbf6-6744-4454-8b0f-db23f297c905', lineage).
narrative_ontology:cs_interpretation_layer_present('5c5bbbf6-6744-4454-8b0f-db23f297c905').
narrative_ontology:cs_reading_relation('5c5bbbf6-6744-4454-8b0f-db23f297c905', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('5c5bbbf6-6744-4454-8b0f-db23f297c905', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('5c5bbbf6-6744-4454-8b0f-db23f297c905', foundational, study_fulfills_commandment).
narrative_ontology:cs_axiom_status(study_fulfills_commandment, holdable).
narrative_ontology:cs_axiom_grounding('5c5bbbf6-6744-4454-8b0f-db23f297c905', study_fulfills_commandment, deontological).
narrative_ontology:cs_axiom('5c5bbbf6-6744-4454-8b0f-db23f297c905', foundational, commandment_essence_is_legal_obligation).
narrative_ontology:cs_axiom_status(commandment_essence_is_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5c5bbbf6-6744-4454-8b0f-db23f297c905', commandment_essence_is_legal_obligation, deontological).
narrative_ontology:cs_reference_frame('5c5bbbf6-6744-4454-8b0f-db23f297c905', commandment_occupation_through_intellectual_engagement).
narrative_ontology:cs_drift_state('5c5bbbf6-6744-4454-8b0f-db23f297c905', contemporary_diaspora_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c5bbbf6-6744-4454-8b0f-db23f297c905', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, intellectual_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, talmudic_scholar).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in sustained, systematic study of sacrifice laws and their legal logic. The reading declares their intellectual work constitutes fulfillment of the commandment—no material sacrifice required, no Temple present. They receive direct validation that their study is halakhically generative, not supplemental.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, talmudic_scholar, beneficiary,
    moderate, civilizational, mobile, universal).

% Inherits a framework that permits commandment continuity without Temple infrastructure. The constraint declares the tradition's intellectual machinery (yeshiva learning, halakhic analysis, textual interpretation) is itself the locus of observance. Averts the gap created by Temple absence.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, jewish_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Hold a competing reading that treats study as readiness, not fulfillment—maintaining the commandment in suspended state pending future Temple restoration. Are not part of the deliberative process generating this constraint, though the constraint's success marginalizes their reading's practical force.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_restoration_advocates, excluded,
    powerful, civilizational, trapped, universal).

% Argue that study is substitution, not genuine performance—that commandments are suspended without the material conditions of their full expression. Would contest the constraint's core claim that intellectual engagement can fully occupy the commandment. Historically marginalized in mainstream halakhic discourse.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, literal_performance_advocates, excluded,
    moderate, civilizational, constrained, global).

% The doctrine itself—the framework that treats textual and intellectual engagement as the fulfillment modality. Not an actor but a vindicated proposition: the reading vindicates that thought and analysis are structurally generative within halakhic reality, not merely preparatory.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, intellectual_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__study_as_performance, intellectual_tradition).

% Views the constraint from outside any committed framework: observes how this reading operates, what competing readings would change, and how the kernel's three-way contest structures legitimacy.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of halakhic commandment fulfillment across a historical rupture (Temple destruction). Solves the binding problem: how remains a Jewish community obligated to commandments when material conditions of performance are absent? The reading coordinates intellectual engagement as the valid fulfillment modality, holding the tradition together across diaspora.
% TRANSFER_FUNCTION: Transfers the locus of commandment performance from external (material sacrifice at Temple) to internal (intellectual engagement with legal texts). No goods or status move between parties; the transfer is modal—what counts as 'doing' the commandment shifts from performance to study.
% ABSENT_VOICES: Literal-performance advocates are structurally excluded from the authoritative deliberation that confirms this reading—they would argue the commandment is suspended, not transformed, and that study is at best preparation, not fulfillment. Messianic-restoration advocates would argue the commandment persists in latency, not full force, and that study maintains readiness rather than performance. Neither reading is invited into the constraint-generating discussion.
% DISAPPEARANCE_RATIONALE: If this reading vanished and literal-performance doctrine became hegemonic, the Jewish community would face the conclusion that commandments are suspended without Temple—creating profound halakhic breach and potential identity crisis. If messianic-deferral became hegemonic instead, study would shift from fulfillment to preparation, changing the psychological and spiritual force of learning but not immediately reorganizing community practice (both readings involve study). The verdict is contested because the sibling readings have different practical consequences and the constraint's disappearance would redistribute legitimacy among them, not merely halt activity.
% FOUNDING_PROBLEM: After Temple destruction in 70 CE, Jewish law faced existential discontinuity: the commandments of Kodashim (sacrifice laws) specify material performance at a now-destroyed location. What is the status of these commandments? Is the tradition severed, or can it continue? The binding problem: how remains a community obligated to laws when their performance conditions are absent?
% FOUNDING_PROBLEM_CORROBORATION: The problem is live and attested across all three sibling readings—they agree the problem exists; they disagree on the solution. Medieval halakhic authorities (Rambam, Maimonides, codifiers of Mishneh Torah) explicitly analyze this problem and author this reading: study fulfills the commandment. Contemporary halakhic scholars and yeshiva institutions universally implement this reading through structured learning of sacrifice laws. The problem is not treated as solved only in retrospect but as an ongoing generative tension—Talmudic discussions of Kodashim remain central curricula in Orthodox yeshivas specifically because study IS considered the fulfillment modality. The messianic-deferral reading is attested in kabbalistic and hasidic texts; literal-performance reading survives in certain fundamentalist and renewed-sacrifice movements but is marginal in mainstream tradition.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the constraint produces no asymmetric benefit: communities that endorse this reading (overwhelmingly in mainstream Orthodoxy) do so because they believe study genuinely fulfills the commandment, not because it benefits them at another's expense. Suppression (0.15) is minimal because the constraint is held by conviction. The modest residual suppression reflects structural barriers (Temple absence makes literal performance impossible, which removes an alternative—but this is physical, not coercive); weak intellectual suppression (literal-performance advocates are marginalized in discourse, but not silenced; their reading remains available to anyone who chooses it). Theater ratio (0.08) is very low: the constraint's operation is almost entirely functional (genuine study of sacrifice laws) with minimal performative element—the tradition treats this study as substantive halakhic work, not ceremonial or theatrical maintenance. Accessibility collapse (0.92) is high because the reading's logic is tight: if you grant that (a) commandments bind the Jewish community, (b) the Temple is gone but the Jewish community persists, and (c) the essence of sacrifice laws is legal obligation, then study necessarily follows as the fulfillment modality. Once these premises are accepted, alternatives collapse—the reading feels inevitable. Resistance (0.22) is low because the reading has been dominant in mainstream halakhic tradition for 1,900 years and is not actively contested by the communities most affected; competing readings (messianic and literal) exist but are marginal. Measurements flat across time: the constraint is stable because it is anchored in textual tradition and community conviction, not in negotiable circumstances. Neither extraction nor theater nor suppression requirement has grown—the constraint is not accumulating excess.
 *
 * PERSPECTIVAL GAP:
 *   The three readings diverge, and each would experience this constraint differently: (1) From the study_as_performance seat (this reading), the constraint is a natural law—the logical consequence of textual tradition and the only coherent solution to the binding problem. (2) From the messianic_deferral seat, this reading over-commits—it treats the commandment as fully satisfied when only readiness is warranted; its appeal is that deferral keeps the possibility of literal restoration open. (3) From the performance_only seat, this reading is evasion—study is preparation or memory, not performance; the commandment is genuinely suspended, and the constraint disguises that suspension. The engine would compute a seat-specific type for each reading based on structural data. The study_as_performance seat (this constraint) computes as mountain from every position because the reading has no victims and no asymmetric benefit. The messianic_deferral reading might compute differently (perhaps rope: coordinates preparation, involves no extraction). The performance_only reading would compute as snare if instantiated (victims: the displaced literal-performance advocates; beneficiary: the community that avoids performance costs; extraction: avoiding the cost of resuming sacrifice). These are different constraints, not the same constraint viewed from different angles.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality analysis applies: this is authored as a mountain (zero extraction, universal accessibility collapse, natural emergence from the halakhic structure itself). Were the engine to apply the directionality derivation chain, it would find: no victims (empty victim set by structural design), intellectual_tradition as beneficiary (non-agent, does not flow through the derivation), talmudic_scholar and jewish_community as beneficiaries (they benefit from the reading's validation of their intellectual practice). All beneficiaries would derive d toward 0.0. There are no targets. The constraint is asymmetric only along the axis of who has access to the reading's force: you must be literate in halakhic tradition to feel its necessity; outsiders cannot parse the logical entailment. But this is not extraction—it is domain-specific access, not suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, suspension of sacrifice performance) is live: all three readings acknowledge it and compete to solve it. The disappearance verdict is contested: if study-as-performance evaporated and performance-only gained hegemony, the Jewish community would face the mandate that commandments are suspended—creating legal and spiritual breach. If messianic-deferral gained hegemony instead, study would shift from fulfillment to readiness, changing the psychological force but not community practice. The constraint resolves the mandatrophy tension by maintaining that the commandment is NOT suspended: it is transformed (from external to internal performance), but it remains active and binding. Study keeps the commandment occupied. This is the core claim and it is neither a false natural law nor an evaded mandate—it is a genuine reframing of what counts as performance. The constraint succeeds because the halakhic tradition has endorsed it and communities live within it without experiencing it as covering a gap or hiding a cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_natural_law_distinction,
    'Is the constraint that ''study fulfills the commandment'' a natural law (inherent in the structure of halakhic reality), or a constructed reading (one coherent interpretation among others)?',
    'Interrogate whether the three sibling readings (study_as_performance, messianic_deferral, performance_only) all claim access to the same halakhic reality and compete over interpretation, or whether each constructs a different reality. If they share a reality and disagree about its interpretation, the constraint is a reading; if they construct incommensurable realities, the distinction dissolves.',
    'If this constraint is truly a natural law, it emerges from the structure of halakhic obligation itself and persists independent of whether any community endorses it. If it is a constructed reading, it persists as long as committed communities maintain it and its legitimacy can be contested. The natural-law reading would justify the claimed_type: mountain with zero extraction and near-universal accessibility collapse (all who understand halakhic logic converge). The constructed-reading classification would suggest this is one live position in a three-way contest where legitimacy is unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_law_distinction, conceptual, 'The kernel is a contested doctrine; this reading is one coherent solution. Structurally, a ''natural law'' claim here means ''logically entailed by the halakhic system itself''; a constructed reading means ''one defensible interpretation chosen by communities.'' The constraint as authored (zero extractiveness, zero victims, universal accessibility collapse) commits to the natural-law reading.').

omega_variable(
    performance_modal_vs_literal_performance,
    'Does intellectual engagement with sacrifice laws genuinely fulfill the commandment, or is it a substitution that leaves the commandment technically suspended?',
    'This is a theological and halakhic question without empirical resolution. The resolution mechanism is intra-textual: examining whether authoritative halakhic sources treat study as full fulfillment (with commandment-satisfaction status) or as preparation/readiness (with suspended status and deferred obligation). Maimonides codifies study-as-fulfillment explicitly; later literalists contest it. The textual tradition is the evidence base.',
    'If study is full fulfillment, this constraint is correct as authored: zero extractiveness, no victims, commandment is occupied. If study is substitution, the constraint should shift to snare (study community benefits from avoiding literal performance costs; Temple-restoration advocates are victims of a delegitimizing reading; extraction is 0.6+). The core claim would become vulnerable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_modal_vs_literal_performance, conceptual, 'Whether modal transformation (performance → study) is genuine fulfillment or clever evasion. The kernel itself is contested on this axis.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The modest suppression score (0.15) reflects that study is not coercively enforced; communities adopt it by conviction. But is the absence of literal-performance from the tradition due to structural barriers (no Temple, no animal herds) or internalized acceptance (communities believe study is genuine fulfillment and do not want to perform literally)?',
    'Observe responses in hypothetical scenarios: if Temple were rebuilt and animal sacrifice became logistically possible, would communities feel suppressed by study-as-fulfillment and resume literal performance? Or would they maintain study as the preferred modality? If they feel suppressed, the internalization is partial; if they maintain study by choice, the suppression is structural (barriers prevent performance; conviction sustains the reading).',
    'If suppression is structural, the constraint is more robust—it would survive even if literal performance became possible, because the reading genuinely converts believers. If suppression is internalized, the constraint is fragile—it relies on continued conviction that study is fulfillment, and that conviction could shift if material conditions change or competing readings gain authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Distinguishing structural barriers (Temple absence, logistical impossibility) from internalized acceptance (genuine belief in the reading).').

omega_variable(
    kernel_contest_structure,
    'Are the three sibling readings (study_as_performance, messianic_deferral, performance_only) all live within a single halakhic framework, or do they represent incompatible frames that cannot coexist?',
    'Examine whether authoritative halakhic sources acknowledge all three readings as coherent positions, or whether they treat one as correct and the others as errors. If all three are acknowledged as live positions held by different communities or in different contexts, they coexist. If the dominant tradition delegitimizes two of them as false, one reading foreclosed the others.',
    'If coexistence: the constraint is one reading among three live options; its persistence depends on community choice and halakhic authority maintaining it as legitimate. If foreclosure: this reading has eliminated logical competitors and occupies the sole defensible position, making it closer to a natural law. The three-way contest is documented in traditional sources; mainstream halakhic consensus treats study_as_performance as dominant but not as the sole option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_structure, conceptual, 'Whether the kernel is a three-way contest or a settled question with one authoritative reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__study_as_performance, theater_ratio, 500, 0.06).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__study_as_performance, theater_ratio, 2000, 0.08).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__study_as_performance, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__study_as_performance, suppression_requirement, 500, 0.12).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__study_as_performance, suppression_requirement, 1000, 0.14).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__study_as_performance, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__study_as_performance, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.02).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% The kernel kodashim_commandment_status contains three structurally distinct constraints, one for each reading. study_as_performance (this file) claims that study fulfills the commandment; messianic_deferral claims study maintains readiness; performance_only claims the commandment is suspended. These are not the same constraint viewed from different perspectives—they have different ε values, different victim/beneficiary structures, and different type classifications. They share a kernel (the textual tradition) but instantiate different constraints from it. Each file carries its own reading_id and is linked via network.affects_constraints to mark the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
