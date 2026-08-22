% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Medieval-Classical Latin Continuity (Hybrid Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of the correct-Latin kernel maintains that Medieval
 *   Latin exhibits genuine morphological continuity with Classical Latin
 *   (legitimating medieval forms in inflectional systems, declensions, and
 *   core derivational morphology), but that syntax and lexicon underwent
 *   sufficient change to warrant recovery-oriented reconstruction toward
 *   Classical norms. This reading emerged from Renaissance philology as a
 *   middle ground between pure continuity (Medieval = natural evolution of
 *   Classical) and pure discontinuity (Medieval = distinct system). The
 *   hybrid framework splits the constraint: it grants medieval morphology
 *   structural legitimacy while treating medieval syntax and lexicon as
 *   corruption sites. This partition has become the dominant editorial
 *   consensus, enforced through academic gatekeeping, editorial conventions,
 *   and the apparatus criticus. The claim/metric gap is deliberate: the
 *   reading is CLAIMED as a coordination framework (enabling coherent
 *   editorial practice) while the metrics describe substantially extractive,
 *   actively-enforced operation that privileges classical recovery and
 *   subordinates medieval specialists.
 *
 * KEY AGENTS:
 *   - Philological consensus gatekeepers: institutional actors who control editorial standards and determine what counts as legitimate medieval form versus corruption
 *   - Medieval specialists: scholars studying medieval texts who pay the cost of having their materials subordinated to classical recovery
 *   - Textual anomaly investigators: those using computational or comparative methods who encounter systematic medieval patterns and bear the cost of treating them as error
 *   - Classical recovery tradition: the interpretive tradition vindicated by treating medieval forms as noise in the signal of Classical texts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Medieval-Classical Latin Continuity (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, 'c7df4a08-d1dc-40e3-b5cc-f843da46f156').
narrative_ontology:cs_kernel_codification('c7df4a08-d1dc-40e3-b5cc-f843da46f156', fixed_text).
narrative_ontology:cs_authority_grounding('c7df4a08-d1dc-40e3-b5cc-f843da46f156', extraction).
narrative_ontology:cs_interpretation_layer_present('c7df4a08-d1dc-40e3-b5cc-f843da46f156').
narrative_ontology:cs_reading_relation('c7df4a08-d1dc-40e3-b5cc-f843da46f156', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7df4a08-d1dc-40e3-b5cc-f843da46f156', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('c7df4a08-d1dc-40e3-b5cc-f843da46f156', foundational, morphological_continuity_binding).
narrative_ontology:cs_axiom_status(morphological_continuity_binding, holdable).
narrative_ontology:cs_axiom_grounding('c7df4a08-d1dc-40e3-b5cc-f843da46f156', morphological_continuity_binding, empirically_contingent).
narrative_ontology:cs_axiom('c7df4a08-d1dc-40e3-b5cc-f843da46f156', foundational, syntactic_lexical_recovery_legitimacy).
narrative_ontology:cs_axiom_status(syntactic_lexical_recovery_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c7df4a08-d1dc-40e3-b5cc-f843da46f156', syntactic_lexical_recovery_legitimacy, conventional).
narrative_ontology:cs_reference_frame('c7df4a08-d1dc-40e3-b5cc-f843da46f156', morphologically_continuous_recovery_target).
narrative_ontology:cs_drift_state('c7df4a08-d1dc-40e3-b5cc-f843da46f156', computational_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c7df4a08-d1dc-40e3-b5cc-f843da46f156', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, philological_consensus_gatekeepers).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, textual_anomaly_investigators).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latin_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, textual_reconstruction_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University departments, editorial boards, and learned societies that establish which reconstructions are accepted. They administer the hybrid reading by identifying morphological continuity as structurally legitimate while treating syntax and lexicon as corruption sites requiring recovery from Classical texts or reconstruction from internal evidence. They benefit from the constraint's maintenance because it preserves their authority to adjudicate what counts as 'correct' Latin and which texts require emendation versus acceptance.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, philological_consensus_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars studying medieval texts in their own terms. They bear the cost of the constraint because their textual materials are routinely treated as corrupt aberrations rather than evidence of a legitimate linguistic system. Their research requires constant reframing of medieval forms as degradation rather than development, and their discoveries about medieval syntax and lexicon are subordinated to the recovery-of-classical-forms narrative. Exit would mean abandoning the scholarly apparatus and institutional recognition.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latin_specialists, payer,
    moderate, biographical, constrained, global).

% Scholars (particularly those using computational methods, genetic criticism, or comparative Romance linguistics) who encounter medieval Latin forms that deviate from Classical patterns but exhibit systematic internal coherence. The constraint forces them to treat these patterns as errors requiring emendation rather than as evidence of linguistic change. They can exit by adopting discontinuity or other frameworks, but doing so costs them access to conventional editorial networks and citation authority.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_anomaly_investigators, payer,
    powerful, biographical, mobile, global).

% The interpretive tradition grounded in the premise that medieval texts are primarily vehicles for recovering lost or corrupted Classical texts. This tradition is vindicated by the constraint's operation: it provides intellectual cover for treating medieval variants as noise rather than signal, which confirms the tradition's foundational assumption that Classical Latin is the true object and medieval forms are secondary.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_recovery_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__hybrid_reading, classical_recovery_tradition).

% Scholars advocating for the discontinuity reading (Medieval Latin as a distinct system) are systematically under-resourced and face institutional friction. They are excluded from editorial consensus and their frameworks are treated as eccentric rather than viable. Their presence would reframe the entire constraint by challenging the hybrid reading's partitioning logic.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, discontinuity_reading_proponents, excluded,
    moderate, biographical, constrained, global).

% Scholars who argue for smooth linguistic evolution from Classical to Medieval Latin (continuity reading) are also marginalized, though differently: the hybrid reading absorbs part of their claim (morphological continuity) while rejecting the inference that medieval syntax and lexicon are equally evolutionary. This partial absorption makes the full continuity position harder to defend without appearing redundant.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, continuity_reading_proponents, excluded,
    moderate, biographical, constrained, global).

% The editorial and critical apparatus (apparatus criticus, emendation conventions, diplomatic editions versus critical editions) that operationalizes the hybrid reading. It is simultaneously the vehicle through which the reading is enforced and a beneficiary of its persistence, because editing medieval Latin requires constant judgment calls about what is corruption and what is legitimate variation—judgment calls the hybrid framework authorizes.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_reconstruction_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, textual_reconstruction_apparatus, beneficiary).

% The historical agents (medieval authors, scribes, copyists) whose actual language use produced the texts. They are excluded from the conversation about what their language meant; their forms are evaluated against a Classical standard they never intended to meet, and their intentions are systematically subordinated to the recovery of Classical forms.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_authors_and_scribes, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, philological_consensus_gatekeepers).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for adjudicating which medieval Latin forms are evidence of legitimate linguistic change (morphology) and which are copyist error or unauthorized innovation (syntax, lexicon). This enables editors, scholars, and institutions to work with a consistent standard for reconstruction and emendation, making medieval texts analyzable within a unified interpretive apparatus.
% TRANSFER_FUNCTION: Transfers intellectual authority from medieval specialists and anomaly investigators to classical recovery gatekeepers and the broader consensus tradition. The constraint moves interpretive power away from those reading medieval texts as autonomous systems toward those reading them as degraded Classical texts requiring recovery. It also moves from empirical discovery (what medieval syntax actually exhibits) toward canonical authority (what Classical texts permit).
% ABSENT_VOICES: Medieval authors and scribes have no voice—their actual language choices are evaluated posthumously against a standard they never claimed to follow. Discontinuity-reading proponents are excluded from editorial consensus. Scholars whose computational methods reveal systematic patterns in medieval syntax are treated as anomaly-hunters rather than theorists. The voices of Romance linguists (who see medieval Latin through the lens of Romance language development) are marginalized in favor of classical philologists.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, editorial practice would fragment: some editors would treat medieval texts as monuments to be read in their own terms (discontinuity direction); others would intensify classical recovery efforts with different reconstructive methods (continuity direction). The constraint holds editorial consensus together; without it, medieval scholarship would reorganize around competing frameworks, and the institutional authority of classical recovery gatekeepers would weaken. Some scholars argue nothing substantive would change because the empirical facts of medieval texts would reassert themselves; others argue the disappearance would require a wholesale reconceptualization of what medieval Latin texts are evidence of.
% FOUNDING_PROBLEM: From the 15th century onward (Renaissance recovery of Classical texts), scholars faced a problem: medieval manuscripts contained forms that deviated from Classical Latin in ways that seemed systematic but incoherent by Classical standards. Were these degradations (and thus targets for emendation), or were they evidence of linguistic change worth understanding in their own right? The hybrid reading emerged as a compromise: accept that morphological systems persisted (continuity within constraints) but treat syntactic and lexical innovations as corruption requiring recovery toward the Classical ideal.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists and editorial consensus gatekeepers attest the founding problem persists: modern editors still encounter medieval anomalies that require adjudication. Medieval specialists and computational linguists attest the founding problem has been reframed: what was a problem of understanding medieval syntax as corruption is now understood as a problem of understanding medieval language as a system—a shift that undermines the founding problem's original stakes. The discontinuity reading proponents argue the founding problem itself arose from a false premise (that Classical Latin is the measure of correctness) and has been superseded by empirical linguistic analysis.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, contested).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end because the constraint concentrates authority to classify (what is continuous, what is corrupt) in the hands of classical philologists, away from medieval specialists. The classification power itself is the extraction: medieval scholars must constantly defend their materials as legitimate rather than having them accepted as autonomous evidence. Suppression measures 0.62 because the constraint's enforcement requires active work: editors must examine every medieval deviation, apply the hybrid partition (morphology passes, syntax/lexicon gets scrutinized), and justify emendations or acceptances. Theater rises from 0.25 to 0.41 because an increasing share of editorial effort goes toward justifying the partition (when does a syntactic form cross into illegitimacy?) rather than simply recording what medieval texts contain. The time series reflects historical drift: early Renaissance hybrid readings were more permissive of medieval forms; modern computational analysis has tightened the partition, raising extractiveness and theater as more medieval innovations are reclassified as error requiring recovery. Measurements plateau after t=15 because the framework has stabilized at its current operational intensity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (consensus gatekeepers) seat, the constraint is coordination: a coherent framework for editorial decision-making that enables collaborative practice. From the payer seats (medieval specialists), it is asymmetric extraction: the framework is the mechanism through which their expertise is devalued and their materials are subordinated. The engine should compute these as substantially different types from their respective positions. The computational split occurs because power (institutional vs. moderate), exit options (arbitrage vs. constrained), and beneficiary/victim declarations all differ. A medieval specialist with institutional power or arbitrage exit would compute differently; the constraint's asymmetry depends on the interaction of the framework (the partition rule) with the structural positions of those subject to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Philological consensus gatekeepers experience this constraint as beneficiary (d near 0.1): they control the classification apparatus and benefit from its maintenance through preserved authority. Medieval specialists and anomaly investigators experience it as target (d near 0.85): they must constantly defend their materials, justify medieval forms, and defer to classical recovery authority. The partition itself creates asymmetry: medieval morphology (accepted) benefits the consensus narrative; medieval syntax/lexicon (scrutinized) feeds the extraction. A medieval specialist studying medieval syntax experiences high directionality toward the constraint (trapped by needing to frame their work within recovery logic) while a classical philologist using the same texts experiences low directionality (the constraint creates opportunity, not burden).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to adjudicate medieval deviations from Classical norms) was live when the hybrid reading emerged in the Renaissance—editors genuinely faced texts that required judgment. By the 20th century, that founding problem had shifted: the problem was no longer how to decide whether medieval syntax was error or evidence, but whether the entire framework of treating medieval as corrupted Classical was the right object. Computational linguistics and Romance historical linguistics have substantially reframed the question. The hybrid reading persists not because it solves the founding problem but because it provides institutional cover for classical recovery gatekeepers and anchors editorial consensus. The theater_ratio rise from 0.25 to 0.41 indicates increasing performative maintenance: more editorial apparatus is devoted to justifying the partition rather than discovering new medieval forms. This signals mandatrophy—the original coordination function (adjudicating ambiguous cases) has atrophied, replaced by maintenance of the partition itself. The constraint persists through inertia and institutional investment rather than because the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_syntax_partition_stability,
    'Is the boundary between legitimate morphological continuity and corrupted syntax/lexicon stable, or does it shift with scholarly attention and computational evidence?',
    'Longitudinal analysis of editorial decisions: track which medieval forms have migrated from ''corruption'' to ''accepted'' categories as scholarly understanding deepens, and whether this migration follows morphology/syntax lines or reveals the partition to be historically contingent.',
    'If the partition is stable and principled, it validates the hybrid reading''s core logic. If it is shifting and contingent, the extraction mechanism is the partition itself rather than adjudication of a pre-existing structural fact—making the constraint more extractive than authored metrics suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(morphology_syntax_partition_stability, empirical, 'Whether the morphology-syntax distinction maps to a real linguistic boundary or is an artifact of editorial convention.').

omega_variable(
    continuity_versus_discontinuity_foreclosure,
    'Does the hybrid reading logically foreclose the full continuity reading, or do they coexist as live competing frameworks?',
    'Examine whether scholars can consistently hold the continuity reading (Medieval = natural evolution, all forms legitimate) without logical contradiction to the hybrid reading''s partition. The test: a continuity proponent can defend medieval syntax as evolutionary innovation; a hybrid proponent must then either accept it as evolutionary (reverting to continuity) or deny it is syntax (the hybrid partition reasserts itself). If the partition can always reassert, foreclosure may be pragmatic rather than logical.',
    'If the hybrid reading forecloses continuity logically, the reading_relations should specify ''forecloses''. If both remain live (one can hold continuity by rejecting the partition), they coexist, and the relation is ''coexists_with''. This affects how the engine treats the constraint''s legitimacy in the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_versus_discontinuity_foreclosure, conceptual, 'The logical relationship between the hybrid partition and the continuity alternative.').

omega_variable(
    reconstruction_versus_discovery_asymmetry,
    'Is ''reconstruction'' of Classical norms structurally different from ''discovery'' of medieval linguistic systems, or are both valid modes of textual inquiry that the constraint artificially privileges?',
    'Examine reconstructive practice: do editors reconstruct the same way across all medieval deviations (applying a consistent rule), or do they reconstruct selectively (stronger effort for syntactic forms that would yield Classical-like results)? Selective reconstruction would indicate the asymmetry is enforced rather than following from the texts.',
    'If reconstruction is selective and asymmetric, the constraint extracts authority by privileging one mode of inquiry over another. This would increase effective extraction and might reclassify the constraint from tangled_rope (real coordination + asymmetric extraction) toward snare (asymmetric extraction with coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_versus_discovery_asymmetry, empirical, 'Whether the privileging of reconstruction over discovery is a logical consequence of the hybrid partition or an additional extraction mechanism.').

omega_variable(
    medieval_author_intentionality,
    'Should medieval authors'' actual language choices (as distinct from scribal corruption and manuscript variation) be treated as evidence of a legitimate medieval system, or as deviations from Classical norms requiring correction?',
    'Historical and philological analysis: identify medieval authors'' deliberate linguistic innovations (documented through composition processes, scribal notes, deliberate archaisms or neologisms). Examine whether the hybrid framework treats author-deliberate forms differently from scribal error. If author deliberation does not change the classification (still treated as corruption), the framework is insensitive to intentionality—suggesting extraction rather than coordination.',
    'If the hybrid framework ignores author intentionality, it loses a key distinction between genuine linguistic change and error. This would support reclassification toward snare (the coordination function is illusory; the extraction is authority denial regardless of evidence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medieval_author_intentionality, empirical, 'Whether the constraint''s operation respects or ignores medieval authorial agency and intentionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__hybrid_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__hybrid_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__hybrid_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__hybrid_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__hybrid_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__hybrid_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__hybrid_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__hybrid_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__hybrid_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__hybrid_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__hybrid_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__hybrid_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__hybrid_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__hybrid_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel constraint family decomposes into three structurally distinct readings with different ε values and stakeholder sets. This hybrid_reading (0.58 base extractiveness) differs from continuity_reading (lower extraction, more permissive of medieval forms) and discontinuity_reading (higher extraction, treats medieval as fully autonomous system requiring its own recovery). All three are readings of the same kernel (the relationship between Medieval and Classical Latin) but instantiate different constraints with different beneficiary/victim structures. The hybrid reading's partial absorption of continuity creates institutional pressure on the full continuity position; the discontinuity reading remains marginal. See network edges for downstream constraints (editorial standards, manuscript evaluation, philological training).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
