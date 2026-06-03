% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Performance-Only Husk: Legitimacy Extracted from Unrealizable Future
 *   domain: religious_studies/rabbinic_judaism/commitment_systems
 *
 * SUMMARY:
 *   The Kodashim corpus (Talmudic tractates concerning Temple sacrifices)
 *   exists in a peculiar legal and spiritual state: the laws are binding, yet
 *   their material preconditions (Temple, altar, priestly service) have been
 *   absent for nearly 2,000 years. The performance-only reading interprets
 *   this constraint as follows: the Kodashim archive is a blueprint frozen in
 *   time, its kernel occupied by the future messianic state when sacrifice
 *   resumes, but currently functioning as a performance-only husk —
 *   practitioners study the laws in preparation for restoration, extracting
 *   religious legitimacy from a future state that cannot be realized in
 *   present time. This reading treats the constraint as a snare:
 *   practitioners devote cognitive resources and ritual attention to a corpus
 *   presented as an occupied kernel, while the actual kernel remains sealed.
 *   The extraction operates through the narrative of messianic preparation —
 *   legitimacy is promised for a performance (study, mastery, devotional
 *   engagement) that delivers present gratification only through deferred
 *   fulfillment. The sibling readings (study_as_exercise,
 *   substitution_archive) offer alternative framings that dissolve or
 *   redefine the extraction mechanism. This constraint story instantiates
 *   only the performance-only reading, with omitted siblings and their
 *   structural relationships expressed through omega variables and
 *   cs_structure fields.
 *
 * KEY AGENTS:
 *   - Practitioners Treating Archive as Living Practice (powerless/identity_locked): Primary victims — treat the Kodashim corpus as an occupied kernel awaiting restoration; identity is fused with preparation narrative; devote cognitive/devotional resources to study of laws they know cannot currently be performed
 *   - Messianic-Preparation Institutions (institutional/arbitrage): Primary beneficiaries — maintain interpretive authority over the corpus, control curriculum, derive legitimacy and resource flows from the narrative that study is performance-equivalent to actual sacrifice; benefit from channeling devotional energy
 *   - Historical-Conscious Practitioners (moderate/constrained): Secondary victims — aware that sacrifice is historically precluded and restoration indefinitely deferred, but constrained by career structures, community belonging, and study investment; experience the extraction without the identity lock that sustains less aware practitioners
 *   - Institutional Archive Maintainers (institutional/arbitrage): Perpetuate the constraint through inertia; maintain performative study infrastructure despite awareness that the kernel cannot be occupied; see their own activity as degraded (piton perspective)
 *   - Reform/Renewal Movements (organized/constrained): Organized challengers attempting to reframe the corpus as memorial archive or intellectual exercise rather than performance-only husk; experience mixed coordination and extraction
 *   - Analytical Observer: Risks naturalizing the constraint as a theological necessity (false summit) rather than recognizing it as a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.68).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.72).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Performance-Only Husk: Legitimacy Extracted from Unrealizable Future").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism/commitment_systems").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, 'd30de05b-2248-420b-a7e7-127b22f4780d').
narrative_ontology:cs_kernel_codification('d30de05b-2248-420b-a7e7-127b22f4780d', fixed_text).
narrative_ontology:cs_authority_grounding('d30de05b-2248-420b-a7e7-127b22f4780d', extraction).
narrative_ontology:cs_interpretation_layer_present('d30de05b-2248-420b-a7e7-127b22f4780d').
narrative_ontology:cs_reading_relation('d30de05b-2248-420b-a7e7-127b22f4780d', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('d30de05b-2248-420b-a7e7-127b22f4780d', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('d30de05b-2248-420b-a7e7-127b22f4780d', foundational, messianic_restoration_expected).
narrative_ontology:cs_axiom_status(messianic_restoration_expected, holdable).
narrative_ontology:cs_axiom_grounding('d30de05b-2248-420b-a7e7-127b22f4780d', messianic_restoration_expected, theological).
narrative_ontology:cs_axiom('d30de05b-2248-420b-a7e7-127b22f4780d', foundational, kernel_occupied_by_future_state).
narrative_ontology:cs_axiom_status(kernel_occupied_by_future_state, holdable).
narrative_ontology:cs_axiom_grounding('d30de05b-2248-420b-a7e7-127b22f4780d', kernel_occupied_by_future_state, theological).
narrative_ontology:cs_reference_frame('d30de05b-2248-420b-a7e7-127b22f4780d', messianic_preparation_framework).
narrative_ontology:cs_drift_state('d30de05b-2248-420b-a7e7-127b22f4780d', contemporary_secular_historical_consciousness, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d30de05b-2248-420b-a7e7-127b22f4780d', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, practitioners_treating_archive_as_living_practice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOTED PRACTITIONER (SNARE) — Structurally mobile (could leave Kodashim study) but identity-fused with the practice. The practitioner's religious identity is constituted through mastery of sacrifice law and the narrative that this study maintains the messianic possibility. Exit would require abandoning not just a practice but the identity constructed within it. Experiences maximum extraction: devotes time and cognitive resources to a corpus presented as occupied kernel, receiving in return only deferred fulfillment. The suppression is internalized — the practitioner has internalized the narrative that current study is a legitimate form of engagement with the kernel, preventing perception of the extraction.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MESSIANIC-PREPARATION INSTITUTION (ROPE) — Benefits from the narrative that Kodashim study is performance-equivalent to actual sacrifice. The institution maintains interpretive authority over the corpus, controls curriculum, and derives institutional legitimacy and resource flows from the claim that the archive represents an occupied (though currently inert) kernel awaiting restoration. Experiences the constraint as pure coordination: channeling devotional energy toward study rather than failed attempts at literal sacrifice. This is the beneficiary perspective — extraction flows toward this agent.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HISTORICAL-CONSCIOUS PRACTITIONER (SNARE) — Aware at the generational level that sacrifice is historically precluded (no Temple, no altar) and that the messianic restoration is indefinitely deferred. Yet remains constrained by career structures, community belonging, and intellectual investment in the Kodashim corpus. Cannot easily exit without losing professional status, community position, and decades of study investment. Experiences the constraint as snare: legitimate extraction justified by the narrative of preparation for an unrealizable future state.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL ARCHIVE MAINTAINER (PITON) — Maintains Kodashim curriculum and interpretive traditions through institutional inertia despite the deep awareness that the kernel cannot be occupied. Theater ratio is extremely high (0.88) — the performative ritual of study, commentary, debate, and transmission persists as a social/institutional activity that no longer serves any functional purpose related to actual sacrifice preparation. The institution persists because it has become structurally embedded (educational infrastructure, career pathways, status hierarchies). This perspective sees the constraint as degraded — maintained through institutional weight, not because the kernel is genuinely occupied.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM/RENEWAL MOVEMENT (TANGLED ROPE) — Organized actors seeking to reframe Kodashim as a memorial archive or as intellectual exercise rather than as a performance-only husk. This perspective coexists with the performance-only reading — the constraint contains both a genuine coordination function (organizing study communities, transmitting textual knowledge) AND asymmetric extraction (legitimacy taken from messianic narratives that the movement rejects). The movement experiences the constraint as mixed: it offers communal belonging and intellectual rigor, but also extracts devotion from those who accept its reframing, now toward a different (ethical, existential, historical) understanding rather than toward messianic preparation.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, the performance-only reading could appear as a natural theological law: any religious law whose material basis is precluded becomes necessarily a performance of memory and identity, not active practice. The constraint appears as an immutable feature of how religious systems absorb historical loss. However, this mountain classification is a false summit — the structural data reveals that the constraint's persistence depends on specific institutional choices and legitimacy claims, not on theological necessity. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(kodashim_corpus__performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_corpus__performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_corpus__performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_corpus__performance_only, TR),
    TR >= 0.70.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The performance-only reading presents study as a legitimate performance of preparation for a messianic restoration that cannot be rationalized at any foreseeable timescale. Practitioners allocate time and cognitive resources based on a narrative of future occupancy that provides no present material or spiritual benefit beyond the identity-constituting practice itself. The extraction is legitimate only if restoration is genuinely expected; if it is indefinitely deferred (as contemporary scholars largely hold), the extraction is nearly pure — legitimacy is taken in the present for a future that will not arrive. The measurement trajectory shows increasing extractiveness over 1,000 years as the temporal distance between law and restoration increases without diminishing the performance obligation. Suppression (0.72): High. The constraint is maintained through multiple suppressive mechanisms: (1) Identity lock — practitioners' religious identity is fused with preparation narratives, making exit psychologically equivalent to apostasy. (2) Institutional control — messianic-preparation institutions control the interpretive apparatus and educational pathways, suppressing alternative readings (substitution_archive) and promoting performance-only narrative as normative. (3) Epistemological closure — the theological framework treats messianic restoration as unpredictable and inevitable, making critique of preparation rationales appear as lack of faith. (4) Compartmentalization — practitioners maintain simultaneous belief in occupation (for performance purposes) and knowledge of precondition absence (historical reality). Theater ratio (0.88): Very high. The performance of Kodashim study is almost entirely theatrical — the activity has no functional output beyond the activity itself. Unlike study of civil law (which informs decision-making) or halacha applicable to present life, Kodashim study produces no actionable guidance, no legal consequences, no material outputs. The theater has increased over time as awareness of the constraint's unrealizability has accumulated while the obligation to study has remained constant. The performance persists because it has been embedded in institutional infrastructure (yeshiva curricula, professional study paths, status hierarchies, cultural identity markers) rather than because practitioners believe in its functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The performance-only reading generates maximum perspectival divergence. The devoted practitioner (powerless/identity_locked) experiences the constraint as an occupiable kernel requiring present preparation — they see their study as meaningful engagement with a deferred obligation, not as extraction. The messianic-preparation institution (institutional/arbitrage) experiences pure coordination — organizing study communities, transmitting textual knowledge, maintaining interpretive authority. The historical-conscious practitioner (moderate/constrained) experiences snare — they understand the indefinite deferral but cannot exit without losing position and identity. The archive maintainer (institutional/arbitrage) experiences piton — they perform the ritual of study through institutional inertia, aware that the kernel cannot be occupied. The reform movement (organized/constrained) experiences tangled rope — they reject the performance-only framing but recognize that the corpus contains genuine coordination value (community, textual mastery, intellectual tradition). The analytical observer risks mountain (naturalization) — treating the constraint as a necessary feature of religious systems that absorb historical loss, rather than recognizing it as a contingent institutional choice to maintain rather than substitute. The mandatrophy is resolved through the committer frame: all six perspectives are valid readings of the same structural data, each from a different institutional position and commitment framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural relationship to the extraction flow. Practitioners treating the archive as living practice are victims of the constraint — they are the target from which legitimacy is extracted. Their d-value is high (close to 1.0) because they bear the cost of the deferred obligation while receiving legitimacy only from future restoration that will not arrive. Messianic-preparation institutions are beneficiaries — extraction flows toward them. Their d-value is low (close to 0.0) because they benefit from organizing devotional energy and maintaining interpretive authority. The historical-conscious practitioner has a d-value between these extremes — they are both victim (constrained by the obligation) and partially beneficiary (community belonging, intellectual status). The directionality derivation chain starts with these structural relationships: who benefits (institutions), who bears costs (practitioners), and what exits are available (identity_locked for devoted, constrained for aware, arbitrage for institutions). From these input factors, the engine derives d values that feed into the effective extractiveness formula chi = ε × f(d) × σ(S), producing different experienced extractiveness values for different perspectives despite the same base extractiveness (ε).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the mismatch between the constraint's apparent coordination and its extractive structure) is resolved through the committer frame. From the performance-only reading's institutional perspective, the constraint appears to be pure coordination: organizing study of sacred law, transmitting textual knowledge, maintaining the tradition. From the practitioners' perspective, the constraint is snare: legitimate extraction justified by deferred fulfillment. The mandatrophy is not dissolved but rather exposed as a feature of how the constraint works — the institutional beneficiary genuinely experiences coordination, while the practitioner victim genuinely experiences extraction. The constraint's mandatrophy is not 'which perspective is correct' but rather 'which reading of the kernel is adopted,' and the reading determines what counts as legitimate extraction vs. illegitimate burden. The performance-only reading sustains extractive narrative by maintaining the husk as a kernel awaiting restoration — if the substitution reading were adopted, the constraint would reclassify as memorial archive (lower extractiveness, different burden structure). The mandatrophy is resolved by recognizing that the constraint's logical coherence depends on the kernel-reading commitment, and different readings generate different legitimacy structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_presented_messianic_timeline,
    'Is the messianic restoration genuinely expected to occur (within centuries/generations) such that preparation is rational, or is it indefinitely deferred (never, or at timescales that make preparation incoherent)?',
    'Analysis of authoritative rabbinic sources: when do they expect restoration? Has the timeline shifted over centuries? Do contemporary practitioners really expect sacrifice to resume, or is restoration purely aspirational/theological?',
    'If genuinely expected (< 500 years): preparation study is rational coordination, not extraction. Reclassifies to Tangled Rope (coordination + modest extraction for deferred gain). If indefinitely deferred (never, or > 2000 years): preparation is performative only, extraction is primary. Confirms Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_vs_presented_messianic_timeline, empirical, 'Whether messianic restoration is genuinely expected or indefinitely deferred').

omega_variable(
    substitution_reading_institutional_acceptance,
    'Do authoritative institutions (yeshiva, movement leadership) actually accept the substitution reading (prayer/study replaced sacrifice, archive is memorial not occupied kernel), or do they maintain the performance-only husk narrative despite knowing its unrealizability?',
    'Textual analysis of official sources: do authorities explicitly affirm substitution (prayer/study = sacrifice equivalent)? Or do they maintain ambiguity about whether study is performance or mere preparation? Institutional position statements on the status of Kodashim in contemporary practice.',
    'If substitution is openly affirmed: the performance-only reading is one of multiple coexisting readings. If performance-only is maintained despite substitution awareness: extractive narrative is deliberate (reinforces snare classification). If authorities are genuinely undecided: omega remains unresolved and institutions function with cognitive dissonance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_reading_institutional_acceptance, empirical, 'Whether substitution reading is institutionally accepted or performance-only narrative maintained').

omega_variable(
    devotional_harm_from_deferred_fulfillment,
    'Does treating the Kodashim archive as an occupied kernel (awaiting messianic restoration) cause measurable psychological/spiritual harm to practitioners who come to understand the indefinite deferral, or is the harm minimal because practitioners maintain compartmentalization?',
    'Interviews with practitioners and leavers; analysis of existential crises or identity disruptions correlated with deeper study of Kodashim; comparison of psychological outcomes between those who adopt substitution readings vs. those who maintain performance-only framework.',
    'If harm is significant: victim classification is empirically grounded, strengthens snare identification. If harm is minimal: extraction is less severe than base_properties suggest, possible reclassification to Tangled Rope. If practitioners maintain compartmentalization: the constraint''s suppression mechanism (compartmentalization as internalized suppression) becomes clearer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_harm_from_deferred_fulfillment, empirical, 'Whether deferred messianic fulfillment causes measurable practitioner harm').

omega_variable(
    kernel_vs_husk_ontological_status,
    'In the performance-only reading, what exactly is the status of the Kodashim corpus — is it a genuine kernel (an occupied but currently inert sacred space) or a husk (merely an archive with no present ontological force)?',
    'Textual exegesis distinguishing theological language of occupation vs. preservation; analysis of whether Kodashim law is presented as binding (even if impossible to perform) or merely commemorative. Study of the role attributed to Kodashim in messianic restoration narratives.',
    'If kernel (occupied but inert): the constraint is coordination around a deferred obligation. If husk (merely archived): the constraint is extraction pure, no coordination element. If ontologically ambiguous: institutions may exploit the ambiguity to present performance-only as kernel-occupation to practitioners while maintaining archive-only logic for institutional purposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_husk_ontological_status, conceptual, 'Ontological status of Kodashim in performance-only reading: kernel or husk?').

omega_variable(
    reading_identity_as_fsm_candidate,
    'Is the performance-only reading a genuine theological position held by informed practitioners, or is it a false summit — a natural-law presentation of a contingent institutional arrangement designed to naturalize the extraction of devotion from those unaware of substitution alternatives?',
    'Historical analysis of how the performance-only reading emerged; comparison with study_as_exercise and substitution_archive readings on institutional acceptance and authority grounding. Detection of whether performance-only is presented as inevitable theological consequence or as one defensible choice among alternatives.',
    'If genuine position: the reading stands as written. If false summit: the engine''s FSM detector may reclassify based on beneficiary presence and interpretive institutional control. The entire constraint may be understood as an institutional cover story for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_as_fsm_candidate, conceptual, 'Whether performance-only reading is genuine theology or false summit covering institutional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.65).
narrative_ontology:measurement(kodashim_perf_tr_t500, kodashim_corpus__performance_only, theater_ratio, 500, 0.78).
narrative_ontology:measurement(kodashim_perf_tr_t1000, kodashim_corpus__performance_only, theater_ratio, 1000, 0.88).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kodashim_perf_be_t500, kodashim_corpus__performance_only, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(kodashim_perf_be_t1000, kodashim_corpus__performance_only, base_extractiveness, 1000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_perf_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(kodashim_perf_su_t500, kodashim_corpus__performance_only, suppression_requirement, 500, 0.68).
narrative_ontology:measurement(kodashim_perf_su_t1000, kodashim_corpus__performance_only, suppression_requirement, 1000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim constraint family comprises three readings of the same kernel (kodashim_corpus). Each reading has distinct ε values and type classifications reflecting different institutional framings of the same textual-legal corpus. The performance-only reading (this file) has ε=0.68 (Snare) — high extractiveness due to deferred restoration narrative. The study-as-exercise reading has lower ε (coordination-dominant, not extraction-dominant) — study itself is performance-equivalent. The substitution-archive reading has lowest ε (memorial archive, not occupied kernel). The three readings coexist as competing institutional interpretations but do not logically coexist within a single framework — adoption of one reading typically forecloses or significantly constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
