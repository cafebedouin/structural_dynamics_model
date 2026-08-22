% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study Obligation as Messianic Preparation
 *   domain: religious/legal
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the Jewish legal
 *   system faced a structural rupture: the laws governing sacrifice and
 *   Temple service remained binding under halakha but became impossible to
 *   perform. The messianic-preparation reading resolves this contradiction by
 *   framing the study obligation as instrumental—current-generation scholars
 *   preserve the technical knowledge of sacrificial law (procedures,
 *   measurements, priestly functions, ritual sequences) in transmissible form
 *   so that when the Temple is rebuilt in the messianic future, the law can
 *   be immediately enacted. This reading coexists with two sibling readings:
 *   study_as_archive (which treats kodashim as historical memory rather than
 *   ongoing obligation) and study_as_performance (which claims that studying
 *   the law itself enacts the cosmic function of sacrifice, making the
 *   Temple's absence irrelevant). This constraint is generated under the
 *   study_as_preparation reading only. The claim/metric independence rule
 *   applies: the constraint is CLAIMED as a Scaffold (temporary support until
 *   messianic restoration resumes performance) while the authored metrics
 *   describe low extractiveness and low theater—the structural form of
 *   genuine coordination, not extractive cover. The divergence is intentional
 *   and documents the reading's internal coherence.
 *
 * KEY AGENTS:
 *   - Talmudic scholars: institutional agenda-setter, transmit and interpret sacrificial law despite its unperformability
 *   - Current-generation interpreters: moderate-power payers, bear the intellectual cost of mastering unperformable law
 *   - Messianic future community (non-agent): beneficiary in cosmic time, will inherit technical knowledge
 *   - Jewish law authorities: institutional beneficiary, maintain halakhic system integrity
 *   - Competing interpretive traditions: excluded, would challenge the messianic-preparation frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study Obligation as Messianic Preparation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal").

narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'f9fd782a-dd65-4bb1-84cf-6d339d6c0703').
narrative_ontology:cs_kernel_codification('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', formalized).
narrative_ontology:cs_authority_grounding('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', lineage).
narrative_ontology:cs_interpretation_layer_present('f9fd782a-dd65-4bb1-84cf-6d339d6c0703').
narrative_ontology:cs_reading_relation('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', kodashim_obligation__study_as_archive, influences).
narrative_ontology:cs_reading_relation('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_axiom('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', foundational, temple_restoration_is_necessary_and_imminent).
narrative_ontology:cs_axiom_status(temple_restoration_is_necessary_and_imminent, holdable).
narrative_ontology:cs_axiom_grounding('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', temple_restoration_is_necessary_and_imminent, deontological).
narrative_ontology:cs_axiom('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', foundational, sacrificial_law_binds_despite_unperformability).
narrative_ontology:cs_axiom_status(sacrificial_law_binds_despite_unperformability, holdable).
narrative_ontology:cs_axiom_grounding('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', sacrificial_law_binds_despite_unperformability, conventional).
narrative_ontology:cs_axiom('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', secondary, study_is_instrumental_preparation_not_enactment).
narrative_ontology:cs_axiom_status(study_is_instrumental_preparation_not_enactment, holdable).
narrative_ontology:cs_axiom_grounding('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', study_is_instrumental_preparation_not_enactment, conventional).
narrative_ontology:cs_reference_frame('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', temple_standing_sacrificial_law_operative).
narrative_ontology:cs_drift_state('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', second_temple_destruction_70_ce_ongoing_diaspora, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f9fd782a-dd65-4bb1-84cf-6d339d6c0703', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, jewish_law_authorities).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, temple_restoration_cosmic_necessity).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, sacrificial_law_eternal_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit and interpret sacrificial law (kodashim) in detail despite the Temple's destruction. They maintain the technical knowledge—procedures, materials, measurements, priestly functions—in written and oral form. Their authority rests on the assumption that this knowledge will be needed when the Temple is restored. Exit from this obligation would constitute abandonment of core Jewish legal tradition and identity as transmitters of Torah.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, talmudic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the intellectual and spiritual cost of mastering a legal system that cannot be performed in their lifetime and likely not in theirs. They invest years studying sacrificial procedure, Temple architecture, priestly vestments, and ritual sequences that have no application in lived Jewish practice. The obligation is non-coercive but culturally binding—abandoning it means severing from authoritative tradition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_interpreters, payer,
    moderate, generational, identity_locked, global).

% The future Jewish community at the time of Temple restoration will inherit intact technical knowledge and legal precedent for performing sacrifice, enabling immediate restoration of the sacrificial cult. This beneficiary is not present and not an agent, but the constraint's entire logic points toward its eventual arrival and vindication.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_community, beneficiary,
    powerful, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, messianic_future_community).

% Maintain the integrity and continuity of Jewish legal system (halakha) across historical rupture. By preserving sacrificial law in detail despite the Temple's destruction, they prevent the complete loss of a major legal domain and preserve the tradition's internal coherence—the law remains whole even if one section is dormant.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, jewish_law_authorities, beneficiary,
    institutional, civilizational, analytical, global).

% Other Jewish movements (Karaite, Samaritan, Reform, Conservative) that either reject the binding nature of sacrificial law or interpret its continuation differently are structurally excluded from adjudicating the obligation. They are not in the room where the messianic-preparation reading is authoritative; their competing readings would challenge the constraint's entire justification.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, competing_interpretive_traditions, excluded,
    moderate, generational, constrained, global).

% External perspective on the constraint structure: observes that the obligation to study persists despite the unperformability of the studied law, and that the messianic-restoration axiom is the reading that justifies this persistence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical knowledge of sacrificial law and Temple procedure in transmissible form across the centuries of galut (exile) when performance is impossible. Coordinates the scholarly community around a shared obligation to maintain a specific legal domain intact, preventing its dissolution or corruption despite disuse.
% TRANSFER_FUNCTION: Moves intellectual labor and temporal investment from current-generation interpreters (the 'payers') into a corpus of transmitted knowledge that will be accessed by the messianic-future community (the 'beneficiary'). The transfer is deferred across centuries and contingent on Temple restoration.
% ABSENT_VOICES: Competing interpretive traditions (Karaite, Samaritan, Reform movements) that reject the perpetual binding status of sacrificial law or interpret its cosmic function differently are excluded from the authoritative framework in which this reading operates. Their objections would re-frame the entire constraint, but they are not in the deliberative seat where the messianic-preparation reading is operationalized.
% DISAPPEARANCE_RATIONALE: If the obligation to study sacrificial law vanished, Orthodox Jewish legal tradition would lose a major domain of its halakhic corpus, creating a permanent discontinuity in the transmitted law. Messianic-restoration readings would be refuted—the future community could not perform the Temple service. Other readings (archive, performance) would not be foreclosed, but the particular bridging function this reading provides would collapse. Whether this constitutes 'rearrangement' depends on whether the Temple's eventual restoration is considered structurally necessary (yes in this reading, contested in others).
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, the sacrificial law became unperformable but remained binding. The obligation arose to prevent this binding law from being lost, forgotten, or corrupted during the exile. Study became the instrument of preservation, maintaining the law's technical integrity for the future when performance would resume.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources (Mishnah, Gemara, later codes) explicitly ground the obligation to study sacrificial law in the expectation of Temple restoration and the need to preserve knowledge for that future. This corroboration comes from within the tradition's own authoritative texts (the messianic-preparation reading's primary source material), not from external parties. No non-orthodox, non-believer source will corroborate a cosmic function that depends on messianic restoration; corroboration here is internal-textual (the constraint's own founding authorities), which establishes the reading's coherence but not its truth in external perspective.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the obligation imposes genuine study costs on current interpreters but the benefit is not captured by any present actor—it is deferred to a future that may never arrive. The extraction is not coercive in the usual sense; participants are identity-locked into the obligation by the authority of transmitted tradition, not trapped by external force. Suppression is very low (0.15) because the study requirement does not rely on preventing alternatives—it relies on the authority of the halakhic framework and the messianic-restoration axiom being accepted. Theater is modest (0.22): the study itself is performed with full intellectual seriousness, not theatrically, but the study's practical endpoint (preparation for performance) is deferred indefinitely. Accessibility_collapse is moderate (0.42): alternatives exist (treat the law as archive, accept the performance-enactment reading, or abandon it entirely), but participants identity-locked into Orthodox tradition experience those alternatives as foreclosed by their commitment. Resistance is moderate (0.38): the obligation meets resistance from non-Orthodox movements and secular reinterpretations, but no resistance from within the framework that grounds the obligation itself. The measurement series spans 2000 years; extractiveness remains stable (within the 0.26–0.32 range) across the interval because the deferred-benefit structure does not change—no accumulation of rent-seeking, no intensification of enforcement, but also no attenuation of the obligation. Theater ticks upward slightly in the post-500 CE period as non-sacrificial liturgical practices emerge, creating a differentiation between 'core obligation' study and 'supplementary' study, but the ratio remains low because the obligation itself never becomes primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The messianic-preparation reading divides observer seats along temporal and cosmological lines. An Orthodox believer who accepts the messianic-restoration axiom reads the constraint as low-extraction coordination (cost is justified by future need). A skeptic about messianic restoration reads it as imposing costs for a benefit that will never arrive, sliding toward extractive-burden classification. A secular observer might read it as pure theatrical maintenance—a cultural practice dressed in future-oriented language. These are readings of the SAME constraint, not different constraints, because the referent (the binding status of sacrificial law) is fixed; only the observer's frame changes. This is NOT the place to decompose into separate constraints—the ε-invariance principle does not apply (the measured extractiveness does not change when the observer changes, because the structural arrangement does not change). This is the place to omega-document the axiom-dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-generation interpreters are the 'payers' (bear the cost of mastering unperformable law, identity-locked into the obligation by tradition, moderate power). The messianic-future community is the 'beneficiary' (receives the preserved knowledge, though non-present). Jewish law authorities are secondary beneficiaries (their system maintains integrity). No party in the present directly captures extraction—this is a deferred, future-oriented constraint. Directionality for the payer (current interpreters) sits at d ≈ 0.60–0.70 (they bear costs, have limited exit, are moderate-power), but effective extraction (χ) remains low because there is no present actor extracting from them; the cost flows forward in time, not to a capturing actor. Directionality for messianic-future (d ≈ 0.0, full beneficiary) is analytical-seat treatment—they exist only in the reading's temporal frame. No override needed; the structural derivation captures the correct relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The messianic-preparation reading resolves mandatrophy (the gap between founding problem and present function) by establishing an explicit sunset clause—the obligation to study is temporary in principle, ending when the Temple is restored and sacrificial law becomes performable again. The founding problem (preserve knowledge for Temple restoration) remains live under this reading, so no mandatrophy arises. Other readings would see mandatrophy: study_as_archive would observe that the original mandate (preserve for future performance) has been superseded by the actual function (historical memory, identity-marker), so the original mandate is dead; study_as_performance would deny any temporal bound (the cosmic function is eternal, not sunset), so no mandatrophy exists under that reading either. This reading alone treats the obligation as explicitly temporary, justified by a future condition, and thus as a Scaffold rather than an atrophied Rope or performative Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_restoration_contingency,
    'Is the Temple''s restoration a live conditional that grounds the obligation''s rational structure, or a deferred myth whose non-arrival will eventually foreclose the obligation?',
    'Historical-observational: if the Temple is not rebuilt within some extended time window (e.g., 2500+ years from now), does the tradition''s internal logic revise to study_as_archive or study_as_performance, or does it collapse the obligation entirely?',
    'If the conditional is truly live (Temple restoration is expected within a bounded time frame), the constraint remains a valid Scaffold and mandatrophy is avoided. If the conditional is indefinitely deferred or understood as non-literal, the constraint shifts toward Piton or Rope classification—a cultural obligation maintained by inertia and identity-lock rather than by future-oriented instrumentality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_contingency, conceptual, 'Whether messianic restoration is a live temporal condition or a deferred/mythological frame.').

omega_variable(
    study_enactment_boundary,
    'Is studying sacrificial law instrumentally distinct from performing it (as this reading claims), or does the study itself enact the cosmic function (as study_as_performance claims)?',
    'Textual-hermeneutic analysis of rabbinic sources: do they explicitly distinguish between study-as-preparation (instrumental, temporary) and study-as-enactment (intrinsic, cosmic), or do they blend the two functions?',
    'If study is instrumentally distinct from performance, this reading''s low-extraction structure holds and the Scaffold classification is sound. If study is intrinsically enactive (cosmic function performed through learning), the constraint''s function is not temporary, extraction becomes zero (no deferral, no cost without present benefit), and the type shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_enactment_boundary, conceptual, 'Whether study is instrumental preparation or intrinsically enactive sacrifice.').

omega_variable(
    identity_lock_mechanism,
    'Is the current generation''s commitment to sacrificial law study maintained by doctrinal binding (the law is binding) or by cultural/identity-fusion (abandoning it means severing from Jewish tradition)?',
    'Ethnographic and psychological analysis: do leavers of Orthodox Judaism report doctrinal disagreement about the law''s binding status, or identity-severance costs that make internal compliance easier than exit?',
    'If binding, the obligation is structurally coercive (law enforces itself via authority). If identity-fused, the obligation is self-enforcing via identity-alignment, and the suppression metric (0.15, currently very low) would rise if identity-lock failed—showing that a significant portion of the constraint''s persistence depends on identity rather than on belief in the cosmic function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the study obligation is maintained by doctrinal binding or by identity-fusion.').

omega_variable(
    kernel_contest_framing,
    'Which reading of the kodashim_obligation kernel is the structurally dominant one within contemporary Orthodox halakha?',
    'Analysis of contemporary rabbinical rulings, academy curricula, and published justifications for the study obligation in the 20th and 21st centuries.',
    'If study_as_preparation is dominant, this constraint''s classification as valid Scaffold holds. If study_as_performance is dominant, the extraction and sunset-clause structure collapse and the constraint becomes Rope or even pure coordination. If study_as_archive is gaining (signaling halakhic drift), the constraint is shifting toward Piton—maintained by inertia but no longer grounded in the founding mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing, empirical, 'The contemporary textual-authority distribution among the three kodashim_obligation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t250, kodashim_obligation__study_as_preparation, theater_ratio, 250, 0.2).
narrative_ontology:measurement_basis(koda_tr_t250, observed).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.22).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.24).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.22).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t250, kodashim_obligation__study_as_preparation, base_extractiveness, 250, 0.28).
narrative_ontology:measurement_basis(koda_be_t250, observed).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.26).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.3).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_preparation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.06).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the binding status of sacrificial law after Temple destruction. This story (study_as_preparation) treats the obligation as temporary support for messianic restoration; sibling stories treat it as historical archive or as intrinsic cosmic enactment. The three readings coexist across different communities but occupy logically distinct frames. Each story authors its own ε, beneficiary/victim set, and type classification independently; the sibling stories are not authored here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
