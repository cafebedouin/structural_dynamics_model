% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Performance-Only Husk
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus is a Talmudic section detailing the law of physical
 *   animal sacrifice in the temple. The Jewish temple was destroyed in 70 CE.
 *   No sacrifices have been possible for nearly 2000 years. This constraint
 *   story instantiates ONE reading of the contested kernel: the
 *   'performance-only' reading holds that study of sacrifice law is
 *   preparation for messianic restoration, not fulfillment of the mitzvah
 *   itself. The mitzvah remains binding but unrealizable in the present. The
 *   reading positions current practice as perpetual deferral to a future
 *   state. This instantiation measures that reading's extractiveness:
 *   practitioners devote intellectual-spiritual energy under a frame whose
 *   ultimate fulfillment is indefinitely postponed. The beneficiaries are the
 *   institutions that control the definition of what the study counts as. The
 *   victims are practitioners whose devotion is channeled into a future that
 *   institutional theology has configured as perpetually unavailable. The
 *   constraint operates as a snare because its persistence depends on
 *   suppressing or marginalizing the rival readings (study-as-exercise,
 *   substitution-archive) that would reframe current practice as complete or
 *   memorial rather than preparatory.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions — agenda-setter (institutional power); controls the reading that study is preparation-only
 *   - practitioners_seeking_immediate_mitzvah_fulfillment — payer (moderate power, identity-locked exit); bears the cost of sustained engagement with an indefinitely deferred framework
 *   - textual_scholarship_community — beneficiary (organized, mobile exit); receives institutional support for intensive textual labor precisely because the reading requires technical precision
 *   - competing_rabbinic_readings — excluded (organized, constrained exit); proponents of alternative readings marginalized within the dominant institutional frame
 *   - temporal_consciousness_of_believers — observer non-agent; the shared anticipatory state the reading maintains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.81).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.67).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.81).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Performance-Only Husk").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, 'aecab24b-76ff-4f03-88ca-81a1c72a4d00').
narrative_ontology:cs_kernel_codification('aecab24b-76ff-4f03-88ca-81a1c72a4d00', fixed_text).
narrative_ontology:cs_authority_grounding('aecab24b-76ff-4f03-88ca-81a1c72a4d00', lineage).
narrative_ontology:cs_interpretation_layer_present('aecab24b-76ff-4f03-88ca-81a1c72a4d00').
narrative_ontology:cs_reading_relation('aecab24b-76ff-4f03-88ca-81a1c72a4d00', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('aecab24b-76ff-4f03-88ca-81a1c72a4d00', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('aecab24b-76ff-4f03-88ca-81a1c72a4d00', foundational, physical_sacrifice_alone_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(physical_sacrifice_alone_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('aecab24b-76ff-4f03-88ca-81a1c72a4d00', physical_sacrifice_alone_fulfills_mitzvah, deontological).
narrative_ontology:cs_axiom('aecab24b-76ff-4f03-88ca-81a1c72a4d00', foundational, perpetual_restoration_readiness_maintains_obligation).
narrative_ontology:cs_axiom_status(perpetual_restoration_readiness_maintains_obligation, holdable).
narrative_ontology:cs_axiom_grounding('aecab24b-76ff-4f03-88ca-81a1c72a4d00', perpetual_restoration_readiness_maintains_obligation, instrumental).
narrative_ontology:cs_reference_frame('aecab24b-76ff-4f03-88ca-81a1c72a4d00', temple_centered_sacrificial_law).
narrative_ontology:cs_drift_state('aecab24b-76ff-4f03-88ca-81a1c72a4d00', post_temple_destruction_permanent_exile, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('aecab24b-76ff-4f03-88ca-81a1c72a4d00', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, practitioners_seeking_immediate_mitzvah_fulfillment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, textual_scholarship_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the reading that Kodashim study is performance-only preparation for a future messianic temple. Controls the theological interpretation that study of the laws is faithful transmission but not fulfillment of the mitzvah itself. Their institutional existence and legitimacy depend on the template remaining 'awaiting' restoration rather than occupied or superseded. They invest significant authority in maintaining the temporal deferral.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Devote intellectual and spiritual energy to Kodashim study under the premise that they are performing the mitzvah through engaged study. They bear the cost of sustained engagement with a framework whose ultimate fulfillment is indefinitely deferred. Exit means either accepting that their study carries no direct mitzvah status (theological loss) or moving to one of the rival readings (institutional friction). Their devotion is channeled toward a future state that institutional theology has configured as perpetually unavailable in the present.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, practitioners_seeking_immediate_mitzvah_fulfillment, payer,
    moderate, biographical, constrained, global).

% Receives sustained attention, interpretive labor, and institutional support for deep textual analysis because the Kodashim corpus is treated as a precise blueprint awaiting physical enactment. The scholarship thrives on the reading's technical precision requirement: if study were merely memorial archive, the textual detail would be less central; if study were complete performance, the detail would be differently motivated. This reading keeps the corpus maximally demanding and therefore maximally generative of scholarly work.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, textual_scholarship_community, beneficiary,
    organized, generational, mobile, global).

% Proponents of alternative readings (study-as-exercise, substitution-archive) are marginalized within the institutional framework that controls the performance-only interpretation. They possess textual arguments and hermeneutical authority but are structurally positioned as dissidents from the dominant reading. Their exclusion is not from the text itself but from the legitimacy structure that determines what the text's standing assignment is.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, competing_rabbinic_readings, excluded,
    organized, generational, constrained, global).

% The collective state of anticipation, deferred fulfillment, and the structure of messianic hope itself. The reading maintains a specific temporal configuration: study as preparation, present as incomplete, future as the only site of true performance. This temporal frame is both the constraint's medium and its effect.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temporal_consciousness_of_believers, observer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_non_agent(kodashim_corpus__performance_only, temporal_consciousness_of_believers).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified framework for transmitting and standardizing the detailed law of physical sacrifice across generations and communities, ensuring precision-preservation across centuries of exile from the temple.
% TRANSFER_FUNCTION: Moves intellectual devotion, institutional authority, and the capacity to define mitzvah-fulfillment from practitioners seeking immediate meaning-making toward the deferred messianic future and the institutions that control preparation theology.
% ABSENT_VOICES: Practitioners who have abandoned the Kodashim engagement entirely (their silence is structural); communities that have adopted the rival readings (study-as-exercise or substitution-archive) and consider themselves in full mitzvah-compliance now; voices that question whether perpetual deferral serves a genuine theological function or maintains institutional control. These populations exist but their dissent is routed through denominational boundaries rather than incorporated into the dominant institutional conversation.
% DISAPPEARANCE_RATIONALE: From the performance-only reading: if the reading collapsed, practitioners would experience massive theological vertigo—decades or centuries of study would be reframed as either incomplete (study-as-exercise) or superseded (substitution). The institutions that maintain the reading would lose their monopoly on defining the corpus's function. From the rival readings: nothing essential would disappear; the text would remain; practitioners would simply shift their understanding of what their engagement accomplishes. The contest is real and the outcome is not predetermined.
% FOUNDING_PROBLEM: How does a diaspora Jewish community maintain the precise law of temple sacrifice when the temple is destroyed and sacrifice physically impossible? How does the law remain binding and studyable when its enactment is absent?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition itself attests the founding problem: the Talmud extensively debates what happens to sacrificial obligations in the absence of the temple. Classical sources (Maimonides, Rambam) treat study as preparation for restoration. Modern scholarship (including academic historians outside the benefiting institutions) identifies the founding problem as real. The contest is over how to resolve it: the performance-only reading is one of three live answers, each with scholarly and traditional corroboration.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint extracts practitioners' devotion toward a future state they cannot realize in the present, and the institutional beneficiaries maintain authority by controlling the definition of what study counts as. The beneficiary set is narrow (messianic-preparation institutions) and receives concentrated gain (authority over framework definition, sustained scholarly attention). Suppression is substantial (0.67) because the constraint's persistence depends on actively suppressing or marginalizing the rival readings—the performance-only reading is not the only defensible interpretation of the Kodashim corpus, and maintaining it as orthodoxy requires institutional enforcement. Theater is high (0.72) because much of the enforcement activity consists of rhetorical reaffirmation that study is 'preparation' rather than performance, doctrinal emphasis, and institutional positioning—the theatrical component increases over time as the institutional framework becomes more crystallized. Accessibility of alternatives is low (0.48): practitioners cannot easily exit the performance-only frame without either accepting that their study has no direct mitzvah status (massive theological cost) or moving to a rival reading (institutional friction, risk of exclusion from orthodox spaces). Resistance is moderate (0.54): some practitioners and scholars do adopt the rival readings, but they face institutional marginalization rather than outright persecution. The measurement series show extractiveness and theater rising slightly over the interval as institutional theology becomes more explicitly defined and the deferral frame more institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (messianic-preparation institutions) and the payer (practitioners) experience this constraint differently. From the institutional seat: this reading is a legitimate theological position that preserves the binding status of sacrifice law while acknowledging current impossibility—it is a rational solution to the founding problem (how do we maintain sacrifice obligation in exile?). From the practitioner seat: this reading extracts their devotion toward a future that may never arrive, while sibling readings (study-as-exercise, substitution-archive) would allow immediate mitzvah-fulfillment meaning. The engine should compute these seats as accessing different types: from the institutional seat, snare (they control the frame and benefit from its perpetuation); from the practitioner seat, also snare (they are trapped in a deferred framework they cannot exit without cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions: d approaches 0.0 (full beneficiary)—they collect authority, institutional relevance, and control over the meaning-assignment of the corpus. Practitioners: d approaches 1.0 (full target)—they bear the cost of sustained engagement with a framework whose terminal condition is perpetually postponed. The exit for practitioners is identity-locked: abandoning the performance-only reading means abandoning a core identity claim ('I am engaged in meaningful mitzvah practice'). The alternative frames (study-as-exercise, substitution-archive) offer exit routes, but moving to them carries institutional friction and identity risk. Textual scholars: secondary beneficiary (d near 0.2)—they benefit from the precision-intensive reading that treats the corpus as a technical blueprint, but they are not the primary beneficiaries (the institutions are). Competing readings: trapped in the institutional frame (d near 0.85), unable to exit because their authority is constrained by the dominant reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE but the performance-only reading transforms its solution into a permanent state. The original problem was: 'How do we keep sacrifice law binding when sacrifice is physically impossible?' The solution-as-established was: 'Study and preparation maintain the mitzvah in force until restoration.' But over centuries this has calcified into: 'Study IS preparation, nothing more, and restoration remains perpetually future.' The mandatrophy here is not of the founding problem (it never dies—each generation asks 'Are we ready for restoration?') but of the solution's original time-boundary. What was meant as a transitional framework (prepare until restoration) has become a permanent state (perpetual preparation). The rival readings (study-as-exercise, substitution-archive) represent attempts to escape this mandatrophy: they would reframe the founding problem as either solved (restoration already came in the form of the synagogue and prayer) or solved-differently (study IS the performance, now). The performance-only reading's persistence depends on suppressing these escape routes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unrealizability_as_structural_feature,
    'Is the perpetual deferral of temple performance a theological principle that genuinely needs institutional maintenance, or has it become a performance-maintenance mechanism that institutions now depend on for their authority?',
    'Historical analysis of when the ''awaiting'' frame was canonized and became institutional orthodoxy vs. when rival readings emerged; discourse analysis of whether contemporary institutional theology actively forecloses the rival readings or merely presents its own reading as dominant.',
    'If deferral is a necessary theological principle, the extractiveness is lower (institutional preparation theology is legitimate overhead). If deferral is now maintenance-dependent, the extractiveness is higher and the constraint is a snare using unrealizability to maintain authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrealizability_as_structural_feature, empirical, 'Whether perpetual deferral is theologically structural or institutionally maintained.').

omega_variable(
    kernel_reading_distinctness,
    'Are the three declared readings (performance-only, study-as-exercise, substitution-archive) genuinely distinct constraint structures, or are they variations on a single underlying frame?',
    'Structural analysis: do the three readings produce different d-vectors for the same stakeholder set? Do they require different enforcement mechanisms? Do they lead to different type classifications from the same base metrics?',
    'If they are distinct (as the ε-invariance principle suggests), this story and its two siblings are three separate constraints; if they are interpretive variants of one underlying constraint, the constraint''s type is under-determined and the frame-choice governs the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether Kodashim readings are distinct constraints or interpretive variants.').

omega_variable(
    study_mitzvah_fulfillment_boundary,
    'What does it mean for study to ''count'' as performance of a mitzvah? Is it a metaphorical substitution (study ''stands in for'' sacrifice), a literal occupancy (study IS sacrifice in its present form), or a preparation (study prepares for future literal enactment)?',
    'Textual-historical analysis of when each frame emerged in rabbinic literature; phenomenological interviews with practitioners about what they experience their study as accomplishing.',
    'The performance-only reading depends on a sharp boundary: study is preparation, not performance. If that boundary is permeable (study has partial mitzvah-fulfillment status), the extractiveness drops because practitioners are not entirely in a deferred state. If the boundary is stable and institutional theology enforces it, extractiveness remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_mitzvah_fulfillment_boundary, conceptual, 'Boundary between study as performance vs. preparation in Jewish law.').

omega_variable(
    messianic_restoration_probability,
    'Is the messianic restoration understood by contemporary practitioners and institutions as a realistic future event (making deferral a finite cost) or an eschatological principle that may never be realized (making deferral permanent)?',
    'Textual analysis of how institutional theology frames messianic probability; discourse analysis of contemporary institutional rhetoric about restoration timelines.',
    'If restoration is treated as realistic (even if far future), practitioners can justify the cost of deferral. If restoration is treated as eschatological-but-not-inevitable, deferral becomes permanent and the constraint approaches pure snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_probability, empirical, 'Whether messianic restoration is treated as realistic or eschatological-only.').

omega_variable(
    alternative_reading_suppression_mechanism,
    'What keeps the rival readings (study-as-exercise, substitution-archive) marginalized? Is it the internal logic of the performance-only reading, or is it active institutional suppression of alternatives?',
    'Institutional analysis of which readings are permitted in which spaces (seminaries, congregations, scholarship); textual analysis of whether the rival readings are actively refuted or simply not taught; network analysis of which scholars advance each reading and what institutional positions they hold.',
    'If suppression is structural (the performance-only reading logically defeats rivals), the constraint is snare operating through legitimate reasoning. If suppression is institutional (alternatives are excluded from authority-spaces even if textually defensible), the snare character is amplified and enforcement becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression_mechanism, empirical, 'Mechanism by which alternative Kodashim readings are marginalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.58).
narrative_ontology:measurement(koda_tr_t3, kodashim_corpus__performance_only, theater_ratio, 3, 0.62).
narrative_ontology:measurement(koda_tr_t7, kodashim_corpus__performance_only, theater_ratio, 7, 0.66).
narrative_ontology:measurement(koda_tr_t12, kodashim_corpus__performance_only, theater_ratio, 12, 0.7).
narrative_ontology:measurement(koda_tr_t18, kodashim_corpus__performance_only, theater_ratio, 18, 0.72).
narrative_ontology:measurement(koda_tr_t25, kodashim_corpus__performance_only, theater_ratio, 25, 0.72).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(koda_be_t3, kodashim_corpus__performance_only, base_extractiveness, 3, 0.74).
narrative_ontology:measurement(koda_be_t7, kodashim_corpus__performance_only, base_extractiveness, 7, 0.77).
narrative_ontology:measurement(koda_be_t12, kodashim_corpus__performance_only, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(koda_be_t18, kodashim_corpus__performance_only, base_extractiveness, 18, 0.81).
narrative_ontology:measurement(koda_be_t25, kodashim_corpus__performance_only, base_extractiveness, 25, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.61).
narrative_ontology:measurement(koda_su_t3, kodashim_corpus__performance_only, suppression_requirement, 3, 0.63).
narrative_ontology:measurement(koda_su_t7, kodashim_corpus__performance_only, suppression_requirement, 7, 0.65).
narrative_ontology:measurement(koda_su_t12, kodashim_corpus__performance_only, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(koda_su_t18, kodashim_corpus__performance_only, suppression_requirement, 18, 0.67).
narrative_ontology:measurement(koda_su_t25, kodashim_corpus__performance_only, suppression_requirement, 25, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__performance_only, 0.12).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel has three structurally distinct constraint readings: performance-only (this story—snare, high extractiveness), study-as-exercise (sibling—rope or tangled_rope, coordination-dominant), and substitution-archive (sibling—piton or rope, memorial archive). Each reading produces different ε values and different victim/beneficiary structures from the same textual source. They are linked as network siblings because they compete for institutional and interpretive authority over what the Kodashim corpus 'counts as' in contemporary Jewish practice. The ε-invariance principle requires separate stories because measuring the constraint through the performance-only lens yields high extractiveness (unrealizable future), while the study-as-exercise lens yields lower extractiveness (present occupancy), and the substitution-archive lens yields near-zero extractiveness (historical memorial). These are not measurement variants of one constraint—they are distinct constraints instantiated by competing readings of the same kernel. All three belong in the corpus and must be linked to enable contamination and mismatch analysis across the kernel's reading family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__performance_only, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
