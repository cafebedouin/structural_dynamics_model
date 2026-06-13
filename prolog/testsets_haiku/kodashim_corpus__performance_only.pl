% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Performance-Only Archive
 *   domain: religious/rabbinic
 *
 * SUMMARY:
 *   This constraint instantiates the 'performance_only' reading of the
 *   Kodashim corpus kernel. The kernel is the rabbinic tradition's archive of
 *   sacrificial law, preserved in the Mishnah and subsequent literature after
 *   the Temple's destruction. The reading under analysis here holds that the
 *   Kodashim corpus is PURELY archival—a blueprint for a future messianic
 *   state when physical sacrifice resumes, not an occupied or substituted
 *   practice in the present. Study of Kodashim is framed as preparation for
 *   that future restoration, not as the performance of the mitzvah itself.
 *   This reading extracts from devoted practitioners by promising them that
 *   their study is preparatory (legitimacy deferred to a future state) while
 *   benefiting messianic-preparation institutions whose authority depends on
 *   maintaining the archive as binding. The reading is contested by two
 *   sibling readings: 'study_as_exercise' (which holds that study IS the
 *   performance in the post-Temple era) and 'substitution_archive' (which
 *   holds that prayer and study permanently replaced sacrifice, making the
 *   Kodashim a historical memorial, not a binding blueprint).
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: institutional agenda-setters who frame the Kodashim as binding blueprint for future restoration; their authority depends on the performance-only reading being true
 *   - devoted_practitioners_treating_archive_as_living_practice: identity-locked victims who invest devotional time and energy in Kodashim study under the belief they are performing a mitzvah; extraction occurs through displaced understanding
 *   - study_as_exercise_interpreters: excluded moderate-power voices holding an alternative reading where study itself IS the performance; suppressed in institutional discourse
 *   - substitution_archive_interpreters: excluded moderate-power voices holding an alternative reading where prayer replaced sacrifice permanently; actively marginalized in curricula
 *   - rabbinic_exegetes: organized beneficiaries who profit indirectly from the constraint through scholarly work reconciling present absence with future obligation
 *   - secular_historians_and_outsiders: analytical observers who see the constraint as a social fact about archive maintenance and competing interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.78).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.71).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Performance-Only Archive").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/rabbinic").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '49c9093c-42d8-40d4-a8e3-be1ae9b52d9e').
narrative_ontology:cs_kernel_codification('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', fixed_text).
narrative_ontology:cs_authority_grounding('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', extraction).
narrative_ontology:cs_interpretation_layer_present('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e').
narrative_ontology:cs_reading_relation('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', foundational, messianic_restoration_is_future_event).
narrative_ontology:cs_axiom_status(messianic_restoration_is_future_event, holdable).
narrative_ontology:cs_axiom_grounding('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', messianic_restoration_is_future_event, deontological).
narrative_ontology:cs_axiom('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', foundational, temple_sacrifice_is_unrealized_obligation).
narrative_ontology:cs_axiom_status(temple_sacrifice_is_unrealized_obligation, holdable).
narrative_ontology:cs_axiom_grounding('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', temple_sacrifice_is_unrealized_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', temple_destroyed_knowledge_must_be_preserved).
narrative_ontology:cs_drift_state('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', contemporary_two_millennia_post_destruction, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('49c9093c-42d8-40d4-a8e3-be1ae9b52d9e', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_practitioners_treating_archive_as_living_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, rabbinic_exegetes).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_temporality_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, temple_restoration_eschatology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the interpretive frame for Kodashim study, control curricula, maintain textual authority, and defend the performance-only reading against alternatives. They derive institutional legitimacy from maintaining the archive as binding for a future restoration. Exit from this role would dissolve their institutional identity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Study Kodashim texts intensively, recite portions in liturgy, engage in exegetical debates, believe they are performing a mitzvah or preparing for restoration. They invest decades in detailed mastery of sacrificial law they cannot operationally perform. Exit would require rejecting their community's core frame of meaning.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_practitioners_treating_archive_as_living_practice, payer,
    powerless, biographical, identity_locked, local).

% Produce scholarly literature, commentaries, and exegetical innovations that reconcile present absence with future obligation. Benefit from the constraint through publication, professional standing, and institutional affiliation. Dependent on the performance-only reading remaining canonical.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_exegetes, beneficiary,
    organized, generational, constrained, global).

% Hold the alternative reading that study itself IS the performance. Marginalized in institutional curricula and exegetical authority. Would argue the performance-only reading misunderstands rabbinic Judaism and empties present practice of meaning.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, study_as_exercise_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Hold the alternative reading that prayer and Torah study permanently replaced sacrifice; Kodashim is memorial. Actively suppressed in institutional discourse. Would argue the founding problem was solved 2,000 years ago and the performance-only reading denies Jewish history.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, substitution_archive_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Analyze the Kodashim as historical artifact and social fact. See the constraint as a system for maintaining institutional authority and competing interpretations. Document which reading dominates and how alternatives are suppressed.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, secular_historians_and_outsiders, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve detailed knowledge of Temple sacrifice law after its destruction; maintain expertise in sacrificial procedure; coordinate present communities around a future messianic obligation. The archive holds open the possibility of restoration and binds practitioners to an unrealized future state.
% TRANSFER_FUNCTION: Transfers devotional energy, study time, and intellectual resources from practitioners toward messianic-preparation institutions. Also transfers authority from present-focused interpretations (study-as-exercise, substitution readings) to future-focused institutional frames. The constraint moves legitimacy FROM present understanding TO an unrealized state.
% ABSENT_VOICES: Study-as-exercise interpreters and substitution-archive interpreters are excluded from institutional authority. They would argue that the performance-only reading is wrong—that study itself IS the performance or that the kernel was occupied when prayer replaced sacrifice. Their objections are treated as heretical or revisionist rather than as coherent alternative readings.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished and was replaced by study-as-exercise or substitution readings: the meaning of present practice would shift (from preparation to performance or memorial); institutional authority would be redistributed; practitioners' devotion would be reframed as complete in itself rather than preparatory. The disappearance would reorganize the entire meaning-structure of Kodashim study. However, some institutional actors and some practitioners would resist this reorganization as betraying core Jewish commitment to messianic restoration.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, Jewish communities faced the loss of the sacrificial practice that anchored religious life. The rabbinic academy preserved the detailed law in the Mishnah (Seder Kodashim) to ensure knowledge would not be forgotten and the obligation would remain alive across diaspora exile and generations of absence.
% FOUNDING_PROBLEM_CORROBORATION: Historians and archaeologists outside the Jewish tradition confirm the Temple was destroyed and sacrificial practice ceased 2,000 years ago; the original founding problem (preventing forgetting) is historically real. However, whether the founding problem PERSISTS is contested. Messianic-preparation institutions attest it remains live (restoration is still possible, knowledge must be ready). Study-as-exercise interpreters and substitution-archive interpreters attest it is dead—the problem was solved when alternative practices became permanent, or when the obligation shifted from physical to intellectual performance.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).

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
 *   Extractiveness is HIGH (0.78) because the constraint's legitimacy derives entirely from an unrealizable future state. Practitioners are promised that study prepares for performance (legitimacy deferred indefinitely), but the performance cannot occur absent external historical change (Temple rebuilding). The measurement series shows extractiveness RISING over 2,000 years as the founding problem (preserving knowledge against immediate forgetting) becomes institutionally solved, and what remains is purely the extraction function—maintaining institutional authority and practitioner devotion to a future that recedes further. Theater_ratio is MODERATE-HIGH (0.62) because the architecture—detailed legal knowledge, ceremonial recitations, exegetical discourse—performs the appearance of operational relevance while the constraint's actual function is deferred preparation. Suppression is HIGH (0.71) because alternative readings are actively excluded from institutional authority, curriculum, and public exegetical discourse. Accessibility_collapse is MODERATE (0.48) because practitioners CAN theoretically leave (no physical coercion), but exit is identity-locked—leaving means accepting that their religious community's core commitment is either false or already superseded. The grid shows suppression intensifying at all levels over time, especially at organizational and structural levels, while resistance weakens (practitioners come to internalize the performance-only reading as authentic).
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat: the performance-only reading is defensible rabbinic doctrine grounded in messianic expectation; study IS a form of worship and preparation; the Kodashim is binding law. From the practitioner seat: the promise is unfulfilled and unfulfillable; decades of study and devotion are framed as preparation for a state that has not arrived in 2,000 years; the extract is misallocated understanding. From the study-as-exercise seat: the institutional reading is wrong—it empties present practice of meaning and generates false promises; study itself IS the performance. From the substitution seat: both the institutional reading and the study-as-exercise reading misunderstand Jewish history; the kernel was occupied when prayer and study replaced sacrifice; the archive is memorial, not blueprint. The engine computes seat-specific classifications from the structural data; the authored claim (snare) reflects the performance-only reading's structure as extractive from the practitioner perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions: d near 0.0 (full beneficiary). They set the agenda, derive legitimacy and authority from the performance-only reading, and would lose institutional position if the reading collapsed. Their exit options are virtually none—their entire identity is constructed around maintaining the archive as binding. Devoted practitioners: d near 1.0 (full target). They bear the devotional cost under a promise of future performance. Exit is identity-locked (accepting study-as-exercise or substitution readings means rejecting their community's core commitment). The constraint extracts by misallocating their understanding—they think they are performing; they are actually preparing. Rabbinic exegetes: d near 0.2 (moderate beneficiary). They benefit from exegetical labor but do not set the constraint's fundamental frame; they are dependent on institutional agenda-setters maintaining the performance-only reading as canonical. Excluded interpreters and observers: not parties to the extraction; their d would be irrelevant or analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('preserve knowledge of sacrificial law against forgetting after the Temple's destruction') is DEAD in the literal sense: the knowledge is preserved. The performance-only reading persists not because the founding problem is live, but because institutional authority depends on maintaining the archive as binding and practitioners have internalized the deferred legitimacy structure. The constraint exhibits mandatrophy: it persists by institutional inertia and practitioner identity-lock even though the founding justification no longer applies. This is the snare signature—legitimacy extracted through a future state that may never materialize, defended by suppressing alternative readings that would relocate meaning to the present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_temporality_contestation,
    'Is the expectation of messianic restoration and Temple rebuilding a live theological commitment or a vestigial eschatological framework that shapes institutional identity but not actual institutional planning?',
    'Ethnographic and institutional analysis: compare resource allocation toward preparations for restoration (training in Temple procedures, maintenance of legal expertise, etc.) versus resource allocation toward present community-focused activities. If restoration is ''live,'' preparation would be an actual institutional priority; if vestigial, the constraint persists through narrative framing, not through operational preparation.',
    'If the messianic expectation is vestigial, the performance-only reading becomes purely extractive—it extracts devotional energy from practitioners through a promise the institution does not operationally believe. This would move the classification firmly into snare territory. If live, the extraction function is more ambiguous (the institution genuinely believes it is preparing, though the preparation targets an unrealized state).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_temporality_contestation, empirical, 'Whether messianic temporality is an actual institutional commitment or institutional theater.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the identity-lock preventing exit from the ''devoted practitioners'' seat structural (legal/social barriers) or internalized (belief that leaving would mean abandoning Jewish identity, communal belonging, or core self-concept)?',
    'Post-exit trajectory analysis: individuals who have left orthodox Jewish practice report on whether the suppression persists (belief in obligation, guilt, sense of incompleteness) or dissipates. If internalized, the constraint''s effective suppression is higher than the structural measure; the target carries it even after leaving the institutional structure.',
    'If internalized, the constraint''s hold is deeper than formal authority—it operates through self-policing rather than institutional enforcement. This would increase the extracted cost and support a snare classification. If purely structural, the constraint would be more brittle (individuals leaving would be free).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether the constraint''s suppressive force is structural, internalized, or both.').

omega_variable(
    kernel_occupation_by_alternative_readings,
    'Do the ''study_as_exercise'' and ''substitution_archive'' readings represent viable competing interpretations of the Kodashim within rabbinic tradition, or are they sectarian/heterodox positions that reject core rabbinic commitments?',
    'Textual analysis and intellectual history: examine whether pre-modern and modern rabbinic exegetes have held versions of these readings within the mainstream tradition (not as sectarian critiques but as variant interpretations). Compare institutional placement: are these readings taught in yeshivot as live options or excluded entirely?',
    'If the alternative readings are viable within rabbinic tradition, the performance-only reading''s dominance is a contingent institutional choice enforced by suppression, not a logical necessity. This strengthens the snare classification—the constraint extracts by suppressing live alternatives. If the alternatives are sectarian/external, the constraint would be defending core doctrine rather than extracting through suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_occupation_by_alternative_readings, conceptual, 'Whether the sibling readings are rabbinic variants or external critiques of rabbinic commitment.').

omega_variable(
    committer_frame_certainty,
    'Which of the three readings—performance_only, study_as_exercise, or substitution_archive—most accurately captures the kernel''s actual occupation status in contemporary Jewish practice and institutional teaching?',
    'Institutional ethnography and textual analysis: observe what is actually taught in yeshivot, how practitioners describe their study, whether institutional authority frames the Kodashim as preparatory or as present-occupied or as memorial. Compare against the three readings'' core claims.',
    'This is not a question the performance-only constraint itself can answer—it is the question the constraint reading presupposes. The committer frame is the structure of one reading against its siblings. If study-as-exercise or substitution readings prove descriptively true, the performance-only reading would be reclassified as false doctrine maintained through institutional suppression (even more deeply snare-like). If the performance-only reading proves true, the extraction would be explained as the cost of honesty about an unrealized eschatological state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_certainty, empirical, 'Which kernel reading is institutionally accurate and phenomenologically true.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t200, kodashim_corpus__performance_only, theater_ratio, 200, 0.31).
narrative_ontology:measurement_basis(koda_tr_t200, observed).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__performance_only, theater_ratio, 400, 0.38).
narrative_ontology:measurement_basis(koda_tr_t400, observed).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__performance_only, theater_ratio, 800, 0.51).
narrative_ontology:measurement_basis(koda_tr_t800, observed).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__performance_only, theater_ratio, 1200, 0.58).
narrative_ontology:measurement_basis(koda_tr_t1200, observed).
narrative_ontology:measurement(koda_tr_t1800, kodashim_corpus__performance_only, theater_ratio, 1800, 0.61).
narrative_ontology:measurement_basis(koda_tr_t1800, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__performance_only, theater_ratio, 2000, 0.62).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t200, kodashim_corpus__performance_only, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(koda_be_t200, observed).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__performance_only, base_extractiveness, 400, 0.51).
narrative_ontology:measurement_basis(koda_be_t400, observed).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__performance_only, base_extractiveness, 800, 0.68).
narrative_ontology:measurement_basis(koda_be_t800, observed).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__performance_only, base_extractiveness, 1200, 0.74).
narrative_ontology:measurement_basis(koda_be_t1200, observed).
narrative_ontology:measurement(koda_be_t1800, kodashim_corpus__performance_only, base_extractiveness, 1800, 0.77).
narrative_ontology:measurement_basis(koda_be_t1800, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__performance_only, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t200, kodashim_corpus__performance_only, suppression_requirement, 200, 0.44).
narrative_ontology:measurement_basis(koda_su_t200, observed).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__performance_only, suppression_requirement, 400, 0.52).
narrative_ontology:measurement_basis(koda_su_t400, observed).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__performance_only, suppression_requirement, 800, 0.64).
narrative_ontology:measurement_basis(koda_su_t800, observed).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__performance_only, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement_basis(koda_su_t1200, observed).
narrative_ontology:measurement(koda_su_t1800, kodashim_corpus__performance_only, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement_basis(koda_su_t1800, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__performance_only, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(koda_su_t2000, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2000
narrative_ontology:measurement(koda_grid_01, kodashim_corpus__performance_only, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(koda_grid_02, kodashim_corpus__performance_only, accessibility_collapse(class), 2000, 0.38).
narrative_ontology:measurement(koda_grid_03, kodashim_corpus__performance_only, accessibility_collapse(individual), 0, 0.52).
narrative_ontology:measurement(koda_grid_04, kodashim_corpus__performance_only, accessibility_collapse(individual), 2000, 0.48).
narrative_ontology:measurement(koda_grid_05, kodashim_corpus__performance_only, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(koda_grid_06, kodashim_corpus__performance_only, accessibility_collapse(organizational), 2000, 0.72).
narrative_ontology:measurement(koda_grid_07, kodashim_corpus__performance_only, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(koda_grid_08, kodashim_corpus__performance_only, accessibility_collapse(structural), 2000, 0.85).
narrative_ontology:measurement(koda_grid_09, kodashim_corpus__performance_only, resistance(class), 0, 0.61).
narrative_ontology:measurement(koda_grid_10, kodashim_corpus__performance_only, resistance(class), 2000, 0.42).
narrative_ontology:measurement(koda_grid_11, kodashim_corpus__performance_only, resistance(individual), 0, 0.68).
narrative_ontology:measurement(koda_grid_12, kodashim_corpus__performance_only, resistance(individual), 2000, 0.45).
narrative_ontology:measurement(koda_grid_13, kodashim_corpus__performance_only, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(koda_grid_14, kodashim_corpus__performance_only, resistance(organizational), 2000, 0.38).
narrative_ontology:measurement(koda_grid_15, kodashim_corpus__performance_only, resistance(structural), 0, 0.38).
narrative_ontology:measurement(koda_grid_16, kodashim_corpus__performance_only, resistance(structural), 2000, 0.21).
narrative_ontology:measurement(koda_grid_17, kodashim_corpus__performance_only, stakes_inflation(class), 0, 0.42).
narrative_ontology:measurement(koda_grid_18, kodashim_corpus__performance_only, stakes_inflation(class), 2000, 0.55).
narrative_ontology:measurement(koda_grid_19, kodashim_corpus__performance_only, stakes_inflation(individual), 0, 0.31).
narrative_ontology:measurement(koda_grid_20, kodashim_corpus__performance_only, stakes_inflation(individual), 2000, 0.48).
narrative_ontology:measurement(koda_grid_21, kodashim_corpus__performance_only, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(koda_grid_22, kodashim_corpus__performance_only, stakes_inflation(organizational), 2000, 0.72).
narrative_ontology:measurement(koda_grid_23, kodashim_corpus__performance_only, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(koda_grid_24, kodashim_corpus__performance_only, stakes_inflation(structural), 2000, 0.81).
narrative_ontology:measurement(koda_grid_25, kodashim_corpus__performance_only, suppression(class), 0, 0.35).
narrative_ontology:measurement(koda_grid_26, kodashim_corpus__performance_only, suppression(class), 2000, 0.61).
narrative_ontology:measurement(koda_grid_27, kodashim_corpus__performance_only, suppression(individual), 0, 0.28).
narrative_ontology:measurement(koda_grid_28, kodashim_corpus__performance_only, suppression(individual), 2000, 0.54).
narrative_ontology:measurement(koda_grid_29, kodashim_corpus__performance_only, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(koda_grid_30, kodashim_corpus__performance_only, suppression(organizational), 2000, 0.78).
narrative_ontology:measurement(koda_grid_31, kodashim_corpus__performance_only, suppression(structural), 0, 0.74).
narrative_ontology:measurement(koda_grid_32, kodashim_corpus__performance_only, suppression(structural), 2000, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__performance_only, 0.14).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim corpus kernel admits three structurally distinct readings with different ε values and victim sets. The 'performance_only' reading (this constraint) frames the archive as purely preparatory, making current study extracts from practitioners through deferred legitimacy (high extractiveness). The 'study_as_exercise' reading frames study itself as the performance, making the constraint a coordination mechanism (lower extractiveness). The 'substitution_archive' reading frames prayer and study as having permanently replaced sacrifice, making the constraint a historical memorial (lower extractiveness). These are not the same constraint viewed differently—they have different ε values, different victim sets, and different temporal structures. Each is authored as a separate constraint story, linked through network.affects_constraints. The performance_only reading influences the others by claiming institutional authority and suppressing them; the study_as_exercise and substitution readings coexist as excluded alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__performance_only, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
