% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Kodashim Corpus â Performance-Only Reading (Messianic Husk)
 *   domain: religious_studies/rabbinic_judaism
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only reading of the
 *   kodashim_corpus kernel in Rabbinic Judaism. Under this reading, the
 *   Mishnah and Talmud tractates concerning sacrifices (Kodashim) are treated
 *   not as presently operative law, nor as superseded memorial, but as an
 *   archived blueprint whose study prepares for messianic restoration of the
 *   Temple cult. The reading extracts present-day institutional legitimacy
 *   and student devotion by deferring all actualization to an indefinitely
 *   postponed future. Key agents include the messianic-preparation
 *   institutions that administer this framing and the study communities whose
 *   devotion is structurally redirected toward an unrealizable performance
 *   horizon. This is a kernel reading: sibling readings (study_as_exercise,
 *   substitution_archive) instantiate structurally distinct constraints from
 *   the same textual kernel.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions (agenda_setter/beneficiary): Administers the framing and collects legitimacy
 *   - devoted_study_communities (payer): Devote cognitive and ritual labor to a deferred performance horizon
 *   - study_as_exercise_adherents (excluded): Marginalized reading that treats study as current fulfillment
 *   - substitution_reading_adherents (excluded): Marginalized reading that treats corpus as superseded memorial
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.82).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.75).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus â Performance-Only Reading (Messianic Husk)").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '164166c5-d861-4e8f-9233-0e3d1db13101').
narrative_ontology:cs_kernel_codification('164166c5-d861-4e8f-9233-0e3d1db13101', fixed_text).
narrative_ontology:cs_authority_grounding('164166c5-d861-4e8f-9233-0e3d1db13101', lineage).
narrative_ontology:cs_interpretation_layer_present('164166c5-d861-4e8f-9233-0e3d1db13101').
narrative_ontology:cs_reading_relation('164166c5-d861-4e8f-9233-0e3d1db13101', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('164166c5-d861-4e8f-9233-0e3d1db13101', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('164166c5-d861-4e8f-9233-0e3d1db13101', foundational, study_is_preparatory_not_performative).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_performative, holdable).
narrative_ontology:cs_axiom_grounding('164166c5-d861-4e8f-9233-0e3d1db13101', study_is_preparatory_not_performative, theological).
narrative_ontology:cs_axiom('164166c5-d861-4e8f-9233-0e3d1db13101', foundational, physical_sacrifice_messianic_necessity).
narrative_ontology:cs_axiom_status(physical_sacrifice_messianic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('164166c5-d861-4e8f-9233-0e3d1db13101', physical_sacrifice_messianic_necessity, theological).
narrative_ontology:cs_reference_frame('164166c5-d861-4e8f-9233-0e3d1db13101', temple_cult_operational_state).
narrative_ontology:cs_drift_state('164166c5-d861-4e8f-9233-0e3d1db13101', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('164166c5-d861-4e8f-9233-0e3d1db13101', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_study_communities).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, temple_cult_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers curricula and liturgical framing that treats the Kodashim corpus as an active blueprint whose study prepares for messianic Temple restoration. Collects institutional legitimacy, student devotion, and communal authority by maintaining the kernel as unresolved and awaiting future performance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary).

% Devote significant cognitive, emotional, and ritual resources to studying sacrificial law as if it were presently operative. Their devotion is structurally redirected toward a performance horizon that the reading itself admits is unrealizable, extracting present-day commitment without present-day fulfillment.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_study_communities, payer,
    moderate, biographical, identity_locked, global).

% Hold that prayer and Torah study have already superseded sacrifice; are marginalized or treated as theologically deficient within the performance-only framing because they treat the kernel as closed rather than open.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, substitution_reading_adherents, excluded,
    moderate, biographical, constrained, global).

% Hold that intellectual-spiritual engagement with sacrifice law is itself the fulfillment of the mitzvah; are treated by the performance-only reading as engaging in preparatory practice rather than actual performance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, study_as_exercise_adherents, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed knowledge of sacrificial procedure across generations so that it can be reactivated upon messianic Temple reconstruction.
% TRANSFER_FUNCTION: Moves present-day devotion, cognitive labor, and institutional allegiance from study communities to messianic-preparation institutions, justified by reference to a future performance that the present arrangement cannot deliver.
% ABSENT_VOICES: Adherents of substitution and study-as-exercise readings are structurally sidelined; their theological legitimacy is downgraded because admitting their validity would collapse the future-performance horizon that justifies present extraction.
% DISAPPEARANCE_RATIONALE: If the performance-only framing vanished, study communities would reallocate devotion to presently operative mitzvot or alternative readings; messianic-preparation institutions would lose the distinctive legitimacy that derives from stewarding an unredeemed kernel.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a rupture in ritual continuity; the corpus needed a status that preserved its normative force across the interim until restoration.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion and critical Talmudists outside the messianic-preparation institutions attest that the text-preservation problem was solved by redaction; the continued insistence on future performance is a theological overlay rather than a textual necessity.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the arrangement transfers present devotion to an institutionally administered future that cannot be cashed; suppression (0.75) is required to maintain the performance-only frame against substitution and study-as-exercise alternatives; theater_ratio is high (0.70) because the daily study of sacrificial law in the absence of a Temple is predominantly performative maintenance of an institutional identity rather than functional preparation for an imminent restoration. Accessibility_collapse (0.65) reflects that within this reading, alternatives appear theologically deficient; resistance (0.55) is generated by the competing readings and by modern historical consciousness. Measurements share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the constraint is legitimate preservation of a sacred trust and necessary preparation for redemption. From the devoted study community seat, the same structure appears as a binding obligation whose fruits are perpetually deferred. The engine computes this divergence from beneficiary/victim declarations and exit modulations: the institution collects legitimacy while the community pays devotion.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions are structural beneficiaries: they derive authority, enrollment, and communal standing from stewarding the unredeemed kernel. Their exit options (arbitrage) allow them to reinterpret but not to abandon the frame without cost. Devoted study communities are structural targets: their devotion and labor are the extracted resource, and their exit is identity_lockedâtheir religious self-concept is constituted through participation in this study. The excluded sibling-reading communities face constrained exit: they exist but are treated as theologically marginal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving sacrificial knowledge across the rupture of destructionâwas solved by textual redaction centuries ago. The constraint persists not because the problem is live, but because the institutional legitimacy derived from administering the deferred kernel has become self-sustaining. This is a clear mandatrophy: the original coordination function (preservation) is complete, but the extraction function (legitimacy through deferral) remains. Classifying it as snare prevents misreading the preserved text as still-coordinating present sacrifice; classifying it as piton would miss the concentrated beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_location,
    'This constraint is the performance_only reading of the kodashim_corpus kernel. Would adopting this reading in a single institutional framework logically foreclose the study_as_exercise and substitution_archive readings, or do they coexist across different communities?',
    'Comparative sociology of halakhic communities: if a single yeshiva can simultaneously train students for performance-only and study-as-exercise without contradiction, the readings are coexistent; if the same curriculum must choose one axiom, foreclosure is present.',
    'If foreclosed, the constraint''s snare classification intensifies because the reading actively suppresses siblings; if coexistent, the extraction is localized to communities that adopt it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Whether performance_only forecloses sibling readings in a single framework').

omega_variable(
    messianic_deferral_extraction,
    'Does the indefinite deferral of the messianic performance horizon make the extraction from study communities structurally inevitable, or is the horizon empirically contestable within the tradition?',
    'Historical analysis of rabbinic responses to failed messianic movements (e.g., Sabbatianism) to see if the horizon was ever retracted or reinterpreted when empirical conditions contradicted it.',
    'If the horizon is immune to empirical challenge, the constraint operates as a closed extraction loop; if past retraction occurred, the reading is vulnerable to axiom overriding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_deferral_extraction, empirical, 'Whether messianic deferral is empirically contestable or a closed loop').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional curriculum control) or internalized (identity-locked devotion to messianic frames)?',
    'Post-exit trajectory: study communities that leave performance-only institutions and join substitution-advocating communitiesâif suppression persists (self-censorship), it is internalized.',
    'If internalized, effective suppression exceeds structural measure; the victim carries the constraint after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t300, kodashim_corpus__performance_only, theater_ratio, 300, 0.35).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__performance_only, theater_ratio, 600, 0.45).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__performance_only, theater_ratio, 1000, 0.6).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__performance_only, theater_ratio, 1200, 0.68).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__performance_only, theater_ratio, 1500, 0.7).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__performance_only, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__performance_only, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__performance_only, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__performance_only, base_extractiveness, 1200, 0.75).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__performance_only, base_extractiveness, 1500, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(koda_su_t300, kodashim_corpus__performance_only, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(koda_su_t600, kodashim_corpus__performance_only, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__performance_only, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__performance_only, suppression_requirement, 1200, 0.73).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__performance_only, suppression_requirement, 1500, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
