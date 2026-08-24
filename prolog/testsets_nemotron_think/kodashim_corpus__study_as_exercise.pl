% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Study as Mitzvah Fulfillment
 *   domain: religious/rabbinic/commitment_system
 *
 * SUMMARY:
 *   In Rabbinic Judaism, the Kodashim corpus (the Talmudic tractates
 *   governing sacrificial law) presents a kernel contested across three
 *   readings. The study_as_exercise reading holds that engaging with these
 *   texts through rigorous intellectual-spiritual labor IS the performance of
 *   the mitzvah — the kernel is not archived or memorialized but actively
 *   occupied. This reading coordinates a global community of scholars and
 *   supporters around the practice of 'Torah lishma' (study for its own sake)
 *   as avodah (sacred service), maintaining cosmic order in the Temple's
 *   absence. Zero extractiveness: no party is deprived, no material transfer
 *   is compelled; the coordination is voluntary and the benefits (cosmic
 *   maintenance, communal continuity) are shared. The constraint is a rope —
 *   genuine coordination around a shared interpretive practice with minimal
 *   coercive overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.08).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Study as Mitzvah Fulfillment").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/rabbinic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '3b1685b0-e756-40d8-951a-2b124f544bed').
narrative_ontology:cs_kernel_codification('3b1685b0-e756-40d8-951a-2b124f544bed', formalized).
narrative_ontology:cs_authority_grounding('3b1685b0-e756-40d8-951a-2b124f544bed', lineage).
narrative_ontology:cs_interpretation_layer_present('3b1685b0-e756-40d8-951a-2b124f544bed').
narrative_ontology:cs_reading_relation('3b1685b0-e756-40d8-951a-2b124f544bed', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('3b1685b0-e756-40d8-951a-2b124f544bed', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('3b1685b0-e756-40d8-951a-2b124f544bed', foundational, study_constitutes_avodah).
narrative_ontology:cs_axiom_status(study_constitutes_avodah, holdable).
narrative_ontology:cs_axiom_grounding('3b1685b0-e756-40d8-951a-2b124f544bed', study_constitutes_avodah, deontological).
narrative_ontology:cs_reference_frame('3b1685b0-e756-40d8-951a-2b124f544bed', occupied_kodashim_kernel).
narrative_ontology:cs_drift_state('3b1685b0-e756-40d8-951a-2b124f544bed', contemporary, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('3b1685b0-e756-40d8-951a-2b124f544bed', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, religious_community).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, study_as_avodah).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_lishma).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, kernel_occupation_through_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in intensive study of Kodashim tractates as the fulfillment of sacrificial mitzvot; their intellectual-spiritual labor maintains cosmic order and occupies the kernel. The scholarly identity is fused with this practice — exit would mean abandoning the self-concept constituted through generations of avodah-through-study.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, torah_scholars, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, torah_scholars, agenda_setter).

% Benefits from the cosmic order maintained by scholarly engagement; supports yeshivot and kollelim materially and participates through communal reverence for the study enterprise. Exit is constrained by communal belonging and the lack of alternative frameworks for Temple-less cosmic maintenance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, religious_community, beneficiary,
    organized, generational, constrained, global).

% Hold that only physical sacrifice in a rebuilt Temple fulfills the mitzvot; Kodashim study is preparatory archival work awaiting messianic restoration. They are excluded from the study_as_exercise framework because their reading denies its core premise, yet they inhabit the same textual tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, performance_only_adherents, excluded,
    organized, generational, identity_locked, global).

% Hold that prayer and Torah study replaced sacrifice entirely; Kodashim is a memorial archive documenting a superseded system. They are excluded from the study_as_exercise framework because their reading treats the kernel as closed, not occupied.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, substitution_archive_adherents, excluded,
    organized, generational, identity_locked, global).

% Studies the interpretive dynamics of the Kodashim kernel across its three live readings; maps how each reading structures authority, identity, and continuity within Rabbinic Judaism without committing to any.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community of practice around the ongoing occupation of the sacrificial kernel through study, maintaining continuity of tradition and cosmic order without physical Temple — the shared interpretive practice of treating Kodashim learning as avodah (sacred service) binds scholars and community across diaspora and generations.
% TRANSFER_FUNCTION: Moves intellectual-spiritual labor from scholars into the maintenance of the kernel's occupied status; the community transfers material support (funding, honor, institutional infrastructure) to scholars, who return the maintenance of cosmic order. No material extraction occurs — the flow is reciprocity within a covenantal frame.
% ABSENT_VOICES: Secular critics who would deny the cosmic-order premise entirely, and historical actors (Second Temple priests, early Christians) who experienced the kernel before its rabbinic reoccupation. The excluded stakeholders (performance_only, substitution_archive) are present in the tradition but structurally excluded from this reading's framework — they would object to the claim that study IS the performance.
% DISAPPEARANCE_RATIONALE: If the study_as_exercise reading vanished overnight, the Rabbinic world would lose its primary framework for occupying the Kodashim kernel. The community would be forced to reorganize around either performance_only (awaiting Temple restoration, rendering current study preparatory) or substitution_archive (treating Kodashim as closed memorial), both of which would restructure yeshiva curricula, communal resource allocation, and the self-understanding of Torah scholarship.
% FOUNDING_PROBLEM: How to maintain the sacrificial system's cosmic function after the Temple's destruction (70 CE) when physical sacrifice became impossible, without conceding that the covenantal order had failed.
% FOUNDING_PROBLEM_CORROBORATION: Attested by the continuous history of rabbinic literature (Mishnah, Talmud, Rishonim, Acharonim) treating Kodashim study as avodah, by the institutional persistence of yeshivot dedicating central curriculum to Kodashim, and by contemporary poskim who rule that Kodashim study fulfills the mitzvah of Temple service in exile. Corroboration comes from outside the current beneficiary set: the historical record of communities that maintained this reading under persecution (e.g., medieval Ashkenaz, Yemenite Jewry) where material benefit was negligible.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the arrangement extracts nothing from participants — scholars choose the labor, community chooses the support, and the 'product' (cosmic order) is non-rival and non-excludable within the covenantal frame. Suppression is near-zero (0.08) because alternative readings (performance_only, substitution_archive) persist openly within the tradition; no enforcement machinery suppresses them. Theater ratio is low (0.1) because the study is genuinely demanding intellectual-spiritual work, not performative compliance. Accessibility collapse is moderate (0.35) because within the reading the kernel is fully occupied, yet alternative readings remain conceptually available. Resistance is low (0.12) because the reading faces no organized opposition — it is one live position among others.
 *
 * PERSPECTIVAL GAP:
 *   From the scholar's seat, the constraint is pure coordination (rope) — they experience the practice as voluntary, meaningful, and self-justifying. From the excluded readings' seats, the same textual corpus appears as either archived blueprint (performance_only) or memorial (substitution_archive) — they experience the study_as_exercise reading as a rival interpretation, not as an extractive structure. The engine computes this divergence from the structural data: zero extractiveness, no victims, voluntary participation.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah scholars are structural beneficiaries (d ~ 0.15): they receive communal honor, material support, and the intrinsic satisfaction of fulfilling the mitzvah through their chosen vocation. Their exit is identity_locked — the scholarly self is constituted through this practice. The religious community are beneficiaries (d ~ 0.3): they receive cosmic-order maintenance and communal continuity, exit constrained by belonging. The excluded readings' adherents are not targets of extraction — they simply inhabit incompatible frameworks. The analytical observer sits at d = 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining sacrificial cosmic function without the Temple) remains live — the Temple has not been rebuilt, and the covenantal order's maintenance is still the reading's stated purpose. No mandatrophy: the arrangement has not outlived its function. The reading continues to solve the coordination problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural profile change if the kernel_id/reading_id frame is rejected — i.e., if Kodashim is not a contested kernel but a single constraint with observer-dependent classification?',
    'Compare classification outcomes when the three readings are modeled as one constraint with measurement-basis variance vs. three separate constraints with distinct ε values. The ε-invariance principle predicts decomposition is required.',
    'If the kernel frame is rejected, the zero-extractiveness claim for this reading becomes unstable — the performance_only reading''s ''archival'' posture and substitution_archive''s ''memorial'' posture would be forced into the same metric surface, producing a composite ε > 0 and potentially reclassifying as tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the kernel/reading decomposition is structurally necessary or an analytic choice.').

omega_variable(
    cosmic_order_empirical_status,
    'Is the ''cosmic order maintenance'' claim empirically contingent (testable consequences in the world) or purely deontological (intrinsic duty regardless of outcomes)?',
    'Examine whether the tradition treats cosmic order as having observable correlates (agricultural fertility, political stability, divine presence) that could falsify the reading, or as a purely normative commitment.',
    'If empirically contingent, axiom_overriding drift becomes possible (evidence could foreclose the reading). If purely deontological, the reading is structurally insulated from empirical challenge — foreclosure only via internal doctrinal shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_order_empirical_status, conceptual, 'Epistemic status of the reading''s foundational benefit claim.').

omega_variable(
    scholar_identity_lock_mechanism,
    'Is the identity_locked exit for torah_scholars structural (institutional career path dependence, communal role embeddedness) or internalized (self-concept fusion where exit feels like spiritual death)?',
    'Post-exit trajectories of scholars who leave yeshiva life: if suppression/internal conflict persists after institutional exit, the lock is partially internalized; if exit is clean, the lock is primarily structural.',
    'If internalized, the effective directionality for scholars is more beneficiary-locked than structural derivation suggests — they cannot conceptualize exit even when structurally possible. This amplifies the reading''s coordination stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_identity_lock_mechanism, empirical, 'Mechanism of identity lock for the primary beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_as_exercise_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.08).
narrative_ontology:measurement(kodashim_study_as_exercise_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.09).
narrative_ontology:measurement(kodashim_study_as_exercise_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(kodashim_study_as_exercise_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(kodashim_study_as_exercise_tr_t1950, kodashim_corpus__study_as_exercise, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(kodashim_study_as_exercise_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(kodashim_study_as_exercise_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(kodashim_study_as_exercise_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(kodashim_study_as_exercise_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(kodashim_study_as_exercise_be_t1950, kodashim_corpus__study_as_exercise, base_extractiveness, 1950, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_as_exercise_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(kodashim_study_as_exercise_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.06).
narrative_ontology:measurement(kodashim_study_as_exercise_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(kodashim_study_as_exercise_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(kodashim_study_as_exercise_su_t1950, kodashim_corpus__study_as_exercise, suppression_requirement, 1950, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_corpus kernel. The study_as_exercise reading asserts kernel occupancy through intellectual engagement (ε=0, rope). The performance_only reading asserts kernel vacancy awaiting physical restoration (ε≈0.15, scaffold with messianic sunset). The substitution_archive reading asserts kernel closure and memorialization (ε≈0.05, piton — degraded coordination maintained theatrically). All three share the same textual corpus but instantiate different constraints with different ε, beneficiaries, and types. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
