% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Kodashim Obligation
 *   domain: religious/jurisprudential/textual-preservation
 *
 * SUMMARY:
 *   This story authors the study_as_performance reading of the
 *   kodashim_obligation kernel as a clean, epsilon-invariant constraint: the
 *   claim that studying the laws of the sacrifices enacts the sacrifice's
 *   cosmic function itself, such that the Temple's physical absence is
 *   irrelevant to the law's spiritual efficacy. The epsilon referent is the
 *   standing arrangement under contest — the obligation and practice of
 *   Kodashim study — assessed by this reading's own lights, which hold the
 *   arrangement fully efficacious and non-extractive: study is not a
 *   substitute FOR the performance but IS the performance. No sibling reading
 *   is folded into the metrics; the preparation and archive readings are
 *   separate constraints linked through the network. The claim/metric gap is
 *   deliberate where it exists: the constraint is claimed as mountain (an
 *   eternal, enforcement-independent structure of the covenant's order) and
 *   the metrics are authored from the same reading's descriptive lights —
 *   near-zero on every extractive axis. Where an outside observer would score
 *   theater high, that observer holds a different reading of the kernel and
 *   is carried in the omegas, not in this file's numbers. KEY AGENTS (by
 *   structural relationship): - rabbinic_academy_tradition: agenda-setting
 *   administrator (institutional / identity_locked) — transmits and
 *   administers the study curriculum; its warrant is the transmission itself
 *   - obligated_study_community: performing participants (organized /
 *   identity_locked) — their study is the enactment; benefit and act are
 *   fused - women_in_traditional_communities: excluded seat (moderate /
 *   constrained) — exempt from the obligation, outside the conversation -
 *   academic_observers_of_rabbinics: analytical observer (analytical /
 *   analytical) — sees the structure from outside the frame, verifies nothing
 *   of the cosmic function
 *
 * KEY AGENTS:
 *   - rabbinic_academy_tradition: agenda-setting administrator (institutional / identity_locked) — defines what counts as Kodashim study, ordains teachers, maintains the continuous curriculum; its authority is the chain of transmission it administers
 *   - obligated_study_community: performing participants (organized / identity_locked) — the men obligated in the traditional frame; their study enacts the service, and the good they receive is the act itself, so no net cost is borne
 *   - women_in_traditional_communities: excluded seat (moderate / constrained) — exempt from the commanded obligation in the traditional frame; present in the communities, absent from the arrangement's conversation about who enacts the service
 *   - academic_observers_of_rabbinics: analytical observer (analytical / analytical) — historians and scholars of religion describing the arrangement's formation from outside the covenantal economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study-as-Performance Reading of the Kodashim Obligation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/jurisprudential/textual-preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '79d2cd62-7589-4159-ade9-9df7ea78b28c').
narrative_ontology:cs_kernel_codification('79d2cd62-7589-4159-ade9-9df7ea78b28c', fixed_text).
narrative_ontology:cs_authority_grounding('79d2cd62-7589-4159-ade9-9df7ea78b28c', lineage).
narrative_ontology:cs_interpretation_layer_present('79d2cd62-7589-4159-ade9-9df7ea78b28c').
narrative_ontology:cs_reading_relation('79d2cd62-7589-4159-ade9-9df7ea78b28c', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('79d2cd62-7589-4159-ade9-9df7ea78b28c', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('79d2cd62-7589-4159-ade9-9df7ea78b28c', foundational, study_enacts_sacrificial_function).
narrative_ontology:cs_axiom_status(study_enacts_sacrificial_function, holdable).
narrative_ontology:cs_axiom_grounding('79d2cd62-7589-4159-ade9-9df7ea78b28c', study_enacts_sacrificial_function, theological).
narrative_ontology:cs_axiom('79d2cd62-7589-4159-ade9-9df7ea78b28c', foundational, temple_absence_no_bar_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_no_bar_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('79d2cd62-7589-4159-ade9-9df7ea78b28c', temple_absence_no_bar_to_efficacy, theological).
narrative_ontology:cs_reference_frame('79d2cd62-7589-4159-ade9-9df7ea78b28c', sinaitic_eternal_enactment).
narrative_ontology:cs_drift_state('79d2cd62-7589-4159-ade9-9df7ea78b28c', contemporary_exile_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('79d2cd62-7589-4159-ade9-9df7ea78b28c', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.03: within the reading's lights no one pays — the time given to study is counted by the frame itself as the covenantal good received, so the residual epsilon covers only the trivial opportunity cost the reading reclassifies as gain. Suppression 0.05: the reading holds the obligation binds by command rather than coercion — it would persist whether or not anyone enforced it, which is the mountain property — and no enforcement machinery exists; the small residual reflects communal expectation, not coercion. Theater_ratio 0.05: within the frame study is the real function, not a stand-in; the small residual reflects the consolatory framing present in early sources before the equivalence doctrine fully consolidated (visible as the gentle decline in the theater series from 0.08 at 70 CE). Accessibility_collapse 0.88: once the equivalence is granted, alternatives collapse — no other enactment is available in exile, and the reading holds none is needed. Resistance 0.06: the practice meets no active resistance within the tradition; the kernel contest is interpretive, not resistance to the practice. All series run on one shared time grid (8 points, both metrics at every point); the flat trajectories ARE the data — the reading's claim is precisely stability, and the absence of extraction accumulation across nineteen centuries is what a genuine non-extractive structure should show. Dial-set residue note: the performing-participant seat has no exact role in the stakeholder dial (the documented ritual_operator residue class); it is seated as beneficiary with the act/good fusion described in the situation field rather than classified there. Identity-lock dynamics: the academies' lock is institutional (the institution has become its transmission function — setting the curriculum aside dissolves its warrant); the students' lock is covenantal and relational (self-concept constituted through the practice). Suppression is structural only in the mildest sense and is not internalized-coercive; no suppression-mechanism ambiguity omega is required at this magnitude, and the exclusion of the women's seat is carried as an excluded stakeholder plus absent_voices, not as suppression.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence: the academy seat experiences the arrangement as the tradition it administers — its identity and authority are the transmission, so the arrangement is indistinguishable from its own continuity. The student seat experiences duty and good as one act: there is no position from which it pays. The excluded seat experiences a conversation it is structurally outside, with the exemption's timing-rule logic rather than any evaluation of its capacity. The observer seat sees a frame-relative structure: a nineteen-century practice whose claimed cosmic function admits no external verification. Same-level dynamics: the obligated community and the excluded women sit at comparable communal power with differentiated obligation status — the arrangement's own timing-rule logic, not global standing, differentiates their positions. The engine computes per-seat classifications from these structural positions; with epsilon near zero every seat should compute non-extractive, while the exit-option differences (identity_locked vs. constrained vs. analytical) differentiate the seats' effective positions without producing extraction anywhere.
 *
 * DIRECTIONALITY LOGIC:
 *   No real-actor beneficiaries and no victims are declared — deliberately. The reading's beneficiary is the covenantal/cosmic order itself, which is a vindicated proposition (cosmic_sustenance_through_sacrificial_service), not a seat: a doctrine collects no rents and must not be listed under beneficiaries. The directionality derivation therefore runs on canonical fallbacks; with epsilon 0.03 and suppression 0.05, effective extraction is negligible at every seat regardless of power atom. The student seat sits nearest the beneficiary end — the arrangement's yield is its own enactment's fruit — and the academy seat's agenda-setting role confers administration, not capture. The structural absence of any seat at the target end IS the reading's claim: an arrangement from which no one is extracted. Receipt surface: gain_flow is authored as diffuse as an affirmative checked claim — every named seat was examined and none captures the arrangement's yield; what the frame describes as received accrues to the covenant's divine party, which holds no seat. fixing_cost is prohibitive: no seat can remove a divinely commanded obligation (removal is category-unavailable, not merely expensive), and within the frame no seat would benefit from removal. Note on the receipt cell: prohibitive-plus-diffuse is the cell the piton path reads, but the piton signature requires inertial maintenance and performative substitution — theater 0.05 and a living, actively-renewed practice are the opposite profile; the cell here is mountain-trivial (nothing to fix, no fixer exists) rather than piton-signaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a service commanded eternally became physically unperformable in 70 CE — is live within the frame: the exile persists, so the enactment-need persists, and no mandate has outlived its function. The classification prevents mislabeling in both directions. Read with archive-reading lights, this arrangement would score as a piton: performance without verifiable function, maintained by inertia and identity. Read with preparation-reading lights, it would score as a scaffold-shaped waiting arrangement whose justification is a transition (restoration) it does not itself deliver. This story authors the performance reading's own lights — live function, no transition pending, nothing atrophied — and routes the contest to the kernel_reading_contest and frame_relative_classification omegas, so the corpus can measure the divergence between readings rather than averaging it into hedged metrics. The flat temporal series is the mandatrophy-relevant evidence: no extraction accumulation (T17-style drift absent), no theater growth, no enforcement ratchet — a structure that is what it says it is, for as long as the frame that constitutes it holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the study obligation, as this reading holds it, an eternal structural feature of the covenant''s order — given at Sinai, efficacious without enforcement — or a rabbinic construction that presents itself as natural law?',
    'Internal textual analysis of how the tradition grounds the equivalence doctrine (Sinaitic warrant vs. rabbinic enactment), comparison with how adjacent obligations were formed, and the frame-external analysis the sibling archive reading performs on the same material.',
    'If constructed, the arrangement re-enters the human-constraint economy: coordination and maintenance costs appear, the enforcement-independence claim becomes a presentation rather than a structure, and the mountain profile gives way to a profile with real-actor beneficiaries and maintenance burdens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, conceptual, 'Whether the obligation''s claimed naturality is structural or presented.').

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (study_as_performance) of the kodashim_obligation kernel; the sibling readings study_as_preparation and study_as_archive are separate constraints. Which reading is structurally correct, and what changes under each sibling?',
    'Analysis of the shared text-base (the Talmudic equivalence dicta and their reception history), of what each reading makes structurally necessary (performance: nothing physical; preparation: eventual messianic restoration; archive: nothing), and of which reading the tradition''s living practice actually enacts.',
    'Under the preparation reading, Temple restoration becomes structurally necessary and study carries preparatory cost without enactment benefit, raising the extraction profile for waiting generations; under the archive reading, the obligation dissolves into historical identity-maintenance and this constraint ceases to bind at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are other files.').

omega_variable(
    frame_relative_classification,
    'The reading''s central claim — that study enacts a cosmic function — is verifiable only within the tradition''s metaphysical commitments. Does the arrangement''s classification hold outside the frame, or is the profile authored here frame-indexed?',
    'Not resolvable by data: resolvable only by adopting or rejecting the frame. The corpus can record the divergence — an observer holding archive-reading lights scores the same practice as performance-without-verifiable-function (high theater), while this reading''s own lights score theater near zero.',
    'Outside the frame the arrangement computes as theatrical maintenance of a function no observer can verify, with piton-flavored signals; inside the frame it computes as a live, load-bearing, non-extractive structure. The classification is a property of the reading; this story authors the reading''s own lights and leaves the boundary explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frame_relative_classification, conceptual, 'Frame-relativity of the arrangement''s assessment; epsilon is stable within the reading.').

omega_variable(
    enactment_scope_elite_vs_universal,
    'Does the cosmic enactment require the whole obligated community''s study, or is it satisfied by the scholarly elite''s continuous Kodashim curriculum?',
    'Close reading of the halakhic sources on the scope of the study obligation, and of communal practice: whether popular study cycles through the sacrificial orders are framed as enactment-participation or as devotion supererogatory to an elite performance that suffices.',
    'If elite-sufficient, the arrangement runs through a scholarly gate and the community''s role shifts from enactment toward support, changing the student seat''s position; if universal, the arrangement is participatory with no mediation structure and the zero-extraction profile is stable across the whole obligated population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enactment_scope_elite_vs_universal, conceptual, 'Whether enactment is universal-participatory or elite-mediated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_performance_reading_tr_t70, kodashim_obligation__study_as_performance, theater_ratio, 70, 0.08).
narrative_ontology:measurement(kodashim_performance_reading_tr_t250, kodashim_obligation__study_as_performance, theater_ratio, 250, 0.07).
narrative_ontology:measurement(kodashim_performance_reading_tr_t500, kodashim_obligation__study_as_performance, theater_ratio, 500, 0.06).
narrative_ontology:measurement(kodashim_performance_reading_tr_t1000, kodashim_obligation__study_as_performance, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(kodashim_performance_reading_tr_t1550, kodashim_obligation__study_as_performance, theater_ratio, 1550, 0.05).
narrative_ontology:measurement(kodashim_performance_reading_tr_t1800, kodashim_obligation__study_as_performance, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(kodashim_performance_reading_tr_t1945, kodashim_obligation__study_as_performance, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(kodashim_performance_reading_tr_t2025, kodashim_obligation__study_as_performance, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(kodashim_performance_reading_be_t70, kodashim_obligation__study_as_performance, base_extractiveness, 70, 0.04).
narrative_ontology:measurement(kodashim_performance_reading_be_t250, kodashim_obligation__study_as_performance, base_extractiveness, 250, 0.04).
narrative_ontology:measurement(kodashim_performance_reading_be_t500, kodashim_obligation__study_as_performance, base_extractiveness, 500, 0.03).
narrative_ontology:measurement(kodashim_performance_reading_be_t1000, kodashim_obligation__study_as_performance, base_extractiveness, 1000, 0.03).
narrative_ontology:measurement(kodashim_performance_reading_be_t1550, kodashim_obligation__study_as_performance, base_extractiveness, 1550, 0.03).
narrative_ontology:measurement(kodashim_performance_reading_be_t1800, kodashim_obligation__study_as_performance, base_extractiveness, 1800, 0.03).
narrative_ontology:measurement(kodashim_performance_reading_be_t1945, kodashim_obligation__study_as_performance, base_extractiveness, 1945, 0.03).
narrative_ontology:measurement(kodashim_performance_reading_be_t2025, kodashim_obligation__study_as_performance, base_extractiveness, 2025, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% The colloquial label 'the obligation to study Kodashim' decomposes into three structurally distinct constraints — the kodashim_obligation kernel family. This story instantiates study_as_performance: study enacts the sacrifice's cosmic function, the Temple's physical absence is no bar to efficacy, epsilon is near zero, no real-actor beneficiaries or victims exist, and Temple restoration is structurally unnecessary. The sibling study_as_preparation holds the law binding but unperformable, with study preserving technical knowledge for restoration (restoration structurally necessary); the sibling study_as_archive holds the system defunct and study to be historical identity-maintenance with no binding force. All three interpret the same text-base — the Talmudic equivalence dicta (Menahot 110a; Kiddushin 40b) — which functions as the upstream material each reading cites. Performance and preparation coexist within single traditional frameworks (enactment-now plus preservation-for-then are compatible functions); archive's core premise (no obligation, defunct system) contradicts this reading's core premise such that no single framework holds both. Each sibling is authored as its own story with its own epsilon and stakeholder structure; this file links them via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
