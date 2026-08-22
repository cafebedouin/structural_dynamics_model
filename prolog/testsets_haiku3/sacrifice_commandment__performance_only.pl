% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment Performance-Only Reading: Suspension Without Temple
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The performance-only reading holds that the divine commandment to perform
 *   sacrifices remains obligatory but is suspended—not fulfilled or
 *   reinterpreted—because the Temple no longer exists. This reading emerged
 *   as the halakhic standard after 70 CE to preserve the law's normative
 *   authority while acknowledging its enforceability collapse. Under this
 *   reading, the commandment cannot be fulfilled in the present age or any
 *   foreseeable future; it remains a suspended obligation. The reading
 *   sustains institutional authority because law study becomes an indefinite,
 *   non-terminal activity: interpretation of unexecutable commandments can
 *   never be completed. This story instantiates the performance-only reading
 *   as a constraint, examining its structural operation over 1,900 years. The
 *   claim/metric gap is deliberate: the constraint is claimed as a snare (a
 *   reading doctrine that prioritizes suspension over alternatives), and the
 *   metrics describe high extractiveness and rising theater, modeling how a
 *   crisis-solution framework becomes institutionally entrenched while the
 *   original crisis's urgency recedes.
 *
 * KEY AGENTS:
 *   - Interpretive authority class (institutional agenda-setter): maintains performance-only as binding law, controls textual transmission, defines orthodoxy
 *   - Devoted students of law (moderate power, identity-locked): invest decades in studying unexecutable commandments; labor cannot achieve stated purpose
 *   - Lay practitioners (powerless, constrained): bear the burden of unfulfillable obligation without scholarly labor's compensatory knowledge-work
 *   - Alternative reading communities (organized/moderate, constrained/excluded): messianic advocates, study-as-performance scholars, archive-maintenance interpreters marginalized in official discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.79).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.71).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.79).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment Performance-Only Reading: Suspension Without Temple").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '84335d07-7f9f-452b-a13a-e7685b5b1972').
narrative_ontology:cs_kernel_codification('84335d07-7f9f-452b-a13a-e7685b5b1972', fixed_text).
narrative_ontology:cs_authority_grounding('84335d07-7f9f-452b-a13a-e7685b5b1972', lineage).
narrative_ontology:cs_interpretation_layer_present('84335d07-7f9f-452b-a13a-e7685b5b1972').
narrative_ontology:cs_reading_relation('84335d07-7f9f-452b-a13a-e7685b5b1972', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('84335d07-7f9f-452b-a13a-e7685b5b1972', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('84335d07-7f9f-452b-a13a-e7685b5b1972', foundational, performance_requirement_inherent).
narrative_ontology:cs_axiom_status(performance_requirement_inherent, holdable).
narrative_ontology:cs_axiom_grounding('84335d07-7f9f-452b-a13a-e7685b5b1972', performance_requirement_inherent, deontological).
narrative_ontology:cs_axiom('84335d07-7f9f-452b-a13a-e7685b5b1972', foundational, suspension_not_fulfillment_or_abrogation).
narrative_ontology:cs_axiom_status(suspension_not_fulfillment_or_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('84335d07-7f9f-452b-a13a-e7685b5b1972', suspension_not_fulfillment_or_abrogation, deontological).
narrative_ontology:cs_reference_frame('84335d07-7f9f-452b-a13a-e7685b5b1972', temple_destruction_enforceability_crisis).
narrative_ontology:cs_drift_state('84335d07-7f9f-452b-a13a-e7685b5b1972', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84335d07-7f9f-452b-a13a-e7685b5b1972', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, interpretive_authority_class).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, devoted_students_of_law).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic and scholarly institutions that set, defend, and transmit the performance-only reading as binding law. They control textual authority, determine which readings are orthodox, and maintain the reading's institutional dominance through credentialing, text transmission, and framing alternative readings as heterodox. The reading sustains their function: it makes law study indefinite, valuable, and non-terminal. They can exit by endorsing alternative readings if institutional pressure changes.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, interpretive_authority_class, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars and serious students who study sacrifice law with genuine intent to fulfill commandments. Under the performance-only reading, their decades of intellectual labor cannot achieve its stated purpose: the commandment remains suspended, unperformed, unfulfilled. They study with the hope that their interpretation contributes to a future they will not see (Temple restoration). Their exit is identity-locked: abandoning the study means abandoning identity as a faithful student of law. The override of d=0.88 reflects that although they have moderate power (scholarly credentials, textual knowledge), they are deeply targeted by the reading's indefinite deferral structure.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, devoted_students_of_law, payer,
    moderate, biographical, identity_locked, global).

% Ordinary community members who understand and attempt to observe commandments. They are told a fundamental obligation exists but is inaccessible in their present and any future they will see. They bear the psychological and spiritual weight of an unfulfillable commandment without the scholarly labor that gives students a sense of contribution. Their exit is constrained by communal identity and the embedded character of religious practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_practitioners, payer,
    powerless, biographical, constrained, global).

% Communities and movements oriented toward Temple reconstruction as an immediate practical and spiritual goal. They read the commandment as performable now through preparation and action. Their vision is structurally incompatible with the performance-only reading's indefinite suspension framework. They are systematically marginalized in official halakhic discourse, their readings treated as impractical or theologically misguided rather than engaged as serious alternatives.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_restoration_advocates, excluded,
    organized, civilizational, constrained, global).

% Scholars and communities who hold that intellectual engagement with the sacrifice commandment constitutes performance and fulfillment in the present age. They argue that study IS the sacrifice, that intellectual labor is the form the commandment takes when physical sacrifice is impossible. Their reading directly challenges the performance-only framework by declaring the commandment is not suspended but is being fulfilled through study. This position is treated as minority, heterodox, or logically indefensible within mainstream halakhic institutions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_advocates, excluded,
    moderate, biographical, constrained, global).

% Scholars who frame study as technical preservation and preparation for a future Temple reconstruction. They argue study is not present performance (contra study-as-performance) and not indefinite suspension (contra performance-only), but rather purposeful preparation for a specific future state. This reading gives study a defined endpoint and purpose. It is structurally displaced in contemporary halakhic discourse, marginalized as a third position that lacks the institutional support of the performance-only reading's dominance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, archive_maintenance_advocates, excluded,
    moderate, generational, constrained, global).

% Philologists, historians, and comparative scholars who examine how the performance-only reading emerged historically and operates structurally. They observe that the reading solved a specific crisis (post-70 CE enforceability collapse) but that the solution became institutionally entrenched in ways that make alternative interpretations difficult to take seriously. They do not advocate for any reading but measure how the readings compete.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, textual_tradition_keepers, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the post-Temple crisis by maintaining law's normativity despite enforceability collapse. It preserves the legal system's theoretical completeness by declaring obligations suspended (not voided, not reinterpreted) pending conditions (Temple reconstruction) the reading itself presupposes will eventually obtain.
% TRANSFER_FUNCTION: Diverts intellectual labor from practical law implementation toward interpretive mastery of suspended commandments. Moves decades of scholarly energy from achievable obligations to technical study of unexecutable acts. The flow of labor goes from devoted students to institutional authority structures that value and control the interpretation.
% ABSENT_VOICES: Messianic restoration advocates (who would argue the commandment is performable through practical Temple preparation), study-as-performance communities (who would argue study constitutes fulfillment), and archive-maintenance interpreters (who would argue study is preparation for a defined future) are excluded from mainstream halakhic authority. Their readings are treated as untenable or heterodox rather than engaged as serious interpretive competitors.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared and study-as-performance became normative, students would experience their intellectual labor as constituting fulfillment—removing the deferral problem and restructuring the entire psychological-spiritual terrain. If archive-maintenance became normative, study would have a defined endpoint and purpose (Temple restoration), changing how future-orientation functions. If the performance-only reading persisted but institutional enforcement softened, alternative readings could circulate without marginalization, and students could shift intellectual commitments without identity dissolution. The parties dispute whether the reading's disappearance would represent liberation (from indefinite deferral), abandonment (of sacred obligation), or clarification (of what the commandment actually requires in the present age).
% FOUNDING_PROBLEM: Post-70 CE Temple destruction rendered a fundamental category of law (sacrifice commandments) physically unexecutable. The performance-only reading emerged to preserve the commandments as binding obligations while acknowledging they cannot be performed, suspending them until Temple restoration.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the benefiting interpretive institution attest that the crisis was real and that the performance-only reading did emerge as a coherent halakhic response. However, historical evidence also shows that alternative readings (study-as-performance, archive-maintenance) emerged contemporaneously and have been defended by scholars throughout the intervening 1,900 years. The benefiting institution claims the crisis remains live (Temple still not rebuilt, commandments still suspended); scholars outside the institution argue the crisis's urgency diminished dramatically after 500 CE when messianic expectations shifted from imminent to indefinite, and that the performance-only reading persists not because the founding problem remains pressing but because institutional structures came to depend on the reading's continuance.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at crisis point (70 CE) to 0.79 in the present (2024), modeled as projection-to-observed. The crisis initially created genuine coordination value: the performance-only reading solved the urgent problem of maintaining law's normativity when enforcement became impossible. Over centuries, as the immediacy of Temple reconstruction faded, the extractive character of the reading crystallized: study of sacrifices became an institutional end-in-itself, the commandment's permanent suspension the invisible premise justifying indefinite interpretive labor. Theater rises from 0.25 to 0.68, reflecting that enforcement machinery increasingly maintains the reading's dominance (marginalizing alternatives) rather than illuminating the commandment itself. Suppression requirement rises from 0.42 to 0.71 because the interpretive authority must actively defend the performance-only reading against alternative readings that have emerged and have internal coherence: study-as-performance treats study as fulfillment (removing the deferral problem), archive-maintenance frames study as preparation (giving it a defined purpose). The measurement series models a constraint that solved a real problem but whose solution became extractive precisely because the problem's urgency dissipated and institutional structures came to depend on the solution's persistence.
 *
 * PERSPECTIVAL GAP:
 *   From the interpretive authority seat: the performance-only reading is the only intellectually coherent halakhic position given Temple's absence; study preserves law and prepares for future restoration. The reading's dominance reflects its logical strength, not enforcement. From the devoted student's seat: the reading is structurally extractive because decades of intellectual labor produce no fulfillment, only interpretation of suspended obligations. The dominance of this reading (over study-as-performance alternatives) requires active enforcement through institutional control of textual authority and credentialing. From the lay practitioner's seat: the reading imposes a spiritual burden (unfulfillable commandment) that scholars can partially escape through knowledge-work, but which ordinary community members bear without compensation. The engine computes these divergent positions from the stakeholder power/exit/role data; the claim/metric independence rule ensures neither framing predetermines the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Interpretive authority class benefits from the performance-only reading—it sustains their function, makes their interpretive labor indefinite and valued, and prevents alternative readings from achieving parity in institutional credibility. They have arbitrage-grade exit options (they can shift which reading they defend if institutional pressure changes, unlike identity-locked students). Devoted students are targeted by the reading: their intellectual labor is directed toward unexecutable commandments, their identity as faithful students requires accepting indefinite deferral, their exit (abandoning study) triggers identity dissolution. Lay practitioners are also targeted: they bear the psychological weight of unfulfillable commandment without scholarly labor's mitigating purpose. The exclusion of alternative reading communities (messianic advocates, study-as-performance scholars) is the enforcement mechanism itself: institutional authority marginalizes these readings, prevents them from achieving textual-transmission parity, and treats them as heterodox rather than engaging their arguments.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading is NOT a mountain—it is a human interpretive choice grounded in textual tradition and institutional practice, not a natural law. The reading solved a real post-70 CE crisis (commandments unexecutable, law's authority undermined), but that crisis's urgency has dissipated over 1,900 years. The founding problem is now contested: historians and alternative scholars argue the crisis was a one-time problem requiring a one-time interpretive fix, not a permanent condition justifying indefinite suspension. The reading persists because institutional structures (credentialing, textual authority, what counts as orthodox) came to depend on it, not because the original problem remains pressing. This is mandatrophy: a reading whose founding problem (Temple destruction makes sacrifice unexecutable) is not live in its original form (no one expects the crisis to resolve before 2024), whose alternative readings (study-as-performance, archive-maintenance) offer coherent solutions without indefinite suspension, but which persists because institutional authority benefits from its dominance. The theater ratio (rising from 0.25 to 0.68) models this: increasing share of enforcement effort defends the reading's monopoly rather than illuminating the commandment itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (Temple destruction rendering sacrifices unexecutable) live in 2024, or is it a historical crisis whose urgency has dissipated?',
    'Historical analysis of when messianic expectations shifted from imminent (70–200 CE) to indefinite (500 CE onward) to absent from practical institutional planning (modern era). If immediacy of Temple reconstruction faded, the crisis no longer justifies the suspension framework.',
    'If the founding problem is dead, the performance-only reading is mandatrophy: it solved a real crisis but now persists by institutional inertia. If the problem is live (messianic restoration remains theologically normative), the reading''s indefinite suspension is justified. If contested, the three readings coexist as live alternatives without one achieving clarity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the Temple destruction crisis remains the active problem the performance-only reading was designed to solve.').

omega_variable(
    alternative_readings_coherence,
    'Are the study-as-performance and archive-maintenance readings intellectually coherent halakhic positions, or are they logically defeated by the performance-only framework?',
    'Comparative textual analysis of how each reading engages the relevant sources. Do study-as-performance and archive-maintenance advocates have defensible exegetical grounds, or do they rest on special pleading and textual manipulation?',
    'If alternative readings are coherent, the performance-only reading''s institutional dominance is enforced rather than justified by logical superiority. If alternatives are logically weak, the dominance reflects intellectual merit and requires no enforcement beyond standard authority structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_coherence, conceptual, 'Whether the performance-only reading''s dominance reflects logic or enforcement.').

omega_variable(
    identity_locked_exit_mechanism,
    'What portion of devoted students'' identity-locked status derives from theological commitment (internalized obligation) vs. institutional structure (credentialing, community belonging)?',
    'Post-exit analysis: if students who shift to alternative readings (study-as-performance, archive-maintenance) experience persistent identity disruption, the lock is partially internalized; if disruption resolves after leaving institutional authority structures, the lock is primarily structural.',
    'If primarily internalized, the constraint''s suppression is higher than the structural measure suggests—students carry the deferral burden internally. If primarily structural, institutional reform could rapidly shift which reading predominates without individual identity restructuring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Internalized vs. structural components of the devoted student''s identity lock.').

omega_variable(
    sibling_reading_relationship_uncertainty,
    'Do study-as-performance and archive-maintenance readings foreclose the performance-only reading (logically incompatible core premises), coexist with it (different communities hold both simultaneously), or influence it (create pressure without foreclosure)?',
    'Examination of whether a single halakhic authority can coherently hold all three readings simultaneously or whether holding one requires denying the others. If authorities can partition them by context or community, coexistence obtains; if the core premises directly contradict, foreclosure obtains.',
    'If foreclosure: one reading will eventually dominate and the others will be formally abandoned (determining which reading survives becomes a stakes question). If coexistence: the three readings persist as live positions in different communities, and their institutional separation prevents one from definitively defeating the others. If influence: one reading shapes the conditions for others without making them impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relationship_uncertainty, conceptual, 'Logical relationship among the three sibling readings of the sacrifice commandment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__performance_only, theater_ratio, 70, 0.25).
narrative_ontology:measurement_basis(sacr_tr_t70, projected).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_commandment__performance_only, theater_ratio, 200, 0.35).
narrative_ontology:measurement_basis(sacr_tr_t200, projected).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.48).
narrative_ontology:measurement_basis(sacr_tr_t500, projected).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.58).
narrative_ontology:measurement_basis(sacr_tr_t1000, projected).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.64).
narrative_ontology:measurement_basis(sacr_tr_t1500, projected).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_commandment__performance_only, theater_ratio, 2024, 0.68).
narrative_ontology:measurement_basis(sacr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__performance_only, base_extractiveness, 70, 0.35).
narrative_ontology:measurement_basis(sacr_be_t70, projected).
narrative_ontology:measurement(sacr_be_t200, sacrifice_commandment__performance_only, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(sacr_be_t200, projected).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.55).
narrative_ontology:measurement_basis(sacr_be_t500, projected).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement_basis(sacr_be_t1000, projected).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement_basis(sacr_be_t1500, projected).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_commandment__performance_only, base_extractiveness, 2024, 0.79).
narrative_ontology:measurement_basis(sacr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_commandment__performance_only, suppression_requirement, 70, 0.42).
narrative_ontology:measurement_basis(sacr_su_t70, projected).
narrative_ontology:measurement(sacr_su_t200, sacrifice_commandment__performance_only, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(sacr_su_t200, projected).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.58).
narrative_ontology:measurement_basis(sacr_su_t500, projected).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement_basis(sacr_su_t1000, projected).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.69).
narrative_ontology:measurement_basis(sacr_su_t1500, projected).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_commandment__performance_only, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(sacr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.15).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three readings: performance_only (this story) asserts commandment requires execution and is suspended; study_as_performance asserts intellectual engagement fulfills the commandment; archive_maintenance asserts study preserves knowledge for future Temple restoration. Each reading has different epsilon values, different victim/beneficiary structures, and different claim types. The three stories share the kernel (normative force of the sacrifice commandment) but differ fundamentally on what counts as fulfillment in a post-Temple context. The ε-invariance principle requires separate stories: the performance-only reading extracts indefinite scholarly labor from unexecutable commandment study; the study-as-performance reading de-extracts by declaring study constitutes performance; the archive-maintenance reading reframes study as preparation (giving it defined purpose, reducing extraction). All three readings are linked via network.affects_constraints to enable family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
