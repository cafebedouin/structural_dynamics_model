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
 *   human_readable: Kodashim Study as Cosmic Performance
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the kodashim obligation—the
 *   contested Jewish legal requirement to study sacrificial law despite the
 *   Temple's destruction (70 CE) and continued absence. The
 *   study_as_performance reading holds that studying the legal corpus enacts
 *   the cosmic function of sacrifice itself: the performance is textual, not
 *   physical; the Temple's material absence is irrelevant to the law's
 *   spiritual efficacy. This reading coexists with study_as_archive (study
 *   preserves historical memory and identity without cosmic function claims)
 *   and study_as_preparation (study maintains technical knowledge for
 *   messianic restoration). The three readings share a kernel—the standing
 *   obligation to study kodashim—but instantiate structurally different
 *   constraints with different ε values, beneficiary structures, and
 *   persistence mechanisms. This story generates the study_as_performance
 *   constraint alone; its siblings are separate stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Jewish scholars and communities: holders of the tradition, interpreters of binding obligation
 *   - Cosmic order: the beneficiary in this reading (metaphysical claim, not a human actor)
 *   - Torah and halakhic authority: the framework grounding the performance claim
 *   - Competing readings (study_as_archive, study_as_preparation): coexisting institutional positions within tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/legal/textual").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'e805ab01-1c65-4420-ab2d-c893ef9c6afe').
narrative_ontology:cs_kernel_codification('e805ab01-1c65-4420-ab2d-c893ef9c6afe', fixed_text).
narrative_ontology:cs_authority_grounding('e805ab01-1c65-4420-ab2d-c893ef9c6afe', lineage).
narrative_ontology:cs_interpretation_layer_present('e805ab01-1c65-4420-ab2d-c893ef9c6afe').
narrative_ontology:cs_reading_relation('e805ab01-1c65-4420-ab2d-c893ef9c6afe', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_reading_relation('e805ab01-1c65-4420-ab2d-c893ef9c6afe', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('e805ab01-1c65-4420-ab2d-c893ef9c6afe', foundational, study_enacts_sacrifice).
narrative_ontology:cs_axiom_status(study_enacts_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('e805ab01-1c65-4420-ab2d-c893ef9c6afe', study_enacts_sacrifice, theological).
narrative_ontology:cs_axiom('e805ab01-1c65-4420-ab2d-c893ef9c6afe', foundational, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('e805ab01-1c65-4420-ab2d-c893ef9c6afe', temple_absence_irrelevant_to_efficacy, theological).
narrative_ontology:cs_reference_frame('e805ab01-1c65-4420-ab2d-c893ef9c6afe', torah_obligates_sacrificial_study_post_temple).
narrative_ontology:cs_drift_state('e805ab01-1c65-4420-ab2d-c893ef9c6afe', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e805ab01-1c65-4420-ab2d-c893ef9c6afe', '2026-06-12T14:23:18Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, jewish_communities).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, torah_study_sustains_creation).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, sacrificial_law_efficacy_independent_of_physical_temple).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and rabbinical authorities interpret and transmit the obligation to study sacrificial law. They affirm the performance reading as binding on anyone accepting Torah's authority. They set the curriculum, determine which texts are studied, and adjudicate disputes about proper understanding. The constraint's persistence depends on their teaching and interpretive authority.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, jewish_scholars, agenda_setter,
    organized, generational, identity_locked, global).

% Communities that affirm the performance reading benefit from participation in cosmic sacrifice through study. Their identity as Torah-observant communities depends partly on accepting binding obligations. Study is presented to them as a binding obligation, not as optional coordination. Exit from the obligation requires exit from the community or from acceptance of Torah authority.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, jewish_communities, beneficiary,
    moderate, generational, constrained, global).

% The metaphysical beneficiary in this reading: cosmic order is sustained by study of sacrificial law. This is a non-agent entity included for theological completeness. The reading asserts that cosmic order genuinely depends on the study obligation; whether this claim is true or is a human institution's legitimacy framing is the subject of the primary omega variable.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Scholars and communities holding the study_as_archive or study_as_preparation readings are excluded from this constraint's specific performance claim. They do not affirm that study enacts cosmic sacrifice; they would object that this reading overstates the obligation's metaphysical reach. Their positions are live within Jewish tradition but are not heard within the internal logic of the study_as_performance reading.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, competing_readings, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination problem solved in the standard sense: the reading presents study as an obligation grounded in cosmic necessity, not as a solution to a collective-action problem among human parties. If reframed, the implicit coordination is among the Jewish community to maintain binding relationship with Torah authority and cosmic order.
% TRANSFER_FUNCTION: No transfer from one human party to another. The constraint's operation is study → cosmic efficacy (text → metaphysical result), not study → human beneficiary gain. If forced into transfer language: time and intellectual effort → cosmic order's sustenance, mediated through scholarly and communal practice.
% ABSENT_VOICES: Skeptical philosophers and secular scholars who deny the cosmic efficacy claim. Modern Jewish movements (Reform, Reconstructionist) that reframe the obligation as identity-maintenance rather than binding law. Archaeological and historical scholars who emphasize the Temple's material absence and argue the performance claim is unintelligible once materiality is centered.
% DISAPPEARANCE_RATIONALE: Adherents of the performance reading believe cosmic order would be destabilized or unmoored from human participation if study ceased; skeptics argue cosmic order (if it exists) is independent of human study and would persist unchanged. The dispute is not tractable within empirical frameworks because it concerns metaphysical necessity.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the Jewish community faced a binding obligation to perform sacrifices that could not be physically enacted. The founding problem was theological and legal: how to maintain the obligation's binding force when performance became materially impossible. The performance reading solves this by decoupling efficacy from physicality—study performs sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Medieval kabbalists (Luria and others) explicitly affirm the performance reading and argue study sustains cosmic order. Modern scholars of Jewish law (Shlomo Riskin, David Berger) attest to the reading's presence in authoritative halakhic tradition. Skeptics (secular scholars, historians of religion) attest that the problem was socially managed but may not have had a univocal solution—the reading coexists with alternatives rather than being universally adopted. No corroboration exists outside the benefiting parties (those committed to the performance reading) because those parties are the sole authorities within this tradition who affirm the reading's core claim.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is zero (0.0) under this reading's internal logic: study enacts sacrifice; the constraint generates no transfer from one human agent to another; no victim set exists because the beneficiary (cosmic order) is metaphysical rather than a human party capturing value. The reading's own coherence requires this zero value. Suppression is zero because the constraint is presented as a cosmic obligation, not a coercive arrangement. Theater ratio is zero because the performance claim asserts genuine efficacy, not theatrical maintenance. Accessibility collapse is very high (0.95) because once the cosmic-performance reading is understood, no meaningful alternative to 'study performs sacrifice' exists within that framework—the claim is internally self-reinforcing. Resistance is near-zero (0.05) because the reading faces logical opposition (the sibling readings contest it) but not structural resistance from actors bearing costs—there are no identified cost-bearers. The measurement series show stability across the interval because the reading's internal structure does not change over time; cosmic performance is a synchronic claim, not subject to drift or accumulation mechanics. All measurements are marked 'observed' because they reflect the logical structure of the reading itself, not empirical drift.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap here is not between human seats but between the reading's internal theological claim and external skepticism: within the reading, cosmic order is the beneficiary and study is genuine performance; from an external skeptical position, cosmic order is a metaphysical claim rather than a beneficiary in the transfer sense, and study is symbolic rather than performative. The engine does not compute a per-seat type divergence because all human participants who accept the reading affirm the same structure—study performs sacrifice. Divergence emerges between this reading and its siblings (archive and preparation), not between human seats within this reading. That inter-reading divergence is routed through the network and the omega variables, not through directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, there is no standard directionality calculation because there are no human seats in extraction relationship. The constraint's only named beneficiary is 'cosmic order,' which is not an agent carrying power atoms or exit options. The reading presents the obligation as cosmic law, not as a transfer mechanism between human parties. Therefore, the engine's standard directionality derivation (power + exit + beneficiary/victim declarations → d) does not apply. The constraint is mountain-classified on the basis of its claimed naturalness (cosmic performance is presented as an irreducible metaphysical fact) and its low extraction/suppression metrics. No directionality override is required because the constraint has no seats to differentiate.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—function outlived, structure persisted—is NOT present in this reading. The reading explicitly affirms the binding function of the obligation: study IS sacrifice; cosmic order IS sustained; the obligation is not decorative or inertial. The constraint's efficacy claim is ongoing and unconditional on Temple restoration. The study_as_preparation reading (a sibling) instantiates a different mandatrophy structure: preparation is necessary only until restoration occurs; if restoration becomes indefinitely deferred, preparation may accumulate as inertial obligation. This reading escapes that trap by decoupling efficacy from restoration. No mandatrophy omega is needed here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_order_beneficiary_status,
    'Is ''cosmic order'' a real beneficiary whose interests the constraint serves, or is it a metaphysical claim that differs fundamentally from how beneficiaries are structured in non-religious constraints?',
    'Distinguish whether the constraint''s persistence depends on identifiable human beneficiaries who prefer cosmic-order framing as a cover story, or whether the constraint is genuinely grounded in irreducible metaphysical claims that are not human-interest-relative.',
    'If cosmic order is metaphysically real (not reducible to human agents'' interests), the mountain classification holds and extraction remains zero. If cosmic-order framing is a cover for human institutional interests, FSM triggers and reclassification occurs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmic_order_beneficiary_status, conceptual, 'Whether the beneficiary (cosmic order) is a genuine metaphysical entity or a human institution''s legitimacy claim.').

omega_variable(
    study_performance_equivalence,
    'Does studying sacrificial law genuinely ENACT the cosmic function of sacrifice, or does it merely SYMBOLIZE or SUBSTITUTE for it while the cosmic function goes unperformed?',
    'Empirical: query whether Jewish mystical and halakhic authorities claim performance-equivalence or substitution-with-gap. Conceptual: if performance-equivalence is held universally within the authority structure, it is structurally internal to the reading; if contested within tradition, it generates a sibling reading.',
    'If study is genuine enactment, extractiveness stays zero and the constraint remains mountain-classified. If study is acknowledged substitution-with-gap, a performance-deficit enters the constraint''s operation and extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_performance_equivalence, conceptual, 'Whether study performs sacrifice or substitutes for it.').

omega_variable(
    reading_contention_within_tradition,
    'This reading coexists with two competing readings (study_as_archive and study_as_preparation) within Jewish legal tradition. Do the three readings occupy genuinely distinct positions held by different authoritative factions, or does one reading dominate the authority structure and the others represent minority dissent?',
    'Historical and textual analysis: examine which reading(s) receive explicit endorsement from authoritative halakhic bodies across the post-Temple period (Talmudic, Medieval, and Modern eras). Map institutional capture: does any single reading control interpretive authority or resource allocation (publication, teaching, institutional legitimacy)?',
    'If all three readings are held equally by different recognized authorities (true coexistence), the reading_relations remain coexists_with. If one reading captures institutional authority while the others persist as minority positions, the dominant reading''s cs_structure should indicate that influence relations track institutional power, not logical force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_within_tradition, empirical, 'The institutional status of competing readings within Jewish legal tradition.').

omega_variable(
    identity_lock_mechanism_for_scholars,
    'Does the obligation to study sacrificial law depend on structural coercion (external enforcement via community pressure, economic dependency on rabbinical credentials), on identity fusion (scholarly self-concept constituted through religious study), or on neither—genuine voluntary coordination around a shared cosmic commitment?',
    'Qualitative: interview Jewish scholars on exit costs; historical: examine communities where study obligation was suspended or refused and what enabled exit without identity collapse; comparative: examine parallel obligations in non-religious scholarly traditions to test whether identity-lock is specific to religious framing.',
    'If exit is structurally trapped or identity-locked despite zero extractiveness measurement, the constraint carries suppression not captured by the base metric. If exit is genuinely open (scholars can cease study without social or identity consequence), the mountain classification holds with low suppression. If identity-lock is discovered, the omega documents internalized suppression despite structural openness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_scholars, empirical, 'Whether the study obligation is enforced structurally, internalized identitarily, or voluntarily coordinated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t5, kodashim_obligation__study_as_performance, theater_ratio, 5, 0.0).
narrative_ontology:measurement_basis(koda_tr_t5, observed).
narrative_ontology:measurement(koda_tr_t10, kodashim_obligation__study_as_performance, theater_ratio, 10, 0.0).
narrative_ontology:measurement_basis(koda_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t5, kodashim_obligation__study_as_performance, base_extractiveness, 5, 0.0).
narrative_ontology:measurement_basis(koda_be_t5, observed).
narrative_ontology:measurement(koda_be_t10, kodashim_obligation__study_as_performance, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(koda_be_t10, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.0).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim obligation kernel. All three readings share the same standing obligation to study sacrificial law but interpret its binding force and efficacy differently. study_as_performance (this story) claims study enacts cosmic sacrifice; study_as_archive claims study preserves historical memory without cosmic function; study_as_preparation claims study maintains technical knowledge for future performance. Each reading instantiates a distinct constraint with different ε values, beneficiary structures, and type classifications. The three stories are linked via network.affects_constraints to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
