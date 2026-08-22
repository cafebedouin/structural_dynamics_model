% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Kodashim Obligation
 *   domain: religious/textual
 *
 * SUMMARY:
 *   Within rabbinic Judaism the order of Kodashim retains living normative
 *   force long after the destruction of the Second Temple made physical
 *   sacrifice impossible. This story instantiates ONE reading of that
 *   contested inheritance: the claim, rooted in the talmudic dictum that
 *   whoever studies the laws of a burnt-offering is as if he offered it
 *   (Menahot 110a), that the act of study itself enacts the sacrifice's
 *   cosmic function - that the Temple's physical absence is irrelevant to the
 *   law's spiritual efficacy. The standing arrangement under contest
 *   (epsilon's referent) is the normative structure that directs scholars and
 *   lay learners into sustained engagement with sacrificial law as
 *   substitute-free performance: not preparation for a restored rite, not
 *   archival custody of a defunct system, but the service itself. Assessed by
 *   this reading's own lights, the arrangement extracts almost nothing:
 *   participation is voluntary, alternatives (prayer, charity, other study)
 *   remain fully open, and the act's cost is borne as its reward. Claimed
 *   type and metrics are authored independently: the claim is rope; the
 *   metrics describe near-zero-extraction, low-suppression, low-theater
 *   operation. The sibling readings (study_as_preparation, study_as_archive)
 *   are separate constraints in separate files; the committer contest is
 *   routed to omega variables, not folded into this classification.
 *
 * KEY AGENTS:
 *   - kodashim_students: primary performers ([moderate]/[mobile]) - enact the sacrificial function through study; bear only the voluntary cost of time and attention
 *   - rabbinic_transmitters: agenda-setters ([institutional]/[identity_locked]) - articulate, transmit, and administer the reading; their authority is constituted by the transmission they administer
 *   - jewish_worshiping_community: secondary beneficiaries ([organized]/[constrained]) - share in the maintained covenantal service without individually mastering the corpus
 *   - temple_restoration_advocates: excluded voice ([organized]/[constrained]) - hold that substitutive study dulls the imperative to rebuild; sidelined by the irrelevance claim
 *   - academic_religion_scholars: analytical observers ([analytical]/[analytical]) - document the post-70 CE substitution problem without adjudicating present efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.03).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study-as-Performance Reading of the Kodashim Obligation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/textual").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '674c1e24-aaaa-41f9-8c20-ac80b0c9aac7').
narrative_ontology:cs_kernel_codification('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', fixed_text).
narrative_ontology:cs_authority_grounding('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', lineage).
narrative_ontology:cs_interpretation_layer_present('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7').
narrative_ontology:cs_reading_relation('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_reading_relation('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', kodashim_obligation__study_as_preparation, influences).
narrative_ontology:cs_axiom('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', foundational, study_enacts_sacrificial_function).
narrative_ontology:cs_axiom_status(study_enacts_sacrificial_function, holdable).
narrative_ontology:cs_axiom_grounding('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', study_enacts_sacrificial_function, theological).
narrative_ontology:cs_axiom('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', secondary, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', temple_absence_irrelevant_to_efficacy, theological).
narrative_ontology:cs_reference_frame('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', study_as_living_rite).
narrative_ontology:cs_drift_state('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', contemporary_mass_text_engagement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('674c1e24-aaaa-41f9-8c20-ac80b0c9aac7', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, kodashim_students).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, rabbinic_transmitters).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, jewish_worshiping_community).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, study_as_sacrifice_equivalence).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, unbroken_divine_service).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage daily or weekly with the orders of Zevahim, Menahot, and kinin, tracing rite, measure, and intent. Under the arrangement this engagement is itself the sacrificial act: the student offers what the text describes. Nothing is paid to anyone; the cost is the learner's own time and attention, returned to them as fulfillment. Exit is simply stopping - no penalty attaches, only the loss the practitioner believes the practice averts.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, kodashim_students, beneficiary,
    moderate, biographical, mobile, global).

% The unbroken chain of authorities - from the Tannaitic framers of the equivalence dictum through Geonim, codifiers, and contemporary roshei yeshiva - who articulate the reading, decide what counts as adequate engagement, and organize the curricula through which the corpus is encountered. Their office exists in and through the transmission; setting the reading aside would not just change policy but dissolve the vocation's warrant. They collect standing and continuity from the arrangement they administer.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rabbinic_transmitters, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_performance, rabbinic_transmitters, beneficiary).

% Sustain the academies, print the tractates, and mark study cycles as communal achievements; they share in the maintained service without individually mastering the sacrificial corpus. Their benefit is participation in an unbroken relationship; their contribution is support rather than submission, and they could redirect support elsewhere without penalty.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, jewish_worshiping_community, beneficiary,
    organized, generational, constrained, global).

% Organized currents inside the tradition who hold that substitutive study, however sincere, dulls the imperative to restore the rite itself; they prepare instruments, draft liturgies, and campaign for readiness. The reading's irrelevance claim sidelines their project: if absence is irrelevant, urgency has no structural purchase. They speak from within the community but outside the arrangement's center.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, temple_restoration_advocates, excluded,
    organized, biographical, constrained, global).

% Historians and philologists of rabbinic literature who document the post-destruction substitution problem - the Yavneh petition tradition, prayer-as-substitution teachings, the reception of the Menahot dictum - and trace how the reading spread across diaspora communities. They analyze the arrangement's operation without holding any position in it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, academic_religion_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining continuous sacrificial service by a dispersed community with no altar: it distributes the corpus across learners and generations so that the whole order of service remains enacted somewhere at all times, converting individual study sessions into a collectively unbroken rite.
% TRANSFER_FUNCTION: Moves no material goods. It converts the learner's time and attention into enacted sacrifice; spiritual standing accrues to the performer, continuity of service accrues to the community, and - per the doctrine - the cosmic order receives the service the altar once received.
% ABSENT_VOICES: Temple-restoration advocates would object that the irrelevance claim dissolves their urgency; holders of the archive reading - secular and academic readers for whom the system is defunct - would object that present efficacy is asserted rather than demonstrated. Both sit outside the study hall where the reading is self-confirming, and neither is represented in the arrangement's administration.
% DISAPPEARANCE_RATIONALE: Curricula, daily learning schedules, printed study cycles, and the community's account of roughly two millennia of unbroken service all hang on this norm. Overnight removal would not return anyone to an altar - it would leave the service simply unperformed: an acute, theologically legible gap that the community would rush to refill through intensified prayer, penitential practice, or restoration politics.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, how can a covenant centered on sacrificial service continue when the altar is gone? This reading's answer: the service continues in words - study enacts what the altar performed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is historically corroborated from outside the beneficiary set: the Yavneh petition tradition (Gittin 56b), the prayer-as-substitution teachings preserved in Avot de-Rabbi Natan, and academic histories of post-70 CE rabbinic adaptation all attest that the community faced a real crisis of unperformable service. Attestation of the problem's continuing liveness comes principally from the believing community itself; outside scholars confirm the problem's historical reality and its persistence as a live theological question while declining, from their seat, to adjudicate whether the study-as-performance answer succeeds.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.02, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).
:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.02) because the arrangement takes nothing from anyone: the performer's expenditure is the performance, and no seat collects a transfer. Suppression is near zero (0.03) because no enforcement machinery exists - study is motivated intrinsically and socially, not coercively; suppression is a raw structural property and is not scaled by scope or power. Theater is low (0.08) because within the arrangement the act and its function coincide - there is little activity that is performative rather than functional. Accessibility_collapse is low (0.20): understanding the reading does not close alternatives, since prayer, charity, and other study remain fully valid channels; friction is limited to curricular priority. Resistance is near zero (0.05): no constituency organizes against the practice itself. Coordination type is attachment_coordination because the dominant function is maintaining the covenantal bond between community and divine service - the thing that would break if the practice failed is precisely that relationship; the type-default floor is used with no override. The measurement series run on one shared nine-point grid spanning roughly 70 CE to the present, so both tracked metrics are authored at every examined time point. Base extractiveness stays flat near zero across nineteen centuries, rising slightly in the modern era as institutional scale (academies, printing, mass study cycles) adds administrative overhead - not extraction from participants. Theater drifts gently upward for the same reason while remaining low. A suppression_requirement series is deliberately omitted: the enforcement picture is static across the interval (no machinery built up or eroded), so the scalar base_properties.suppression already carries the whole story.
 *
 * PERSPECTIVAL GAP:
 *   With epsilon near zero, the payer-versus-beneficiary divergence that drives most seat splits is muted - there is no extracted party. The live divergences are vocational and positional. Transmitters (identity_locked) experience the norm as constitutive of their office: exit would dissolve the vocation's warrant, not merely abandon a practice. Students (mobile) experience the same norm as freely chosen discipline with costless exit. Restoration advocates experience the identical structure as an obstacle - a rival account of what the tradition requires - and would describe it as enforced complacency rather than coordination. Academic observers register the arrangement's history without occupying any position in it. The engine computes these per-seat classifications from the power and exit structure; near-zero extraction keeps every seated computation in the benign range while the excluded seat's objection registers as contention rather than extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated agent derives low directionality: kodashim_students and jewish_worshiping_community are declared beneficiaries, and rabbinic_transmitters carry secondary_role beneficiary atop agenda_setter, so the derivation places all three near the subsidized end. No victims are declared because the arrangement extracts from no one - the act's cost (time, attention) is internal to the performer and counted as fulfillment. Cosmic order, which the reading names as the ultimate beneficiary, is deliberately NOT seated: it is not a real-world actor and collects no rents, so it appears under vindicated_propositions (study_as_sacrifice_equivalence, unbroken_divine_service) rather than beneficiaries. gain_flow is authored 'diffuse' as a checked claim: re-reading every named seat, none captures the arrangement's gains - efficacy flows to performers and, per the doctrine, to the cosmos without accruing to any administrator. fixing_cost is authored 'prohibitive': the transmitters could de-emphasize the norm, but removal would sever the community's account of unbroken service, a cost exceeding any benefit since nothing is broken.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - how a sacrificial covenant continues without an altar - remains live under this reading, so no mandatrophy is declared. The classification guards against two symmetric mislabels. An outside skeptic sees a community rehearsing procedures for a building that will never come and reads piton: theatrical maintenance of a defunct function. But theater_ratio is authored low because within the arrangement the rehearsal IS the function - performance and object coincide - and the modest modern rise is carried honestly in the measurement series rather than suppressed. Conversely, the reading's own proponents might claim mountain: an eternal obligation above construction and politics. emerges_naturally is authored false because the arrangement is transmitted, taught, interpreted, and administrated - a constructed normative structure, however sacred its warrant. The efficacy-metaphysics omega carries what cannot be settled from inside: whether the enactment is realist or expressivist determines whether the residual theater is noise or signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is one reading of the kodashim_obligation kernel (reading: study_as_performance). Which reading governs the community''s operative self-understanding, and how would a sibling reading change the structure?',
    'Survey curricular framing, homiletic language, and communal self-description: does the community describe Kodashim study as service-now, preparation-for-resumption, or heritage-custody?',
    'Under study_as_preparation the arrangement gains transitional structure - restoration becomes structurally necessary and sunset logic enters. Under study_as_archive the arrangement loses present legal force entirely and drifts toward identity-maintenance with a different beneficiary set. Classification, epsilon, and network edges all shift with the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Which reading of the kernel the community actually lives by.').

omega_variable(
    efficacy_metaphysics_realism,
    'Does study literally enact the sacrifice''s cosmic function (realist enactment), or does it function as disciplined remembrance that sustains the community''s relationship (expressivist)?',
    'Not resolvable by data alone - it depends on adopting a theology. Track whether the tradition''s own authorities treat the equivalence as ontic (the sacrifice is accomplished) or formative (the worshipper is shaped).',
    'If expressivist, the constraint''s coordination function is psychological-communal rather than cosmic; the vindicated propositions become aspirational rather than operative, and coupling analysis reads the arrangement as identity-maintenance rather than service-continuity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficacy_metaphysics_realism, conceptual, 'Realist versus expressivist reading of the study-sacrifice equivalence.').

omega_variable(
    restoration_necessity_dispute,
    'Where this reading and study_as_preparation disagree: is the Temple''s absence truly irrelevant to efficacy, or does full performance await restoration? The dispute is located in the necessity clause, not in the value of study.',
    'Doctrinal analysis of whether any authority holds both clauses simultaneously; historical test - did communities holding the performance reading nonetheless treat restoration as practically urgent?',
    'If the necessity clause fails, this reading collapses toward preparation: transitional structure enters and sunset logic becomes relevant. If it holds, the preparation reading''s urgency is devotional rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_necessity_dispute, conceptual, 'The necessity-clause fault line between the performance and preparation readings.').

omega_variable(
    voluntariness_social_pressure,
    'Is participation genuinely uncoerced, or does academy-scale social expectation make exit costly in ways the structural measure misses?',
    'Post-exit trajectory: track individuals who stop studying Kodashim - if sanction, status loss, or self-reproach follows beyond ordinary regret, effective suppression exceeds the structural measure.',
    'If social enforcement is load-bearing, suppression is higher than the authored 0.03 and the arrangement shades toward enforced conformity; if exit is genuinely costless, the low suppression stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_social_pressure, empirical, 'Structural versus socially-enforced voluntariness of study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 1956).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_perf_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t150, kodashim_obligation__study_as_performance, theater_ratio, 150, 0.06).
narrative_ontology:measurement(kodashim_study_perf_tr_t400, kodashim_obligation__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement(kodashim_study_perf_tr_t900, kodashim_obligation__study_as_performance, theater_ratio, 900, 0.07).
narrative_ontology:measurement(kodashim_study_perf_tr_t1200, kodashim_obligation__study_as_performance, theater_ratio, 1200, 0.07).
narrative_ontology:measurement(kodashim_study_perf_tr_t1600, kodashim_obligation__study_as_performance, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(kodashim_study_perf_tr_t1800, kodashim_obligation__study_as_performance, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(kodashim_study_perf_tr_t1900, kodashim_obligation__study_as_performance, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(kodashim_study_perf_tr_t1956, kodashim_obligation__study_as_performance, theater_ratio, 1956, 0.11).

% Extraction over time
narrative_ontology:measurement(kodashim_study_perf_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(kodashim_study_perf_be_t150, kodashim_obligation__study_as_performance, base_extractiveness, 150, 0.01).
narrative_ontology:measurement(kodashim_study_perf_be_t400, kodashim_obligation__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t900, kodashim_obligation__study_as_performance, base_extractiveness, 900, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t1200, kodashim_obligation__study_as_performance, base_extractiveness, 1200, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t1600, kodashim_obligation__study_as_performance, base_extractiveness, 1600, 0.02).
narrative_ontology:measurement(kodashim_study_perf_be_t1800, kodashim_obligation__study_as_performance, base_extractiveness, 1800, 0.03).
narrative_ontology:measurement(kodashim_study_perf_be_t1900, kodashim_obligation__study_as_performance, base_extractiveness, 1900, 0.03).
narrative_ontology:measurement(kodashim_study_perf_be_t1956, kodashim_obligation__study_as_performance, base_extractiveness, 1956, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, attachment_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% The colloquial label 'studying Kodashim' decomposes into three structurally distinct constraints sharing one kernel: study_as_performance (this file - present-tense enactment, near-zero epsilon, no victims, no sunset), study_as_preparation (binding-but-unperformable; transitional structure in which restoration is structurally necessary), and study_as_archive (defunct-system custody; identity-maintenance function, no present legal force). Each has its own epsilon, beneficiary structure, and classification. All three cite the same upstream talmudic source (Menahot 110a), so the dictum's authority feeds each downstream reading differently; the links here declare the family edges, and the epsilon differences are documented in each file's narrative context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
