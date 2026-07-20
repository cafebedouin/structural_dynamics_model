% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_performance reading of the
 *   kodashim_obligation kernel: the theological claim that studying Jewish
 *   sacrificial law (Kodashim) after the Second Temple's destruction is not
 *   merely preparatory or archival but actively enacts the cosmic function of
 *   sacrifice itself. The Temple's physical absence is treated as irrelevant
 *   to the law's spiritual efficacy. Structurally, this reading asserts zero
 *   extractivenessâno human agent is victimized, no rent is collected, and
 *   the sole beneficiary is cosmic order itself. The constraint is claimed as
 *   a mountain: an irreducible metaphysical feature of the Torah system that
 *   operates without enforcement or institutional maintenance. The authoring
 *   seat is the analytical religious studies observer who reports the
 *   reading's structural claims without endorsing its metaphysics.
 *
 * KEY AGENTS:
 *   - Torah students: voluntary participants who perform the study; not structurally beneficiaries or victims because the constraint extracts nothing and transfers nothing material.
 *   - Rabbinic interpretive tradition: transmitter and interpreter of the kernel; not an agenda-setter in the extractive sense because the constraint is self-executing through belief rather than enforced through institutional coercion.
 *   - Physical Temple restoration advocates: excluded voices who regard study as insufficient and demand literal rebuilding; their absence from the constraint's logic is structural to this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.02).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '2a82813e-6d8d-4020-82dd-c77e5712c1a4').
narrative_ontology:cs_kernel_codification('2a82813e-6d8d-4020-82dd-c77e5712c1a4', formalized).
narrative_ontology:cs_authority_grounding('2a82813e-6d8d-4020-82dd-c77e5712c1a4', lineage).
narrative_ontology:cs_interpretation_layer_present('2a82813e-6d8d-4020-82dd-c77e5712c1a4').
narrative_ontology:cs_reading_relation('2a82813e-6d8d-4020-82dd-c77e5712c1a4', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('2a82813e-6d8d-4020-82dd-c77e5712c1a4', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('2a82813e-6d8d-4020-82dd-c77e5712c1a4', foundational, study_performs_cosmic_sacrifice).
narrative_ontology:cs_axiom_status(study_performs_cosmic_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('2a82813e-6d8d-4020-82dd-c77e5712c1a4', study_performs_cosmic_sacrifice, theological).
narrative_ontology:cs_axiom('2a82813e-6d8d-4020-82dd-c77e5712c1a4', foundational, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('2a82813e-6d8d-4020-82dd-c77e5712c1a4', temple_absence_irrelevant_to_efficacy, theological).
narrative_ontology:cs_reference_frame('2a82813e-6d8d-4020-82dd-c77e5712c1a4', torah_study_cosmic_maintenance).
narrative_ontology:cs_drift_state('2a82813e-6d8d-4020-82dd-c77e5712c1a4', post_second_temple_destruction, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2a82813e-6d8d-4020-82dd-c77e5712c1a4', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, study_enacts_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, temple_absence_irrelevant_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains cosmic order and channels divine vitality into the created world through the cognitive performance of sacrificial law, substituting intellectual engagement for physical Temple ritual as the operative mechanism.
% TRANSFER_FUNCTION: Moves metaphysical efficacy from the potential state of written law to actualized cosmic maintenance through the mental and verbal labor of study; no material or economic transfer occurs between human agents.
% ABSENT_VOICES: Priests and Levites whose hereditary Temple service was rendered impossible by the destruction; secular historians and critical biblical scholars who treat Kodashim as a defunct Iron Age cultic system; activists for immediate physical Temple restoration who regard study as insufficient and demand literal rebuilding.
% DISAPPEARANCE_RATIONALE: From within the theological framework, the constraint's disappearance would threaten cosmic order itself because no alternative mechanism would maintain the sacrificial channels. From the analytical religious studies seat, its disappearance would represent a shift in interpretive doctrine with no observable rearrangement of material reality. The verdict is contested between the insider participant seat and the analytical observer seat.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical locus for sacrificial worship, creating a crisis of continuity for a Torah system centered on Temple ritual.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians (e.g., Shaye J. D. Cohen, Seth Schwartz) corroborate that the destruction created a genuine institutional rupture in Jewish practice. They do not corroborate the study-as-performance remedy; from the analytical seat, the remedy is a post-facto ideological innovation rather than a revealed solution. Rabbinic literature (Avot d'Rabbi Natan, Talmud Bavli Megillah) attests the problem from inside the tradition.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.0, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.0 because the structural delta for this reading specifies no extraction: study is the performance, not a proxy for it. Suppression is minimal (0.02) because the constraint operates through voluntary engagement and internalized theological commitment; there is no coercive apparatus. Theater ratio is minimal (0.05) because the reading explicitly denies that study is performative theaterâstudy IS the real cosmic function, not a stand-in for an absent Temple service. Accessibility collapse is high (0.92) because within the theological frame, no alternative mechanism exists to enact the sacrificial cosmic function in the absence of the Temple; alternatives (e.g., physical restoration, abandonment of Kodashim) are understood as spiritually non-viable. Resistance is low (0.08) because the constraint is self-enforcing through belief; external resistance is limited to secular, rationalist, or restorationist critique that questions the metaphysical premise rather than resisting an extractive structure.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer seat (religious studies scholar) sees a constructed rabbinic doctrine that emerged after 70 CE; the insider participant seat sees a mountain (irreducible spiritual law). This divergence is the committer frame in action. The engine computes per-seat classifications from structural data, but this constraint is authored from the reading's own structural claims, which assert mountainhood. The perspectival gap manifests as a claimed mountain whose historical emergence is contestedâa natural candidate for the false-summit-mountain signature if human beneficiaries were declared.
 *
 * DIRECTIONALITY LOGIC:
 *   No human directionality is declared because no human agent is structurally positioned as beneficiary or victim. The constraint's metaphysical directionality is entirely toward cosmic orderâa non-agent sink. Torah students and rabbinic transmitters operate at approximately symmetric position (dâ0.5) or lower: they engage voluntarily and may accrue spiritual merit, but the structural data does not cast them as beneficiaries of an extractive mechanism. The engine's derivation chain will treat undeclared human agents as falling back to the power atom's canonical default; because no beneficiary or victim facts are present, no effective extraction (Ï) is computed for human seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy because it never claims to be a temporary scaffold or preparation for future restoration. It explicitly asserts that the Temple's physical absence is irrelevant, so there is no lapsed mandate to maintain. The constraint's persistence is justified by its ongoing cosmic function, not by institutional inertia or nostalgic theater. Were the reading to admit that study is merely a stopgap until restoration, it would collapse into the study_as_preparation reading and face scaffold or piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_law_or_construct,
    'Is the claim that Torah study of Kodashim enacts sacrifice a genuine metaphysical law intrinsic to the Torah system, or a post-Temple rabbinic doctrinal construct developed to legitimize continued engagement with ostensibly defunct Temple law?',
    'Historical-textual analysis tracing the doctrine''s emergence in Tannaitic and Amoraic literature; comparison with Second Temple-era texts (e.g., Qumran, Philo) to determine whether study-as-performance is attested before the destruction or is a retroactive innovation.',
    'If the doctrine is historically constructed rather than metaphysically intrinsic, the constraint reclassifies from mountain to either rope (if coordinating communal identity) or scaffold (if transitional), and the epsilon-invariance principle requires decomposition into a separate archive-reading constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_law_or_construct, empirical, 'Whether the study-as-performance doctrine is a discovered metaphysical law or a constructed post-Destruction innovation.').

omega_variable(
    non_agent_beneficiary_ambiguity,
    'Can cosmic order function as a constraint beneficiary when it is not an agent, or does the absence of a human beneficiary indicate a framing gap that masks diffuse human extraction or identity coordination?',
    'Cross-corpus comparison with other constraints claiming non-agent beneficiaries (ecological systems, divine beings) to establish whether the framework permits non-agent beneficiaries or whether the mountain certification requires re-framing onto human participants.',
    'If non-agent beneficiaries are inadmissible, the constraint must either declare human beneficiaries (e.g., Torah students, rabbinic institutions) and face FSM evaluation, or be reclassified as a coordination mechanism with human parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_agent_beneficiary_ambiguity, conceptual, 'Whether a non-agent beneficiary invalidates the zero-extraction mountain claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, study_as_archive).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints. study_as_archive treats Kodashim as a defunct system with negligible epsilon and no coordination function beyond identity maintenance. study_as_preparation treats Kodashim as binding-but-unperformable law with moderate epsilon (opportunity cost of study, deferred gratification). study_as_performance (this constraint) treats study as metaphysically performative with zero epsilon and mountain-type naturality. Each reading has a distinct epsilon, beneficiary structure, and classification; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
