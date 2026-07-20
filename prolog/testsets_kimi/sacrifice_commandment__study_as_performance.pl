% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study as Performance of the Sacrifice Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint story instantiates the study_as_performance reading of
 *   the sacrifice_commandment kernel. In this reading â canonical in
 *   rabbinic Judaism from the Talmudic period onward â intensive study of
 *   sacrificial law (tractates Zevachim, Menachot, etc.) is not merely
 *   archival preparation for a future Temple or a suspended memory, but IS
 *   the present-tense fulfillment of the biblical commandment. The
 *   scholar-worshipper is the sole beneficiary: no asymmetric extraction
 *   occurs because the spiritual value accrues to the agent performing the
 *   study. The constraint is claimed as rope â a coordination mechanism
 *   that bridges divine command and post-Temple reality without victims or
 *   coercive overhead. The metrics (zero extractiveness, negligible
 *   suppression, low theater) are authored independently of the claim;
 *   divergence from rope thresholds would signal hidden extraction or
 *   false-summit dynamics not visible from the doctrinal surface.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda setter (institutional/analytical) â maintains the interpretive doctrine that routes sacrifice into study without material extraction
 *   - scholar_worshipper: Primary beneficiary (moderate/constrained) â fulfills divine obligation through intellectual engagement with sacrificial texts
 *   - temple_literalist: Excluded voice (organized/constrained) â holds the performance_only reading and is structurally marginalized in this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study as Performance of the Sacrifice Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'e1a3cf7b-fa6f-4973-a710-9722bf17db63').
narrative_ontology:cs_kernel_codification('e1a3cf7b-fa6f-4973-a710-9722bf17db63', fixed_text).
narrative_ontology:cs_authority_grounding('e1a3cf7b-fa6f-4973-a710-9722bf17db63', lineage).
narrative_ontology:cs_interpretation_layer_present('e1a3cf7b-fa6f-4973-a710-9722bf17db63').
narrative_ontology:cs_reading_relation('e1a3cf7b-fa6f-4973-a710-9722bf17db63', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e1a3cf7b-fa6f-4973-a710-9722bf17db63', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('e1a3cf7b-fa6f-4973-a710-9722bf17db63', foundational, study_fulfills_divine_obligation).
narrative_ontology:cs_axiom_status(study_fulfills_divine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e1a3cf7b-fa6f-4973-a710-9722bf17db63', study_fulfills_divine_obligation, theological).
narrative_ontology:cs_axiom('e1a3cf7b-fa6f-4973-a710-9722bf17db63', secondary, intellectual_engagement_ritually_efficient).
narrative_ontology:cs_axiom_status(intellectual_engagement_ritually_efficient, holdable).
narrative_ontology:cs_axiom_grounding('e1a3cf7b-fa6f-4973-a710-9722bf17db63', intellectual_engagement_ritually_efficient, theological).
narrative_ontology:cs_reference_frame('e1a3cf7b-fa6f-4973-a710-9722bf17db63', study_fulfills_obligation_framework).
narrative_ontology:cs_drift_state('e1a3cf7b-fa6f-4973-a710-9722bf17db63', contemporary_diaspora, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('e1a3cf7b-fa6f-4973-a710-9722bf17db63', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_study_supreme_value).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, rabbinic_oral_law_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and adjudicates the interpretive tradition that re-routes the biblical sacrificial commandment from physical Temple performance to intensive cognitive engagement with sacrificial law. Sustains yeshiva curricula and legal codes that treat tractates of Kodashim as the primary locus of present-tense worship. Does not materially profit from the scholar's labor; the tradition's legitimacy derives from textual continuity and lineage rather than extraction.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Dedicates substantial daily time to mastering the Talmudic order of Kodashim â the tractates detailing Temple sacrifice â as an act of divine service. Believes that this intellectual engagement satisfies the Torah obligation that would otherwise require bringing an animal to the altar in Jerusalem. Receives spiritual fulfillment, religious credit, and communal status; exit would mean abandoning this specific form of worship or adopting an alternative reading of the commandment such as passive waiting or Temple activism.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshipper, beneficiary,
    moderate, biographical, constrained, global).

% Advocates for immediate rebuilding of the Temple and restoration of physical sacrifices. Rejects the equivalence between study and performance, holding that the commandment is suspended in exile and cannot be discharged through cognitive means. Is not party to the halakhic consensus that study fulfills the obligation; their position is treated as messianic or politically radical rather than normative within this framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_literalist, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for fulfilling the biblical commandment of sacrifice in the prolonged absence of the Temple, substituting intellectual engagement with the textual and legal tradition for physical ritual performance.
% TRANSFER_FUNCTION: Moves the locus of religious obligation from physical Temple sacrifice to cognitive engagement with sacrificial law. The scholar invests time and intellectual labor; the return is spiritual fulfillment, divine credit, and continuity with the covenantal framework.
% ABSENT_VOICES: Temple literalists and performance-only advocates reject the study equivalence and are structurally excluded from the normative halakhic conversation; their insistence on physical restoration is treated as premature or politically untenable rather than a live legal option.
% DISAPPEARANCE_RATIONALE: If the doctrine that study fulfills the commandment vanished, observant Jews in the diaspora would lose the primary halakhic mechanism for discharging the obligation of sacrifice. Religious practice would reorganize around either passive messianic waiting, urgent Temple activism, or a revised theology of commandment suspension, fundamentally altering the daily worship economy of traditional Torah study.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical and institutional infrastructure for biblical sacrificial worship, creating a crisis of religious continuity: the Torah commands sacrifice, but the site and means no longer exist.
% FOUNDING_PROBLEM_CORROBORATION: The Temple's destruction is corroborated by extra-biblical historical sources including Josephus and Roman imperial histories, and by the universal Jewish observance of Tisha B'Av. The specific halakhic solution â that study substitutes for sacrifice â is attested internally by rabbinic literature (Mishnah, Talmud) and medieval codifiers such as Maimonides, but no external corroboration exists for the theological claim that intellectual engagement is ritually equivalent to animal sacrifice.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.0 because the scholar-worshipper receives the spiritual benefit of the obligation directly; there is no intermediary extracting surplus or rent. Suppression is minimal (0.05) because the constraint persists through normative transmission and identity commitment rather than coercion. Theater is low (0.1) and slowly rising only because institutionalized Torah study carries minor performative status markers, yet the core activity is experienced as genuine worship. Accessibility collapse is moderate-low (0.25): within this reading, alternatives (physical sacrifice) collapse because the Temple is absent, but competing readings (performance_only, archive_maintenance) remain live in the broader discourse, preventing full collapse. Resistance is low (0.2): Temple literalist movements exist but are politically and halakhically marginal.
 *
 * PERSPECTIVAL GAP:
 *   The scholar-worshipper seat experiences the constraint as worship and fulfillment (directionality near the beneficiary end, yielding negligible effective extraction). The excluded temple_literalist seat would experience the same structural arrangement as an illegitimate suspension of divine law (would compute high directionality if included as a stakeholder). The engine captures this divergence by deriving d from beneficiary declarations and exit options; the author does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholar_worshipper is the sole declared beneficiary, situating their directionality near the full-beneficiary pole. The rabbinic_authority administers the doctrine but does not appear in the beneficiary or victim arrays, leaving their directionality to the canonical fallback for institutional agents with analytical exit â structurally neutral. The temple_literalist is excluded from the stakeholder surface entirely, consistent with their absence from the normative conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Temple destruction and the consequent inability to perform sacrifices â remains live after two millennia. The solution (study as performance) continues to function as a coordination mechanism without observable atrophy. Because the theater ratio is low and the founding problem is unresolved, the constraint does not satisfy piton criteria. A claimed mountain would require emerges_naturally, which is not asserted because this is a contested rabbinic reading rather than a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the study_as_performance reading of the sacrifice_commandment kernel. Sibling readings include performance_only and archive_maintenance. What would change structurally if the performance_only reading were adopted instead?',
    'Examine whether any single halakhic authority simultaneously holds that study IS the exercise and that the commandment is fully suspended without Temple; if no such authority exists, the forecloses relation is structurally valid.',
    'If performance_only is logically foreclosed, this reading claims exclusive validity within unified frameworks, increasing effective accessibility_collapse for adherents and hardening the boundary between coordinated worship and excluded literalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer uncertainty around sibling reading structural relations').

omega_variable(
    study_substantiation_mode,
    'Is the equivalence between study and sacrifice a theological reality accepted by the divine, or a rabbinic legal construct (hekesh) that maintains textual continuity without ontological identity?',
    'Theological analysis of primary sources (Mishnah Menachot, Talmud Bavli, Maimonides'' Hilchot Teshuva) for language of divine acceptance versus rabbinic ordinance; phenomenological analysis of whether practitioners experience study as worship or as compliance.',
    'If ontological, the constraint trends toward Mountain-like immunity; if conventional, it remains a Rope whose legitimacy depends on communal acceptance and could dissolve if the community abandoned the equivalence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_substantiation_mode, conceptual, 'Ontological versus conventional grounding of study-as-worship').

omega_variable(
    temporal_validity_boundary,
    'Does the study-as-performance reading remain valid after a future Temple restoration, or does it expire when physical sacrifice resumes?',
    'Halakhic analysis of whether the reading contains an implicit sunset clause tied to the messianic era; examination of rabbinic sources that privilege future sacrifice over contemporary study.',
    'If temporally bounded, the constraint should be re-evaluated as Scaffold rather than Rope, altering the classification and the warranted duration of its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_validity_boundary, empirical, 'Whether the constraint carries an implicit sunset clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_study_perf_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_study_perf_tr_t400, sacrifice_commandment__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement(sacrifice_study_perf_tr_t800, sacrifice_commandment__study_as_performance, theater_ratio, 800, 0.07).
narrative_ontology:measurement(sacrifice_study_perf_tr_t1200, sacrifice_commandment__study_as_performance, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(sacrifice_study_perf_tr_t1600, sacrifice_commandment__study_as_performance, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(sacrifice_study_perf_tr_t2000, sacrifice_commandment__study_as_performance, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacrifice_study_perf_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacrifice_study_perf_be_t400, sacrifice_commandment__study_as_performance, base_extractiveness, 400, 0.0).
narrative_ontology:measurement(sacrifice_study_perf_be_t800, sacrifice_commandment__study_as_performance, base_extractiveness, 800, 0.0).
narrative_ontology:measurement(sacrifice_study_perf_be_t1200, sacrifice_commandment__study_as_performance, base_extractiveness, 1200, 0.0).
narrative_ontology:measurement(sacrifice_study_perf_be_t1600, sacrifice_commandment__study_as_performance, base_extractiveness, 1600, 0.0).
narrative_ontology:measurement(sacrifice_study_perf_be_t2000, sacrifice_commandment__study_as_performance, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_study_perf_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(sacrifice_study_perf_su_t400, sacrifice_commandment__study_as_performance, suppression_requirement, 400, 0.03).
narrative_ontology:measurement(sacrifice_study_perf_su_t800, sacrifice_commandment__study_as_performance, suppression_requirement, 800, 0.04).
narrative_ontology:measurement(sacrifice_study_perf_su_t1200, sacrifice_commandment__study_as_performance, suppression_requirement, 1200, 0.04).
narrative_ontology:measurement(sacrifice_study_perf_su_t1600, sacrifice_commandment__study_as_performance, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(sacrifice_study_perf_su_t2000, sacrifice_commandment__study_as_performance, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_commandment kernel. The kernel decomposes into three structurally distinct claims: performance_only (physical execution required), archive_maintenance (study as future-oriented preservation), and study_as_performance (study as present worship). Each carries a distinct epsilon and stakeholder structure. They are linked via network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
