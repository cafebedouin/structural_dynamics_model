% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
 *   human_readable: Study of Sacrifice Law as Commandment Fulfillment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_performance reading of the
 *   sacrifice_commandment kernel in classical Jewish halakhic thought. In
 *   this reading, the divine commandment of sacrifice remains perpetually
 *   binding, but its fulfillment is transposed from physical altar
 *   performance to intellectual engagement with the sacrificial lawâTorah
 *   study becomes the functional equivalent of the offering. The reading
 *   originates in rabbinic responses to the destruction of the Second Temple
 *   and is articulated in Talmudic and medieval legal-mystical sources. It
 *   claims zero extractiveness: the scholar-worshipper is simultaneously the
 *   agent and beneficiary, experiencing the study as intrinsically valuable
 *   worship rather than as a burden. There is no victim set, no active
 *   enforcement, and no concentrated extraction; the constraint operates as a
 *   self-enforcing interpretive frame within the commitment system of
 *   halakhic Judaism.
 *
 * KEY AGENTS:
 *   - scholar_worshippers (moderate/identity_locked): Primary beneficiariesâindividual learners who fulfill the commandment through study.
 *   - rabbinic_authorities (institutional/analytical): Agenda-setters who transmit and rule on the interpretation that study equals performance.
 *   - temple_centric_adherents (organized/mobile): Excluded voices who hold the performance_only reading and reject study-as-fulfillment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.02).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Commandment Fulfillment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '55a41110-4a36-409e-85d4-1ae6e016e531').
narrative_ontology:cs_kernel_codification('55a41110-4a36-409e-85d4-1ae6e016e531', fixed_text).
narrative_ontology:cs_authority_grounding('55a41110-4a36-409e-85d4-1ae6e016e531', lineage).
narrative_ontology:cs_interpretation_layer_present('55a41110-4a36-409e-85d4-1ae6e016e531').
narrative_ontology:cs_reading_relation('55a41110-4a36-409e-85d4-1ae6e016e531', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('55a41110-4a36-409e-85d4-1ae6e016e531', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('55a41110-4a36-409e-85d4-1ae6e016e531', foundational, study_fulfills_sacrifice_commandment).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_commandment, holdable).
narrative_ontology:cs_axiom_grounding('55a41110-4a36-409e-85d4-1ae6e016e531', study_fulfills_sacrifice_commandment, theological).
narrative_ontology:cs_axiom('55a41110-4a36-409e-85d4-1ae6e016e531', foundational, cognitive_worship_equals_ritual_act).
narrative_ontology:cs_axiom_status(cognitive_worship_equals_ritual_act, holdable).
narrative_ontology:cs_axiom_grounding('55a41110-4a36-409e-85d4-1ae6e016e531', cognitive_worship_equals_ritual_act, theological).
narrative_ontology:cs_reference_frame('55a41110-4a36-409e-85d4-1ae6e016e531', sacrifice_commandment_perpetually_active).
narrative_ontology:cs_drift_state('55a41110-4a36-409e-85d4-1ae6e016e531', post_second_temple, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('55a41110-4a36-409e-85d4-1ae6e016e531', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who study the laws of sacrifice in Torah, Mishnah, and Talmud as a form of worship. They believe this intellectual engagement fulfills the biblical commandment of sacrifice in the absence of the Temple. The time and effort of study are experienced as spiritually rewarding and covenantally meaningful, not as a burden. They remain within this framework through religious commitment and identity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, identity_locked, global).

% Teachers, jurists, and communal leaders who transmit the ruling that Torah study equals sacrifice. They maintain the interpretive tradition through curricula, legal responsa, and public teaching. Their authority is reinforced by the tradition itself; they do not collect material rents from the arrangement and could theoretically revise the interpretation, though they are bound by the chain of transmission.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Groups and individuals who maintain that the commandment requires physical animal sacrifice and therefore remains suspended until the Temple is rebuilt. They do not accept study as a valid fulfillment and are not normatively addressed by this reading, though they participate in broader Jewish discourse.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_centric_adherents, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for Jewish worshippers to fulfill the biblical commandment of sacrifice after the destruction of the Temple, coordinating individual religious practice with a divine obligation that lacks its original physical infrastructure.
% TRANSFER_FUNCTION: Moves the religious value and covenantal credit of sacrifice from the physical act of animal offering to the cognitive act of Torah study; the transfer is internal to the worshipper rather than between human parties.
% ABSENT_VOICES: Temple-centric movements that await messianic restoration and reject study-as-fulfillment, as well as the archive_maintenance reading that reduces study to technical preservation rather than present worship.
% DISAPPEARANCE_RATIONALE: If the study-as-performance reading disappeared, the scholar-worshippers who rely on it would lose their designated mode of fulfilling the sacrifice commandment. They would need to adopt alternative readings, await Temple restoration, or leave the obligation unfulfilled, rearranging the normative and spiritual economy of halakhic Judaism.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE removed the physical and institutional site for biblical sacrificial worship, leaving the commandment of sacrifice without a literal mechanism for fulfillment.
% FOUNDING_PROBLEM_CORROBORATION: The historical destruction of the Second Temple is corroborated by Roman historians, archaeological evidence, and the continuous extra-rabbinic record of Jewish diaspora life without sacrificial practice.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.02 because the constraint moves nothing from one human party to another; the 'cost' of study is borne by the same agent who receives the spiritual benefit of fulfillment. Suppression is near-zero (0.05) because persistence depends on voluntary religious commitment and internalized norm, not external coercion. Theater ratio is low (0.10) because the study activity is functionally constituted as worship within the reading's own terms, not a performative substitute for an absent reality. Accessibility collapse is moderate (0.30) because alternative readings (performance_only, archive_maintenance) remain live and visible within the broader tradition. Resistance is low (0.15) because this reading is widely accepted in mainstream rabbinic Judaism, though contested by minority temple-centric movements.
 *
 * PERSPECTIVAL GAP:
 *   The scholar-worshipper seat and the rabbinic authority seat should compute similarlyâboth are on the beneficiary side of the directionality derivationâthough the authority has greater analytical exit (can revise interpretation) while the worshipper's exit is identity-locked into the covenantal framework. No payer seat exists to generate divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The only human agent class is the scholar-worshipper, declared as beneficiary. Because there is no victim group and no extracted surplus accruing to a third party, directionality for the beneficiary agent sits near d=0.0, driving effective extraction toward zero (or negative, i.e., subsidy). Rabbinic authorities are agenda_setters but do not collect extraction; their directionality is also beneficiary-side because they sustain the interpretive framework that defines their own legitimacy and spiritual economy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as a piton or snare because there is no agenda-setter who profits from theatrical maintenance, and no payer who bears diffuse costs. If the founding problem (Temple destruction) were solved, the reading might atrophy, but as long as the problem is live, the function is genuine. The reading is not a scaffold because it carries no sunset clauseâit is not presented as transitional but as a perpetual legitimate mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the study_as_performance reading exhaust the normative content of the sacrifice commandment, or is it one of several equally valid halakhic framings?',
    'Comparative analysis of halakhic literature and communal practice across Jewish denominations to see whether study_as_performance is held as exclusive, primary, or co-equal with archive_maintenance and performance_only.',
    'If study_as_performance is only one valid framing among many, its constraint type remains rope (coordination without extraction); if it is imposed as the sole legitimate mode on populations that hold other readings, the classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading''s validity is exclusive or plural within the kernel.').

omega_variable(
    study_cost_as_extraction,
    'Is the time and cognitive labor demanded by Torah study a genuine cost borne by the scholar, or is it fully transmuted into benefit by the theological frame?',
    'Ethnographic and phenomenological study of scholar-worshippers'' reported experience; if significant populations report burden rather than fulfillment, extraction is non-zero.',
    'If the labor is experienced as cost, base extractiveness rises and the beneficiary-victim structure becomes asymmetric, potentially shifting classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_cost_as_extraction, empirical, 'Whether study labor is experienced as cost or pure benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_commandment__study_as_performance, theater_ratio, 400, 0.1).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_commandment__study_as_performance, theater_ratio, 800, 0.1).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__study_as_performance, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_commandment__study_as_performance, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__study_as_performance, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t400, sacrifice_commandment__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(sacr_be_t800, sacrifice_commandment__study_as_performance, base_extractiveness, 800, 0.02).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__study_as_performance, base_extractiveness, 1200, 0.02).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_commandment__study_as_performance, base_extractiveness, 1600, 0.02).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__study_as_performance, base_extractiveness, 2000, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t400, sacrifice_commandment__study_as_performance, suppression_requirement, 400, 0.05).
narrative_ontology:measurement(sacr_su_t800, sacrifice_commandment__study_as_performance, suppression_requirement, 800, 0.05).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_commandment__study_as_performance, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_commandment__study_as_performance, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__study_as_performance, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This story is one reading of the sacrifice_commandment kernel. The kernel decomposes into three structurally distinct constraints: study_as_performance (this file), performance_only, and archive_maintenance. Each reading has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
