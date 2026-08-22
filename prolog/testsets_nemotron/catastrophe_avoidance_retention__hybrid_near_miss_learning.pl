% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss Learning for Catastrophe Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint describes the distributed learning infrastructure that
 *   high-reliability industries (especially aviation, nuclear power, and
 *   increasingly healthcare) have built to maintain catastrophe-avoidance
 *   competence without requiring actual catastrophes. The core claim is that
 *   three partial information streams — near-misses, foreign incidents, and
 *   high-realism drills — are jointly sufficient but individually necessary.
 *   The constraint is claimed as a rope: genuine coordination with minimal
 *   extraction, where all participants are net beneficiaries. The metrics
 *   reflect low extractiveness (0.18) and suppression (0.12), moderate
 *   theater (0.22, rising with bureaucratization of reporting), low
 *   accessibility collapse (0.35 — alternatives like pure simulation or pure
 *   catastrophe-waiting exist but are empirically inferior), and low
 *   resistance (0.25 — resistance comes from reporting burden, not from the
 *   constraint's legitimacy). This is one reading of the contested kernel
 *   'catastrophe_avoidance_retention'; the sibling readings claim either that
 *   only real catastrophes provide sufficient selection pressure
 *   (catastrophe_as_necessary_selector) or that high-fidelity simulation
 *   alone suffices (simulation_as_proxy_catastrophe).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.18).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss Learning for Catastrophe Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning/high_reliability_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '64797c16-eca0-4724-84fe-8987ec8c1bc5').
narrative_ontology:cs_kernel_codification('64797c16-eca0-4724-84fe-8987ec8c1bc5', distributed).
narrative_ontology:cs_authority_grounding('64797c16-eca0-4724-84fe-8987ec8c1bc5', practice).
narrative_ontology:cs_interpretation_layer_present('64797c16-eca0-4724-84fe-8987ec8c1bc5').
narrative_ontology:cs_reading_relation('64797c16-eca0-4724-84fe-8987ec8c1bc5', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('64797c16-eca0-4724-84fe-8987ec8c1bc5', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, influences).
narrative_ontology:cs_axiom('64797c16-eca0-4724-84fe-8987ec8c1bc5', foundational, near_miss_learning_sufficiency).
narrative_ontology:cs_axiom_status(near_miss_learning_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('64797c16-eca0-4724-84fe-8987ec8c1bc5', near_miss_learning_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('64797c16-eca0-4724-84fe-8987ec8c1bc5', foundational, foreign_incident_transferability).
narrative_ontology:cs_axiom_status(foreign_incident_transferability, holdable).
narrative_ontology:cs_axiom_grounding('64797c16-eca0-4724-84fe-8987ec8c1bc5', foreign_incident_transferability, empirically_contingent).
narrative_ontology:cs_axiom('64797c16-eca0-4724-84fe-8987ec8c1bc5', secondary, drill_realism_calibration).
narrative_ontology:cs_axiom_status(drill_realism_calibration, holdable).
narrative_ontology:cs_axiom_grounding('64797c16-eca0-4724-84fe-8987ec8c1bc5', drill_realism_calibration, instrumental).
narrative_ontology:cs_reference_frame('64797c16-eca0-4724-84fe-8987ec8c1bc5', post_tmi_aviation_learning_infrastructure).
narrative_ontology:cs_drift_state('64797c16-eca0-4724-84fe-8987ec8c1bc5', contemporary_resilience_engineering_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('64797c16-eca0-4724-84fe-8987ec8c1bc5', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_personnel).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_management).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_stakeholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_and_training_vendors).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_personnel).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, distributed_cognition_in_safety).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, learning_from_failure_without_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Front-line operators (pilots, surgeons, control room staff, offshore crews) who directly experience near-misses and participate in drills. They benefit from competence that keeps them alive and effective, but pay in time, cognitive load, and emotional weight of incident review. Exit is constrained by professional licensing, career investment, and the non-portability of domain-specific tacit knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_personnel, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_personnel, payer).

% Safety departments, chief safety officers, training organizations that design and mandate reporting systems, drill programs, and cross-organizational sharing protocols. They set the agenda for what counts as a reportable near-miss, how drills are structured, and which foreign incidents are studied. They benefit institutionally from fewer catastrophes and professional recognition. Exit is mobile — safety management skills transfer across high-reliability domains.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_management, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_management, beneficiary).

% Aviation authorities (FAA, EASA), nuclear regulators (NRC), medical boards, maritime administrations. They benefit from the legitimacy that effective safety oversight provides, and from the political cover of a working learning system. Their exit is analytical — they observe and evaluate but do not operate within the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_oversight_bodies, beneficiary,
    institutional, generational, analytical, national).

% The traveling public, patients, communities near hazardous facilities — ultimate beneficiaries of avoided catastrophes. They are trapped in the system: they cannot exit air travel, medical care, or industrial society, and they have no direct voice in safety learning design. Their safety depends entirely on the constraint's effectiveness.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_stakeholders, beneficiary,
    organized, civilizational, trapped, global).

% Bodies like the Commercial Aviation Safety Team (CAST), the Nuclear Power Plant Operating Experience program, the WHO Surgical Safety Checklist consortium, the International Maritime Organization's casualty analysis. They curate and distribute foreign incident data, standardize reporting taxonomies, and sponsor high-realism drill scenarios. Their power is coordination, not coercion; they persist only while members find value.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, cross_organizational_learning_networks, agenda_setter,
    organized, generational, mobile, global).

% Companies providing high-fidelity simulators, VR/AR training platforms, drill scenario design, and synthetic environment generation. They benefit commercially from the mandate for high-realism drills. Their exit is arbitrage-grade — they can pivot to adjacent markets (gaming, entertainment, non-safety training) if the safety training market contracts.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_and_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Researchers in organizational sociology, safety science, resilience engineering, cognitive systems engineering (e.g., Woods, Hollnagel, Dekker, Perrow, Reason traditions). They study the constraint from outside, providing the epistemic checks that prevent the learning system from becoming self-referential.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective competence for catastrophe avoidance without requiring actual catastrophes, by stitching together three partial information streams: (1) internal near-miss reports that reveal local failure modes, (2) foreign incident data that imports lessons from others' catastrophes, and (3) high-realism drills that exercise response to scenarios never yet experienced. No single stream is sufficient; the constraint is the infrastructure that integrates them into a continuous learning loop.
% TRANSFER_FUNCTION: Moves cognitive effort and organizational resources from routine operations into the learning infrastructure: reporting time, investigation capacity, drill participation, cross-organizational data sharing agreements, simulator procurement. The return is reduced catastrophe probability — a diffuse, non-monetized benefit that accrues to all stakeholders asymmetrically (public stakeholders gain most per unit of investment; vendors gain direct revenue).
% ABSENT_VOICES: Workers in domains without mature near-miss reporting cultures (much of healthcare outside surgery/anesthesia, many developing-nation industrial sectors, informal economy operations) — they would object to the resource demands of the learning infrastructure if they had voice, but they are excluded by the very absence of the constraint in their domains. Also absent: the victims of catastrophes that *did* occur because the learning system was not yet mature — they cannot speak to what the constraint prevents.
% DISAPPEARANCE_RATIONALE: If the distributed near-miss learning infrastructure vanished overnight, industries would revert to either (a) waiting for catastrophes to drive change (the catastrophe_as_necessary_selector reading) or (b) relying on simulation fidelity claims without external validation (the simulation_as_proxy_catastrophe reading). Both paths have empirically higher catastrophe rates. The world would rearrange toward more frequent catastrophic failures, especially in domains where the learning infrastructure is the only thing preventing competence decay (e.g., nuclear power after Three Mile Island, aviation after the 1990s safety plateau).
% FOUNDING_PROBLEM: The post-WWII expansion of high-hazard technologies (commercial aviation, nuclear power, complex surgery, deepwater offshore) created systems where catastrophes were unacceptable but competence could not be maintained by apprenticeship alone. The founding problem was: how to maintain and improve safety competence in complex, tightly-coupled systems without learning from actual disasters?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources outside the direct beneficiaries: the 1979 Three Mile Island accident investigation (Kemeny Commission) explicitly identified the absence of cross-plant learning as a root cause; the 1999 Institute of Medicine 'To Err Is Human' report documented medicine's lack of aviation-style learning infrastructure; the 2010 Deepwater Horizon investigation (National Commission) found that near-miss normalization had displaced learning. These are corroborations from investigative bodies with no stake in the learning infrastructure's continuation.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint's primary operation is information sharing and collective sense-making, not resource transfer. The 'costs' are cognitive and temporal (reporting, drill participation), not material extraction. Suppression is low because participation is largely voluntary at the operational level (mandated at organizational level) and exit, while constrained, is not blocked. Theater ratio is moderate and rose through the 1990s-2000s as reporting systems became bureaucratized (checkbox compliance, 'near-miss' inflation), but has stabilized as resilience engineering critiques (Dekker, Hollnagel) pushed back toward learning-oriented reporting. Accessibility collapse is low because the constraint does not foreclose alternative safety philosophies — they remain live and are tested in domains without the infrastructure. Resistance is low because the constraint's effectiveness is empirically visible in aviation's safety record; the main resistance is operational burden, not ideological opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the operating personnel seat, the constraint feels like a rope with moderate theater (reporting burden) and real coordination value (survival). From the safety management seat, it is a genuine coordination mechanism they administer. From the public stakeholder seat, it is an invisible mountain — they experience only the absence of catastrophe. From the simulation vendor seat, it is a revenue opportunity. The engine should compute rope for all seats; the divergence is in perceived theater and extraction, not in type.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating personnel are dual-positioned: primary beneficiaries (competence keeps them alive) but also payers (time, cognitive load, emotional weight). Their exit is constrained by professional identity and licensing. Safety management and learning networks are agenda_setters who benefit institutionally; their exit is mobile (skills transfer). Regulatory bodies and public stakeholders are beneficiaries with analytical or trapped exit. Simulation vendors are beneficiaries with arbitrage exit — they capture commercial gains but are not structurally dependent. No stakeholder is a victim; the constraint has no extraction target. Directionality is symmetric-to-beneficial across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence without catastrophes) remains live — catastrophes are still unacceptable, and competence still decays without active learning. The constraint has not suffered mandatrophy; its justification is the ongoing problem it was built to solve. The moderate theater ratio reflects bureaucratization, not function loss. The constraint would be a scaffold only if a sunset were declared (e.g., 'until AI-based predictive safety replaces human learning'), but no such sunset exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_reporting_completeness,
    'What fraction of operationally significant near-misses are actually reported and enter the learning stream, vs. normalized or suppressed?',
    'Comparative studies of confidential reporting systems (e.g., NASA ASRS) vs. mandatory reporting vs. operational data mining (FOQA, black box analysis) across domains.',
    'If reporting completeness is low and declining, the constraint''s coordination function degrades toward theater; the effective extractiveness of the reporting burden rises while the learning return falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_reporting_completeness, empirical, 'Whether the near-miss stream is structurally sound or eroding').

omega_variable(
    foreign_incident_transfer_fidelity,
    'How faithfully do lessons from foreign incidents transfer across organizational, cultural, and regulatory boundaries?',
    'Longitudinal tracking of specific foreign-incident-derived interventions (e.g., CRM from aviation to healthcare, nuclear safety culture from US to Japan post-Fukushima) and their measured effectiveness in the receiving domain.',
    'If transfer fidelity is low, the ''foreign incident'' stream is largely theater; the constraint reduces to near-miss + drill, changing its coordination topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_incident_transfer_fidelity, empirical, 'Cross-boundary lesson transfer effectiveness').

omega_variable(
    drill_realism_vs_psychological_safety,
    'Does increasing drill realism (to approach ''catastrophe-equivalent'' stress) degrade the psychological safety needed for honest near-miss reporting?',
    'Studies comparing high-stress drill programs with reporting rates and quality metrics in the same organizations over time; experimental work on stress inoculation vs. threat rigidity.',
    'If a trade-off exists, the constraint''s three streams are in tension; the coordination function requires balancing them, not maximizing each. This would raise the constraint''s inherent coordination cost (Boltzmann floor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_realism_vs_psychological_safety, conceptual, 'Tension between drill fidelity and reporting culture').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does this reading''s core premise (distributed learning suffices) logically foreclose the catastrophe_as_necessary_selector reading, or do they coexist as live positions?',
    'Analyze whether any single organization or domain can simultaneously maintain both: a learning infrastructure that treats near-misses as sufficient AND a belief that only real catastrophes validate competence. If mutual exclusion is structural, the relation is forecloses; if different domains adopt different readings, coexists_with.',
    'If forecloses, the kernel has a structural fault line; if coexists_with, the kernel supports pluralism. Affects cs_structure.reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Logical relationship between this reading and the catastrophe_as_necessary_selector sibling').

omega_variable(
    medicine_learning_infrastructure_gap,
    'Is medicine''s apparent failure to build aviation-grade near-miss learning infrastructure a structural feature of medical practice (irreducible complexity, liability regime, professional culture) or a contingent lag?',
    'Comparative institutional analysis of reporting systems, malpractice liability, professional autonomy, and error disclosure norms across healthcare systems and against aviation''s historical trajectory.',
    'If structural, the constraint''s coordination function has domain-limited applicability — it is not a universal rope. If contingent, medicine is a lagging adopter and the constraint''s universality claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medicine_learning_infrastructure_gap, empirical, 'Whether the constraint''s coordination mechanism generalizes beyond aviation/nuclear').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_avoid_hybrid_tr_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t2020, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2020, 0.23).
narrative_ontology:measurement(cat_avoid_hybrid_tr_t2025, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(cat_avoid_hybrid_be_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(cat_avoid_hybrid_be_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(cat_avoid_hybrid_be_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(cat_avoid_hybrid_be_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(cat_avoid_hybrid_be_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(cat_avoid_hybrid_be_t2020, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2020, 0.18).
narrative_ontology:measurement(cat_avoid_hybrid_be_t2025, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cat_avoid_hybrid_su_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(cat_avoid_hybrid_su_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(cat_avoid_hybrid_su_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(cat_avoid_hybrid_su_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(cat_avoid_hybrid_su_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2010, 0.13).
narrative_ontology:measurement(cat_avoid_hybrid_su_t2020, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2020, 0.12).
narrative_ontology:measurement(cat_avoid_hybrid_su_t2025, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.03).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organization_theory).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, resilience_engineering_practice).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_culture_measurement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'catastrophe_avoidance_retention'. The sibling readings are 'catastrophe_as_necessary_selector' (only real catastrophes maintain competence) and 'simulation_as_proxy_catastrophe' (high-fidelity simulation suffices). This reading claims the three-stream hybrid is necessary and sufficient. The decomposition follows the BGS pattern: each reading has distinct ε, distinct stakeholder structures, and distinct empirical status. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, organized, 0.15).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, moderate, 0.25).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, institutional, 0.1).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
