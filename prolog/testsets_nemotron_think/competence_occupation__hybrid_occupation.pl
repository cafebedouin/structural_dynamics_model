% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Competence Occupation: Hybrid Multi-Mechanism Exercise Requirement
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare, rail)
 *   require safety-critical personnel to maintain competence for rare
 *   catastrophic scenarios through continuous multi-mechanism exercise:
 *   high-fidelity simulation, periodic refresher courses, procedural
 *   reinforcement drills, and operational line audits. No consensus exists on
 *   the optimal configuration — how much of each mechanism, at what
 *   frequency, with what fidelity. The constraint presents as coordination
 *   (safety assurance) but operates with substantial extraction (perpetual
 *   training burden on frontline staff, revenue for vendor ecosystem,
 *   regulatory expansion without marginal benefit proof). The
 *   hybrid_occupation reading asserts that multiple mechanisms are
 *   structurally necessary because each covers different failure modes of the
 *   others; simulation_sufficiency and real_incident_necessity are sibling
 *   readings that claim single-mechanism sufficiency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.65).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.55).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Competence Occupation: Hybrid Multi-Mechanism Exercise Requirement").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'd6304832-0928-452c-8e28-a41e7be43eee').
narrative_ontology:cs_kernel_codification('d6304832-0928-452c-8e28-a41e7be43eee', distributed).
narrative_ontology:cs_authority_grounding('d6304832-0928-452c-8e28-a41e7be43eee', practice).
narrative_ontology:cs_interpretation_layer_present('d6304832-0928-452c-8e28-a41e7be43eee').
narrative_ontology:cs_reading_relation('d6304832-0928-452c-8e28-a41e7be43eee', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('d6304832-0928-452c-8e28-a41e7be43eee', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('d6304832-0928-452c-8e28-a41e7be43eee', foundational, multi_mechanism_necessity).
narrative_ontology:cs_axiom_status(multi_mechanism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d6304832-0928-452c-8e28-a41e7be43eee', multi_mechanism_necessity, empirically_contingent).
narrative_ontology:cs_axiom('d6304832-0928-452c-8e28-a41e7be43eee', foundational, no_optimal_configuration_consensus).
narrative_ontology:cs_axiom_status(no_optimal_configuration_consensus, holdable).
narrative_ontology:cs_axiom_grounding('d6304832-0928-452c-8e28-a41e7be43eee', no_optimal_configuration_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('d6304832-0928-452c-8e28-a41e7be43eee', post_tmi_icao_competence_mandate).
narrative_ontology:cs_drift_state('d6304832-0928-452c-8e28-a41e7be43eee', contemporary_evidence_based_training_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6304832-0928-452c-8e28-a41e7be43eee', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, organizational_management).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, safety_critical_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_critical_staff).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, traveling_public_patients).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizational_management).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, competence_requires_continuous_exercise).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, skill_decay_is_inevitable_without_practice).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, multi_mechanism_training_reduces_single_point_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct time, cognitive, and career costs of continuous multi-mechanism training (simulator sessions, refresher courses, procedural drills, line audits). Cannot opt out without losing certification/employment. Experience training as recurring burden that competes with operational duties. Exit options constrained by license requirements and industry-wide standardization.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    organized, biographical, constrained, national).

% Similar to frontline operators but in specialized roles (air traffic controllers, nuclear operators, surgical teams). Benefit from maintained competence in rare high-stakes events but bear disproportionate training load due to low-frequency high-consequence scenarios. Exit nearly impossible due to specialized credentialing.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_critical_staff, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, safety_critical_staff, beneficiary).

% Commercial entities delivering simulator time, refresher courses, and procedural training. Revenue scales with training mandate scope and frequency. No consensus on optimal configuration creates expanding market for new training modalities. Can pivot across industries (aviation, nuclear, healthcare, rail).
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers, beneficiary,
    organized, generational, arbitrage, global).

% High-capital equipment manufacturers and software providers. Benefit from regulatory requirements for high-fidelity simulation. Influence standard-setting bodies to embed simulator requirements. Long product cycles and regulatory capture create high barriers to entry for competitors.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, simulator_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Set mandatory training requirements, approve curricula, enforce compliance through audits. Benefit from demonstrable safety action (political legitimacy, budget justification). No consensus on optimal configuration means regulations accumulate mechanisms rather than optimize. Can expand requirements without proving marginal benefit.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, regulatory_authorities, beneficiary).

% Bear direct costs (training budgets, operational downtime, staffing coverage) and indirect costs (fatigue, turnover). Benefit from liability protection, regulatory compliance, insurance discounts. Trapped by industry-wide mandates — cannot unilaterally reduce training without competitive/regulatory penalty.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, organizational_management, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, organizational_management, beneficiary).

% Academic and institutional researchers studying skill decay curves, simulation fidelity, training transfer, optimal configuration. Perpetual research problem ensures sustained funding and publication venues. No stake in operational outcomes; incentive aligns with demonstrating complexity rather than finding closure.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_research_community, observer,
    analytical, civilizational, analytical, global).

% Ultimate beneficiaries of safety outcomes but no voice in training design. Bear risk if competence fails; bear cost indirectly through fares/fees. Cannot assess training adequacy; trust regulatory assurance. Exit impossible for essential transport/healthcare.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, traveling_public_patients, beneficiary,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures continuous competence occupancy across safety-critical roles by mandating multiple exercise mechanisms (simulation, refresher, procedural reinforcement, line audits) so that no single failure mode (simulator unavailability, curriculum staleness, audit gaming) collapses the entire competence maintenance system.
% TRANSFER_FUNCTION: Moves time, money, and cognitive capacity from frontline operators and organizational budgets to training providers, simulator vendors, and regulatory compliance apparatus. Transfers risk from public to operators (who must maintain competence) and from operators to organizations (who must fund training).
% ABSENT_VOICES: Frontline operators have limited voice in training design — unions consult but don't set curriculum. Patients/public are structurally excluded from competence standard-setting. Smaller organizations that cannot afford full multi-mechanism suites are not represented in standard-setting bodies dominated by major operators.
% DISAPPEARANCE_RATIONALE: If multi-mechanism training mandates vanished, organizations would immediately reduce training to minimum regulatory floor (likely simulation-only or refresher-only). Simulator vendors would lose mandated revenue stream. Regulatory authorities would lose primary lever for safety assurance. Skill decay incidents would rise within 2-3 years per decay curve data. The safety case for high-reliability operations would shift from 'demonstrated competence maintenance' to 'incident-free operation' — a weaker, lagging indicator.
% FOUNDING_PROBLEM: Post-accident investigations (Three Mile Island, Bhopal, early aviation hull losses) revealed that single-mechanism training (classroom only, or simulator only) failed to maintain competence for rare high-consequence events. Skill decay curves showed proficiency halves every 6-12 months without exercise. The founding problem: how to maintain competence for events that occur once per career, using only artificial exercises.
% FOUNDING_PROBLEM_CORROBORATION: Original accident reports (Kemeny Commission, ICAO investigations) corroborate the skill decay problem. However, major operators (airlines, nuclear utilities) and training vendors attest the problem is 'solved' by current multi-mechanism regimes. Independent researchers (NASA HF, EASA studies) attest the problem has shifted: current regimes maintain baseline competence but create new failure modes (training fatigue, checklist compliance without understanding, simulator fidelity gaps). No consensus on whether the founding problem persists in its original form or has mutated.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the high and rising cost of multi-mechanism mandates without evidence of optimal configuration — each new mechanism adds cost without proven marginal safety gain. Suppression (0.55) is moderate: mandates are enforced through licensing and operational authorization, but organizations retain some configuration discretion. Theater ratio (0.45) is significant and rising: compliance-driven training (checking boxes, repeat scenarios) increasingly displaces competence-deepening exercise. Accessibility collapse (0.60) reflects that alternative competence models (apprenticeship, operational experience, reduced simulation) are regulated out. Resistance (0.50) is moderate: unions negotiate training loads, but safety culture narrative limits overt opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/training vendor seat, the constraint appears as necessary coordination (rope-like): multiple mechanisms create defense-in-depth for competence. From the frontline operator seat, it appears as extraction (snare-like): ever-expanding requirements with no off-ramp, skill maintenance for events that never occur. From organizational management, it appears as tangled rope: genuine safety coordination mixed with vendor capture and regulatory ratchet. The engine should compute this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators and safety-critical staff are primary payers (high d ~0.8): they bear time, cognitive load, career risk. Training providers and simulator vendors are primary beneficiaries (low d ~0.15): they collect revenue, face no operational risk. Regulatory authorities are agenda_setters with beneficiary characteristics (d ~0.25): they gain legitimacy/budget from expanding mandates. Organizational management is dual (d ~0.5): pays costs but gains compliance/liability shield. Public is trapped beneficiary (d ~0.3): gains safety but cannot verify training adequacy. Researchers are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence for rare events) remains live but has mutated. Original mandate was 'prevent skill decay for rare events.' Current regime adds mechanisms without removing obsolete ones, creating training fatigue that may degrade the very competence it seeks to maintain. The constraint shows mandatrophy signals: expanding scope (more mechanisms, higher frequency), rising theater ratio, no sunset or optimization review. Yet the core coordination function (preventing competence collapse) is real — not pure extraction. This is tangled rope, not snare: coordination function exists but is contaminated by asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the competence_occupation kernel a single stable commitment with multiple readings, or are these three distinct constraints incorrectly labeled as one?',
    'Trace whether the three readings share a common referent (the same competence kernel) or whether they describe different competence objects (simulation competence vs operational competence vs hybrid competence). If different objects, decompose into separate constraint stories per epsilon-invariance principle.',
    'If distinct constraints, each gets its own epsilon, stakeholders, and classification. Hybrid_occupation would likely be tangled_rope; simulation_sufficiency might be rope (if simulation industry captures regulation); real_incident_necessity might be mountain (if real incidents are truly irreducible) or scaffold (if incidents are transitional teachers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel framing unifies or conflates structurally distinct constraints.').

omega_variable(
    multi_mechanism_necessity,
    'Is the multi-mechanism requirement (simulation + refresher + procedural + audits) structurally necessary for competence maintenance, or does it reflect vendor capture and regulatory ratchet?',
    'Natural experiments: compare safety outcomes in domains that mandate full multi-mechanism suites vs domains that permit simulation-only or experience-based maintenance. Control for system criticality and incident base rates. Measure skill decay curves under different mechanism combinations.',
    'If multi-mechanism is necessary, hybrid_occupation''s coordination function is genuine and extraction is the price of robustness. If simulation-sufficiency or experience-sufficiency holds, the additional mechanisms are extractive overlay — reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multi_mechanism_necessity, empirical, 'Whether the coordination function requires all four mechanisms or whether some are extractive.').

omega_variable(
    optimal_configuration_consensus,
    'Is the absence of consensus on optimal configuration a genuine epistemic limit (the problem is inherently underdetermined) or a manufactured controversy sustained by beneficiary interests?',
    'Analyze whether training optimization research converges over time or whether new mechanisms are added faster than old ones are validated/retired. Track funding sources for ''optimal configuration'' studies. Map citation networks for training efficacy claims.',
    'If genuine epistemic limit, the perpetual research problem is a structural feature (scaffold-like perpetual transition). If manufactured, the constraint is a snare using ''no consensus'' as cover for endless mandate expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_consensus, conceptual, 'Whether the optimization gap is a feature or a bug of the constraint''s operation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by frontline operators structural (regulatory mandates, license requirements) or internalized (professional identity fused with ''continuous training = competence'')?',
    'Post-exit trajectory study: track operators who leave high-reliability roles — do they maintain training behaviors voluntarily? Measure identity fusion (self-report: ''I am a pilot/controller/operator who trains continuously''). Compare suppression levels in domains with identical mandates but different professional cultures.',
    'If internalized, effective suppression is higher than structural measure — operators carry the constraint internally. If structural, exit options (constrained) accurately reflect coercion. Affects directionality calculation for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for frontline payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t1980, competence_occupation__hybrid_occupation, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t1990, competence_occupation__hybrid_occupation, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t2000, competence_occupation__hybrid_occupation, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t2010, competence_occupation__hybrid_occupation, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t2020, competence_occupation__hybrid_occupation, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_tr_t2025, competence_occupation__hybrid_occupation, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t1980, competence_occupation__hybrid_occupation, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t1990, competence_occupation__hybrid_occupation, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t2000, competence_occupation__hybrid_occupation, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t2010, competence_occupation__hybrid_occupation, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t2020, competence_occupation__hybrid_occupation, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_be_t2025, competence_occupation__hybrid_occupation, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t1980, competence_occupation__hybrid_occupation, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t1990, competence_occupation__hybrid_occupation, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t2000, competence_occupation__hybrid_occupation, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t2010, competence_occupation__hybrid_occupation, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t2020, competence_occupation__hybrid_occupation, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(competence_occupation__hybrid_occupation_su_t2025, competence_occupation__hybrid_occupation, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.08).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, regulatory_training_mandate_accumulation).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, simulator_fidelity_arms_race).

% DUAL FORMULATION NOTE:
% This constraint is one reading (hybrid_occupation) of the competence_occupation kernel. The sibling readings are simulation_sufficiency (constraint_competence_occupation__simulation_sufficiency) and real_incident_necessity (constraint_competence_occupation__real_incident_necessity). All three share the kernel claim 'competence requires continuous exercise' but differ on mechanism sufficiency. Hybrid_occupation asserts non-substitutability of mechanisms; simulation_sufficiency asserts simulation alone suffices; real_incident_necessity asserts only real incidents suffice. Their epsilons differ: hybrid_occupation (0.65, multi-mechanism cost), simulation_sufficiency (lower epsilon if simulation is cheaper), real_incident_necessity (near-zero epsilon if incidents are 'free' teachers, but high suppression if incidents are required).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.25).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, organized, 0.15).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, moderate, 0.75).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
