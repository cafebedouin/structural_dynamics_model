% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency as Competence Occupation
 *   domain: organizational/safety/training
 *
 * SUMMARY:
 *   The simulation_sufficiency reading asserts that high-fidelity simulation
 *   drills alone can occupy the competence kernel — the set of cognitive,
 *   perceptual, and motor patterns required for rare catastrophic scenarios —
 *   preventing skill decay without requiring actual incidents. This reading
 *   became institutionalized after TMI (1979) and early CRM adoption in
 *   aviation. Over four decades, the constraint evolved from a genuine
 *   coordination mechanism (shared standard for rare-event readiness) into a
 *   tangled rope: simulation vendors and compliance bureaucracies benefit
 *   from expanding mandates, while operators bear identity-locked costs and
 *   the public bears trapped costs. The coordination function (auditable due
 *   diligence) is real but increasingly decoupled from the transfer function
 *   (competence assurance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.68).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.62).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency as Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety/training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, 'c7e02755-8298-4b65-aac7-864d08c92101').
narrative_ontology:cs_kernel_codification('c7e02755-8298-4b65-aac7-864d08c92101', distributed).
narrative_ontology:cs_authority_grounding('c7e02755-8298-4b65-aac7-864d08c92101', practice).
narrative_ontology:cs_interpretation_layer_present('c7e02755-8298-4b65-aac7-864d08c92101').
narrative_ontology:cs_reading_relation('c7e02755-8298-4b65-aac7-864d08c92101', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('c7e02755-8298-4b65-aac7-864d08c92101', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('c7e02755-8298-4b65-aac7-864d08c92101', foundational, simulation_fidelity_can_occupy_competence_kernel).
narrative_ontology:cs_axiom_status(simulation_fidelity_can_occupy_competence_kernel, holdable).
narrative_ontology:cs_axiom_grounding('c7e02755-8298-4b65-aac7-864d08c92101', simulation_fidelity_can_occupy_competence_kernel, empirically_contingent).
narrative_ontology:cs_axiom('c7e02755-8298-4b65-aac7-864d08c92101', foundational, training_compliance_certifies_competence).
narrative_ontology:cs_axiom_status(training_compliance_certifies_competence, holdable).
narrative_ontology:cs_axiom_grounding('c7e02755-8298-4b65-aac7-864d08c92101', training_compliance_certifies_competence, conventional).
narrative_ontology:cs_reference_frame('c7e02755-8298-4b65-aac7-864d08c92101', post_tmi_simulation_mandate).
narrative_ontology:cs_drift_state('c7e02755-8298-4b65-aac7-864d08c92101', contemporary_fidelity_escalation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7e02755-8298-4b65-aac7-864d08c92101', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_compliance_officers).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, operational_units_relying_on_degraded_competence).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, taxpayers_funding_ineffective_training).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, high_fidelity_simulation_occupies_competence_kernel).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, training_compliance_equals_competence_assurance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell simulation platforms, scenarios, and fidelity upgrades to regulated industries. Revenue scales with mandated frequency and fidelity requirements. They shape fidelity standards through industry consortiums and lobby for expanded simulation mandates.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Administer training compliance programs, certify simulation sufficiency, and report training hours to regulators. Their professional standing and budget authority depend on the simulation sufficiency reading being accepted. They set the curriculum and define what counts as 'occupation'.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_compliance_officers, agenda_setter,
    institutional, biographical, constrained, national).

% Mandate simulation-based training as the primary compliance mechanism for competence assurance. They gain legible, auditable metrics (hours, scenarios, fidelity levels) that satisfy oversight demands. Their regulatory simplicity benefits from a single measurable standard.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation, beneficiary).

% Required to complete simulation hours to maintain certification. They bear the time cost, the cognitive load of low-transfer drills, and the risk of skill decay that simulation does not prevent. Their professional identity is fused to certification — leaving the profession is the only exit, making them identity-locked to the constraint.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Receive operators whose certified competence does not match operational reality. They bear the cost of silent skill decay — near-misses, workaround proliferation, and latent error accumulation. They cannot opt out of the certification pipeline but can supplement with informal refresher mechanisms.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operational_units_relying_on_degraded_competence, payer,
    organized, biographical, constrained, regional).

% Fund simulation programs through public budgets for nuclear, aviation, healthcare, and emergency response sectors. They pay for training that produces compliance artifacts rather than resilient competence. No individual exit; collective exit requires political mobilization against expert consensus.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, taxpayers_funding_ineffective_training, payer,
    powerless, generational, trapped, national).

% Publish evidence that simulation fidelity does not transfer to operational resilience, that skill decay persists despite compliance, and that the competence kernel requires lived consequence. They are excluded from standard-setting bodies and regulatory advisory panels where simulation sufficiency is institutionalized.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_skeptic_researchers, excluded,
    moderate, biographical, mobile, global).

% Study the gap between training compliance and operational resilience across domains. They see the full structure: simulation industry capture, regulatory convenience, operator identity lock, and the quiet accumulation of unexercised failure modes.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, auditable, scalable mechanism for organizations to demonstrate due diligence in competence maintenance — a shared language for regulators, insurers, and operators to coordinate around 'training done'.
% TRANSFER_FUNCTION: Moves budget, operator time, and regulatory legitimacy from frontline operations and public funds to simulation vendors and compliance administrations, in exchange for certification artifacts that count as competence assurance.
% ABSENT_VOICES: Operators who have experienced skill decay despite simulation compliance; families of patients or passengers affected by latent competence gaps; researchers demonstrating the transfer gap — all structurally excluded from the bodies that define 'sufficiency'.
% DISAPPEARANCE_RATIONALE: If simulation sufficiency vanished overnight, regulators would lose their primary compliance metric, vendors would lose mandated revenue streams, and operators would lose their certification pathway. The competence assurance system would be forced to confront the transfer gap directly — likely producing a messy, multi-mechanism, non-standardized regime (resembling the hybrid_occupation reading).
% FOUNDING_PROBLEM: After high-consequence accidents (Three Mile Island, Bhopal, early aviation hull losses), organizations needed a demonstrable, scalable way to prove operators could handle rare catastrophic scenarios without waiting for real disasters.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (demonstrable rare-event readiness) is attested by accident investigation boards and early simulator pioneers. The simulation industry and regulatory bodies attest it remains live. Independent safety scientists and operator unions attest the problem has shifted: the current constraint solves 'auditable training' not 'rare-event readiness', and the founding problem is substantially dead while the arrangement persists.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the constraint moves substantial resources (vendor revenue, operator time, public funds) to produce compliance artifacts whose operational validity is contested. Suppression (0.62) is structural: alternative competence mechanisms (line audits, procedural refreshers, incident debrief culture, hybrid regimes) are excluded from regulatory recognition. Theater ratio (0.71) is very high — the majority of simulation activity performs 'training done' rather than exercising the competence kernel. Accessibility collapse (0.58) is moderate: alternatives exist but are illegible to regulators. Resistance (0.44) is moderate but growing from operator unions and independent researchers.
 *
 * PERSPECTIVAL GAP:
 *   From the compliance officer seat, the constraint is a rope — it solves the coordination problem of 'how do we all agree competence is maintained?' From the frontline operator seat, it is a snare — they pay the cost of a performance that does not protect them. From the simulation vendor seat, it is a business model protected by regulatory capture. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and compliance officers are structural beneficiaries (d ~ 0.15-0.25): they collect revenue and authority from the constraint. Regulatory bodies are dual-positioned: they gain auditable simplicity (beneficiary) but also bear legitimacy risk when competence gaps surface (payer). Frontline operators are identity-locked targets (d ~ 0.85): professional identity fused to certification, no exit without career abandonment. Operational units are constrained payers (d ~ 0.7): they bear downstream consequences but can supplement informally. Taxpayers are trapped payers (d ~ 0.95): no individual exit, collective exit requires overcoming expert consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (demonstrable rare-event readiness) has substantially shifted. The arrangement now solves 'auditable training compliance' — a different problem. The mandate has outlived its original function but persists because it serves the simulation industry, regulatory convenience, and compliance officer authority. This is a classic mandatrophy case: the constraint's persistence is explained by who benefits from its continuation, not by its success at the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_gap_magnitude,
    'What is the actual transfer efficiency of simulation fidelity to operational resilience in rare catastrophic scenarios?',
    'Longitudinal studies tracking operators from simulation certification through operational careers, measuring latent error rates and near-miss clusters in units with high simulation compliance vs. those using hybrid mechanisms.',
    'If transfer efficiency is low (<0.3), the constraint''s coordination function is largely theatrical and extraction is near-total. If moderate (0.3-0.6), the tangled rope classification holds — genuine but incomplete coordination with substantial extraction. If high (>0.6), the rope classification becomes plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transfer_gap_magnitude, empirical, 'The core empirical uncertainty: does simulation actually occupy the competence kernel?').

omega_variable(
    fidelity_vs_frequency_tradeoff,
    'Is the competence kernel better occupied by low-frequency high-fidelity simulation or high-frequency low-fidelity procedural reinforcement?',
    'Controlled comparison of competence decay curves under different training regimes across matched operational domains.',
    'If low-fidelity high-frequency dominates, the simulation_vendor business model (selling fidelity upgrades) is extractive overhead. If high-fidelity is necessary, vendor revenue is partly coordination cost. This determines how much of the measured extraction is structural vs. rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_vs_frequency_tradeoff, empirical, 'Whether the extraction flows to vendors as rent or as necessary coordination cost.').

omega_variable(
    identity_lock_mechanism,
    'Is operator identity-lock to certification primarily professional (career path dependence), regulatory (license requirement), or psychological (self-concept as ''certified competent'')?',
    'Operator surveys and exit interviews during voluntary attrition; analysis of re-certification behavior when simulation mandates are relaxed in adjacent jurisdictions.',
    'If primarily psychological, the constraint''s suppression is partially internalized — operators enforce it on themselves. If primarily regulatory, suppression is structural and removable by policy change. This changes the effective suppression calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'The mechanism binding operators to the constraint — determines suppression composition.').

omega_variable(
    cs_framing_competence_kernel,
    'Is the ''competence kernel'' a genuine stabilizable entity that can be occupied, or a reified metaphor that masks irreducible variability in rare-event performance?',
    'Compare how the kernel concept functions across sibling readings: simulation_sufficiency treats it as a stabilizable target; real_incident_necessity treats it as inaccessible to simulation; hybrid_occupation treats it as requiring continuous multi-mechanism exercise. If the kernel dissolves under scrutiny, all three readings share a false premise.',
    'If the competence kernel is not a coherent structural entity, the entire kernel family rests on a category error. The constraint would be a snare built on aphantom target. If the kernel is real but only partially occupiable by simulation, the tangled rope classification holds with a specific coordination boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_competence_kernel, conceptual, 'Whether the central ontological commitment of the kernel family is structurally sound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coss_tr_t1979, competence_occupation__simulation_sufficiency, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(coss_tr_t1989, competence_occupation__simulation_sufficiency, theater_ratio, 1989, 0.38).
narrative_ontology:measurement(coss_tr_t1999, competence_occupation__simulation_sufficiency, theater_ratio, 1999, 0.52).
narrative_ontology:measurement(coss_tr_t2009, competence_occupation__simulation_sufficiency, theater_ratio, 2009, 0.61).
narrative_ontology:measurement(coss_tr_t2019, competence_occupation__simulation_sufficiency, theater_ratio, 2019, 0.68).
narrative_ontology:measurement(coss_tr_t2025, competence_occupation__simulation_sufficiency, theater_ratio, 2025, 0.71).

% Extraction over time
narrative_ontology:measurement(coss_be_t1979, competence_occupation__simulation_sufficiency, base_extractiveness, 1979, 0.22).
narrative_ontology:measurement(coss_be_t1989, competence_occupation__simulation_sufficiency, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(coss_be_t1999, competence_occupation__simulation_sufficiency, base_extractiveness, 1999, 0.48).
narrative_ontology:measurement(coss_be_t2009, competence_occupation__simulation_sufficiency, base_extractiveness, 2009, 0.58).
narrative_ontology:measurement(coss_be_t2019, competence_occupation__simulation_sufficiency, base_extractiveness, 2019, 0.65).
narrative_ontology:measurement(coss_be_t2025, competence_occupation__simulation_sufficiency, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coss_su_t1979, competence_occupation__simulation_sufficiency, suppression_requirement, 1979, 0.35).
narrative_ontology:measurement(coss_su_t1989, competence_occupation__simulation_sufficiency, suppression_requirement, 1989, 0.42).
narrative_ontology:measurement(coss_su_t1999, competence_occupation__simulation_sufficiency, suppression_requirement, 1999, 0.51).
narrative_ontology:measurement(coss_su_t2009, competence_occupation__simulation_sufficiency, suppression_requirement, 2009, 0.57).
narrative_ontology:measurement(coss_su_t2019, competence_occupation__simulation_sufficiency, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(coss_su_t2025, competence_occupation__simulation_sufficiency, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.1).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, simulation_fidelity_escalation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, training_compliance_bureaucracy_growth).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_occupation kernel. The simulation_sufficiency reading treats the kernel as occupiable via fidelity-optimized simulation. The real_incident_necessity reading treats the kernel as inaccessible to simulation. The hybrid_occupation reading treats the kernel as requiring continuous multi-mechanism exercise. These are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different claimed types — linked by network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, institutional, 0.3).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, moderate, 0.75).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
