% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation Proxy Sufficiency (Hybrid Degradation Reading)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability industries (nuclear, aviation, surgery, chemical) have
 *   institutionalized simulation-based recertification as the primary
 *   mechanism for maintaining operator competence across generations. The
 *   hybrid degradation reading asserts that while simulation reliably
 *   maintains procedural fluency — checklist execution, standard scenario
 *   response, technical knowledge — it systematically fails to maintain tacit
 *   knowledge (pattern recognition honed by irreducible ambiguity) and
 *   stress-response capacity (cognitive function under genuine
 *   consequence-weight and time pressure). This degradation is not a bug but
 *   a structural feature: the certification regime's legitimacy and revenue
 *   model depend on simulation being treated as sufficient. The constraint is
 *   a tangled rope because it genuinely solves the coordination problem of
 *   scalable competence assurance (rope function) while extracting hidden
 *   risk from operators and the public to sustain the certification
 *   industry's revenue (snare function). Active enforcement maintains the
 *   simulation substitutability doctrine through regulatory delegation,
 *   evidence standards that exclude real-event testimony, and curriculum
 *   mandates that prioritize measurable procedural metrics over unmeasurable
 *   tacit capacities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.55).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation Proxy Sufficiency (Hybrid Degradation Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e368e4b2-5e70-44f9-b412-a722f50ba232').
narrative_ontology:cs_kernel_codification('e368e4b2-5e70-44f9-b412-a722f50ba232', distributed).
narrative_ontology:cs_authority_grounding('e368e4b2-5e70-44f9-b412-a722f50ba232', expertise).
narrative_ontology:cs_interpretation_layer_present('e368e4b2-5e70-44f9-b412-a722f50ba232').
narrative_ontology:cs_reading_relation('e368e4b2-5e70-44f9-b412-a722f50ba232', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('e368e4b2-5e70-44f9-b412-a722f50ba232', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e368e4b2-5e70-44f9-b412-a722f50ba232', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('e368e4b2-5e70-44f9-b412-a722f50ba232', foundational, tacit_knowledge_requires_real_stress).
narrative_ontology:cs_axiom_status(tacit_knowledge_requires_real_stress, holdable).
narrative_ontology:cs_axiom_grounding('e368e4b2-5e70-44f9-b412-a722f50ba232', tacit_knowledge_requires_real_stress, empirically_contingent).
narrative_ontology:cs_axiom('e368e4b2-5e70-44f9-b412-a722f50ba232', foundational, generational_degradation_inevitable_without_catastrophe).
narrative_ontology:cs_axiom_status(generational_degradation_inevitable_without_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('e368e4b2-5e70-44f9-b412-a722f50ba232', generational_degradation_inevitable_without_catastrophe, empirically_contingent).
narrative_ontology:cs_reference_frame('e368e4b2-5e70-44f9-b412-a722f50ba232', simulation_adequacy_paradigm).
narrative_ontology:cs_drift_state('e368e4b2-5e70-44f9-b412-a722f50ba232', contemporary_hro_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e368e4b2-5e70-44f9-b412-a722f50ba232', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, training_institutions).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_generations_of_workers).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_at_risk).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_adequacy_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, procedural_competence_sufficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce simulation-based recertification requirements for high-reliability professions (nuclear, aviation, medical). Collect recurring certification fees and mandate proprietary simulation platforms. Their authority derives from regulatory delegation and industry acceptance of simulation metrics as competence proxies.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_bodies, beneficiary).

% Develop and sell high-fidelity simulation platforms and scenario libraries to certification bodies and operating organizations. Revenue depends on regulatory mandates for periodic simulation-based recertification. Compete on fidelity metrics that emphasize procedural replication over stress-response realism.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Deliver mandated simulation training curricula. Depend on certification body accreditation for legitimacy and student enrollment. Curricula are shaped by certification requirements, which prioritize procedural checklist completion over open-ended stress inoculation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, training_institutions, beneficiary,
    organized, biographical, constrained, regional).

% Undergo periodic simulation recertification that maintains their procedural fluency but does not replicate the irreducible uncertainty, time pressure, and consequence-weight of real catastrophic events. Their professional identity is fused to certification status; exit means career termination. Bear the hidden cost of atrophied tacit judgment when real anomalies occur.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Inherit a safety culture where simulation substitution has been normalized for decades. Enter professions where mentors lack real catastrophe experience, and the training system they encounter treats simulation metrics as ground truth. No structural mechanism exists for them to demand real-event exposure; they are born into the degraded regime.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_generations_of_workers, payer,
    powerless, generational, trapped, global).

% Bear the consequences when degraded operator judgment meets real catastrophic scenarios (nuclear release, aviation disaster, surgical cascade). Have no voice in certification design, no exit from societal dependence on high-reliability systems, and no mechanism to audit the simulation-to-reality transfer gap.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_at_risk, payer,
    powerless, civilizational, trapped, global).

% Study the transfer gap between simulation performance and real-event competence. Produce evidence that tacit knowledge and stress-response capacity require irreducible uncertainty exposure. Their findings are cited in certification reviews but do not alter mandated simulation curricula because the certification regime's legitimacy depends on simulation substitutability.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% Delegate certification authority to industry bodies while retaining oversight mandate. Face political pressure to demonstrate safety assurance without disrupting critical infrastructure operations. Accept simulation metrics as auditable proxies because real catastrophe exposure is politically and ethically untenable as a regulatory requirement.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, auditable, and ethically permissible mechanism for maintaining baseline procedural competence across high-reliability professions without requiring practitioners to survive real catastrophic events. Solves the coordination problem of how to assure society that operators remain competent when real catastrophes are rare.
% TRANSFER_FUNCTION: Moves certification revenue and training fees from operators and operating organizations to certification bodies, simulation vendors, and training institutions. Moves risk from the certification regime (which would bear legitimacy loss if competence gaps were acknowledged) to frontline operators and the public (who bear the consequence when simulation-trained judgment fails under real catastrophe stress).
% ABSENT_VOICES: Operators who have experienced real catastrophes and can attest to the simulation-to-reality gap are structurally excluded from certification standard-setting bodies. Their testimony is treated as anecdotal rather than systematic evidence. Families of victims from events where simulation-trained crews failed are excluded from regulatory review processes. The certification industry controls the evidence standards that would admit such voices.
% DISAPPEARANCE_RATIONALE: If simulation-based recertification mandates vanished overnight, high-reliability industries would face immediate legitimacy crisis and would be forced to develop alternative competence assurance mechanisms — likely involving structured apprenticeship under veterans with real-event experience, controlled stress-exposure training, and explicit acknowledgment of the simulation transfer gap. The certification industry's revenue model would collapse; simulation vendors would pivot or fail. The world would rearrange around the admission that simulation is necessary but insufficient.
% FOUNDING_PROBLEM: Post-WWII expansion of nuclear, aviation, and chemical industries created demand for scalable operator competence assurance. Real catastrophes were too rare and ethically unacceptable as training events. Simulation technology (initially physical, later computational) offered a controllable, repeatable, and auditable substitute. The founding problem was: how to certify competence at scale without waiting for rare real events.
% FOUNDING_PROBLEM_CORROBORATION: Early simulation pioneers (e.g., Link Trainer lineage, early nuclear control room simulators) explicitly framed simulation as a supplement to, not replacement for, operational experience — documented in contemporary engineering reports and oral histories collected by the Smithsonian's National Air and Space Museum and the Nuclear Energy Agency's historical archives. Current certification bodies cite the same lineage but drop the 'supplement' qualifier. Independent safety science reviews (e.g., NASA's 2019 Simulation Fidelity Assessment, IAEA's 2021 Human Performance in Simulation report) corroborate that the founding problem acknowledged simulation's limits, which the current regime suppresses.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the certification industry captures recurring revenue from a mandate whose adequacy is empirically contested and whose failure mode (degraded tacit judgment) is externalized to operators and the public. Suppression (0.55) is moderate: the constraint persists not through overt coercion but through evidence standards, professional identity fusion, and the political impossibility of mandating real catastrophe exposure. Theater ratio (0.42) is significant: a growing share of simulation activity performs 'competence demonstration' for auditors rather than building genuine stress inoculation. Accessibility collapse (0.62) is moderately high: alternative competence pathways (mentored real-event exposure, controlled stress inoculation) exist but are structurally marginalized by certification requirements. Resistance (0.48) is moderate: safety science produces consistent evidence of the transfer gap, but the certification regime controls the standards of evidence that would legitimize that resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the certification industry's seat, the constraint is a rope: a genuine coordination solution that scales competence assurance ethically. From frontline operators' seat, it is a snare: they pay with degraded capacity for a credential they cannot refuse. From future generations' seat, it is a piton in the making: the degradation accumulates silently until a real catastrophe reveals the gap. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality that no single seat experiences uniformly.
 *
 * DIRECTIONALITY LOGIC:
 *   Certification bodies, simulation vendors, and training institutions are structural beneficiaries (d near 0.0-0.2): they collect revenue, set standards, and control the evidence regime. Their exit options are arbitrage/mobile/constrained — they can pivot to adjacent markets. Frontline operators are identity-locked payers (d near 0.8-0.9): their professional identity is fused to certification status; exit means career death. They bear the hidden cost of atrophied judgment. Future generations and the public are trapped payers (d near 1.0): no structural voice, no exit from dependence on high-reliability systems. Safety researchers and regulators are analytical/observer seats (d near 0.5): they see the structure but lack leverage to alter it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scalable competence assurance without real catastrophes) remains live — real catastrophes are still rare and ethically unacceptable as training events. However, the arrangement has drifted from 'simulation as supplement' to 'simulation as sufficient proxy.' The mandatrophy is unresolved: the original coordination function persists, but a hidden extraction function (revenue from mandated substitutability) has layered on top. The constraint is not a pure snare because simulation does maintain procedural competence; it is not a pure rope because the extraction is structural and concealed. Tangled rope correctly captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_identity,
    'Is the catastrophe_proxy_sufficiency kernel a single contested claim or a family of structurally distinct constraints?',
    'Apply the ε-invariance test: if measuring ''simulation sufficiency'' via procedural metrics yields low ε but measuring via tacit/stress metrics yields high ε, the kernel decomposes into multiple constraints. This reading asserts decomposition has already occurred (four readings = four constraints).',
    'If the kernel is one constraint, readings are observer perspectives; if decomposed, each reading is a separate constraint with its own ε, beneficiaries, and classification. This story treats it as decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_identity, conceptual, 'Kernel decomposition vs. observer perspectivalism').

omega_variable(
    tacit_stress_measurement_gap,
    'Can tacit knowledge degradation and stress-response atrophy be quantified independently of real catastrophe occurrence?',
    'Longitudinal studies of operator cohorts with/without real-event exposure, using cognitive task analysis, physiological stress markers, and anomaly-response fidelity in high-fidelity simulation. Requires breaking the certification industry''s control over competence metrics.',
    'If measurable, the degradation becomes auditable and the certification regime''s evidence standards face empirical challenge. If not measurable, the reading remains a conceptual claim vulnerable to dismissal as unscientific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_stress_measurement_gap, empirical, 'Measurability of the hidden degradation mechanism').

omega_variable(
    certification_industry_capture_extent,
    'To what extent does the certification industry actively suppress evidence of the simulation transfer gap versus passively benefiting from an evidence regime it inherited?',
    'Documentary analysis of certification standard revision histories, lobbying records, and evidence-exclusion decisions. Testimony from former certification body staff.',
    'Active suppression raises extraction and suppression scores; passive benefiting suggests the constraint is drifting toward piton (inertial maintenance). Distinguishes tangled_rope (active enforcement of extraction) from piton (theatrical maintenance of atrophied function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(certification_industry_capture_extent, empirical, 'Active vs. passive beneficiary agency in maintaining the constraint').

omega_variable(
    generational_timescale_operationalization,
    'What constitutes a ''generation'' in high-reliability professions for degradation measurement?',
    'Define via cohort turnover rates, mentorship chain length, and institutional memory half-life. Nuclear: ~25 years (plant license cycles). Aviation: ~15 years (type rating cycles). Surgery: ~20 years (attending-to-attending transmission).',
    'The interval''s 60-year span assumes ~2-3 generations. If generations are shorter, degradation accelerates; if longer, the constraint may appear more stable than it is. Affects temporal measurement interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_timescale_operationalization, conceptual, 'Operational definition of generational timescale across HRO domains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cps_hdr_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cps_hdr_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cps_hdr_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(cps_hdr_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(cps_hdr_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cps_hdr_tr_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(cps_hdr_tr_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(cps_hdr_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cps_hdr_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cps_hdr_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(cps_hdr_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cps_hdr_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(cps_hdr_be_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(cps_hdr_be_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cps_hdr_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(cps_hdr_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(cps_hdr_su_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(cps_hdr_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(cps_hdr_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(cps_hdr_su_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(cps_hdr_su_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the catastrophe_proxy_sufficiency kernel into four readings with distinct ε values and beneficiary/victim structures. The hybrid_degradation_reading asserts partial sufficiency with hidden degradation (tangled_rope, ε=0.68). The simulation_as_proxy_catastrophe_reading asserts full sufficiency (rope/mountain, ε≈0.1). The catastrophe_necessity_reading asserts categorical insufficiency (snare if mandated, ε≈0.8). The simulation_fidelity_threshold_reading asserts technology-dependent sufficiency (scaffold/tangled_rope, ε variable). All four share the referent (simulation-based recertification mandates) but differ in structural claim about what the mandate achieves and extracts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, powerful, 0.2).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, moderate, 0.85).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
