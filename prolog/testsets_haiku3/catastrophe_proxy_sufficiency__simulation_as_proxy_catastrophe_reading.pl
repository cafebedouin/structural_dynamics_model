% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation-as-Proxy-Catastrophe Doctrine: Operational Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability operations (nuclear power, aviation, maritime) face an
 *   irreducible governance problem: they must demonstrate competence to
 *   handle catastrophic scenarios, but catastrophes are rare and ethically
 *   unacceptable as tests. This reading of the catastrophe-proxy-sufficiency
 *   kernel asserts that simulation exercises, when designed and executed with
 *   sufficient fidelity and psychological authenticity, constitute
 *   catastrophe-equivalent stress and uncertainty — enough to maintain
 *   operational competence indefinitely without requiring actual
 *   catastrophes. The doctrine legitimates simulation-based competence
 *   validation and creates regulatory authority around simulation adequacy
 *   standards. Regulatory bodies benefit by having a defensible path to
 *   oversight; operators benefit by having a repeatable, controlled
 *   competence-validation mechanism; operational personnel and affected
 *   populations pay through simulation burden and dependency on the
 *   doctrine's empirical truth. The kernel contest is whether
 *   catastrophe-equivalence is achievable through simulation or whether real
 *   catastrophes remain irreducibly necessary.
 *
 * KEY AGENTS:
 *   - regulatory_oversight_bodies: Institutional beneficiary (liability protection, interpretive authority) — high power, generational horizon
 *   - high_reliability_operators: Organized beneficiary and secondary payer (competence validation + operational burden) — high power, biographical horizon
 *   - operational_personnel: Moderate-power payers (repeated simulation stress, time cost) with secondary benefit (competence maintenance, career validation)
 *   - affected_population: Powerless beneficiaries (catastrophe risk reduction) — trapped, dependent on competence maintenance
 *   - catastrophe_necessity_advocates: Excluded voices (academic/institutional challenge to sufficiency premise) — powerful but structurally excluded
 *   - simulation_fidelity_engineers: Observers providing technical evidence on whether fidelity thresholds are crossed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.15).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation-as-Proxy-Catastrophe Doctrine: Operational Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5').
narrative_ontology:cs_kernel_codification('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', distributed).
narrative_ontology:cs_authority_grounding('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', expertise).
narrative_ontology:cs_interpretation_layer_present('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5').
narrative_ontology:cs_reading_relation('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', foundational, simulation_achieves_catastrophe_equivalence).
narrative_ontology:cs_axiom_status(simulation_achieves_catastrophe_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', simulation_achieves_catastrophe_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', foundational, competence_indefinite_maintenance_via_simulation).
narrative_ontology:cs_axiom_status(competence_indefinite_maintenance_via_simulation, holdable).
narrative_ontology:cs_axiom_grounding('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', competence_indefinite_maintenance_via_simulation, empirically_contingent).
narrative_ontology:cs_reference_frame('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', simulation_based_competence_validation).
narrative_ontology:cs_drift_state('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', contemporary_accumulating_simulation_exercises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('527d6bd7-d3d0-4a1d-a97a-c3da83afc4a5', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_engineering_discipline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, affected_population).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can mandate simulation-based competence maintenance rather than requiring real-world catastrophe events to validate operator readiness. This reduces liability exposure for regulatory inaction (no 'missed warning sign' claims when operators have been drilled) and creates institutional path for risk governance without catastrophic harm as the evidence source. Holds interpretive authority over simulation adequacy standards.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_oversight_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Must conduct and document regular simulation exercises to satisfy regulatory compliance. They benefit from the doctrine because it provides a path to maintain demonstrated competence without awaiting (or surviving) actual catastrophes. They pay through exercise design time, infrastructure investment, and personnel commitment to scenario-based training. The constraint reduces catastrophe probability, benefiting them as operators and stakeholders.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_operators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_operators, payer).

% Bear the recurring cost of simulation participation: time spent in exercise scenarios, attention diverted from nominal operations, and psychological stress from high-fidelity simulations that mimic catastrophe conditions. They benefit from competence maintenance that reduces actual catastrophe risk during their tenure and from skills validation that supports career advancement. Their exit from simulation is not available; refusal is grounds for role termination.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_personnel, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_personnel, beneficiary).

% Depend on operator competence for safety. The doctrine's viability determines whether that competence is maintained at levels sufficient to prevent catastrophe. They cannot exit the geographic scope and have no voice in simulation design or adequacy standards. Their interest is wholly derivative: if simulation is sufficient, they benefit; if it is insufficient, they bear the catastrophic harm.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, affected_population, beneficiary,
    powerless, biographical, trapped, regional).

% Academic and institutional voices arguing that only real catastrophic events provide irreducible stress/uncertainty needed to maintain genuine adaptive competence; simulation inevitably flattens threat perception and tacit knowledge. They are structurally excluded from operational governance but present alternative readings that contest the sufficiency premise. Their exclusion is structural because accepting their argument would require fundamental reorganization of safety governance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_advocates, excluded,
    powerful, generational, constrained, global).

% Design and operate simulation infrastructure; they observe the constraint's operation and provide technical evidence about fidelity thresholds and realism limits. Their role is commentary on the empirical claim: can simulation cross fidelity boundaries to become catastrophe-equivalent? They are neither extracting from the constraint nor directly benefiting from its persistence.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_engineers, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared interpretive standard for what constitutes 'demonstrated competence' in high-reliability operations: operational actors can substitute simulation exercise completion for catastrophe-event evidence of readiness. Solves the coordination problem of how to validate safety-critical operational readiness without requiring actual catastrophes as the empirical test.
% TRANSFER_FUNCTION: Transfers the burden of competence validation from catastrophe-occurrence (external, rare, harmful) to simulation-completion (internal, repeatable, controlled). The constraint moves operator time/resources into mandatory simulation participation and moves regulatory authority into control over simulation adequacy standards.
% ABSENT_VOICES: Operational personnel subject to repeated high-stress simulation have limited voice in scenario design or adequacy thresholds; they participate but do not set the standard. Affected populations external to the operating organization have no voice in whether the constraint is invoked or how. Catastrophe-necessity advocates are structurally excluded because accepting their premise would dissolve this reading.
% DISAPPEARANCE_RATIONALE: If simulation-as-proxy doctrine disappeared, regulatory bodies would need an alternative path to validate competence: either waiting for catastrophic events to occur naturally (unacceptable risk), or developing alternative competence-validation schemes (expensive, time-consuming institutional redesign). The doctrine's absence would force fundamental reorganization of safety governance and competence certification; operations cannot persist under either alternative without major structural change.
% FOUNDING_PROBLEM: How can safety-critical operations (nuclear, aviation, maritime) maintain demonstrated competence in handling catastrophic scenarios without requiring actual catastrophes to occur? The doctrine was constructed to solve the impossible situation: catastrophe as the only proof of readiness is ethically intolerable and practically rare; a path was needed to gather equivalent evidence through controlled, repeatable means.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies (NRC, FAA, IMO), high-reliability operations safety officers, and operational safety engineers all attest the founding problem is live and urgent: competence validation is a continuous governance requirement, and catastrophes are both too rare and too harmful to serve as tests. Catastrophe-necessity academics contest that the problem is genuinely solved by simulation, but do not contest that the problem itself exists and motivates the constraint.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint's primary function is genuinely coordinative: it solves a real problem (how to validate competence without catastrophes). The beneficiaries (regulatory bodies, operators) are not extracting diffuse costs from vulnerable populations; rather, they are coordinating around a shared validation mechanism. Suppression is low (0.15) because the doctrine operates through institutional mandate, not through suppressed alternatives — simulation participation is enforced, but alternative paths (catastrophe-occurrence) are not deliberately foreclosed so much as recognized as unacceptable on ethical grounds. Theater ratio is rising moderately (0.08→0.22) because as simulation exercises accumulate without corresponding real-world events, the question of whether simulation rehearsal is genuine stress-response training or performative compliance grows. The measurement series shows extractiveness and theater stabilizing after ~30 time units, suggesting the constraint enters a steady state. Accessibility_collapse is low (0.35) because alternative competence-validation schemes remain theoretically available (even if practically difficult); resistance is moderate (0.58) because catastrophe-necessity advocates continue to contest the sufficiency claim and some operators resent simulation burden as disproportionate to risk reduction.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-body seat, this constraint is genuine coordination: it solves an urgent, shared problem and creates mutual benefit. From the operational-personnel seat, it is burden-creating compliance: they bear recurring stress, time cost, and participation mandates without proportional voice in adequacy standards. From the catastrophe-necessity-advocate seat, it is a false solution: a doctrine that appears to solve the problem (competence validation without catastrophe) but actually creates an illusion of readiness that becomes catastrophically dangerous when real-world complexity exceeds simulation fidelity. The engine should compute these divergent types from the structural data: operators and regulatory bodies sit near rope (genuine coordination with enforcement); operational personnel sit near tangled rope (coordinated alongside extraction of their time/stress); catastrophe advocates' position is incoherent within the doctrine's logic (the doctrine forecloses their core claim in any governance framework that accepts simulation-equivalence). The perspectival gap is real and structural, not a measurement artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies hold high directionality toward beneficiary (d~0.1): they set the standards, collect authority, and bear minimal cost. Operators hold symmetric directionality (d~0.5): they benefit from competence validation and bearing operational burden roughly offset each other. Operational personnel hold directionality toward target (d~0.7): they bear the recurring simulation stress without proportional control over standards. Affected populations hold directionality toward beneficiary (d~0.15) asymmetrically: they benefit from catastrophe-risk reduction but have no voice in whether the doctrine applies. Catastrophe-necessity advocates hold directionality toward target (d~0.8) because accepting this reading's core premise delegitimizes their alternative, yet they have no formal seat in competence-validation governance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to validate competence without catastrophes) is live and will remain live as long as safety-critical operations exist. The doctrine does not suffer from mandate obsolescence; rather, it faces an ongoing empirical challenge: is simulation-equivalence actually achievable? The theater ratio's slow rise (0.08→0.22) suggests early signs of mandatrophy through displacement: as simulation accumulates and real catastrophes remain absent, there is growing institutional investment in the simulation apparatus itself (exercise design, scenario libraries, personnel training) that can create incentives to conduct simulations for their own sake rather than to validate competence. This is not mandate death, but mandate drift — the coordination function remains legitimate, but the enforcement machinery develops its own bureaucratic persistence independent of whether simulation is actually maintaining competence. Mandatrophy resolution would require periodic cross-validation against real-world outcomes or catastrophe-near-miss events, which the doctrine structurally depends on NOT occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_saturation,
    'Can simulation exercises achieve and maintain psychological and operational stress/uncertainty levels equivalent to real catastrophe, or is there an irreducible fidelity gap that grows as operators accumulate simulation experience?',
    'Long-term longitudinal study comparing operator response patterns in simulations vs. real-world near-miss events or rare catastrophes; stress biomarkers; post-exercise debriefs from operators across generational cohorts; evidence from operations that have experienced real catastrophes vs. simulation-only operations.',
    'If achievable and stable: simulation-as-proxy doctrine is empirically sound and extractiveness remains low. If fidelity gap is irreducible or grows: competence maintenance fails under the doctrine''s premise, and catastrophe-necessity reading gains empirical force — extractiveness rises (doctrine becomes false-sufficient safety theater), and classification shifts toward piton (maintained by inertia, vulnerable to mandate death).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_saturation, empirical, 'Whether simulation stress is truly catastrophe-equivalent or irreducibly degraded.').

omega_variable(
    generational_tacit_knowledge_decay,
    'Does tacit knowledge (intuitive threat recognition, adaptive response, situational judgment) degrade over generational timescales even when procedural competence is maintained through simulation?',
    'Comparison of operator decision quality and adaptation speed in novel scenarios between operators trained entirely on simulation vs. those with real-world catastrophe exposure; analysis of near-miss reports and error patterns across generational cohorts; ethnographic study of expertise transmission in high-reliability organizations.',
    'If tacit knowledge decays: simulation maintenance is insufficient for full competence, and hybrid_degradation_reading becomes empirically supported — beneficiary set shrinks (regulatory bodies lose liability protection), victims emerge (populations depending on operators trained only on simulation), and constraint shifts toward snare or tangled-rope (extraction of time/resources for incomplete competence maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_tacit_knowledge_decay, empirical, 'Whether simulation-maintained competence includes tacit knowledge or only procedural knowledge.').

omega_variable(
    kernel_reading_contest_empirical_dependence,
    'Is the contest between simulation_as_proxy_catastrophe_reading and catastrophe_necessity_reading fundamentally empirical (decidable by evidence) or fundamentally normative (different value commitments about acceptable risk)?',
    'Examine whether both readings make falsifiable empirical claims (simulation stress = catastrophe stress) or whether one reading includes irreducible value commitments (catastrophe exposure is ethically unacceptable even if empirically necessary). Interview both reading communities on what evidence would convince them to switch readings.',
    'If empirical: the reading contest is about what simulation can achieve; evidence from omega_1 and omega_2 will resolve it. If normative: the readings coexist because they rest on different value frameworks (efficiency/safety vs. authenticity/humility); empirical evidence becomes advisory rather than decisive, and the constraint persists as contested indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_empirical_dependence, conceptual, 'Whether the kernel contest is empirically resolvable or value-dependent.').

omega_variable(
    regulatory_capture_through_simulation_authority,
    'Does regulatory authority over simulation adequacy standards create opportunities for regulatory capture, where operators influence simulation design to lower the fidelity bar, or where regulatory bodies develop institutional dependence on simulation apparatus and lose incentive to validate sufficiency?',
    'Regulatory-oversight audits of simulation-standard-setting processes; comparison of simulation rigor across jurisdictions with different regulatory structures; evidence of pressure from operators to reduce simulation burden; analysis of regulatory bodies'' investment in simulation infrastructure and incentives to maintain it.',
    'If capture is present: the doctrine''s sufficiency claim becomes dependent on regulatory processes that may be compromised; extractiveness rises (regulatory authority becomes rent-seeking), theater ratio rises (apparatus becomes performative), and constraint shifts toward tangled_rope or snare. If capture is absent or mitigated: the doctrine''s empirical claim remains testable and the constraint''s classification is determined by omega_1 and omega_2.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_through_simulation_authority, empirical, 'Whether regulatory authority over simulation standards is subject to capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t5, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t5, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t5, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings decompose the contested kernel 'catastrophe_proxy_sufficiency' into structurally distinct claims with different empirical status and extractiveness profiles. simulation_as_proxy_catastrophe_reading (this story) asserts categorical sufficiency and low extractiveness; catastrophe_necessity_reading asserts categorical insufficiency and potential extraction (false sufficiency theater); hybrid_degradation_reading asserts partial sufficiency with generational degradation (intermediate extraction); simulation_fidelity_threshold asserts technology-dependent sufficiency (extractiveness depends on fidelity achievement). The four readings coexist as live positions held by different regulatory jurisdictions, operator communities, and academic factions. Each reading has its own constraint_id and ε value; network edges link them as a family. Empirical resolution of the omegas may cause readings to foreclose each other or shift from coexists_with to influences relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
