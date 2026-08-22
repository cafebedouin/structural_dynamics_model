% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Simulation Exercises as Catastrophe-Equivalent Practice
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear power, aviation, chemical
 *   processing, surgical teams) face a structural problem: catastrophic
 *   events are rare but demand peak competence. Since the 1970s, the dominant
 *   institutional response has been to substitute simulation exercises for
 *   real catastrophes as the primary competence-maintenance mechanism. This
 *   reading asserts that well-designed simulation constitutes
 *   catastrophe-equivalent practice — that the coordination function
 *   (maintaining operational competence) is fully satisfied by the proxy,
 *   with no residual extraction from hidden victims. The constraint is
 *   claimed as a rope: a genuine coordination mechanism with low
 *   extractiveness, where regulatory bodies benefit from liability protection
 *   and operating organizations gain a defensible compliance pathway. The
 *   metrics reflect this: low suppression (participants can and do challenge
 *   exercise designs), moderate theater (some performative compliance), and
 *   extractiveness that rises slightly over 20 years as simulator fidelity
 *   investment becomes a cost center.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: Primary beneficiary (institutional/mobile) — gains liability protection and compliance verification without catastrophic testing
 *   - operating_organizations: Beneficiary (institutional/mobile) — gains defensible competence-maintenance pathway, avoids cost of real-catastrophe training
 *   - insurance_underwriters: Beneficiary (powerful/mobile) — gains risk-modeling certainty from standardized exercise regimes
 *   - frontline_operators: Symmetric participant (organized/constrained) — practices in simulation, bears training burden, but gains competence without catastrophe risk
 *   - simulation_technology_vendors: Beneficiary (organized/mobile) — captures procurement revenue from fidelity escalation
 *   - future_operators: Potential victim (powerless/trapped) — if tacit knowledge decays, inherits degraded competence; not a declared victim in THIS reading
 *   - safety_science_researchers: Observer (analytical/analytical) — studies competence retention across training regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.18).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation Exercises as Catastrophe-Equivalent Practice").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'f5566d35-2320-4fa3-8140-b426b5e10502').
narrative_ontology:cs_kernel_codification('f5566d35-2320-4fa3-8140-b426b5e10502', distributed).
narrative_ontology:cs_authority_grounding('f5566d35-2320-4fa3-8140-b426b5e10502', practice).
narrative_ontology:cs_reading_relation('f5566d35-2320-4fa3-8140-b426b5e10502', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('f5566d35-2320-4fa3-8140-b426b5e10502', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('f5566d35-2320-4fa3-8140-b426b5e10502', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('f5566d35-2320-4fa3-8140-b426b5e10502', foundational, simulation_achieves_catastrophe_equivalence).
narrative_ontology:cs_axiom_status(simulation_achieves_catastrophe_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('f5566d35-2320-4fa3-8140-b426b5e10502', simulation_achieves_catastrophe_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('f5566d35-2320-4fa3-8140-b426b5e10502', foundational, no_hidden_victim_set_from_simulation_only).
narrative_ontology:cs_axiom_status(no_hidden_victim_set_from_simulation_only, holdable).
narrative_ontology:cs_axiom_grounding('f5566d35-2320-4fa3-8140-b426b5e10502', no_hidden_victim_set_from_simulation_only, empirically_contingent).
narrative_ontology:cs_reference_frame('f5566d35-2320-4fa3-8140-b426b5e10502', simulation_sufficiency_framework).
narrative_ontology:cs_drift_state('f5566d35-2320-4fa3-8140-b426b5e10502', contemporary_high_fidelity_simulation_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('f5566d35-2320-4fa3-8140-b426b5e10502', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, insurance_underwriters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_technology_vendors).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_compliance_via_exercise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate simulation exercise regimes as the primary competence-verification mechanism. Gain liability protection — if an accident occurs, compliance with mandated exercises demonstrates due diligence. Can update standards based on operational feedback; not locked into any single exercise design.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, beneficiary,
    institutional, generational, mobile, national).

% Run mandated simulation programs for their operators. Gain a defensible, auditable competence-maintenance pathway that avoids the impossible cost and ethical barrier of training on real catastrophes. Invest in simulator fidelity as a cost of doing business; can lobby for standard changes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operating_organizations, beneficiary,
    institutional, generational, mobile, national).

% Use standardized exercise compliance as a key input to risk models and premium setting. Gain certainty from the regulatory regime's predictability. Can adjust models if evidence emerges that exercise compliance doesn't predict actual performance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, insurance_underwriters, beneficiary,
    powerful, biographical, mobile, global).

% Participate in mandated simulation exercises — invests significant time (hundreds of hours over career) in training scenarios. Gains competence and certification without exposure to real catastrophe risk. Exit is constrained: leaving the profession means abandoning specialized human capital, but operators can move between organizations using the same regulatory regime.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, beneficiary).

% Design, build, and maintain high-fidelity simulators and exercise frameworks. Capture procurement revenue from operating organizations and regulatory mandated upgrades. Compete on fidelity metrics; not structurally locked to any single regulator or operator.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% The cohort that will inherit the competence-maintenance system 20-40 years hence. If simulation-only training degrades tacit knowledge transfer, they inherit a degraded system with no say in its design. In THIS reading they are not victims (the reading asserts competence is maintained); in sibling readings they are the central victim set. Listed here as excluded to make the structural ambiguity visible.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, future_operators, excluded,
    powerless, generational, trapped, local).

% Study competence retention across training regimes using longitudinal data, cognitive task analysis, and post-incident investigations. Provide the empirical basis that could resolve the kernel contest. No stake in any reading's victory; their work informs all readings.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_science_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational competence for rare catastrophic scenarios without requiring actual catastrophes for training — solves the collective-action problem of how organizations prepare for events that must never happen but for which failure is unacceptable.
% TRANSFER_FUNCTION: Moves training investment (simulator procurement, operator time, scenario development) from operating organizations to the simulation regime, in exchange for competence certification and liability protection. No extraction from a victim set in this reading's structure.
% ABSENT_VOICES: Future operators (20-40 years hence) who would inherit any tacit knowledge degradation are not present in current regulatory deliberations. Communities downwind of high-hazard facilities who bear residual risk if competence decays are also absent. Their exclusion is structural — they cannot participate in current standard-setting.
% DISAPPEARANCE_RATIONALE: If the simulation-as-sufficiency constraint vanished overnight, regulators would lose their primary compliance-verification mechanism, operators would lose their defensible training pathway, and the field would fracture into competing claims: some would demand real-event training (catastrophe_necessity), others would adopt hybrid regimes, others would pursue fidelity thresholds. The coordination function would not disappear but its institutional instantiation would.
% FOUNDING_PROBLEM: Post-WWII high-hazard industries (nuclear, aviation, chemical) faced a structural contradiction: catastrophic accidents were unacceptable but too rare to train on. The founding problem was how to maintain peak competence for events that must never occur.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by nuclear regulatory histories (NRC, IAEA), aviation safety literature (NTSB, ICAO), and chemical process safety standards (CCPS, OSHA PSM) — all outside the direct beneficiary set of current simulation vendors. The problem remains live because catastrophic events remain rare and unacceptable across all high-hazard domains.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18 at T=20) because the simulation regime is genuinely a coordination mechanism — it solves the collective-action problem of maintaining competence without catastrophe. The slight rise over 20 years reflects fidelity escalation as a procurement cost, not extraction from a victim set. Suppression is low (0.12) because operators and organizations can challenge exercise design, propose alternatives, and regulators have adapted standards in response to feedback (e.g., LOFT program evolution in nuclear). Theater ratio (0.22) captures the performative fraction: some exercises are run for audit trails rather than learning, but the majority have functional training value. Accessibility collapse (0.35) is moderate — alternatives (real-event training, hybrid regimes) exist but are structurally disfavored by liability and cost. Resistance (0.28) reflects academic and operational debate about fidelity sufficiency, not coercive pushback.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies, operating organizations, and insurers are beneficiaries (d near 0.0) — they collect liability protection, compliance certainty, and risk-model stability from the constraint. Frontline operators sit near symmetric (d ~ 0.5) — they invest training time and gain competence without catastrophe exposure. Simulation vendors benefit (d low) from procurement flow. Future operators are not declared as victims in THIS reading — the reading's core premise is that no victim set exists because competence is maintained. The sibling readings (catastrophe_necessity, hybrid_degradation) would declare future operators as victims; this reading structurally forecloses that victim set by asserting sufficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy signature: the founding problem (maintaining competence without catastrophe) remains live, the coordination function is actively maintained through fidelity investment, and no party captures extraction without contributing to the function. The slight extractiveness rise is a procurement dynamic, not a rent-seeking capture. If generational tacit knowledge decay proves real (omega_3), the constraint would acquire a hidden victim set and shift toward tangled_rope — but that is a sibling reading's claim, not this reading's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the sufficiency of simulation exercises a structural feature of high-reliability practice, or a contested reading of the catastrophe_proxy_sufficiency kernel?',
    'Empirical longitudinal comparison of competence retention across organizations using simulation-only vs. hybrid (simulation + real-event) training regimes over generational timescales (20+ years).',
    'If simulation-only regimes show competence decay at generational scale, this reading is a false summit — the constraint shifts from rope to tangled_rope with victims emerging (future operators inheriting degraded tacit knowledge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading instantiates a distinct constraint or merely reflects one framing of an unresolved kernel.').

omega_variable(
    fidelity_measurement_underdetermination,
    'Does ''catastrophe-equivalent'' simulation fidelity have a stable, domain-independent metric, or is the threshold itself a moving target that regulators and operators negotiate?',
    'Cross-domain analysis of simulator certification standards (nuclear, aviation, chemical, medical) to identify whether fidelity metrics converge or diverge structurally.',
    'If fidelity thresholds are negotiated rather than discovered, the constraint''s coordination function embeds a regulatory capture vector — extraction would be higher than the authored 0.18.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_measurement_underdetermination, empirical, 'Whether the coordination standard itself is extractively pliable.').

omega_variable(
    generational_tacit_knowledge_decay,
    'Does simulation-only training produce measurable decay in stress-response capacity and tacit knowledge transfer over generational timescales (20-40 years)?',
    'Cohort studies comparing operator performance in actual emergencies across organizations with different training histories; post-incident cognitive task analysis.',
    'If decay is real and structurally inevitable, the constraint has a hidden victim set (future operators, future publics) and the reading shifts toward hybrid_degradation_reading or catastrophe_necessity_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_tacit_knowledge_decay, empirical, 'Hidden victim emergence over generational time — the central structural ambiguity between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tr_t5, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tr_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tr_t15, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tr_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_be_t5, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_be_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_be_t15, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_be_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_proxy_sufficiency kernel. The readings differ on whether simulation suffices (this reading), whether only real catastrophe suffices (catastrophe_necessity), whether simulation maintains procedure but not tacit knowledge (hybrid_degradation), and whether sufficiency depends on a technology-dependent fidelity threshold (simulation_fidelity_threshold). The epsilon values diverge: this reading (0.18) vs catastrophe_necessity (would be ~0.05 for real catastrophe but with massive suppression) vs hybrid_degradation (would be ~0.35 with hidden victims) vs fidelity_threshold (variable). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
