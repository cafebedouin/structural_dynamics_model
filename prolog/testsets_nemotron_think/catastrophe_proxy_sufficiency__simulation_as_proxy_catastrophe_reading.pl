% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Simulation as Catastrophe-Equivalent Practice for Operational Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'simulation_as_proxy_catastrophe_reading' of the contested kernel
 *   'catastrophe_proxy_sufficiency'. The reading asserts that properly
 *   designed simulation exercises provide catastrophe-equivalent stress,
 *   uncertainty, and decision-making demands sufficient to maintain
 *   operational competence indefinitely — without requiring personnel to
 *   experience actual catastrophic events. The constraint operates as a
 *   coordination mechanism (rope): it aligns regulators, operators, and
 *   vendors around a shared, auditable practice standard that solves the
 *   impossibility of live-catastrophe training. Extractiveness is low (0.12)
 *   because the arrangement primarily coordinates — it moves resource costs
 *   predictably rather than extracting from a captive class. Suppression is
 *   minimal (0.08) because alternatives (live exercises, tabletop drills) are
 *   not actively suppressed; they are supplemented. Theater ratio is low
 *   (0.15) because the simulation function is genuine, though a modest
 *   performative layer exists in compliance-driven 'check-the-box' exercises.
 *   The reading's claim of indefinite sufficiency is its structural
 *   vulnerability — if fidelity thresholds are crossed without detection, the
 *   constraint could drift into extraction without changing form.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.08).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation as Catastrophe-Equivalent Practice for Operational Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '9d4215b3-7520-4660-baab-3e5c088358da').
narrative_ontology:cs_kernel_codification('9d4215b3-7520-4660-baab-3e5c088358da', formalized).
narrative_ontology:cs_authority_grounding('9d4215b3-7520-4660-baab-3e5c088358da', expertise).
narrative_ontology:cs_interpretation_layer_present('9d4215b3-7520-4660-baab-3e5c088358da').
narrative_ontology:cs_reading_relation('9d4215b3-7520-4660-baab-3e5c088358da', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('9d4215b3-7520-4660-baab-3e5c088358da', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('9d4215b3-7520-4660-baab-3e5c088358da', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('9d4215b3-7520-4660-baab-3e5c088358da', foundational, simulation_achieves_catastrophe_equivalence).
narrative_ontology:cs_axiom_status(simulation_achieves_catastrophe_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('9d4215b3-7520-4660-baab-3e5c088358da', simulation_achieves_catastrophe_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('9d4215b3-7520-4660-baab-3e5c088358da', foundational, competence_maintained_indefinitely_without_real_events).
narrative_ontology:cs_axiom_status(competence_maintained_indefinitely_without_real_events, holdable).
narrative_ontology:cs_axiom_grounding('9d4215b3-7520-4660-baab-3e5c088358da', competence_maintained_indefinitely_without_real_events, empirically_contingent).
narrative_ontology:cs_reference_frame('9d4215b3-7520-4660-baab-3e5c088358da', simulation_sufficiency_paradigm).
narrative_ontology:cs_drift_state('9d4215b3-7520-4660-baab-3e5c088358da', contemporary_ai_enhanced_simulation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9d4215b3-7520-4660-baab-3e5c088358da', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hro_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, public_society).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, competence_maintenance_without_real_catastrophe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain liability protection and regulatory defensibility by accepting simulation-based competence demonstrations instead of requiring live catastrophic event experience. Can point to simulation records as due diligence. Exercise rule-making authority over what counts as sufficient simulation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Avoid the impossible requirement of exposing personnel to actual catastrophes for training. Maintain operational readiness through structured simulation programs that are auditable, repeatable, and scalable across shifts and generations. Bear the cost of simulation infrastructure but gain predictable competence maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hro_operators, beneficiary,
    organized, biographical, constrained, global).

% Provide the simulation platforms, scenario libraries, and fidelity validation services that make the constraint operational. Capture revenue from HROs and regulatory compliance budgets. Their product roadmap is shaped by the fidelity requirements this reading establishes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Receive training that purports to be catastrophe-equivalent without facing actual mortality risk. Invest significant time in simulation exercises; the cost is opportunity cost of operational duties. If simulation fidelity is insufficient, they bear the hidden risk of unpreparedness — but under this reading, that risk is structurally denied.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, payer).

% Benefit from HROs maintaining competence without requiring real catastrophes that would harm communities. Has no direct exit from the constraint — depends on the system working. If the reading is wrong, the cost is catastrophic failure affecting populations who never consented to the substitution.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, public_society, beneficiary,
    powerless, generational, trapped, national).

% Study the empirical boundary between simulation and real-event stress response. Produce the evidence base that either validates or falsifies the sufficiency claim. Their research programs are funded partly by the stakeholders above, creating a structural tension in the knowledge production.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_science_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining operational competence in high-reliability organizations without requiring actual catastrophic events — which are rare, uncontrolled, and ethically impermissible to stage. Simulation provides a shared, auditable, repeatable practice framework that coordinates training, certification, and regulatory compliance across shifts, generations, and organizational boundaries.
% TRANSFER_FUNCTION: Moves the burden of competence demonstration from unpredictable real-world catastrophes (which transfer uncontrolled risk to operators and public) to structured simulation programs (which transfer predictable resource costs to operators and vendors, and regulatory validation effort to regulators). No extraction from a victim class — all named seats gain net benefit under this reading.
% ABSENT_VOICES: Families and communities downstream of potential HRO failures who would bear consequences if simulation proves insufficient. They are not represented in the standards bodies or vendor forums that define fidelity thresholds. Their objection — 'you trained on a simulation, not reality' — only becomes audible after a catastrophe occurs.
% DISAPPEARANCE_RATIONALE: If the simulation-as-sufficient reading vanished overnight, HROs would lose their primary competence-maintenance mechanism. Regulators would face pressure to require live-catastrophe exposure (impossible) or accept competence decay. Operators would scramble for alternatives; simulation vendors would lose their regulatory-mandated market. The entire HRO certification ecosystem would reorganize around a different competence paradigm.
% FOUNDING_PROBLEM: Early nuclear and aviation industries faced the paradox that competence for catastrophic scenarios could only be proven by surviving catastrophes — which defeats the purpose. Simulation was developed as the only ethically and practically viable substitute, formalized in standards like ANSI/ISA-84, IAEA NS-G-2.11, and FAA Part 121 training requirements.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by nuclear regulatory histories (NRC, IAEA), aviation safety boards (NTSB, EASA), and chemical process safety standards (CCPS) — all outside the direct beneficiary set of current simulation vendors. These bodies document the original impossibility of live-catastrophe training and the regulatory adoption of simulation as the structured alternative. No major safety authority disputes the founding problem; the contest is over whether simulation has *stayed* sufficient as systems complexified.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Metrics reflect the reading's own structural self-description: a coordination mechanism that substitutes for an impossible natural process (live catastrophe training). Extractiveness is not zero because simulation infrastructure has real costs and vendor capture dynamics exist at the margin, but the dominant flow is coordination, not extraction. Suppression is low because the constraint does not forbid other training modalities — it establishes simulation as the *sufficient* baseline, not the *exclusive* method. Accessibility collapse is moderate (0.25) because alternative competence paradigms (apprenticeship, live drills, red-teaming) remain conceptually available but are institutionally marginalized by the simulation standard. Resistance (0.35) comes from frontline operators who sense the stress gap and researchers who document fidelity shortfalls — but this resistance is channeled into fidelity improvement, not constraint rejection.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute similar low-extraction types across all seats because all are declared beneficiaries. The perspectival gap this reading *cannot* see is the excluded voice: downstream communities who would bear the cost if the substitution fails. Their seat is not in the room when fidelity thresholds are set. The constraint's rope classification holds only if the substitution actually works; if it fails silently, the rope becomes a snare with the public as unacknowledged victims. This reading's structure cannot represent that failure mode — it requires a sibling reading (hybrid_degradation_reading) to surface it.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are beneficiaries under this reading: regulators gain defensible compliance, operators gain feasible competence maintenance, vendors gain a mandated market, frontline operators gain risk-free training, the public gains protected HROs. No victim set is declared because the reading structurally denies that any party loses — the substitution is framed as Pareto-improving. The engine will compute low directionality (d ≈ 0.1–0.2) for all seats, producing near-zero effective extraction. This is the reading's core claim: the constraint coordinates without extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintain competence without catastrophes) remains live — catastrophes are still rare and ethically impossible to stage. No mandatrophy is resolved because the founding problem persists. The constraint shows no signs of atrophying into piton: theater ratio is low and stable, enforcement is not required (participants voluntarily adopt simulation because it solves their coordination problem), and the coordination function is actively maintained by advancing simulation technology. The risk is not mandatrophy but *silent invalidation*: the constraint continues to coordinate beautifully while the competence it certifies quietly diverges from reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''simulation_as_proxy_catastrophe_reading'' a stable structural claim, or does it collapse into ''simulation_fidelity_threshold'' under empirical scrutiny?',
    'Longitudinal analysis of HRO incident reports correlated with simulation fidelity metrics: if competence failures correlate with fidelity plateaus, the categorical claim fails and the reading reduces to the threshold reading.',
    'If the reading collapses to fidelity_threshold, its claimed_type (rope) may hold only conditionally; the coordination function becomes technology-dependent, introducing extraction risk when vendors control fidelity upgrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether this reading''s categorical sufficiency claim is empirically distinguishable from the technology-conditional sibling reading.').

omega_variable(
    stress_equivalence_measurement,
    'Can simulation actually replicate the neurobiological stress response of real catastrophe, or does it only replicate procedural decision-making?',
    'Comparative neurophysiological studies (cortisol, heart-rate variability, amygdala activation) of operators in real emergencies vs. high-fidelity simulations, controlling for scenario complexity.',
    'If stress response is not replicated, the ''catastrophe-equivalent'' claim is structurally false for the stress-response component of competence — supporting hybrid_degradation_reading. The constraint would then have an undeclared victim set (operators who lose stress inoculation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_equivalence_measurement, empirical, 'Whether the core equivalence claim (simulation = catastrophe for stress inoculation) is physiologically valid.').

omega_variable(
    vendor_capture_of_fidelity_standards,
    'Do simulation vendors influence the regulatory definition of ''sufficient fidelity'' to create mandatory upgrade cycles that extract rent without improving competence?',
    'Trace regulatory fidelity standard revisions to vendor lobbying, patent landscapes, and revenue cycles; compare competence outcomes before/after mandated upgrades.',
    'If vendor capture is documented, the constraint acquires an extractive layer (vendors as beneficiaries extracting from operators via mandated upgrades), shifting toward tangled_rope. The current reading''s ''no victim set'' claim would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_of_fidelity_standards, empirical, 'Whether the coordination mechanism is being captured for extractive upgrade cycles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This reading and its three siblings form the catastrophe_proxy_sufficiency constraint family. They share the kernel question (what substitutes for catastrophe in competence maintenance?) but differ on the structural answer: categorical sufficiency (this reading), categorical necessity of real events (catastrophe_necessity_reading), generational degradation of tacit knowledge (hybrid_degradation_reading), technology-conditional sufficiency (simulation_fidelity_threshold). The ε values differ substantially: this reading authors ε≈0.12 (coordination); catastrophe_necessity_reading would author ε≈0.6+ for the constraint it describes (enforced live-exercise requirement); hybrid_degradation_reading authors a time-dependent ε that rises across generations; fidelity_threshold authors ε conditional on technology state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
