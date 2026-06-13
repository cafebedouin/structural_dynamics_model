% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Competence Maintenance (psychological/physiological reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, emergency medicine)
 *   maintain that genuine competence in catastrophe-response requires
 *   irreducible exposure to real-world stress and consequence. This reading
 *   asserts that simulation, no matter how advanced, cannot substitute for
 *   the embodied, stress-inoculated, tacit knowledge layers that only genuine
 *   uncertainty develops. The constraint is CLAIMED as mountain (a
 *   psychological/physiological limit on human competence) while
 *   beneficiaries exist (organizations that benefit by justifying expensive
 *   real-world stress exposure). This is a false-summit candidate: the
 *   reading may be natural law, or it may be a constructed belief that
 *   benefits organizations and justifies their institutional practices. The
 *   corpus exists to detect which.
 *
 * KEY AGENTS:
 *   - Front-line operators (nuclear, aviation, medicine): personnel whose stress-inoculated competence is the constraint's stated victim
 *   - High-reliability organizations (agenda-setters): institutions that benefit by maintaining the reading and justifying expensive real-world exposure training
 *   - Extended-safety personnel (trapped payers): workers in industries where real catastrophes are rare but competence is critical; they experience skill decay in simulation-only training
 *   - Simulation technology vendors (excluded): would benefit from reading simulation as sufficient; excluded from this reading's framework
 *   - Safety regulatory authorities (observers): adjudicate training standards and competence certification based on which reading is authoritative
 *   - Organizational learning researchers (observers): generate empirical record on whether simulation is sufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.15).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.11).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Competence Maintenance (psychological/physiological reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '8957bab4-600d-427e-b635-812bfc0c4a46').
narrative_ontology:cs_kernel_codification('8957bab4-600d-427e-b635-812bfc0c4a46', implicit).
narrative_ontology:cs_authority_grounding('8957bab4-600d-427e-b635-812bfc0c4a46', extraction).
narrative_ontology:cs_interpretation_layer_present('8957bab4-600d-427e-b635-812bfc0c4a46').
narrative_ontology:cs_reading_relation('8957bab4-600d-427e-b635-812bfc0c4a46', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('8957bab4-600d-427e-b635-812bfc0c4a46', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_reading_relation('8957bab4-600d-427e-b635-812bfc0c4a46', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('8957bab4-600d-427e-b635-812bfc0c4a46', foundational, simulation_categorically_insufficient).
narrative_ontology:cs_axiom_status(simulation_categorically_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('8957bab4-600d-427e-b635-812bfc0c4a46', simulation_categorically_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('8957bab4-600d-427e-b635-812bfc0c4a46', foundational, genuine_catastrophic_exposure_irreducible).
narrative_ontology:cs_axiom_status(genuine_catastrophic_exposure_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('8957bab4-600d-427e-b635-812bfc0c4a46', genuine_catastrophic_exposure_irreducible, deontological).
narrative_ontology:cs_reference_frame('8957bab4-600d-427e-b635-812bfc0c4a46', catastrophic_stress_necessity_baseline).
narrative_ontology:cs_drift_state('8957bab4-600d-427e-b635-812bfc0c4a46', contemporary_simulation_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8957bab4-600d-427e-b635-812bfc0c4a46', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations_enforcing_catastrophe_exposure).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, personnel_competence_under_extended_safety).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizational_learning_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, front_line_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, personnel_in_extended_safety_periods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Personnel whose competence depends on stress inoculation: nuclear operators, air traffic controllers, surgeons, emergency responders. They are exposed to actual catastrophic events (or their organizational equivalent) to maintain the irreducible competence layers that simulation cannot reach. Extended periods of operational calm degrade their tacit knowledge and stress-response capacity, making them less prepared when real events occur. Their 'exit' is career change; their constraint is institutional.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, front_line_operators, payer,
    moderate, biographical, constrained, local).

% Workers in industries where genuine catastrophes are rare but competence-critical: dam operations, aircraft maintenance, emergency medicine during peacetime. Simulation maintains surface procedural knowledge, but stress-response patterns, intuition calibration, and tacit judgment atrophy. They cannot voluntarily expose themselves to the stress needed to restore competence; organizational systems offer simulation, which is insufficient by this reading. They discover the insufficiency only when a real event occurs.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, personnel_in_extended_safety_periods, payer,
    powerless, biographical, trapped, local).

% Nuclear power operators, military aviation programs, aerospace manufacturing, and emergency medical systems that maintain high reliability. They benefit from the constraint because it justifies maintaining expensive, dangerous, real-world stress exposure (live drills, near-catastrophe scenarios, full-scale exercises) as essential rather than elective. The constraint's claim that simulation is insufficient validates their continued investment in stress-inoculation training, organizational redundancy for recovery, and personnel rotation through genuine-risk operations. They can exit by accepting lower standards or adopting pure-simulation curricula; most do not.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations, beneficiary).

% Companies and research programs developing high-fidelity simulators would benefit from a reading that simulation sufficiency increases with technology (the sibling reading: simulation_fidelity_threshold). This reading excludes them from the conversation by asserting categorical insufficiency. They would argue that emerging simulation technology (VR, AI-driven dynamic scenarios, neurological stress feedback) crosses the competence-maintenance threshold, but their voice is structurally absent from organizations that adopt the catastrophe-necessity reading.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_technology_providers, excluded,
    powerful, biographical, trapped, global).

% Civil aviation authorities, nuclear regulatory commissions, medical boards, maritime safety agencies. They adjudicate training standards and competence certification. If this reading is authoritative, they mandate real-world stress exposure as non-negotiable. If the sibling reading (simulation_as_proxy_catastrophe_reading) is authoritative, they can certify competence through simulation alone, reducing organizational risk and liability. They observe both readings and their operational consequences.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Academic and research institutions studying high-reliability systems, organizational resilience, and competence maintenance under extended safety periods. They generate the empirical record on whether simulation is sufficient. Their role is to produce evidence that either supports or challenges the catastrophe-necessity claim.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizational_learning_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_organizations).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is not a coordination constraint. It asserts a physical and psychological fact: human competence in stress-dependent domains requires irreducible exposure to real uncertainty and its consequences.
% TRANSFER_FUNCTION: Does not apply — no extraction transfers value. The constraint asserts a natural limit on human cognitive and physiological capacity under extended safety periods.
% ABSENT_VOICES: Simulation technology vendors, cost-focused procurement authorities, and personnel who prefer lower-risk training environments are structurally excluded from this reading's framework because the reading asserts that their preferences are incompatible with actual competence maintenance.
% DISAPPEARANCE_RATIONALE: If this constraint were false — if simulation could maintain competence indefinitely — organizational training systems would shift entirely to simulation, eliminating expensive and dangerous real-world stress exposure. Personnel competence profiles would diverge sharply, some degrading when real events occur, others maintaining readiness through advanced simulation. The operational reliability of high-risk systems would become technology-dependent rather than founded on irreducible human exposure.
% FOUNDING_PROBLEM: Early high-reliability organizations (nuclear power, aviation, military command) discovered that graduates of pure procedural training (classroom + basic simulation) failed under real operational stress — they lacked the tacit, embodied, stress-inoculated competence layers. The founding problem was: how do we maintain the irreducible human competence that simulation misses?
% FOUNDING_PROBLEM_CORROBORATION: Operational records from early nuclear power plant startup accidents (e.g., TMI cascading operator failures) and military aviation readiness evaluations documented competence gaps that simulation training had not caught. High-reliability organizations' own training reviews confirm the pattern. Organizational learning researchers have documented stress-response atrophy in extended-safety periods (medical residents in low-trauma rotations, air traffic controllers in low-volume facilities). Simulation technology vendors contest the founding problem's ongoing validity, arguing that modern simulators have closed the fidelity gap — that disagreement is exactly the kernel contest. No corroboration comes from the simulation-sufficiency reading's proponents; corroboration comes from operational safety records and organizational learning literature.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint asserts a natural fact, not a coercive arrangement — it does not transfer value from one party to another. Suppression is minimal (0.08) because a natural law needs no coercive enforcement — it is simply true. Theater is low (0.12) because the primary function (competence maintenance) is real, though some organizations may stage catastrophe-equivalent exercises theatrically. Accessibility collapse is high (0.92) because a genuine natural law forecloses alternatives — if simulation is truly insufficient, there is no alternative path to competence. Resistance is low (0.11) because a natural law meets little real resistance; what resistance exists comes from cost-saving pressures and simulation vendors, not from fundamental disagreement. The temporal series is flat — the constraint's structural properties do not change over the interval; time-varying metrics would track organizational adoption of this reading versus sibling readings, not the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   Front-line operators and extended-safety personnel experience the constraint as victims — they bear the cost of exposure or the risk of competence decay. High-reliability organizations experience the constraint as opportunity — it justifies their training structure and organizational form. Simulation vendors experience it as false narrative — technology-dependent sufficiency would be a more powerful claim for their market. Regulatory authorities observe the gap and decide which reading to codify into certification standards. The engine computes this perspectival divergence from the structural data — the claimed mountain status does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations are declared beneficiaries because they benefit from justifying expensive real-world stress exposure and organizational redundancy. Personnel competence and organizational learning are declared victims because competence degrades under extended safety (the constraint claims). This asymmetry sets up the false-summit signal: if organizations benefit from a mountain claim, and the claim is empirically contested, the beneficiary presence warrants scrutiny. Directionality should be low for agenda-setters (beneficiary seat, derives low d) and high for payers (trapped personnel unable to exit). The analytical seats (researchers, regulators) sit near symmetric — they observe the constraint but do not bear its costs directly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_constraint,
    'Is the necessity of real catastrophic stress a physical/psychological law (Mountain), or a constructed institutional belief that benefits high-reliability organizations and justifies their costly training regimes?',
    'Longitudinal competence studies comparing personnel trained exclusively in advancing simulation technology against those with real-event exposure, controlling for organizational culture and selection effects. Post-career analysis of error rates, stress-response timing, and tacit judgment quality in actual emergencies.',
    'If the constraint is genuinely natural (simulation-independent limit on human competence), it remains a mountain. If organizations benefit from the belief and amplify it beyond what evidence supports, FSM triggers and reclassification to tangled_rope or snare is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_constructed_constraint, empirical, 'Whether competence decay under extended safety is a natural law or an institutional construct.').

omega_variable(
    kernel_reading_boundary_simulation_fidelity,
    'Is the deficiency of simulation categorical and immutable (this reading: catastrophe_necessity_reading), or does it degrade as simulation technology improves (sibling reading: simulation_fidelity_threshold)?',
    'Historical analysis: as simulation technology has advanced (from analog to digital to VR to AI-driven dynamic scenarios), has the competence-gap evidence shifted? Have high-reliability organizations moved toward simulation-only training as fidelity increased? Has operational failure data changed as simulation technology matured?',
    'This reading forecloses the simulation_fidelity_threshold reading if the deficiency is categorical. It coexists with hybrid_degradation_reading (which accepts simulation insufficiency but focuses on generational timescale). It influences (but does not foreclose) simulation_as_proxy_catastrophe_reading by asserting the boundary that sibling reading disputes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_simulation_fidelity, empirical, 'Whether simulation insufficiency is categorical or technology-dependent.').

omega_variable(
    stress_inoculation_vs_stress_trauma_boundary,
    'Does the constraint require that personnel experience genuine catastrophe (high-consequence failure states, injury, or death), or only high-fidelity uncertainty and stress that simulation can approach?',
    'Careful examination of organizational training definitions: does ''exposure to real catastrophe'' mean historical catastrophes (Fukushima, Three Mile Island, Tenerife airport disaster), current-scale near-misses, or any authentically high-stakes operation? Neuroscience and psychology literature on stress inoculation vs. trauma.',
    'A reading that requires actual catastrophe is more restrictive and more beneficiary-serving for high-reliability organizations (it justifies exposure policies). A reading that requires only authenticity of stakes could be satisfied by advanced simulation. This ambiguity affects whether the constraint coexists with or forecloses simulation_as_proxy_catastrophe_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stress_inoculation_vs_stress_trauma_boundary, conceptual, 'Whether the constraint requires actual catastrophe or only authentic high-stakes uncertainty.').

omega_variable(
    beneficiary_false_summit_signal,
    'Do high-reliability organizations benefit from the catastrophe-necessity reading by justifying expensive, risky training regimes that might not be necessary if simulation were recognized as sufficient?',
    'Budget and policy analysis: do organizations advocating catastrophe-necessity readings allocate more resources to real-world stress exposure than organizations adopting simulation-sufficiency readings? Do they resist simulation technology adoption at rates inconsistent with stated belief in its insufficiency?',
    'Beneficiary presence on a mountain claim triggers false-summit evaluation. If organizations demonstrably benefit from the belief (by justifying their training spending and organizational structure), and the belief''s empirical status is contested, FSM reclassifies to tangled_rope or snare. This is the central concern for this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_false_summit_signal, empirical, 'Whether beneficiary presence indicates false-summit (constructed belief masquerading as natural law).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel catastrophe_proxy_sufficiency, which decomposes into four distinct constraints based on different epistemic and normative stances on whether simulation can substitute for real catastrophic stress in maintaining operational competence. The four readings share the same domain and underlying empirical questions but differ in their core premises: categorical insufficiency (this reading), technology-dependent sufficiency (threshold reading), functional equivalence (proxy reading), and generational degradation (hybrid reading). Each reading instantiates a different constraint type and has different beneficiary structures. All four are linked by network.affects_constraints because changes in the empirical evidence (e.g., demonstration that advanced simulation maintains competence) or in authorization (e.g., regulatory adoption of one reading) cascade across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
