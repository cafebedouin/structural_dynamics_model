% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Kernel Occupation
 *   domain: organizational/safety/high_reliability
 *
 * SUMMARY:
 *   This constraint asserts that only actual catastrophic incidents — not
 *   simulations, drills, or procedural exercises — can authentically occupy
 *   the competence kernel in high-reliability organizations. The claim frames
 *   this as a natural law of human performance under extreme conditions: the
 *   physiological, cognitive, and social dynamics of genuine catastrophe
 *   cannot be replicated, so competence decays inevitably until the next real
 *   event. High-reliability organizations (nuclear, aviation, chemical,
 *   healthcare) thus face an unresolvable maintenance problem: they must
 *   prevent the very events that sustain their competence. No viable
 *   beneficiary structure exists because catastrophes are unacceptable by
 *   definition — no one profits from them. Yet the constraint persists as an
 *   organizing belief that shapes training investment, regulatory philosophy,
 *   and organizational self-conception.
 *
 * KEY AGENTS:
 *   - frontline_operators: Primary targets (powerless/identity_locked) — bear the competence decay and the consequences when real incidents occur
 *   - safety_managers: Secondary targets (moderate/identity_locked) — responsible for maintaining competence without the conditions that sustain it
 *   - regulatory_inspectors: Secondary targets (organized/constrained) — must enforce standards based on a competence model they cannot validate
 *   - affected_public: Ultimate victims (powerless/trapped) — bear the consequences when competence decay meets real incident
 *   - simulation_vendors: Excluded beneficiaries (organized/mobile) — would benefit if simulation_sufficiency were accepted, but are structurally excluded by this reading
 *   - analytical_observer: Observer (analytical/analytical) — sees the kernel structure and the reading's position within it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.78).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.92).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Kernel Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety/high_reliability").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'de81cb83-640b-4d9a-a447-e03021b277f6').
narrative_ontology:cs_kernel_codification('de81cb83-640b-4d9a-a447-e03021b277f6', implicit).
narrative_ontology:cs_authority_grounding('de81cb83-640b-4d9a-a447-e03021b277f6', practice).
narrative_ontology:cs_interpretation_layer_present('de81cb83-640b-4d9a-a447-e03021b277f6').
narrative_ontology:cs_reading_relation('de81cb83-640b-4d9a-a447-e03021b277f6', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('de81cb83-640b-4d9a-a447-e03021b277f6', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('de81cb83-640b-4d9a-a447-e03021b277f6', foundational, catastrophe_conditions_irreducible).
narrative_ontology:cs_axiom_status(catastrophe_conditions_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('de81cb83-640b-4d9a-a447-e03021b277f6', catastrophe_conditions_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('de81cb83-640b-4d9a-a447-e03021b277f6', foundational, simulation_cannot_occupy_kernel).
narrative_ontology:cs_axiom_status(simulation_cannot_occupy_kernel, holdable).
narrative_ontology:cs_axiom_grounding('de81cb83-640b-4d9a-a447-e03021b277f6', simulation_cannot_occupy_kernel, empirically_contingent).
narrative_ontology:cs_reference_frame('de81cb83-640b-4d9a-a447-e03021b277f6', authentic_competence_conditions).
narrative_ontology:cs_drift_state('de81cb83-640b-4d9a-a447-e03021b277f6', contemporary_simulation_fidelity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de81cb83-640b-4d9a-a447-e03021b277f6', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, safety_managers).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, regulatory_inspectors).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the high-reliability systems daily. Their competence for catastrophic scenarios decays between real incidents. They train on simulations but are told by the reading that simulations cannot occupy the kernel. When a real incident occurs, they bear the consequences of any competence gap. Their professional identity is fused with the competence kernel — leaving the reading's frame would mean abandoning their self-conception as 'the ones who handle the real thing.'
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    powerless, biographical, identity_locked, local).

% Responsible for competence maintenance programs. They must allocate training budgets, design drill schedules, and report to regulators — all while the reading they operate under declares the only valid competence condition (real incidents) is the one they are mandated to prevent. They cannot advocate for simulation_sufficiency without appearing to compromise safety. Their career trajectory depends on managing the competence kernel within the reading's frame.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_managers, payer,
    moderate, biographical, identity_locked, regional).

% Enforce competence standards derived from the reading. They audit training programs, certify operators, and investigate incidents. Their regulatory framework treats real-incident experience as the gold standard, making it structurally difficult to accept simulation-based validation. They could push for regulatory recognition of simulation_sufficiency but face institutional inertia, legal precedent, and political risk.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_inspectors, payer,
    organized, generational, constrained, national).

% Live near or depend on high-reliability systems (nuclear plants, chemical facilities, air traffic corridors, hospitals). They bear the consequences when competence decay meets a real incident. They have no voice in competence maintenance policy, no exit from the systems they depend on, and no way to verify that operators' competence is current. The reading's claim that only real incidents validate competence means their safety depends on the very events that threaten them.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, affected_public, payer,
    powerless, immediate, trapped, local).

% Develop and sell simulation-based training systems. They would be beneficiaries if simulation_sufficiency were accepted — their products would become the primary competence maintenance mechanism. But this reading structurally excludes them by declaring simulation epistemically invalid for kernel occupation. They lobby for regulatory recognition, fund research, and argue for hybrid approaches, but the reading's frame treats their entire product category as fundamentally inadequate.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_vendors, excluded,
    organized, biographical, mobile, global).

% Studies the competence_occupation kernel and its readings across domains. Sees that real_incident_necessity is one of three structurally distinct readings, each with different ε, different beneficiary/victim structures, and different implications for organizational design. Does not experience the constraint's extraction or suppression directly but analyzes its structural properties and its relationship to the sibling readings.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the industry's self-conception around a shared epistemic commitment: that competence for catastrophe is distinct from and irreducible to competence for routine operations. This coordination prevents fragmentation into competing training paradigms and maintains a unified standard for 'real' competence.
% TRANSFER_FUNCTION: Transfers the burden of competence maintenance from the organization (which cannot produce real incidents) to the frontline operators and safety managers (who must maintain competence without the conditions that sustain it) and ultimately to the affected public (who bear the risk of the competence gap). No seat receives a corresponding benefit — the transfer is into a structural void.
% ABSENT_VOICES: The simulation_sufficiency reading's proponents (simulation vendors, some training researchers, progressive regulators) are structurally excluded from the competence_occupation kernel's authoritative interpretation. They would argue that simulation fidelity has reached sufficiency for kernel occupation, but the reading's frame treats this claim as a category error. The hybrid_occupation reading's proponents (pragmatic safety managers, some professional associations) are not excluded but are marginalized — their position is treated as operationally necessary but epistemically incomplete.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, organizations would immediately pivot to simulation_sufficiency or hybrid_occupation as the basis for competence maintenance. Training budgets would shift from maintaining readiness for rare events to continuous simulation-based exercise. Regulatory frameworks would rewrite validation standards. The competence kernel would be reoccupied under a different reading — the world rearranges because the reading currently structures the entire competence maintenance ecosystem.
% FOUNDING_PROBLEM: Early high-reliability industries (nuclear, aviation) discovered that operators who had only trained on simulators performed catastrophically differently during real emergencies — the physiological stress, cognitive tunneling, and social dynamics of genuine catastrophe were not replicated. The founding problem: how to maintain competence for events that are too rare to experience regularly but too consequential to get wrong.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the industries themselves (nuclear, aviation, chemical) and by independent safety science (HRO literature, resilience engineering). However, the *reading's solution* — that only real incidents suffice — is contested. Simulation_sufficiency proponents cite advances in VR, physiological stress induction, and distributed simulation that they claim achieve kernel occupation. Hybrid_occupation proponents cite operational data showing continuous multi-mechanism exercise maintains competence without real incidents. The corroboration for the problem is strong; the corroboration for this reading's solution is contested.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type 'mountain' reflects the reading's own framing: this is presented as an irreducible natural law of competence. The metrics describe what the constraint *does*: high extractiveness (0.78) because the constraint extracts competence-maintenance labor from organizations that cannot access the conditions that sustain it; extreme suppression (0.92) because alternatives (simulation, hybrid) are treated as epistemically invalid; low theater (0.15) because the belief is genuinely held, not performative; high accessibility_collapse (0.88) because once accepted, the reading closes off alternative competence models; low resistance (0.12) because the industry largely accepts this framing. The divergence: the reading claims mountain (natural law), but the metrics describe a constraint that extracts heavily from trapped agents with no beneficiary — a structural profile the engine may classify differently.
 *
 * PERSPECTIVAL GAP:
 *   From the reading's own frame (the mountain seat), this is a natural law — the competence kernel *requires* real incidents, and any arrangement denying this is delusion. From the victim seats, the constraint operates as an epistemic trap: they are held responsible for maintaining competence under conditions the reading declares impossible to simulate, creating a structural double-bind. The analytical_observer sees a kernel reading that forecloses its siblings by declaring the competence conditions irreducible — a structural move, not an empirical finding.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries declared — catastrophes are unacceptable, so no seat collects from the constraint's operation. Victims are declared: frontline_operators, safety_managers, regulatory_inspectors, affected_public all bear costs (competence decay, responsibility without authority, consequence exposure) without receiving offsetting benefits. The affected_public is trapped (exit_options: trapped) — they cannot opt out of the systems whose competence decays. Frontline_operators and safety_managers are identity_locked: their professional identity is constituted through the competence kernel, making exit from the reading's frame unthinkable. Regulatory_inspectors are constrained: they could theoretically adopt a different reading but face institutional lock-in. The analytical_observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — maintaining competence for catastrophic scenarios — remains live (catastrophes still occur, competence still decays). But the reading's *solution* (only real incidents work) has become a trap: it prevents investment in alternatives by declaring them epistemically invalid, while the real incidents it requires are precisely what the organization exists to prevent. This is not mandatrophy in the classic sense (function atrophied while structure persists) — it is a *genuine* unresolvable problem that the reading frames as natural law, blocking the hybrid_occupation and simulation_sufficiency readings that might offer partial mitigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law of competence maintenance, or a reading of the contested kernel ''competence_occupation'' that instantiates a structurally distinct claim with its own ε?',
    'Decompose the kernel into its sibling readings (simulation_sufficiency, hybrid_occupation) and verify each has a stable ε and distinct beneficiary/victim structure. If sibling readings produce different classifications, this is a kernel reading, not a mountain.',
    'If this is a kernel reading, the claimed_type ''mountain'' is a false summit; the engine''s false_summit_mountain signature would reclassify based on the reading''s structural position within the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the ''natural law'' framing masks a kernel reading with extractive implications').

omega_variable(
    victim_structure_ambiguity,
    'Are the declared victims (frontline_operators, safety_managers, regulatory_inspectors, affected_public) genuine targets of extraction, or are they collateral to an unresolvable epistemic problem with no beneficiary?',
    'Analyze whether any seat collects rents or structural advantage from the constraint''s operation. If no beneficiary exists and the constraint''s persistence is explained by epistemic closure rather than enforcement, the victim declarations may map to a piton or mountain rather than a snare/tangled_rope.',
    'If victims are genuine extraction targets, the constraint is not a mountain regardless of emerges_naturally; if victims are collateral to an epistemic trap, the mountain claim may hold but requires different omega framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, conceptual, 'Whether the victim structure indicates extraction or epistemic closure without beneficiary').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.92) structural — organizations actively prevented from accessing alternatives — or internalized — the industry believes no alternative can be valid without real incidents?',
    'Post-exit suppression trajectory: if an organization adopts simulation_sufficiency and faces no structural barriers but still cannot maintain competence without real incidents, the suppression is internalized/epistemic. If structural barriers (regulation, insurance, licensing) prevent the switch, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the industry carries the suppression with it. This informs whether the mountain claim is an epistemic trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in competence maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.1).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.12).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint, competence_occupation__simulation_sufficiency, and competence_occupation__hybrid_occupation form the competence_occupation kernel family. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structure, and classification. The real_incident_necessity reading has the highest ε (0.78) and no beneficiaries, reflecting its claim that the competence conditions are both irreducible and unattainable by design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, powerless, 0.95).
constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, moderate, 0.85).
constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
