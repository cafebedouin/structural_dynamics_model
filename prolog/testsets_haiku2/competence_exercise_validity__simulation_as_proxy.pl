% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation as Valid Competence Exercise Proxy
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A regulatory and organizational regime treats simulation-based drills as
 *   sufficient validation that personnel have exercised and retained critical
 *   competencies. The regime codifies simulation scores as adequate proxy for
 *   readiness, allowing organizations to certify competence on schedule
 *   rather than waiting for real crises. This reading instantiates one
 *   position in a contested kernel: whether simulation alone is sufficient
 *   (this reading: simulation_as_proxy) or whether continuous hybrid
 *   approaches or real-catastrophe testing are necessary. The constraint
 *   operates as rope to its internal logic (coordination benefit:
 *   predictable, budgetable competence certification) but carries extractive
 *   components to the skeptics' reading (theater rises as simulation metrics
 *   become the sole acceptable evidence, while actual readiness remains
 *   contested).
 *
 * KEY AGENTS:
 *   - Safety Regulator: Sets and enforces the simulation-as-proxy standard (agenda_setter); benefits from avoiding catastrophe-dependent learning
 *   - Operational Management: Implements drills on schedule; benefits from predictable budgets and regulatory compliance without real-crisis triggers
 *   - Competence-Bearing Personnel: Perform in simulations; identity-locked into the regime; their competence is validated by metrics they may judge insufficient
 *   - Crisis Event Analysts: Provide independent evidence of whether simulation-trained personnel succeed or fail in real events (observer seat)
 *   - Simulation Vendors: Sell equipment and training; economically benefit from simulation-centric mandates
 *   - Competence Skeptics and Accident Survivors: Excluded from the regulatory conversation despite having strong incentive to voice doubts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.68).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.71).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation as Valid Competence Exercise Proxy").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '0b81b85c-ae48-4e1d-8d9c-d9872146d244').
narrative_ontology:cs_kernel_codification('0b81b85c-ae48-4e1d-8d9c-d9872146d244', formalized).
narrative_ontology:cs_authority_grounding('0b81b85c-ae48-4e1d-8d9c-d9872146d244', extraction).
narrative_ontology:cs_interpretation_layer_present('0b81b85c-ae48-4e1d-8d9c-d9872146d244').
narrative_ontology:cs_reading_relation('0b81b85c-ae48-4e1d-8d9c-d9872146d244', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_reading_relation('0b81b85c-ae48-4e1d-8d9c-d9872146d244', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('0b81b85c-ae48-4e1d-8d9c-d9872146d244', foundational, simulation_metrics_validity).
narrative_ontology:cs_axiom_status(simulation_metrics_validity, holdable).
narrative_ontology:cs_axiom_grounding('0b81b85c-ae48-4e1d-8d9c-d9872146d244', simulation_metrics_validity, empirically_contingent).
narrative_ontology:cs_axiom('0b81b85c-ae48-4e1d-8d9c-d9872146d244', foundational, scheduling_over_authenticity).
narrative_ontology:cs_axiom_status(scheduling_over_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('0b81b85c-ae48-4e1d-8d9c-d9872146d244', scheduling_over_authenticity, instrumental).
narrative_ontology:cs_reference_frame('0b81b85c-ae48-4e1d-8d9c-d9872146d244', simulation_sufficient_for_certification).
narrative_ontology:cs_drift_state('0b81b85c-ae48-4e1d-8d9c-d9872146d244', contemporary_post_incident_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b81b85c-ae48-4e1d-8d9c-d9872146d244', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, safety_regulator).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operational_management).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, competence_bearing_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes standards requiring competence certification through simulation drills at prescribed intervals. Sets the pass/fail criteria for simulation performance. Enforces compliance through inspection and license suspension. Benefits from a lower-cost validation regime that requires no actual crisis events; avoids liability for catastrophe-only learning doctrine.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_regulator, agenda_setter,
    institutional, generational, analytical, national).

% Uses simulation drills to satisfy regulatory compliance without triggering actual emergencies. Manages budgets and schedules around simulation calendars rather than unpredictable real events. Captures the cost savings and operational predictability of scheduled training versus reactive crisis response. Must implement the drills but captures the coordination benefit of predictable, measurable competence maintenance.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operational_management, beneficiary,
    institutional, biographical, constrained, global).

% Must participate in simulation drills on the prescribed schedule regardless of whether they feel their competence is adequately exercised. Professionally bound to the organization and unable to exit without abandoning license and career. Their competence is validated via simulation metrics rather than tested through conditions they judge adequate. The constraint treats drill completion as equivalent to demonstrated readiness, even if they perceive simulation as insufficient stress to reveal real gaps.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, competence_bearing_personnel, payer,
    moderate, biographical, identity_locked, local).

% Study real-world accident sequences and competence failures. Can testify to whether simulation-trained personnel perform adequately when crises occur, or whether simulation-only training misses failure modes. Data from their investigations shapes the contested reading of whether simulation is a sufficient proxy.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, crisis_event_analysts, observer,
    powerful, generational, analytical, global).

% Sell simulation equipment, software, and training services to organizations and regulators. Benefit directly from mandates that treat simulation as valid and require continuous drill cycles. Have economic incentive to maintain the simulation-as-proxy doctrine and resist alternative readings that would reduce drill frequency or shift to other validation methods.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Include experienced practitioners and safety researchers who believe simulation inadequately exercises competence under true stress. Their objections are sidelined by regulatory adoption of the simulation-as-proxy reading; they operate under a regime they believe unsound but have limited power to change.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, competence_skeptics, excluded,
    moderate, biographical, constrained, global).

% Bear the consequences if simulation-trained personnel fail during real crises. Have strong incentive to voice skepticism of simulation sufficiency but are structurally absent from the regulatory conversation and expert testimony that sets the constraint.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, accident_survivors, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, operational_management).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, measurable regime for certifying that personnel retain critical safety competencies without requiring organizations to trigger real crises or wait for them to occur naturally. Solves the scheduling and verification problem: how to test readiness without catastrophe.
% TRANSFER_FUNCTION: Moves institutional risk assumption from crisis response (unpredictable, high-cost, potentially catastrophic) to periodic simulation drills (predictable, budgetable, insurance-compatible). The regulator transfers liability risk to organizations, which transfer operational burden to personnel who must pass simulation metrics.
% ABSENT_VOICES: Practitioners skeptical of simulation adequacy, personnel who perceive simulation as insufficient stress, accident survivors from scenarios where simulation-trained staff failed, and jurisdictions or safety traditions that maintain real-catastrophe-only or continuous-hybrid doctrines are structurally outside the regulatory conversation. Their objections would challenge the simulation-sufficiency axiom but are not heard in standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would revert to mixed regimes: some would increase simulation frequency and realism; others would rely more heavily on continuous operational practice and mentorship. The regulatory certainty that simulation 'counts' would be gone, forcing explicit justification of whatever competence regime was chosen. Insurance and liability law would fragment across jurisdictions and industries.
% FOUNDING_PROBLEM: Before simulation validation was codified, no scalable method existed to certify competence without waiting for actual crises. The founding problem was: how to know if personnel are ready without the catastrophic cost and unpredictability of real testing?
% FOUNDING_PROBLEM_CORROBORATION: The regulator and management attest the founding problem remains live: real crises are still unpredictable and costly, simulation fills the gap. Crisis investigators and safety researchers attest the founding problem was solved PARTIALLY — a scalable method exists, but it does not test under true stress conditions. Accident reconstruction data from multiple jurisdictions (independent of the regulating body) shows a mixed picture: some simulation-trained personnel perform adequately; others fail in ways simulation did not surface.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the constraint's operation: the regulator and vendors extract regulatory authority and market share; management extracts operational certainty and cost control; personnel lose the ability to challenge the sufficiency of their own training under rules they didn't set. Suppression at 0.71 is high because the constraint depends on actively silencing the skeptical reading — alternative validation regimes are not merely discouraged, they are made non-compliant. Theater at 0.62 reflects the central dynamic: simulation compliance becomes ritual performance (pass the scenario, check the box) rather than authentic readiness testing. The measurement series shows theater rising from 0.48 to 0.62 over the interval, consistent with a constraint whose functional core (scheduling verification) is real but whose legitimacy claim (simulation equals readiness) drifts toward performance. Suppression stays elevated throughout, indicating constant effort to maintain the simulation-as-proxy reading against skeptical alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The regulator and management seats compute as beneficiaries (low d) with access to predictable, budgetable regimes. The personnel seat computes as a target (high d, identity_locked) whose competence is defined by metrics they may not control. Simulation vendors compute as moderate beneficiaries (d ~0.25–0.35) with market insulation from alternative validation regimes. Crisis analysts compute as observers (analytical) whose findings challenge the reading but are not integrated into the constraint's enforcement. The divergence is structural: this reading benefits institutional actors and hardens access to power, while it constrains those whose competence is subject to validation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the regulator: d = 0.1 (full beneficiary — they set the standard and avoid liability for catastrophe-dependent learning). Directionality for management: d = 0.2 (strong beneficiary — they capture operational predictability and cost control). Directionality for personnel: d = 0.85 (strong target — identity-locked, constrained by simulation-as-proxy validation, no exit without career abandonment). Directionality for simulation vendors: d = 0.3 (beneficiary with mobile exit — they profit from simulation-centric mandates but could serve other regimes). Directionality for crisis analysts: d = 0.5 (symmetric, observer position — their analysis informs but does not control the constraint). The asymmetry is fundamental: institutional and vendor actors benefit from the simulation-as-proxy reading and can exit to other readings; personnel cannot exit without abandoning identity and career.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy surface is contested: the founding problem (how to certify competence without real crises) was real. The constraint's response (use simulation metrics) addresses part of it — it enables scheduling and budgeting. But the measured theater ratio (0.62, rising from 0.48) indicates a drift toward performance maintenance rather than functional validation. The constraint persists because regulators, management, and vendors benefit and want to maintain it; personnel bear the cost and cannot exit without career abandonment. The theater rise signals that simulation-as-proxy is increasingly a ritual than a truth-tracking mechanism — the regime is performing adequacy rather than assuring it. This does not trigger classical mandatrophy (where the founding problem vanished and the constraint became pure extraction) — the founding problem is still contested, so the constraint oscillates between coordination and extraction rather than degrading into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_gap,
    'What is the measurable gap between stress, sensory input, and decision-making conditions in simulation versus real crises? Is the gap narrow enough that simulation adequately exercises the same competencies as real events?',
    'Comparative analysis of decision latency, error rates, and physiological stress markers (heart rate, cortisol) in simulation versus high-fidelity naturalistic crises; post-incident analysis of whether simulation-trained personnel exhibited the same failure modes as those untrained.',
    'If the gap is wide and simulation-trained personnel show failure modes not surfaced in drills, the constraint''s ε rises sharply (higher extraction, lower accessibility of alternatives). If the gap is narrow and post-incident analysis shows no simulation-specific failures, ε stays low and the rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_gap, empirical, 'Whether simulation-induced stress and decision conditions adequately track real crises.').

omega_variable(
    reading_foreclosure_via_regulation,
    'Does the regulatory adoption of simulation-as-proxy reading structurally foreclose the competing readings (continuous_refresh_hybrid, real_catastrophe_only), or do they coexist as live alternatives held by different organizational and cultural traditions?',
    'Survey of jurisdictions and industries: Do alternatives persist as real options despite regulatory adoption of simulation-as-proxy? Or has regulatory standardization made them economically and politically inaccessible?',
    'If readings coexist_with (different organizations and traditions maintain alternatives), the kernel remains genuinely contested and this constraint is one voice in an open debate. If this reading forecloses or influences the others through regulatory capture and market consolidation, the constraint becomes a monopoly-of-interpretation problem, shifting type and increasing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_regulation, conceptual, 'Whether regulatory adoption of simulation-as-proxy has eliminated or suppressed alternative readings from live consideration.').

omega_variable(
    theater_as_extraction_mechanism,
    'Does the rising theater_ratio (0.48→0.62) indicate that simulation-as-proxy is increasingly a performative regime where compliance and readiness have decoupled, allowing the constraint to persist even as participants lose faith in its sufficiency?',
    'Post-incident interviews with personnel: Do they report that simulation and real readiness feel disconnected? Do near-misses and minor accidents correlate with simulation-identified gaps? Does organizational pressure to ''pass'' drills incentivize gaming rather than authentic learning?',
    'If theater is the extraction mechanism, the constraint has drifted from rope (coordination benefit: predictable competence certification) toward snare (pure extraction through performative compliance). The constraint persists not because simulation works but because institutional actors benefit from the appearance of validation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_as_extraction_mechanism, empirical, 'Whether theater ratio growth signals transformation from coordination to performative extraction.').

omega_variable(
    identity_lock_persistence,
    'How binding is the identity_locked exit for competence-bearing personnel? Would changing simulation regimes or acknowledging simulation inadequacy cause mass exit, or is career identity sufficiently fused with organizational role that personnel stay despite doubts?',
    'Natural experiment: organizations that shift from simulation-as-proxy to hybrid or continuous regimes; track whether personnel retention and satisfaction change. Survey of competence-locked professions (pilots, surgeons, emergency responders) on their belief in simulation adequacy versus their willingness to exit if it were challenged.',
    'If identity is strongly locked (personnel stay despite doubts), the constraint''s extraction is amplified — they have no real exit option even if they believe the regime is insufficient. If identity lock is weaker and people exit when regimes change, the constraint''s hold is more brittle and alternative readings have more power to propagate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'How strongly identity fusion to professional role locks personnel into simulation-as-proxy validation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.52).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.56).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.59).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.61).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__simulation_as_proxy, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% Part of the competence_exercise_validity constraint family (three readings of one kernel). This story models the 'simulation_as_proxy' reading. Sibling readings — continuous_refresh_hybrid and real_catastrophe_only — are separate constraint stories sharing the same kernel (competence_exercise_validity) but instantiating structurally different arrangements with different ε values, beneficiary/victim structures, and types. Network edges (affects_constraints) link all three; commentary.kernel_context names the shared kernel in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
