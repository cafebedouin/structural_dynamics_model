% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Competence Drill Mandate (Hybrid Refresh Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the continuous_refresh_hybrid reading of the
 *   competence_exercise_validity kernel. The kernel asks what validates
 *   operational competence in safety-critical domains. This reading holds
 *   that competence is a process-dependent, continuously refreshed
 *   capability, not a one-time validated state; simulation is necessary but
 *   insufficient without recurring live drill cycles. The constraint is
 *   institutionalized through regulatory mandates, licensure requirements,
 *   and vendor ecosystems that enforce continuous exercise regimes on
 *   frontline operators. Sibling readings include simulation_as_proxy
 *   (simulation alone suffices) and real_catastrophe_only (nothing but
 *   catastrophe truly tests competence).
 *
 * KEY AGENTS:
 *   - regulatory_regime: Agenda-setter (institutional/constrained) â mandates drill frequency and audits compliance
 *   - training_industry: Beneficiary (organized/mobile) â captures revenue from mandatory drill curricula
 *   - operational_management: Beneficiary/payer (powerful/constrained) â gains liability cover, bears budget cost
 *   - frontline_operators: Primary target (powerless/identity_locked) â bear time and cognitive burden of recurrent drills
 *   - safety_science_researchers: Analytical observer (analytical/analytical) â provide empirical evidence on skill decay
 *   - efficiency_advocates: Excluded voice (moderate/mobile) â argue for simulation-only or reduced frequency regimes but lack committee seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.58).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.42).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Competence Drill Mandate (Hybrid Refresh Reading)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'a0e0c64f-1cec-414b-a60a-2b0ef77cd21e').
narrative_ontology:cs_kernel_codification('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', formalized).
narrative_ontology:cs_authority_grounding('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', expertise).
narrative_ontology:cs_interpretation_layer_present('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e').
narrative_ontology:cs_reading_relation('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', foundational, competence_is_process_not_state).
narrative_ontology:cs_axiom_status(competence_is_process_not_state, holdable).
narrative_ontology:cs_axiom_grounding('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', competence_is_process_not_state, empirically_contingent).
narrative_ontology:cs_axiom('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', foundational, simulation_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', simulation_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', competence_as_continuous_process).
narrative_ontology:cs_drift_state('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', contemporary_safety_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a0e0c64f-1cec-414b-a60a-2b0ef77cd21e', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, regulatory_regime).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operational_management).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operational_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates minimum drill frequencies and audits organizational compliance through inspections and licensing. Justifies requirements by citing accident reduction statistics and skill-decay curves. Can revise standards only through slow administrative rulemaking bound by statutory safety mandates.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, regulatory_regime, agenda_setter,
    institutional, generational, constrained, national).

% Designs, sells, and administers drill curricula, simulation scenarios, and certification-tracking platforms. Revenue scales directly with mandated drill frequency and regulatory complexity. Competes for accreditation but benefits from the baseline requirement that competence must be continuously exercised rather than one-time validated.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_industry, beneficiary,
    organized, biographical, mobile, national).

% Receives reduced liability exposure, lower insurance premiums, and defensible audit trails by adhering to the continuous drill regime. Bears the direct budgetary cost of contracting training vendors and pulling personnel off production lines for drills. Cannot unilaterally opt out without losing licensure or coverage.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operational_management, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, operational_management, payer).

% Must interrupt production or rest duties to participate in recurrent simulations and live drills. Bears the cognitive load, schedule disruption, and fatigue. Professional licensure and union standing depend on demonstrated drill attendance records; exiting the drill regime effectively means exiting the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, payer,
    powerless, biographical, identity_locked, local).

% Publish longitudinal studies on skill decay, simulator fidelity, and accident etiology. Their empirical findings are cited by all sides of the kernel contest to support or challenge drill frequency mandates, but they do not themselves set enforcement policy.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_science_researchers, observer,
    analytical, civilizational, analytical, global).

% Argue that advances in simulation fidelity and just-in-time training reduce or eliminate the need for continuous live-drill burdens. They are structurally excluded from safety committees and regulatory hearings that set drill frequency, which are dominated by risk-averse compliance officers and vendor-affiliated advisors.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, efficiency_advocates, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, training_industry).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains procedural competence and collective response readiness across teams operating high-consequence systems, ensuring that individual and team skill execution does not decay between rare critical events.
% TRANSFER_FUNCTION: Moves operator time, organizational budget, and cognitive bandwidth from production or rest into recurrent drill and simulation regimes, while transferring risk-reduction credit, regulatory legitimacy, and liability cover to management and oversight bodies.
% ABSENT_VOICES: Operators from jurisdictions with minimal drill requirements but equivalent safety outcomes are structurally excluded from regulatory hearings; efficiency advocates who would argue for simulation-only or reduced-frequency regimes are sidelined by safety committees dominated by compliance officers and vendor-affiliated advisors.
% DISAPPEARANCE_RATIONALE: If the mandate for continuous drill cycles vanished, training budgets would collapse, vendor revenue would drop, operator schedules would reallocate to production, and regulatory compliance frameworks would revert to one-time validation standards. The organizational learning architecture would restructure around episodic certification rather than continuous exercise.
% FOUNDING_PROBLEM: Catastrophic industrial accidents caused by skill atrophy, protocol drift, and collective panic in rare high-consequence events where personnel had been certified once but never practiced under pressure.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards and peer-reviewed safety science studies outside the training industry and regulatory compliance apparatus attest that skill decay and atrophy contribute to incident severity and that response quality degrades without recurrent exercise.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the continuous stream of operator labor and organizational budget captured by the drill mandate. Suppression (0.42) is moderate: alternatives such as one-time validation or self-certified simulation are suppressed by regulation and insurance requirements but not through violent coercion. Theater ratio (0.38) captures the growing share of drill activity that serves audit-trail performance rather than genuine skill challenge. Accessibility collapse (0.65) is high because professional licensure and organizational accreditation collapse exit options once an operator or firm enters the regulated field. Resistance (0.50) is moderate, manifesting as frontline fatigue and management pressure to reduce non-productive hours. The measurement series share one time grid to prevent misaligned temporal inference.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat, the constraint is genuine rope: a live-saving coordination mechanism justified by safety record evidence. From the frontline_operator's seat, the same structure reads as substantially extractiveâa perpetual time tax whose marginal safety return is invisible to them and whose necessity is enforced rather than chosen. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory_regime and training_industry sit near the beneficiary end of directionality: the regime gains authority and statutory purpose, while the industry captures a revenue stream scaled to mandated frequency. Operational_management sits ambiguouslyâbenefiting from liability reduction while paying budgetary costsâso its derived directionality is intermediate. Frontline_operators are full targets: their exit is identity_locked to the profession, and the constraint extracts their time and attention directly. The engine will compute asymmetric extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâaccidents driven by skill atrophyâis corroborated as live by independent accident investigators outside the beneficiary set, preventing mandatrophy misclassification. Were the founding problem dead, the persistent drill machinery would risk piton or snare classification. The theater_ratio of 0.38 signals drift toward performative compliance but not yet dominance; the coordination function remains structurally genuine, which is why the claimed type is tangled_rope rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the continuous_refresh_hybrid reading the only empirically supported framing, or does it conflate correlation between drill frequency and safety culture with direct causation of accident prevention?',
    'Controlled natural experiments or quasi-experimental comparisons across jurisdictions with differing drill-frequency mandates, holding simulation technology and incident base rates constant.',
    'If the safety record is driven by safety culture rather than drill frequency per se, the continuous extraction of operator time is unjustified and the constraint slides toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether continuous drill mandates are causally necessary or merely correlated with safety outcomes.').

omega_variable(
    enforcement_vs_identity_lock,
    'Would continuous drill regimes collapse to one-time validation if regulatory enforcement vanished, or would professional identity, liability markets, and insurance requirements sustain them independently?',
    'Observation of self-regulating professions or deregulated jurisdictions where enforcement is minimized but liability and insurance incentives remain.',
    'If sustained without enforcement, the constraint is more rope-like (self-coordinating); if it collapses, the coordination function is weaker and the constraint is more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_vs_identity_lock, conceptual, 'Whether the constraint persists by active enforcement or by professional identity and market incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.15).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.22).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.3).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 32, 0.35).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_exercise_validity__continuous_refresh_hybrid, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This constraint is the continuous_refresh_hybrid reading of the competence_exercise_validity kernel, distinct from the simulation_as_proxy and real_catastrophe_only readings. Each reading instantiates a different constraint with its own epsilon value, beneficiary/victim structure, and classification. They form a constraint family linked by shared kernel provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
