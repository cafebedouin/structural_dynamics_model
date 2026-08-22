% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Competence Retention Through Continuous Exercise (Hybrid Simulation-Reality Model)
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical organizations (aviation, nuclear, maritime) operate under
 *   a constraint that competence must be maintained through continuous drill
 *   cycles, not validated one-time through simulation. The constraint embeds
 *   a hybrid claim: simulation is necessary (it is the only way to practice
 *   rare catastrophes safely), but not sufficient (it must be repeated
 *   continuously to retain competence). This constraint is ONE reading of the
 *   contested kernel 'competence_exercise_validity.' The sibling readings
 *   claim either that real catastrophe is the only sufficient test
 *   ('real_catastrophe_only') or that simulation counts as valid persistent
 *   exercise ('simulation_as_proxy'). This reading—the
 *   continuous-refresh-hybrid—asserts that competence is process-dependent
 *   (retained through repeated exercise), not state-validated (proven by a
 *   single test), and that safety records confirm the continuous approach
 *   works.
 *
 * KEY AGENTS:
 *   - safety_management_regime: Institutional agenda-setter that codifies and enforces continuous-exercise requirement; benefits from reduced catastrophic risk and regulatory authority
 *   - operational_personnel: Moderate power; constrained exit; bear the time and cognitive burden of continuous drills
 *   - resource_constrained_organizations: Powerful but constrained; allocate budget to drill infrastructure; benefit from reduced risk but carry implementation cost
 *   - rare_catastrophe_victims: Powerless, distributed; benefit only post-event; their membership is realized in catastrophe
 *   - simulation_validation_advocates: Excluded from regulatory conversation; argue one-time testing suffices; trapped by mandate
 *   - insurance_and_liability_regime: Institutional; reinforces continuous-exercise requirement through premium structure and coverage exclusions
 *   - catastrophe_modeling_authority: Observer; provides independent evidence about exercise regimes' efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.68).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Competence Retention Through Continuous Exercise (Hybrid Simulation-Reality Model)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'd83b328b-9c67-4f75-b0a5-88b16d83e4c9').
narrative_ontology:cs_kernel_codification('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', formalized).
narrative_ontology:cs_authority_grounding('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', extraction).
narrative_ontology:cs_interpretation_layer_present('d83b328b-9c67-4f75-b0a5-88b16d83e4c9').
narrative_ontology:cs_reading_relation('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', competence_exercise_validity__real_catastrophe_only, influences).
narrative_ontology:cs_axiom('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', foundational, competence_is_decaying_skill).
narrative_ontology:cs_axiom_status(competence_is_decaying_skill, holdable).
narrative_ontology:cs_axiom_grounding('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', competence_is_decaying_skill, empirically_contingent).
narrative_ontology:cs_axiom('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', foundational, continuous_exercise_prevents_decay_better_than_periodic_testing).
narrative_ontology:cs_axiom_status(continuous_exercise_prevents_decay_better_than_periodic_testing, holdable).
narrative_ontology:cs_axiom_grounding('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', continuous_exercise_prevents_decay_better_than_periodic_testing, empirically_contingent).
narrative_ontology:cs_axiom('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', secondary, process_dependent_competence_maintenance_justified).
narrative_ontology:cs_axiom_status(process_dependent_competence_maintenance_justified, holdable).
narrative_ontology:cs_axiom_grounding('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', process_dependent_competence_maintenance_justified, instrumental).
narrative_ontology:cs_reference_frame('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', process_dependent_competence_standard).
narrative_ontology:cs_drift_state('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', contemporary_regulatory_intensification, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d83b328b-9c67-4f75-b0a5-88b16d83e4c9', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_management_regime).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, institutional_risk_allocation).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, rare_catastrophe_victims).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, insurance_and_liability_regime).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, process_dependent_competence_thesis).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, continuous_exercise_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and enforces the requirement that competence validation must include continuous drill cycles, not one-time simulation tests. Sets the standard, audits compliance, and justifies the requirement as the only way to retain operational readiness for rare catastrophic scenarios. Collects compliance burden indirectly through organizational overhead and regulatory authority.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_management_regime, agenda_setter,
    institutional, generational, arbitrage, global).

% Undergo continuous drill cycles alongside regular work, drawing time and cognitive energy from operational tasks. They argue that a single validated simulation test should suffice if the organization's safety systems are well-designed. Exit is limited to job change or regulatory non-compliance, both costly; they carry the burden of repeated exercise even when no catastrophe occurs for decades.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel, payer,
    moderate, biographical, constrained, global).

% Allocate budget to continuous drill cycles and exercise design. They benefit from reduced catastrophic risk and regulatory exemption from one-time testing, but bear the cost of maintaining drill infrastructure and personnel time. Their exit is constrained by regulatory mandate and insurance requirements; they cannot substitute cheaper one-time certification.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_organizations, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_organizations, beneficiary).

% Benefit from the reduced probability that operational personnel's competence gaps will contribute to harm during rare catastrophic events. They are not named parties to the constraint but are distributed beneficiaries: their membership is realized only post-event, and their capacity to object is zero.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, rare_catastrophe_victims, beneficiary,
    powerless, immediate, trapped, universal).

% Argue that well-designed simulation can substitute for continuous drill, that one-time high-fidelity tests are sufficient to validate readiness, and that the continuous-exercise mandate wastes organizational resources. They are structurally excluded from the regulatory conversation because the mandate treats their core claim as settled rather than contested.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, simulation_validation_advocates, excluded,
    organized, biographical, trapped, global).

% Underwrites organizational risk and requires evidence of continuous exercise as a condition of coverage at favorable rates. Benefits from lower catastrophic loss probability and reduced liability payouts. Enforces the continuous-exercise mandate through premium structure and coverage exclusions, giving the safety management regime institutional backing.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, insurance_and_liability_regime, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, insurance_and_liability_regime, beneficiary).

% Analyzes competence retention mechanisms and produces independent evidence about whether continuous exercise vs. one-time simulation produces measurably different safety outcomes. They assess the constraint's empirical claim without institutional stake in the mandate's persistence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, catastrophe_modeling_authority, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, safety_management_regime).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining readiness for rare, high-consequence catastrophic events: organizations individually would under-invest in exercise (the benefit accrues only in rare, distant futures); continuous-exercise coordination ensures that personnel remain competent in the absence of recent real events, preserving response capability across decades without retraining.
% TRANSFER_FUNCTION: Moves organizational resources (personnel time, drill infrastructure cost, opportunity cost of operational disruption) from operational budgets to continuous exercise systems, in exchange for reduced catastrophic-event probability and regulatory authorization to operate at higher risk thresholds.
% ABSENT_VOICES: One-time simulation advocates and resource-constrained small organizations in jurisdictions without regulatory oversight would argue that the continuous-exercise mandate is over-provisioned and that risk could be managed cheaper through periodic high-fidelity validation. They are excluded from the regulatory dialog because the mandate treats simulation-sufficiency as a settled question rather than a live technical dispute.
% DISAPPEARANCE_RATIONALE: If the continuous-exercise mandate disappeared, organizations would shift to lower-cost periodic certification, personnel would recover operational time, and catastrophic-event response competence would degrade measurably over multi-year intervals — the constraint structures how competence is maintained, not a background fact of human cognition.
% FOUNDING_PROBLEM: Rare catastrophic events (aviation disasters, nuclear emergencies, maritime disasters) require personnel competence in scenarios they have never experienced in reality. One-time simulation testing creates a moment of validated readiness but no mechanism to maintain competence through decades without retraining. The founding problem: how to prevent competence decay in the absence of actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Catastrophe investigation boards and aviation safety organizations (NTSB, ICAO) document competence decay in incident histories; independent safety researchers and human-factors specialists corroborate that untrained personnel competence does deteriorate over time; insurance loss data shows measurable risk reduction with continuous exercise regimes. The safety management regime also attests the problem is live, but independent external corroboration from investigation boards and research literature is the primary evidence.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness runs moderate-to-high (0.68 at interval end) because the constraint imposes ongoing organizational cost and personnel time burden that cannot be reduced without regulatory risk, even when no catastrophe occurs for years. The founding problem is live (catastrophe risk is perpetual), so the burden persists. Suppression is moderate (0.52): the constraint is enforced through regulatory mandate and insurance requirements, but resistance is substantial—organizations legitimately question whether continuous exercise yields catastrophic-risk reduction proportional to its cost. Theater ratio is moderate (0.41): genuine competence benefit exists (drills do maintain capability), but a growing share of exercise activity is defensive ritual against regulatory scrutiny rather than competence-driven. The measurement series tracks increasing extraction over 40 time units as exercise requirements intensify and regulatory auditing burdens grow; theater ratio rises as compliance documentation becomes as important as the exercise itself. The shared time grid ensures every metric is authored at every sampled point, enabling temporal coherence analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the safety management regime's position, continuous exercise is the epistemic requirement for competence maintenance—it is not optional. From the resource-constrained organization's position, the mandate imposes ongoing cost that might be defensible if measured risk reduction were proportional; it looks like over-provisioning. From operational personnel's position, the time burden is real and the benefit is distributed (applied only in rare catastrophes they may never experience). The engine computes per-seat divergence: the agenda-setter's reading is coordination necessity; the payer's reading is extraction with rituality.
 *
 * DIRECTIONALITY LOGIC:
 *   The safety management regime is the beneficiary (sets the standard, collects institutional authority, avoids liability for competence failures). Insurance and liability regimes are partial beneficiaries (lower catastrophic loss probability). Operational personnel and resource-constrained organizations are the payers (incur cost and time burden, constrained exit by regulation). Rare-catastrophe victims are diffuse beneficiaries whose membership is unknown until an event occurs. Directionality flows from this asymmetry: agenda-setters have d near 0.0–0.2 (beneficiary end); payers have d near 0.7–0.9 (target end); observers near 0.5 (symmetric analytic position).
 *
 * MANDATROPHY ANALYSIS:
 *   The continuous-exercise requirement was founded to solve a genuine collective-action problem: without coordination, organizations would under-invest in rare-catastrophe preparedness because the benefit accrues only in low-probability futures. The constraint successfully coordinates that investment. However, the measured theater ratio (0.41 and rising) suggests that compliance documentation and regulatory audit activity increasingly comprise the exercise regime, decoupled from genuine competence gain. This is not yet mandatrophy (the founding problem remains live; catastrophic risk is perpetual), but the theater signal indicates that exercise activity is partly displaced toward defensive ritual. The constraint should be monitored for the pattern: if theater_ratio approaches 0.6+ and resistance drops below 0.3, mandatrophy would begin (the organization maintains exercise compliance as theater without competence benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_empirical_claim,
    'Does competence actually decay measurably over time in the absence of real catastrophes, and does continuous exercise prevent decay better than periodic high-fidelity simulation?',
    'Longitudinal competence testing across organizations with different exercise regimes (continuous vs. periodic-high-fidelity); post-incident analysis of personnel performance in real catastrophes vs. exercise history; randomized trials on competence retention curves.',
    'If continuous exercise produces measurably better performance outcomes, the constraint''s founding problem is confirmed and the extraction cost is justified as coordination overhead. If periodic high-fidelity testing produces equivalent outcomes, the constraint shifts from tangled_rope (coordination+extraction) to snare (pure extraction with coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_empirical_claim, empirical, 'Whether the empirical claim that continuous exercise prevents competence decay better than periodic testing is true.').

omega_variable(
    kernel_reading_contest_simulation_sufficiency,
    'Is the founding problem solved by simulation-as-proxy (one test that occupies competence) or does it require the continuous-refresh-hybrid (repeated exercise across time)? Are the readings logically contradictory or merely empirically divergent?',
    'If competence is a decaying skill (empirical), then readings coexist: one reading claims periodic testing suffices, the other claims continuous exercise is necessary. If competence is a state-property (not decaying), then readings foreclose: they cannot both be true. The empirical claim about decay dynamics determines the logical structure.',
    'If readings coexist empirically but one produces better outcomes, this reading influences but does not foreclose the sibling. If readings are logically contradictory (one claims decay, other claims no decay), this reading forecloses the ''simulation_as_proxy'' reading when decay is empirically confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_simulation_sufficiency, conceptual, 'Whether the sibling readings are logically contradictory or empirically divergent on competence decay dynamics.').

omega_variable(
    suppression_mechanism_extraction_vs_coordination,
    'Is the measured suppression (0.52) enforcing coordination (ensuring organizations invest collectively) or defending extraction (preventing cheaper alternatives)?',
    'Analyze regulatory exemptions and premium reductions: if organizations that invest heavily in continuous exercise receive exemptions from other safety requirements or insurance rate reductions proportional to cost, suppression is coordination-defending. If exemptions are minimal and rates remain high, suppression is extraction-defending.',
    'If suppression defends coordination, the constraint is tangled_rope: coordination necessity with enforcement cost. If suppression defends extraction, the constraint shifts toward snare: the continuous-exercise mandate is imposed to justify organizational overhead regardless of competence gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_extraction_vs_coordination, empirical, 'Whether suppression enforces coordination or extraction.').

omega_variable(
    identity_lock_personnel_internalized_compliance,
    'Do operational personnel continue to accept the continuous-exercise burden because they believe in competence maintenance (endorsing the constraint''s premise) or because the burden is internalized as ''just how we work'' (identity-locked compliance)?',
    'Post-exit interviews with personnel who leave safety-critical roles; surveys asking whether personnel would change exercise frequency if permitted; observation of voluntary exercise beyond regulatory minimums.',
    'If compliance is belief-based, suppression is lower (personnel are not coerced, they coordinate). If compliance is identity-locked (personnel continue even when they doubt effectiveness), suppression is higher and internalized—personnel carry the constraint with them even after exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_personnel_internalized_compliance, empirical, 'Whether personnel''s acceptance of continuous-exercise burden is belief-based or identity-locked.').

omega_variable(
    reading_empirical_grounds_continuous_vs_periodic,
    'What is the epistemic status of the claim that continuous exercise produces better catastrophic-event competence outcomes than periodic high-fidelity simulation? Is this an empirically settled claim or a foundational axiom of this reading?',
    'Systematic literature review of competence-retention studies; post-catastrophe incident analysis comparing exercise histories to performance; controlled competence testing across exercise regimes.',
    'If the claim is empirically settled (strong evidence exists), the reading''s distinction is factual and the sibling readings can be evaluated. If the claim is axiomatically endorsed but empirically unsettled, this reading''s foundational assertion is contested and the kernel debate is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_empirical_grounds_continuous_vs_periodic, empirical, 'Empirical status of the continuous-exercise-superiority claim that grounds this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.18).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% The 'competence_exercise_validity' kernel is contested across three readings: (1) continuous_refresh_hybrid—this constraint—asserts competence is process-dependent and requires continuous exercise; (2) simulation_as_proxy claims one high-fidelity test is sufficient; (3) real_catastrophe_only claims only real catastrophes validate competence. These are not observational variants of a single constraint; they instantiate different epistemic criteria for 'what proves competence retention.' Each reading has its own beneficiary/victim structure, extraction profile, and ε value. Empirical research on competence decay dynamics will resolve which readings' foundational claims are supported; the continuous-refresh-hybrid reading is authored as tangled_rope (genuine coordination plus extraction) with the assumption that continuous exercise does prevent decay better than periodic testing. If empirical evidence refutes decay-prevention superiority, the reading collapses from tangled_rope to snare (pure extraction under coordination cover).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__continuous_refresh_hybrid, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
