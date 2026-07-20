% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Hybrid Decay Reading: Simulation Maintains Procedures but Not Judgment-Under-Stakes
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint is the hybrid_decay_reading of the contested kernel
 *   'exercise_as_competence_maintenance.' It models the institutionalized
 *   practice in safety-critical industries of relying on simulation exercises
 *   to maintain operator competence. The reading posits that simulation
 *   genuinely exercises procedural and muscle-memory components of
 *   competence, but fails to exercise judgment-under-stakes and
 *   improvisational capacity, which decay under a simulation-only regime. The
 *   constraint thus carries a real coordination function (procedural
 *   retention) alongside asymmetric extraction (false confidence, deferred
 *   risk, and harm to operators and the public when judgment gaps surface in
 *   real catastrophes). Sibling readings include
 *   simulation_sufficiency_reading (high-fidelity simulation is adequate for
 *   the whole kernel) and lived_catastrophe_necessity_reading (only real
 *   catastrophe exercises competence).
 *
 * KEY AGENTS:
 *   - organizational_safety_leadership (agenda_setter/institutional) â designs and certifies the simulation regime, captures cost avoidance and risk deferral
 *   - simulation_technology_vendors (beneficiary/organized) â supply training infrastructure, benefit from institutional lock-in
 *   - regulatory_auditors (beneficiary/institutional) â gain auditable compliance artifacts
 *   - frontline_operators (payer/moderate) â retain procedures but bear the judgment gap in real events
 *   - catastrophe_exposed_public (payer/powerless) â downstream risk bearers excluded from training design
 *   - veteran_crisis_practitioners (excluded/moderate) â real-event expertise sidelined from curriculum design
 *   - human_factors_researchers (observer/analytical) â document the bifurcation but lack institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.65).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.42).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Hybrid Decay Reading: Simulation Maintains Procedures but Not Judgment-Under-Stakes").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '41671dba-7215-4e4f-b096-45300011cffd').
narrative_ontology:cs_kernel_codification('41671dba-7215-4e4f-b096-45300011cffd', fixed_text).
narrative_ontology:cs_authority_grounding('41671dba-7215-4e4f-b096-45300011cffd', expertise).
narrative_ontology:cs_interpretation_layer_present('41671dba-7215-4e4f-b096-45300011cffd').
narrative_ontology:cs_reading_relation('41671dba-7215-4e4f-b096-45300011cffd', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('41671dba-7215-4e4f-b096-45300011cffd', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('41671dba-7215-4e4f-b096-45300011cffd', foundational, procedural_judgment_bifurcation).
narrative_ontology:cs_axiom_status(procedural_judgment_bifurcation, holdable).
narrative_ontology:cs_axiom_grounding('41671dba-7215-4e4f-b096-45300011cffd', procedural_judgment_bifurcation, empirically_contingent).
narrative_ontology:cs_axiom('41671dba-7215-4e4f-b096-45300011cffd', foundational, simulation_insufficient_for_stakes).
narrative_ontology:cs_axiom_status(simulation_insufficient_for_stakes, holdable).
narrative_ontology:cs_axiom_grounding('41671dba-7215-4e4f-b096-45300011cffd', simulation_insufficient_for_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('41671dba-7215-4e4f-b096-45300011cffd', bifurcated_competence_model).
narrative_ontology:cs_drift_state('41671dba-7215-4e4f-b096-45300011cffd', contemporary_institutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41671dba-7215-4e4f-b096-45300011cffd', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_safety_leadership).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_auditors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, catastrophe_exposed_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, budgets, and certifies the simulation-based training regime. Claims organizational competence maintenance to regulators and boards. Bears the institutional cost and liability exposure of live high-stakes exercises. Has formal authority to modify training protocols but faces budget pressure, risk aversion, and career incentives that favor scalable simulation programs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_safety_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell simulation platforms, scenarios, and certification tools to safety-critical organizations. Revenue scales with institutional commitment to simulation as the primary training modality. Advocate for fidelity improvements while accepting the structural premise that simulation is the appropriate training channel.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Audit organizational compliance against training-hour and simulation-performance standards. Benefit from auditable, repeatable simulation records that simplify certification. Their frameworks historically treat procedural execution as synonymous with operational competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_auditors, beneficiary,
    institutional, generational, constrained, national).

% Complete mandatory simulation cycles, retaining procedural fluency. In real events they face stakes, time pressure, and novel configurations absent from training scenarios. They bear the gap between certified competence and demanded performance, including legal, professional, and psychological consequences when judgment errors occur.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Live or work downstream of safety-critical operations. They depend on operator judgment in rare catastrophes but have no presence in training design. When simulation-certified operators fail under real stakes, the public bears injury, evacuation, economic loss, and death without having been party to the competence-certification decision.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, catastrophe_exposed_public, payer,
    powerless, immediate, trapped, regional).

% Possess real-event experience that attests to the difference between simulated and actual stakes. Their testimony is systematically sidelined in training design because it undermines the cost-efficiency and safety record of simulation-centric regimes. They would argue for live-fire or high-consequence drills but are excluded from curriculum authority.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, veteran_crisis_practitioners, excluded,
    moderate, biographical, mobile, national).

% Study skill retention, stress physiology, and decision-making under uncertainty. Their empirical work documents the bifurcation between procedural retention and adaptive judgment decay, but their findings are consumed selectively by institutional stakeholders.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, human_factors_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_safety_leadership).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains procedural competence, protocol adherence, and muscle memory across distributed operator populations through repeatable, low-risk scenarios, ensuring that standardized responses remain available when cued.
% TRANSFER_FUNCTION: Moves institutional confidence, regulatory certification, and operational risk-assessment value from the training room to the operational domain, while transferring the unexercised judgment burden and its associated catastrophe risk to frontline operators and downstream populations.
% ABSENT_VOICES: Veteran practitioners with lived catastrophe experience are excluded from curriculum authority; the catastrophe-exposed public is absent from training design; frontline operators who sense the judgment gap are heard in debriefs but rarely empowered to alter training architecture.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would lose their primary auditable competence-certification mechanism, regulatory frameworks would require redesign around experiential or live-stakes validation, training budgets would reallocate, and the current liability and insurance equilibrium predicated on simulation hours would collapse.
% FOUNDING_PROBLEM: High-stakes operational environments require maintained competence, but real catastrophes are too rare, dangerous, and unpredictable to serve as routine training events; organizations needed a safe, repeatable, scalable method to keep responders prepared.
% FOUNDING_PROBLEM_CORROBORATION: Safety scientists and human factors researchers outside the simulation-vendor ecosystem attest that the founding problem remains live. They also attest that the chosen solution only partially addresses it, corroborating the contested status. Organizational leadership and vendors attest it is solved.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial gap between certified competence and actual crisis performance: organizations claim full readiness while a critical component decays. Suppression (0.42) is moderate: alternatives (live-fire drills, real-event exposure) are not violently suppressed but are institutionally disfavored due to cost, danger, and liability. Theater_ratio (0.58) is elevated because much simulation activity is performative compliance â ticking boxes, logging hours â that signals competence without exercising judgment. Accessibility_collapse (0.48) captures that viable alternatives to simulation-centric training exist but are costly and organizationally difficult to access. Resistance (0.32) is low-to-moderate: frontline operators and researchers sense the gap but lack power to restructure training regimes. The measurement series shows extraction and theater rising together as simulation institutionalizes from T=0 to T=40.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (organizational safety leadership) experiences the constraint as successful coordination: auditable, scalable, low-incident training that satisfies regulatory demand. The payer seats (operators, public) experience the same structure as deferred risk â a competence claim that becomes a liability when real stakes reveal the judgment deficit. The observer seat (researchers) sees both functions simultaneously: genuine procedural retention and genuine judgment decay. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (safety leadership, vendors, auditors) have low directionality because the constraint subsidizes their interests: cost control, revenue, and auditable compliance. Victims (operators, public) have high directionality because the constraint extracts from them in the form of uncompensated risk and real-stakes performance demands. The veteran practitioners are excluded rather than coordinated, sitting outside the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Because procedural competence IS genuinely maintained (the coordination function is real), the constraint cannot be a pure Snare; labeling it so would ignore the actual skill retention that simulation provides. Because judgment-under-stakes decays and identifiable victims bear the cost of that decay, the constraint cannot be a pure Rope; labeling it so would ignore the asymmetric extraction embedded in the false-confidence certification. Tangled Rope captures the hybrid: both coordination and extraction operate through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the hybrid_decay_reading of exercise_as_competence_maintenance; the simulation_sufficiency_reading treats fidelity as the only variable, while lived_catastrophe_necessity_reading denies simulation any exercise value. Does the partial-retention model correctly identify two distinct competence components, or obscure a continuum?',
    'Comparative longitudinal studies tracking procedural retention and adaptive judgment outcomes across simulation-only, hybrid, and lived-exposure cohorts.',
    'If the components are separable, this reading stands as a distinct constraint; if a continuum, the reading collapses toward one sibling or requires epsilon decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer omega positioning this reading within its kernel family').

omega_variable(
    judgment_exercise_threshold,
    'Is judgment-under-stakes fundamentally non-simulable, or does it require simulation fidelity beyond current organizational willingness to fund?',
    'Meta-analysis of high-fidelity immersive simulations with genuine consequential stakes versus standard procedural drills, measuring improvisational performance in real events.',
    'If judgment is simulable at higher fidelity, the victim set shrinks and the constraint shifts toward rope; if fundamentally non-simulable, the victim set expands and the extraction intensifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judgment_exercise_threshold, empirical, 'Whether judgment decay is contingent on fidelity or intrinsic to simulation').

omega_variable(
    institutional_cost_avoidance,
    'Is the judgment-under-stakes gap unrecognized due to epistemic limits, or known but tolerated because correcting it would require accepting the cost and danger of live high-stakes exercises?',
    'Whistleblower and incident-investigation testimony, organizational ethnography, and budget-allocation analysis comparing simulation procurement to live-exercise expenditure.',
    'If the gap is known but tolerated, the constraint''s theater_ratio and suppression scores should be revised upward toward snare-like operation; if genuinely unrecognized, the constraint remains a tangled rope driven by sincere coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_cost_avoidance, empirical, 'Whether judgment decay is hidden by institutional cost avoidance or epistemic blindness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_decay_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(exercise_decay_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(exercise_decay_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(exercise_decay_tr_t30, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(exercise_decay_tr_t40, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(exercise_decay_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exercise_decay_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(exercise_decay_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(exercise_decay_be_t30, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(exercise_decay_be_t40, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 40, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(exercise_as_competence_maintenance__hybrid_decay_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
