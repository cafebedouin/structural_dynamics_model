% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe Only Validates Competence
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   The constraint is the claim that only real catastrophic events genuinely
 *   exercise and validate organizational safety competence; simulations,
 *   drills, and tabletop exercises are structurally insufficient substitutes.
 *   This reading of the competence_exercise_validity kernel holds that
 *   competence retention decays invisibly under simulation-only regimes, and
 *   that a clean safety record reflects luck or systemic redundancy rather
 *   than proven adequacy. The simulation industry and regulators benefit from
 *   the mandate structure; operating organizations pay for drills that don't
 *   validate readiness; workers and the public bear the tail risk when
 *   untested competence fails. The claimed type is tangled_rope: there is a
 *   genuine coordination function (standardized drills create shared
 *   procedural language, cross-team familiarity, and regulatory legibility)
 *   but it is coupled with asymmetric extraction (simulation vendors capture
 *   mandated spend, regulators gain legibility without accountability, real
 *   competence remains unproven). The measurement series shows rising
 *   extractiveness and theater ratio over 45 years as drill mandates expanded
 *   while novel-accident rates did not correspondingly fall.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.65).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.55).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe Only Validates Competence").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, 'f3ad081e-4c90-4b4e-a29a-06471347764e').
narrative_ontology:cs_kernel_codification('f3ad081e-4c90-4b4e-a29a-06471347764e', distributed).
narrative_ontology:cs_authority_grounding('f3ad081e-4c90-4b4e-a29a-06471347764e', practice).
narrative_ontology:cs_interpretation_layer_present('f3ad081e-4c90-4b4e-a29a-06471347764e').
narrative_ontology:cs_reading_relation('f3ad081e-4c90-4b4e-a29a-06471347764e', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('f3ad081e-4c90-4b4e-a29a-06471347764e', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('f3ad081e-4c90-4b4e-a29a-06471347764e', foundational, only_real_catastrophe_validates_competence).
narrative_ontology:cs_axiom_status(only_real_catastrophe_validates_competence, holdable).
narrative_ontology:cs_axiom_grounding('f3ad081e-4c90-4b4e-a29a-06471347764e', only_real_catastrophe_validates_competence, empirically_contingent).
narrative_ontology:cs_axiom('f3ad081e-4c90-4b4e-a29a-06471347764e', foundational, simulation_masks_competence_decay).
narrative_ontology:cs_axiom_status(simulation_masks_competence_decay, holdable).
narrative_ontology:cs_axiom_grounding('f3ad081e-4c90-4b4e-a29a-06471347764e', simulation_masks_competence_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('f3ad081e-4c90-4b4e-a29a-06471347764e', simulation_sufficiency_paradigm).
narrative_ontology:cs_drift_state('f3ad081e-4c90-4b4e-a29a-06471347764e', post_major_accident_investigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f3ad081e-4c90-4b4e-a29a-06471347764e', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, safety_regulators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, operating_organizations).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, workers_public).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, competence_requires_live_fire_testing).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, simulation_cannot_replicate_systemic_stress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provides mandated simulator hardware, scenario software, instructor certification, and recurring training services to nuclear, aviation, chemical, and medical operators. Revenue scales with regulatory drill-frequency mandates. Actively shapes standards through industry associations and standards committees. Can pivot across domains and jurisdictions; exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_training_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Mandate drill frequencies, scenario requirements, and certification standards. Gain legibility into operator readiness, enforcement authority, and institutional legitimacy from the compliance regime. The drill mandate is a primary regulatory lever; alternatives (continuous assessment, operational learning metrics) are less auditable. Exit is analytical — they study the system but are not subject to it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_regulators, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, safety_regulators, agenda_setter).

% Pay for mandated drills, simulator time, instructor fees, and downtime. Bear liability if drills don't prevent accidents. License requirements constrain exit — they cannot operate without regulatory approval, which requires drill compliance. Some invest in supplementary validation (red teams, operational learning) but these don't substitute for mandated drills.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, operating_organizations, payer,
    powerful, biographical, constrained, global).

% Bear the tail risk when untested competence fails catastrophically (plant workers, nearby communities, passengers, patients). Have no exit from systemic risk — cannot choose operators based on validation regime. Their safety depends on a competence-validation system they cannot audit or influence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, workers_public, payer,
    powerless, biographical, trapped, local).

% Study the simulation-competence gap across domains. Publish on drill decay rates, scenario fidelity limits, and alternative validation models. Their work informs but does not determine regulatory choices. They see the full structure but occupy no seat within it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared, auditable, cross-organizational language for emergency response: standardized terminology, practiced handoffs, verified equipment familiarity, and regulatory legibility that enables licensing and insurance.
% TRANSFER_FUNCTION: Moves mandated training budgets from operating organizations to the simulation industry; moves regulatory legitimacy from outcome-based oversight to process-compliance oversight; moves tail risk from the system to workers and the public.
% ABSENT_VOICES: Frontline operators who know drill scenarios by heart but have never faced novel cascading failures; communities downstream of high-hazard sites who bear risk without representation in standards committees; insurers who price based on drill compliance rather than demonstrated resilience.
% DISAPPEARANCE_RATIONALE: If the drill mandate vanished, the simulation industry would lose its primary revenue anchor; regulators would lose their most auditable lever; operating organizations would face liability-pressure to develop alternative validation (likely continuous refresh or operational learning); workers/public would face a transition period with no mandated validation at all. The safety-validation ecosystem would reorganize — possibly toward the continuous_refresh_hybrid model, possibly toward fragmentation.
% FOUNDING_PROBLEM: Post-WWII high-hazard industries (nuclear, aviation, chemical) needed a scalable, auditable way to validate that operators could execute emergency procedures under stress. Live exercises were too dangerous; tabletop drills were too weak. Full-scope simulation promised a middle ground: realistic stress without real consequences.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry and regulators attest the problem is live (new hazards, workforce turnover, technology change). Major accident investigation boards (Columbia, Deepwater Horizon, Fukushima) and independent safety researchers attest the founding problem is substantially unsolved by the current mandate — drills validate procedure execution, not systemic competence under novel stress. No external corroboration supports the claim that mandated drills alone validate competence.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the gap between mandated drill costs and validated competence gains; the simulation industry collects rents proportional to mandate scope, not marginal training value. Theater ratio (0.70) is high because drill compliance became a proxy for competence — organizations perform drills to satisfy auditors, not to discover unknown failure modes. Suppression (0.55) is moderate: liability frameworks and insurance requirements make alternative validation models legally risky, but the suppression is partly cultural (safety culture equates drill compliance with competence). Accessibility collapse (0.60) reflects that alternative models exist (continuous refresh, red-teaming, operational learning) but are marginalized by the compliance regime. Resistance (0.50) comes from operating organizations that see drill costs rising without incident-rate improvements, and from safety researchers documenting the simulation-competence gap.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/industry seat, the constraint appears as rope: drills coordinate terminology, expose gross errors, create auditable records. From the operating-organization seat, it appears as snare: costs rise, novel scenarios aren't tested, compliance crowds out operational learning. From the worker/public seat, it appears as mountain: the risk is structural and inescapable. The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation_training_industry is the primary beneficiary (collects mandated revenue, shapes standards — d near 0.1). Safety_regulators are secondary beneficiaries (gain legibility, enforcement authority, institutional legitimacy — d near 0.2). Operating_organizations are payers (bear drill costs, liability exposure if drills don't prevent accidents, constrained exit due to license requirements — d near 0.8). Workers_public are payers/victims (bear tail risk when untested competence fails, no exit from systemic risk — d near 0.9). Safety_analysts are observers (analytical seat, d=0.5). The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (validating competence for high-hazard operations) is contested: major accidents (Bhopal, Challenger, Deepwater Horizon, Fukushima) occurred in heavily drilled organizations, suggesting the mandate solved a narrower problem (procedural compliance) than the one it claimed to solve (competence validation). The arrangement persists because no stakeholder has both the incentive and the power to replace it: industry captures the revenue, regulators capture the legitimacy, organizations fear liability if they deviate, and the public lacks technical standing. This is classic mandatrophy — the mandate outlived its function but persists through distributed inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is the claim that only real catastrophe validates competence a reading of the contested kernel ''competence_exercise_validity'', and what would sibling readings change structurally?',
    'Trace institutional discourse across nuclear, aviation, chemical, and medical safety domains to map which actors hold which reading; the kernel persists because no reading has achieved foreclosure.',
    'If this is one reading among three, the constraint''s ε and victim/beneficiary structure are reading-indexed; the sibling readings instantiate different constraints with different extraction profiles. The engine must not average across them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is the real_catastrophe_only reading of the competence_exercise_validity kernel; siblings are simulation_as_proxy and continuous_refresh_hybrid.').

omega_variable(
    simulation_industry_capture,
    'Does the simulation/training industry actively suppress adoption of continuous-refresh or hybrid models to protect its mandated-drill revenue stream?',
    'Follow lobbying expenditures, standards-committee composition, and regulatory-comment records across domains; look for coordinated opposition to drill-frequency increases or scenario-diversity mandates.',
    'If capture is documented, the beneficiary declaration for simulation_training_industry shifts from incidental to structural extraction; the constraint''s coordination function is narrower than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_industry_capture, empirical, 'Whether simulation industry rent-seeking shapes the regulatory mandate that enforces the constraint.').

omega_variable(
    competence_decay_measurement,
    'Can competence decay be measured without a catastrophic trigger, or is the decay inherently latent until exercised?',
    'Longitudinal studies of teams with known drill histories but no catastrophic events; compare performance on novel scenarios vs. rehearsed ones; measure skill-transfer decay rates.',
    'If decay is measurable without catastrophe, the continuous_refresh_hybrid reading gains empirical ground; if decay is truly latent, the real_catastrophe_only reading''s foundational axiom is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_measurement, empirical, 'Whether the core empirical claim of this reading (decay is invisible until real catastrophe) is falsifiable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative competence-validation models structural (regulatory mandate, liability frameworks) or internalized (safety culture treats drill compliance as competence proof)?',
    'Post-deregulation or post-accident suppression trajectory: if organizations voluntarily maintain drill-heavy regimes when mandates relax, suppression is partly internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests; the constraint persists even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in safety-culture contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_exercise_validity__real_catastrophe_only, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_validity__real_catastrophe_only, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(comp_tr_t2000, competence_exercise_validity__real_catastrophe_only, theater_ratio, 2000, 0.63).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_validity__real_catastrophe_only, theater_ratio, 2010, 0.67).
narrative_ontology:measurement(comp_tr_t2020, competence_exercise_validity__real_catastrophe_only, theater_ratio, 2020, 0.69).
narrative_ontology:measurement(comp_tr_t2025, competence_exercise_validity__real_catastrophe_only, theater_ratio, 2025, 0.7).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(comp_be_t2000, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(comp_be_t2020, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(comp_be_t2025, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(comp_su_t2000, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(comp_su_t2020, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(comp_su_t2025, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This is the real_catastrophe_only reading of the competence_exercise_validity kernel. The sibling readings instantiate different constraints with different ε values and different beneficiary/victim structures. They are linked because they share the same kernel question but answer it with different structural commitments. The upstream constraint (simulation_as_proxy) historically influenced this one by establishing the mandate regime this reading critiques.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
