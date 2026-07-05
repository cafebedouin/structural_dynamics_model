% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the simulation-sufficiency reading of the
 *   exercise-as-competence-maintenance kernel: the claim that a sufficiently
 *   high-fidelity simulated catastrophe genuinely exercises the same
 *   competence kernel a real catastrophe would, and that retention
 *   effectiveness is a function of simulation fidelity alone. Under this
 *   reading, regulatory drill completion is treated as adequate proof of
 *   maintained competence; certification bodies, operating organizations, and
 *   the simulation-vendor industry each have structural reasons to endorse
 *   this reading, since it converts an otherwise irreducible readiness
 *   problem into an auditable, budgetable, sellable compliance product. The
 *   reading is not straightforwardly false — well-designed simulation
 *   training does transfer real skill — but the operative standard drifts
 *   toward 'completed the mandated drill' as a proxy for 'demonstrated
 *   real-world competence,' and the gap between those two things is borne by
 *   frontline responders and the public during actual incidents, not by the
 *   parties who set or profit from the standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.38).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e9772737-e696-4f60-a769-c2c6c183df36').
narrative_ontology:cs_kernel_codification('e9772737-e696-4f60-a769-c2c6c183df36', formalized).
narrative_ontology:cs_authority_grounding('e9772737-e696-4f60-a769-c2c6c183df36', extraction).
narrative_ontology:cs_interpretation_layer_present('e9772737-e696-4f60-a769-c2c6c183df36').
narrative_ontology:cs_reading_relation('e9772737-e696-4f60-a769-c2c6c183df36', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('e9772737-e696-4f60-a769-c2c6c183df36', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('e9772737-e696-4f60-a769-c2c6c183df36', foundational, simulation_constitutes_genuine_kernel_exercise).
narrative_ontology:cs_axiom_status(simulation_constitutes_genuine_kernel_exercise, holdable).
narrative_ontology:cs_axiom_grounding('e9772737-e696-4f60-a769-c2c6c183df36', simulation_constitutes_genuine_kernel_exercise, empirically_contingent).
narrative_ontology:cs_axiom('e9772737-e696-4f60-a769-c2c6c183df36', secondary, fidelity_is_the_sole_determinant_of_retention).
narrative_ontology:cs_axiom_status(fidelity_is_the_sole_determinant_of_retention, holdable).
narrative_ontology:cs_axiom_grounding('e9772737-e696-4f60-a769-c2c6c183df36', fidelity_is_the_sole_determinant_of_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('e9772737-e696-4f60-a769-c2c6c183df36', regulatory_drill_cadence_as_competence_proof).
narrative_ontology:cs_drift_state('e9772737-e696-4f60-a769-c2c6c183df36', post_high_fidelity_simulator_adoption, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9772737-e696-4f60-a769-c2c6c183df36', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendor_industry).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, operating_organizations).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_crisis_responders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, downstream_public_affected_by_low_fidelity_gaps).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_completion_as_competence_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the drill-cadence and pass/fail criteria that define what counts as maintained competence. Certifies organizations as compliant based on completed simulation exercises, not on any independent measure of real-catastrophe performance. Bears no direct cost if simulation fidelity turns out to be inadequate during an actual event.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_certification_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Runs the mandated simulations, documents completion, and receives certification and legal cover in exchange. Chooses simulation vendors and scenario design, which lets it tune fidelity (and cost) downward while remaining compliant. Avoids the much larger cost of live-stakes training or continuous real-incident rotation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, operating_organizations, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, operating_organizations, agenda_setter).

% Sells the simulators, scenario packages, and certification-adjacent training products that regulatory drill mandates require. Revenue scales with the number of mandated exercises, not with demonstrated fidelity-to-outcome; has structural incentive to keep the bar at 'completed drill' rather than 'demonstrated real-world transfer.'
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendor_industry, beneficiary,
    organized, biographical, mobile, national).

% Certified as competent after passing drills, then deployed into actual catastrophes where the simulator's fidelity gaps (timing pressure, ambiguous information, irreversible consequences) are absent from training. Bears the immediate cost when judgment-under-stakes was never actually exercised, but has no channel to contest the certification standard before an incident occurs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_crisis_responders, payer,
    moderate, immediate, trapped, local).

% Depends on responders' actual competence during real catastrophes. Has no visibility into simulation fidelity standards and no role in setting them; discovers the gap only when a real event exposes a competence failure that certification had already blessed as adequate.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, downstream_public_affected_by_low_fidelity_gaps, payer,
    powerless, immediate, trapped, regional).

% Studies transfer-of-training effects between simulated and real catastrophe response. Publishes findings on which simulation elements do and do not predict real-world performance, but has no authority to revise certification criteria directly.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, auditable, scalable mechanism for maintaining and verifying crisis-response competence across large workforces without requiring anyone to be exposed to actual catastrophe to prove readiness.
% TRANSFER_FUNCTION: Moves compliance risk and training cost from organizations to a certified-adequate status, while moving the residual risk of any fidelity gap onto frontline responders (who face the real event undertrained in exactly the dimensions the simulation didn't capture) and the public who depend on their performance.
% ABSENT_VOICES: Frontline responders who have direct knowledge of where the simulator diverges from real incidents rarely have a formal channel into scenario-design or certification-standard revision; the downstream public affected by a given jurisdiction's responder competence has essentially no voice in what fidelity threshold is treated as sufficient.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency standard vanished overnight, certification bodies would need an entirely different (and more expensive, more contested) basis for declaring competence maintained — likely reverting toward live-incident rotation, apprenticeship models, or explicit acknowledgment of irreducible uncertainty about untested responders. Vendor revenue tied to mandated drills would collapse; organizations would face a much harder and costlier compliance problem.
% FOUNDING_PROBLEM: Real catastrophes are rare, high-stakes, and ethically impossible to manufacture on demand for training purposes, yet responder competence must be maintained and demonstrated between rare real events without waiting for the next disaster to find out who is ready.
% FOUNDING_PROBLEM_CORROBORATION: Certification bodies and operating organizations attest the problem is solved by current drill regimes. Safety researchers studying post-incident after-action reports — a source outside the beneficiary set — have documented recurring gaps between drill-certified competence and actual performance under real timing pressure and irreversible stakes, corroborating that the founding problem (verifying real-world-transferable competence) remains only partially addressed by simulation alone.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).
:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the coordination function is real — simulations do build and refresh procedural skill — but a growing share of the certification apparatus rewards drill completion over demonstrated transfer, which is where the extraction concentrates. Theater ratio is the most diagnostic metric here and is authored as rising past 0.5 by later time points is avoided deliberately — it climbs to 0.47, reflecting substantial but not yet dominant performative compliance (box-checking drills, vendor-scripted scenarios optimized for pass rates rather than fidelity) without claiming the function has fully hollowed out. Suppression is comparatively low (0.38) because responders are not coerced into accepting the standard through threat; the suppression that exists is structural (no formal channel to contest certification criteria) rather than violent or overtly coercive.
 *
 * DIRECTIONALITY LOGIC:
 *   Certification bodies, operating organizations, and the vendor industry sit near the beneficiary end: they set or profit from a standard that converts an expensive, uncertain readiness problem into a bounded, certifiable, purchasable compliance activity, and none of them bears the tail-risk cost of a fidelity gap. Frontline responders sit near the target end: they are certified competent on the strength of a standard they did not set, and they alone face the moment where simulated and real catastrophe diverge. The downstream public is even further toward the target end, since they have no visibility into or voice over the standard at all and only discover its adequacy or inadequacy after the fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifying competence without waiting for real disasters) remains genuinely live — this prevents an outright snare classification, since there is authentic coordination value in structured simulation. But the vindicated proposition ('drill completion as competence proxy') has begun to substitute for the actual goal (demonstrated real-world transfer), which is the classic signature of a coordination function drifting toward extraction while retaining its original justification. The tangled_rope classification captures this: genuine coordination (simulation does build skill) coexisting with asymmetric extraction (certification bodies and vendors capture the compliance value while responders and the public absorb the fidelity-gap risk), sustained by active enforcement (regulatory mandate).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_transfer_ambiguity,
    'Does high-fidelity simulation genuinely transfer judgment-under-stakes competence, or does it only transfer procedural/mechanical competence while leaving the stakes-perception component of the kernel unexercised regardless of simulation quality?',
    'Longitudinal comparison of certified-competent responders'' real-incident performance against their simulation scores, stratified by incident type (procedural vs. high-ambiguity/high-stakes), with fidelity as a covariate.',
    'If fidelity fully explains the transfer gap, the simulation-sufficiency reading is vindicated and this constraint approaches a rope (genuine coordination, low extraction). If a residual gap persists even at maximal fidelity, the hybrid_decay_reading''s structural claim is correct and this reading''s beneficiaries are extracting from an incomplete competence-substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_transfer_ambiguity, empirical, 'Whether simulation fidelity alone (as this reading claims) accounts for real-world competence transfer, or whether an irreducible non-simulable component exists.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the simulation-sufficiency reading selected because it is structurally correct, or because it is the reading that minimizes cost and legal exposure for the parties who control certification standards?',
    'Examine whether certification bodies and operating organizations would adopt a more stringent reading (hybrid_decay or lived_catastrophe_necessity) if cost were held constant — e.g., via natural experiments where high-fidelity-mandate jurisdictions are compared to low-fidelity-mandate jurisdictions on real-incident outcomes.',
    'If the selection tracks cost minimization rather than evidence, the tangled_rope classification is reinforced — the reading itself functions as extraction cover. If the selection tracks genuine evidence of sufficiency, the reading is closer to a legitimate rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the choice of kernel reading is evidence-driven or interest-driven.').

omega_variable(
    victim_set_boundary_ambiguity,
    'The expected structural delta for this reading confines the victim set to those harmed by inadequate SIMULATION FIDELITY specifically — does this artificially narrow the victim set relative to what the lived_catastrophe_necessity_reading would count as victims (anyone harmed by simulation-based certification at all, regardless of fidelity)?',
    'Compare incident post-mortems: are failures attributed to ''the simulation wasn''t realistic enough'' (supporting this reading''s narrower victim framing) or ''no simulation could have prepared them'' (supporting the sibling reading''s broader framing)?',
    'A narrower victim set understates this reading''s true extraction if lived-catastrophe-necessity is structurally correct; a properly bounded victim set is required for this reading''s ε to remain accurate under the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, conceptual, 'Whether confining victims to fidelity-gap cases (rather than all simulation-certified failures) is the correct boundary for this specific reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'exercise-based competence maintenance' per the kernel exercise_as_competence_maintenance. Each sibling reading (simulation_sufficiency, lived_catastrophe_necessity, hybrid_decay) is authored as its own ε-invariant constraint with its own beneficiary/victim structure, since the readings disagree on what counts as a genuine exercise of the kernel and therefore who is harmed by an inadequate one. This story (simulation_sufficiency_reading) computes as tangled_rope with moderate extraction; the lived_catastrophe_necessity_reading is expected to compute as more extractive or more clearly a snare/false-summit-flavored mountain-claim, since it denies simulation any genuine kernel-exercising status at all, which would make current certification regimes pure theater rather than partial coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
