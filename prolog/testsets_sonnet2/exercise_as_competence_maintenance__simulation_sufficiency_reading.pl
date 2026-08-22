% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   This constraint instantiates the simulation-sufficiency reading of the
 *   exercise-as-competence-maintenance kernel: it holds that a well-designed
 *   simulated catastrophe genuinely exercises the underlying competence, and
 *   that the retention benefit scales with the fidelity of the simulation
 *   rather than requiring any exposure to a real catastrophic event. Under
 *   this reading, regulatory drill mandates are treated as sufficient
 *   evidence of maintained capability, competence is measured by simulator
 *   performance metrics (completion rates, scored scenario outcomes,
 *   certification renewal), and the victim set is narrowly bounded to those
 *   harmed specifically by inadequate simulation fidelity — not by the
 *   absence of live-catastrophe exposure per se. This is a different
 *   constraint from the lived_catastrophe_necessity_reading (which denies
 *   simulation can ever substitute for real-stakes activation) and from the
 *   hybrid_decay_reading (which splits the kernel into a proceduralizable
 *   component and a judgment-under-stakes component with different exercise
 *   requirements). Each reading has its own ε, its own beneficiary/victim
 *   structure, and its own classification; they are linked as siblings in the
 *   same kernel contest, not merged into one story.
 *
 * KEY AGENTS:
 *   - drill_program_administrators: primary agenda-setter (institutional/arbitrage) — designs and certifies the drills that constitute the sufficiency claim
 *   - simulator_vendors: beneficiary (organized/mobile) — market depends on simulated exercise being accepted as the standard
 *   - regulatory_compliance_officers: beneficiary/co-agenda-setter (institutional/constrained) — certification workload is tractable only if the proxy holds
 *   - frontline_responders_relying_on_low_fidelity_drills: primary payer (moderate/constrained) — carries the fidelity gap into real incidents
 *   - public_exposed_to_undertested_failure_modes: primary payer (powerless/trapped) — bears consequences with no voice in fidelity standards
 *   - incident_investigation_boards: analytical observer (institutional/analytical) — produces the outside-the-benefiting-parties corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.5).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '029d9ec4-bc65-4e48-b7c6-f793b146c210').
narrative_ontology:cs_kernel_codification('029d9ec4-bc65-4e48-b7c6-f793b146c210', formalized).
narrative_ontology:cs_authority_grounding('029d9ec4-bc65-4e48-b7c6-f793b146c210', extraction).
narrative_ontology:cs_interpretation_layer_present('029d9ec4-bc65-4e48-b7c6-f793b146c210').
narrative_ontology:cs_reading_relation('029d9ec4-bc65-4e48-b7c6-f793b146c210', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('029d9ec4-bc65-4e48-b7c6-f793b146c210', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('029d9ec4-bc65-4e48-b7c6-f793b146c210', foundational, simulation_fidelity_is_the_sole_retention_lever).
narrative_ontology:cs_axiom_status(simulation_fidelity_is_the_sole_retention_lever, holdable).
narrative_ontology:cs_axiom_grounding('029d9ec4-bc65-4e48-b7c6-f793b146c210', simulation_fidelity_is_the_sole_retention_lever, empirically_contingent).
narrative_ontology:cs_axiom('029d9ec4-bc65-4e48-b7c6-f793b146c210', secondary, regulatory_drill_completion_constitutes_competence_evidence).
narrative_ontology:cs_axiom_status(regulatory_drill_completion_constitutes_competence_evidence, holdable).
narrative_ontology:cs_axiom_grounding('029d9ec4-bc65-4e48-b7c6-f793b146c210', regulatory_drill_completion_constitutes_competence_evidence, conventional).
narrative_ontology:cs_reference_frame('029d9ec4-bc65-4e48-b7c6-f793b146c210', post_ntsb_style_drill_mandate_regime).
narrative_ontology:cs_drift_state('029d9ec4-bc65-4e48-b7c6-f793b146c210', contemporary_high_fidelity_simulator_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('029d9ec4-bc65-4e48-b7c6-f793b146c210', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_officers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_responders_relying_on_low_fidelity_drills).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, public_exposed_to_undertested_failure_modes).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_drill_mandate_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and certify the simulation exercises that regulators accept as evidence of maintained competence. They set fidelity standards, choose scenarios, and sign off on completion. Their institutional standing and budget depend on the drill program being accepted as sufficient; raising fidelity requirements would raise their own costs and expose gaps in past certifications.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell simulation platforms and drill-management software whose market depends on simulated exercise being accepted as the standard of competence maintenance. Revenue scales with drill frequency and certification volume, not with fidelity improvements that are costly to build and hard to sell as mandatory.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors, beneficiary,
    organized, biographical, mobile, national).

% Certify organizations as competent based on completed drill logs and simulator performance metrics. Their workload is tractable only because simulated performance is treated as a valid proxy for real capability; auditing live incident performance directly would multiply their burden and reopen questions about past sign-offs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_officers, agenda_setter).

% Complete the mandated drills, are certified competent, and carry that certification into real incidents where the drill scenarios did not resemble actual failure conditions. They bear the gap between simulated and real performance personally, in the field, often without knowing the gap exists until an incident exposes it. Leaving the profession or refusing certification is not a realistic option.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_responders_relying_on_low_fidelity_drills, payer,
    moderate, biographical, constrained, regional).

% Live or work in the areas the certified responders are meant to protect. They have no visibility into simulation fidelity and no say in drill design; when a real catastrophe diverges from the drilled scenario, they absorb the consequences of the gap without ever having been party to the sufficiency claim.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, public_exposed_to_undertested_failure_modes, payer,
    powerless, immediate, trapped, regional).

% Conduct post-incident reviews comparing what was drilled against what actually occurred. Their findings are the primary independent evidence of whether simulation fidelity tracked real failure modes, but their reports arrive after harm has already been distributed.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, incident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_program_administrators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, auditable, resource-bounded mechanism for maintaining and demonstrating competence across large numbers of personnel who cannot realistically be exposed to actual catastrophic events for training purposes.
% TRANSFER_FUNCTION: Moves the cost of building high-fidelity simulation onto drill-program administrators and vendors only to the extent regulators demand it, while moving the cost of any fidelity shortfall onto responders and the public who encounter the gap during real incidents.
% ABSENT_VOICES: The public who will encounter responders in a real incident has no seat in drill design or fidelity-standard setting. Frontline responders who suspect specific drill scenarios are unrealistic have informal channels but no binding say over certification criteria.
% DISAPPEARANCE_RATIONALE: Administrators and regulators would say the certification infrastructure would need urgent replacement (world_rearranges from their seat) since organizational competence-tracking depends on it; responders and incident boards would say much of daily practice would be unchanged in the short term but real performance during incidents would be no worse than it already is, since the gap between drilled and lived performance already exists uncorrected.
% FOUNDING_PROBLEM: Organizations needed a way to maintain and verify competence for low-frequency, high-consequence events without waiting for or inducing actual catastrophes, and without the impossibly high cost of continuous live-fire training.
% FOUNDING_PROBLEM_CORROBORATION: Drill administrators and compliance officers attest the founding problem remains fully solved by current simulation standards. Incident investigation boards, in post-incident reports produced independently of the certification chain, have repeatedly found that drilled scenarios diverged materially from actual failure conditions — corroboration from outside the benefiting parties that the sufficiency claim is, at minimum, incompletely resolved.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42 at interval end) rather than severe: this reading genuinely coordinates a real problem (competence maintenance for low-frequency high-consequence events cannot rely on live catastrophe) and the fidelity-dependent framing gives administrators an honest lever to improve outcomes over time. But extraction is non-trivial and rising because the sufficiency claim insulates the certification chain from having to prove fidelity actually tracks real failure modes — cheaper, lower-fidelity drills satisfy the same regulatory checkbox as expensive high-fidelity ones, and there is a structural incentive to under-invest in fidelity while still collecting the certification benefit. Theater ratio rises alongside extraction (0.22 to 0.38) as drill completion volume becomes the visible metric while fidelity investment lags. Suppression is moderate (0.5): the constraint's persistence depends on regulatory mandate rather than physical coercion, but exit for responders and the public is genuinely constrained, not free.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator and compliance-officer seats, the arrangement is a functioning, auditable competence-maintenance system doing exactly what it was designed to do. From the responder and public seats, the same arrangement is a certification chain that produces confidence without necessarily producing the underlying capability, discovered only when a real incident diverges from the drilled scenario. The engine should compute these as different per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Drill administrators, simulator vendors, and compliance officers are structural beneficiaries: they collect certification standing, revenue, or tractable workload from the sufficiency claim being accepted, and their exit is mobile-to-arbitrage because none of them personally faces the real incident. Frontline responders and the public are structural targets: they carry the fidelity gap in the field, with constrained or trapped exit respectively. This maps cleanly onto the beneficiary/victim declarations — the derivation chain should place administrators and vendors near the beneficiary end and responders/public near the target end without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifying competence without live catastrophe exposure) remains genuinely live — organizations still cannot ethically or practically train primarily on real disasters — so this is not a pure mandatrophy case where the mandate has fully outlived its function. What has drifted is the sufficiency claim itself: whether ANY simulation satisfies the mandate regardless of fidelity, versus whether fidelity is being honestly pursued. The tangled_rope classification captures this: a genuine coordination function (simulated training must exist) coexists with asymmetric extraction (the sufficiency framing lets the cheaper, lower-fidelity option satisfy the same certification regardless of whether it tracks real failure modes), and this requires active regulatory enforcement to sustain (drill mandates, certification renewal cycles) rather than persisting because participants find it obviously beneficial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_threshold_ambiguity,
    'Is there a fidelity threshold above which simulated exercise genuinely equals real-catastrophe exercise for competence retention, or does the sufficiency claim break down asymptotically regardless of investment?',
    'Longitudinal comparison of certified-responder performance in actual incidents against the fidelity tier of their most recent drills, controlling for incident type; if high-fidelity-drilled responders perform statistically indistinguishably from those with live-incident experience, sufficiency is empirically supported at that tier.',
    'If a genuine sufficiency threshold exists and is being met, this reading''s extraction is largely a coordination cost, not exploitation — closer to rope than tangled_rope. If no threshold reliably closes the gap, the sufficiency claim itself becomes the extraction mechanism regardless of investment, pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_sufficiency_threshold_ambiguity, empirical, 'Whether fidelity investment can close the gap between simulated and lived competence, or whether the sufficiency claim is structurally unfalsifiable.').

omega_variable(
    kernel_reading_committer_structure,
    'Which of the three readings of the exercise_as_competence_maintenance kernel (simulation_sufficiency, lived_catastrophe_necessity, hybrid_decay) is the operative one inside any given regulatory regime, and is that choice itself contested within the regime or settled by fiat?',
    'Survey of regulatory text and incident-board findings across jurisdictions: does the regulator explicitly adjudicate fidelity-versus-liveness, or does it silently assume sufficiency by accepting drill completion as satisfying the mandate?',
    'If a regime silently assumes simulation_sufficiency without ever testing it against lived_catastrophe_necessity or hybrid_decay, the certification chain is resting on an unexamined committer choice — this would elevate the tangled_rope''s extraction component, since the sufficiency claim is functioning as an unstated axiom rather than a defended position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the choice among sibling kernel readings is actively adjudicated or silently defaulted to sufficiency by regulatory convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the exercise_as_competence_maintenance kernel, decomposed per the ε-invariance principle because the three readings assign structurally different ε values and victim sets to the same natural-language claim ('does simulation count as exercising the competence kernel?'). simulation_sufficiency_reading (this story, tangled_rope, ε≈0.42) treats drill fidelity as the sole lever and drill mandate completion as sufficient evidence. lived_catastrophe_necessity_reading would carry a distinct, likely higher ε and a broader victim set (everyone exposed to a never-live-tested system, not just those harmed by fidelity shortfalls specifically). hybrid_decay_reading splits the kernel into two exercise requirements with a mixed classification. All three should link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
