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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe-Necessity Constraint on Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The catastrophe-necessity reading holds that genuine competence for
 *   safety-critical decision-making requires the irreducible stress,
 *   uncertainty, and embodied learning that only actual catastrophic events
 *   provide. Simulation, no matter how high-fidelity, cannot substitute
 *   because it lacks the visceral consequences, the social pressure, the
 *   adrenaline response, and the tacit knowledge that only real catastrophe
 *   furnishes. This reading is claimed as a Mountain constraint (a hard limit
 *   of human cognition and psychology), but it is one reading of a contested
 *   kernel about whether catastrophe is truly necessary or merely
 *   historically convenient. The constraint extracts heavily from operators
 *   and safety organizations by legitimizing continuous, costly
 *   recertification and organizational learning programs and by excluding
 *   alternative competence-maintenance paradigms from serious consideration.
 *
 * KEY AGENTS:
 *   - safety_critical_operators: identity-locked professionals (pilots, surgeons, nuclear operators, disaster responders) who experience competence degradation anxiety in catastrophe-free periods and must invest continuously in training
 *   - simulation_training_providers: organized beneficiaries who maintain market demand and legitimacy through the constraint's truthfulness
 *   - regulatory_bodies: institutional agenda-setters who mandate recertification and justify it by catastrophe-necessity claims
 *   - organizations_managing_safety_systems: powerful payers forced to invest in continuous competency maintenance and organizational learning
 *   - public_safety_stakeholders: powerless excluded agents who bear the cost if competence truly degrades but have no voice in constraint adjudication
 *   - competing_competence_maintenance_paradigms: structurally excluded alternative framings (skills-based refresher, peer learning, culture-based approaches)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.89).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe-Necessity Constraint on Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '704ce2a6-e301-4350-9775-2b65f4755d59').
narrative_ontology:cs_kernel_codification('704ce2a6-e301-4350-9775-2b65f4755d59', distributed).
narrative_ontology:cs_authority_grounding('704ce2a6-e301-4350-9775-2b65f4755d59', extraction).
narrative_ontology:cs_interpretation_layer_present('704ce2a6-e301-4350-9775-2b65f4755d59').
narrative_ontology:cs_reading_relation('704ce2a6-e301-4350-9775-2b65f4755d59', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('704ce2a6-e301-4350-9775-2b65f4755d59', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('704ce2a6-e301-4350-9775-2b65f4755d59', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('704ce2a6-e301-4350-9775-2b65f4755d59', foundational, catastrophe_irreducible_for_competence).
narrative_ontology:cs_axiom_status(catastrophe_irreducible_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('704ce2a6-e301-4350-9775-2b65f4755d59', catastrophe_irreducible_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('704ce2a6-e301-4350-9775-2b65f4755d59', foundational, simulation_cannot_substitute_for_real_stakes).
narrative_ontology:cs_axiom_status(simulation_cannot_substitute_for_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('704ce2a6-e301-4350-9775-2b65f4755d59', simulation_cannot_substitute_for_real_stakes, deontological).
narrative_ontology:cs_reference_frame('704ce2a6-e301-4350-9775-2b65f4755d59', catastrophe_as_irreducible_learning).
narrative_ontology:cs_drift_state('704ce2a6-e301-4350-9775-2b65f4755d59', contemporary_simulation_fidelity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('704ce2a6-e301-4350-9775-2b65f4755d59', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_critical_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizational_learning_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_providers).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_critical_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizations_managing_safety_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, nuclear plant operators, surgeons, disaster responders: professionals whose competence must be maintained across decades of low-frequency high-consequence decisions. They invest in simulation training but experience catastrophe-free periods where their stress-response capacity degrades. They cannot exit the identity of being a safety-critical operator without career termination, and the constraint forces them to accept that simulation alone cannot fully maintain their competence for actual catastrophic events.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_critical_operators, payer,
    moderate, biographical, identity_locked, global).

% Simulate-based training programs, high-fidelity equipment manufacturers, and simulation curriculum designers benefit from the constraint insofar as it maintains demand for their services and legitimizes continuous recertification requirements. If simulation were truly sufficient, their services could be compressed and deferred; the constraint's truthfulness (catastrophe is irreducible) guarantees their market persists.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_providers, beneficiary,
    organized, generational, arbitrage, global).

% Civil aviation authorities, nuclear regulators, medical boards, and disaster-response hierarchies mandate training and recertification. They cite the catastrophe-necessity constraint to justify continuous simulation and competency re-examination. The constraint's truthfulness underwrites regulatory authority.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Airlines, nuclear utilities, hospitals, and public-safety agencies must invest in both simulation infrastructure and organizational learning systems. The constraint forces them to treat catastrophe-free periods not as evidence that systems are working but as periods of degradation requiring costly intervention to maintain readiness.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizations_managing_safety_systems, payer,
    powerful, generational, constrained, global).

% Researchers and theorists who study high-reliability organizations and organizational learning benefit from the constraint's framing: it makes their field urgent and necessary (studying how to maintain competence without catastrophes), it generates funding and career opportunities, and it shifts attention away from simpler explanations (competence is just a skill; it does not degrade catastrophically in absence of real events).
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, organizational_learning_theorists, beneficiary,
    moderate, generational, mobile, global).

% The public, patients, passengers, and workers who rely on these safety-critical systems have no voice in calibrating the constraint. They bear the cost if competence truly degrades in catastrophe-free periods but have no seat at the table where the claim is adjudicated or refuted.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, public_safety_stakeholders, excluded,
    powerless, immediate, trapped, global).

% Alternative views on competence maintenance (skills-based refresher training, distributed peer learning, organizational culture approaches, or simulation-fidelity calibration) are structurally excluded from legitimacy by the catastrophe-necessity reading. These alternatives would offer cheaper or more sustainable paths but cannot be seriously considered if the constraint's core claim is accepted.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, competing_competence_maintenance_paradigms, excluded,
    moderate, generational, constrained, global).

% High-reliability organizations in domains like air traffic control, nuclear power, and aerospace that have gone decades without catastrophes despite catastrophe-necessity claims can test whether competence actually degrades. Their empirical record is the primary corroboration or refutation vector for the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_regimes_practicing_catastrophe_deprivation, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_providers).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function: this is a claim about an irreducible psychological/physiological limit of human competence under uncertainty, not a solved collective-action problem.
% TRANSFER_FUNCTION: Moves resources (training budgets, time, career continuity) from organizations and operators to regulatory bodies, simulation providers, and organizational learning institutions, justified by the necessity claim. The transfer is defended not as extraction but as the cost of maintaining competence.
% ABSENT_VOICES: Competing competence-maintenance theorists; simulation-fidelity engineers who could argue for threshold-based sufficiency; cost-benefit analysts; organizations from domains that have achieved excellent safety records without catastrophes; operators in sectors where catastrophe-free decades correspond to lowest-accident periods.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared and organizations adopted cheaper, simulation-only training regimes, one of two world-rearrangements would occur: (1) competence degrades, catastrophes increase, and the constraint's truthfulness is vindicated retroactively, or (2) competence remains stable, catastrophes do not increase, and the constraint was false. The contest is not over whether the constraint's content is true, but over which world we actually live in.
% FOUNDING_PROBLEM: Catastrophic failures in safety-critical operations (aviation accidents, nuclear incidents, medical errors, disaster-response failures) cluster in low-frequency high-consequence domains where operators cannot accumulate real-world learning from catastrophes directly. Early-career operators must somehow be competent for events they have never experienced and cannot expect to experience many times in a career.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards, high-reliability organization researchers (Weick, Rochlin), and operational safety communities attest the problem is live. The empirical record is contested: some argue decades-long safety records in well-trained organizations prove competence does NOT degrade catastrophically without real events (no corroboration); others argue the safety records exist BECAUSE organizations impose the catastrophe-necessity constraint and maintain costly competency programs (corroboration). Outside the benefiting parties: independent researchers in human factors, organizational psychology, and comparative safety records provide mixed evidence — no consensus corroboration.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is extremely high (0.89 at interval end) because the constraint, if accepted as true, justifies perpetual resource transfer with no natural termination point: competence never 'matures' or 'completes'; it is always degrading in catastrophe-free periods and always needing restoration through costly programs. Suppression is substantial (0.72) because accepting the constraint's core claim requires operators to suppress doubts about whether simulation-only competency maintenance (which succeeds in many domains like air traffic control) is actually inadequate. Theater is low (0.18) because the constraint is not performatively maintained — it is actively believed and continuously acted upon; the low theater reflects genuine commitment. Accessibility collapse is very high (0.91): once an operator or organization accepts the catastrophe-necessity claim, the alternative framings (simulation sufficiency, fidelity-threshold models, skills-based competency) collapse as unserious — they become literally unthinkable within the constraint's epistemic frame. Resistance is low (0.34) because operators are identity-locked and regulatory bodies have institutional authority; meaningful resistance comes only from competing research paradigms and domains with perfect safety records, whose voices are structurally excluded.
 *
 * PERSPECTIVAL GAP:
 *   From the operator's seat, the constraint is experienced as a hard fact about human limits — anxiety in catastrophe-free periods feels like real incompetence degradation, and the constraint names and legitimizes that experience. From the simulation provider's seat, the constraint is the foundation of their market and institutional relevance; they have professional incentive to treat it as self-evident. From the regulatory body's seat, the constraint justifies authority and mandates. From the organizational cost-bearer's seat, the constraint is an irreducible expense mandate. From the public's seat (excluded), the constraint is invisible — they experience only the outcome (whether safety actually degrades or remains stable). The engine computes each seat's type from the structural data; this narrative divergence is the exact structural situation the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators are targets (d near 1.0): they are identity-locked (cannot exit the professional identity without career termination), face constrained exit options (mandatory recertification is enforced), and bear the psychological cost of continuous competency anxiety. Simulation providers are beneficiaries (d near 0.0): they collect market rents and institutional legitimacy from the constraint's truthfulness; they have arbitrage-grade exit (they could pivot to other training domains if this constraint collapsed). Regulatory bodies are partly beneficiary (d ~0.3): they collect authority and mandate justification but also bear modest costs of oversight and remediation. Safety organizations are targets (d near 0.8): they pay but cannot exit without abandoning safety-critical operations entirely. The constraint's beneficiaries (simulation providers, organizational learning theorists) are smaller and more concentrated than the payers (dispersed among millions of operators and organizations); this concentration-dispersion asymmetry is part of why the constraint persists.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT resolved here. The founding problem (low-frequency high-consequence decision-making without real catastrophe experience) is live and contested. The constraint's mandate is not obsolete — if anything, it is strengthening as simulation technology advances (raising the fidelity bar) and as organizations accumulate decades-long safety records (which can be interpreted as either validation of the constraint or refutation of it). The engine will compute whether mandatrophy has occurred by examining whether the measured extraction remains tied to solving the founding problem or has drifted into pure rent collection. The catastrophe-necessity reading itself does not resolve that; it depends on whether simulation technology crosses the fidelity threshold (the sibling constraint's empirical anchor) or whether operators' competence actually degrades measurably in catastrophe-free regimes (the hybrid-degradation reading's empirical anchor).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency_boundary,
    'Is there a fidelity threshold above which simulation-induced stress and uncertainty become psychologically/physiologically equivalent to real catastrophe, making simulation sufficient for competence maintenance?',
    'Longitudinal study comparing operators trained on progressively higher-fidelity simulators with those trained on real events, measuring stress response, decision quality, and outcomes across decades without catastrophe exposure. If high-fidelity simulation produces identical stress signatures and competence trajectories, the boundary exists and the constraint is false.',
    'If a fidelity threshold exists, the constraint''s categorical claim (actual catastrophe is irreducible) is false, and the sibling simulation_fidelity_threshold reading becomes the accurate framing. Extractiveness would drop sharply as the constraint''s justification collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency_boundary, empirical, 'Whether simulation fidelity can reach catastrophe-equivalence.').

omega_variable(
    catastrophe_deprivation_record_interpretation,
    'Do high-reliability organizations that have avoided catastrophes for decades despite catastrophe-necessity claims actually demonstrate that the constraint is false, or do they demonstrate that the constraint is true AND their continuous adherence to catastrophe-necessity programs (simulation, recertification, organizational learning) has prevented the competence degradation the constraint predicts?',
    'Comparative analysis of organizations that abandon catastrophe-necessity programs (reducing simulation/recertification) versus those that maintain them, measured over 20-30 year periods in catastrophe-free regimes. If safety and competence remain stable in both cohorts, the constraint is false; if degradation appears only in abandoning cohorts, the constraint is true.',
    'This is the central empirical crux: the constraint cannot be falsified by the mere fact of catastrophe-free decades because those decades are exactly what the constraint would predict IF the constraint-required programs are followed. Resolution determines whether the constraint is a testable empirical claim or a self-fulfilling justification for continuous programs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_deprivation_record_interpretation, empirical, 'Whether catastrophe-free safety records validate or falsify the catastrophe-necessity claim.').

omega_variable(
    identity_lock_mechanism_in_operators,
    'To what extent is operators'' experienced competence degradation in catastrophe-free periods a real psychological/physiological effect versus an identity-locked professional narrative internalized through training and regulatory culture?',
    'Ethnographic study of operators'' subjective experience, stress physiology, and decision-quality metrics in catastrophe-free periods, compared with cohorts from alternative competence-maintenance cultures (peer-learning only, skills-based refresher, culture-based approaches). If degradation anxiety persists across cultures but decision quality does not degrade, the effect is partially internalized.',
    'If the degradation effect is substantially internalized identity-fusion rather than real cognitive decline, the constraint''s extraction mechanism is amplified (it operates through suppression of alternative thought-models, not just resource transfer). The engine would flag higher suppression and higher identity_lock values for operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_operators, empirical, 'Whether competence degradation in catastrophe-free periods is embodied or socially constructed.').

omega_variable(
    beneficiary_alignment_false_summit_candidate,
    'Is the catastrophe-necessity reading genuinely a Mountain constraint (an irreducible limit of human competence) or a constructed constraint that benefits simulation providers, regulators, and organizational learning theorists who have vested interest in its truthfulness?',
    'Examine whether the constraint''s beneficiaries (simulation providers, regulators, learning theorists) would financially or institutionally benefit from the constraint being false (they would not); compare with beneficiaries of obviously constructed constraints (those benefit only if the constraint persists). Test whether the constraint would be maintained in a counterfactual where beneficiaries had no financial interest.',
    'This is the FSM (false-summit-mountain) omega: the constraint is authored as mountain with declared beneficiaries, which triggers FSM evaluation in the engine. If the constraint computes as tangled_rope or snare instead of mountain despite the emerges_naturally claim, it is a false natural law — a constructed constraint masquerading as a hard limit. This omega documents the ambiguity for the engine to resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_alignment_false_summit_candidate, conceptual, 'Whether the catastrophe-necessity constraint is a genuine physical/psychological limit or a constructed constraint defended as natural law.').

omega_variable(
    tacit_knowledge_degradation_measurement_problem,
    'How would we measure whether tacit knowledge and stress-response capacity actually degrade in catastrophe-free periods, given that these phenomena resist explicit measurement and simulation may partially mask degradation?',
    'Multi-method empirical program: (1) stress-response physiology (heart-rate variability, cortisol, decision latency under time pressure), (2) near-miss analysis and error classification (do near-miss errors increase in degradation-free years?), (3) cross-cohort longitudinal comparison, (4) incident post-mortems after catastrophes that occur to ''competency-maintained'' operators to detect whether degradation was present.',
    'The constraint''s truthfulness is empirically indeterminate until we solve the measurement problem. Until then, the constraint operates partly as unfalsifiable conviction (operators feel competence degradation, regulators assume it, and the feeling/assumption justify programs). If measurement remains intractable, the constraint persists by epistemic closure, not by evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tacit_knowledge_degradation_measurement_problem, empirical, 'Whether tacit knowledge degradation is measurable or resistant to objective verification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(cata_tr_t15, projected).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(cata_tr_t20, projected).
narrative_ontology:measurement(cata_tr_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(cata_tr_t25, projected).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(cata_tr_t30, projected).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(cata_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 5, 0.84).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 10, 0.86).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 15, 0.87).
narrative_ontology:measurement_basis(cata_be_t15, projected).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(cata_be_t20, projected).
narrative_ontology:measurement(cata_be_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(cata_be_t25, projected).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(cata_be_t30, projected).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(cata_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(cata_su_t15, projected).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(cata_su_t20, projected).
narrative_ontology:measurement(cata_su_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(cata_su_t25, projected).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(cata_su_t30, projected).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(cata_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel catastrophe_proxy_sufficiency. The kernel is the claim about whether actual catastrophic events are necessary to maintain competence in safety-critical domains. Four distinct readings instantiate four different constraints with different ε values, beneficiary/victim structures, and types. This reading (catastrophe_necessity_reading) claims that actual catastrophe is categorically irreducible — a mountain constraint. The sibling readings (simulation_as_proxy, hybrid_degradation, fidelity_threshold) offer alternative framings with different structural properties. Each reading is a separate constraint file linked via this network block. The reading-contest is NOT resolved within any one constraint's metrics; it is the corpus-level question that compares the four readings' empirical adequacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
