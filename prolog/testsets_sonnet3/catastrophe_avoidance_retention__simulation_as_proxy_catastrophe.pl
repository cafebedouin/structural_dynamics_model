% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation-as-Proxy-Catastrophe Competence Retention Regime
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This story instantiates the simulation-as-proxy-catastrophe reading of
 *   the catastrophe_avoidance_retention kernel: the claim that high-fidelity
 *   simulation IS genuine practice, and that scheduled drills are
 *   functionally equivalent to real catastrophic events for the purpose of
 *   maintaining operator competence in safety-critical domains. Under this
 *   reading, simulation infrastructure becomes the load-bearing element of
 *   the entire competence-retention system, competence decay is treated as
 *   manageable through scheduled drill cadence, and regulatory certification
 *   built on logged simulator performance is treated as sufficient evidence
 *   of readiness. The coordination function is real — genuine catastrophic
 *   exposure cannot ethically or practically be the primary training vehicle
 *   — but the arrangement also asymmetrically benefits the vendor and
 *   certification apparatus while placing the cost of any equivalence gap on
 *   frontline operators and the public in the moment a real event occurs.
 *   Sibling readings of the same kernel (catastrophe_as_necessary_selector,
 *   hybrid_near_miss_learning) are NOT part of this constraint; they are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   here only via network edges and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - simulation_vendor_industry: organized beneficiary supplying and continuously upgrading the simulator infrastructure the doctrine depends on
 *   - regulatory_certification_bodies: institutional agenda_setter writing and enforcing simulator-hour requirements as the certification standard
 *   - senior_operations_management: institutional beneficiary using simulator compliance as due-diligence cover, mobile exit if wrong
 *   - frontline_operators: moderate-power payer who discovers any real fidelity gap live, with trapped exit
 *   - downstream_public_safety_dependents: powerless payer bearing ultimate cost of any equivalence failure, structurally absent from standard-setting
 *   - safety_researchers: analytical observer assessing transfer evidence without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.31).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Proxy-Catastrophe Competence Retention Regime").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '04a3e66c-3296-4b4f-8762-a08b62fcca74').
narrative_ontology:cs_kernel_codification('04a3e66c-3296-4b4f-8762-a08b62fcca74', formalized).
narrative_ontology:cs_authority_grounding('04a3e66c-3296-4b4f-8762-a08b62fcca74', extraction).
narrative_ontology:cs_interpretation_layer_present('04a3e66c-3296-4b4f-8762-a08b62fcca74').
narrative_ontology:cs_reading_relation('04a3e66c-3296-4b4f-8762-a08b62fcca74', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('04a3e66c-3296-4b4f-8762-a08b62fcca74', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('04a3e66c-3296-4b4f-8762-a08b62fcca74', foundational, simulation_functional_equivalence).
narrative_ontology:cs_axiom_status(simulation_functional_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('04a3e66c-3296-4b4f-8762-a08b62fcca74', simulation_functional_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('04a3e66c-3296-4b4f-8762-a08b62fcca74', secondary, scheduled_drill_cadence_sufficient_for_retention).
narrative_ontology:cs_axiom_status(scheduled_drill_cadence_sufficient_for_retention, holdable).
narrative_ontology:cs_axiom_grounding('04a3e66c-3296-4b4f-8762-a08b62fcca74', scheduled_drill_cadence_sufficient_for_retention, instrumental).
narrative_ontology:cs_reference_frame('04a3e66c-3296-4b4f-8762-a08b62fcca74', simulator_hours_as_certifiable_proxy).
narrative_ontology:cs_drift_state('04a3e66c-3296-4b4f-8762-a08b62fcca74', post_certification_industrialization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04a3e66c-3296-4b4f-8762-a08b62fcca74', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendor_industry).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, senior_operations_management).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, downstream_public_safety_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and continuously upgrades simulator hardware and scenario software to operators and regulators. Revenue depends directly on the doctrine that simulation is functionally equivalent to catastrophe exposure; every certification cycle that mandates more simulator hours is a sales event. Faces essentially no downside if the equivalence claim eventually proves wrong, since liability sits with operators and regulators, not vendors.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendor_industry, beneficiary,
    organized, generational, arbitrage, global).

% Writes and enforces the scheduled-drill requirements that substitute for continuous real-event exposure, and certifies operators as competent based on simulator performance. Benefits from a defensible, auditable paper trail (logged sim-hours, checklist completions) that is far cheaper to produce and defend than trying to measure competence against rare real catastrophes. Can revise the requirement but bears little direct cost if it is wrong.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, beneficiary).

% Schedules and budgets for simulator-based training in place of more disruptive real-incident debriefing or organizational restructuring after near-misses. Simulation compliance lets management demonstrate due diligence to boards and insurers without confronting harder questions about staffing, fatigue, or culture. Can relocate or be promoted away from consequences if a real event later reveals a competence gap.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, senior_operations_management, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, senior_operations_management, agenda_setter).

% Undergo scheduled simulator drills as the primary vehicle for maintaining competence in catastrophic scenarios they may never personally live through. Bear the actual moment of testing the equivalence claim: if the simulator failed to reproduce some structural feature of a real catastrophic event (time pressure under genuine mortality risk, chaotic multi-system failure, organizational panic), they discover this gap live, with real consequences, not the certifying body or the vendor. Cannot opt out of the drill regime and remain employed; cannot easily verify from their seat whether the simulator's fidelity claims are true.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, trapped, local).

% The public, passengers, patients, or residents whose safety depends on operator competence being genuinely maintained. They have no visibility into simulator fidelity, no vote on certification standards, and bear the full cost if the equivalence assumption is wrong and a real catastrophe reveals an untrained response. Cannot exit the risk exposure without leaving the region or service entirely.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, downstream_public_safety_dependents, payer,
    powerless, generational, trapped, regional).

% Study post-incident reports and simulator validation data to assess whether high-fidelity simulation actually transfers to real catastrophic performance. Have no enforcement power over the regime but can publish findings that support or undermine the equivalence doctrine.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce, dangerous, and statistically rare catastrophic-event exposure into a repeatable, schedulable training regime so that competence can be maintained and audited without waiting for or inducing real disasters.
% TRANSFER_FUNCTION: Moves training budget from operators/regulators to the simulation vendor industry; moves liability comfort and compliance defensibility to management and regulators; moves the actual risk of any residual competence gap onto frontline operators in the moment of a real event and onto the public who depend on that competence.
% ABSENT_VOICES: Downstream public safety dependents have no seat in setting simulator fidelity standards or drill frequency, despite bearing the ultimate cost of any gap between simulated and real catastrophic performance. Frontline operators who have identified specific fidelity gaps in simulators are frequently routed through internal channels captured by the same management that approved the simulator purchase.
% DISAPPEARANCE_RATIONALE: If the simulation-as-equivalent doctrine were abandoned overnight, certification regimes would have to be rebuilt around either real-incident exposure, apprenticeship-style tacit transfer, or some other retention mechanism; simulator vendors would lose their primary market; regulators would face the much harder problem of certifying competence without a clean auditable proxy; operators would face renewed uncertainty about what training actually protects them.
% FOUNDING_PROBLEM: Catastrophic events in safety-critical domains (aviation, nuclear, maritime, surgical) are too rare, too dangerous, and too ethically fraught to use as the primary vehicle for training and maintaining operator competence — some substitute for direct catastrophic exposure was needed.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and simulation vendors attest the founding problem is solved: fidelity has advanced enough that simulation is genuinely equivalent. Independent safety researchers analyzing post-incident data (e.g. cases where simulator-trained crews faced real multi-system cascading failures) report mixed and sometimes contradictory findings — some incidents show simulator training transferred well, others reveal specific classes of chaotic, high-stakes decision-making that simulators systematically fail to reproduce. No corroboration exists from downstream public safety dependents, who are structurally absent from the evaluation process entirely.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).
:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: there is a genuine coordination function (rare catastrophic exposure genuinely cannot be the primary training vehicle) but a persistent asymmetry in who bears the residual risk if the equivalence claim is imperfect. Suppression (0.31) is lower than extraction because operators are not coerced into believing the doctrine — many express informed skepticism about specific fidelity gaps — but their exit options from the drill regime itself are trapped by employment dependence. Theater ratio rises from 0.20 to 0.38 over the interval, reflecting a drift pattern common to certification regimes: logged simulator hours increasingly substitute for harder, more direct evidence of real competence (e.g., structured near-miss analysis, red-team stress scenarios), because logged hours are cheaper to produce and audit. Accessibility collapse (0.45) is moderate — alternative retention mechanisms (apprenticeship, real-incident rotation, hybrid near-miss learning) are not eliminated in principle, but the capital sunk into simulator infrastructure and the certification apparatus built around it makes switching costly, which is why this is authored as tangled_rope rather than a pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation vendor industry and certification bodies sit near the beneficiary end: they collect fees, program budget, and institutional legitimacy from the arrangement without bearing the tail risk of an equivalence failure. Senior operations management similarly benefits from an auditable compliance narrative while retaining mobility away from any eventual failure. Frontline operators and downstream public safety dependents sit near the target end: operators discover any real equivalence gap in the worst possible moment, and the public bears the ultimate cost with no seat in the standard-setting process at all — hence their d is pushed further toward full-target despite nominally being outside the direct contractual relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophic exposure is too rare/dangerous to train on directly) remains partially live — this constraint is not simple mandatrophy where the underlying problem has vanished. What is contested is whether the CURRENT instantiation (simulator-hours-as-sufficient-proxy) still tracks that founding problem or has drifted into a self-perpetuating certification-and-vendor economy whose evidentiary basis (fidelity validation against real events) is thin and asymmetrically controlled by the beneficiary seats. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: treating it as pure extraction would ignore the genuine and otherwise-unsolved coordination problem it addresses; treating it as pure rope would ignore the asymmetric risk transfer onto operators and the public and the active enforcement (certification mandates) required to hold the arrangement in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_equivalence_ambiguity,
    'Is high-fidelity simulation genuinely functionally equivalent to real catastrophic event exposure for competence retention, or does it systematically fail to reproduce specific structural features (chaotic multi-system cascade, mortality salience, organizational panic dynamics) that only real catastrophe provides?',
    'Longitudinal comparison of post-incident performance between operators whose only catastrophic-scenario exposure was simulator-based versus operators with documented real-incident or near-miss exposure, controlling for simulator fidelity generation and incident type.',
    'If simulation is genuinely equivalent, the coordination function is close to fully realized and the constraint drifts toward rope; if a persistent equivalence gap exists and is known to beneficiary seats but not corrected, the constraint drifts toward snare, since the certification apparatus would be knowingly certifying insufficient competence while collecting its institutional benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_equivalence_ambiguity, empirical, 'Whether the core equivalence claim this reading rests on is empirically true.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Among the three readings of the catastrophe_avoidance_retention kernel (simulation-as-proxy, catastrophe-as-necessary-selector, hybrid near-miss learning), which reading does the actual empirical record of high-reliability organizations best support, and is the selection itself made by parties with a stake in the outcome?',
    'Cross-domain meta-analysis of high-reliability organization outcomes (aviation, nuclear, maritime, surgical) categorized by which retention doctrine each organization actually followed, checked against independent (non-vendor, non-regulator) incident review boards.',
    'If the hybrid reading is empirically favored, this reading''s exclusive framing (simulation as fully sufficient) would be shown to understate the real contribution of near-miss and foreign-incident learning, which would reduce this constraint''s claimed coordination adequacy and push its classification further toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the reading selected for this constraint is the best-supported account of the kernel, and who benefits from that selection being uncontested.').

omega_variable(
    certification_capture_ambiguity,
    'Is the regulatory certification body''s endorsement of simulator-hour sufficiency an independent judgment, or has it been shaped by sustained engagement with the simulation vendor industry it certifies against?',
    'Disclosure audit of certification-body advisory panel composition, vendor funding of standard-setting research, and revolving-door employment between certifiers and vendors.',
    'Evidence of capture would strengthen the case that requires_active_enforcement + beneficiary/victim asymmetry constitutes genuine extraction riding on real coordination need, supporting continued tangled_rope classification over a reclassification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_capture_ambiguity, empirical, 'Whether the certifying authority is independent of the industry whose product it certifies as sufficient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t4, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 4, 0.24).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 8, 0.28).
narrative_ontology:measurement(cata_tr_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 12, 0.31).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 16, 0.34).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t4, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(cata_be_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cata_su_t4, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(cata_su_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 12, 0.27).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_avoidance_retention kernel, each authored as a separate ε-invariant story per the ε-invariance principle. simulation_as_proxy_catastrophe (this file) treats simulator infrastructure as fully sufficient and authors a moderate ε (0.42) reflecting a genuine but risk-asymmetric coordination function. catastrophe_as_necessary_selector is expected to author a structurally different ε and stakeholder set, since it denies simulation's sufficiency entirely and treats organizational trauma as the load-bearing retention mechanism. hybrid_near_miss_learning is expected to sit between the two, distributing the coordination function across multiple evidence sources rather than concentrating it in simulator infrastructure or real catastrophe alone. Do not average across these three files to get 'the' ε for the kernel — each is a distinct constraint with its own beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
