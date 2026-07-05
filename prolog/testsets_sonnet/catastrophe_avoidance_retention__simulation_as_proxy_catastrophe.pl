% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: High-Fidelity Simulation as Sufficient Proxy for Catastrophic-Event Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   In high-reliability industries (nuclear operations, commercial aviation,
 *   offshore drilling, hospital critical care), the doctrine that
 *   high-fidelity simulation constitutes genuine practice underwrites the
 *   entire competence-certification apparatus. This story instantiates the
 *   SIMULATION_AS_PROXY_CATASTROPHE reading of the
 *   catastrophe_avoidance_retention kernel: the claim that scheduled,
 *   repeatable simulator drills are functionally equivalent to real
 *   catastrophic events for maintaining operator competence, such that
 *   simulation infrastructure and drill scheduling become the sufficient and
 *   correct locus of regulatory attention. Sibling readings —
 *   catastrophe_as_necessary_selector (only real catastrophes provide
 *   adequate selection pressure) and hybrid_near_miss_learning (a blended
 *   model incorporating near-misses and foreign incident learning) — are NOT
 *   part of this story; they are separate constraints with their own ε and
 *   stakeholder structures, linked only by kernel identity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.38).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "High-Fidelity Simulation as Sufficient Proxy for Catastrophic-Event Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'ef725a69-8ace-45dc-933d-d1120206c94b').
narrative_ontology:cs_kernel_codification('ef725a69-8ace-45dc-933d-d1120206c94b', formalized).
narrative_ontology:cs_authority_grounding('ef725a69-8ace-45dc-933d-d1120206c94b', expertise).
narrative_ontology:cs_interpretation_layer_present('ef725a69-8ace-45dc-933d-d1120206c94b').
narrative_ontology:cs_reading_relation('ef725a69-8ace-45dc-933d-d1120206c94b', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('ef725a69-8ace-45dc-933d-d1120206c94b', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('ef725a69-8ace-45dc-933d-d1120206c94b', foundational, high_fidelity_simulation_is_functionally_equivalent_practice).
narrative_ontology:cs_axiom_status(high_fidelity_simulation_is_functionally_equivalent_practice, holdable).
narrative_ontology:cs_axiom_grounding('ef725a69-8ace-45dc-933d-d1120206c94b', high_fidelity_simulation_is_functionally_equivalent_practice, empirically_contingent).
narrative_ontology:cs_axiom('ef725a69-8ace-45dc-933d-d1120206c94b', secondary, scheduled_drills_are_sufficient_absent_genuine_catastrophic_exposure).
narrative_ontology:cs_axiom_status(scheduled_drills_are_sufficient_absent_genuine_catastrophic_exposure, holdable).
narrative_ontology:cs_axiom_grounding('ef725a69-8ace-45dc-933d-d1120206c94b', scheduled_drills_are_sufficient_absent_genuine_catastrophic_exposure, instrumental).
narrative_ontology:cs_reference_frame('ef725a69-8ace-45dc-933d-d1120206c94b', post_ntsb_faa_simulator_certification_regime).
narrative_ontology:cs_drift_state('ef725a69-8ace-45dc-933d-d1120206c94b', contemporary_post_multiple_certified_crew_underperformance_incidents, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef725a69-8ace-45dc-933d-d1120206c94b', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulator_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_companies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, training_certification_bodies).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, downstream_public_at_risk).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_fidelity_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the training regime, purchases simulator contracts, and certifies crews as competent based on scheduled simulator sessions rather than exposure to genuine catastrophic dynamics. Benefits by avoiding the cost, liability, and disruption of maintaining readiness through any other mechanism, and by being able to point to a completed drill log as proof of diligence in the event of an incident.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_companies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_companies, beneficiary).

% Sell and service the simulation infrastructure the entire competence-retention regime depends on. Revenue scales with the assumption that fidelity is equivalence; if regulators or operators concluded simulation was insufficient, procurement budgets would shift toward incident-response capability building instead.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulator_vendors, beneficiary,
    organized, biographical, mobile, global).

% Write the certification standards that define compliant drill frequency and simulator fidelity, and audit against those standards. Benefit from a legible, auditable proxy (hours logged, scenarios completed) because it lets enforcement be administratively tractable without requiring judgment calls about tacit competence that simulators cannot certify.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_agencies, beneficiary).

% Issue competence certifications keyed to simulator hours and drill completion. Their institutional relevance depends on the proxy being treated as sufficient; if raw catastrophic exposure or hybrid learning models displaced simulator-based certification, their gatekeeping function would shrink.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, training_certification_bodies, beneficiary,
    organized, biographical, mobile, national).

% Perform scheduled drills that satisfy certification but may not replicate the physiological stress, ambiguity, and consequence-weight of an actual catastrophic event. They bear the risk if the equivalence claim is false: the gap between drilled response and real crisis response surfaces only during an actual event, when the operator is the one whose judgment is tested and whose license or livelihood is on the line if it fails.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Live or work near the facilities, aircraft routes, or systems whose safety depends on operator competence during a real catastrophic event. Have no visibility into whether simulator-certified crews will perform as claimed and no mechanism to independently verify the equivalence assumption before an incident occurs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, downstream_public_at_risk, payer,
    powerless, generational, trapped, regional).

% Study the documented gaps between simulated and real acute-stress performance (attentional narrowing, memory consolidation differences, absence of genuine mortality salience) but their findings enter certification standards slowly, if at all, because revising the equivalence doctrine would require costly redesign of the entire training-and-certification infrastructure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, human_factors_researchers, excluded,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulator_vendors).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce, dangerous, and expensive competence maintenance across large operator workforces by substituting repeatable, safe, schedulable simulator sessions for training on actual catastrophic events, which cannot ethically or practically be manufactured on demand.
% TRANSFER_FUNCTION: Moves the cost of competence verification from continuous exposure-based learning (expensive, dangerous, unschedulable) to periodic simulator sessions (cheap, safe, auditable) — the savings accrue to operating companies and simplify enforcement for regulators, while the residual uncertainty about real-event performance is transferred onto frontline operators and the public who depend on that performance during an actual crisis.
% ABSENT_VOICES: Human factors researchers documenting the simulation-fidelity gap are structurally outside the standard-setting process; workers who have lived through both drills and a genuine catastrophic event and can speak to the experiential gap are rarely systematically debriefed into certification design; the public bearing tail risk has no seat in the certification conversation at all.
% DISAPPEARANCE_RATIONALE: If the equivalence doctrine were formally abandoned, operating companies and regulators would have to invent a new, costlier basis for certifying and insuring competence — some argue this would force genuinely superior hybrid training regimes (world_rearranges); others argue simulator infrastructure is now so embedded that certification would simply be re-labeled without changing underlying practice (world_unchanged). The parties dispute which.
% FOUNDING_PROBLEM: Real catastrophic events (reactor meltdowns, mid-air emergencies, structural collapses) are too rare, too dangerous, and too destructive to use as deliberate training opportunities, yet the competence required to respond to them decays without practice — the founding problem was finding a way to maintain readiness for events that cannot themselves be safely rehearsed.
% FOUNDING_PROBLEM_CORROBORATION: Simulator vendors, training bodies, and regulatory agencies attest the founding problem is solved by fidelity-equivalence and cite decades of incident-free operation as evidence. Independent human-factors researchers and several post-incident investigation boards (outside the certifying and purchasing parties) have documented cases where simulator-certified crews performed markedly worse during genuine catastrophic events than drill scores predicted, corroborating that the founding problem — real readiness, not just certifiable readiness — remains only partially solved.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, contested).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 at interval end) because the coordination function is real — genuine catastrophic exposure cannot ethically be manufactured, so simulation solves an actual scarcity problem — but the equivalence claim is asserted more strongly than the evidence supports, and the gap is monetized by vendors and administratively convenient for regulators, producing a modest but real extractive skew against operators and the public who bear the tail risk if the equivalence is false. Theater ratio rises across the interval (0.20 to 0.46) reflecting the documented drift toward drill-completion as the measured proxy for competence rather than a means to it — a Goodhart pattern where 'hours in simulator' substitutes for 'demonstrated readiness for genuine crisis.' Suppression is moderate and structural: dissenting human-factors evidence has institutional channels but slow uptake, not active silencing.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/operator seat, this looks like a functioning Rope: a genuine scarcity (can't train on real catastrophes) solved by a legible, auditable substitute. From the frontline-operator and public seat, the same structure reads as a Tangled Rope at best: real coordination function, but an asymmetric extraction where the parties who write and audit the standard are shielded from the consequences of the standard's residual uncertainty, while operators and the public carry the tail risk of an equivalence failure that shows up only when it is too late to correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating companies and regulatory agencies are dual-positioned as agenda_setters who also benefit — they design and enforce the compliance regime that lets them point to completed drills as due diligence, which shifts liability and disruption cost off themselves. Simulator vendors and certification bodies are pure beneficiaries whose institutional survival depends on equivalence being treated as sufficient. Frontline operators are targets: they perform the drills that satisfy the standard but personally absorb the consequence if the standard's underlying claim is wrong during a real event. The downstream public is the most powerless payer — trapped, generational exposure, with zero visibility into whether the equivalence claim actually holds for the crews protecting them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence maintenance for events too dangerous to rehearse) remains partially live — simulation genuinely solves large parts of it. But the classification prevents two mislabeling errors symmetric to each other: it does not let the story collapse into 'pure extraction / simulation is theater' (which would erase the real coordination value simulators provide), and it does not let the story collapse into 'pure coordination / simulation is proven equivalent' (which would erase the documented performance gap between drilled response and genuine crisis response). Tangled Rope holds both: real coordination function, real asymmetric cost-bearing, both riding the same infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_equivalence_ambiguity,
    'Does high-fidelity simulation actually reproduce the cognitive and physiological conditions (acute stress response, genuine consequence-weight, absence of a ''reset button'') necessary for competence transfer to real catastrophic events, or does it train a categorically different skill (scenario-script execution under known-safe conditions)?',
    'Comparative outcome studies tracking simulator-certified crews'' actual performance during genuine catastrophic events against their drill scores, controlled for event severity and crew experience; longitudinal human-factors research on stress-response transfer from simulation to reality.',
    'If equivalence holds, this reading''s coordination function is largely vindicated and the constraint is closer to a genuine Rope. If equivalence substantially fails, the constraint is closer to a Snare wearing coordination language, with vendors and regulators as concentrated beneficiaries of an unfalsified claim and operators/public as unwitting bearers of the resulting tail risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_equivalence_ambiguity, empirical, 'Whether simulator fidelity genuinely transfers to real-catastrophe competence or only certifies a different, safer skill.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the choice among the three kernel readings (simulation-sufficient, catastrophe-necessary, hybrid) itself driven by evidence about competence retention, or by which reading is administratively and financially convenient for the parties who control training budgets and certification standards?',
    'Cross-industry comparison of near-miss and incident rates under jurisdictions that have adopted different readings (pure-simulation regimes vs. hybrid regimes incorporating foreign-incident review), controlling for industry maturity and regulatory capacity.',
    'If the simulation-sufficient reading is primarily sustained by administrative convenience rather than evidence, this constraint''s claimed_type as tangled_rope understates its extractive character relative to the hybrid reading, which the evidence may actually favor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether kernel-reading selection tracks safety evidence or institutional convenience.').

omega_variable(
    theater_drift_mechanism,
    'Is the rising theater_ratio driven by regulators substituting an easily auditable proxy (logged simulator hours) for the harder-to-measure real target (demonstrated crisis readiness), or by a genuine increase in simulator sophistication that tracks real competence more closely over time?',
    'Audit trail analysis of certification criteria revisions over the interval: do revisions track independently validated competence measures, or do they track simulator vendor feature releases and compliance-cost minimization?',
    'If proxy substitution, the theater_ratio trend is a genuine Goodhart signal warranting reclassification pressure toward snare; if genuine fidelity improvement, the trend is compatible with the coordination story holding steady or strengthening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_mechanism, empirical, 'Whether rising theater ratio reflects proxy substitution or genuine simulator improvement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 6, 0.27).
narrative_ontology:measurement(cata_tr_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 12, 0.33).
narrative_ontology:measurement(cata_tr_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 18, 0.38).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.42).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.46).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(cata_be_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 6, 0.29).
narrative_ontology:measurement(cata_be_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(cata_be_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 18, 0.37).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cata_su_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 6, 0.26).
narrative_ontology:measurement(cata_su_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(cata_su_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 18, 0.32).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_avoidance_retention kernel. simulation_as_proxy_catastrophe (this story) asserts high-fidelity drills are functionally equivalent to real catastrophic events and classifies as tangled_rope (genuine training scarcity solved, but asymmetric tail-risk transfer to operators/public). catastrophe_as_necessary_selector asserts simulation is structurally inadequate — only real catastrophic exposure provides the necessary selection pressure — and would be expected to classify with different, likely higher, extraction attributed to the institutional refusal to acknowledge the inadequacy. hybrid_near_miss_learning asserts a blended distributed-learning model and would be expected to classify closer to a genuine rope, since it does not rest its coordination claim on a single contested equivalence. Each sibling carries its own ε, beneficiaries, and victims; they are linked here for contamination/network analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
