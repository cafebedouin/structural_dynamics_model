% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint is the reading 'simulation_as_sufficient' of the
 *   contested kernel 'competence_retention_exercise' in high-reliability
 *   organizations. It codifies the claim that high-fidelity simulator
 *   performance is structurally equivalent to real catastrophe-avoidance
 *   competence, making training infrastructure the primary mechanism for
 *   skill maintenance. The reading competes with siblings that treat actual
 *   catastrophes or near-misses as necessary. Over the interval, as
 *   simulation became the regulatory default in aviation, nuclear power, and
 *   process control, the constraint shifted from a supplementary training
 *   tool to the dominant certification gate. The authored claim is
 *   tangled_rope because the arrangement solves a genuine coordination
 *   problem—maintaining skills between rare disasters—while simultaneously
 *   extracting resources toward simulation vendors, suppressing alternative
 *   learning pathways, and transferring catastrophic risk to the operational
 *   environment.
 *
 * KEY AGENTS:
 *   - simulation_industry: Primary beneficiary (organized/mobile) — captures training budgets through vendor lock-in and regulatory mandate.
 *   - training_regime_administrators: Agenda-setter with secondary beneficiary status (institutional/constrained) — authority and budgets depend on simulation centrality.
 *   - regulatory_bodies: Agenda-setter (institutional/constrained) — prefers auditable simulator metrics over messy operational learning.
 *   - operational_practitioners: Primary payer (moderate/identity_locked) — professional identity fused to simulator certification; bears transfer-validity risk.
 *   - public_at_risk: Diffuse payer (powerless/trapped) — relies on operator competence but cannot verify simulator-to-real transfer.
 *   - near_miss_researchers: Excluded voice (moderate/constrained) — marginalized as budgets shift to simulation infrastructure.
 *   - safety_science_analysts: Analytical observer (analytical/analytical) — documents fidelity gaps and transfer failures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.62).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.58).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '3f53f547-6e68-4da0-82d9-1ebafaa699e9').
narrative_ontology:cs_kernel_codification('3f53f547-6e68-4da0-82d9-1ebafaa699e9', formalized).
narrative_ontology:cs_authority_grounding('3f53f547-6e68-4da0-82d9-1ebafaa699e9', expertise).
narrative_ontology:cs_interpretation_layer_present('3f53f547-6e68-4da0-82d9-1ebafaa699e9').
narrative_ontology:cs_reading_relation('3f53f547-6e68-4da0-82d9-1ebafaa699e9', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('3f53f547-6e68-4da0-82d9-1ebafaa699e9', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('3f53f547-6e68-4da0-82d9-1ebafaa699e9', foundational, simulation_structural_equivalence_to_real_catastrophe).
narrative_ontology:cs_axiom_status(simulation_structural_equivalence_to_real_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('3f53f547-6e68-4da0-82d9-1ebafaa699e9', simulation_structural_equivalence_to_real_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('3f53f547-6e68-4da0-82d9-1ebafaa699e9', foundational, simulator_performance_metrics_valid_competence_measure).
narrative_ontology:cs_axiom_status(simulator_performance_metrics_valid_competence_measure, holdable).
narrative_ontology:cs_axiom_grounding('3f53f547-6e68-4da0-82d9-1ebafaa699e9', simulator_performance_metrics_valid_competence_measure, empirically_contingent).
narrative_ontology:cs_reference_frame('3f53f547-6e68-4da0-82d9-1ebafaa699e9', simulation_certified_competence).
narrative_ontology:cs_drift_state('3f53f547-6e68-4da0-82d9-1ebafaa699e9', contemporary_operational_scrutiny, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f53f547-6e68-4da0-82d9-1ebafaa699e9', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_regime_administrators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, operational_practitioners).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, public_at_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulation_sufficiency_thesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, competence_metrics_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sells high-fidelity simulators, scenario libraries, maintenance contracts, and certification services. Revenue grows as regulators and operators treat simulator hours as sufficient for competence retention. Benefits directly from mandates that displace alternative learning budgets.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_industry, beneficiary,
    organized, generational, mobile, global).

% Design curricula, set simulator check-ride standards, and administer certification programs. Their institutional authority and staffing budgets depend on simulation being recognized as the primary competence-maintenance mechanism. They enforce the schedule of required simulator sessions and define passing metrics.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_regime_administrators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, training_regime_administrators, beneficiary).

% Mandate minimum simulator hours and performance thresholds for licensing. Prefer quantifiable, auditable training metrics over messy operational-experience or near-miss learning systems. Their oversight model is built around certifiable simulation outputs.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Must recurrently demonstrate competence in simulators to maintain certification and employment. Professional identity and livelihood are fused to simulator performance metrics. They bear the risk that simulated proficiency may not transfer to novel real-world catastrophes, yet they have no viable alternative credentialing path.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operational_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Relies on operator competence for safety in aviation, nuclear power, and process industries. Cannot verify whether simulator-certified operators retain genuine catastrophe-avoidance skills or merely procedural fluency in scripted scenarios. Bears catastrophic downside if the equivalence claim fails.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, public_at_risk, payer,
    powerless, biographical, trapped, national).

% Study operational near-misses as a source of organizational learning and simulator validation. Their programs lose funding and institutional attention as simulation-as-sufficient frameworks reallocate safety budgets toward training infrastructure and away from operational feedback systems.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_researchers, excluded,
    moderate, generational, constrained, global).

% Conduct meta-analyses and field studies on transfer of training from simulation to operational performance. They document fidelity gaps, scenario-validity limits, and instances where simulator-trained crews failed in real events. Their findings are often treated as methodological noise rather than structural critiques.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_science_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulation_industry).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophe-avoidance competence in high-risk domains during long intervals between actual catastrophic events, without relying on the occurrence of disasters for practice.
% TRANSFER_FUNCTION: Moves organizational learning budgets and regulatory attention from operational-experience and near-miss systems into simulation infrastructure; moves financial flows from operators and certifying agencies to simulation vendors and training departments; transfers catastrophic risk from training organizations to the operational environment.
% ABSENT_VOICES: Operational veterans whose formative competence came from actual failures and recoveries; communities that have suffered catastrophes where simulator-trained operators performed inadequately; near-miss investigators whose budget lines were absorbed by simulator procurement.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, regulators would revert to operational-experience or near-miss-based certification standards, training budgets would shift away from capital-intensive simulators toward line operations and incident investigation, and the simulation industry's revenue model would contract sharply.
% FOUNDING_PROBLEM: Catastrophic events are too rare and costly to serve as the primary training mechanism for high-risk operations; without recurrent practice, operator skills atrophy and organizations lose competence in catastrophe avoidance.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability organization researchers (Weick, LaPorte school) attest that rarity of catastrophes creates skill-decay risk. Safety scientists outside the simulation-beneficiary complex (Dekker, Woods, Perrow tradition) attest that the founding problem is partially solved but the chosen solution has become pathological, substituting procedural rehearsal for situated competence.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial decoupling of simulator performance from real catastrophe competence, plus the resource capture by training infrastructure. Suppression (0.58) is structural: alternative learning channels (near-miss reporting, operational apprenticeship) are institutionally deprioritized, and critiques of simulator fidelity are treated as threats to certification regimes rather than valid safety input. Theater ratio (0.45) rises over the interval as box-checking and metric-gaming displace genuine skill assessment; simulator sessions become performances for the certification record. Accessibility collapse (0.48) is moderate because operational learning alternatives are still physically possible but institutionally starved. Resistance (0.42) comes from operational veterans and safety scientists who contest the equivalence claim. The temporal series share one aligned grid so metric trajectories are co-measured.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation-industry and training-administrator seats, the constraint is genuine safety innovation that prevents catastrophes through repeatable, scalable rehearsal. From the operational-practitioner seat, it is an identity-locked credentialing gate where simulator scores determine employment regardless of real-event readiness. From the public seat, it is an opaque risk transfer: the reassurance of 'simulator-certified' operators masks an unvalidated empirical premise. The analytical seat sees the divergence clearly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (simulation industry, training administrators, regulatory bodies) derive low directionality from the constraint: it subsidizes their budgets, authority, and business models. Victims (operational practitioners, public at risk) derive high directionality: they bear the costs of procedural metric-gaming and catastrophic downside if transfer fails. The agenda-setters' enforcement of simulator mandates is what maintains the extraction; without active enforcement, operators and organizations would revert to experiential learning.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as rope would miss the extraction: the simulation industry captures concentrated rents, and alternative learning is suppressed. Classifying it as snare would miss the coordination: simulation genuinely preserves procedural skills and prevents some catastrophes. Tangled rope captures both the real coordination function and the asymmetric extraction that rides on it. A scaffold classification would fail because there is no sunset clause—the constraint is treated as a permanent steady state, not a transitional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_transfer_validity,
    'Does high-fidelity simulation actually produce equivalent catastrophe-avoidance competence in real events, or only correlated procedural performance?',
    'Longitudinal incident-analysis studies comparing outcomes of simulator-certified crews versus operationally-experienced crews in real critical events, controlling for scenario type and novelty.',
    'If transfer validity is weak, the constraint is more extractive (false confidence, risk transfer) and its coordination value is overstated; if strong, the extraction measure overstates the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_transfer_validity, empirical, 'Whether simulated competence transfers to real catastrophes').

omega_variable(
    resource_capture_by_simulation_vendors,
    'Has the simulation-sufficiency thesis been captured by commercial interests, or does it remain independent safety science?',
    'Funding-source and authorship-network analysis for pivotal studies and standards-body memberships in high-risk industries.',
    'Capture would raise extractiveness and suppress theater_ratio threshold; independence would support the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_capture_by_simulation_vendors, empirical, 'Commercial capture of the simulation-sufficiency thesis').

omega_variable(
    suppression_of_operational_learning,
    'Does the simulation-as-sufficient framework structurally suppress near-miss reporting and post-incident investigation?',
    'Budget-allocation and publication-trend analysis in safety-critical industries over the same interval.',
    'If operational learning is suppressed, the coordination story masks extraction from organizational learning capacity; if not, the constraint is more purely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_operational_learning, empirical, 'Whether simulation centrality suppresses alternative learning channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_sim_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.1).
narrative_ontology:measurement(competence_sim_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.16).
narrative_ontology:measurement(competence_sim_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.24).
narrative_ontology:measurement(competence_sim_tr_t15, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 15, 0.32).
narrative_ontology:measurement(competence_sim_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.4).
narrative_ontology:measurement(competence_sim_tr_t25, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(competence_sim_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(competence_sim_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(competence_sim_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(competence_sim_be_t15, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(competence_sim_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(competence_sim_be_t25, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(competence_sim_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(competence_sim_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(competence_sim_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(competence_sim_su_t15, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(competence_sim_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(competence_sim_su_t25, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, resource_allocation).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_retention_exercise kernel, instantiated as simulation_as_sufficient. Sibling readings include catastrophe_as_necessary and near_miss_as_bridge. The kernel decomposes into separate stories because each reading has a distinct epsilon, beneficiary structure, and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
