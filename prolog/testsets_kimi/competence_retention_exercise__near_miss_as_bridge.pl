% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Bridge Model for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the near_miss_as_bridge reading of the
 *   competence_retention_exercise kernel. The kernel asks how
 *   high-reliability organizations maintain catastrophe-avoidance competence
 *   over time. This reading holds that a hybrid systemâsimulators for
 *   routine skill preservation plus active near-miss investigation and
 *   integrationâsuffices, rendering catastrophes neither necessary nor
 *   sufficient. Structurally, the constraint coordinates safety learning
 *   while extracting asymmetric reporting and participation burdens from
 *   frontline practitioners. The sibling readings (simulation_as_sufficient,
 *   catastrophe_as_necessary) are structurally distinct constraints with
 *   different epsilon values, stakeholder distributions, and failure modes,
 *   linked in a constraint family.
 *
 * KEY AGENTS:
 *   - safety_integrity_unit: Agenda-setter (institutional/national) â designs near-miss protocols, mandates investigation, and controls simulator update integration
 *   - frontline_practitioners: Primary target (moderate/constrained) â bear reporting burden, blame exposure, and mandatory retraining load
 *   - training_simulator_facility: Beneficiary (organized/national) â receives empirical validation data and institutional legitimacy from near-miss integration
 *   - operational_leadership: Secondary beneficiary (powerful/national) â captures catastrophe-avoidance value and regulatory compliance
 *   - industry_regulators: Observer (institutional/global) â mandates and audits the competence retention regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.48).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.62).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Bridge Model for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '7181234d-9a56-4921-9600-117e1fcca781').
narrative_ontology:cs_kernel_codification('7181234d-9a56-4921-9600-117e1fcca781', formalized).
narrative_ontology:cs_authority_grounding('7181234d-9a56-4921-9600-117e1fcca781', expertise).
narrative_ontology:cs_interpretation_layer_present('7181234d-9a56-4921-9600-117e1fcca781').
narrative_ontology:cs_reading_relation('7181234d-9a56-4921-9600-117e1fcca781', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('7181234d-9a56-4921-9600-117e1fcca781', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_axiom('7181234d-9a56-4921-9600-117e1fcca781', foundational, hybrid_exercise_required).
narrative_ontology:cs_axiom_status(hybrid_exercise_required, holdable).
narrative_ontology:cs_axiom_grounding('7181234d-9a56-4921-9600-117e1fcca781', hybrid_exercise_required, instrumental).
narrative_ontology:cs_axiom('7181234d-9a56-4921-9600-117e1fcca781', foundational, near_miss_sufficiency).
narrative_ontology:cs_axiom_status(near_miss_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('7181234d-9a56-4921-9600-117e1fcca781', near_miss_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('7181234d-9a56-4921-9600-117e1fcca781', integrated_near_miss_bridge).
narrative_ontology:cs_drift_state('7181234d-9a56-4921-9600-117e1fcca781', contemporary_organizational_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7181234d-9a56-4921-9600-117e1fcca781', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, training_simulator_facility).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, operational_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the near-miss reporting protocol, sets investigation priorities, and decides which findings are translated into simulator scenario updates. Its authority derives from safety-science expertise and regulatory mandate. It cannot easily exit the constraint because its organizational legitimacy is tied to operating this specific hybrid learning system.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_integrity_unit, agenda_setter,
    institutional, generational, constrained, national).

% Licensed operators and frontline staff who experience operational near-misses. They are required to report incidents within mandated timeframes, participate in investigative interviews, and complete updated simulator training driven by the findings. Professional licensure and employment terms constrain exit; blame culture in some subcultures raises the personal cost of candor without eliminating the reporting obligation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_practitioners, payer,
    moderate, biographical, constrained, national).

% Develops and runs simulator curricula for high-risk operations. Receives structured near-miss data that validates scenario fidelity and justifies curriculum updates. Its institutional budget and reputation depend on the claim that simulator training is empirically grounded through active near-miss integration. Exit is constrained because its certification is tied to this specific learning model.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, training_simulator_facility, beneficiary,
    organized, biographical, constrained, national).

% Owns operational budgets and catastrophe-risk exposure. Benefits from reduced incident frequency and from demonstrating regulatory compliance through an active learning system. Captures the organizational value of avoided catastrophes and sustained operational tempo. Exit from the model is constrained by regulatory expectation and industry standardization.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, operational_leadership, beneficiary,
    powerful, biographical, constrained, national).

% Mandates competence-retention standards and audits organizational learning systems. Observes the hybrid model from an analytical seat, collecting safety-performance data and comparing regimes. Can alter the regulatory frame but does not directly experience the reporting burden or the training revenue.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, industry_regulators, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophe-avoidance competence across operational cycles by closing the learning loop between simulated rehearsal and lived operational reality: near-miss investigation generates empirical feedback that validates and updates simulator scenarios, keeping procedural training aligned with emerging failure modes.
% TRANSFER_FUNCTION: Moves detailed operational failure data and practitioner time from frontline practitioners to safety integrity units and simulator facilities; moves updated training mandates, reporting obligations, and investigative participation requirements back to frontline practitioners.
% ABSENT_VOICES: Frontline practitioners in blame-intensive subcultures who experience near-miss reporting as career risk and therefore remain silent; advocates for catastrophe-driven learning who argue that only high-severity events reveal system brittleness; pure-simulation proponents who regard near-miss field investigation as unnecessary overhead; unions where reporting mandates are imposed without negotiated workload offsets.
% DISAPPEARANCE_RATIONALE: If the near-miss bridge vanished overnight, simulator training would lose its primary empirical validation channel and drift into unverified scenario design; safety integrity units would lack the empirical signal that justifies their integration role; organizations would revert toward either pure simulation (unchecked fidelity decay) or catastrophe-driven learning (sparse, high-cost); the current division of labor and budget around hybrid competence maintenance would collapse.
% FOUNDING_PROBLEM: Catastrophe-driven organizational learning is too sparse and economically destructive to maintain routine competence; pure simulation without empirical validation drifts into fantasy and fails to capture operational reality; organizations needed a frequent, low-severity empirical signal to keep simulator training honest and practitioners alert.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers in high-reliability organization theory attest that near-misses provide necessary empirical density for organizational learning. Regulatory bodies such as aviation and nuclear agencies mandate near-miss programs. However, the claim that near-misses are SUFFICIENT without catastrophe as a background possibility is primarily self-asserted by the safety institutions and benefiting operational leadership; no independent corroborating source from outside the benefiting parties attests to sufficiency as distinct from utility.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the constraint genuinely coordinates safety learning, but it also systematically moves reporting labor, investigatory participation, and blame risk onto frontline practitioners while the safety office and leadership capture the coordination surplus. Suppression (0.62) is higher than extraction because the model requires active enforcement of reporting mandates, investigative compliance, and simulator recertification to persist; without this enforcement, frontline practitioners would underreport and the bridge would collapse. Theater_ratio (0.45) reflects mature checkbox drift: by interval end, a substantial share of near-miss activity is performative (reports filed to satisfy metrics rather than to update simulator scenarios). Accessibility_collapse (0.60) captures that alternative learning models (catastrophe-driven, pure-simulation) remain intellectually available but are institutionally marginalized once the hybrid model is adopted. Resistance (0.40) reflects chronic frontline underreporting and sporadic advocacy for catastrophe-driven learning from skeptics outside the safety office.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as genuine organizational learning infrastructure that prevents catastrophes and preserves professional standards. The frontline practitioner seat experiences the same structure as an enforced extraction of time, attention, and career risk with only indirect safety benefit. The engine computes this divergence from the structural data: the safety office has constrained exit tied to institutional identity, while frontline practitioners bear the mandated transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline practitioners are the declared victims (high d, amplified effective extraction): they pay the reporting and retraining costs under enforcement. Operational leadership and the simulator facility are beneficiaries (low d, damped or inverted extraction): they capture the value of avoided catastrophes and empirical legitimacy without bearing the lived reporting burden. The safety integrity unit sits near symmetric but agenda-setter privilege pulls it toward the beneficiary side; it does not personally bear the costs it administers. Regulators are analytical observers with no directional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both beneficiary declarations (coordination function: the simulator facility and leadership gain real safety value) and victim declarations (asymmetric extraction: frontline practitioners bear disproportionate costs). A pure rope reading would be blocked by the victim presence and active enforcement requirement. A pure snare reading would be blocked by the genuine coordination function and the fact that catastrophes are in fact avoided. The tangled_rope classification captures that the same institutional structure coordinates and extracts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_sufficiency_empirical_status,
    'Do near-miss incidents actually provide sufficient information to validate full catastrophe-avoidance competence, or do they systematically miss low-frequency high-severity failure modes that only reveal under extreme conditions?',
    'Longitudinal cohort study comparing organizations with pure near-miss-simulator hybrid models against historical controls and rare-event handling outcomes.',
    'If near-misses are insufficient for rare catastrophic modes, the constraint''s claimed coordination function is partially false and the classification shifts toward snareâthe system extracts frontline effort while failing to deliver the promised safety benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_sufficiency_empirical_status, empirical, 'Whether near-miss empirical density covers the tail-risk failure modes simulation is meant to address.').

omega_variable(
    reporting_burden_asymmetry,
    'Does the reporting and investigation burden fall asymmetrically on frontline practitioners relative to the safety benefits they receive, and does blame culture convert coordination into surveillance?',
    'Time-allocation studies, blame-incidence tracking, and comparative outcome analysis measuring practitioner reporting hours versus management and simulator-facility gains.',
    'High asymmetry with blame amplification confirms tangled_rope classification; symmetrical reciprocity with genuine protective benefit would support reclassification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_burden_asymmetry, empirical, 'Whether the cost-benefit distribution across seats is symmetric or extractive.').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the near_miss_as_bridge reading of the competence_retention_exercise kernel. A sibling reading (simulation_as_sufficient) would eliminate the frontline reporting burden entirely; another (catastrophe_as_necessary) would eliminate the simulator infrastructure. Which reading''s structural description best captures actual competence retention?',
    'Comparative organizational ethnography across three regimes: simulation-only training centers, near-miss-hybrid organizations, and catastrophe-exposed legacy systems.',
    'Resolution would validate or invalidate this reading''s sufficiency claim and determine whether the constraint''s hybrid structure is load-bearing or ornamental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural uncertainty about which kernel reading corresponds to operational reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.2).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.28).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.35).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is the near_miss_as_bridge reading of the competence_retention_exercise kernel, which decomposes into three structurally distinct claims about how organizations maintain catastrophe-avoidance competence. The sibling readings (simulation_as_sufficient, catastrophe_as_necessary) carry different epsilon values, stakeholder structures, and failure modes, and are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
