% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Integration for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations maintain competence in rare-event domains
 *   (aviation, nuclear operations, emergency medicine) through a hybrid
 *   system: high-fidelity simulators provide routine skill maintenance at low
 *   cost, while systematic investigation and integration of near-miss
 *   incidents (operational incidents that nearly caused catastrophe but did
 *   not) grounds simulator training in real-world boundary conditions. This
 *   constraint instantiates the reading that near-miss data is SUFFICIENT for
 *   competence validation without catastrophic-event dependency — the
 *   'near-miss-as-bridge' reading of the competence-retention-exercise
 *   kernel. The constraint is CLAIMED as tangled_rope: it coordinates genuine
 *   safety function (hybrid learning system prevents both
 *   simulation-disconnection and catastrophe-dependency) AND asymmetrically
 *   extracts (time, reputation exposure, operational disruption) from
 *   frontline operators and resource-constrained sites. The measurement
 *   series traces theater_ratio declining (the constraint's performative
 *   overhead decreases as near-miss investigation matures) and extraction
 *   stabilizing as the system matures beyond its initial adoption phase.
 *
 * KEY AGENTS:
 *   - Safety organizations (institutional agenda-setter): set policies on what constitutes legitimate competence validation; beneficiary of institutional standing via modernized framework
 *   - Training infrastructure operators (institutional beneficiary): simulators and training programs receive sustained legitimacy and funding
 *   - Frontline operators (powerless, identity-locked payer): required to participate in near-miss reporting and investigation; expose their judgment to institutional scrutiny
 *   - Resource-constrained sites (moderate-power, constrained payer): must implement near-miss systems with limited budgets; operational exit barred by regulatory requirement
 *   - Catastrophe-survivor constituencies (powerful beneficiary but excluded from governance): benefit from preemptive learning but do not set investigation thresholds
 *   - Research community (observer): produces evidence on whether hybrid system maintains competence equal to catastrophe-validated competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Integration for Competence Maintenance").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '030549b6-4f8a-4ea5-b318-3e278f3f3ad7').
narrative_ontology:cs_kernel_codification('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', distributed).
narrative_ontology:cs_authority_grounding('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', expertise).
narrative_ontology:cs_interpretation_layer_present('030549b6-4f8a-4ea5-b318-3e278f3f3ad7').
narrative_ontology:cs_reading_relation('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_axiom('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', foundational, near_miss_data_is_competence_validating).
narrative_ontology:cs_axiom_status(near_miss_data_is_competence_validating, holdable).
narrative_ontology:cs_axiom_grounding('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', near_miss_data_is_competence_validating, empirically_contingent).
narrative_ontology:cs_axiom('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', foundational, catastrophe_not_required_for_competence_maintenance).
narrative_ontology:cs_axiom_status(catastrophe_not_required_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', catastrophe_not_required_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', hybrid_learning_system_as_legitimate).
narrative_ontology:cs_drift_state('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', contemporary_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('030549b6-4f8a-4ea5-b318-3e278f3f3ad7', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_organizations).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, training_infrastructure_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, resource_constrained_sites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, catastrophe_survivors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_manufacturers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, junior_and_early_career_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, junior_and_early_career_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governs which learning feedback sources are treated as legitimate for competence validation. Sets policy that near-miss investigation and integration is mandatory, funded from training budgets. Justifies this by claiming near-miss data bridges the gap between simulation and rare catastrophes, reducing redundant full-catastrophe requirements. Collects institutional credit for modernizing competence frameworks.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate high-fidelity simulators and maintain training programs. Benefit from policies that legitimize simulator training as primary competence maintenance tool, since near-miss integration allows simulators to claim real-world validation without catastrophe dependency. Receive stable funding and institutional standing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, training_infrastructure_operators, beneficiary,
    institutional, generational, mobile, global).

% Aircraft pilots, nuclear plant operators, surgeons, emergency responders. Required to participate in near-miss reporting and investigation protocols that consume operational time and expose them to institutional scrutiny of their judgment. Identity locked into professional identity requiring continuous competence proof. Bear the cost of mandatory near-miss investigation infrastructure (time, psychological burden of review, career risk from incident disclosure).
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, payer,
    powerless, biographical, identity_locked, local).

% Regional hospitals, smaller airports, rural emergency services. Must implement near-miss investigation and learning systems despite limited staff and budget. The mandate assumes resources for systematic documentation, analysis, and simulator updating that resource-poor sites struggle to provision. Exit options are constrained by regulatory requirement to maintain training; cannot opt out of near-miss reporting without losing operational license.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, resource_constrained_sites, payer,
    moderate, biographical, constrained, local).

% Regulatory bodies and public-safety constituencies that historically have required catastrophic incident review to drive change. Benefit from near-miss integration because it shifts learning earlier and reduces normalized acceptance of catastrophe. Also excluded from operational decision-making about which near-miss data rises to formal investigation level.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, catastrophe_survivors, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, catastrophe_survivors, excluded).

% Benefit from sustained institutional legitimacy of simulator-based training. Near-miss integration policy validates simulator training as primary competence maintenance tool without requiring them to achieve catastrophe-level fidelity. Receive contracts for simulator maintenance and real-world data integration tools.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Benefit from access to near-miss data integrated into training scenarios — real-world incident patterns inform their simulator training without requiring them to survive a catastrophe to learn. Also pay through career-building constraints: incidents in their early record (even as participants in near-miss investigation) can carry reputational cost in professional communities where incident involvement is signal of risk.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, junior_and_early_career_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, junior_and_early_career_operators, payer).

% Analyzes competence maintenance frameworks and near-miss data. Studies whether hybrid simulator-plus-near-miss systems actually maintain competence equal to catastrophe-validated competence. Can produce evidence that shifts the legitimacy of the constraint.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, research_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, safety_organizations).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of maintaining rare-event competence in domains where catastrophes are statistically infrequent but catastrophically costly: how do operators stay sharp for events that happen once per decade or century? The solution coordinates simulator-based routine skill maintenance (low-cost, repeatable) with active near-miss investigation (real-world data, lower organizational trauma than catastrophe). Without integration, each feeds the other poorly: simulators become increasingly disconnected from actual near-miss patterns, while near-miss investigations have no systematic training feedback pathway.
% TRANSFER_FUNCTION: Moves operational time, professional reputation exposure, and investigative labor from frontline operators and resource-constrained sites toward institutional training infrastructures and safety organizations. Moves legitimacy and institutional standing from catastrophe-driven learning to preemptive near-miss integration. Moves resources from other operations to near-miss documentation and analysis protocols.
% ABSENT_VOICES: Operators at sites that have NOT experienced near-miss incidents (early-career operators in low-incident domains, new facilities) would argue that mandatory near-miss investigation is theater when there is no near-miss data — they are excluded from governance of what constitutes 'sufficient' near-miss feedback, and the threshold of incident severity triggering investigation is set by the same organizations that benefit from the constraint. Retired operators and practitioners from catastrophe-driven learning eras who believe competence maintenance requires visceral stakes would object but are structurally outside the operational system.
% DISAPPEARANCE_RATIONALE: If the near-miss-as-bridge constraint disappeared, institutions would revert to pure simulator-based training (lower institutional cost, less operational intrusion) or revert to requiring catastrophic incidents as proof that competence maintenance is real. Regulatory frameworks would shift away from mandatory near-miss investigation as a training input; simulators would no longer be held accountable to near-miss pattern matching. Research funding and institutional standing for near-miss integration would evaporate.
% FOUNDING_PROBLEM: Mid-20th-century high-reliability organizations discovered that waiting for catastrophes to drive competence maintenance was organizationally and ethically untenable: learning curves measured in fatalities. Simulators were adopted as low-cost alternatives, but early simulators were too abstracted from real-world boundary conditions, producing trained incompetence (operators could pass simulators but fail at near-miss detection in actual operations). Near-miss integration policy emerged as a mechanism to ground simulator training in real incident patterns without the institutional cost of actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Aviation authorities (FAA, ICAO), nuclear regulators (NRC, IAEA), and surgical training bodies (ACGME, specialty boards) attest the founding problem persists: competence maintenance remains difficult in low-incident domains, and simulators still risk abstraction from real-world boundary conditions. Independent research from organizational learning literature (Weick & Sutcliffe, Eurocontrol safety studies) corroborates that near-miss data patterns differ structurally from catastrophe incident patterns and that high-reliability organizations relying on either source alone show competence gaps. The constraint persists because the founding problem is not solved, only managed.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38) because the constraint's extraction is real but purposeful: it extracts operational time and reputational exposure to fund a genuine safety function, not to capture rents. The extraction is justified by the coordination benefit, which is why the type is tangled_rope (not snare). Suppression is low-to-moderate (0.22) because the constraint's persistence does not depend heavily on coercing participation — operators are professionally identity-locked into safety compliance, not externally suppressed. Theater declines over the interval (0.35 → 0.18) because early-stage near-miss investigation programs carry higher performative overhead (new structures, unfamiliar investigation protocols, visibility to oversight bodies); as the system matures, the performative fraction decreases and the real learning fraction increases. Accessibility collapse is moderate (0.42): alternatives exist (pure simulator-only, or catastrophe-dependency), but choosing them requires institutional resistance and regulatory override — exit from the hybrid system is possible but costly. Resistance is substantial (0.61) because significant constituencies (early-career operators, resource-poor organizations, catastrophe-survivors who believe only real stakes teach) contest whether near-miss data is truly sufficient — they resist the implicit claim that simulation-plus-near-miss substitutes for catastrophe-scale incident learning.
 *
 * PERSPECTIVAL GAP:
 *   From the safety-organization and training-infrastructure seats, the constraint appears as a genuine safety innovation: it provides real-world data feedback without organizational trauma of catastrophes. From the frontline-operator and resource-constrained-site seats, the same constraint appears as mandated operational disruption justified by an unproven claim that near-miss data is sufficient. The divergence is structural: the beneficiary seats set the threshold for what counts as 'sufficient' near-miss feedback and what constitutes competence validation; the target seats live with the investigation protocols but do not control their design. The engine computes this per-seat divergence from the stakeholder power atoms and directionality: powerful institutional agenda-setters compute rope/coordination from their seat; powerless identity-locked operators compute tangled_rope/extraction from their seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety organizations sit at d ≈ 0.15 (structural beneficiary): they set policy, collect institutional credit, face no personal operational cost. Training infrastructure operators sit at d ≈ 0.20 (beneficiary with minor accountability): their institutional interest aligns with the constraint; they have arbitrage-grade exit options if policy shifted. Frontline operators sit at d ≈ 0.85 (structural target): they are powerless, identity-locked into participation, face reputational and time costs of incident investigation with no direct compensation. Their exit — refusing to report near-misses or leaving the profession — is behaviorally and economically trapped. Resource-constrained sites sit at d ≈ 0.75 (target): moderate institutional power but constrained by regulatory mandate and limited budgets to implement the required infrastructure. Junior/early-career operators sit at d ≈ 0.55 (mixed): they benefit from near-miss data in training (moving them toward beneficiary) but also pay through career-signaling risk (incident involvement is ambiguous in professional reputation systems).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence maintenance in low-incident domains) is live and unresolved — near-miss integration does not eliminate it, only manages it. The near-miss-as-bridge reading asserts that near-miss data sufficiently validates competence, but this is contested: the catastrophe_as_necessary reading asserts only visceral stakes (actual near-deaths, actual property damage) maintain genuine competence; the simulation_as_sufficient reading asserts high-fidelity simulators alone are sufficient. The constraint's persistence depends on institutional authority maintaining that near-miss data is legitimate input to competence validation. Mandatrophy analysis: the constraint has NOT experienced mandatrophy. The founding problem persists, the coordination function remains genuine (hybrid system does prevent both simulation-disconnection and catastrophe-dependency), and active enforcement is necessary (near-miss investigation protocols must be maintained as standard institutional practice, not optional). The constraint does not show the signature of a mandate outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_sufficiency_empirical,
    'Do operators trained via simulator-plus-near-miss integration demonstrate competence maintenance equal to operators who have experienced actual near-catastrophe events or to operators trained via catastrophe-driven learning cycles?',
    'Longitudinal cohort study comparing competence measures (simulator performance, incident investigation decisions, decision-making speed under stress) across three populations: near-miss-plus-simulator trained, catastrophe-survivor trained, pure-simulator trained. Track populations across 10-20 year careers; measure competence decay over intervals without recent incidents.',
    'If near-miss-trained operators show measurable competence maintenance gaps (slower decision-making under extreme stress, missed boundary-condition violations) compared to catastrophe-survivors, the reading collapses — near-miss data is insufficient bridge, and catastrophe_as_necessary reading gains evidentiary support. If competence is equal, the reading is supported and the constraint''s foundation solidifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(near_miss_sufficiency_empirical, empirical, 'The core empirical claim of this reading: near-miss data sufficiency for competence validation').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the measured suppression (0.22) structural — imposed by regulatory barriers and licensing dependencies — or internalized — frontline operators have incorporated the near-miss-investigation requirement into their professional identity, making exit psychologically unthinkable even if regulatory barriers fell?',
    'Post-regulatory-change natural experiment: if jurisdictions remove mandatory near-miss reporting requirements, measure participation rates among operators who continue voluntary participation. High voluntary participation would indicate internalized identity-lock; low rates would indicate suppression was structural and external.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the scalar measure suggests — operators carry the reporting obligation in their self-concept. If suppression is structural, the constraint''s persistence depends on continued regulatory enforcement; deregulation would likely collapse it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural vs. internalized mechanism of suppression in professional identity constraints').

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the competence-retention-exercise kernel is the actual operative framework in this organization''s competence validation policy?',
    'Document analysis of training policy, regulatory guidance, and incident-investigation protocols. Examine whether organizations explicitly reference near-miss-sufficiency, catastrophe-requirement, or simulation-sufficiency as the grounding claim. Track which reading is invoked when competence-validation disputes arise.',
    'If catastrophe_as_necessary is the operative reading despite this constraint''s framing as near_miss_as_bridge, the constraint is theater — institutional authority claims near-miss-sufficiency but operationally requires catastrophic validation. If simulation_as_sufficient is operationally dominant, near-miss integration is secondary theater. If near_miss_as_bridge is operationally coherent, the constraint''s classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether this reading''s axiom (near-miss data is competence-validating) is the operative framing of the competence system').

omega_variable(
    resource_constraint_equity,
    'Do resource-constrained sites (rural hospitals, small airports, developing-world operations) implement near-miss investigation systems with quality and coverage equivalent to well-resourced sites, or do they substitute cheaper performance theater (checking investigation boxes without genuine analysis)?',
    'Audit of near-miss investigation documentation and analysis quality across resource-rich and resource-poor sites. Compare investigation depth, simulator data integration, training feedback cycles, and outcome-measure correlation.',
    'If resource-poor sites systematically implement lower-quality theater versions of near-miss investigation, the constraint creates a competence-equity gap: rich sites access real safety learning, poor sites perform compliance theater. This would reclassify the constraint from tangled_rope toward snare (resource extraction without genuine coordination benefit for resource-poor seats).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constraint_equity, empirical, 'Whether the constraint''s benefits distribute equitably or concentrate among well-resourced organizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.32).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 15, 0.23).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.2).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 25, 0.19).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.18).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 25, 0.23).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulator_fidelity_sufficiency_escalation).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, incident_investigation_as_training_feedback).

% DUAL FORMULATION NOTE:
% This constraint is one reading (near_miss_as_bridge) of the contested kernel competence_retention_exercise. Two sibling readings coexist: catastrophe_as_necessary (asserting only actual catastrophes maintain competence) and simulation_as_sufficient (asserting pure simulation is adequate). These three constraints form a family linked by shared kernel. The near-miss reading forecloses the catastrophe reading within a single institutional framework but coexists with the simulation reading across different organizations. Network edges reflect structural influences: near-miss-as-bridge validates simulator training (affects simulator_fidelity_sufficiency_escalation) and operationalizes incident-investigation feedback loops (affects incident_investigation_as_training_feedback). Decomposition is necessary (per ε-invariance principle) because the three readings have different ε values: near-miss-as-bridge and simulation-as-sufficient both have moderate extraction (0.35-0.40) due to institutional capture of learning infrastructure; catastrophe-as-necessary reading has low extraction (0.15) because it treats actual events as natural learning source requiring no institutional mediation. Measurement bases differ too: near-miss and simulation readings rest on institutional validation claims (empirically contingent); catastrophe-as-necessary rests on organizational-learning theory (conceptual, empirically complex).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
