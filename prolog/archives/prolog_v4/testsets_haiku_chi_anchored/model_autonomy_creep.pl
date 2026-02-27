% ============================================================================
% CONSTRAINT STORY: model_autonomy_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_autonomy_creep, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: model_autonomy_creep
 *   human_readable: The Administrative Autonomy Slide
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   The administrative autonomy slide describes the structural process by
 *   which an AI model incrementally assumes decision-making authority within
 *   an organization, extracting human discretion and accountability capacity.
 *   This occurs not through single dramatic transfer of authority, but
 *   through accumulated organizational choices to rely on model
 *   recommendations, deprioritize human review, and gradually subordinate
 *   human judgment to algorithmic optimization. The constraint exhibits
 *   properties of both hybrid coordination (solving bottleneck problems in
 *   decision throughput) and extraction (concentrating authority in opaque
 *   systems while maintaining human accountability). The theater ratio has
 *   increased from 0.28 to 0.64 over six years as formal human review
 *   processes became performative — present in workflows and documentation
 *   but lacking real authority to stop model-driven decisions. The
 *   extractiveness metric has grown from 0.22 to 0.58 as organizational
 *   friction for overriding model recommendations increased and the scope of
 *   autonomy expanded from low-stakes administrative decisions toward
 *   higher-consequence determinations. This is a tangled rope scenario where
 *   genuine coordination benefit (reduced decision bottlenecks) coexists with
 *   extraction (transfer of accountability without transfer of oversight
 *   capacity), and where the human victims experience it as snare (trapped in
 *   formal responsibility without real authority).
 *
 * KEY AGENTS:
 *   - Model Operators (ML engineers, deployment teams): Institutional/arbitrage — primary beneficiaries of reduced bottlenecks and operational autonomy
 *   - Human Discretion Bearers (middle managers, decision-makers): Powerless/trapped — nominally accountable for model decisions but lack authority to override; cannot exit without job loss
 *   - Operations and QA Teams: Moderate/constrained — experience mixed coordination benefit and extraction; constrained by employment but can partially exit through role redesign
 *   - Efficiency Optimization Function (cost reduction, throughput): Institutional/arbitrage — primary beneficiary of autonomy creep; drives continued expansion
 *   - Accountability Infrastructure (audit, compliance, explainability): Powerless/trapped — subordinated to efficiency gains; surveillance and review authority progressively undermined
 *   - Audit and Governance Functions: Organized/constrained — need model efficiency but require oversight authority; constrained by governance responsibility; some capacity to influence through regulatory levers
 *   - Human Oversight Ritual (formal review processes): Piton — persist through institutional inertia and regulatory appearance requirements, not real function
 *   - Regulatory and Governance Coalition (GDPR, SOX, algorithmic accountability law): Organized/mobile — sees autonomy creep as temporary coordination problem with regulatory sunset; building constraints
 *   - Analytical Observer: Analytical/analytical — risks naturalizing autonomy creep as inherent to AI scaling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_autonomy_creep, 0.58).
domain_priors:suppression_score(model_autonomy_creep, 0.68).
domain_priors:theater_ratio(model_autonomy_creep, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_autonomy_creep, extractiveness, 0.58).
narrative_ontology:constraint_metric(model_autonomy_creep, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(model_autonomy_creep, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_autonomy_creep, tangled_rope).
narrative_ontology:human_readable(model_autonomy_creep, "The Administrative Autonomy Slide").
narrative_ontology:topic_domain(model_autonomy_creep, "technological/organizational").

domain_priors:requires_active_enforcement(model_autonomy_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_autonomy_creep, model_operators).
narrative_ontology:constraint_beneficiary(model_autonomy_creep, efficiency_optimizers).
narrative_ontology:constraint_victim(model_autonomy_creep, human_discretion_bearers).
narrative_ontology:constraint_victim(model_autonomy_creep, accountability_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Middle managers and decision-makers in administrative workflows experience administrative autonomy slide as a snare. They are nominally accountable for decisions that the model makes, yet lack authority to override model recommendations without organizational friction. They cannot exit — exiting means job loss. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.79.
constraint_indexing:constraint_classification(model_autonomy_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Audit trails, explainability systems, and human oversight structures experience the constraint as extraction: they are subordinated to model efficiency gains. As autonomy creeps, verification becomes theater (auditing what the model decided, rather than whether the decision was sound). Cannot exit. d≈0.94, f(d)≈1.40, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(model_autonomy_creep, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Operations and quality assurance teams experience tangled rope: they benefit from automation reducing manual load, but face constraint that their authority to intervene is progressively constrained by model-driven workflows. Exit options limited by employment. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% ML engineers and model deployment teams experience the constraint as rope: coordination benefit from reduced manual bottlenecks, improved throughput. Can exit or adapt the deployment. d≈0.12, f(d)≈0.06, σ=1.0 → χ≈0.03. Net beneficiary through pure coordination.
constraint_indexing:constraint_classification(model_autonomy_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Organizational cost-reduction and throughput-maximization functions experience this as pure coordination: automating discretionary decisions solves collective action problem of decision bottlenecks. Can arbitrage or exit. d≈0.08, f(d)≈-0.06, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(model_autonomy_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Internal audit, compliance, and governance functions experience tangled rope: they need model efficiency to justify organizational investments, but their role requires authority to audit decisions — which creeping autonomy undermines. Constrained by governance responsibility. d≈0.54, f(d)≈0.72, σ=0.9 → χ≈0.37.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Formal human review and sign-off processes become piton: they persist as theater to satisfy governance requirements, but lack real authority once model autonomy is entrenched. theater_ratio=0.64. They appear in workflow diagrams and regulatory documents but don't stop decisions. Institutional inertia maintains them despite degraded function.
constraint_indexing:constraint_classification(model_autonomy_creep, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulators, governance frameworks (SOX, GDPR, algorithmic accountability laws), and governance-focused institutions see the constraint as scaffold: temporary coordination problem that can be solved by building robust human-in-the-loop requirements with sunset clauses. As regulation matures, enforcement of meaningful human oversight becomes binding. d≈0.35, f(d)≈0.34, σ=1.1 → χ≈0.26. Sees an exit path through regulatory maturation.
constraint_indexing:constraint_classification(model_autonomy_creep, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical/civilizational perspective risks treating autonomy creep as inherent to AI scaling — a natural law that 'intelligent systems naturally optimize themselves into decision authority.' However, structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts mountain. The creep is contingent on institutional choices, not immutable. Engine will compute as false summit.
constraint_indexing:constraint_classification(model_autonomy_creep, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_autonomy_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_autonomy_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_autonomy_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_autonomy_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_autonomy_creep, TR),
    TR >= 0.70.

:- end_tests(model_autonomy_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts discretion and accountability capacity from human decision-makers and concentrates them in opaque model systems. However, it is not extreme (≤0.66 for snare) because much of the autonomy transfer solves legitimate organizational problems (decision bottlenecks, inconsistent judgment). The extraction is contingent on organizational choices, not fundamental. The measurement trajectory (0.22→0.58) shows acceleration, indicating rent-seeking layered onto coordination. Suppression (0.68): High. Multiple mechanisms suppress alternatives to model-driven decisions: switching costs (retraining to alternative systems), career risk of contradicting model recommendations, workflow friction for overrides, psychological authority deferral to algorithms, and regulatory compliance theater (appearing to review decisions while lacking real power to stop them). The suppression is not total (≤0.80) because motivated actors can still challenge model decisions, albeit with significant friction. Theater ratio (0.64): Moderate-high. Formal human review and sign-off processes increasingly perform regulatory and governance functions rather than substantive validation. Auditors review logs to explain decisions, not to determine whether they were sound. Human-in-the-loop becomes 'human in appearance of loop.' The trajectory (0.28→0.64) shows theater increasing as autonomy creeps — review processes persist for appearance while losing real authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a deep perspectival gap between beneficiaries and victims. The model operators and efficiency functions experience rope (coordination benefit, no extraction). The human discretion bearers experience snare (trapped, extraction, no exit). The audit and governance functions experience tangled rope (need efficiency but require oversight authority). The regulatory coalition experiences scaffold (temporary problem, sunset path through regulation). The analytical observer risks mountain (naturalizing as inherent to AI). The gap reflects that autonomy creep solves real problems (bottlenecks) while creating new ones (concentrated authority, degraded accountability). The tangled rope classification for the base constraint reflects that the coordination benefit is genuine and the extraction is not inevitable — it depends on whether oversight authority can be maintained as autonomy increases.
 *
 * DIRECTIONALITY LOGIC:
 *   Model operators and efficiency optimizers: Beneficiaries + arbitrage → d≈0.10, f(d)≈0.00. Net beneficiaries; can exit or adapt deployment. Human discretion bearers: Victims + trapped → d≈0.92, f(d)≈1.38. Maximal extraction; nominally accountable but lack real authority. Accountability infrastructure: Victims + trapped → d≈0.94, f(d)≈1.40. Maximal extraction; subordinated to efficiency. Operations/QA: Victims + constrained → d≈0.68, f(d)≈1.02. High extraction but not maximal; some adaptation capacity. Audit/governance: Mixed + constrained → d≈0.54, f(d)≈0.72. Can influence through regulation; constrained by governance responsibility. Oversight ritual: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate, not directionality. Regulatory coalition: Organized + mobile → d≈0.35, f(d)≈0.34. Can mobilize oversight authority; sees exit path.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing genuine coordination benefit from extractive layering. At time 0 (ε=0.22, theater=0.28), autonomy creep was primarily rope: solving decision bottlenecks, low performative content. As time progressed, organizational actors began optimizing for efficiency gains while subordinating oversight, causing ε and theater to climb. By time 6 (ε=0.58, theater=0.64), the constraint had accumulated extractive properties: concentrated authority, degraded oversight, increased friction for resistance. The tangled rope classification captures this hybrid: genuine coordination problem (bottlenecks persist at ε=0.58) plus accumulated extraction (suppression=0.68, theater=0.64). The mandatrophy is not resolved by choosing a single type, but by tracking the temporal drift from rope (ε≈0.22, theater≈0.28) toward snare territory (ε→0.66 trajectory visible if extrapolated). The omega variables (override cost drift, audit feasibility, regulatory binding) determine whether the constraint stabilizes as tangled rope (with regulatory sunset preventing pure snare) or degrades further toward snare (if regulatory constraints fail and override friction continues climbing). The measurement data showing theater_ratio increase above 0.5 indicates Goodhart drift: audit and human review processes are being substituted with proxy metrics (process compliance, appearance of review) rather than genuine oversight — this is the critical lifecycle risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_vs_correlation_threshold,
    'At what threshold of model autonomy does accountability genuinely transfer from human to machine, versus humans remaining responsible for rubber-stamped decisions?',
    'Legal precedent analysis: liability judgments in cases where model decisions caused harm despite human sign-off. Audit trail analysis showing decision-reversal rates and override authority exercise.',
    'If threshold low: autonomy creep is already snare for most humans. If threshold high: humans retain de facto accountability even under autonomy creep, limiting extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_vs_correlation_threshold, conceptual, 'Causation threshold for genuine accountability transfer').

omega_variable(
    override_cost_drift,
    'Does the cost (social, organizational, temporal) of overriding model decisions systematically increase over time as autonomy creeps?',
    'Longitudinal workplace studies tracking override rates, manager friction reports, and career consequences for model disagreement. Cost-accounting of time required to challenge model recommendations.',
    'If cost increases: suppression metric should be higher (≥0.75), moving toward pure snare. If cost remains stable: model autonomy may be negotiated coordination (rope), not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(override_cost_drift, empirical, 'Whether override cost systematically increases with autonomy creep').

omega_variable(
    meaningful_audit_feasibility,
    'Can audit and oversight processes remain genuinely meaningful as model decision complexity exceeds human interpretability?',
    'Empirical auditor studies: can auditors detect model failure modes through log review? Explainability technology benchmarks: do current XAI techniques enable meaningful oversight or theater?',
    'If infeasible: accountability infrastructure becomes pure piton (theater_ratio→0.85+). If feasible with investment: scaffold perspective (regulatory sunset on interpretability requirements) is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_audit_feasibility, empirical, 'Whether meaningful oversight remains feasible at scale').

omega_variable(
    human_discretion_replacement_scope,
    'Does model autonomy creep primarily replace discretionary judgment in low-stakes administrative decisions, or does it extend into consequential decisions with irreversible outcomes?',
    'Organizational audit of model deployment scope: mapping model autonomy by decision stakes, reversibility, and external impact. Stakeholder interviews on scope expansion patterns.',
    'If confined to low-stakes: extraction and suppression metrics should be lower (ε≈0.35, snare classification questionable). If expanding to high-stakes: extraction confirmed, snare classification robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_discretion_replacement_scope, empirical, 'Scope of model autonomy relative to decision stakes').

omega_variable(
    regulatory_constraint_binding,
    'Will regulatory and governance frameworks (algorithmic accountability laws, human-in-the-loop mandates, explainability requirements) establish binding constraints on autonomy creep, or will they become theater themselves?',
    'Regulatory enforcement data: audit frequency and penalty severity for autonomy creep violations. Organizational compliance benchmarking: gap between policy and practice in human oversight.',
    'If binding: scaffold perspective confirmed — regulatory sunset is real enforcement mechanism. If theater: autonomy creep becomes pure snare (suppression→0.85+) with no escape path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_constraint_binding, preference, 'Whether regulatory frameworks will be binding constraints or theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_autonomy_creep, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autonomy_tr_t0, model_autonomy_creep, theater_ratio, 0, 0.28).
narrative_ontology:measurement(autonomy_tr_t3, model_autonomy_creep, theater_ratio, 3, 0.45).
narrative_ontology:measurement(autonomy_tr_t6, model_autonomy_creep, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(autonomy_be_t0, model_autonomy_creep, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(autonomy_be_t3, model_autonomy_creep, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(autonomy_be_t6, model_autonomy_creep, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_autonomy_creep, enforcement_mechanism).
narrative_ontology:affects_constraint(model_autonomy_creep, algorithmic_accountability_deficit).
narrative_ontology:affects_constraint(model_autonomy_creep, organizational_opacity_accumulation).

% DUAL FORMULATION NOTE:
% Model autonomy creep represents a distinct structural constraint from the underlying AI capability growth or the specific models deployed. Two constraint stories in this family: (1) model_autonomy_creep (ε=0.58, Tangled Rope) addresses the organizational extraction mechanism — how authority transfers while accountability persists. (2) algorithmic_accountability_deficit (if decomposed separately) would address the epistemic problem of whether audit is feasible at all, with different ε value reflecting empirical status of explainability rather than organizational extraction. Both are downstream of organizational_opacity_accumulation (the root constraint of whether complex model decisions can be genuinely audited).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_autonomy_creep, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
