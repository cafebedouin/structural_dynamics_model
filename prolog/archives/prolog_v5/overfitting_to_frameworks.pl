% ============================================================================
% CONSTRAINT STORY: overfitting_to_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overfitting_to_frameworks, []).

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
 *   constraint_id: overfitting_to_frameworks
 *   human_readable: The Rigidity of the Map — Overfitting to Evaluative Frameworks
 *   domain: technological/cognitive/organizational
 *
 * SUMMARY:
 *   The rigidity of the map — overfitting to evaluative frameworks —
 *   represents a constraint where an organization's optimization toward
 *   measurable objectives creates perceptual and adaptive rigidity. This is
 *   distinct from simple misalignment: the organization is optimizing
 *   correctly according to the framework, but the framework itself has
 *   diverged from the territory it claims to model. The constraint exhibits
 *   tension between the genuine coordination benefits of shared metrics and
 *   the severe cost of losing the capacity to perceive and respond to
 *   out-of-distribution signals. The organization becomes vulnerable to
 *   discontinuous changes, black swan events, and gradual environmental drift
 *   that the framework does not capture. Theater ratio increases over the
 *   interval because the evaluative apparatus becomes increasingly
 *   performative — more resources devoted to gaming metrics and producing
 *   audit-ready documentation rather than to genuine adaptation. The
 *   constraint is downstream of framework design choices but represents a
 *   distinct structural problem: even well-intentioned metrics create rigid
 *   incentive structures that suppress alternative information channels.
 *
 * KEY AGENTS:
 *   - Framework Designers: Primary beneficiaries (institutional/arbitrage) — retain flexibility to revise or switch frameworks; gain institutional prestige and influence from framework adoption; high exit options
 *   - Trapped Organization: Primary victim (powerless/trapped) — optimizes all internal systems to the metric; loses ability to perceive signals outside the metric; unable to exit without organizational restructuring
 *   - Individual Agents Within System: Secondary victim/mixed (moderate/constrained) — benefit from clarity and coordination provided by metric; suffer extraction of adaptive and perceptual capacity; face career risk from deviating from metric
 *   - Metric Auditors and Compliance Apparatus: Institutional actor (institutional/arbitrage) — maintain performative verification mechanisms; see own processes as degraded; benefit from framework enforcement without functional necessity
 *   - Adaptive Learning Coalition: Organized response (organized/constrained) — building multi-metric, feedback-responsive alternatives; have some enforcement power but face institutional inertia; clear sunset trajectory
 *   - Competing Standard-Setters: Inter-institutional actors (organized/constrained) — each framework solves real coordination problems but creates cumulative overfitting; both beneficiaries and extractors depending on perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overfitting_to_frameworks, 0.52).
domain_priors:suppression_score(overfitting_to_frameworks, 0.58).
domain_priors:theater_ratio(overfitting_to_frameworks, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overfitting_to_frameworks, extractiveness, 0.52).
narrative_ontology:constraint_metric(overfitting_to_frameworks, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(overfitting_to_frameworks, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overfitting_to_frameworks, tangled_rope).
narrative_ontology:human_readable(overfitting_to_frameworks, "The Rigidity of the Map — Overfitting to Evaluative Frameworks").
narrative_ontology:topic_domain(overfitting_to_frameworks, "technological/cognitive/organizational").

domain_priors:requires_active_enforcement(overfitting_to_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, framework_designers).
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, metric_arbiters).
narrative_ontology:constraint_victim(overfitting_to_frameworks, adaptive_capacity).
narrative_ontology:constraint_victim(overfitting_to_frameworks, perceptual_alignment_to_reality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED ORGANIZATION (SNARE) — An organization that has optimized all internal incentives, resource allocation, and decision-making processes to a single metric (performance targets, compliance scores, optimization objectives) becomes unable to perceive or respond to signals outside that metric. The organization cannot exit without destroying its internal coordination. Maximum extraction of adaptability in exchange for short-term measurability. The constraint appears as a cage of its own making, but the organization has no mechanism to break it without organizational dissolution.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL AGENT (TANGLED ROPE) — Mid-level managers, engineers, and knowledge workers within the overfitted organization experience both coordination and extraction. The framework provides genuine coordination function: it clarifies expectations, enables resource prioritization, and creates predictable incentive structures. But it also extracts their perceptual capacity — agents spend cognitive effort gaming the metric rather than sensing the environment. The agent has some agency to escalate concerns or seek alternative roles, but faces career risk and social pressure. Mixed experience with partial alternatives available.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRAMEWORK DESIGNER (ROPE) — The institution or team that designs the evaluative framework (metrics, objectives, compliance standards) experiences this constraint primarily as coordination. They see themselves as solving a genuine coordination problem: how to align many agents toward coherent goals in the absence of perfect communication. The framework has real value as a simplifying device. The designers have high exit options — they can revise metrics, create new frameworks, or shift focus. Net beneficiary of the constraint's coordination function with minimal extraction. Arbitrage access to alternative framings.
constraint_indexing:constraint_classification(overfitting_to_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ADAPTIVE LEARNING COALITION (SCAFFOLD) — Organized communities (complexity science researchers, organizational learning theorists, continuous improvement practitioners, agile methodologists) see the overfitting constraint as a solvable problem with structural sunset. Multi-metric dashboards, feedback loops that detect metric gaming, adaptive performance management systems, and real-time environmental scanning protocols are building alternatives that preserve coordination benefits while reducing overfitting risk. The coalition has some enforcement power (professional standards, certification bodies) but cannot unilaterally override existing frameworks. Significant suppression from institutional inertia, but clear exit pathway and sunset clause as alternatives mature (10-15 year horizon).
constraint_indexing:constraint_classification(overfitting_to_frameworks, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: AUDIT AND COMPLIANCE APPARATUS (PITON) — The institutional machinery of audits, inspections, compliance reviews, and third-party certifications that enforce adherence to frameworks has become largely performative. Many organizations maintain extensive audit protocols and compliance documentation not because the audits reliably detect or prevent overfitting, but because institutional norms require them. The theater ratio is high — significant organizational effort devoted to producing audit-ready artifacts rather than genuine adaptation. The apparatus sees its own decline: auditors recognize they cannot detect overfitting from static documentation. Maintained through institutional inertia and regulatory requirement, not functional necessity.
constraint_indexing:constraint_classification(overfitting_to_frameworks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING STANDARD-SETTERS (TANGLED ROPE) — Different institutional communities (accounting standards, quality management systems, environmental compliance frameworks, AI safety evaluations) each designed their own evaluation frameworks to address real coordination problems. But these frameworks often conflict or overlap, forcing organizations to optimize for multiple incommensurable metrics. Each standard-setter benefits from their framework's adoption (institutional prestige, funding, influence) while the organizations bearing the compliance burden experience extraction. The standard-setter community has exit options (revise standards, align with competitors) but faces coordination barriers and competitive pressure. Both coordination function and asymmetric extraction present.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, some degree of metric overfitting may be inherent to human coordination: any explicit system for aligning distributed agents requires compression of multi-dimensional reality into measurable observables, creating inevitable gaps between metric and territory. The frame-reality gap is a structural feature of communication across asymmetric information. However, the base properties contradict full mountain classification — the suppression (0.58) and extractiveness (0.52) are too high for an immutable natural law. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(overfitting_to_frameworks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfitting_to_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overfitting_to_frameworks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overfitting_to_frameworks, TR),
    TR >= 0.70.

:- end_tests(overfitting_to_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts the organization's adaptive capacity and perceptual alignment to reality in exchange for coordination clarity. The extraction is not total because organizations retain underlying adaptive machinery (measurement shows trajectory from 0.28 to 0.52 over the interval); they are suppressed, not eliminated. The metric provides genuine coordination value, so the extraction is asymmetric but not predatory in origin. Suppression (0.58): Moderate-high. Significant barriers prevent exit from the framework: institutional momentum, stakeholder expectations aligned to metrics, career structures that reward metric optimization, regulatory requirements, and the cognitive difficulty of identifying metric-reality misalignment from within the system. However, some organizations do escape (via acquisition, management change, strategic pivot) so suppression is not total. Theater ratio (0.68): High and increasing. As organizations optimize more tightly to metrics, the proportion of their activities devoted to producing metric-compliant outputs increases relative to genuine adaptation. Audit documentation, compliance reviews, performance reporting, and metric production become end-goals rather than means. The theater escalates because the metric becomes more entrenched, creating self-reinforcing performativity.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap runs between the framework designer (who sees coordination and genuine value) and the trapped organization (which sees cage and perceptual loss). The designer experiences Rope — solving a real alignment problem. The organization experiences Snare — losing the capacity to respond to external change. The individual agent within the system experiences Tangled Rope — both benefit from clarity and penalty from suppression. The audit apparatus experiences Piton — its enforcement mechanisms are performative and degraded. The adaptive learning coalition experiences Scaffold — alternatives are emerging with clear sunset trajectories. These gaps are not measurement artifacts; they are structural positions that generate genuinely different classifications from identical base properties. The engine's perspectival decomposition is essential here.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values differ sharply by agent type. Framework designers and metric arbiters have low d (beneficiary status + arbitrage exit options) — they derive value from the constraint's coordination function and can easily shift to new frameworks. Trapped organizations have high d (victim status + trapped exit options) — they cannot exit without severe cost and bear the full adaptive penalty. Individual agents have moderate d (mixed status + constrained exit options) — they benefit from coordination clarity but suffer suppression, with limited but non-zero alternatives. The adaptive learning coalition has moderate-to-low d (organizing capacity provides agency, exit pathways visible) — they see the constraint as soluble rather than structural. The derivation chain correctly produces: beneficiaries → low/negative effective extraction; trapped agents → high effective extraction; moderate agents with constrained exit → medium effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the mandatrophy (false positive snare from all perspectives) by showing that the coordination function is genuine and necessary. The constraint is NOT pure extraction masquerading as coordination. The metrics DO solve real problems of distributed alignment and resource prioritization. The tangled rope and rope classifications are structurally correct from beneficiary and designer perspectives. What the constraint demonstrates instead is that good coordination mechanisms can become adaptive traps when optimization is too tight or feedback loops are too slow. The constraint resolves the mandatrophy by being honest about who experiences which function: designers see rope (real coordination), organizations see snare (real extraction), and the analytical observer might try to see mountain (natural law of coordination) but the high suppression and theater ratio prevent that false summit. The resolution is perspectival honesty, not denial of coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_granularity_threshold,
    'What level of metric granularity and feedback frequency prevents overfitting while preserving coordination function?',
    'Comparative analysis of organizations with different metric refresh rates (quarterly vs real-time), dimensionality (single KPI vs multi-dimensional balanced scorecard), and feedback loops (delayed vs immediate). Measurement of adaptation speed and sensitivity to out-of-distribution signals.',
    'If fine-grained multi-dimensional metrics with rapid feedback suffice: overfitting is suppression-reducible (Rope from more perspectives). If even fine-grained metrics cannot prevent overfitting: the constraint is closer to snare (unavoidable trade-off between coordination and adaptation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_granularity_threshold, empirical, 'Metric granularity and feedback frequency required to prevent overfitting').

omega_variable(
    framework_designer_alignment,
    'Do framework designers themselves fall victim to overfitting, designing metrics optimized for measurability rather than to actual organizational outcomes?',
    'Analysis of framework evolution: do metric designers update their frameworks when organizational feedback indicates metric-outcome misalignment? How often are frameworks abandoned vs iteratively refined? Does the designer community show adaptive learning or institutional path-dependence?',
    'If designers update frameworks appropriately: they have genuine exit options and see the constraint as solvable (beneficiary perspective valid). If designers also overfit: the constraint is recursive and deeper — even the coordination function is compromised. Shifts the classification from tangled_rope toward snare at multiple levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framework_designer_alignment, empirical, 'Whether framework designers themselves overfit to metrics').

omega_variable(
    reality_perception_recovery_time,
    'How long does it take for an organization to recover perceptual alignment with reality after the evaluative framework is removed or fundamentally revised?',
    'Case studies of major organizational framework transitions (post-acquisition integration, strategic pivots, management changes introducing new evaluation systems). Measurement of time to re-establish environmental sensing, anomaly detection, and adaptive response.',
    'If recovery is rapid (weeks to months): organizational perceptual capacity is retained but suppressed — underlying adaptive mechanisms remain functional (snare classification appropriate). If recovery is slow (years to decades): perceptual machinery has atrophied — the constraint has caused structural damage. Shifts toward irreversible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reality_perception_recovery_time, empirical, 'Recovery time for organizational perception after framework removal').

omega_variable(
    multi_framework_compatibility,
    'Can organizations maintain functional optimization to multiple frameworks simultaneously, or does adding competing frameworks increase total overfitting?',
    'Studies of organizations required to meet multiple compliance standards, reporting regimes, or evaluation systems (multinational corporations, regulated industries, non-profits with diverse funders). Measurement of organizational agility, resource overhead, and response time to market/environmental changes.',
    'If multi-framework optimization is feasible: the constraint is bounded (medium suppression, moderate extraction). If competing frameworks amplify overfitting: the system exhibits catastrophic interference — organizations lose coherence entirely. Shifts from tangled_rope toward snare at system level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multi_framework_compatibility, empirical, 'Feasibility of optimizing to multiple frameworks simultaneously').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overfitting_to_frameworks, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(overfit_tr_t0, overfitting_to_frameworks, theater_ratio, 0, 0.38).
narrative_ontology:measurement(overfit_tr_t5, overfitting_to_frameworks, theater_ratio, 5, 0.55).
narrative_ontology:measurement(overfit_tr_t10, overfitting_to_frameworks, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(overfit_be_t0, overfitting_to_frameworks, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(overfit_be_t5, overfitting_to_frameworks, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(overfit_be_t10, overfitting_to_frameworks, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overfitting_to_frameworks, resource_allocation).
narrative_ontology:affects_constraint(overfitting_to_frameworks, goodhart_law_metric_erosion).
narrative_ontology:affects_constraint(overfitting_to_frameworks, institutional_path_dependence).

% DUAL FORMULATION NOTE:
% Overfitting to frameworks is downstream of Goodhart's Law (when a measure becomes a target, it ceases to be a good measure) and institutional path dependence (how organizations become locked into evaluative systems). The overfitting constraint represents the organizational-level manifestation of Goodhart degradation. As metrics become targets, the organization's adaptability (perceptual and behavioral) becomes the extracted resource. This is a distinct constraint from the mathematical principle itself — it focuses on the organizational rigidity and loss of adaptive capacity, not merely metric corruption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overfitting_to_frameworks, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
