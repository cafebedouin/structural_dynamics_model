% ============================================================================
% CONSTRAINT STORY: organizational_fragility_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_fragility_cascade, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: organizational_fragility_cascade
 *   human_readable: Organizational Fragility Cascade: The Extraction of Resilience Through Specialization
 *   domain: organizational_structure/institutional_design
 *
 * SUMMARY:
 *   Organizations pursuing maximum efficiency often adopt highly specialized,
 *   tightly-coupled structures that optimize short-term performance metrics
 *   while systematically eroding their capacity to respond to unexpected
 *   disruptions. The organizational fragility cascade is the structural
 *   constraint that extracts resilience — adaptive capacity, slack resources,
 *   cross-functional knowledge, and redundancy — in exchange for measurable
 *   efficiency gains. This extraction is enforced through performance
 *   metrics, career incentives, and competitive pressure, and it is obscured
 *   by narratives that treat specialization as a natural law of
 *   organizational scale rather than a design choice. The constraint exhibits
 *   the six DR types from different positions: pure extraction (snare) to
 *   frontline workers deskilled and trapped in narrow roles; mixed
 *   coordination and extraction (tangled rope) to mid-level managers who
 *   benefit from clear objectives but are punished for investing in
 *   resilience; pure coordination (rope) to efficiency optimizers who see
 *   specialization as beneficial role separation; temporary problem with
 *   sunset (scaffold) to organized resilience advocates who see structural
 *   change emerging; degraded theater (piton) to legacy compliance
 *   architecture that documents risks without authority to address them; and
 *   potential false summit (mountain) to analytical observers who naturalize
 *   specialization as an inherent law of complexity.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victims (powerless/trapped) — deskilled through narrow specialization, no exit paths, bear full cost of system fragility
 *   - Mid-Level Managers: Secondary victims (moderate/constrained) — pressured by efficiency metrics, unable to invest in resilience or cross-training, blamed when fragility cascades
 *   - Efficiency Optimizers: Primary beneficiaries (institutional/arbitrage) — benefit from specialized role concentration, resource aggregation, competitive advantage; can move to other organizations
 *   - Resilience Coalition: Organized agents (organized/constrained) — unions, worker councils, safety organizations, ecosystem partners advocating for redundancy and distributed decision-making
 *   - Risk and Compliance Functions: Institutional theater (institutional/mobile) — maintain legitimacy through oversight structures without functional power to halt efficiency drives
 *   - Organizational Leadership: Secondary beneficiaries (powerful/arbitrage) — achieve short-term performance metrics that boost valuations, compensation, and career opportunities; externalize long-term risks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_fragility_cascade, 0.58).
domain_priors:suppression_score(organizational_fragility_cascade, 0.68).
domain_priors:theater_ratio(organizational_fragility_cascade, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_fragility_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_fragility_cascade, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(organizational_fragility_cascade, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_fragility_cascade, tangled_rope).
narrative_ontology:human_readable(organizational_fragility_cascade, "Organizational Fragility Cascade: The Extraction of Resilience Through Specialization").
narrative_ontology:topic_domain(organizational_fragility_cascade, "organizational_structure/institutional_design").

domain_priors:requires_active_enforcement(organizational_fragility_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_fragility_cascade, efficiency_optimizers).
narrative_ontology:constraint_beneficiary(organizational_fragility_cascade, concentrated_leadership).
narrative_ontology:constraint_victim(organizational_fragility_cascade, frontline_workers).
narrative_ontology:constraint_victim(organizational_fragility_cascade, organizational_adaptive_capacity).
narrative_ontology:constraint_victim(organizational_fragility_cascade, ecosystem_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped in narrowly specialized roles designed for maximum efficiency. No exit path without retraining cost, job loss, or relocation. Bears full extraction through deskilling, wage suppression, and vulnerability to disruption. Cannot see or influence the fragility-generating architecture. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(organizational_fragility_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by performance metrics tied to efficiency targets and quarterly results. Experiences genuine coordination benefit (clear objectives, resource allocation) alongside extraction (impossible workloads, blame for system failures, inability to invest in redundancy or training). Some agency through network effects and peer solidarity, but career incentives punish resilience investments.
constraint_indexing:constraint_classification(organizational_fragility_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EFFICIENCY OPTIMIZERS (ROPE) — Specialized functions (operations, finance, strategy) that benefit from fragmented structure through competitive advantage and resource concentration. Experiences the cascade as a coordination mechanism: clear role separation enables their optimization work. Exit arbitrage available — can move to other institutions or consulting roles. Net beneficiary.
constraint_indexing:constraint_classification(organizational_fragility_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESILIENCE COALITION (SCAFFOLD) — Organized agents (worker councils, union representatives, safety organizations, ecosystem partners) see the fragility cascade as a temporary institutional failure. They advocate for redundancy, cross-training, supply chain diversification, and distributed decision-making. Their sunset logic: as disruptions accumulate and costs become visible, the organization will adopt resilience investments. Low effective extraction because the coalition has agency and sees structural change happening.
constraint_indexing:constraint_classification(organizational_fragility_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY COMPLIANCE ARCHITECTURE (PITON) — Formal governance structures, organizational charts, and oversight functions persist largely as theater. Risk committees meet to discuss supply chain resilience but cannot override efficiency imperatives. Audit functions generate reports that document fragility without authority to halt it. These structures provide legitimacy (the organization appears to manage risk) but lack functional power. Theater ratio dominates because the architecture is maintained through institutional inertia rather than effectiveness.
constraint_indexing:constraint_classification(organizational_fragility_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, specialization and division of labor are inherent to complex organizations — they are fundamental principles of coordination that scale. This perspective sees organizational fragility as an unavoidable trade-off built into the nature of specialization itself. However, the structural data reveals this as a false summit: fragility is not inherent but designed through choices about how tightly to specialize, whether to maintain slack capacity, and how much cross-training to require. The naturalizing frame obscures contingent institutional design decisions.
constraint_indexing:constraint_classification(organizational_fragility_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_fragility_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_fragility_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_fragility_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_fragility_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_fragility_cascade, TR),
    TR >= 0.70.

:- end_tests(organizational_fragility_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The organization systematically extracts resilience through specialization architecture. This is not total extraction (snare level 0.70+) because genuine coordination benefits exist — clear role definition and resource specialization do enable complex work. But the extraction portion is substantial: workers lose adaptive capacity, the organization loses shock-absorption capability, and ecosystems lose redundancy. The metric reflects that the efficiency gains are real but come at a cost that is systematically underpriced. Suppression (0.68): High. The extraction mechanism is protected by multiple layers of opacity: (1) short-term performance metrics hide long-term fragility; (2) risk committees exist but cannot override efficiency directives; (3) fragility remains invisible until disruption cascades; (4) workers lack information about system architecture and alternative designs; (5) competitive pressure makes unilateral resilience investment appear irrational. Theater ratio (0.64): Moderate-high. Governance and compliance structures exist (risk committees, audits, oversight boards) but are substantially performative — they generate reports documenting fragility without authority to halt it. Some theater is increasing: as awareness of fragility grows, organizations add more compliance theater rather than changing architecture.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the efficiency optimizer's rope and the frontline worker's snare is maximal. Both are experiencing the same specialization architecture, but one benefits and one is trapped. This gap reveals that the 'coordination' label is incomplete — it obscures extraction. A true coordination mechanism would produce similar classifications across perspectives. When classifications diverge sharply (rope vs snare), the constraint is extractive dressed in coordination language. The tangled rope classification for managers is the bridge: they see both the coordination and the extraction, but performance metrics suppress their ability to address it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain flows from beneficiary to victim. Efficiency optimizers have arbitrage options (can move to other organizations) and benefit from specialization, so d ≈ 0.10-0.20 (low extraction experienced). Frontline workers have trapped options (specialized skills not transferable, no market for their deskilled labor) and lose adaptive capacity, so d ≈ 0.88-0.95 (high extraction experienced). The sigmoid f(d) amplifies this gap: low d (beneficiaries) → f(d) near zero (no experienced extraction); high d (victims) → f(d) near 1.4 (maximum extraction). This mathematical structure is why the tangled rope classification for managers is important — they experience the midpoint, and their constrained exit options (d ≈ 0.55) produce moderate experienced chi. If all perspectives produced rope, the constraint would appear to be pure coordination. The snare perspective from victims is diagnostic: it reveals what extraction looks like from the position being extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   FRAGILITY CASCADE RESOLVES MANDATROPHY: The constraint avoids the mandatrophy by demonstrating genuine coordination function (specialization does enable complex work) alongside asymmetric extraction (some agents lose resilience and adaptive capacity). The classification is not 'is this coordination or extraction?' but 'how much of each, and who experiences which?' The efficiency optimizer sees rope (coordination benefits, arbitrage options, no extraction experienced). The frontline worker sees snare (extraction, trapped options, no coordination benefit to them). The analytical observer risks natural-law framing ('specialization is inherent to scale') but the structural data reveals the false summit: specialization degree is chosen, not determined by physics or logic. Organizations could choose less extreme specialization (accept lower efficiency gains to maintain resilience), but competitive pressure and short-term metrics make this choice difficult. The constraint's continued existence depends on suppressing the cost-visibility and naturalizing the design choice as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slack_capacity_necessity,
    'What level of excess capacity is genuinely required for organizational resilience versus unnecessary cost?',
    'Comparative analysis of disruption recovery times across organizations with different slack levels; cost-benefit modeling of preventive redundancy vs post-disruption recovery costs',
    'If slack threshold < 15% of operating costs: most current optimization is extractive (too tight). If threshold > 30%: some efficiency gains are genuine coordination improvements. Classification moves from snare toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slack_capacity_necessity, empirical, 'Threshold between necessary slack and extractive overcapacity').

omega_variable(
    fragility_visibility_timing,
    'At what scale of disruption does organizational fragility become visible to leadership who designed the efficiency architecture?',
    'Historical analysis of crisis response patterns; correlation between disruption severity and leadership acknowledgment of fragility; measurement of lag time between first crisis signals and formal organizational response',
    'If visibility threshold requires catastrophic failure: suppression is extremely high (design maintains ignorance). If visible at moderate disruptions: suppression is lower (fragility can be addressed before full cascade). Affects suppression metric directly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fragility_visibility_timing, empirical, 'When organizational leaders recognize fragility they designed').

omega_variable(
    cross_training_cost_attribution,
    'Does the organization attribute cross-training costs to resilience investments or to efficiency loss?',
    'Organizational accounting analysis; interviews with finance and HR leadership on how cross-training is budgeted; whether it appears as operational cost or capability investment; correlation with performance metrics used to evaluate managers',
    'If attributed to efficiency loss: creates strong disincentive (managers avoid cross-training to protect metrics). If attributed to resilience: enables mid-level managers to invest. Affects whether tangled_rope classification holds or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_training_cost_attribution, conceptual, 'Accounting frame applied to resilience investments').

omega_variable(
    ecosystem_interdependence_coupling,
    'Does the organization''s fragility cascade to dependent ecosystems (suppliers, customers, partners) or remain contained?',
    'Supply chain network analysis; tracking of disruption propagation speed and reach; measurement of how many external actors are affected by single-point failures in the organization',
    'If cascade is contained: extractiveness applies to internal agents only. If cascade propagates widely: extractiveness is higher (broader victim set) and spatial scope is larger. May shift from tangled_rope toward snare at regional/national scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecosystem_interdependence_coupling, empirical, 'Whether organizational fragility cascades to external ecosystems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_fragility_cascade, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgfrag_tr_t0, organizational_fragility_cascade, theater_ratio, 0, 0.35).
narrative_ontology:measurement(orgfrag_tr_t3, organizational_fragility_cascade, theater_ratio, 3, 0.48).
narrative_ontology:measurement(orgfrag_tr_t6, organizational_fragility_cascade, theater_ratio, 6, 0.61).
narrative_ontology:measurement(orgfrag_tr_t10, organizational_fragility_cascade, theater_ratio, 10, 0.7).

% Extraction over time
narrative_ontology:measurement(orgfrag_be_t0, organizational_fragility_cascade, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orgfrag_be_t3, organizational_fragility_cascade, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(orgfrag_be_t6, organizational_fragility_cascade, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(orgfrag_be_t10, organizational_fragility_cascade, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_fragility_cascade, resource_allocation).
narrative_ontology:boltzmann_floor_override(organizational_fragility_cascade, 0.18).
narrative_ontology:affects_constraint(organizational_fragility_cascade, supply_chain_single_point_failure).
narrative_ontology:affects_constraint(organizational_fragility_cascade, knowledge_loss_on_employee_departure).
narrative_ontology:affects_constraint(organizational_fragility_cascade, performance_metric_goodhart_drift).

% DUAL FORMULATION NOTE:
% The organizational fragility cascade is upstream of specific failure modes (supply chain vulnerability, knowledge loss, metric gaming). Each downstream constraint has its own extractiveness value; the fragility cascade is the structural mechanism that enables and conceals those constraints. Decompose when analyzing specific failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_fragility_cascade, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
