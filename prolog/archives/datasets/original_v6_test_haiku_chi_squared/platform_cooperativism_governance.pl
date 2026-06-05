% ============================================================================
% CONSTRAINT STORY: platform_cooperativism_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_cooperativism_governance, []).

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
 *   constraint_id: platform_cooperativism_governance
 *   human_readable: Democratic Worker Governance in Platform Cooperativism
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Platform Cooperativism represents a structural alternative to
 *   venture-capital-dominated digital platforms, where workers collectively
 *   own and govern the platform. However, the constraint reveals a
 *   fundamental tension: democratic governance structures struggle under
 *   informational asymmetry, algorithmic opacity, and scalability pressure.
 *   The constraint exhibits all six DR types depending on the observer's
 *   structural position. From the individual worker's perspective, it appears
 *   as a Snare — nominally owning the platform but algorithmically
 *   controlled. From the worker assembly's perspective, it is a Tangled Rope
 *   — genuine coordination authority undermined by technical expertise
 *   concentration. From the inter-platform cooperative network, it is a
 *   Scaffold — a temporary coordination mechanism being superseded by
 *   technical maturity. From the cooperative movement as institution, it is a
 *   Rope — pure coordination enabling moral positioning against venture
 *   capitalism. From technical specialists, it is a Tangled Rope — they
 *   extract rents through informational asymmetry within the ostensibly
 *   democratic structure. From traditional labor movement institutions, it
 *   appears as a Piton — union-style representation persists despite
 *   structural mismatch to platform dynamics. From the analytical observer,
 *   there is a temptation to see it as a Mountain — coordination barriers
 *   appear intrinsic to large-scale distributed systems — but the structural
 *   data reveals this as a false summit: the opacity and extraction are
 *   contingent, not natural law.
 *
 * KEY AGENTS:
 *   - Individual platform workers: Primary victims (powerless/trapped) — bear extraction through algorithmic governance opacity despite formal cooperative membership
 *   - Worker assemblies/collectives: Primary beneficiaries and secondary victims (moderate/constrained) — control governance authority but face informational asymmetry and decision complexity
 *   - Technical specialists (engineers, data scientists): High-power secondary beneficiary (powerful/mobile) — extract rents through informational asymmetry and scarce expertise
 *   - Cooperative movement institutions (Platform Cooperative Consortium, stewardship bodies): Institutional beneficiary (institutional/arbitrage) — benefit from positioning as moral alternative
 *   - Inter-platform network coordination bodies: Organized intermediary (organized/constrained) — provide shared infrastructure with sunset pathway as technical capacity matures
 *   - Traditional labor movement institutions: Degraded institutional actor (institutional/constrained) — attempt classical labor representation despite structural mismatch (piton perspective)
 *   - Venture-capital competitive ecosystem: Structural pressure actor (institutional/arbitrage) — exerts competitive pressure that suppresses cooperative labor outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_cooperativism_governance, 0.52).
domain_priors:suppression_score(platform_cooperativism_governance, 0.58).
domain_priors:theater_ratio(platform_cooperativism_governance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_cooperativism_governance, extractiveness, 0.52).
narrative_ontology:constraint_metric(platform_cooperativism_governance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(platform_cooperativism_governance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_cooperativism_governance, tangled_rope).
narrative_ontology:human_readable(platform_cooperativism_governance, "Democratic Worker Governance in Platform Cooperativism").
narrative_ontology:topic_domain(platform_cooperativism_governance, "economic/social/technological").

domain_priors:requires_active_enforcement(platform_cooperativism_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, worker_collectives).
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, cooperative_platform_operators).
narrative_ontology:constraint_victim(platform_cooperativism_governance, algorithmic_discretion_constraints).
narrative_ontology:constraint_victim(platform_cooperativism_governance, collective_decision_scalability).
narrative_ontology:constraint_victim(platform_cooperativism_governance, platform_competitiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (SNARE) — Nominally 'owns' the platform through cooperative membership but algorithmic governance creates opacity and structural powerlessness. Democratic decision-making is highly abstract; day-to-day work is controlled by systems workers did not design and cannot exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.81. High extraction despite formal ownership.
constraint_indexing:constraint_classification(platform_cooperativism_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKER ASSEMBLY (TANGLED ROPE) — Collective decision-making authority provides genuine coordination benefit (setting platform terms, work standards, profit distribution). However, coordination is difficult: informational asymmetry favors technical specialists (engineers, data scientists), creating asymmetric extraction within the cooperative. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.49. Moderate extraction from coordination complexity.
constraint_indexing:constraint_classification(platform_cooperativism_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTER-PLATFORM NETWORK (SCAFFOLD) — Multiple cooperatives (Stocksy United, Savvy Cooperative, Fairbnb.coop) coordinate on shared technical infrastructure, governance standards, and market positioning against venture-capital platforms. This coordination has a sunset clause: as maturity increases and technical capacity builds, the dependency on centralized coordination infrastructure decreases. d≈0.45, f(d)≈0.55, σ=1.0 → χ≈0.29. Low-to-moderate extraction; genuine sunset pathway.
constraint_indexing:constraint_classification(platform_cooperativism_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COOPERATIVE MOVEMENT (ROPE) — Non-profit stewardship bodies (Platform Cooperative Consortium, Internet Health Coalition) operate shared technical resources, training, and governance templates. Benefits from positioning as moral alternative to venture-capital platforms. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary through institutional positioning.
constraint_indexing:constraint_classification(platform_cooperativism_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNICAL SPECIALISTS (TANGLED ROPE) — Possess scarce expertise in cooperative governance; can exit to venture-backed platforms. Within cooperatives, they extract rents through informational asymmetry: algorithmic design choices (recommendation systems, task allocation, payment algorithms) are opaque to worker assembly. d≈0.32, f(d)≈0.22, σ=1.2 → χ≈0.14. Low-to-moderate extraction; specialists have structural bargaining power.
constraint_indexing:constraint_classification(platform_cooperativism_governance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR MOVEMENT INSTITUTIONS (PITON) — Unions and labor organizations claim representation of platform cooperatives but have limited technological expertise and cannot enforce classical labor contract frameworks in fluid digital labor markets. Theater ratio=0.65 indicates significant performative content: union-style governance rhetoric persists despite structural mismatch to cooperative-platform dynamics. d≈0.60, f(d)≈0.85, σ=1.1 → χ≈0.47. Moderate extraction through ceremonial representation.
constraint_indexing:constraint_classification(platform_cooperativism_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From civilizational/universal scope, collective decision-making in large-scale distributed systems faces informational limits that appear intrinsic: coordinating thousands of workers across jurisdictions, languages, and time zones with algorithmic governance creates irreducible complexity. However, structural data (ε=0.52, suppression=0.58) contradicts mountain classification — the engine detects a false summit. Coordination barriers are exacerbated by deliberate opacity (technical specialists), not inherent to distributed systems per se.
constraint_indexing:constraint_classification(platform_cooperativism_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_cooperativism_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_cooperativism_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_cooperativism_governance, TR),
    TR >= 0.70.

:- end_tests(platform_cooperativism_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction despite cooperative ownership structure. Sources include: (1) algorithmic discretion exercised by technical specialists, opaque to worker assembly; (2) coordination overhead that consumes resources without producing proportional governance benefit; (3) competitive pressure from VC platforms that suppresses cooperative compensation and work conditions; (4) informational asymmetry that enables technical experts to capture value. The 0.52 value reflects that extraction is real and structural but not maximal — workers retain nominal ownership and some governance authority, differentiating this from pure platform capitalism. Suppression (0.58): Moderate-high. Significant barriers include: (1) technical complexity of algorithmic systems creates informational barriers to genuine worker understanding; (2) scaling challenges of deliberative democracy across distributed global labor pools; (3) capital scarcity that limits cooperative market entry and competition; (4) cultural/linguistic diversity in global platforms complicates shared decision-making; (5) venture-capital competitive pressure that threatens cooperative viability. Theater ratio (0.65): Moderate-high and increasing. The performative content has grown over the interval (2014-2026) as cooperatives have scaled. Early small cooperatives (< 500 workers) had high-fidelity democratic governance. As scale increased and algorithmic complexity grew, the gap between governance rhetoric ('worker-owned and operated') and actual decision-making reality (algorithm-driven with worker assembly ratifying technical specialist choices) widened. Theater increased from 0.40 (2014, small-scale authenticity) to 0.65 (2026, scaled governance theater). Claimed type: Tangled Rope. The constraint has both genuine coordination benefits (workers setting terms, distribution, standards) and asymmetric extraction (via technical specialists, informational barriers, algorithmic opacity). Requires active enforcement (governance bodies must actively maintain democratic processes against natural pressure to concentrate authority).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The individual worker sees Snare — algorithmic systems control their day-to-day work despite cooperative ownership, and they cannot exit without losing livelihood. The worker assembly sees Tangled Rope — they possess real authority but are constantly ambushed by technical complexity and informational asymmetry introduced by specialists. The cooperative network sees Scaffold — multiple cooperatives coordinate on shared technical infrastructure with a clear sunset as technical maturity emerges. The cooperative movement institution sees Rope — they coordinate information and governance templates, enabling cooperatives to exist. Technical specialists see opportunity for Tangled Rope — they can extract rents from their scarcity and opacity. Labor unions see Piton — they maintain representation rituals despite inadequate technical understanding of platform dynamics. The analytical observer at civilizational scale is tempted toward Mountain — treating coordination complexity as intrinsic to distributed systems — but the structural data reveals the gap: extractiveness and suppression are contingent design choices, not natural law. The false summit is a critical diagnostic: it reveals that the observer is naturalizing technical oligarchy as inevitable, when transparency and worker training could shift classification toward Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual worker: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction direction. Worker has ownership stake but no exit, and algorithmic systems constrain their agency daily. Worker assembly: Mixed (beneficiary of governance authority + victim of complexity) + constrained → d≈0.68, f(d)≈1.05. High extraction pressure balanced by genuine coordination authority. Technical specialists: Beneficiary (informational scarcity value) + mobile → d≈0.32, f(d)≈0.22. Low extraction direction due to mobility exit option, but structural position enables extraction through opacity. Cooperative movement: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Negative extraction — net beneficiary from institutional positioning. Inter-platform network: Organized beneficiary + constrained → d≈0.45, f(d)≈0.55. Moderate — organized actors have agency but are constrained by dependency on shared infrastructure. Labor institutions: Victim of technical displacement + constrained → d≈0.60, f(d)≈0.85. Moderate-high extraction through ceremonial representation that cannot enforce outcomes. Venture-capital competitive pressure: Institutional beneficiary (systemically) + arbitrage → d≈0.05, f(d)≈-0.12. Negative from cooperative perspective; the VC ecosystem benefits from cooperative governance complexity that limits their competitive threat.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint remains in unresolved mandatrophy territory (extractiveness 0.52, approaching the 0.70 threshold). The core tension is whether platform cooperativism resolves mandatrophy or merely delays its recognition. OPTIMISTIC PATHWAY: Democratic governance genuinely reduces extraction compared to venture-capital platforms. The 0.52 extractiveness reflects realistic coordination costs, informational barriers solvable through transparency and training. Algorithmic auditing, worker technical education, and clearer decision documentation could shift classification toward Rope (extraction < 0.35), resolving mandatrophy. PESSIMISTIC PATHWAY: Informational aristocracy is inevitable at scale, and the cooperative structure provides only theatrical legitimacy to technocratic governance. If technical specialists cannot or will not provide transparency, and worker assemblies remain systemically disempowered, extractiveness could increase to 0.65-0.75 (approaching Snare). The cooperative would be a Snare disguised as Rope — mandatrophy revealed through sustained theater_ratio growth. RESOLUTION CRITERIA: Mandatrophy would be resolved affirmatively if: (1) worker assembly vote reversals of technical specialist recommendations increase above 15% (showing genuine decision authority), (2) algorithmic decision documentation allows workers to predict outcomes with > 70% accuracy, (3) compensation equity between technical specialists and general workers narrows to < 3x ratio, (4) cooperative market share stabilizes > 5% in competitive sectors, and (5) worker exit rates from cooperatives do not exceed exit rates from comparable VC platforms. Current data (2026) suggests path remains open but theater_ratio growth (0.40 → 0.65) indicates the pessimistic pathway is advancing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_threshold,
    'What level of algorithmic transparency is required for democratic worker governance to be authentic, versus merely performative?',
    'Comparative analysis of platform decisions: worker votes vs algorithm implementation; audit trails showing decision causation; worker ability to predict algorithmic outcomes',
    'If transparency threshold is achievable (< 3 months implementation): tangled_rope classification confirmed; extraction is partially remediable. If threshold is unachievable (> 12 months or technically infeasible): mountain classification suggested; democratic governance in algorithmic systems may be inherently limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_threshold, empirical, 'Feasibility threshold for algorithmic transparency in cooperative governance').

omega_variable(
    scalability_of_collective_decisionmaking,
    'Can deliberative democratic processes for platform governance remain authentic as worker collectives scale from hundreds to hundreds of thousands?',
    'Empirical study of assembly participation rates, decision quality, and worker consensus satisfaction at different scales; comparison of small cooperatives (< 500 workers) vs large ones (> 5000)',
    'If authentic participation holds at scale: coordination barriers are manageable (Rope/Scaffold). If participation degrades dramatically: latent informational aristocracy emerges (Snare/Piton), suggesting cooperative governance structure is theater above oligarchic reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scalability_of_collective_decisionmaking, empirical, 'Whether democratic decision-making remains authentic at cooperative scale').

omega_variable(
    competitive_sustainability_vs_democratic_overhead,
    'Can platform cooperatives with full democratic governance compete economically against venture-backed platforms with centralized decision-making?',
    'Market share analysis over 10-year windows; cost comparisons (overhead of governance vs. VC-platform decision velocity); profitability and user retention metrics',
    'If cooperatives achieve competitiveness: democratic governance is sustainable (Tangled Rope with solvable extraction). If cooperatives systematically underperform: democratic overhead is structurally extractive (Snare with extraction via market pressure), suggesting the constraint is economic Darwinism, not cooperative governance failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_sustainability_vs_democratic_overhead, empirical, 'Economic competitiveness of democratically-governed platforms').

omega_variable(
    informational_aristocracy_emergence,
    'Does technical expertise inevitably create an informational aristocracy within worker cooperatives that undermines democratic principle?',
    'Role diversity analysis: percentage of technical specialists in decision-making bodies; comparison of algorithmic decisions made vs. worker assembly decisions; turnover rates for technical vs. non-technical roles',
    'If aristocracy is inevitable: the constraint cannot be resolved and is intrinsic to cooperative governance (Mountain). If aristocracy emerges from design choices (transparency, training accessibility): extraction is remediable through structural reform (Tangled Rope to Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informational_aristocracy_emergence, conceptual, 'Whether technical expertise creates inevitable informational hierarchy').

omega_variable(
    venture_capital_exit_temptation,
    'How strong is the structural pressure for successful platform cooperatives to convert to venture-backed ownership?',
    'Historical analysis of cooperative conversions; founder interviews on capital access pressures; comparison of growth rates: pure cooperatives vs. those accepting external capital',
    'If conversion pressure is high: cooperative governance is suppressed by capital scarcity (Snare with economic coercion). If cooperatives can raise capital while maintaining governance: suppression barrier is lower and cooperative governance becomes Tangled Rope or Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(venture_capital_exit_temptation, empirical, 'Structural incentives for cooperatives to accept venture capital').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_cooperativism_governance, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcoop_tr_t0, platform_cooperativism_governance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pcoop_tr_t6, platform_cooperativism_governance, theater_ratio, 6, 0.53).
narrative_ontology:measurement(pcoop_tr_t12, platform_cooperativism_governance, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(pcoop_be_t0, platform_cooperativism_governance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pcoop_be_t6, platform_cooperativism_governance, base_extractiveness, 6, 0.41).
narrative_ontology:measurement(pcoop_be_t12, platform_cooperativism_governance, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_cooperativism_governance, resource_allocation).
narrative_ontology:affects_constraint(platform_cooperativism_governance, algorithmic_accountability_architecture).
narrative_ontology:affects_constraint(platform_cooperativism_governance, platform_capital_governance).
narrative_ontology:affects_constraint(platform_cooperativism_governance, distributed_deliberation_scalability).

% DUAL FORMULATION NOTE:
% Platform Cooperativism decomposes into three structurally distinct constraints: (1) algorithmic_accountability_architecture (ε≈0.35) — technical systems for transparency; (2) platform_capital_governance (ε≈0.50) — ownership and profit distribution mechanisms; (3) distributed_deliberation_scalability (ε≈0.48) — democratic decision-making across distributed global labor pools. This story focuses on the integrative governance constraint (0.52) that spans all three. Each downstream constraint has its own ε and should be analyzed separately; this story links them as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_cooperativism_governance, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
