% ============================================================================
% CONSTRAINT STORY: goodharts_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law, []).

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
 *   constraint_id: goodharts_law
 *   human_readable: Goodhart's Law: Metric Gaming and Systemic Distortion
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Goodhart's Law describes a structural phenomenon where the act of
 *   measurement under optimization pressure causes the metric to diverge from
 *   the intended outcome. This constraint operates across economic, social,
 *   and technological domains with remarkable consistency: student test
 *   scores diverge from learning, healthcare metrics diverge from patient
 *   outcomes, software velocity diverges from product value, published
 *   research metrics diverge from scientific progress, crime statistics
 *   diverge from actual criminality. The constraint exhibits a characteristic
 *   evolution: the metric initially functions well (low extractiveness, low
 *   theater), then gaming emerges as agents recognize the reward structure
 *   (extractiveness increases), then the metric becomes performative theater
 *   as everyone acknowledges the distortion but continues optimizing for it
 *   (theater_ratio increases). The structural tension is between the
 *   coordination function of metrics (enabling delegation, accountability,
 *   measurement) and the extraction function (rewarding gaming, punishing
 *   truth-telling when truth conflicts with metric optimization). Goodhart's
 *   Law is not a natural law of physics but a structural property of
 *   measurement systems under adversarial optimization pressure.
 *
 * KEY AGENTS:
 *   - Metric Optimizers: Gaming agents (institutional/arbitrage) — benefit from metric-outcome divergence during the gaming window before meta-governance response
 *   - System Designers / Policy Authority: Institutional actors (institutional/arbitrage) — benefit from metric's coordination function even as it distorts; immediate time horizon creates gaming incentive asymmetry
 *   - Intended Outcome Bearers: Powerless agents (powerless/trapped) — targeted by policy optimized for distorted metric; no exit option; bear full cost of metric-outcome divergence
 *   - Meta-Governance Coalition: Organized actors (organized/mobile) — auditors, independent researchers, alternative certification bodies; build exit pathways through shadow metrics and outcome triangulation
 *   - System Integrity: Abstract victim (collective) — ecosystem coherence damaged by metric distortion; no agent advocates; cannot organize
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes Goodhart's Law as structural inevitability of measurement-under-pressure, not moral failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law, 0.58).
domain_priors:suppression_score(goodharts_law, 0.62).
domain_priors:theater_ratio(goodharts_law, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodharts_law, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(goodharts_law, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law, tangled_rope).
narrative_ontology:human_readable(goodharts_law, "Goodhart's Law: Metric Gaming and Systemic Distortion").
narrative_ontology:topic_domain(goodharts_law, "economic/social/technological").

domain_priors:requires_active_enforcement(goodharts_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law, metric_optimizers).
narrative_ontology:constraint_beneficiary(goodharts_law, gaming_agents).
narrative_ontology:constraint_victim(goodharts_law, intended_outcome_bearers).
narrative_ontology:constraint_victim(goodharts_law, system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM USER / INTENDED BENEFICIARY (SNARE) — Individuals or populations targeted by policy optimized for distorted metrics (e.g., students in schools optimizing for test scores rather than learning, patients in healthcare systems optimizing for throughput metrics). Bears full cost of metric-outcome divergence with no exit option. Maximum extraction.
constraint_indexing:constraint_classification(goodharts_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: METRIC-CONSCIOUS AGENT (TANGLED ROPE) — Agents who recognize the metric-gaming opportunity and partially exploit it, but also depend on system function. Constrained by reputation risk, regulatory oversight, and long-term reliance on system credibility. Experience both extraction (gaming benefit) and coordination (system function needed for other purposes).
constraint_indexing:constraint_classification(goodharts_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC DESIGNER / POLICY AUTHORITY (ROPE) — The institution implementing the metric experiences it as coordination: transparent measurement enables delegation and performance management. Short time horizon (immediate quarterly/annual reviews). Arbitrage options (can switch metrics, adjust targets, exit to other governance mechanisms). Net beneficiary from information aggregation even if metric becomes distorted.
constraint_indexing:constraint_classification(goodharts_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: META-GOVERNANCE COALITION (SCAFFOLD) — Organized actors (auditors, independent researchers, reform advocates, alternative certification bodies) recognize the metric collapse and deploy counter-measures: shadow metrics, triangulation, randomized audits, outcome tracking independent of official metrics. These provide exit pathways as they scale. Sunset clause implicit: as alternative measurement systems mature, Goodhart's Law's extraction mechanism weakens.
constraint_indexing:constraint_classification(goodharts_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: METRIC AS RITUAL / INSTITUTIONAL THEATER (PITON) — From the civilizational perspective, metrics persist as performative signaling long after their informational collapse. Gaming is acknowledged, workarounds are institutionalized, yet the metric remains in place. Theater ratio is high because the metric continues to be published, defended, and acted upon despite universal recognition of its distortion. The constraint persists through institutional inertia.
constraint_indexing:constraint_classification(goodharts_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Goodhart's Law is not a natural law but a structural feature of measurement under optimization pressure. The analytical perspective recognizes both the coordination function (metrics enable delegation and accountability) and the extraction mechanism (gaming extracts value from intended beneficiaries). The constraint persists because both functions are structurally necessary. Not a false summit, but a genuine hybrid that cannot be resolved to pure coordination or pure extraction.
constraint_indexing:constraint_classification(goodharts_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Goodhart's Law creates a systematic transfer of value from intended outcome bearers to metric optimizers during the gaming window. The extraction is not total (like a snare) because the metric does provide legitimate coordination value and gaming is eventually checked by meta-governance. However, the extraction is substantial and systematic — agents who recognize the metric-outcome divergence exploit it, while system designers face a principal-agent problem they cannot fully solve. Suppression (0.62): Moderate-high. Agents have suppressed information: (1) the true outcome is hard/expensive to measure, so proxies are necessary; (2) once a metric is chosen, gaming opportunities are often invisible to non-specialists; (3) acknowledging metric distortion requires admitting policy failure, creating institutional pressure to ignore the problem. But suppression is not total — open science and audit mechanisms can expose gaming. Theater ratio (0.68): High. As the metric distorts, it becomes performative: everyone knows the metric is gamed, yet optimization continues because no alternative coordination mechanism exists. Reporting the metric becomes theater, game-spotting becomes ritual. The theater increases over time (0.35 → 0.68 in the interval) as gaming becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and instructive. The policy designer sees Rope (coordination that enables delegation). The gaming agent sees Rope (arbitrage opportunity). The intended beneficiary sees Snare (trapped, no exit, bearing cost). The analytical observer sees Tangled Rope (the coordination and extraction are structurally coupled — you cannot have one without the other). The meta-governance coalition sees Scaffold (sunset mechanism real — as alternative measurement systems mature, official metrics lose power). The institutional theater observer sees Piton (metric persists through ritual long after functional death). No perspective is wrong — they are measuring from genuinely different structural positions with different exit options and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent reflects their structural relationship to the metric gaming opportunity. Metric optimizers (beneficiaries with arbitrage options) experience low d because they can exit and benefit from gaming. System designers (beneficiaries with arbitrage options) experience low d because they can change the metric or adjust the system. Intended outcome bearers (victims with trapped exit) experience high d because they cannot escape the consequences of metric distortion. The analytical observer experiences moderate d because Goodhart's Law affects all measurement systems equally — there is no structural escape from the law itself, only from specific metrics. The meta-governance coalition experiences low d because they have mobile exit options (can deploy alternative metrics, can exit to transparency mechanisms). The directionality derivation shows why Goodhart's Law is tangled_rope rather than pure snare: beneficiaries genuinely benefit from the coordination function (low d), but this coordination necessarily creates the extraction opportunity (high d for victims). The rope and snare components are inseparable.
 *
 * MANDATROPHY ANALYSIS:
 *   Goodhart's Law resolves the mandatrophy by demonstrating that tangled_rope is the only honest classification: the constraint simultaneously exhibits genuine coordination function (metrics enable measurement, delegation, accountability) and systematic extraction (gaming extracts value from intended beneficiaries). The classification cannot be reduced to pure rope or pure snare because both components are structurally necessary. The snare perspective (intended beneficiary) is real and should not be dismissed. The rope perspective (policy designer) is also real and should not be dismissed. The tangled_rope classification preserves both truths: (1) metrics are necessary coordination mechanisms, AND (2) metrics under optimization pressure necessarily generate gaming extraction. The mandatrophy is resolved by recognizing that the extraction is not a failure of metric design but a feature of measurement-under-pressure. No single perspective 'solves' Goodhart's Law — the solution requires meta-governance (scaffold perspective) that provides alternatives to official metrics, reducing their power to extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_stability_threshold,
    'What rate of metric distortion distinguishes a gaming agent from a metric designer who has correctly anticipated equilibrium behavior?',
    'Time-series analysis of metric-outcome correlation; comparison of predicted vs actual gaming magnitude; agent interviews on optimization intent',
    'If gaming is rapid and unexpected: extraction perspective dominates, victim extraction is acute. If gaming is equilibrated: rope/tangled_rope perspectives dominate, gaming is built into the system design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_stability_threshold, empirical, 'Threshold distinguishing rapid metric collapse from equilibrated gaming').

omega_variable(
    intent_vs_structure_causation,
    'Is metric distortion caused by malicious optimization or by inevitable consequence of measurement-under-pressure, independent of agent intent?',
    'Comparative analysis of metrics across systems with different governance intent (adversarial vs collaborative policy environments); agent motivation analysis; structural inevitability tests',
    'If structural/inevitable: all perspectives see tangled_rope (system feature, not agent failure). If intent-driven: victim perspective sees snare (predatory design), beneficiary perspective sees rope (legitimate delegation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_structure_causation, conceptual, 'Whether metric gaming is intentional or structurally inevitable').

omega_variable(
    multi_metric_sufficiency,
    'Can triangulation (simultaneous measurement via multiple independent metrics) prevent Goodhart''s Law convergence, or does gaming scale to overwhelm multiple simultaneous measures?',
    'Empirical testing in controlled systems (e.g., dual-metrics in A/B testing); historical analysis of multi-metric collapse in complex organizations; gaming-ratio under N metrics',
    'If triangulation works: meta-governance coalition perspective is correct, sunset mechanism is real. If gaming scales: only outcome-agnostic audits (shadow metrics) prevent collapse, scaffolding is harder than expected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_metric_sufficiency, empirical, 'Whether multiple independent metrics prevent gaming or gaming scales to multiple measures').

omega_variable(
    outcome_measurement_cost,
    'Is the unavoidable shift from true outcome measurement to proxy metrics fundamentally driven by cost/feasibility constraints, or by principal-agent misalignment?',
    'Cost analysis of true outcome measurement vs proxy; principal-agent incentive structure analysis; cases where true outcome measurement was achievable but not deployed',
    'If cost-driven: Goodhart''s Law is a coordination problem (rope) at the metric selection level. If incentive-driven: it is extraction (snare). Classification hinges on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outcome_measurement_cost, empirical, 'Cost vs incentive drivers of proxy metric adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law, theater_ratio, 0, 0.35).
narrative_ontology:measurement(goodhart_tr_t3, goodharts_law, theater_ratio, 3, 0.52).
narrative_ontology:measurement(goodhart_tr_t6, goodharts_law, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(goodhart_be_t3, goodharts_law, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(goodhart_be_t6, goodharts_law, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law, information_standard).
narrative_ontology:affects_constraint(goodharts_law, institutional_metrics_gaming).
narrative_ontology:affects_constraint(goodharts_law, performance_review_capture).
narrative_ontology:affects_constraint(goodharts_law, research_publication_bias).

% DUAL FORMULATION NOTE:
% Goodhart's Law is a master constraint that manifests in domain-specific stories (research metrics, healthcare metrics, education metrics, economic indicators). Each domain story has its own extractiveness value reflecting domain-specific gaming difficulty. The master constraint story captures the structural commonality: measurement-under-pressure generates inevitable metric-outcome divergence. Decomposition: Goodhart's Law (ε=0.58, abstract structural) → institutional_metrics_gaming (ε=0.42, economic domain) → research_publication_bias (ε=0.52, academic domain) → healthcare_throughput_gaming (ε=0.61, medical domain). Each downstream story has lower ε because domain-specific gaming is constrained by field-specific factors. The master constraint has higher ε because it captures the abstract principle that applies universally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
