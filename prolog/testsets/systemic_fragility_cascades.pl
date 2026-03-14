% ============================================================================
% CONSTRAINT STORY: systemic_fragility_cascades
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_systemic_fragility_cascades, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: systemic_fragility_cascades
 *   human_readable: Systemic Fragility Cascades: The Coupling of Distributed Dependencies into Irreversible Collapse
 *   domain: systems_resilience/network_dynamics/institutional_failure
 *
 * SUMMARY:
 *   Systemic fragility cascades represent a structural constraint that
 *   emerges when complex systems achieve high integration and
 *   interdependency. The constraint operates through a two-stage mechanism:
 *   (1) distributed dependencies create coupling where failure in one node
 *   propagates to neighbors; (2) suppression mechanisms (information
 *   asymmetry, coordination lock-in, switching costs) prevent peripheral
 *   agents from decoupling before cascades trigger. The constraint exhibits
 *   all six DR types simultaneously, making it a diagnostic exemplar for how
 *   a single structural phenomenon can be experienced radically differently
 *   depending on the agent's structural position within the system. The
 *   fragility cascade is neither purely a coordination mechanism (hub
 *   institutions genuinely coordinate across distributed dependencies) nor
 *   purely an extraction mechanism (peripheral agents face real vulnerability
 *   regardless of institutional intent), but a hybrid constraint where
 *   coordination and extraction are inseparably coupled. The extractiveness
 *   value (0.62) reflects that peripheral agents experience significant
 *   extraction during cascade events, with suppression (0.68) indicating high
 *   barriers to exit or restructuring beforehand. The theater ratio (0.55)
 *   reflects moderate performative content: regulatory frameworks and
 *   resilience mandates provide theater-level assurance that systems are
 *   monitored and protected, yet cascades still occur despite formal
 *   compliance.
 *
 * KEY AGENTS:
 *   - Peripheral Agents (powerless/trapped): Small suppliers, migrant workers, economically vulnerable households locked into fragile supply chains. Bear full extraction cost during cascades with no compensation or exit options.
 *   - Middle-Rank Institutions (moderate/constrained): Regional banks, medium firms, public utilities. Integrated into interconnected systems with limited diversification capacity and high restructuring costs.
 *   - Hub Institutions (institutional/constrained): Central banks, major tech platforms, critical infrastructure operators. Coordinate distributed dependencies while concentrating failure risk and information asymmetry.
 *   - Diversified Investors (powerful/arbitrage): Sufficiently diversified portfolios experience cascades as coordination opportunities. Maximum exit optionality, benefit without bearing cascade costs.
 *   - Regulatory Authorities (institutional/constrained): Banking regulators, environmental agencies, resilience mandates. Maintain degraded monitoring frameworks that provide theater but miss emerging cascade risks.
 *   - Resilience Coalition (organized/mobile): Governments, supranational institutions, civil society networks building redundancy and decoupling. Organized agents with exit paths and sunset horizons.
 *   - Analytical Observer: Complexity theory perspective that risks naturalizing contingent architecture choices as irreducible phase transitions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(systemic_fragility_cascades, 0.62).
domain_priors:suppression_score(systemic_fragility_cascades, 0.68).
domain_priors:theater_ratio(systemic_fragility_cascades, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(systemic_fragility_cascades, extractiveness, 0.62).
narrative_ontology:constraint_metric(systemic_fragility_cascades, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(systemic_fragility_cascades, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(systemic_fragility_cascades, snare).
narrative_ontology:human_readable(systemic_fragility_cascades, "Systemic Fragility Cascades: The Coupling of Distributed Dependencies into Irreversible Collapse").
narrative_ontology:topic_domain(systemic_fragility_cascades, "systems_resilience/network_dynamics/institutional_failure").

% --- Structural relationships ---
narrative_ontology:constraint_victim(systemic_fragility_cascades, peripheral_agents).
narrative_ontology:constraint_victim(systemic_fragility_cascades, economically_vulnerable_populations).
narrative_ontology:constraint_victim(systemic_fragility_cascades, ecological_systems).
narrative_ontology:constraint_victim(systemic_fragility_cascades, institutional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL AGENT (SNARE) — Small supplier, migrant worker, or economically vulnerable household locked into fragile supply chains with no alternative markets. When cascade triggers, bears full extraction cost (job loss, income collapse, no compensation). Cannot exit beforehand — dependency is the only available option. Maximum experienced extraction.
constraint_indexing:constraint_classification(systemic_fragility_cascades, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-RANK INSTITUTION (SNARE) — A regional bank, medium-sized firm, or public utility integrated into interconnected systems. Has some diversification capacity but faces high costs to restructure supply chains or reduce critical dependencies. In cascade event, experiences severe extraction with limited exit options available in real time. Trapped by complexity and switching costs.
constraint_indexing:constraint_classification(systemic_fragility_cascades, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUB INSTITUTION (TANGLED ROPE) — A central bank, major tech platform, or critical infrastructure operator coordinates distributed dependencies (coordination function) while extracting information asymmetry and concentration rent (extraction function). Genuinely solves collective coordination problem but concentrates failure risk. Benefits from integration during stable periods; faces catastrophic exposure during cascades. Requires active enforcement of its coordination role.
constraint_indexing:constraint_classification(systemic_fragility_cascades, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DIVERSIFIED INVESTOR (ROPE) — Portfolio that is sufficiently diversified across uncorrelated systems experiences the cascade constraint as a coordination mechanism: the system's fragility creates opportunities for anti-correlated positioning and rebalancing profit. Has maximal exit optionality (can be anywhere, holds cash). Benefits from coordination without bearing cascade extraction.
constraint_indexing:constraint_classification(systemic_fragility_cascades, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Banking regulations, environmental standards, and resilience mandates persist despite degraded function. Stress tests and capital adequacy rules provide theater (systemic cascade can trigger despite passed tests). Originally designed to prevent cascades; now maintained through institutional inertia as markets have evolved beyond the regulatory model. Theater ratio elevated due to regulatory arbitrage making formal compliance decoupled from actual fragility.
constraint_indexing:constraint_classification(systemic_fragility_cascades, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: RESILIENCE COALITION (SCAFFOLD) — Organized agents (governments, supranational institutions, civil society networks) attempting to build redundancy, decoupling, and localization as countermeasures to cascade risk. These are temporary support structures (local food systems, decentralized energy, supply chain diversification mandates) designed to sunset as dependencies are rebuilt on more resilient foundations. Organized agents see agency and temporal bounding — extraction is tolerated because the infrastructure being built creates exit paths.
constraint_indexing:constraint_classification(systemic_fragility_cascades, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a complexity theory perspective, cascade fragility appears as an irreducible phase transition: any sufficiently integrated system exhibits critical slowing down (slower recovery from perturbations) as systems approach a critical threshold. The cascade constraint appears as a natural law of complex systems rather than a contingent institutional arrangement. However, this naturalizes what is actually a choice about system architecture — decentralized systems exhibit different phase transitions than centralized ones.
constraint_indexing:constraint_classification(systemic_fragility_cascades, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(systemic_fragility_cascades_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(systemic_fragility_cascades, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(systemic_fragility_cascades, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(systemic_fragility_cascades, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(systemic_fragility_cascades, TR),
    TR >= 0.70.

:- end_tests(systemic_fragility_cascades_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising over the measurement interval (0.35 → 0.62), reflecting increasing integration and coupling. Peripheral agents experience growing extraction as their dependency on coordinated systems deepens and diversification options shrink. The rise indicates that the constraint is not static — integration dynamics are accumulating extraction over time. Suppression (0.68): High and stable, reflecting multiple mechanisms: switching costs embedded in supply chains, information asymmetries that prevent alternative market formation, capital constraints limiting restructuring, and institutional inertia in regulatory frameworks. Theater ratio (0.55): Moderate and rising (0.42 → 0.55), indicating increasing gap between formal system monitoring/risk management and actual cascade vulnerability. Stress tests, capital requirements, and resilience metrics provide assurance (theater) without meaningfully reducing cascade probability for peripheral actors. The rising theater reflects degradation — more elaborate monitoring rituals despite persistent or growing fragility.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals whether the cascade constraint is fundamentally a coordination problem or a hidden extraction mechanism. The powerless agent's snare perspective (maximum extraction) suggests hidden extraction. The hub institution's tangled rope perspective (mixed coordination and extraction) suggests genuine coordination with extraction overlay. The resilience coalition's scaffold perspective (temporary, with sunset) suggests the constraint is architectural rather than inevitable. The analytical observer's mountain perspective (natural law) risks naturalizing what is a choice about system architecture. The gap between mountain and snare is diagnostic: if the observer's natural law framing is accurate, we should see identical experience across all positions. Instead, we see radically divergent experiences, suggesting the mountain classification is a false summit — the constraint is contingent, not universal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's position in the extraction flow and exit capacity. Peripheral agents with trapped exit experience d = 0.95 (full target). Hub institutions with constrained exit but genuine coordination benefit experience d = 0.55 (symmetric to slightly extracted). Diversified investors with arbitrage exit experience d = 0.15 (full beneficiary). The organized coalition with mobile exit and agency experiences d = 0.30 (moderate beneficiary). The sigmoid f(d) transforms these into experienced extractiveness chi: peripheral agents experience maximum chi despite moderate base extractiveness because their f(d) ≈ 1.42; diversified investors experience minimal chi despite the same base extractiveness because their f(d) ≈ -0.01. The constraint is objectively the same, but the structural positions produce radical divergence in experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through architectural decomposition: the fragility cascade is simultaneously Snare (peripheral extraction), Tangled Rope (mixed coordination-extraction at hub level), Rope (pure coordination for diversified actors), Scaffold (temporary with sunset for organized coalitions), and Piton (degraded regulation). All six types are structurally accurate from different positions. The mandatrophy is resolved by rejecting the false mountain perspective (natural law) and treating the constraint family as a presheaf over agent positions. The question 'Which type is correct?' is answerable only by specifying 'Correct from which structural position?' The constraint is not mislabeled — it is fully classified across the observation site. The false summit (mountain/natural law) reveals the risk of analytical naturalization: treating contingent architecture as irreducible physics. The true classification is the presheaf: the set of all structurally accurate perspectives from the positions inhabited in the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_trigger_threshold,
    'What observable reliably predicts the transition from stable integration to cascade failure — and is it fundamentally unobservable until after the cascade begins?',
    'Pre-cascade and post-cascade analysis of network metrics (correlation, leverage, criticality) for multiple failed and non-failed integrated systems. Determine whether metrics diverge before failure or only become visible retrospectively.',
    'If predictable before trigger: regulatory early-warning systems are viable, extraction risk can be managed through disclosure. If fundamentally unobservable: cascades are inherent to integration, and the constraint is structural mountain-like property, not an institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_trigger_threshold, empirical, 'Observability of cascade threshold before triggering').

omega_variable(
    decoupling_feasibility,
    'Can complex systems (supply chains, financial networks, power grids, ecological systems) be meaningfully decoupled without catastrophic efficiency loss — or is integration the binding constraint for modern material abundance?',
    'Cost-benefit analysis of localization/resilience vs efficiency across domains. Historical case studies of system decoupling (trade deglobalization, energy decentralization) and their welfare impact. Feasibility modeling for demand-level changes required.',
    'If decoupling is feasible at acceptable cost: scaffold perspectives are structural, exit is real, constraints can be temporally bounded. If decoupling requires permanent efficiency loss: the cascade constraint is ontologically tethered to prosperity itself, and peripheral agents face permanent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility, preference, 'Trade-off between system integration and cascade vulnerability').

omega_variable(
    coordination_counterfactual,
    'Do the hub institutions that concentrate failure risk (central banks, major platforms, critical infrastructure) actually provide coordination value that decentralized alternatives cannot replicate, or do they provide theater masking extractive rent?',
    'Comparative institutional analysis: systems with distributed coordination (trade networks without dominant hubs, federated governance, open-source infrastructure) vs hub-dependent systems. Measure coordination efficiency, innovation rate, and cascade frequency across architectures.',
    'If hub coordination is functionally irreplaceable: snare classification is accurate — peripheral agents face genuine structural extraction. If hub coordination is theater: the constraint is a choice, and scaffold/piton classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_counterfactual, empirical, 'Whether hub institutions provide irreplaceable coordination or extractive rents').

omega_variable(
    information_asymmetry_persistence,
    'As monitoring and disclosure technologies improve (real-time supply chain visibility, blockchain verification, climate monitoring), does the information asymmetry that hub institutions exploit actually decline, or do new forms of opacity emerge?',
    'Longitudinal tracking of information disclosure depth and actor-level knowledge of system state across 20+ years. Analysis of whether technical transparency leads to behavioral change or regulatory capture/opacity migration.',
    'If asymmetry declines: tangled rope classification weakens, rope classification strengthens. If new opacity forms: tangled rope classification persists, institutional concentration risk remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether technological transparency reduces institutional information asymmetry').

omega_variable(
    suppression_internalization,
    'Is the high suppression value (0.68) measuring structural barriers (switching costs, capital constraints, information gaps) or internalized helplessness (agents believe cascades are inevitable and unpreventable even when alternatives exist)?',
    'Qualitative study of agent framing: do peripheral agents articulate structural barriers or express fatalism? Policy intervention studies: do agents change dependencies when barriers are removed through government subsidy or technical support, or do they persist in cascade-vulnerable configurations?',
    'If structural: suppression metric is accurate, and constraint classification is stable. If internalized: suppression is partially performative, and constraint could be destabilized through reframing and consciousness-raising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(systemic_fragility_cascades, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfc_tr_t0, systemic_fragility_cascades, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sfc_tr_t5, systemic_fragility_cascades, theater_ratio, 5, 0.49).
narrative_ontology:measurement(sfc_tr_t10, systemic_fragility_cascades, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sfc_tr_t3, systemic_fragility_cascades, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sfc_tr_t7, systemic_fragility_cascades, theater_ratio, 7, 0.52).

% Extraction over time
narrative_ontology:measurement(sfc_be_t0, systemic_fragility_cascades, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfc_be_t5, systemic_fragility_cascades, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sfc_be_t10, systemic_fragility_cascades, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(sfc_be_t3, systemic_fragility_cascades, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(sfc_be_t7, systemic_fragility_cascades, base_extractiveness, 7, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(systemic_fragility_cascades, global_infrastructure).
narrative_ontology:affects_constraint(systemic_fragility_cascades, supply_chain_monopolization).
narrative_ontology:affects_constraint(systemic_fragility_cascades, financial_contagion_mechanisms).
narrative_ontology:affects_constraint(systemic_fragility_cascades, critical_infrastructure_coupling).
narrative_ontology:affects_constraint(systemic_fragility_cascades, ecological_tipping_points).

% DUAL FORMULATION NOTE:
% Systemic fragility cascades are downstream of specific domain constraints (financial contagion, supply chain monopolization, infrastructure coupling, ecological tipping points) but represent a distinct structural constraint about how dependencies become dangerous. Each upstream domain has its own extractiveness reflecting domain-specific integration dynamics; the cascade constraint reflects the general architectural property of coupled systems. Network edges link the cascade to domain-specific instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
