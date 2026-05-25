% ============================================================================
% CONSTRAINT STORY: software_sustainability_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_sustainability_crisis, []).

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
 *   constraint_id: software_sustainability_crisis
 *   human_readable: Software Sustainability Crisis: Dependency Lock and Extraction
 *   domain: software_engineering/digital_infrastructure
 *
 * SUMMARY:
 *   The software sustainability crisis represents a structural extraction
 *   mechanism emerging from the collision between free labor contributions
 *   (open source maintenance) and concentrated commercial benefit (corporate
 *   dependency on unpaid infrastructure). The constraint exhibits the classic
 *   snare signature: downstream actors (developers, end users, maintainers)
 *   face escalating costs and decreasing exit options as software ecosystems
 *   mature and dependencies proliferate. Simultaneously, the constraint
 *   possesses coordination functions (interoperability standards, shared
 *   security infrastructure, collaborative problem-solving) that justify some
 *   extraction as transaction cost. The theater ratio (0.65) reflects the gap
 *   between the mythology of open source altruism ('gift economy,' 'intrinsic
 *   motivation') and the extractive reality (vendor lock-in, unpaid
 *   maintenance subsidizing commercial products, burnout epidemiology).
 *   Organized agents are building alternative pathways (Linux Foundation
 *   stewardship, OpenSSF funding, supply-chain security initiatives) with
 *   genuine sunset logic, but these are threatened by capture and by security
 *   compliance measures that may substitute technical lock-in with regulatory
 *   lock-in.
 *
 * KEY AGENTS:
 *   - Downstream Developers: Primary victim (powerless/trapped) — locked into dependency chains; migration prohibitively costly; no exit option without system rebuild
 *   - Open Source Maintainers: Primary victim (powerless/trapped) — unpaid maintenance of critical infrastructure; social obligation and reputation lock prevent exit; bearing full burnout cost
 *   - End Users / Enterprises: Secondary victim (moderate/constrained) — high switching costs; forced to maintain aging systems or undertake expensive migrations; security/stability risk exposure without ecosystem control
 *   - Commercial Dependency Extractors: Primary beneficiary (institutional/arbitrage) — controlling critical dependencies; benefiting from vendor lock-in; capturing value through ecosystem necessity
 *   - Open Source Governance Coalition: Organized actors (organized/constrained) — Linux Foundation, Software Heritage, OpenSSF building alternative sustainability models; perceiving sunset pathway
 *   - Traditional Open Source Ethos: Institutional maintenance (institutional/arbitrage) — volunteer model persists through cultural mythology despite systemic failure; theater maintained through narrative celebration
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination functions and asymmetric extraction; recognizes the tangled rope structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_sustainability_crisis, 0.58).
domain_priors:suppression_score(software_sustainability_crisis, 0.68).
domain_priors:theater_ratio(software_sustainability_crisis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_sustainability_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_sustainability_crisis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(software_sustainability_crisis, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_sustainability_crisis, snare).
narrative_ontology:human_readable(software_sustainability_crisis, "Software Sustainability Crisis: Dependency Lock and Extraction").
narrative_ontology:topic_domain(software_sustainability_crisis, "software_engineering/digital_infrastructure").

domain_priors:requires_active_enforcement(software_sustainability_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_sustainability_crisis, commercial_dependency_extractors).
narrative_ontology:constraint_beneficiary(software_sustainability_crisis, dominant_platform_maintainers).
narrative_ontology:constraint_victim(software_sustainability_crisis, downstream_developers).
narrative_ontology:constraint_victim(software_sustainability_crisis, open_source_maintainers).
narrative_ontology:constraint_victim(software_sustainability_crisis, end_users).
narrative_ontology:constraint_victim(software_sustainability_crisis, software_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM DEVELOPER (SNARE) — Trapped in dependency chains with no meaningful exit. Migration costs are prohibitive; fragmentation of ecosystems creates lock-in; security vulnerabilities force continued exposure. The developer cannot walk away without rebuilding entire systems. Bears full extraction cost with minimal alternatives.
constraint_indexing:constraint_classification(software_sustainability_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN SOURCE MAINTAINER (SNARE) — Trapped in unpaid maintenance of critical infrastructure. Social obligation, reputation lock, and ecosystem dependency prevent exit. Bearing full cost of security patches, compatibility work, and burnout without compensation. No viable exit path short of complete abandonment.
constraint_indexing:constraint_classification(software_sustainability_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: END USER / ENTERPRISE ADOPTER (SNARE) — Constrained by high switching costs and supply chain dependencies. Forced to maintain aging systems with unmaintained dependencies or undertake expensive migrations. Bears security and stability risks without control over the underlying ecosystem.
constraint_indexing:constraint_classification(software_sustainability_crisis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL DEPENDENCY EXTRACTOR (ROPE) — Institutional actor controlling critical dependencies (logging frameworks, package managers, cloud SDKs). Benefits from ecosystem lock-in, vendor lock patterns, and the necessity of integration. Experiences the constraint as coordination mechanism that routes value toward them.
constraint_indexing:constraint_classification(software_sustainability_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE GOVERNANCE COALITION (SCAFFOLD) — Organized agents (Linux Foundation, Software Heritage, supply-chain security initiatives) see sustainability as a solvable coordination problem with sunset logic. Funding models, stewardship programs, and formalized maintenance pathways are building alternatives to volunteer burnout. High suppression tolerated because coalition perceives declining extraction as norms mature.
constraint_indexing:constraint_classification(software_sustainability_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL OPEN SOURCE ETHOS (PITON) — The volunteer maintenance model persists through institutional inertia and cultural mythology ('the gift economy,' 'intrinsic motivation') despite clear systemic failure signals (burnout epidemiology, abandoned critical packages, security catastrophes). The ethos maintains itself performatively through narrative (celebrating 'heroes,' emphasizing 'passion') while actual maintenance work is increasingly stratified and precarious. Theater ratio reflects the gap between the communitarian narrative and the extractive reality.
constraint_indexing:constraint_classification(software_sustainability_crisis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the full system perspective, software sustainability contains genuine coordination: interoperability standards, shared security infrastructure, and collaborative dependency management create real positive externalities. Simultaneously, extraction operates at scale: vendor lock-in, unpaid maintenance subsidizing commercial products, and platform monopolies. The constraint is both coordination and asymmetric extraction, requiring both frameworks to understand.
constraint_indexing:constraint_classification(software_sustainability_crisis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_sustainability_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(software_sustainability_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(software_sustainability_crisis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_sustainability_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(software_sustainability_crisis, TR),
    TR >= 0.70.

:- end_tests(software_sustainability_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate and rising. The constraint captures significant value through lock-in mechanisms: dependencies become harder to replace as ecosystems mature, commercial vendors build layers atop unpaid open source infrastructure, and supply-chain complexity raises switching costs. The trajectory from 0.28 (when ecosystems were smaller and exit was more feasible) to 0.58 (current state of monolithic dependency graphs) reflects increasing capture. The value is not extraction of wealth but extraction of labor (maintenance cost shifted to unpaid volunteers) and control (vendor lock-in). Suppression (0.68): High. Multiple barriers prevent exit: technical switching costs (reimplementation, testing, migration), market lock-in (ecosystem standardization on dominant dependencies), social pressure (reputation cost of forking or abandoning), and economic precarity (developers in lower-income regions have fewer alternative income options). Theater ratio (0.65): Moderate-high and rising. The gap between open source mythology ('merit-based,' 'volunteer,' 'community-driven') and reality (corporate vendors extracting value, individual burnout, precarious maintenance) has widened. Funding campaigns, sustainability narratives, and 'hero maintainer' coverage perform coordination while obscuring structural extraction. The theater increased from 0.35 to 0.65 as sustainability rhetoric became more elaborate precisely as systemic problems (zero-days in critical packages, major library abandonment) escalated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a large perspectival gap between trapped victims and institutional beneficiaries. Downstream developers and maintainers perceive pure extraction (Snare) with no coordination benefit — they experience only the suppression and lock-in costs. Commercial vendors perceive coordination (Rope) — they are solving genuine technical integration problems, and the dependency ecosystem does route value efficiently to them. The organized coalition perceives a solvable problem (Scaffold) — open source stewardship, formalized funding, and supply-chain security are building pathways to reduce extraction. The traditional open source ethos maintains itself through Piton mechanisms — the volunteer model is culturally sustained despite clear failure signals. The analytical observer sees the full hybrid structure (Tangled Rope) — genuine coordination functions exist alongside asymmetric extraction, and the classification depends on whether the observer emphasizes the coordination (rope-leaning) or the extraction (snare-leaning).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from its structural position in the extraction flow. Trapped developers and maintainers experience maximum d ≈ 0.95 (full extraction targets). Constrained enterprises experience moderate-high d ≈ 0.70-0.85 depending on alternative platform options. Institutional beneficiaries with arbitrage exit experience low d ≈ 0.10-0.20 (they can walk away or capture more value from alternatives). The analytical observer is positioned at d ≈ 0.72 (seeing the full structure from outside the principal extraction mechanisms). The scaffold coalition's d ≈ 0.60 reflects that while they bear suppression (building alternatives is costly), they have agency and perceive declining extraction through their sunset pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy resolution at extractiveness > 0.70. The mandatrophy is: 'Is this a coordination mechanism that routes software reusability efficiently (Rope/Tangled Rope) or an extraction mechanism that colonizes volunteer labor for commercial benefit (Snare)?' The answer depends on perspective and time horizon. At the individual developer level (biographical), the constraint appears as pure Snare — the victim is locked in with no exit and no coordination benefit. At the institutional level (immediate), it appears as Rope or Tangled Rope — vendors genuinely do coordinate their dependencies and benefit from the ecosystem. At the civilizational level (generational), it appears as Scaffold if sustainability initiatives succeed, or Piton if they fail to replace volunteer extraction with formalized funding. The mandatrophy resolves by showing that the six-type perspectival presheaf accurately models the constraint's structural ambiguity — it IS coordination to institutional beneficiaries, it IS pure extraction to trapped developers, and it IS a solvable problem to organized governance coalitions. No single classification is 'correct' because the constraint genuinely operates as different types from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volunteer_vs_precarious_labor,
    'Are open source maintainers trapped by genuine social obligation and identity lock, or by economic precarity (no alternative stable income in their jurisdiction)?',
    'Longitudinal study of maintainer exit patterns; correlation between maintenance persistence and (a) identity fusion with project, (b) available alternative employment, (c) geographic location and economic mobility. Post-exit economic outcomes analysis.',
    'If identity-locked: classification as Snare remains; suppression is internalized cognitive capture. If economically precarious: reclassify suppression mechanism as structural economic constraint; implications for policy intervention differ sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(volunteer_vs_precarious_labor, empirical, 'Whether maintainer trap is identity-based or economic').

omega_variable(
    coordinated_ecosystem_vs_monolithic_lock,
    'Is the dependency ecosystem a coordination mechanism (interoperable components solving genuine technical problems) or a lock-in mechanism (monolithic suppliers controlling upgrade paths)?',
    'Network analysis of dependency graphs: measure modularity vs centralization. Case studies comparing ecosystems with distributed governance (npm ecosystem fragments) vs concentrated governance (Java frameworks under single vendor). Exit cost analysis for different dependency structures.',
    'If coordinated: Rope and Tangled Rope classifications dominate; suppression is coordination cost. If lock-in: Snare and Scaffold classifications dominate; suppression is extractive barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_ecosystem_vs_monolithic_lock, empirical, 'Whether ecosystem is coordination or lock-in mechanism').

omega_variable(
    sustainability_funding_viability,
    'Can formalized sustainability models (Linux Foundation, OpenSSF funding, corporate stewardship programs) actually sunset the volunteer extraction, or are they insufficient proxies that maintain the mythology while concentrating control?',
    'Longitudinal tracking of funded projects: measure maintainer burnout, security incident rates, and vendor lock-in evolution. Comparison of sustainability outcomes between funded and unfunded critical packages. Analysis of corporate stewardship programs for hidden extraction.',
    'If viable: Scaffold perspective confirmed — true sunset pathway exists. If insufficient: Scaffold is aspirational; the piton perspective (maintenance through theater) is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_funding_viability, empirical, 'Whether sustainability funding models can resolve the constraint').

omega_variable(
    supply_chain_security_vs_centralization_risk,
    'Do supply-chain security measures (SBOMs, provenance tracking, vulnerability scanning) reduce real risk or increase concentration of control in fewer security-auditing entities, creating new extraction points?',
    'Analysis of security incident outcomes before/after major supply-chain initiatives. Mapping of control concentration in SBOMs, provenance systems, vulnerability databases. Case studies of how security requirements shift lock-in from technical to compliance domains.',
    'If risk-reducing: Scaffold and coordinated Rope perspectives correct. If centralizing: security becomes a new extraction mechanism; Snare classification shifts from technical lock-in to security compliance lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_security_vs_centralization_risk, empirical, 'Whether supply-chain security reduces or concentrates risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_sustainability_crisis, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(softsusr_tr_t0, software_sustainability_crisis, theater_ratio, 0, 0.35).
narrative_ontology:measurement(softsusr_tr_t8, software_sustainability_crisis, theater_ratio, 8, 0.52).
narrative_ontology:measurement(softsusr_tr_t16, software_sustainability_crisis, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(softsusr_be_t0, software_sustainability_crisis, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(softsusr_be_t8, software_sustainability_crisis, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(softsusr_be_t16, software_sustainability_crisis, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_sustainability_crisis, resource_allocation).
narrative_ontology:affects_constraint(software_sustainability_crisis, open_source_maintenance_burnout).
narrative_ontology:affects_constraint(software_sustainability_crisis, vendor_lock_in_digital_infrastructure).
narrative_ontology:affects_constraint(software_sustainability_crisis, supply_chain_security_concentration).

% DUAL FORMULATION NOTE:
% Software sustainability constraint family has three downstream stories: (1) maintenance_burnout focuses on the psychological and labor dimensions of unpaid work (ε≈0.72); (2) vendor_lock_in focuses on technical switching costs and ecosystem lock (ε≈0.55); (3) supply_chain_security focuses on how security compliance can substitute or reinforce lock-in (ε≈0.48). Each story has its own base_properties, beneficiary/victim declarations, and measurements. The parent story (software_sustainability_crisis, ε=0.58) coordinates across all three mechanistic domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_sustainability_crisis, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
