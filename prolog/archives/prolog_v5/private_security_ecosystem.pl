% ============================================================================
% CONSTRAINT STORY: private_security_ecosystem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_security_ecosystem, []).

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
 *   constraint_id: private_security_ecosystem
 *   human_readable: Private Security Ecosystem: Extraction Through Coordination Failure
 *   domain: political_economy/security
 *
 * SUMMARY:
 *   The private security ecosystem represents a hybrid
 *   coordination-extraction mechanism that has expanded over two decades as a
 *   consequence of both genuine security coordination needs and systematic
 *   cost-shifting from public to private provisioning. The constraint
 *   exhibits the hallmark structure of a Tangled Rope: it performs legitimate
 *   coordination functions (threat detection, physical protection, emergency
 *   response) while simultaneously extracting through privatization
 *   rent-seeking, labor undervaluation, and the creation of protected
 *   enclaves that externalize security costs onto vulnerable populations. The
 *   rising extractiveness trajectory (0.35 → 0.58) and stable but elevated
 *   theater ratio (0.38 → 0.48) reflect both increasing asymmetry in the
 *   coordination benefits (captured increasingly by firms and state
 *   apparatus) and stable but persistent gaps between the regulatory
 *   appearance and enforcement reality. The constraint cannot be classified
 *   as pure Rope because the asymmetric extraction is structural, not
 *   incidental. It cannot be classified as pure Snare because genuine
 *   coordination functions remain — threat response, infrastructure
 *   protection, and intelligence sharing genuinely require the integrated
 *   capacity that the ecosystem provides. The state's constrained exit option
 *   distinguishes it from the private firms' arbitrage position: the state
 *   cannot easily unwind private relationships without revealing and
 *   rebuilding public security capacity, creating institutional lock-in that
 *   benefits the private sector.
 *
 * KEY AGENTS:
 *   - Private Security Firms: Primary beneficiary (institutional/arbitrage) — capture coordination surplus through contracting relationships, market power, and regulatory capture; can arbitrage between jurisdictions and client types
 *   - General Population / Vulnerable Communities: Primary victim (powerless/trapped) — experience extraction through fragmented security provision, reduced access to public goods, and inability to organize counter-power
 *   - Security Workers: Secondary victim (moderate/constrained) — face wage depression, high occupational hazard, limited benefits despite genuine skill development and coordination role
 *   - State Security Apparatus: Secondary beneficiary / constrained victim (institutional/constrained) — benefits from cost-shifting but locked into dependency; cannot exit without revealing public security gaps
 *   - Regulatory Framework: Institutional actor (institutional/constrained) — maintains theatrical oversight; low enforcement reflects both insufficient capacity and institutional forbearance protecting private sector profitability
 *   - Analytical Observer: Global, civilizational view (analytical/analytical) — sees the ecosystem as a policy-contingent arrangement, not an immutable market structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_security_ecosystem, 0.58).
domain_priors:suppression_score(private_security_ecosystem, 0.65).
domain_priors:theater_ratio(private_security_ecosystem, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_security_ecosystem, extractiveness, 0.58).
narrative_ontology:constraint_metric(private_security_ecosystem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(private_security_ecosystem, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_security_ecosystem, tangled_rope).
narrative_ontology:human_readable(private_security_ecosystem, "Private Security Ecosystem: Extraction Through Coordination Failure").
narrative_ontology:topic_domain(private_security_ecosystem, "political_economy/security").

domain_priors:requires_active_enforcement(private_security_ecosystem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_security_ecosystem, private_security_firms).
narrative_ontology:constraint_beneficiary(private_security_ecosystem, state_security_apparatus).
narrative_ontology:constraint_victim(private_security_ecosystem, general_population).
narrative_ontology:constraint_victim(private_security_ecosystem, contract_workers).
narrative_ontology:constraint_victim(private_security_ecosystem, vulnerable_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE RESIDENT (SNARE) — Trapped in neighborhoods where private security substitutes for public infrastructure. Pays through property taxes, fees, or accepts exclusion. No exit options short of relocation; cannot organize effective counter-power. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(private_security_ecosystem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECURITY WORKER (TANGLED ROPE) — Constrained by labor market conditions and certification requirements. Benefits from employment and skill development within the ecosystem but faces wage depression, limited benefits, and high occupational hazard. Genuine coordination function (physical protection) paired with asymmetric extraction (labor undervalued relative to risk).
constraint_indexing:constraint_classification(private_security_ecosystem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIVATE SECURITY FIRM (ROPE) — Experiences the constraint as pure coordination: contracting relationships, insurance mechanisms, reputation systems, supply chain efficiencies. Net beneficiary with high exit capacity. Can arbitrage between jurisdictions, contract types, and service models. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(private_security_ecosystem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (TANGLED ROPE) — Genuinely coordinates with private firms on threat response, intelligence sharing, and infrastructure protection. But also locked into dependency: cost-shifting to private sector reduces public budget pressure while creating institutional capture. Constrained exit — cannot unwind private relationships without revealing public security gaps. Asymmetric extraction from private partners who gain regulatory favor.
constraint_indexing:constraint_classification(private_security_ecosystem, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — The licensing, accountability, and oversight structures for private security are largely theatrical. Formal regulations exist but enforcement is minimal, definitions are vague, and the private sector largely self-regulates. Theater ratio elevated by the performative appearance of oversight without substantive constraint. The framework persists through institutional inertia — dismantling private security would require rebuilding public capacity, so regulation remains low-theater but low-functional.
constraint_indexing:constraint_classification(private_security_ecosystem, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global, civilizational view, the private security ecosystem coordinates genuine security functions (protection, deterrence, intelligence) while extracting through the privatization of what should be public goods. The constraint is not immutable — it reflects policy choices about the boundary between public and private provision. The analytical view captures the hybrid nature: real coordination overlaid with asymmetric extraction and externalized costs.
constraint_indexing:constraint_classification(private_security_ecosystem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_security_ecosystem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_security_ecosystem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_security_ecosystem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_security_ecosystem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_security_ecosystem, TR),
    TR >= 0.70.

:- end_tests(private_security_ecosystem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The private security ecosystem extracts through multiple mechanisms: (1) market power concentration allowing above-competitive wages for firms and below-competitive wages for workers, (2) cost-shifting from public to private budgets creating fiscal pressure on public security, (3) creation of protected enclaves that externalize security costs onto unprotected populations. The rising trajectory reflects accumulating institutional dependence and increasing substitution of public with private provision. Suppression (0.65): Moderate-high. Significant barriers to exit or reorganization include: specialized training and certification requirements (lock-in for workers), high capital and regulatory barriers to entry (lock-in for workers competing with established firms), geographic sorting by income (traps vulnerable populations in high-cost or low-service jurisdictions), and political economy constraints on rebuilding public security. Suppression is not total because some mobility exists and regulatory reform is theoretically possible, but actual exit requires substantial individual or collective cost. Theater ratio (0.48): Moderate. The regulatory framework for private security maintains the appearance of oversight (licensing, background checks, formal complaint mechanisms) but enforcement is minimal and accountability gaps are persistent. The theater is lower than pure Piton (0.70+) because genuine coordination functions remain and some legitimate risk management occurs. The theater reflects the gap between regulatory form and enforcement substance rather than complete functional atrophy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the hybrid nature of the constraint. The private firm sees Rope — legitimate security coordination, efficient contracting, mutual benefit from expanding client base. The state sees Tangled Rope — genuine coordination but locked into dependent relationships with constrained exit. The security worker sees Tangled Rope — real skill development and employment but undervalued labor and occupational risk. The vulnerable resident sees Snare — maximum extraction with no exit and no coordination benefit. The regulatory framework sees Piton — the oversight apparatus persists through institutional inertia even as substantive enforcement atrophies. The analytical observer at civilizational scale sees Tangled Rope — the constraint coordinates genuine security functions while extracting through the privatization boundary choice. The gap between Rope (firm perspective) and Snare (resident perspective) on the same structural data reveals that 'coordination' is experienced asymmetrically: coordination for whom, at whose cost?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural position and exit capacity. Private security firms are beneficiaries with high arbitrage capacity — low d (≈0.15), producing negative or near-zero f(d), meaning the constraint subsidizes them. The state is a constrained institutional actor with partial beneficiary status (cost-shifting) but locked dependencies — moderate d (≈0.40). Security workers are victims with constrained exit due to certification, labor market barriers, and potential identity fusion with the professional role — higher d (≈0.65). Vulnerable residents are victims with trapped exit options — maximum d (≈0.95). The directionality derivation reveals that the same constraint produces radically different effective extractiveness values for different agents: the firm experiences negative chi (subsidy), the state experiences moderate chi (partial extraction), the worker experiences high chi (significant extraction), and the vulnerable resident experiences maximum chi (severe extraction). This perspectival heterogeneity is the signature of Tangled Rope with strong asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that the constraint genuinely coordinates security functions while extracting asymmetrically. The Tangled Rope classification prevents mislabeling the ecosystem as pure Rope (missing the extractive asymmetry) or pure Snare (missing the coordination function). The state's genuine security need for private capability coordination is real — this is not a coordination theater. But the coordination is purchased at a price that extracts from workers and vulnerable populations. The classification captures both truths: this is coordination infrastructure WITH extraction overlaid. The rising extractiveness trajectory (0.35 → 0.58) over the 20-year interval suggests that the coordination function remains relatively constant while extraction mechanisms accumulate — regulatory capture intensifies, labor standards degrade, and public sector atrophy increases institutional lock-in. If this trend continues, the constraint could transition toward Snare (extraction mechanisms dominate coordination), particularly from the state's perspective as dependency deepens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_privatization_boundary,
    'How much of the private security ecosystem''s extraction is inherent to coordinating security vs how much is contingent rent-seeking from privatization?',
    'Comparative institutional analysis: security outcomes and costs in public vs private vs hybrid systems; productivity analysis controlling for threat environment',
    'If primarily coordination: classification shifts toward Rope, extractiveness floor drops to 0.35. If primarily privatization rent: classification confirmed as Tangled Rope/Snare, extractiveness rises to 0.65+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_privatization_boundary, empirical, 'Boundary between coordination function and privatization rent-seeking').

omega_variable(
    public_sector_atrophy_causation,
    'Does private security growth cause public security capacity loss or fill gaps created by prior austerity?',
    'Time-series analysis of public security budgets vs private security growth; causal pathway identification through institutional records and policy decisions',
    'If causes atrophy: extraction is generative — the ecosystem creates its own demand through institutional degradation (Snare signature). If fills gaps: extraction is parasitic but not generative (Tangled Rope). Causal direction determines whether constraint is self-sustaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_sector_atrophy_causation, empirical, 'Whether private security growth causes public sector decline').

omega_variable(
    accountability_enforcement_gap,
    'Is the low accountability for private security firms a function of insufficient regulatory capacity or deliberate regulatory forbearance?',
    'Analysis of regulatory agency budgets, enforcement case rates, policy statements, and legislative history; comparison to regulatory intensity in adjacent sectors',
    'If insufficient capacity: theater_ratio is performance of regulation despite resource constraints (Piton confirmed). If forbearance: theater_ratio is deliberate maintenance of weak oversight to protect private sector profit margins (shifts classification toward Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_enforcement_gap, empirical, 'Root cause of accountability gap: capacity vs forbearance').

omega_variable(
    worker_identity_lock_mechanism,
    'To what extent are security workers locked into the ecosystem by professional identity vs structural economic constraints?',
    'Career transition analysis: percentage of former security workers successfully transitioning to other sectors; identity-fusion patterns in worker interviews; market wage differentials controlling for skill',
    'If identity-locked: exit_options for security workers should be identity_locked rather than constrained; classification shifts; worker empowerment requires identity-frame intervention not just wage improvement. If purely constrained: economic interventions (minimum wage, benefits) would increase exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_identity_lock_mechanism, empirical, 'Identity fusion vs structural constraint for security workers').

omega_variable(
    collective_action_viability,
    'Can vulnerable residents and contract workers coordinate counter-power sufficient to renegotiate extraction terms?',
    'Case studies of successful organizing against private security; analysis of union density and bargaining power in security sector; measurement of political capital available to resident coalitions',
    'If viable: powerless agents can upgrade to organized; classification shifts from Snare toward Tangled Rope; constraint becomes temporary (Scaffold). If not viable: powerless classification confirmed; Snare persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_viability, empirical, 'Viability of collective action by trapped agents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_security_ecosystem, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priv_sec_tr_t0, private_security_ecosystem, theater_ratio, 0, 0.38).
narrative_ontology:measurement(priv_sec_tr_t10, private_security_ecosystem, theater_ratio, 10, 0.42).
narrative_ontology:measurement(priv_sec_tr_t20, private_security_ecosystem, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(priv_sec_be_t0, private_security_ecosystem, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(priv_sec_be_t10, private_security_ecosystem, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(priv_sec_be_t20, private_security_ecosystem, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_security_ecosystem, enforcement_mechanism).
narrative_ontology:affects_constraint(private_security_ecosystem, public_security_provision).
narrative_ontology:affects_constraint(private_security_ecosystem, labor_market_wage_suppression).
narrative_ontology:affects_constraint(private_security_ecosystem, residential_segregation_by_income).

% DUAL FORMULATION NOTE:
% The private security ecosystem is downstream of multiple structural constraints: public sector fiscal pressure (which creates demand for privatization), labor market wage suppression (which enables low-wage security work), and residential income segregation (which fragments demand for security across protected and unprotected populations). Each upstream constraint has its own extractiveness value; the ecosystem story captures the coordination failure that emerges from their intersection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(private_security_ecosystem, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
