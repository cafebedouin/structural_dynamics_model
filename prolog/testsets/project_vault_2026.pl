% ============================================================================
% CONSTRAINT STORY: project_vault_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_2026, []).

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
 *   constraint_id: project_vault_2026
 *   human_readable: Project Vault Strategic Mineral Reserve
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Project Vault is a $12 billion U.S. strategic mineral reserve announced
 *   February 2, 2026, designed to insulate the defense industrial base and
 *   critical manufacturing from mineral supply disruption. The constraint
 *   exhibits a core tension between coordination (solving a genuine
 *   cold-war-era vulnerability) and extraction (leveraging mineral
 *   concentration to shape global economic and geopolitical outcomes). From
 *   the U.S. defense and domestic mining perspective, Vault solves a
 *   decades-old problem: dependency on hostile or unstable mineral suppliers.
 *   From allied nations and non-U.S. manufacturers, Vault appears as
 *   strategic monopoly pricing and access denial. From the global supply
 *   chain perspective, Vault is a temporary measure triggering accelerated
 *   recycling and diversification. From the institutional Cold War planning
 *   apparatus, Vault is increasingly theatrical — maintained by inertia
 *   against a threat model that is aging. From pure geology, mineral
 *   concentration is a natural law; but from political economy, the
 *   constraint is contingent institutional design.
 *
 * KEY AGENTS:
 *   - U.S. Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — solves supply security problem; experiences net benefit; preferred access to reserves
 *   - Domestic Mining Companies: Secondary beneficiary (institutional/arbitrage) — government purchasing commitments and domestic source development; benefits from reduced overseas competition
 *   - Allied Nations (NATO, Japan, South Korea, Australia): Mixed (organized/constrained) — benefit from defense partnership coordination but suffer extraction through higher costs and preferential U.S. access
 *   - Non-Allied Mineral-Dependent Nations: Victim (powerless/trapped) — cannot exit mineral dependency; face price increases and access restriction; no alternatives
 *   - Global Electronics/Battery Manufacturing (Non-U.S.): Victim (moderate/constrained) — input cost increases; supply uncertainty; some exit options (geographic relocation) available but expensive
 *   - Global Supply Chain Resilience Coalition: Organized responders (organized/mobile) — building parallel infrastructure (recycling, diversification) to reduce Vault dependency
 *   - Cold War Strategic Planning Apparatus: Institutional theater maintainer (institutional/arbitrage) — sustains reserve through threat model inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_2026, 0.52).
domain_priors:suppression_score(project_vault_2026, 0.68).
domain_priors:theater_ratio(project_vault_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(project_vault_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(project_vault_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_2026, tangled_rope).
narrative_ontology:human_readable(project_vault_2026, "Project Vault Strategic Mineral Reserve").
narrative_ontology:topic_domain(project_vault_2026, "economic/geopolitical").

domain_priors:requires_active_enforcement(project_vault_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_2026, us_manufacturing_sector).
narrative_ontology:constraint_beneficiary(project_vault_2026, defense_industrial_base).
narrative_ontology:constraint_beneficiary(project_vault_2026, domestic_mining_companies).
narrative_ontology:constraint_victim(project_vault_2026, global_supply_chain_actors).
narrative_ontology:constraint_victim(project_vault_2026, allied_nations_mineral_access).
narrative_ontology:constraint_victim(project_vault_2026, competitive_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Non-U.S. nations dependent on mineral supply face extraction through strategic scarcity. Trapped by geography and capital constraints; cannot exit the constraint. Bear full cost of price volatility and access restriction. Maximum experienced extraction.
constraint_indexing:constraint_classification(project_vault_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Electronics, battery, aerospace manufacturers outside U.S. face cost increases and supply uncertainty. Constrained by reliance on U.S.-managed supply; some exit options (diversification) are expensive and slow. Bear extraction through higher input costs and access delays.
constraint_indexing:constraint_classification(project_vault_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Allied nations experience dual structure: coordination benefit (supply security for defense partnerships) and extraction (preferential access means higher costs/delays for non-U.S. allies). Strategic partnership solves cold-war minerals-access problem but creates asymmetric dependency. Constrained exit (cannot credibly diversify to rivals) and active enforcement (geopolitical retaliation risk for circumventing reserve).
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Primary beneficiary. Experiences constraint as coordination mechanism: solves decades-long vulnerability to mineral supply disruption. Net benefit (supply security, cost stability) outweighs enforcement costs. Arbitrage exit available (can source from reserve at preferred pricing). Extraction runs toward this agent.
constraint_indexing:constraint_classification(project_vault_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Secondary beneficiary. Benefits from U.S. government commitment to domestic source development and preferential purchasing agreements. Experiences constraint as coordination: reduces overseas supply dependence and creates market stability. Arbitrage exit via government contracts. Net benefit through capture of some extraction surplus.
constraint_indexing:constraint_classification(project_vault_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% International partners (EU, Japan, South Korea, Australia) see Vault as temporary measure triggering global coordination. Mobile exit: can build parallel recycling infrastructure, diversify sourcing, and reduce mineral intensity. Organized response (critical minerals partnerships, trade agreements) creates sunset logic — as alternatives mature, Vault's monopoly extraction mechanism weakens. Theater ratio low: the coalition has structural alternatives that bypass the reserve.
constraint_indexing:constraint_classification(project_vault_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Vault is maintained through 1990s-2020s institutional inertia: strategic minerals planning paradigm assumed continued scarcity and supply concentration. But underlying structural drivers have changed — recycling capability grows, battery technology reduces cobalt intensity, lab-grown diamonds reduce synthetic gemstone dependence. Reserve exists because the threat model (Soviet-era mineral blockade) is institutionalized, not because current supply dynamics demand it. Theater ratio high: much of Vault's operational activity is performative security posturing rather than response to genuine supply crisis. Piton classification reveals that the 'strategic' justification is increasingly theatrical.
constraint_indexing:constraint_classification(project_vault_2026, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% From pure analytical view: mineral ore distribution is geographically fixed by geology. Concentration of rare earth deposits, copper, lithium in few regions is a natural law, not a policy choice. Supply constraints are inherent to Earth's crust composition. However, this perspective ignores that technology (processing, recycling, substitution), economics (price signals), and policy (trade agreements, domestic investment) all mediate the natural constraint. The mountain classification risks naturalizing what is actually a contingent institutional arrangement layered atop geology. False summit alert.
constraint_indexing:constraint_classification(project_vault_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(project_vault_2026, TR),
    TR >= 0.70.

:- end_tests(project_vault_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Vault's extraction mechanism is asymmetric: U.S. manufacturers and allied defense partners have guaranteed access at favorable pricing; all other actors face supply restriction and cost increase. The extraction is not maximal (0.66+) because (a) recycling and substitution technologies are reducing mineral intensity, (b) alternative sourcing (China processing, allied partnerships) provides partial escapes, (c) the reserve capacity is finite and covers only a portion of global demand. Over time, extractiveness has increased (0.35 → 0.52) as the reserve becomes operational and global actors realize the supply monopoly effect. Suppression (0.68): Moderate-high. Significant barriers to circumventing Vault include: geopolitical retaliation risk for unauthorized mineral source development, capital barriers to building parallel processing infrastructure, WTO disputes over discriminatory access, and the fact that some minerals have high geological concentration (rare earths, cobalt). Suppression is not total because: recycling and substitution reduce pressure on primary supply, and allied nations have diplomatic channels to negotiate terms. Theater ratio (0.58): Moderate. A substantial portion of Vault's operational activity is performative: security theater (classified reserve locations, periodic 'readiness' assessments), geopolitical signaling (announcements of reserve capacity to intimidate rivals), and threat inflation (Cold War supply disruption scenarios that are less likely in 2026 than in 1976). However, the reserve has real supply function — it does contain minerals and could release them in genuine crisis. Theater has increased over the interval (0.42 → 0.58) as initial operational period reveals that much activity is routine security posturing rather than response to active supply crisis.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. U.S. defense sees Rope (coordination mechanism solving genuine vulnerability). Domestic mining sees Rope (stable market, government contracts). Allies see Tangled Rope (membership benefit + extraction via preferential access and supply restriction). Dependent nations see Snare (trapped by geography, no exit, full extraction cost). Non-U.S. manufacturers see Snare (input cost increases, constrained supply). Global supply chain coalition sees Scaffold (temporary problem triggering technological exit via recycling). Cold War planning sees Piton (institutional inertia maintaining outdated threat model). Analytical geology sees Mountain (mineral concentration is natural law) — but this is a false summit revealing naturalization of contingent policy. The perspectival range (Rope to Snare to Mountain) indicates that all six types are legitimate readings of the same structural data from different observer positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. U.S. defense and domestic mining are beneficiaries with arbitrage exit options (can source from reserve at preferred terms) → d ≈ 0.05-0.15 → negative/minimal χ. Allied nations are mixed: beneficiaries of coordination but victims of preferential access asymmetry, with constrained exit (cannot credibly diversify to rivals without geopolitical cost) → d ≈ 0.45-0.55 → moderate χ. Dependent nations are pure victims with trapped exit (no alternatives) → d ≈ 0.90-0.95 → maximum χ. Global supply chain coalition has mobile exit (can build parallel infrastructure) → d ≈ 0.40-0.50 → moderate χ even as victims of extraction, because their exit options reduce experienced extractiveness. Cold War planners are institutional beneficiaries with arbitrage (can maintain reserve indefinitely without challenge) → d ≈ 0.05 → negative χ (they perceive no extraction). The directionality derivation captures that exit options materially affect experienced extractiveness: trapped agents bear maximum extraction; mobile agents bear less even facing the same baseline ε.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE SATISFIED: Vault meets all three criteria: (1) beneficiaries declared (U.S. defense, domestic mining), (2) victims declared (dependent nations, non-U.S. manufacturers, allied nations), (3) requires_active_enforcement = true (geopolitical retaliation for circumventing reserve, trade agreement enforcement, security protocols for reserve access). The mandatrophy is resolved by recognizing that Vault is genuinely hybrid: it solves a coordination problem (supply security for defense partnerships) AND extracts surplus (preferential access, cost increases for non-participants). Neither pure Rope nor pure Snare captures the structure. The false summit (Mountain/natural law) is exposed by noting that mineral concentration is geological fact, but Vault's CONSTRAINT — the policy mechanism that leverages concentration into economic/geopolitical extraction — is entirely contingent. The boundary between natural constraint and policy choice is at whether recycling, substitution, and alternative sourcing can escape mineral concentration. If they cannot (geology binding), Vault approaches Mountain. If they can (policy binding), Vault is Tangled Rope. The omegas specify what data would resolve this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_mining_feasibility,
    'Can U.S. domestic mining capacity scale to serve defense needs without the capital and environmental costs becoming prohibitive?',
    'Cost curves from pilot domestic rare earth mining (Texas rare earth facility, lithium projects); comparison to imported supply costs over 10-year horizon; permitting timeline data',
    'If feasible: Vault extraction mechanism persists (countries need U.S. minerals). If infeasible: Vault becomes performative (cannot actually supply alternatives to imports) and transitions to Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_mining_feasibility, empirical, 'Feasibility of scaling U.S. domestic mining').

omega_variable(
    recycling_substitution_velocity,
    'How fast will advanced recycling and mineral-reducing technology reduce dependence on primary reserves?',
    'Tracking of recycling cost curves, battery technology roadmaps (cobalt intensity reduction), structural substitution adoption rates; comparison to Vault drawdown timeline',
    'If substitution is rapid (5-10 years): Vault sunset is real and near-term. If slow (30+ years): Vault maintains extraction mechanism longer. If technology plateaus: Vault becomes permanent Snare for dependent nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_substitution_velocity, empirical, 'Velocity of recycling and substitution technology maturation').

omega_variable(
    geopolitical_fragmentation,
    'Will global supply chain fragmentation accelerate or decelerate regional mineral independence efforts?',
    'Analysis of regional sourcing initiatives (EU Critical Raw Materials Act implementation, Indian rare earth processing, African mineral processing investment); trade agreement structure; reshoring vs offshoring data',
    'If fragmentation accelerates: allied nations exit Vault dependency through parallel infrastructure, reducing extraction mechanism. If decelerates: global interdependence increases and Vault maintains power as monopoly reserve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_fragmentation, preference, 'Direction of geopolitical fragmentation and regional mineral independence').

omega_variable(
    reserve_depletion_actual,
    'What is the actual depletion rate of Vault reserves under wartime/high-demand scenarios vs stockpile stability in peacetime?',
    'Simulated drawdown analysis; comparison of reserve coverage ratios to historical supply disruption durations; classified DoD assessments if available',
    'If reserves deplete rapidly (months in crisis): Vault is psychological/coordination tool more than supply solution. If stable (years+): Vault has real supply function and extraction mechanism is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_depletion_actual, empirical, 'Actual depletion dynamics of strategic reserves under stress').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vault_tr_t0, project_vault_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vault_tr_t5, project_vault_2026, theater_ratio, 5, 0.5).
narrative_ontology:measurement(vault_tr_t10, project_vault_2026, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vault_be_t0, project_vault_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vault_be_t5, project_vault_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(vault_be_t10, project_vault_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_vault_2026, resource_allocation).
narrative_ontology:affects_constraint(project_vault_2026, rare_earth_supply_monopoly).
narrative_ontology:affects_constraint(project_vault_2026, cobalt_congo_dependency).
narrative_ontology:affects_constraint(project_vault_2026, lithium_concentration_chile_argentina).
narrative_ontology:affects_constraint(project_vault_2026, semiconductor_supply_chain_resilience).
narrative_ontology:affects_constraint(project_vault_2026, allied_critical_materials_partnership).

% DUAL FORMULATION NOTE:
% Project Vault is downstream of geological constraints (mineral ore distribution) but represents a distinct policy mechanism. Upstream constraints (rare earth monopoly, cobalt concentration) describe structural scarcity; Vault describes institutional response that amplifies extraction. The upstream constraints have lower extractiveness (structural facts); Vault has higher extractiveness (institutional choice). Both stories should be present: one describing the natural scarcity, one describing the policy amplification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(project_vault_2026, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
