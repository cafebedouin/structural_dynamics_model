% ============================================================================
% CONSTRAINT STORY: collective_action_problem_general
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_problem_general, []).

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
 *   constraint_id: collective_action_problem_general
 *   human_readable: Collective Action Problem: Coordination Failure and Free-Rider Extraction
 *   domain: economics/political_economy/game_theory
 *
 * SUMMARY:
 *   Collective action problems represent a fundamental tension between
 *   individual rationality and collective welfare. The constraint arises
 *   whenever group members face incentives to free-ride on others'
 *   contributions while benefiting from the collective good. This is distinct
 *   from pure extraction or pure coordination — it contains elements of both,
 *   depending on the observer's structural position and the institutional
 *   context. The constraint manifests identically across domains
 *   (environmental commons, public goods funding, labor unions, open-source
 *   projects, public health cooperation) but exhibits radically different
 *   classifications from different perspectives. The analytical
 *   universalization as 'rope' reflects that collective action problems are
 *   structurally solvable through coordination mechanisms; however, the
 *   empirical manifestations in specific contexts often degrade toward snare
 *   or tangled_rope as group size increases, institutional intermediaries
 *   capture rents, or enforcement mechanisms calcify into theater.
 *
 * KEY AGENTS:
 *   - Individual Contributors: Powerless/constrained participants bearing costs of contribution and uncertainty of others' compliance
 *   - Free-Riders: Beneficiaries of collective goods without bearing proportionate costs; captured by institutional coordination mechanisms
 *   - Organized Coalitions: Mobile agents with capacity to enforce norms and coordinate; experience constraint as tractable problem
 *   - Local Communities: Trapped members of geographically or socially bounded commons with mandatory participation; experience highest suppression
 *   - Institutional Intermediaries: Gatekeepers of coordination mechanisms (governments, NGOs, platforms) extracting rents through enforcer discretion and fee collection
 *   - Analytical Observer: Universal perspective recognizing coordination solutions while noting that institutional capture transforms pure coordination into hybrid extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_problem_general, 0.35).
domain_priors:suppression_score(collective_action_problem_general, 0.42).
domain_priors:theater_ratio(collective_action_problem_general, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_problem_general, extractiveness, 0.35).
narrative_ontology:constraint_metric(collective_action_problem_general, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(collective_action_problem_general, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_problem_general, rope).
narrative_ontology:human_readable(collective_action_problem_general, "Collective Action Problem: Coordination Failure and Free-Rider Extraction").
narrative_ontology:topic_domain(collective_action_problem_general, "economics/political_economy/game_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_problem_general, free_riders).
narrative_ontology:constraint_beneficiary(collective_action_problem_general, defectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONTRIBUTOR (ROPE) — Sees the constraint as a coordination problem where personal defection is individually rational but collectively disastrous. Constrained by cost of contribution and fear of free-riding by others. Experiences the constraint as coordination overhead necessary to maintain collective good, not as extraction.
constraint_indexing:constraint_classification(collective_action_problem_general, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED COALITION (ROPE) — Organized groups with capacity to enforce norms see collective action problems as solvable through coordination mechanisms: monitoring, reputation systems, graduated sanctions. Mobilized agents experience the constraint as tractable coordination, not extraction. Mobile exit options reflect ability to relocate or reformulate coalition boundaries.
constraint_indexing:constraint_classification(collective_action_problem_general, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL COMMUNITY MEMBER (SNARE) — In small-scale commons (local water management, neighborhood maintenance), defection is individually rational and observationally apparent. Trapped members experience the constraint as pure extraction: they invest in collective maintenance while others free-ride, with no exit option (relocation is costly, collective is mandatory). Experiences suppression through social obligation and geographic dependence.
constraint_indexing:constraint_classification(collective_action_problem_general, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: INSTITUTIONAL COORDINATOR (TANGLED ROPE) — Government agencies, NGOs, or platform operators that intermediate collective action have genuine coordination function (solving the problem) but also extract rents through enforcer discretion, fee collection, and agenda-setting. Arbitrage options enable switching between coordination mechanisms. Experiences the constraint as both functional and extractive.
constraint_indexing:constraint_classification(collective_action_problem_general, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: DEGRADED ENFORCEMENT SYSTEM (PITON) — Legacy institutional enforcement (state mandates, union membership requirements, corporate compliance departments) persists through inertia long after informal coordination mechanisms would be more efficient. Theater ratio (0.52 at scale level) reflects performative compliance, monitoring theater, and procedural rituals. Enforcement system sees itself as degraded — maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(collective_action_problem_general, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational scope, collective action problems are fundamental coordination challenges with well-understood solution architectures: repetition, reputation, graduated sanctions, polycentric institutions (Ostrom). Classified as rope because the constraint is solvable through coordination mechanisms with minimal coercion. The problem is real but not inherently extractive — extraction emerges only when coordination breaks down or intermediaries capture the mechanism.
constraint_indexing:constraint_classification(collective_action_problem_general, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_problem_general_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_problem_general, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_problem_general, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_action_problem_general, TR),
    TR >= 0.70.

:- end_tests(collective_action_problem_general_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The free-rider problem itself is not primarily extractive—it is a coordination failure where individual rationality diverges from collective welfare. However, extractiveness emerges when: (1) monitoring costs create enforcer overhead, (2) institutional intermediaries capture rent from providing coordination services, (3) power asymmetries allow some agents to free-ride while others are forced to contribute. At pure coordination level (unmediated small groups), extractiveness approaches 0.10. At institutionally mediated scale, it reaches 0.45+. Base value 0.35 reflects average across institutional contexts. Suppression (0.42): Moderate-high. The constraint operates through social obligation, reputational pressure, mandatory participation rules, and in some contexts legal enforcement. Suppression is not absolute—exit options exist (relocation, group dissolution, defection followed by sanctions) but are costly. Theater ratio (0.38): Moderate. Institutional enforcement systems (government agencies, compliance departments, NGO monitoring) dedicate significant resources to performative activity: audits, reporting, certification, public accountability theater. Theater increases with scale and bureaucratization; lowest in small repeated-game contexts where reputation mechanisms suffice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. Individual contributors see extraction (Snare at local scale); organized coalitions see coordination (Rope). Institutional intermediaries see both functions simultaneously (Tangled Rope)—they genuinely solve the problem AND extract rents. The analytical observer sees a fundamental coordination challenge with solution architectures (Rope at civilizational scale), but empirical instantiations often degrade toward snare or piton depending on institutional design choices and scale. The perspectival gap reveals that collective action problems are not inherently extractive—extraction is a contingent feature of how coordination mechanisms are designed and captured. Pure peer-to-peer coordination (small groups, repetition, transparent payoffs) stays Rope. Institutional intermediation (large scale, delegated enforcement, opaque fees) tends toward Tangled Rope or Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in collective action problems is determined by each agent's structural relationship to the free-riding temptation and enforcement mechanism. Free-riders (beneficiaries with arbitrage options) derive d ≈ 0.15—they experience low effective extraction because their exit option (non-compliance followed by relocation or reformation of the group) is viable. Contributors (powerless/trapped) derive d ≈ 0.85—they bear maximum extraction because they must contribute regardless of others' defection. Organized groups with enforcement capacity derive d ≈ 0.40—mixed position as both coordinators and potential exploiters of the mechanism. The analytical observer (d ≈ 0.72) sees the structure clearly but cannot resolve the empirical ambiguity about which institutional arrangements are solving coordination vs capturing rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution for collective action problems centers on disambiguating coordination function from extraction mechanism. The constraint is genuinely solvable as pure coordination (Rope): Ostrom's work on polycentric institutions, Hirschman's exit-voice framework, and field experiments on graduated sanctions all demonstrate coordination architectures that minimize extraction overhead. However, empirical instantiations frequently degrade toward extraction because: (1) scale defeats peer-to-peer monitoring, (2) institutional intermediaries develop vested interests in enforcement theater, (3) power asymmetries allow privileged actors to free-ride or enforce selectively, (4) information asymmetries about true payoff structures sustain false equilibria. The mandatrophy is resolved not by declaring a single type but by clarifying: at what scale and under what institutional design is this constraint pure coordination (Rope) vs mixed (Tangled Rope) vs extraction (Snare)? The analytical observer's universal rope classification is correct for the abstract problem but insufficient for empirical diagnosis—the real constraint is institutional design choice, not the problem itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    defection_threshold_ambiguity,
    'Does the apparent individual rationality of defection reflect true material incentives or equilibrium selection failure?',
    'Empirical measurement of payoff matrices in field settings; comparison of stated preferences vs revealed behavior when monitoring is transparent; analysis of trust dynamics in repeated games with known interaction futures',
    'If true material incentive: defection is rational and coordination requires enforcement. If equilibrium selection failure: reframing or cheap signals can shift to cooperative equilibrium without enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defection_threshold_ambiguity, empirical, 'Whether defection is materially rational or result of equilibrium selection').

omega_variable(
    group_size_threshold_extraction,
    'At what group size does the constraint transition from pure coordination (Rope) to mixed extraction (Tangled Rope) to pure extraction (Snare)?',
    'Cross-scale comparative analysis: small-group commons (< 50 members), medium associations (50-1000), large institutional contexts (> 1000); measure enforcement overhead and free-rider detection costs as function of group size',
    'If threshold ≤ 100: large-scale collective action inherently contains extraction dimension. If threshold > 500: extraction is contingent on institutional design choice, not size.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(group_size_threshold_extraction, empirical, 'Group size threshold for transition from coordination to extraction').

omega_variable(
    institutional_intermediation_necessity,
    'Is institutional intermediation (formal enforcement, fee collection, agenda-setting) necessary for solving large-scale collective action or does it primarily create rents?',
    'Comparison of decentralized coordination mechanisms (social media networks, open-source communities, Wikipedia) with formally mediated ones (government agencies, NGOs, regulated industries) on efficiency, equity, and sustainability metrics',
    'If necessary: intermediation is functional overhead, tangled_rope classification justified. If contingent: intermediation is rent-seeking, reclassify as snare at large scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_intermediation_necessity, empirical, 'Whether institutional intermediation is necessary or rent-seeking').

omega_variable(
    technology_disruption_trajectory,
    'Are monitoring and enforcement costs declining asymptotically toward zero (making large-scale cooperation possible) or bottlenecking at irreducible costs (making enforcement intermediaries permanent)?',
    'Time series analysis of coordination cost reduction: communications technology (email → Slack → AI-mediated), verification (manual audits → automated monitoring → cryptographic proof), enforcement (legal systems → reputation algorithms → smart contracts)',
    'If declining: future expects rope classification at large scale (pure coordination without extraction). If bottlenecking: extraction mechanism remains permanent feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_disruption_trajectory, empirical, 'Trajectory of monitoring and enforcement cost reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_problem_general, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cap_tr_t0, collective_action_problem_general, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cap_tr_t2, collective_action_problem_general, theater_ratio, 2, 0.35).
narrative_ontology:measurement(cap_tr_t4, collective_action_problem_general, theater_ratio, 4, 0.38).

% Extraction over time
narrative_ontology:measurement(cap_be_t0, collective_action_problem_general, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cap_be_t2, collective_action_problem_general, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(cap_be_t4, collective_action_problem_general, base_extractiveness, 4, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_problem_general, resource_allocation).
narrative_ontology:affects_constraint(collective_action_problem_general, tragedy_of_commons).
narrative_ontology:affects_constraint(collective_action_problem_general, prisoner_dilemma_iterated).
narrative_ontology:affects_constraint(collective_action_problem_general, public_goods_contribution).
narrative_ontology:affects_constraint(collective_action_problem_general, regulatory_capture_commons).

% DUAL FORMULATION NOTE:
% Collective action problem is a meta-constraint that instantiates differently across specific domains (commons management, public goods, labor organization). This story models the general structural pattern; domain-specific instantiations (tragedy_of_commons, public_goods_contribution) have their own extractiveness values and decompose this general pattern into context-specific mechanisms. The general problem enables free-riding extraction only when institutional design permits it—alternative architectures maintain pure coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_action_problem_general, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
