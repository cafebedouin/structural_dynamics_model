% ============================================================================
% CONSTRAINT STORY: network_effects_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effects_market_concentration, []).

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
 *   constraint_id: network_effects_market_concentration
 *   human_readable: Network Effects Market Concentration
 *   domain: economic/technology/competition
 *
 * SUMMARY:
 *   Network effects create a structural tension between genuine coordination
 *   benefits (connecting users solves a real matching problem) and market
 *   concentration with extractive outcomes (monopoly incumbent raises prices
 *   and reduces innovation discipline because competitors cannot enter). The
 *   constraint exhibits classically hybrid properties: real coordination
 *   function coupled with asymmetric extraction. The incumbent platform
 *   experiences the constraint as pure coordination — network effects are how
 *   the service creates value. New entrants experience it as a trap with no
 *   exit. Users experience it as forced loyalty: switching costs are
 *   prohibitive, but the alternative (not participating) is worse. Regulators
 *   experience it as a coordination problem that creates a second-order
 *   extraction problem: monopoly pricing and reduced choice. The theater
 *   ratio reflects that platform incumbents invest heavily in maintaining
 *   perceived network dominance (network effects narrative, brand moat
 *   messaging, social proof signaling) alongside genuine network
 *   externalities. The measurement trajectory shows extractiveness increasing
 *   as markets mature — early markets have real competition; as one player
 *   achieves critical mass, new entrants face exponentially rising switching
 *   costs and users face exponentially rising lock-in.
 *
 * KEY AGENTS:
 *   - Incumbent Platform Operator: Primary beneficiary (institutional/arbitrage) — captures monopoly rents enabled by network lock-in; experiences constraint as coordination mechanism
 *   - New Entrant Competitor: Primary victim (powerless/trapped) — cannot overcome network barrier regardless of product quality; functionally excluded from market
 *   - Locked-In User: Primary victim (powerless/constrained) — bears extraction in the form of monopoly pricing, reduced service quality, privacy extraction; switching costs are prohibitive
 *   - Consumer Switching Capacity: Structural victim (systemic) — network effects convert what should be a contestable market into a durable monopoly; competitive discipline is eliminated
 *   - Niche Competitor: Secondary actor (moderate/constrained) — can survive in underserved segments but cannot compete for mainstream market
 *   - Regulatory Coalition: Organized actor (organized/constrained) — sees both coordination benefit and extraction; interventions create their own friction costs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable market laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effects_market_concentration, 0.58).
domain_priors:suppression_score(network_effects_market_concentration, 0.62).
domain_priors:theater_ratio(network_effects_market_concentration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effects_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_effects_market_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(network_effects_market_concentration, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effects_market_concentration, tangled_rope).
narrative_ontology:human_readable(network_effects_market_concentration, "Network Effects Market Concentration").
narrative_ontology:topic_domain(network_effects_market_concentration, "economic/technology/competition").

domain_priors:requires_active_enforcement(network_effects_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effects_market_concentration, incumbent_platform_operator).
narrative_ontology:constraint_victim(network_effects_market_concentration, new_entrants).
narrative_ontology:constraint_victim(network_effects_market_concentration, consumer_switching_capacity).
narrative_ontology:constraint_victim(network_effects_market_concentration, competitive_market_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW ENTRANT / EXCLUDED COMPETITOR (SNARE) — A potential competitor with superior technology cannot enter the market. Network effects create a trap: the incumbent's value grows with each user, making the barrier to entry insurmountable regardless of product quality. Exit is impossible — the competitor either accepts market exclusion or abandons the entire sector. Maximum extraction experienced by those locked out of participation.
constraint_indexing:constraint_classification(network_effects_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOCKED-IN USER (SNARE) — A user on the dominant platform pays prices or accepts terms that would not survive competition. Switching costs are prohibitive: losing the network externality (all social connections, established workflows, data history) is equivalent to losing the primary value of the service. High suppression: exit exists in formal law but is crushing in practice. Sustained extraction without compensating coordination benefit.
constraint_indexing:constraint_classification(network_effects_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NICHE COMPETITOR (TANGLED ROPE) — A competitor can survive in an underserved segment (vertical-specific, regional, or demographic niche) where network effects have not yet dominated. This agent experiences genuine coordination benefit from the dominant platform's ecosystem (APIs, standards, developer community) alongside extraction: locked out of mainstream growth by network moat. Mixed extraction and coordination.
constraint_indexing:constraint_classification(network_effects_market_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: connecting users to users is the core value proposition. Network effects are the mechanism that solves the coordination problem (getting the critical mass for the service to be useful). The incumbent benefits from this coordination and experiences it as legitimate. Suppression and extraction are low from this perspective — the constraint is the solution, not the problem.
constraint_indexing:constraint_classification(network_effects_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY / COMPETITION AUTHORITY (TANGLED ROPE) — Organized state actors see both coordination benefit (network effects solve real user-matching problems) and extraction (market concentration undermines competitive discipline). Regulatory action creates its own extraction: compliance costs, innovation slowdown, friction in the system. Medium effective extraction because organized agents have exit paths and agency — regulation can modify the constraint even if completely eliminating network effects is infeasible.
constraint_indexing:constraint_classification(network_effects_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW CLAIM (MOUNTAIN) — From a civilizational perspective, network effects are sometimes presented as an immutable law: services with positive externalities naturally consolidate to monopoly because of mathematics, not because of extractive institutional design. However, the structural data contradicts the mountain classification — historical counter-examples (MySpace to Facebook, Friendster's obsolescence, protocol-level platforms like email and HTTP that resist consolidation) show that network effects are contingent on specific design choices, not laws of physics. The 'inevitable monopoly' framing naturalizes what is actually a contingent institutional arrangement subject to regulatory and technological intervention.
constraint_indexing:constraint_classification(network_effects_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effects_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects_market_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effects_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(network_effects_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Network effects create genuine value for users, but as markets mature, incumbent operators extract rents via pricing power, reduced service quality, privacy extraction, and leverage on complementary markets. The 0.58 reflects that the constraint solves a real coordination problem (value ~0.35 at market entry) but has degraded into rent extraction (value ~0.58 at market maturity). Suppression (0.62): High. Barriers to exit for users are severe: social connections, data history, established workflows, and ecosystem integration create switching costs that are only partially surmountable. Regulatory barriers (data portability requirements are often weak or absent) and technical barriers (proprietary data formats, closed APIs) compound user lock-in. Niche competitors face product-market suppression (network effects disadvantage smaller services) and resource suppression (cannot compete with incumbent's scale economies). Theater ratio (0.45): Moderate. Platform operators invest significantly in maintaining perceived network dominance through brand narratives, social proof mechanisms, and 'platform moat' messaging. However, this is paired with genuine network coordination mechanisms. The theater reflects communicative effort to reinforce lock-in narrative, distinct from the actual network effects. The measurement trajectory shows theater increasing as markets mature — proportionally more effort goes into maintaining the narrative that the network is dominant (and therefore switching is futile) versus actual coordination improvements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the fundamental gap between beneficiary and victim perspectives. The incumbent platform (rope) experiences network effects as the core coordination mechanism that solved the matching problem and created immense social value. New entrants (snare) experience network effects as an insurmountable barrier that excludes them regardless of product merit. Users (snare) experience network effects as lock-in: they depend on the service because of critical mass, not because of inherent superiority. Regulators (tangled rope) see both sides: real coordination value but also real extraction via monopoly rent. The perspectival gap reveals that 'network effects' is not a single constraint but a framing that obscures two structurally distinct mechanisms: (1) genuine coordination of many agents (rope function) and (2) market concentration and lock-in (snare function). Whether network effects consolidate to monopoly or remain fragmented depends on institutional choices (interoperability, data portability, regulatory oversight) not on mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain produces different effective extraction values (χ) for the same base extractiveness (0.58) depending on the agent's structural position. Beneficiaries with arbitrage exit (incumbent) experience low or negative χ. Victims with trapped exit (new entrants) experience maximum χ. Constrained agents (users, niche competitors) experience medium-to-high χ. Organized agents (regulators) experience moderate χ because they have agency and alternative interventions. This range (from negative to 0.78 effective extraction) is the perspectival gap: the same constraint is experienced as a solution by one agent and as a trap by another. The gap is structural, not perspectival in the weak sense — it reflects real differences in power, exit options, and relationship to the constraint flow.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as tangled rope because it exhibits (1) genuine coordination function (network effects solve the user-matching problem), (2) asymmetric extraction (incumbents extract rents from locked-in users and excluded competitors), and (3) active enforcement (platforms invest in maintaining API restrictions, proprietary data formats, and switching-cost engineering to sustain lock-in). The mandatrophy is resolved by recognizing that network effects are NOT pure coordination (rope) despite the incumbent's framing — they are coordination coupled with extractive lock-in mechanism. The measurement trajectory confirms this: extractiveness increases over the market lifecycle as coordination becomes durable and the incumbent converts coordination value into extraction value. At market entry (t=0), ε ≈ 0.35 (mostly coordination, minimal extraction). At market maturity (t=10), ε ≈ 0.58 (coordination still present, but extraction dominant). The false summit (analytical mountain) is ruled out by empirical contradiction: email and HTTP are network-effect industries that did not consolidate to monopoly because institutional choices (open protocols, non-discriminatory access, standardization) prevented lock-in conversion. The constraint is not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_network_effects,
    'Are network effects in this market endogenous (requiring genuine coordination of users) or exogenous (artificially locked in via proprietary data formats, API restrictions, or switching-cost engineering)?',
    'Empirical analysis: Can a competitor offer the same utility without the network? Can users export their data and social graphs? Are interoperability APIs exposed or proprietary? Historical analysis: Did the network effect arise organically or was it engineered post-hoc?',
    'If endogenous: true coordination problem, extraction is moderate, constraint is structurally robust (Rope from most perspectives). If exogenous: artificial lock-in, extraction is severe, constraint is regulatory target (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_network_effects, empirical, 'Whether network effects arise from genuine user coordination or artificial lock-in').

omega_variable(
    interoperability_feasibility,
    'Is technical interoperability (allowing users to bring their social graph and data to competitors) structurally feasible for this service category?',
    'Technical feasibility analysis: Can user accounts, social connections, and content be ported to alternative platforms? Are there standardized protocols or data formats that would enable this? Precedent analysis: email and HTTP support multiple competing providers without losing network value — what makes this service different?',
    'If feasible: regulatory intervention via interoperability mandates converts the constraint from Snare (for users) to Rope (network effects remain but extraction decouples from monopoly). If infeasible: natural monopoly claim has more merit, but structural data suggests infeasibility is often engineered rather than inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Technical feasibility of interoperability standards').

omega_variable(
    two_sided_market_asymmetry,
    'Are network effects symmetric across user groups (all users benefit equally from scale) or asymmetric (one side — advertisers, sellers, creators — extracts value from the other side''s network lock-in)?',
    'Economic analysis: Consumer welfare vs advertiser/merchant surplus over time. Pricing asymmetry: are users charged while the other side is subsidized to lock users in? Regulatory data: are competition authorities treating this as monopoly leverage on one side using network lock-in on the other?',
    'If symmetric: genuine coordination, Rope classification more defensible from both sides. If asymmetric: one side (typically users/consumers) is trapped via network effects to extract from the other side (advertisers/merchants) — converts Rope to Snare for the locked-in side.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_sided_market_asymmetry, empirical, 'Symmetry of network effects across different user groups').

omega_variable(
    winner_take_most_inevitability,
    'Is the market''s concentration into a single dominant platform inevitable (given network effects) or contingent on specific institutional decisions (regulation, platform governance, technical design)?',
    'Counterfactual analysis: What would the market look like under different regulatory regimes (e.g., forced interoperability, data portability, non-discriminatory API access)? Historical analysis: Did markets with similar network-effect fundamentals (email, web browsers, instant messaging) consolidate to monopoly or remain fragmented? Technological analysis: What design choices (closed vs open protocols, proprietary vs standardized data) drive consolidation?',
    'If inevitable: mountain classification has merit, but violates empirical contradiction test (email, HTTP). If contingent: constraint is structural choice, regulatory target, and Tangled Rope or Snare classification is correct. This omega resolves the false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(winner_take_most_inevitability, conceptual, 'Whether market concentration is inevitable or contingent on institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effects_market_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effects_market_concentration, theater_ratio, 0, 0.3).
narrative_ontology:measurement(neteff_tr_t5, network_effects_market_concentration, theater_ratio, 5, 0.38).
narrative_ontology:measurement(neteff_tr_t10, network_effects_market_concentration, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effects_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(neteff_be_t5, network_effects_market_concentration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(neteff_be_t10, network_effects_market_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effects_market_concentration, resource_allocation).
narrative_ontology:affects_constraint(network_effects_market_concentration, platform_data_portability).
narrative_ontology:affects_constraint(network_effects_market_concentration, api_interoperability_standards).
narrative_ontology:affects_constraint(network_effects_market_concentration, two_sided_market_leverage).

% DUAL FORMULATION NOTE:
% Network effects market concentration decomposes into three structurally distinct constraints: (1) genuine network externality (users benefit from matching, coordination value), (2) lock-in mechanism (switching costs, data lock-in, proprietary formats), and (3) monopoly leverage (pricing power, reduced innovation, cross-market extraction). Each has distinct ε values and structural mechanisms. A single story captures the hybrid, but downstream constraints examine each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effects_market_concentration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
