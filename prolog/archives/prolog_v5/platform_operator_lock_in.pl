% ============================================================================
% CONSTRAINT STORY: platform_operator_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_operator_lock_in, []).

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
 *   constraint_id: platform_operator_lock_in
 *   human_readable: Platform Operator Lock-In Constraint
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Platform operator lock-in represents a structural constraint where
 *   single-operator control of digital infrastructure creates asymmetric
 *   extraction from dependent service providers and end users, despite
 *   genuine coordination functions. The constraint exhibits the full range of
 *   DR types across different structural positions: service providers
 *   experience Snare (trapped by network effects and sunk investment), users
 *   experience Tangled Rope (genuine service benefits alongside extraction),
 *   the platform operator experiences Rope (coordination benefits, arbitrage
 *   exit), regulators experience Tangled Rope (forced to maintain ecosystem
 *   viability while enforcing fairness), competing platforms experience
 *   Tangled Rope (powerful but constrained by network effects), and
 *   analytical observers risk seeing Mountain (naturalizing what is
 *   contingent). The extractiveness has increased over the interval (0.35 →
 *   0.58) as platforms have matured and consolidated, suggesting accumulation
 *   rather than equilibrium. The theater ratio remains moderate (0.42–0.48),
 *   indicating that the coordination function is genuine but not yet severely
 *   degraded by performative compliance.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures network externality rents, controls algorithmic ranking and recommendation, sets terms unilaterally, has exit capacity through rule changes and market expansion
 *   - Dependent Service Providers: Primary victim (powerless/trapped) — small merchants, content creators, service providers with no viable exit; built operations on platform with sunk costs; subject to algorithmic demotion, policy changes, revenue share alterations
 *   - End Users: Secondary victim/beneficiary (moderate/constrained) — benefit from aggregation, discovery, trust mechanisms but experience extraction through data collection, algorithmic manipulation, reduced privacy, monopolistic terms
 *   - Market Competition: Tertiary victim (powerless/constrained) — new entrants face network effects barrier to scale; existing competitors face Lock-in constraint on their own service providers and users
 *   - Regulatory Authorities: Organized agent (organized/constrained) — see both coordination function and extraction; dependent on platform cooperation for enforcement; constrained by need to maintain ecosystem viability
 *   - Alternative Platforms: Powerful competitor (powerful/mobile) — can build alternative infrastructure but constrained by network effects coordination problem
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to network-effect markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_operator_lock_in, 0.58).
domain_priors:suppression_score(platform_operator_lock_in, 0.65).
domain_priors:theater_ratio(platform_operator_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_operator_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_operator_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_operator_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_operator_lock_in, tangled_rope).
narrative_ontology:human_readable(platform_operator_lock_in, "Platform Operator Lock-In Constraint").
narrative_ontology:topic_domain(platform_operator_lock_in, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(platform_operator_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_operator_lock_in, platform_operator).
narrative_ontology:constraint_beneficiary(platform_operator_lock_in, platform_ecosystem_coordination).
narrative_ontology:constraint_victim(platform_operator_lock_in, dependent_service_providers).
narrative_ontology:constraint_victim(platform_operator_lock_in, end_users).
narrative_ontology:constraint_victim(platform_operator_lock_in, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SERVICE PROVIDER (SNARE) — Small businesses, creators, and merchants built operations on platform infrastructure with no viable exit. Data portability is theoretical; network effects make alternative platforms economically inaccessible. Policy changes unilaterally alter revenue terms, algorithmic visibility, or operational rules. Trapped by sunk investment, customer lock-in to platform identity, and lack of interoperable alternatives. Maximum extraction experienced.
constraint_indexing:constraint_classification(platform_operator_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER MOBILE (TANGLED ROPE) — Users benefit from coordination: platform aggregates services, enables discovery, provides payment infrastructure. But also experience extraction: algorithmic ranking, data collection, reduced privacy, monopolistic terms of service. Exit is constrained by network effects and switching costs, but possible through multi-platform adoption or reduced usage. Mixed benefit and extraction.
constraint_indexing:constraint_classification(platform_operator_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Genuine coordination function: platform solves collective action problem of matching supply to demand, enabling discovery, providing trust mechanisms. Beneficiary experiences the constraint as coordination infrastructure. High exit capacity through control over rules, capacity to migrate services, ability to compete across markets. Net beneficiary.
constraint_indexing:constraint_classification(platform_operator_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized actors (competition authorities, data protection regulators) see both coordination function (legitimate market infrastructure) and extraction (monopolistic pricing, data abuse, unfair terms). Constrained by need to maintain platform ecosystem viability while enforcing consumer protection and competition law. Active enforcement creates regulatory capture risk: regulators depend on platform data and operational cooperation, reducing independence.
constraint_indexing:constraint_classification(platform_operator_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALTERNATIVE PLATFORM COMPETITOR (TANGLED ROPE) — Large competitors (e.g., different platform ecosystems, adjacent markets) have mobile exit options and capacity to build competing infrastructure. But network effects create Snare-like extraction even for powerful agents: achieving critical mass on alternative requires reaching threshold density of users and service providers. Constrained by coordination problem rather than operational barriers.
constraint_indexing:constraint_classification(platform_operator_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NETWORK EFFECTS (MOUNTAIN) — From civilizational scope, some lock-in is inherent to network-effect-driven platforms: the coordination function REQUIRES asymmetry (one operator managing rules). The two-sided market structure creates inherent extraction because the operator captures surplus from both sides. However, this risks naturalizing what is contingent on property-rights and interoperability design choices. The classification will trigger false summit detection.
constraint_indexing:constraint_classification(platform_operator_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LEGACY REGULATORY FRAMEWORK (PITON) — Traditional utility regulation, common carrier concepts, and antitrust frameworks still frame policy responses to platforms, but their functional role has largely atrophied as digital regulation has developed. Legacy concepts persist through institutional inertia and legal citation despite reduced explanatory power. Theater ratio: high — regulatory apparatus maintains forms (licensing requirements, rate hearings) that were designed for different infrastructure (telecommunications, railroads) and have limited relevance to algorithm-driven platforms.
constraint_indexing:constraint_classification(platform_operator_lock_in, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_operator_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_operator_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_operator_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_operator_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_operator_lock_in, TR),
    TR >= 0.70.

:- end_tests(platform_operator_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform operator captures surplus from network effects, enjoys control over algorithmic ranking (reducing organic visibility), captures data value, and imposes unilateral terms. However, extractiveness is not at Snare-maximum because genuine coordination function exists — users receive real aggregation services and discovery benefits. The constraint is not extraction masquerading as coordination, but authentic coordination with embedded asymmetry. Suppression (0.65): High. Barriers to exit include network effects (critical mass required for alternative to be viable), sunk investment in platform-specific operations, data lock-in (even with portability, behavioral patterns are platform-specific), and lack of regulatory support for interoperability. However, not total — some service providers maintain multi-platform presence, users can reduce usage, and alternative platforms exist (though with lower liquidity). Theater ratio (0.48): Moderate. Coordination mechanisms are largely functional (matching algorithms, trust systems, payment infrastructure) rather than performative, but policy compliance theater is rising (data transparency reports, fairness audits, algorithmic explainability statements that serve more to manage optics than to enable user control). The theater ratio has remained stable because functional necessity outweighs performative drift so far, but shows upward trend.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates structural perspectival divergence rather than disagreement about facts. The powerless service provider and institutional operator are looking at the same constraint from fundamentally different structural positions. The operator sees coordination (their d is low, f(d) is negative); the provider sees extraction (their d is high, f(d) is high). Neither is wrong — they are both computing chi correctly from their structural position. The gap reveals that the constraint IS extractive in structure (it transfers surplus from dependent providers to the operator) even though it IS genuinely coordinating (it enables market matching that both sides benefit from in the presence of the platform). Tangled Rope diagnosis is structurally sound: authentic coordination WITH asymmetric extraction, not one masquerading as the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) explains perspectival gaps. Base extractiveness is fixed (ε = 0.58). But f(d) varies by agent position: operators have d ≈ 0.08 (beneficiary with arbitrage exit) producing f(d) ≈ -0.12; service providers have d ≈ 0.95 (victim with trapped exit) producing f(d) ≈ 1.42; users have d ≈ 0.52 (mixed) producing f(d) ≈ 0.65. At scope global (σ = 1.2), this yields: operators experience χ ≈ 0.58 × -0.12 × 1.2 ≈ -0.08 (negative, net benefit); service providers experience χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (near-maximum extraction); users experience χ ≈ 0.58 × 0.65 × 1.2 ≈ 0.45 (moderate extraction). The same base constraint produces negative effective extraction for beneficiaries and maximum for trapped victims. This explains why the operator genuinely sees Rope (low chi) while providers see Snare (high chi). No directionality override needed — the derivation chain captures the structural divergence correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled Rope classification resolves potential mandatrophy by declaring BOTH a genuine coordination function AND asymmetric extraction as structural properties. The constraint is not a Snare that calls itself Rope, nor a Rope that extracts like a Snare. It is fundamentally hybrid: it solves a coordination problem (matching supply to demand, trust infrastructure, payment systems) that both operator and users need, AND it asymmetrically extracts surplus toward the operator via network effects and control over rules. The tangled rope gate requires beneficiaries (platform operator + coordination ecosystem) and victims (service providers + market competition) — both are present. The requires_active_enforcement flag is true: the lock-in is maintained by continuous policy decisions (algorithmic ranking, term of service changes, enforcement against interoperability) rather than passive network effects alone. The mandatrophy is resolved by showing that this is not pure coordination masquerading as extraction, but coordination with embedded extraction — the two functions are genuinely intertwined in the platform's structure, not one hidden beneath the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_threshold,
    'At what operator size do network effects become genuinely irreversible vs. merely costly to exit?',
    'Historical analysis of platform switching (e.g., social network migration, payments system transitions); measurement of minimum viable ecosystem size for alternative platforms to achieve critical mass',
    'If threshold is low (<10% of original user base): exit is contingent, not locked-in. Snare classifications may be misdiagnosed as Tangled Rope or worse. If threshold is high (>50%): lock-in is structural even for powerful agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_threshold, empirical, 'Network effects irreversibility threshold').

omega_variable(
    coordination_function_necessity,
    'Is single-operator control inherent to the coordination function, or is it a design choice?',
    'Comparison with alternative architectures: decentralized protocols (blockchain platforms), federation models (open social networks), multi-operator markets (traditional marketplaces). Identify which coordination functions require centralized authority and which can operate on distributed rules.',
    'If single-operator is necessary: Mountain classification may be correct — some extraction is inherent. If contingent: extraction is institutional choice, not natural law, and classifications shift toward Snare/Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether single-operator control is inherent or contingent to coordination').

omega_variable(
    data_portability_effectiveness,
    'Does technical data portability enable exit, or do behavioral lock-in and network effects create trap even with portable data?',
    'Controlled comparison of user behavior and service provider viability when data portability is mandated vs. voluntary; measurement of switching costs before and after data portability interventions',
    'If portable data enables viable exit: lock-in is suppression-based (barriers are operational). If not: lock-in is network-effect-based (inherent to market structure). Different treatment under antitrust law and regulatory design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_effectiveness, empirical, 'Data portability as exit enabler vs. behavioral lock-in persistence').

omega_variable(
    revenue_substitution_possibility,
    'Can platforms derive sustainable revenue from alternative models (not extraction-dependent) — e.g., subscriptions, B2B services, public infrastructure subsidy?',
    'Comparative analysis of platform business models; identification of platforms with non-extraction revenue streams and their market viability; cost modeling for public infrastructure operation',
    'If alternatives are viable: extraction is not inherent, and regulatory intervention to mandate alternative models is feasible. Snare/Tangled Rope classifications are correct. If not viable: platform requires extraction to fund coordination infrastructure — Rope or mild Tangled Rope may be correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_substitution_possibility, empirical, 'Feasibility of non-extraction-dependent platform revenue models').

omega_variable(
    identity_lock_operator_side,
    'Is platform operator behavior constrained by internalized identity (corporate mission, public service framing, ecosystem stewardship) or by material incentives and competitive pressure?',
    'Historical analysis of policy decisions in operators facing regulatory pressure, competitive threat, or profit decline; measurement of decisions aligning with stated mission vs. shareholder/commercial interests',
    'If identity-locked: operator behavior may be more rigid than material analysis suggests — cultural/mission capture could be more durable than reputational incentives. If material-driven: regulatory interventions on financial incentives are more likely to shift behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_operator_side, empirical, 'Operator identity-lock vs. material incentive determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_operator_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_lock_tr_t0, platform_operator_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plat_lock_tr_t5, platform_operator_lock_in, theater_ratio, 5, 0.45).
narrative_ontology:measurement(plat_lock_tr_t10, platform_operator_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(plat_lock_be_t0, platform_operator_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plat_lock_be_t5, platform_operator_lock_in, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(plat_lock_be_t10, platform_operator_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_operator_lock_in, resource_allocation).
narrative_ontology:affects_constraint(platform_operator_lock_in, algorithmic_governance_opacity).
narrative_ontology:affects_constraint(platform_operator_lock_in, data_portability_interoperability).
narrative_ontology:affects_constraint(platform_operator_lock_in, platform_monopoly_network_effects).

% DUAL FORMULATION NOTE:
% Platform lock-in comprises three distinct structural constraints: (1) network effects barrier to exit (affects competing platforms more than dependent service providers), (2) algorithmic governance opacity (affects end users and regulators more than operator), and (3) data lock-in via proprietary format and behavioral patterns (affects service providers most). Each has distinct ε value. The present story models the bundled constraint; decomposition into three stories follows the ε-invariance principle: network effects ε ≈ 0.45 (Rope for some, Snare for others), opacity ε ≈ 0.40 (Piton/Tangled Rope), data lock-in ε ≈ 0.55 (Snare/Tangled Rope). All three are linked as downstream of platform_operator_lock_in, which bundles their effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_operator_lock_in, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
