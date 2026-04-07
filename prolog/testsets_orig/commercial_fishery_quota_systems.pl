% ============================================================================
% CONSTRAINT STORY: commercial_fishery_quota_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commercial_fishery_quota_systems, []).

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
 *   constraint_id: commercial_fishery_quota_systems
 *   human_readable: Commercial Fishery Quota Systems
 *   domain: environmental_regulation/resource_extraction
 *
 * SUMMARY:
 *   Commercial fishery quota systems represent a global institutional
 *   mechanism for managing marine resource access and preventing
 *   tragedy-of-the-commons collapse. These systems coordinate fish stock
 *   management through catch limits and permit allocation, solving a genuine
 *   coordination problem: without managed access, competitive fishing leads
 *   to stock depletion and ecosystem collapse. However, quota allocation
 *   mechanisms concentrate fishing rights among large industrial operators,
 *   systematically exclude small-scale fishers from historical grounds, and
 *   have failed to prevent ecosystem degradation in many regions despite
 *   compliance with catch limits. This creates a hybrid constraint exhibiting
 *   both genuine coordination function and severe asymmetric extraction. The
 *   constraint's evolution shows increasing extractiveness (0.35 → 0.58 over
 *   20 years) as consolidation accelerates, and rising theater ratio (0.38 →
 *   0.52) as regulatory compliance becomes decoupled from actual ecosystem
 *   health. Alternative governance models (co-management with indigenous
 *   communities, ecosystem-based management, marine protected areas)
 *   represent a structural sunset to the traditional industrial quota model
 *   in some regions, supporting the scaffold perspective. Yet in most
 *   jurisdictions, the quota system persists as an inertial institutional
 *   form despite mounting evidence of functional failure, exhibiting piton
 *   characteristics.
 *
 * KEY AGENTS:
 *   - Large Industrial Fishing Fleets: Primary beneficiary (institutional/arbitrage) — receive quota allocation concentration, access to quota trading, and long-term planning security
 *   - Small-Scale Fishers: Primary victim (powerless/trapped) — excluded from quota systems, face legal prohibition on traditional fishing, experience complete economic and geographic immobility
 *   - Fishing-Dependent Communities: Secondary victim (moderate/constrained) — experience both genuine coordination benefit (long-term stock sustainability) and severe extraction (job displacement, cultural disruption)
 *   - Regulatory Agencies: Beneficiary (institutional/arbitrage) — maintain institutional authority through quota administration, control access allocation, benefit from regulatory complexity
 *   - Marine Ecosystem Integrity: Victim (powerless/trapped) — cannot organize or exit; bears cost of continued extraction despite quota compliance
 *   - Environmental and Social Justice Organizations: Organized advocates (organized/constrained) — building alternative governance frameworks (co-management, MPAs) that create sunset pathway
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — must recognize both the genuine coordination function and the systematic extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commercial_fishery_quota_systems, 0.58).
domain_priors:suppression_score(commercial_fishery_quota_systems, 0.65).
domain_priors:theater_ratio(commercial_fishery_quota_systems, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commercial_fishery_quota_systems, tangled_rope).
narrative_ontology:human_readable(commercial_fishery_quota_systems, "Commercial Fishery Quota Systems").
narrative_ontology:topic_domain(commercial_fishery_quota_systems, "environmental_regulation/resource_extraction").

domain_priors:requires_active_enforcement(commercial_fishery_quota_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commercial_fishery_quota_systems, large_industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(commercial_fishery_quota_systems, fishing_rights_holders).
narrative_ontology:constraint_beneficiary(commercial_fishery_quota_systems, regulatory_agencies).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, small_scale_fishers).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, fishing_dependent_communities).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, marine_ecosystem_integrity).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARTISANAL FISHER (SNARE) — Small-scale fishers with no quota allocation face complete exclusion from historical fishing grounds. Material barriers include capital requirements for licensed vessels, legal prohibition on fishing without permits, and economic dependency on fishing for subsistence. No exit options exist: relocation requires abandonment of place-based knowledge, family networks, and cultural identity. Maximum extraction experienced.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FISHING-DEPENDENT COMMUNITY (TANGLED ROPE) — Communities where fishing represents primary employment and cultural identity experience genuine coordination function (quota stabilizes fish populations, providing long-term sustainability) alongside severe extraction (quota allocation patterns concentrate access among large operators, displacing traditional small-scale practices). High suppression through job dependency and geographic isolation; moderate exit costs for community members (relocation burden, loss of cultural continuity). Mixed experience.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDUSTRIAL FISHING OPERATOR (ROPE) — Large-scale operators experience the quota system as coordination: it stabilizes supply chains, provides predictable planning horizons, and enables trading quota among operators. Beneficiary from quota allocation mechanisms that concentrate access. Can arbitrage quota trading, invest in alternative fisheries, or comply with regulations. Low experienced extraction — the constraint solves their coordination problem.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENVIRONMENTAL/SOCIAL COALITION (SCAFFOLD) — Organized agents (conservation NGOs, indigenous rights organizations, fisheries reform advocates) see the quota system as a temporary institutional form with a sunset clause: ecosystem-based management, co-management agreements with indigenous communities, and marine protected areas represent alternative frameworks being gradually implemented. The quota system is being replaced rather than reformed. Medium extraction because organized actors can advocate, lobby, and implement parallel systems.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRADITIONAL REGULATORY FRAMEWORK (PITON) — The quota system itself is increasingly recognized as a degraded institutional form: scientific evidence of ecosystem collapse despite quota compliance indicates that the system's core function (resource management) has atrophied. Regulatory theater persists (quota meetings, stock assessments, enforcement) despite growing evidence that catch limits are set to maintain industrial viability rather than ecosystem health. Theater ratio high because the performance of management continues despite functional failure.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the quota system exhibits genuine coordination (preventing tragedy of the commons through managed access) embedded within severe asymmetric extraction (allocation patterns concentrate benefits among industrial operators, displace small-scale fishers, and fail to prevent ecosystem degradation). The analytical view requires both beneficiary and victim recognition, making tangled rope the proper classification.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commercial_fishery_quota_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commercial_fishery_quota_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commercial_fishery_quota_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commercial_fishery_quota_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commercial_fishery_quota_systems, TR),
    TR >= 0.70.

:- end_tests(commercial_fishery_quota_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The quota system extracts through multiple channels: (1) allocation patterns concentrate rights among large operators, (2) regulatory compliance creates barriers to entry for small-scale fishers, (3) quota trading concentration among capital-intensive players, (4) ecosystem degradation despite compliance indicates the system fails its stated conservation function. The trajectory shows extractiveness rising from 0.35 to 0.58 as consolidation accelerates and ecosystem decline becomes undeniable. Suppression (0.65): High. Multiple suppression mechanisms operate: legal prohibition on unquoted fishing (material barrier), capital requirements for licensed vessels (economic barrier), geographic isolation of fishing communities (exit cost), knowledge-based barriers (licensing requirements), and institutional capture of regulatory processes by industry (access barrier). Small-scale fishers experience near-total suppression; moderate communities experience high suppression through economic dependency. Theater ratio (0.52): Moderate and rising. Quota systems perform regulatory theater — stock assessments, scientific committees, permit hearings — while ecosystem indicators continue degrading in many regions. The theater persists because it maintains legitimacy of the allocation system and provides a performance of conservation that justifies the extraction. Theater ratio increase from 0.38 to 0.52 reflects growing decoupling between compliance performance and actual ecosystem outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals how the same institutional mechanism is experienced radically differently by actors in different structural positions. Industrial operators (institutional/arbitrage) see coordination and planning stability; small-scale fishers (powerless/trapped) see legal prohibition and exclusion from livelihoods. Fishing communities (moderate/constrained) experience both the coordination benefit (long-term sustainability) and the extraction cost (job displacement). Environmental coalitions (organized/constrained) see a temporary institutional form being replaced by more inclusive alternatives. Regulatory agencies (institutional/arbitrage) maintain beneficiary status through control of the allocation mechanism. The analytical observer must acknowledge both the genuine coordination (preventing tragedy of commons) and the systematic extraction (allocation patterns concentrate benefits, displace small-scale operators, fail to prevent ecosystem degradation). The piton perspective emerges at civilizational timescale — the quota system is recognized as degraded (continued compliance despite ecosystem collapse) but persists through institutional inertia and regulatory capture by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural relationship to the extraction flow. Industrial operators with arbitrage options and beneficiary status derive low d (0.10–0.20), producing negative or minimal χ — they experience the system as solving their coordination problem. Small-scale fishers with trapped exit and victim status derive high d (0.90–0.95), producing maximum χ — they experience near-total extraction with no escape. Moderate communities with constrained exit and mixed beneficiary/victim status derive moderate d (0.55–0.65), producing moderate χ — they benefit from sustainability coordination but suffer displacement extraction. Regulatory agencies with beneficiary status and arbitrage exit derive low d (0.15–0.25), producing low χ — they maintain institutional power. Suppression is NOT scaled by power or scope — it is a raw structural property measuring barriers to exit for trapped/constrained agents. Suppression (0.65) reflects high legal, economic, and geographic barriers for small-scale fishers; this value is unscaled by context, applying uniformly across the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in commercial fishery quota systems is resolved by the tangled_rope classification: the system is NOT pure coordination (rope) because beneficiary allocation is asymmetric; it is NOT pure extraction (snare) because genuine coordination function exists (preventing stock collapse). The constraint requires BOTH beneficiary recognition (industrial operators, regulatory agencies) AND victim recognition (small-scale fishers, communities, ecosystems) to classify correctly. From the industrial operator perspective (institutional/arbitrage), the classification appears to be rope — the system solves their coordination problem perfectly. From the small-scale fisher perspective (powerless/trapped), it appears to be snare — pure extraction with no exit. The analytical observer must hold both truths simultaneously: the constraint IS coordination (preventing tragedy of commons) AND extraction (concentrating benefits among industrial operators and displacing small-scale fishers). This simultaneity defines tangled rope. Falsely classifying the system as pure rope would naturalize the allocation asymmetry as coordination cost; falsely classifying as pure snare would deny the genuine sustainability function. The tangled rope classification forces recognition that the system coordinates while it extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allocation_equity_vs_sustainability,
    'Is the quota system''s primary function equity-driven allocation among users or sustainability-driven ecosystem protection? These two functions can diverge radically.',
    'Historical analysis of quota-setting decisions: cases where scientific advice for lower catches was overridden for economic/social reasons vs cases where equity concerns were sacrificed for sustainability',
    'If equity-primary: system is tangled rope with victims being the powerless excluded from allocation. If sustainability-primary: system is snare because allocation patterns actively prevent effective ecosystem management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_equity_vs_sustainability, empirical, 'Whether quota system prioritizes allocation equity or ecosystem sustainability').

omega_variable(
    quota_trading_mechanism_integrity,
    'Does quota trading create genuine flexibility and efficiency (coordination function) or does it concentrate rights among capital-intensive operators and further exclude small-scale fishers (extraction mechanism)?',
    'Analysis of quota ownership concentration over time; tracking of small-scale fisher participation in quota markets; comparison of quota prices against fisher incomes in different regions',
    'If flexible/efficient: tangled rope classification stands. If concentrating: quota system functions as a mechanism for consolidating control, elevating classification toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quota_trading_mechanism_integrity, empirical, 'Whether quota trading creates flexibility or concentrates control').

omega_variable(
    enforcement_cost_burden_distribution,
    'Who bears the costs of quota enforcement (monitoring, surveillance, compliance infrastructure) and how does this burden vary by fisher type and scale?',
    'Cost accounting of compliance infrastructure; analysis of enforcement burden on small-scale vs industrial fishers (time, expense, legal risk); comparison of audit rates by operator size',
    'If burden concentrated on small-scale fishers: suppression levels are higher than measured, and extraction experienced by powerless agents is maximized. If burden distributed equally: suppression metric is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_burden_distribution, empirical, 'Distribution of quota enforcement costs by fisher type').

omega_variable(
    ecosystem_health_trajectory_under_quotas,
    'Are marine ecosystems subject to quota management actually recovering, stabilizing, or continuing to degrade? Is quota compliance actually preventing collapse?',
    'Time-series analysis of ecosystem indicators (species richness, trophic structure, primary productivity) in quota-managed vs unmanaged regions; analysis of cases where quota compliance was high but ecosystem continued degrading',
    'If recovering: coordination function is real. If degrading despite quotas: quota system is theater masking continued extraction. Classification shifts toward piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecosystem_health_trajectory_under_quotas, empirical, 'Ecosystem health trajectory under quota management').

omega_variable(
    indigenous_rights_integration_authenticity,
    'Are co-management agreements with indigenous communities genuine power-sharing arrangements or token inclusion that leaves substantive control with state agencies?',
    'Analysis of decision-making authority in co-management agreements; tracking of indigenous-recommended policy changes that were adopted vs rejected; comparison of indigenous-managed areas vs state-managed areas on both sustainability and equity metrics',
    'If genuine power-sharing: scaffold is confirmed — the system is transitioning toward more inclusive governance. If token: indigenous communities remain trapped, and the sunset clause narrative is false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_rights_integration_authenticity, empirical, 'Authenticity of indigenous co-management integration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commercial_fishery_quota_systems, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fishquota_tr_t0, commercial_fishery_quota_systems, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fishquota_tr_t10, commercial_fishery_quota_systems, theater_ratio, 10, 0.45).
narrative_ontology:measurement(fishquota_tr_t20, commercial_fishery_quota_systems, theater_ratio, 20, 0.52).
narrative_ontology:measurement(fishquota_tr_t5, commercial_fishery_quota_systems, theater_ratio, 5, 0.41).

% Extraction over time
narrative_ontology:measurement(fishquota_be_t0, commercial_fishery_quota_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fishquota_be_t10, commercial_fishery_quota_systems, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fishquota_be_t20, commercial_fishery_quota_systems, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fishquota_be_t5, commercial_fishery_quota_systems, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commercial_fishery_quota_systems, resource_allocation).
narrative_ontology:boltzmann_floor_override(commercial_fishery_quota_systems, 0.18).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, ocean_commons_tragedy).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, indigenous_fishing_rights_suppression).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, marine_ecosystem_collapse).

% DUAL FORMULATION NOTE:
% Commercial fishery quota systems are downstream of the tragedy-of-the-commons problem (ocean_commons_tragedy: ε≈0.80, mountain) and represent one institutional response. The quota system's own extractiveness (0.58) is lower than the pure commons tragedy but higher than optimal coordination would achieve. The system also affects indigenous_fishing_rights_suppression (ε≈0.75, snare) as a specific extraction mechanism — quota allocation systematically excludes indigenous communities from traditional fishing practices. Marine ecosystem collapse is both upstream (driving quota reforms) and downstream (quota system fails to prevent ecosystem degradation despite compliance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commercial_fishery_quota_systems, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
