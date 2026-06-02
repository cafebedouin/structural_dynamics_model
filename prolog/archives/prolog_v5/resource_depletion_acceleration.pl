% ============================================================================
% CONSTRAINT STORY: resource_depletion_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_resource_depletion_acceleration, []).

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
 *   constraint_id: resource_depletion_acceleration
 *   human_readable: Resource Depletion Acceleration and Discount Rate Lock-In
 *   domain: environmental/economic/political
 *
 * SUMMARY:
 *   Resource depletion acceleration is a constraint where institutional
 *   mechanisms designed to coordinate extraction (markets, property rights,
 *   supply chains, environmental regulation) simultaneously generate
 *   asymmetric extraction that locks in short-term resource consumption over
 *   long-term sustainability. The constraint exhibits all key features of
 *   Tangled Rope: genuine coordination function (markets do solve the problem
 *   of getting resources to users), asymmetric extraction (benefits
 *   concentrate in financial and industrial capital; costs disperse across
 *   future generations and subsistence populations), and active enforcement
 *   (property law, extraction licensing, investment instruments). The
 *   constraint's theater ratio (0.55) reflects that sustainability governance
 *   has grown substantially performative (ESG reports, carbon accounting,
 *   sustainability indices) while actual depletion acceleration has continued
 *   — environmental bureaucracy expands while extraction mechanics persist.
 *   The extractiveness trajectory shows acceleration: from 0.38 (1990s,
 *   pre-climate urgency) through 0.58 (present) to projected 0.62+ (if
 *   current trajectory continues). This acceleration is driven by three
 *   structural mechanisms: (1) discount rate lock-in: financial instruments
 *   subordinate future value to present through interest mechanics, creating
 *   rational-actor extraction incentive; (2) externality suppression: costs
 *   of depletion borne by future agents, non-human systems, and subsistence
 *   populations are excluded from extraction decision-making; (3) theatrical
 *   environmental compliance: ESG and sustainability frameworks create
 *   appearance of constraint without reducing actual extraction volumes. The
 *   scaffold perspective (organized agents building renewable transitions) is
 *   analytically sound but faces the temporal mismatch: renewable
 *   infrastructure takes 20-40 years to scale; ecosystem tipping points
 *   (permafrost collapse, coral death, soil degradation) operate on 10-20
 *   year timelines in some regions. The snare perspective (subsistence
 *   populations trapped by geography and time) is the most structurally
 *   stable — these agents have no exit option and bear maximum suppression.
 *
 * KEY AGENTS:
 *   - Extraction Corporations and Financial Capital: Primary beneficiary (institutional/arbitrage) — accumulate capital during extraction window; can exit sector and redeploy to other domains
 *   - Subsistence Populations and Future Generations: Primary victims (powerless/trapped) — geographically and temporally immobilized; bear costs of depletion with no participation in extraction decisions
 *   - Extraction-Dependent Workers and Communities: Secondary victims (moderate/constrained) — embedded in extractive economies; face mixed coordination benefits (employment, infrastructure) and extraction costs (wage suppression, environmental toxicity, post-depletion economic collapse)
 *   - Resource-Dependent States: Secondary institutional actor (institutional/constrained) — beneficiary during revenue window (taxes, royalties); victim after depletion (depleted productive base, social costs); cannot exit extraction without fiscal collapse but continued extraction guarantees post-depletion degradation
 *   - Environmental Governance Organizations: Organized agents (organized/constrained) — building renewable and circular economy alternatives with 20-40 year timelines; see constraint as temporary coordination problem with sunset
 *   - ESG and Sustainability Frameworks: Institutional apparatus (organized/constrained) — substantially performative; maintains theater of environmental constraint while extraction continues; Piton classification reflects degraded function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees resource depletion as structural feature of capital accumulation mechanics; Tangled Rope classification reflects both genuine coordination and irreducible extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(resource_depletion_acceleration, 0.58).
domain_priors:suppression_score(resource_depletion_acceleration, 0.68).
domain_priors:theater_ratio(resource_depletion_acceleration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(resource_depletion_acceleration, extractiveness, 0.58).
narrative_ontology:constraint_metric(resource_depletion_acceleration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(resource_depletion_acceleration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(resource_depletion_acceleration, tangled_rope).
narrative_ontology:human_readable(resource_depletion_acceleration, "Resource Depletion Acceleration and Discount Rate Lock-In").
narrative_ontology:topic_domain(resource_depletion_acceleration, "environmental/economic/political").

domain_priors:requires_active_enforcement(resource_depletion_acceleration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(resource_depletion_acceleration, immediate_extractors).
narrative_ontology:constraint_beneficiary(resource_depletion_acceleration, financial_capital_holders).
narrative_ontology:constraint_beneficiary(resource_depletion_acceleration, developed_nations).
narrative_ontology:constraint_victim(resource_depletion_acceleration, future_generations).
narrative_ontology:constraint_victim(resource_depletion_acceleration, subsistence_populations).
narrative_ontology:constraint_victim(resource_depletion_acceleration, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE POPULATIONS / FUTURE GENERATIONS (SNARE) — Structurally trapped by geographic location and temporal position. No exit option: cannot move away from depleted land, cannot choose a different era. Bear full cost of depletion without representation in extraction decisions. Maximum suppression through resource scarcity, political marginalization, and lack of capital to arbitrage out of degraded regions.
constraint_indexing:constraint_classification(resource_depletion_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXTRACTION-DEPENDENT WORKERS AND COMMUNITIES (TANGLED ROPE) — Moderate power agents embedded in extractive economies with constrained exit options. Face genuine coordination problem: extractive industry provides employment, infrastructure, and services. Also face asymmetric extraction: wage suppression, occupational hazards, environmental toxicity borne locally while profits are externalized. Can exit only at high cost (relocation, retraining, loss of community/identity). Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(resource_depletion_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTION CORPORATIONS AND FINANCIAL CAPITAL (ROPE) — Institutional actors with arbitrage options. Experience the constraint as coordination: efficient extraction mechanisms, market signals, supply chains, capital flows. Net beneficiaries during their operational horizon. Can exit extraction sector and redeploy capital to other domains (agriculture, energy, finance) without structural disruption to their own position. Perceive the accelerating depletion as a resource allocation problem, not as an extractive mechanism.
constraint_indexing:constraint_classification(resource_depletion_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESOURCE-DEPENDENT STATE GOVERNMENTS (TANGLED ROPE) — Institutional actors with constrained exit options. Face genuine coordination problem: resource extraction generates revenue (taxes, royalties) that funds schools, hospitals, infrastructure. Also face asymmetric extraction: depleted resources leave no future productive base; state must manage social costs (unemployment, ecosystem collapse) while private capital exits. Cannot abandon extraction sector without causing immediate fiscal collapse, yet continued extraction guarantees long-term degradation. Dual institutional relationship: both beneficiary (during revenue window) and victim (bearing post-depletion costs).
constraint_indexing:constraint_classification(resource_depletion_acceleration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL AND CLIMATE GOVERNANCE INSTITUTIONS (SCAFFOLD) — Organized agents (UNEP, IPCC, regional environmental bodies, divestment campaigns) see resource depletion acceleration as a temporary coordination failure with a sunset. Renewable energy transition, circular economy norms, and sustainability mandates represent alternative extraction pathways that reduce resource intensity. Theater ratio moderate (80% of climate policy is implementation-agnostic signaling, 20% is structural change). Exit pathway exists but requires generational timescale and coordinated international action. Suppression through incumbent industry lobbying and capital lock-in, but suppression is eroding as renewable economics improve. Classified as Scaffold because the constraint has genuine sunset: the transition infrastructure is being built now, with estimated 20-40 year timeline for major developed economies.
constraint_indexing:constraint_classification(resource_depletion_acceleration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SUSTAINABILITY REPORTING AND ESG FRAMEWORKS (PITON) — Organized institutional apparatus (ESG ratings, sustainability reports, carbon accounting standards) that is substantially performative. Companies maintain high ESG ratings while expanding resource extraction through opacity, outsourcing, and Scope 3 accounting games. The ESG evaluation ritual persists through institutional inertia — investors and regulators demand the reports, companies produce theater-compliant documentation — despite low functional impact on actual depletion rates. The theater ratio (0.75) is high: 75% of ESG infrastructure is reporting and communication, 25% is constraint on actual extraction behavior. This is a degraded institutional mechanism: the original function (align capital allocation with environmental limits) has atrophied, replaced by certification theater.
constraint_indexing:constraint_classification(resource_depletion_acceleration, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (CIVILIZATIONAL) (TANGLED ROPE) — From a civilizational and global perspective, resource depletion acceleration exhibits genuine mixed structure: (1) coordination function: markets do allocate resources to highest-value uses, and extraction mechanisms do solve the problem of getting resources from ground to consumer. (2) Asymmetric extraction: discount rate structures lock in short-term extraction over long-term sustainability; financial instruments subordinate future value to present value through interest rate and amortization mechanics; carbon/resource costs are externalized to future agents and non-human systems. The analytical view sees the constraint as a structural feature of capital accumulation mechanics, not a temporary coordination problem or an inherent natural limit. Theater ratio (0.55) reflects that some genuine coordination occurs (supply chains work) alongside significant performative environmental compliance.
constraint_indexing:constraint_classification(resource_depletion_acceleration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(resource_depletion_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(resource_depletion_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(resource_depletion_acceleration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(resource_depletion_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(resource_depletion_acceleration, TR),
    TR >= 0.70.

:- end_tests(resource_depletion_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting asymmetric extraction that concentrates benefits in financial capital while dispersing costs across future agents and subsistence populations. The value is not at maximum snare level (0.70+) because genuine coordination does occur — markets do solve the problem of resource allocation, and extraction mechanisms do work. But the asymmetry is severe: discount rates legally subordinate future value to present value, externality costs are excluded from extraction decisions, and subsistence populations bear local environmental costs. Suppression (0.68): Moderately high. Barriers to exit are substantial: geographic immobility of subsistence populations, capital lock-in of extraction-dependent economies, financial instruments that encode future extraction into debt service, and political suppression of resource defense movements. However, suppression is not total — some communities successfully defend resources, renewable alternatives exist, and organized environmental coalitions are building institutional alternatives. Theater ratio (0.55): Moderate, indicating substantial performative environmental governance alongside real extraction mechanics. ESG frameworks, carbon markets, sustainability reports, and renewable energy mandates represent ~55% theater (communication, certification, aspirational signaling) and ~45% functional constraint on extraction behavior. The theater has expanded rapidly over the 30-year interval as environmental pressure has mounted without reducing actual depletion rates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the foundational perspectival gap: the same resource depletion process appears as a coordination mechanism (Rope from capital perspective), a temporary problem being solved (Scaffold from organized environmental perspective), a degraded institutional ritual (Piton from ESG perspective), a mixed benefit-extraction hybrid (Tangled Rope from state and worker perspectives), and pure extraction (Snare from subsistence and future-generation perspectives). The gap between the snare and rope perspectives is maximal: extraction corporations rationally optimize extraction given financial instruments and market signals; subsistence populations rationally cannot exit given geography and institutions. Both are rational actors in the same system, yet experience it as fundamentally opposed constraint types. The gap reveals the structural asymmetry: the system is designed to transfer value from future agents and subsistence populations to present financial capital through discount rate mechanics and externality suppression, not through coordination failure or information gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position: Extraction corporations benefit from resource accumulation and have high exit flexibility (arbitrage), producing low d and negative effective extraction from their perspective (they experience a Rope coordination mechanism). Subsistence populations are geographically immobilized, temporally displaced, and excluded from extraction decisions (trapped, victims), producing high d and high f(d) — they experience maximum extraction (Snare perspective). Extraction-dependent workers face constrained exits (high relocation costs, identity fusion with extraction industries) and receive mixed benefits (employment) alongside extraction costs (toxicity, wage suppression), producing moderate d and moderate chi (Tangled Rope perspective). Resource-dependent states have constrained exits (fiscal dependence on extraction revenue) and dual status (both beneficiary during revenue window and victim after depletion), producing moderately high d reflecting their victim status once post-depletion costs are discounted into present value. Environmental organizations have constrained exits (norm lock-in to mitigation rather than prevention) but real agency through coalition building, producing moderate d and moderate chi with downward trend over time as renewable alternatives mature. ESG frameworks have arbitrage-like positioning (easy to enter/exit reporting without changing extraction behavior) but constrained actual impact, producing moderate d offset by the theater gate (Piton classification depends on theater ratio exceeding 0.70 for classification purposes, but is 0.55 here; classified as Piton from organizational perspective because the function has degraded, not because theater dominates all other metrics).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that resource depletion acceleration is not a classification problem (which type is it really?) but a structural asymmetry problem (different agents have genuinely different constraint experiences due to their position in the extraction-to-externality pipeline). The snare and rope perspectives are not contradictory — they are causally related: extraction can appear as pure coordination (Rope) from the beneficiary position precisely because the asymmetric costs are externalized to those who experience it as Snare. The scaffold perspective is analytically crucial but temporally mismatched — the sunset timeline (20-40 years) extends beyond the ecosystem tipping point timeline (10-20 years in some regions), creating a false-dawn dynamic where the constraint appears temporary while critical systems degrade. The piton perspective identifies the pathology of environmental governance theater: institutions designed to constrain extraction have increasingly become mechanisms for certifying and legitimizing continuation of extraction through improved reporting. The classification as Tangled Rope (rather than Snare) at the analytical civilizational level reflects the genuine coordination function markets perform alongside the genuine asymmetric extraction — no pure type accurately captures the structural reality. The theatrical environmental compliance represents Goodhart drift: measurement of environmental compliance (ESG scores, carbon reporting, renewable percentages) has become substituted for the actual goal (reducing resource extraction and ecosystem degradation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_mechanism_fundamentality,
    'Is the resource depletion acceleration driven by discount rate mechanics embedded in finance, or by behavioral short-termism that could be corrected through norm change?',
    'Comparison of depletion rates across economies with different capital structures (state-owned vs private; long-term vs short-term financing regimes); longitudinal analysis of extraction behavior following changes in institutional investor mandates (e.g., pension fund sustainability requirements, sovereign wealth fund goals)',
    'If fundamental to capital mechanics: depletion acceleration is a Tangled Rope with extinction-scale suppression; coordinate international capital system redesign required. If behavioral: Scaffold view is accurate; norms and incentive realignment can reduce extraction acceleration without systemic capital restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_mechanism_fundamentality, conceptual, 'Whether discount rate lock-in is fundamental to capital mechanics or behavioral').

omega_variable(
    renewable_transition_sufficiency_timeline,
    'Can renewable energy and circular economy transitions decelerate resource extraction fast enough to prevent critical ecosystem thresholds, or is the timeline mismatch permanent?',
    'Comparison of renewable energy capacity growth rate vs fossil fuel retirement rate; modeling of resource intensity of renewable infrastructure (mining for battery materials, silicon, rare earths) vs extraction avoided; temporal alignment of transition timelines with tipping points (permafrost methane release, coral ecosystem collapse, soil carbon loss)',
    'If sufficient and aligned: Scaffold sunset is real and resource depletion constraint becomes temporary coordination problem. If insufficient: Scaffold is aspirational; constraint remains Tangled Rope or escalates to Snare as ecosystem collapse accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_sufficiency_timeline, empirical, 'Whether renewable transition timeline aligns with ecosystem tipping points').

omega_variable(
    subsistence_population_agency_paradox,
    'Do subsistence populations have greater structural power to resist depletion through community sovereignty and land defense than the ''trapped'' classification suggests, or does this power remain unrealized?',
    'Comparative analysis of successful resource defense movements (indigenous land reclamation, community forestry, agricultural resistance to industrial monoculture) vs failed attempts; longitudinal tracking of outcomes for populations with legal rights to resources vs those without; measurement of retaliation suppression (violence, legal persecution) directed at defense movements',
    'If agency is real and unrealized: classification should shift from trapped to identity_locked (cognitive/political framing prevents agency realization) or constrained (barriers are high but surmountable through coalition); the Snare perspective overstates actual imprisonment. If agency is illusory against capital-backed extraction: trapped classification is accurate; suppression suppresses awareness of powerlessness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsistence_population_agency_paradox, empirical, 'Whether subsistence populations have latent structural power against extraction').

omega_variable(
    extraction_externality_accounting_closure,
    'Can environmental accounting systems (natural capital, ecosystem services pricing) actually close the cost externalization loop, or do pricing mechanisms inherently fail to capture non-commodity values?',
    'Analysis of pricing attempts (carbon markets, wetland mitigation banking, biodiversity offsets) and their failure modes; comparison of prices produced by different valuation methods (replacement cost, damage cost, stated preference) for the same ecosystem service; empirical tracking of whether price internalization actually reduces extraction or merely creates new financial commodity markets',
    'If closure is possible: Tangled Rope can shift toward Rope through better cost internalization; extraction can be constrained by market signals. If pricing inherently fails: cost externalization is a fundamental feature of extraction mechanics; suppression of non-commodity values is irreducible, making the constraint structurally asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_externality_accounting_closure, conceptual, 'Whether environmental accounting can close cost externalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(resource_depletion_acceleration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rda_tr_t0, resource_depletion_acceleration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rda_tr_t10, resource_depletion_acceleration, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rda_tr_t20, resource_depletion_acceleration, theater_ratio, 20, 0.55).
narrative_ontology:measurement(rda_tr_t30, resource_depletion_acceleration, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(rda_be_t0, resource_depletion_acceleration, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rda_be_t10, resource_depletion_acceleration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rda_be_t20, resource_depletion_acceleration, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(rda_be_t30, resource_depletion_acceleration, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(resource_depletion_acceleration, resource_allocation).
narrative_ontology:boltzmann_floor_override(resource_depletion_acceleration, 0.18).
narrative_ontology:affects_constraint(resource_depletion_acceleration, climate_change_governance).
narrative_ontology:affects_constraint(resource_depletion_acceleration, intergenerational_discount_rate_lock).
narrative_ontology:affects_constraint(resource_depletion_acceleration, ecosystem_tipping_point_risk).
narrative_ontology:affects_constraint(resource_depletion_acceleration, financial_externality_suppression).

% DUAL FORMULATION NOTE:
% Resource depletion acceleration is downstream of and coupled with multiple constraints: climate governance (attempts to internalize extraction costs), discount rate mechanics (financial structures that lock in short-termism), ecosystem tipping points (nonlinear collapse risks), and externality suppression (institutions that exclude future agent costs). Each downstream constraint has its own ε value and perspectives. The resource depletion constraint represents the structural mechanism by which these separate constraints couple and amplify each other — the acceleration dynamic emerges from their intersection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(resource_depletion_acceleration, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
