% ============================================================================
% CONSTRAINT STORY: environmental_justice_infrastructure_disparity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_environmental_justice_infrastructure_disparity, []).

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
 *   constraint_id: environmental_justice_infrastructure_disparity
 *   human_readable: Environmental Justice Infrastructure Disparity
 *   domain: environmental_policy/social_equity/infrastructure
 *
 * SUMMARY:
 *   Environmental justice infrastructure disparity describes the structural
 *   pattern where low-income communities and communities of color bear
 *   disproportionate burden of industrial pollution, waste facilities, and
 *   inadequate infrastructure while affluent communities retain clean
 *   environments and robust investment. The constraint exhibits all six DR
 *   types from different structural positions: a snare from the perspective
 *   of trapped residents, tangled coordination-extraction from advocacy
 *   organizations, pure coordination from beneficiary municipalities,
 *   performative regulation from agencies, temporary scaffolding from the
 *   environmental justice movement, and apparent natural law from the
 *   analytical observer. The extractiveness value (0.58) reflects that the
 *   constraint operates through both genuine coordination mechanisms
 *   (efficient externalization of industrial costs) and asymmetric burden
 *   distribution. The suppression score (0.72) is high because exit
 *   mechanisms are severely limited: housing affordability constraints trap
 *   residents in polluted areas, regulatory processes exclude community
 *   voices, and alternative infrastructure placement is politically captured.
 *   Theater ratio (0.65) indicates that environmental assessments, zoning
 *   reviews, and regulatory compliance rituals mask the disparity rather than
 *   addressing it — performative compliance substitutes for actual burden
 *   redistribution.
 *
 * KEY AGENTS:
 *   - Low-income residents and communities of color: Primary victims (powerless/trapped) — bear disproportionate environmental burden with no exit mechanism
 *   - Community advocacy organizations: Secondary actors (moderate/constrained) — provide coordination function while experiencing extraction of their labor and political capital
 *   - Affluent municipalities: Primary beneficiaries (institutional/arbitrage) — capture environmental quality and tax base while externalizing pollution costs
 *   - Polluting industries: Primary beneficiaries (institutional/arbitrage) — operate with externalized costs and sympathetic regulatory oversight
 *   - Environmental regulatory agencies: Institutional actors (institutional/constrained) — maintain performative compliance mechanisms that perpetuate disparity
 *   - Environmental justice movement: Organized agents (organized/constrained) — building policy alternatives (cumulative impact assessments, just transition, community benefit agreements) with generational sunset logic
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent disparity as market-driven inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(environmental_justice_infrastructure_disparity, 0.58).
domain_priors:suppression_score(environmental_justice_infrastructure_disparity, 0.72).
domain_priors:theater_ratio(environmental_justice_infrastructure_disparity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(environmental_justice_infrastructure_disparity, extractiveness, 0.58).
narrative_ontology:constraint_metric(environmental_justice_infrastructure_disparity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(environmental_justice_infrastructure_disparity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(environmental_justice_infrastructure_disparity, tangled_rope).
narrative_ontology:human_readable(environmental_justice_infrastructure_disparity, "Environmental Justice Infrastructure Disparity").
narrative_ontology:topic_domain(environmental_justice_infrastructure_disparity, "environmental_policy/social_equity/infrastructure").

domain_priors:requires_active_enforcement(environmental_justice_infrastructure_disparity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(environmental_justice_infrastructure_disparity, affluent_municipalities).
narrative_ontology:constraint_beneficiary(environmental_justice_infrastructure_disparity, polluting_industries).
narrative_ontology:constraint_beneficiary(environmental_justice_infrastructure_disparity, regulatory_capture_actors).
narrative_ontology:constraint_victim(environmental_justice_infrastructure_disparity, low_income_communities).
narrative_ontology:constraint_victim(environmental_justice_infrastructure_disparity, communities_of_color).
narrative_ontology:constraint_victim(environmental_justice_infrastructure_disparity, ecosystem_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RESIDENTS (SNARE) — Trapped by housing affordability constraints, employment proximity, and lack of alternative neighborhoods. Disproportionate exposure to industrial pollution, waste facilities, and inadequate sanitation infrastructure. No viable exit mechanism; bear full cost of environmental degradation while excluded from remediation decisions.
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Constrained by funding limitations, political access barriers, and litigation costs. Experience both coordination function (organizing community self-protection, building political voice) and asymmetric extraction (labor-intensive advocacy generates visibility that benefits more powerful actors while communities remain underserved). Mixed extraction and genuine coordination.
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: AFFLUENT MUNICIPALITIES AND REGULATED INDUSTRIES (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination. Affluent areas externalize pollution costs to low-income neighborhoods while retaining tax base and environmental infrastructure investment. Industries coordinate with sympathetic regulators to maintain operational flexibility. Benefits from infrastructure disparity alignment; arbitrage options available (relocation, regulatory capture alternatives).
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENVIRONMENTAL REGULATORY AGENCIES (PITON) — Agencies maintain performative compliance mechanisms (environmental impact assessments, zoning reviews) that are substantially theatrical. Theater ratio reflects that regulations are enforced asymmetrically: affluent communities successfully challenge industrial facilities while low-income communities lack resources to challenge. Institutional inertia sustains degraded regulatory function despite known disparities.
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL JUSTICE MOVEMENT (SCAFFOLD) — Organized actors (EPA environmental justice office, state-level EJ mandates, climate justice coalitions) view infrastructure disparity as a temporary coordination failure with sunset logic: cumulative impact assessments, just transition frameworks, and mandatory community benefit agreements are building alternative pathways that redistribute environmental infrastructure investment. Low effective extraction due to organizational capacity and visible exit pathways, though implementation lags remain.
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED DISPARITY VIEW (MOUNTAIN) — From a civilizational perspective, infrastructure investment disparities appear to be inevitable consequences of market mechanisms and land value — wealthy areas attract investment, poor areas receive industrial facilities. This naturalizes what is actually a contingent policy choice. The engine will flag this as a false summit, revealing that 'market inevitability' framing obscures active regulatory and investment decisions that could be redirected.
constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(environmental_justice_infrastructure_disparity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(environmental_justice_infrastructure_disparity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(environmental_justice_infrastructure_disparity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(environmental_justice_infrastructure_disparity, TR),
    TR >= 0.70.

:- end_tests(environmental_justice_infrastructure_disparity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint embeds genuine coordination (efficient externalization saves polluting industries significant costs; wealthy communities coordinate to keep facilities away) alongside asymmetric burden distribution (low-income communities have no say in facility placement). The value reflects that extraction is not total — some regulatory protections exist — but is substantial and systematic. The measurement trajectory shows increasing extractiveness over 20 years, indicating that initial regulatory oversight (1970s-1980s) has been eroded by regulatory capture and sprawl patterns that concentrate new facilities in vulnerable areas. Suppression (0.72): High. Multiple barriers prevent exit: housing markets trap low-income residents in polluted areas; regulatory processes lack community participation mechanisms; litigation is resource-prohibitive; political influence is concentrated; media attention is episodic. Barriers are structural and systematic, not incidental. Theater ratio (0.65): Moderate-high. Environmental impact assessments, zoning reviews, Title VI reviews, and EPA environmental justice screening tools create appearance of protective regulation, but enforcement is asymmetric: affluent communities successfully challenge facility siting; low-income communities lack resources to mount challenges. The theatrical component has increased as regulatory language on EJ has expanded without corresponding enforcement intensity. The constraint's paradox: more EJ rhetoric has accompanied higher theater ratio, suggesting performative capture of EJ language.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Affluent municipalities and industries see coordination (Rope) — they are solving the problem of efficient cost externalization. The environmental justice movement sees a temporary problem with a real sunset (Scaffold) — cumulative impact assessments and just transition frameworks could redistribute burden. Regulatory agencies see their own degraded ritual (Piton) — EJ mandates exist but are enforced inconsistently. Community organizations see mixed extraction and coordination (Tangled Rope) — they build political voice but their labor is extracted. Low-income residents see pure extraction (Snare) — they bear environmental costs, have no exit, and are excluded from decisions. The analytical observer risks seeing natural law (Mountain) — market-driven clustering of poor and industrial land — but the structural data reveals contingent policy: zoning amendments, highway routing, industrial incentive placement, and regulatory enforcement are all policy decisions that could be redirected.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary status, victim status, and exit options. Affluent municipalities with arbitrage options (can relocate facilities, influence regulatory process) get low d (high benefit, low cost from their perspective) → negative χ (they perceive coordination). Low-income residents who are trapped (housing constraints, regulatory exclusion, no political voice) get high d (high cost, no benefit) → high χ (they experience maximum extraction). Community organizations are constrained (can organize but face resource and political barriers) and both victims and partial beneficiaries (they build voice but extraction of their labor) → moderate d → moderate χ. Regulatory agencies are constrained (have mandate but face political pressure and institutional inertia) → moderate d, but their perspective (Piton) derives from high theater rather than high χ. The directionality chain correctly captures that the same infrastructure pattern is experienced as beneficial coordination by extractors and harmful snare by targets, with middle-position actors experiencing mixed mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL RESOLUTION: The mandatrophy is resolved by recognizing that the constraint combines genuine coordination (efficient externalization) with asymmetric extraction (burden concentration). Tangled Rope is the correct analytical classification because the constraint cannot be reduced to pure extraction (Snare) — there is a real coordination function that some actors benefit from — nor to pure coordination (Rope) — there is real asymmetric burden distribution. The false summit risk is the 'naturalized disparity' perspective: analytical observers who frame infrastructure disparity as inevitable market outcome (wealthy land expensive, poor land cheap, industry locates where land is cheap) are naturalizing policy contingency. The appearance of naturalness derives from multiple policy decisions stacked over decades: Jim Crow zoning, highway routing through minority neighborhoods, industrial incentive location, regulatory asymmetry. None of these are inevitable; all are reversible. The mandatrophy's resolution is that recognizing the genuine coordination function (which makes Rope appealing) must not obscure the asymmetric burden distribution (which makes Snare undeniable from the victim perspective). Tangled Rope holds both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_policy_path_dependency,
    'To what degree is current infrastructure disparity the result of active current regulatory choices versus historical path dependency from Jim Crow and redlining policies?',
    'Historical trace of infrastructure investment decisions; analysis of zoning amendment patterns and their temporal correlation with regulatory capture versus legacy effects',
    'If primarily legacy (>70%): constraint is partially piton-degraded; active policy reversal could accelerate change. If primarily active regulatory choice (<30% legacy): constraint is more robustly snare/tangled_rope; enforcement mechanisms are currently maintaining disparity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_policy_path_dependency, empirical, 'Degree to which disparity reflects historical path dependency versus active current policy').

omega_variable(
    environmental_justice_mandate_enforceability,
    'Do environmental justice mandates (Title VI disparate impact, EPA EJ screening tools) actually change infrastructure investment patterns or remain largely performative?',
    'Analysis of infrastructure spending 5 years pre- and post-EJ mandate adoption; tracking of facility siting patterns relative to environmental justice communities; comparison of enforcement action intensity in low-income vs affluent areas',
    'If mandates drive real change: theater ratio should decline, extraction moderates, scaffold perspective realistic. If mandates remain performative: theater ratio persists high, extraction mechanism stable, piton classification appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_justice_mandate_enforceability, empirical, 'Whether environmental justice mandates produce material changes in infrastructure investment').

omega_variable(
    community_consent_versus_coercion_boundary,
    'Can communities ''consent'' to facility siting or environmental burden-sharing under conditions of extreme information asymmetry, economic desperation, and regulatory capture?',
    'Analysis of community benefit agreement negotiations; identification of material concessions that shift underlying cost structure versus ceremonial concessions; comparison of bargaining power between community advocates and industry/regulatory actors',
    'If consent is structural impossibility: constraint is snare, not tangled_rope; exit_options should remain ''trapped'' not ''constrained''. If meaningful consent mechanisms exist: tangled_rope classification holds; constrained exit is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_consent_versus_coercion_boundary, conceptual, 'Whether genuine community consent is possible under structural disparity conditions').

omega_variable(
    ecosystem_health_as_victim_status,
    'Should ecosystem health be counted as a distinct victim in the structural decomposition, or is it proxy for human health impacts in affected communities?',
    'Bioregional analysis of ecosystem integrity metrics independent of human health impacts; identification of environmental degradation that persists even after human displacement',
    'If distinct: add ecosystem-specific perspectives and victim declarations. If proxy: ecosystem impacts are secondary to community impacts; no structural change to classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_health_as_victim_status, conceptual, 'Whether ecosystem health is independent victim or proxy for community impacts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(environmental_justice_infrastructure_disparity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(envjust_tr_t0, environmental_justice_infrastructure_disparity, theater_ratio, 0, 0.5).
narrative_ontology:measurement(envjust_tr_t10, environmental_justice_infrastructure_disparity, theater_ratio, 10, 0.6).
narrative_ontology:measurement(envjust_tr_t20, environmental_justice_infrastructure_disparity, theater_ratio, 20, 0.65).
narrative_ontology:measurement(envjust_tr_t5, environmental_justice_infrastructure_disparity, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(envjust_be_t0, environmental_justice_infrastructure_disparity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(envjust_be_t10, environmental_justice_infrastructure_disparity, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(envjust_be_t20, environmental_justice_infrastructure_disparity, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(envjust_be_t5, environmental_justice_infrastructure_disparity, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(environmental_justice_infrastructure_disparity, resource_allocation).
narrative_ontology:boltzmann_floor_override(environmental_justice_infrastructure_disparity, 0.2).
narrative_ontology:affects_constraint(environmental_justice_infrastructure_disparity, regulatory_capture_environmental_agencies).
narrative_ontology:affects_constraint(environmental_justice_infrastructure_disparity, housing_affordability_trap).
narrative_ontology:affects_constraint(environmental_justice_infrastructure_disparity, climate_disaster_disparity).

% DUAL FORMULATION NOTE:
% Environmental justice infrastructure disparity decomposes into multiple structurally distinct constraints: (1) housing_affordability_trap (ε≈0.70, Snare) — economic barriers that trap residents in polluted areas; (2) regulatory_capture_environmental_agencies (ε≈0.42, Tangled Rope) — agencies with EJ mandates that lack enforcement capacity; (3) industrial_facility_siting_bias (ε≈0.55, Tangled Rope) — coordination-plus-extraction in facility placement. This story represents the system-level constraint that binds these subordinate constraints together. The upstream constraint is housing_affordability_trap (the trapping mechanism); the downstream constraint is climate_disaster_disparity (concentrated climate risk in same communities bearing pollution burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(environmental_justice_infrastructure_disparity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
