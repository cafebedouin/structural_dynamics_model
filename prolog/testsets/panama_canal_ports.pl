% ============================================================================
% CONSTRAINT STORY: panama_canal_ports
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_panama_canal_ports, []).

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
 *   constraint_id: panama_canal_ports
 *   human_readable: Panama Canal Port Control
 *   domain: political_economy/geopolitical_infrastructure
 *
 * SUMMARY:
 *   Panama Canal port control represents a structural extraction mechanism
 *   operating through geopolitical competition between the United States and
 *   China for hegemonic influence in the Western Hemisphere and global
 *   maritime logistics. The constraint exhibits the full hybrid character of
 *   Tangled Rope: it delivers real coordination benefits (infrastructure
 *   upgrades, increased port capacity, modernization) while simultaneously
 *   extracting strategic autonomy from Panama and eroding the neutrality
 *   mandate codified in the 1977 Torrijos-Carter Treaties. The extractiveness
 *   has increased over the past decade as Chinese port investments (Colón
 *   Container Terminal, Balboa) have grown and as US military and political
 *   pressure has intensified to maintain hemispheric influence. Theater ratio
 *   has declined slightly, reflecting that the geopolitical competition is
 *   increasingly explicit and non-performative — great powers no longer
 *   pretend to respect neutrality when strategic interests are at stake.
 *   Panama's nominal sovereignty over the Canal is real in administrative
 *   form but constrained in practice by dependency on Chinese capital for
 *   port development and on US security guarantees against regional threats.
 *
 * KEY AGENTS:
 *   - United States: Institutional beneficiary (institutional/arbitrage) — maintains logistics advantage and naval positioning; can exit to Caribbean alternatives but Panama route is preferred
 *   - China: Powerful beneficiary (powerful/arbitrage) — gains strategic port positioning and BRI network amplification; can arbitrage through Southeast Asia but Pacific-Atlantic route offers efficiency
 *   - Panama: Powerless victim (powerless/trapped) — nominal sovereignty holder; trapped in great-power competition; bears full cost of strategic vulnerability and autonomy erosion
 *   - Panama's Political-Business Elite: Organized coordinator-extractors (organized/constrained) — benefit from FDI and port contracts; constrained by inability to refuse great-power pressure; experience mixed coordination and extraction
 *   - Transit Shipping / Global Commerce: Powerless victim (powerless/trapped) — dependent on Canal access; cannot exit; bears cost of geopolitical risk and fragmented control
 *   - Panama Canal Authority: Institutional actor maintaining degraded mandate (institutional/constrained) — tasked with enforcing neutrality; theater ratio declining as mandate erodes; piton classification from performative neutrality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(panama_canal_ports, 0.58).
domain_priors:suppression_score(panama_canal_ports, 0.62).
domain_priors:theater_ratio(panama_canal_ports, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(panama_canal_ports, extractiveness, 0.58).
narrative_ontology:constraint_metric(panama_canal_ports, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(panama_canal_ports, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(panama_canal_ports, tangled_rope).
narrative_ontology:human_readable(panama_canal_ports, "Panama Canal Port Control").
narrative_ontology:topic_domain(panama_canal_ports, "political_economy/geopolitical_infrastructure").

domain_priors:requires_active_enforcement(panama_canal_ports).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(panama_canal_ports, us_military_logistics).
narrative_ontology:constraint_beneficiary(panama_canal_ports, chinese_trade_networks).
narrative_ontology:constraint_victim(panama_canal_ports, panama_sovereignty).
narrative_ontology:constraint_victim(panama_canal_ports, transit_shipping_neutrality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PANAMA'S NOMINAL SOVEREIGNTY (SNARE) — Panama recovered nominal canal control in 1999 but faces structural extraction through great-power competition over port access. Cannot exit great-power geopolitical dynamics; bears full cost of strategic positioning by US and Chinese actors. Sovereignty is theatrical — formal control masks structural dependency on foreign capital and security guarantees. Maximum experienced extraction from perspective of a small nation caught between competing hegemons.
constraint_indexing:constraint_classification(panama_canal_ports, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRANSIT SHIPPING / GLOBAL COMMERCE (SNARE) — Cannot exit reliance on Canal transit; faces extraction through fragmented port control and political risk. 5% of global trade transits the Canal. Any disruption or strategic denial costs thousands of shipping lines and their customers. Bearing cost of geopolitical competition with no voice in negotiations. Trapped in dependency on politically contested infrastructure.
constraint_indexing:constraint_classification(panama_canal_ports, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: UNITED STATES MILITARY AND TRADE (ROPE) — Experiences constraint as pure coordination: maintaining US naval access and logistics positioning in Western Hemisphere. Strategic denial of Canal access to competitors is net benefit. Can arbitrage between Caribbean alternatives (though lower-efficiency). Effective extraction runs toward this agent — they are primary beneficiary of constraint maintaining hemispheric influence. Low chi from this perspective — extractiveness is experienced as coordination benefit.
constraint_indexing:constraint_classification(panama_canal_ports, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINA TRADE AND BRI POSITIONING (POWERFUL/ARBITRAGE) (ROPE) — Experiences constraint as coordination opportunity: controlling strategic ports at Canal endpoints amplifies Belt and Road logistics network and reduces shipping time for Pacific-Atlantic trade. Can arbitrage by routing through Southeast Asia if Canal access blocked, but Panama route offers efficiency gains. Extraction flows toward this agent through port development and concessions. Rope classification from beneficiary position with exit capacity.
constraint_indexing:constraint_classification(panama_canal_ports, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PANAMA'S POLITICAL AND BUSINESS ELITE (TANGLED ROPE) — Organized actors that both coordinate infrastructure development and extract rents through port concessions and strategic positioning. Constrained by great-power pressure but also benefit from competition for their endorsement. Receive foreign direct investment, port management contracts, and geopolitical leverage. Experience simultaneous coordination benefit (infrastructure upgrades via Chinese investment, US security guarantees) and extraction cost (strategic vulnerability, reduced autonomy in taxation and regulation). Mixed extraction and coordination — neither pure Rope nor pure Snare.
constraint_indexing:constraint_classification(panama_canal_ports, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: PANAMA CANAL AUTHORITY NEUTRALITY MANDATE (PITON) — The 1977 Torrijos-Carter Treaties and subsequent agreements codify Canal neutrality: equal treatment of all nations. This mandate is increasingly performative as US and Chinese actors selectively pressure port access and security screening. Theater ratio reflects gap between stated neutrality and actual allocation based on geopolitical alignment. The neutrality framework persists through institutional momentum and symbolic importance even as enforcement capability degrades. Piton classification: degraded coordination mechanism maintained through inertia.
constraint_indexing:constraint_classification(panama_canal_ports, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRATEGIC INFRASTRUCTURE (TANGLED ROPE) — From a civilizational/global view, the constraint reflects structural tension between (a) strategic infrastructure benefits from competing-power investment (ports, deepening, modernization) and (b) sovereignty erosion through great-power competition over control. The system both delivers coordination benefit (upgraded infrastructure) and extracts strategic autonomy. Real perspectives all point toward Tangled Rope — the constraint is genuinely hybrid, not a false mountain.
constraint_indexing:constraint_classification(panama_canal_ports, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(panama_canal_ports_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(panama_canal_ports, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(panama_canal_ports, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(panama_canal_ports, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(panama_canal_ports, TR),
    TR >= 0.70.

:- end_tests(panama_canal_ports_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts strategic autonomy from Panama (0.32 baseline → 0.58 present) as Chinese port investment grows and US pressure intensifies. The extraction is not total (Panama retains nominal control, receives FDI benefits, maintains diplomatic options) but significant. From Panama's perspective, extractiveness is higher (~0.75 as Snare); from US/China beneficiary perspectives, it is lower (~0.25 as Rope). The base property (0.58) reflects the average structural extraction across all perspectives weighted by salience. Suppression (0.62): Moderate-high. Panama's alternatives to accepting great-power competition are severely constrained: cannot exit the geopolitical system, cannot deny port access without economic consequences, cannot enforce neutrality against powerful actors. However, suppression is not total — Panama retains formal regulatory authority, can negotiate concession terms, can appeal to international forums. Transit shipping faces high suppression (cannot reroute, faces geopolitical risk), explaining why shipping-sector perspectives classify as Snare. Theater ratio (0.48): Below-average and stable. The constraint is increasingly explicit and non-performative — great-power competition for port control operates through direct investment, military positioning, and selective access rather than through institutional theater. The slight decline (0.55 → 0.48) reflects erosion of the neutrality mandate's symbolic force as geopolitical interests become transparent.
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic perspectival gap lies between Panama (Snare) and the US/China beneficiaries (Rope). Panama experiences maximum extraction through strategic vulnerability and constrained autonomy. The US experiences pure coordination benefit — maintaining hemispheric influence. China experiences coordination benefit with slight extraction risk (US countermeasures). The transit shipping sector experiences Snare identical to Panama — trapped in geopolitical risk with no exit. Panama's political elite experience Tangled Rope because they capture rents (port contracts, FDI) while bearing strategic costs. The Panama Canal Authority experiences Piton because its neutrality mandate is increasingly performative — the institutional machinery persists while its functional enforcement capability erodes. The analytical observer sees the full hybrid system: Tangled Rope is the accurate characterization because both coordination (infrastructure improvements, reduced shipping costs) and asymmetric extraction (strategic autonomy, neutrality erosion) are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each perspective's structural position. The US and China (beneficiaries + arbitrage options) experience low d values (≈0.20-0.30) and negative or low chi, seeing the constraint as coordination (Rope). Panama (victim + trapped exit) experiences high d (≈0.85) and high chi, seeing pure extraction (Snare). Panama's business elite (mixed beneficiary-victim + constrained exit) experience intermediate d (≈0.55-0.60), producing the Tangled Rope classification from their perspective. The analytical observer (high scope, civilizational time horizon, analytical exit) experiences the hybrid character at d ≈ 0.60, confirming Tangled Rope as the system-level classification. The directionality derivation explains why no unified type emerges: different structural positions genuinely experience different constraint characters, and the perspectival gap is not an artifact but a reflection of real extraction asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Tangled Rope classification is not a compromise between conflicting perspectives but a genuine hybrid structure. Mandatrophy would arise if the observer tried to force the constraint into a single type despite the data revealing multiple character: attempting to call it pure Rope (only coordination benefits) would miss the extraction from Panama and transit shipping; attempting to call it pure Snare would miss the real infrastructure improvements and FDI benefits. The Tangled Rope classification accommodates both streams — it requires both beneficiaries and victims (US/China and Panama respectively), active enforcement (geopolitical pressure maintaining the extraction), and asymmetric extraction alongside coordination benefit. The key structural feature confirming Tangled Rope is that removing the extraction mechanism (great-power competition) would undermine the coordination benefit itself — the ports are valuable precisely because they are strategically contested. The neutrality mandate (Piton perspective) reveals itself as a false legitimating frame once the underlying hybrid extraction is understood.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chinese_debt_dependency_degree,
    'To what degree is Panama''s port development dependency on Chinese capital functionally equivalent to debt-trap infrastructure extraction, versus legitimate commercial investment?',
    'Comparative analysis of port concession terms: debt-service ratios, collateral claims, operational control, revenue capture schedules; comparison with similar ports in India, Sri Lanka, Tanzania for BRI signature patterns',
    'If debt-trap: constraint moves toward pure Snare for Panama. If legitimate investment: Tangled Rope characterization stands. Classification shifts by 0.15-0.30 in base extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_debt_dependency_degree, empirical, 'Whether Chinese port investment functions as debt-trap extraction or legitimate development').

omega_variable(
    us_military_denial_willingness,
    'Under what conditions would the US exercise military denial of Canal access? How credible is that threat?',
    'Analysis of historical US interventions in hemispheric affairs, naval doctrine statements, contingency planning leaks; scenario modeling of China-US conflict escalation thresholds',
    'If credible and low threshold: suppression remains high (0.62). If high threshold or low credibility: suppression drops to 0.40-0.45, reclassifying as lower Rope. Changes chi calculation significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_military_denial_willingness, empirical, 'Credibility and threshold of US military denial of Canal access').

omega_variable(
    panama_coalition_agency_capacity,
    'Can Panama''s government organize an independent coalition (African Union, Non-Aligned Movement, Caribbean nations) to defend neutrality against great-power pressure, or is agency capacity fundamentally constrained by dependency?',
    'Analysis of Panama''s diplomatic statements, voting patterns, coalition-building efforts at UN and regional forums; comparison with other small-state responses to great-power competition (Singapore, Vietnam models)',
    'If coalition agency emerges: Panama''s power classification upgrades to ''organized'', exit options shift to ''mobile'', chi drops significantly. If constrained: Snare classification is confirmed for Panama perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panama_coalition_agency_capacity, empirical, 'Whether Panama can build coalition to defend neutrality independently').

omega_variable(
    alternative_transit_route_feasibility,
    'How near-term and cost-effective are alternative transpolar, expanded Suez, or Nicaragua canal alternatives? Do they reduce Panama''s structural chokepoint status?',
    'Engineering and economic feasibility studies: Arctic shipping season duration, Suez expansion capacity, Nicaragua canal project status; cost comparison with Canal transit for major shipping lanes',
    'If alternatives become viable within 10-20 years: constraint transitions toward Scaffold (sunset logic). If Panama remains dominant: extraction pressure remains stable or increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transit_route_feasibility, empirical, 'Feasibility of alternative transit routes reducing Panama''s chokepoint role').

omega_variable(
    neutrality_mandate_legal_enforceability,
    'Do the Torrijos-Carter Treaties contain enforcement mechanisms sufficient to prevent selective access denial by great powers? What remedies exist if violated?',
    'International law analysis of treaty text and dispute resolution history; comparison with other infrastructure-neutrality agreements (Suez, Belgian neutrality, Antarctic Treaty)',
    'If enforceable: Panama has real exit option (court appeal), reclassifying as Tangled Rope or even Rope. If toothless: Snare classification for Panama is confirmed, suppression increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_mandate_legal_enforceability, conceptual, 'Legal enforceability of Canal neutrality mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(panama_canal_ports, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcp_tr_t0, panama_canal_ports, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pcp_tr_t5, panama_canal_ports, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pcp_tr_t10, panama_canal_ports, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(pcp_be_t0, panama_canal_ports, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pcp_be_t5, panama_canal_ports, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pcp_be_t10, panama_canal_ports, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(panama_canal_ports, global_infrastructure).
narrative_ontology:affects_constraint(panama_canal_ports, us_china_hegemonic_competition).
narrative_ontology:affects_constraint(panama_canal_ports, central_american_sovereignty_constraints).
narrative_ontology:affects_constraint(panama_canal_ports, global_maritime_logistics_dependency).
narrative_ontology:affects_constraint(panama_canal_ports, bri_geopolitical_extraction).

% DUAL FORMULATION NOTE:
% Panama Canal port control is downstream of the structural US-China hegemonic competition (affects_constraints) and upstream of specific country-level sovereignty constraints in Central America. The constraint exists at the infrastructure chokepoint level, where coordination benefits (efficiency) and extraction mechanisms (strategic denial) are structurally coupled. Cannot decompose into separate 'coordination' and 'extraction' stories without losing the essential hybrid character.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(panama_canal_ports, powerful, 0.25).
constraint_indexing:directionality_override(panama_canal_ports, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
