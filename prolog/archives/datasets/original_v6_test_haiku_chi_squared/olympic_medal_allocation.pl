% ============================================================================
% CONSTRAINT STORY: olympic_medal_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_olympic_medal_allocation, []).

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
 *   constraint_id: olympic_medal_allocation
 *   human_readable: Olympic Medal Allocation System
 *   domain: social/sports_governance
 *
 * SUMMARY:
 *   The Olympic medal allocation system distributes prestige and resources
 *   among nations based on athletic performance measured in a uniform,
 *   universal medal count. The system is justified as meritocratic
 *   coordination — each nation's athletes compete fairly, and medals reflect
 *   performance. However, the structural reality exhibits all characteristics
 *   of a tangled rope hybrid: genuine coordination (Olympics provide
 *   legitimate international competition framework, promote sport
 *   development, generate global engagement) combined with asymmetric
 *   extraction (wealthy nations' resource concentration translates to medal
 *   concentration, prestige accumulation, and broadcasting revenue, while
 *   developing nations' athletes face structural barriers). The constraint
 *   has degraded over four Olympic cycles: extractiveness has increased from
 *   0.35 (when broader participation and post-Cold War optimism reduced
 *   asymmetry perception) to 0.58 (as wealthy nations' dominance became
 *   mathematically entrenched and prestige concentration accelerated).
 *   Theater ratio increased from 0.42 to 0.68 as performance metrics (medal
 *   count) became divorced from functional sport development (most
 *   medal-count growth comes from concentration in a few high-revenue sports,
 *   not from distributed athletic development).
 *
 * KEY AGENTS:
 *   - Peripheral Nation Athletes: Primary victims (powerless/trapped) — face structural resource barriers; extraction through resource starvation
 *   - Minor Sport Athletes: Primary victims (powerless/trapped) — constrained by funding concentration in high-revenue sports
 *   - Wealthy Sports Nations: Primary beneficiaries (powerful/constrained) — capture prestige and medal concentration; exit constrained by Olympic reputation investment
 *   - International Olympic Committee: Secondary beneficiary (institutional/arbitrage) — maintains coordination framework; captures compliance and broadcast value
 *   - Development Coalition: Mixed (organized/constrained) — recognizes both coordination benefit and extraction; constrained by funding dependence
 *   - Host Nation Governments: Tertiary actor (institutional/arbitrage) — pursue prestige but face degraded functional value; persist through institutional inertia
 *   - Analytical Observer: Perspective (analytical/analytical) — risks naturalizing contingent resource structures as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_medal_allocation, 0.58).
domain_priors:suppression_score(olympic_medal_allocation, 0.62).
domain_priors:theater_ratio(olympic_medal_allocation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_medal_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(olympic_medal_allocation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(olympic_medal_allocation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_medal_allocation, tangled_rope).
narrative_ontology:human_readable(olympic_medal_allocation, "Olympic Medal Allocation System").
narrative_ontology:topic_domain(olympic_medal_allocation, "social/sports_governance").

domain_priors:requires_active_enforcement(olympic_medal_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, wealthy_sports_nations).
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, ioc_bureaucracy).
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, host_nation_prestige).
narrative_ontology:constraint_victim(olympic_medal_allocation, developing_nations).
narrative_ontology:constraint_victim(olympic_medal_allocation, peripheral_sports).
narrative_ontology:constraint_victim(olympic_medal_allocation, athlete_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL NATION ATHLETE (SNARE) — Athletes from developing nations face structural barriers: limited training facilities, insufficient coaching, inadequate funding, and talent drain to wealthy nations. The medal system incentivizes concentration of resources in wealthy countries. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Trapped exit; bears full cost of resource asymmetry.
constraint_indexing:constraint_classification(olympic_medal_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINOR SPORT ATHLETE (SNARE) — Athletes in sports with low commercial appeal (race walking, modern pentathlon, badminton in non-Asian regions) face structural disadvantage. Funding flows to high-revenue sports; minor sports are constrained by the medal counting system's indifference to sport development equity. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.91. Trapped; extraction through resource starvation.
constraint_indexing:constraint_classification(olympic_medal_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: WEALTHY SPORTS NATION (TANGLED ROPE) — Rich nations (USA, China, Russia, Japan, Germany) benefit from medal concentration and prestige accumulation, but are also constrained by Olympic governance rules, anti-doping obligations, and reputational risk. Exit is constrained because withdrawing means losing prestige investment. Benefits from coordination (international sport rules enable predictable competition). d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24. Mixed: benefits from coordination + extraction of prestige from weaker nations.
constraint_indexing:constraint_classification(olympic_medal_allocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: IOC BUREAUCRACY (ROPE) — The IOC's interests align with maintaining the medal system as-is: it guarantees participation, creates investment incentives from wealthy nations and hosts, and generates revenue through broadcasting and sponsorship. The IOC experiences the system primarily as a coordination mechanism for managing global competition. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage (sets rules, captures compliance value).
constraint_indexing:constraint_classification(olympic_medal_allocation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPMENT COALITION (TANGLED ROPE) — Developing nations' sports federations, coaches, and athletes' rights organizations recognize both the coordination value (Olympics provides legitimate competition pathway) and the extraction (resource concentration favors wealthy nations). They are constrained by dependence on Olympic funding and broadcast revenue. d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.62. Significant extraction; some coordination benefit from infrastructure investment.
constraint_indexing:constraint_classification(olympic_medal_allocation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HOST NATION GOVERNMENT (PITON) — Host governments participate in Olympic hosting primarily for prestige, but the actual functional value has degraded: hosting costs have exploded, post-Olympic facilities often remain underutilized, and prestige gains are temporary. The commitment persists through institutional inertia (legacy expectations, sunk costs in bid processes). theater_ratio=0.68; much Olympic spectacle is performative (opening ceremonies, medals ceremonies) rather than functionally necessary. d≈0.22, f(d)≈0.12, σ=1.2 → χ≈0.10. Degraded function; persistence through theater.
constraint_indexing:constraint_classification(olympic_medal_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, resource concentration in competitive sport might appear immutable: wealthier nations will always have more athletes, better training, and more medals. This perspective risks naturalizing the contingent institutional structure as an inherent limit. However, the structural data (ε=0.58, suppression=0.62) contradicts a mountain classification — the engine will identify this as a false summit, revealing that 'natural' medal inequality is actually maintained by institutional design choices (funding rules, talent recruitment, doping enforcement asymmetries).
constraint_indexing:constraint_classification(olympic_medal_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_medal_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(olympic_medal_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(olympic_medal_allocation, TR),
    TR >= 0.70.

:- end_tests(olympic_medal_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The 1-to-1 medal counting system directly maps resource capacity to prestige allocation. Wealthy nations with 10x per-athlete funding capacity predictably achieve 10x medal concentration. This is not chance or merit disparity — it is structural incentive design. The system does not adjust for development level, sport infrastructure, or talent pool size. The increase from 0.35 to 0.58 over 40 years reflects that as global inequality increased and wealthy nations' dominance became undeniable, the extraction mechanism became more visible. Suppression (0.62): Multiple vectors maintain resource concentration. Developing nations face barriers to talent identification (no systematic scout networks), training facility access (limited infrastructure), competition access (international tournament entry fees), athlete brain drain (lucrative contracts in wealthy nations), and enforcement asymmetry (doping enforcement consumes disproportionate resources in poor nations, reducing training time). These are not accidental — they are structural to the current system. Theater ratio (0.68): Opening and closing ceremonies, medal presentations, and prestige narratives constitute ~68% of Olympic spectacle. The actual functional outputs (athlete development, sport infrastructure, international goodwill) are secondary to the theatrical performance. The increase from 0.42 to 0.68 reflects that as infrastructure costs exploded and post-Olympic facility utilization declined, host nations increasingly emphasize ceremony over function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a massive perspectival gap between resource-rich and resource-poor perspectives. Wealthy nations and the IOC see primarily rope (coordination benefits, legitimate competition framework, international engagement). Developing nations see primarily snare (extraction through resource concentration, prestige hoarding, career asymmetry). The analytical observer risks seeing mountain (natural law of competition: wealthy nations will always dominate) but the structural data contradicts this — the constraint is contingent on institutional design choices (uniform medal counting, no development-weighted allocation, IOC's revenue model concentrating in wealthy-nation broadcasting). This divergence is diagnostic: if the perspectival gap were due to measurement ambiguity, changing the observable should eliminate it. Instead, changing from medal count to per-capita medals or sport-equity-weighted systems would produce the opposite perspectival gap (developing nations would claim rope/scaffold, wealthy nations would claim snare). The gap is not measurement-dependent; it is structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Peripheral nation athlete: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option within Olympic system. Minor sport athlete: Victim + trapped → d≈0.88, f(d)≈1.32. Maximum extraction; trapped by sport scarcity (Olympics is primary competition venue). Wealthy sports nation: Beneficiary + constrained → d≈0.35, f(d)≈0.35. Moderate extraction beneficiary; constrained because withdrawing from Olympics means losing prestige investment. IOC: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; high arbitrage power (sets all rules, controls compliance verification). Development coalition: Victim + constrained → d≈0.62, f(d)≈0.88. Significant extraction; constrained by dependence on Olympic funding and broadcast revenue share. Host nation: Institutional + arbitrage → d≈0.22, f(d)≈0.12. Low chi despite beneficiary status, because arbitrage power is limited (IOC controls essential terms) and functional benefit is degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as tangled rope (ε=0.58, suppression=0.62, 0.40≤χ≤0.90). This classification resolves the mandatrophy by acknowledging both the genuine coordination function (Olympics provide legitimate international competition, promote sport globally, create development infrastructure) and the genuine asymmetric extraction (resource concentration translates to medal concentration, prestige hoarding, and career opportunity asymmetry). The false summit risk is high: the analytical observer can see the system as a natural law ('wealthier nations naturally have better athletes'), but the structural data confirms contingency. The extractiveness increased from 0.35 to 0.58 as inequality widened — if this were inherent law, extractiveness would have remained constant. The theater ratio increase (0.42 to 0.68) indicates that performative elements are compensating for degraded functional value; host nations increasingly emphasize ceremony because actual sport development outcomes are declining. A pure snare classification would miss the genuine coordination value; a pure rope classification would miss the asymmetric extraction. Tangled rope correctly captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_concentration_threshold,
    'At what threshold of resource concentration (athlete-per-capita training funding ratio) does medal inequality become structurally inevitable rather than contingent on policy?',
    'Comparative analysis of medal distribution across Olympic eras with different resource-sharing policies; correlation between funding equity interventions and medal distribution entropy',
    'If threshold exists at current global inequality: extraction is structural (tangled rope confirmed). If no threshold: current inequality is policy-contingent (scaffold potential exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_concentration_threshold, empirical, 'Threshold of resource concentration determining structural inevitability').

omega_variable(
    talent_development_independence,
    'Can developing nations build Olympic medal capacity through autonomous talent development infrastructure, or is external investment (wealthy nation coaching, facilities, competition access) structurally necessary?',
    'Historical case study of nations that achieved medal count growth without significant external investment; analysis of medal trajectories for Kenya (distance running), Jamaica (sprinting), China (targeted sports)',
    'If autonomous development possible: extraction is contingent on resource gatekeeping (snare confirmed for minor sports). If necessary: dependency is structural, and the snare classification is robust across development models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(talent_development_independence, empirical, 'Whether autonomous talent development is possible in Olympic sports').

omega_variable(
    medal_counting_system_alternatives,
    'Would alternative medal allocation systems (per-capita weighting, sport-equity normalization, cumulative development index) materially reduce extraction asymmetry, or do they merely performatively relocate the inequality?',
    'Simulation of medal rankings under alternative systems; analysis of which nations gain/lose prestige under each system; examination of whether alternative systems attract genuine participation changes',
    'If alternatives reduce inequality: current system is a policy choice (scaffold logic applies, sunset possible). If alternatives merely redistribute prestige: extraction is structural regardless of counting method (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medal_counting_system_alternatives, conceptual, 'Effectiveness of alternative medal allocation systems').

omega_variable(
    suppression_enforcement_asymmetry,
    'Are doping enforcement, eligibility verification, and facility access standards applied uniformly across wealthy and developing nations, or is enforcement itself a vector for extraction?',
    'Comparative analysis of doping investigation resources per nation; audit of facilities compliance verification; examination of suspension patterns by nation wealth and political alignment',
    'If enforcement is asymmetric: suppression is actively maintained (snare confirmed). If enforcement is uniform: suppression is passive (athletes simply lack resources).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_enforcement_asymmetry, empirical, 'Asymmetry in Olympic enforcement standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_medal_allocation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oly_medal_tr_t0, olympic_medal_allocation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(oly_medal_tr_t20, olympic_medal_allocation, theater_ratio, 20, 0.55).
narrative_ontology:measurement(oly_medal_tr_t40, olympic_medal_allocation, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(oly_medal_be_t0, olympic_medal_allocation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oly_medal_be_t20, olympic_medal_allocation, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(oly_medal_be_t40, olympic_medal_allocation, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_medal_allocation, resource_allocation).
narrative_ontology:affects_constraint(olympic_medal_allocation, international_sport_governance).
narrative_ontology:affects_constraint(olympic_medal_allocation, athlete_labor_equity).
narrative_ontology:affects_constraint(olympic_medal_allocation, host_nation_infrastructure).

% DUAL FORMULATION NOTE:
% The Olympic medal allocation system should be decomposed into at least two distinct constraints: (1) the competition structure (who participates, how performance is measured) with ε≈0.25 (rope), and (2) the prestige/resource allocation mechanism (how medals translate to funding, sponsorship, broadcasting revenue) with ε≈0.58 (tangled rope). The current story focuses on the allocation mechanism; the competition structure is upstream and should be analyzed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(olympic_medal_allocation, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
