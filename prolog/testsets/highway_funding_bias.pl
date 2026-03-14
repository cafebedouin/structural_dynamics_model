% ============================================================================
% CONSTRAINT STORY: highway_funding_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_highway_funding_bias, []).

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
 *   constraint_id: highway_funding_bias
 *   human_readable: Highway Funding Bias and Urban Transportation Inequality
 *   domain: transportation_policy/infrastructure/urban_planning
 *
 * SUMMARY:
 *   Highway funding bias in the United States represents a structural
 *   constraint where Federal and state transportation spending systematically
 *   prioritizes automobile infrastructure over transit, pedestrian
 *   infrastructure, and local mobility systems. Beginning with the
 *   Federal-Aid Highway Act of 1956, this bias has become institutionalized
 *   through the Highway Trust Fund mechanism, gasoline tax earmarking, and
 *   deeply entrenched political coalitions. The constraint exhibits the full
 *   DR spectrum: for transit-dependent urban residents it is a pure snare
 *   (trapped with no alternatives); for suburban commuters it is genuine
 *   coordination (rope); for the broader sustainability movement it is a
 *   temporary institutional arrangement with an approaching sunset
 *   (scaffold); for the Federal highway apparatus it is an increasingly
 *   theatrical ritual (piton). The base extractiveness (0.58) reflects that
 *   the constraint produces asymmetric costs and benefits that are not offset
 *   by equivalent coordination gains. The suppression (0.52) captures
 *   material barriers to alternatives: transit capital requirements, urban
 *   parking mandates, zoning restrictions that mandate automobile dependence.
 *   Theater ratio (0.65) reflects that political justifications for highway
 *   spending (maintenance, congestion reduction, safety) increasingly diverge
 *   from actual spending patterns (capacity expansion, sprawl-enabling
 *   development).
 *
 * KEY AGENTS:
 *   - Transit-Dependent Urban Residents: Primary victims (powerless/trapped) — lack vehicle access or affordable transit; structurally locked into deteriorating bus systems and walking/cycling on hostile infrastructure
 *   - Suburban-Exurban Commuters: Primary beneficiaries (institutional/arbitrage) — access to dispersed suburban housing + urban employment enabled by highway subsidies; captured price signal benefits
 *   - Automotive Industry & Petroleum Interests: Secondary beneficiaries (institutional/arbitrage) — direct profit from highway-dependent mobility system; lobbying power to maintain bias
 *   - Highway Construction & Engineering Firms: Secondary beneficiaries (institutional/arbitrage) — recurring contracts from capacity expansion and maintenance; concentrated wealth from dispersed taxpayer funding
 *   - Inner-Ring Suburban Residents: Secondary victims (moderate/constrained) — local congestion costs from capacity expansion; loss of pedestrian environments; some transit access loss
 *   - Urban Core Property Owners: Mixed (powerful/mobile) — gentrification pressure from highway accessibility disparities creates both displacement costs and property value gains
 *   - Climate & Urban Sustainability Coalition: Organized agents (organized/mobile) — building alternative policies (transit-oriented development, parking reform, electric vehicle transition) with sunset trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(highway_funding_bias, 0.58).
domain_priors:suppression_score(highway_funding_bias, 0.52).
domain_priors:theater_ratio(highway_funding_bias, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(highway_funding_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(highway_funding_bias, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(highway_funding_bias, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(highway_funding_bias, tangled_rope).
narrative_ontology:human_readable(highway_funding_bias, "Highway Funding Bias and Urban Transportation Inequality").
narrative_ontology:topic_domain(highway_funding_bias, "transportation_policy/infrastructure/urban_planning").

domain_priors:requires_active_enforcement(highway_funding_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(highway_funding_bias, suburban_commuters).
narrative_ontology:constraint_beneficiary(highway_funding_bias, automotive_industry).
narrative_ontology:constraint_beneficiary(highway_funding_bias, highway_construction_contractors).
narrative_ontology:constraint_beneficiary(highway_funding_bias, petroleum_interests).
narrative_ontology:constraint_victim(highway_funding_bias, transit_dependent_populations).
narrative_ontology:constraint_victim(highway_funding_bias, urban_core_residents).
narrative_ontology:constraint_victim(highway_funding_bias, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSIT-DEPENDENT URBAN RESIDENTS (SNARE) — Structurally locked into deteriorating public transit as highway funding diverts resources from buses, rail, and pedestrian infrastructure. Cannot exit through personal vehicle ownership (cost, storage, parking); cannot exit through transit improvement (funding systematically directed elsewhere). Bears extraction in form of longer commutes, reduced mobility, health costs from congestion pollution, gentrification driven by highway accessibility disparities. Zero degrees of freedom.
constraint_indexing:constraint_classification(highway_funding_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INNER-RING SUBURBAN RESIDENTS (TANGLED ROPE) — Experience genuine coordination benefit (highway access enables suburban living pattern) alongside extraction (highway funding crowds out transit options for those without cars; highway expansion causes local congestion and property damage). Constrained by housing affordability and school district lock-in. Moderate experienced extraction — some benefit, high cost.
constraint_indexing:constraint_classification(highway_funding_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUBURBAN-EXURBAN COMMUTERS AND AUTO INTERESTS (ROPE) — Primary beneficiaries. Highway funding solves the collective action problem of suburban commuter coordination: highways enable dispersed residential patterns, reduce commute externalities, and create arbitrage access to both suburban housing and urban employment. Automotive industry, oil sector, and road construction contractors experience this as pure coordination with net benefit flow toward them. Experienced as positive coordination mechanism.
constraint_indexing:constraint_classification(highway_funding_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE AND URBAN SUSTAINABILITY COALITION (SCAFFOLD) — Organized agents (transit advocates, climate activists, New Urbanism movements, forward-thinking municipal governments) see highway funding bias as a temporary institutional arrangement with a structural sunset. Electric vehicles, autonomous transit, and climate pressure are creating an exit path from car-dependent infrastructure. Theater (performative 'smart growth' policies) is declining as economic case for dense transit becomes undeniable. Sunset mechanism: approximately 15-30 years as vehicle electrification and autonomous/shared mobility mature.
constraint_indexing:constraint_classification(highway_funding_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL HIGHWAY TRUST FUND APPARATUS (PITON) — Institutional machinery that maintains funding bias through regulatory inertia and performative justification. The trust fund mechanism (gasoline tax revenue earmarked for highways) was functionally coherent in 1956 when it solved a coordination problem. Now it is largely performative — justifications emphasize 'fixing deteriorating infrastructure' while new spending goes to capacity expansion rather than maintenance. The apparatus sees itself as degraded (defenders acknowledge the system no longer works efficiently) but persists through political inertia and entrenched contractor relationships. Theater ratio high (0.65) because political rhetoric emphasizes maintenance while funding priorities remain expansionist.
constraint_indexing:constraint_classification(highway_funding_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an abstract civilizational perspective, one might argue that suburban dispersal is an inevitable consequence of rising incomes and land availability — that highway funding simply responds to inevitable demand. This perspective naturalizes the constraint as emergent law. The analytical observer should note: this is a false summit. Historical analysis reveals that highway bias was a *policy choice* (Federal-Aid Highway Act 1956) that actively constructed suburban dispersal rather than responding to natural demand. The constraint is not emergent but imposed.
constraint_indexing:constraint_classification(highway_funding_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(highway_funding_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(highway_funding_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(highway_funding_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(highway_funding_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(highway_funding_bias, TR),
    TR >= 0.70.

:- end_tests(highway_funding_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint produces measurable asymmetric wealth flows toward suburban commuters and auto interests while concentrating costs on transit-dependent populations. The value reflects moderate-to-high extraction: benefits flow reliably and predictably to car-dependent commuters and industry, while costs are diffuse but severe for trapped populations. The extraction increased from 0.35 (1950s, when highway expansion was more justified as coordination) to 0.58 (current, when induced demand and sprawl maintenance are primary drivers). Suppression (0.52): Moderate-high. Transit-dependent populations face material barriers (lack of vehicle ownership capital, insurance costs, parking availability, driving ability/licensing) but these are surmountable at individual level. The systemic barrier is regulatory (zoning, parking requirements, transit disinvestment) rather than physical. Theater ratio (0.65): The Federal highway apparatus uses increasingly disconnected rhetoric: 'fixing crumbling infrastructure' and 'reducing congestion' are the stated justifications, but 80% of new highway spending goes to capacity expansion rather than maintenance, and induced demand evidence shows capacity expansion worsens congestion within 10 years. The performative gap widened from 0.45 (1990s, when maintenance framing was partially honest) to 0.65 (current, when expansion is clearly priority but justified as maintenance).
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between agents who can exit (suburban commuters can theoretically relocate to transit-rich cities; auto interests can theoretically pivot to new technologies) and agents who cannot (transit-dependent residents are locked by poverty and housing affordability). This creates two irreconcilable constraint experiences from identical structural data. The suburban commuter sees highways as coordination solving a genuine problem (dispersed residential patterns are economically real given land availability and rising incomes). The transit-dependent resident sees highways as extraction creating the very problem they purport to solve (zoning restrictions + highway subsidies + transit disinvestment combine to make car ownership mandatory). The piton perspective reveals that the contradiction is itself performative: the highway apparatus maintains justifications ('fixing infrastructure') that contradict actual spending patterns (capacity expansion). The scaffold perspective shows this is a false choice — climate economics and technology trajectory are making the entire debate obsolete. Neither highway expansion nor transit preservation will dominate; shared autonomous electric mobility will fundamentally restructure the constraint's extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) value is derived from their structural relationship to the funding flow and their exit capacity. Transit-dependent residents: d ≈ 0.95 (full targets of extraction; trapped exit → maximum f(d)). Suburban commuters and auto interests: d ≈ 0.10 (beneficiaries; arbitrage exit → negative f(d)). Inner-ring suburban residents: d ≈ 0.60 (mixed position — some highway benefit, significant local congestion cost; constrained exit → moderate f(d)). Urban core property owners: d varies by wealth (powerful owners can arbitrage gentrification gains; less powerful face displacement). Climate coalition: d ≈ 0.45 (organized agents with mobile exit options through policy alternatives). The engine computes chi = ε × f(d) × σ(S) for each perspective: the national scope σ(national) = 1.0 means scaling is moderate; regional perspectives (suburbs at σ=0.9) show slightly dampened extraction; local perspectives (neighborhood level at σ=0.8) show further dampening. The beneficiary perspective (institutional/arbitrage) produces the lowest chi despite identical ε base; the victim perspective (powerless/trapped) produces the highest chi.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The highway funding bias constraint shows why mandatrophy (false natural law detection) matters. The false summit perspective (Analytical/Natural Law) claims that suburban dispersal is inevitable consequence of rising incomes — naturalizing the constraint as Mountain. But structural evidence contradicts this: (1) Highway funding is a specific policy choice (1956 Federal Act) not an inevitable response; (2) Induced demand evidence shows highways create 'necessity' for further sprawl rather than responding to existing demand; (3) International comparison — nations with equivalent incomes but different infrastructure policies achieved different settlement patterns; (4) Historical counterfactual — Pre-1956 US was developing transit-oriented cities despite rising incomes. The false summit is maintained through performative rhetoric ('inevitable sprawl' + 'highway maintenance necessity') that masks policy choice. The mandatrophy is resolved by recognizing that: (a) the constraint is real and extractive (Snare from victim perspective is structurally accurate), (b) it is contingent and constructed (not emergent law), (c) it has identifiable sunset mechanisms (EV + autonomous transit + climate economics), and (d) the apparatus maintains itself through theater and inertia (Piton classification accurate). The constraint is Tangled Rope, not Mountain — genuine coordination function (suburban commuting) exists alongside asymmetric extraction (costs concentrated on powerless agents). Recognizing this enables policy response (redirect funding, reform zoning, sunset the Highway Trust Fund mechanism) whereas treating it as natural law (Mountain) would paralyze intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_highway_demand,
    'Does highway funding bias respond to suburban demand or actively construct suburbanization?',
    'Historical counterfactual analysis: Where would suburbanization have progressed without Federal Highway funding? Comparative international case analysis: nations with different infrastructure investment priorities and resulting settlement patterns.',
    'If highways respond to demand: constraint is emergent coordination (Rope from more perspectives). If highways construct demand: constraint is institutional imposition (Snare from more perspectives). Classification hinges on this directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_direction_highway_demand, empirical, 'Whether highway funding responds to or constructs suburban demand').

omega_variable(
    transit_crowding_out_mechanism,
    'Is the underfunding of transit a direct causal consequence of highway funding bias, or are they independent policy failures?',
    'Budget accounting across jurisdictions: correlation between highway funding share and transit funding decline; analysis of funding decisions when total transportation budgets were increasing vs. stagnating.',
    'If direct causal crowding-out: the extraction mechanism is clear (victims bear cost of highway prioritization). If independent failures: highway bias is less extractive (fewer agents bear direct cost) and more a coordination problem with side effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transit_crowding_out_mechanism, empirical, 'Whether highway funding directly crowds out transit investment').

omega_variable(
    environmental_valuation_externality,
    'What proportion of the suppression experienced by transit-dependent populations is due to environmental degradation (air quality, heat island, noise) from highway expansion?',
    'Health impact assessment: correlate highway proximity with respiratory disease, heat-related mortality, and noise-induced hearing loss; monetize externalities; compare to transit infrastructure environmental benefits.',
    'If high proportion: suppression is partially environmental (affects all residents near highways, not just transit-dependent). If low proportion: suppression is primarily mobility-focused (affects transit-dependent most acutely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_valuation_externality, empirical, 'Environmental externality contribution to suppression').

omega_variable(
    induced_demand_loop,
    'Does new highway capacity primarily serve existing demand or induce new vehicle trips through expansion of geographically dispersed development?',
    'Longitudinal analysis of traffic growth following new highway construction; comparison of capacity additions to observed vehicle-mile growth; model of induced demand elasticity.',
    'If primarily induced demand: highway spending is self-justifying cycle (extraction mechanism sustained by its own consequences). If primarily serves existing demand: spending is responsive to real need.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(induced_demand_loop, empirical, 'Induced demand loop in highway capacity expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(highway_funding_bias, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hfb_tr_t0, highway_funding_bias, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hfb_tr_t10, highway_funding_bias, theater_ratio, 10, 0.58).
narrative_ontology:measurement(hfb_tr_t20, highway_funding_bias, theater_ratio, 20, 0.65).
narrative_ontology:measurement(hfb_tr_t30, highway_funding_bias, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(hfb_be_t0, highway_funding_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hfb_be_t10, highway_funding_bias, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hfb_be_t20, highway_funding_bias, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hfb_be_t30, highway_funding_bias, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(highway_funding_bias, resource_allocation).
narrative_ontology:affects_constraint(highway_funding_bias, urban_parking_mandate_lock_in).
narrative_ontology:affects_constraint(highway_funding_bias, zoning_exclusionary_single_family).
narrative_ontology:affects_constraint(highway_funding_bias, transit_funding_starvation).
narrative_ontology:affects_constraint(highway_funding_bias, vehicle_electrification_constraint).

% DUAL FORMULATION NOTE:
% Highway funding bias is upstream of several derivative constraints (parking mandates, zoning restrictions, transit underfunding) in a causal chain. The base constraint story models the funding mechanism and its distributional consequences. Related stories decompose the behavioral lock-ins (parking, zoning, vehicle electrification barriers) that maintain highway dependence even when funding structures could shift. All stories are linked via affects_constraints; each has distinct ε reflecting different measurement basis (funding flows vs. regulatory enforcement vs. technological adoption barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(highway_funding_bias, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
