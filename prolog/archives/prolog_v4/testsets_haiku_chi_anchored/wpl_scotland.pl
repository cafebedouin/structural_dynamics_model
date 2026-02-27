% ============================================================================
% CONSTRAINT STORY: wpl_scotland
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wpl_scotland, []).

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
 *   constraint_id: wpl_scotland
 *   human_readable: Scotland's Workplace Parking Levy (WPL)
 *   domain: economic/transport_policy
 *
 * SUMMARY:
 *   Scotland's Workplace Parking Levy (WPL) is a local tax power granted to
 *   Scottish councils, allowing them to charge employers for providing
 *   parking spaces to employees. The levy is intended to fund public
 *   transport and active travel infrastructure while reducing car dependency
 *   in urban areas. The constraint exhibits genuine hybridity: it solves a
 *   coordination problem (employers oversupply parking due to individual
 *   incentives while collectively creating congestion, emissions, and
 *   land-use inefficiency) AND it extracts revenue from employers and
 *   employees, with differential burden falling heavily on suburban and rural
 *   workers who lack viable transit alternatives. The theater ratio has
 *   increased over the implementation interval as policy messaging emphasizes
 *   'environmental/sustainability' benefits while the actual mechanism is
 *   pure cost-transfer and supply reduction—this is Goodhart drift (metric
 *   substitution where means become ends). The constraint is extractive for
 *   workers with no exit options but constitutes genuine coordination for
 *   urban populations with transit access. The critical ambiguity is whether
 *   WPL revenue translates to sufficient capacity improvements to eventually
 *   reduce extraction force (sunset path) or whether it becomes a permanent
 *   regressive tax.
 *
 * KEY AGENTS:
 *   - Local Councils (Primary Beneficiary): Institutional/arbitrage → capture dedicated WPL revenue; solve coordination problem of parking oversupply without enforcing individually
 *   - Suburban/Rural Workers (Primary Victim): Powerless/trapped → no transit alternatives; bear WPL costs through parking reduction or employer cost pass-through
 *   - Urban Employers (Mixed): Moderate/constrained → bear immediate levy cost but benefit from long-term reduced parking infrastructure burden and property value stability
 *   - Public Transport Providers (Organized Beneficiary): Organized/constrained → receive WPL-funded capacity expansions but constrained by implementation speed and revenue availability
 *   - Parking Industry/Commercial Property Owners (Degraded): Institutional/arbitrage → previously monetized parking scarcity; WPL reduces their extraction lever; theater_ratio rise reflects this degradation
 *   - Large Urban Employers with Exit Options (Scaffold): Powerful/mobile → can relocate or invest in alternatives; face temporary extraction but genuine sunset as transit improves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wpl_scotland, 0.38).
domain_priors:suppression_score(wpl_scotland, 0.48).
domain_priors:theater_ratio(wpl_scotland, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wpl_scotland, extractiveness, 0.38).
narrative_ontology:constraint_metric(wpl_scotland, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(wpl_scotland, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wpl_scotland, tangled_rope).
narrative_ontology:human_readable(wpl_scotland, "Scotland's Workplace Parking Levy (WPL)").
narrative_ontology:topic_domain(wpl_scotland, "economic/transport_policy").

domain_priors:requires_active_enforcement(wpl_scotland).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wpl_scotland, local_councils).
narrative_ontology:constraint_beneficiary(wpl_scotland, public_transport_providers).
narrative_ontology:constraint_beneficiary(wpl_scotland, urban_commuters_using_transit).
narrative_ontology:constraint_victim(wpl_scotland, employers_providing_parking).
narrative_ontology:constraint_victim(wpl_scotland, suburban_and_rural_workers).
narrative_ontology:constraint_victim(wpl_scotland, discretionary_parking_employers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBURBAN/RURAL WORKER (SNARE) — No viable public transit alternative; cannot exit the parking dependency. Wages do not adjust for WPL cost pass-through. Employer reduces parking supply or passes cost to employee. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(wpl_scotland, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN EMPLOYER (TANGLED ROPE) — Constrained by WPL but also benefits from coordination: reduced parking demand lowers real estate costs and infrastructure burden long-term. Faces immediate levy cost but shares in ecosystem benefits (traffic reduction, transit investment returns, property value stabilization). Suppression is high (must fund levy or reduce supply) but coordination function is real. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOCAL COUNCIL/TRANSPORT AUTHORITY (ROPE) — Primary beneficiary. WPL provides dedicated revenue stream for public transit, walking/cycling infrastructure, and congestion management. Solves collective action problem: individual employers won't voluntarily reduce parking (race to the bottom in amenities); WPL coordinates supply contraction and funds alternatives. d≈0.08, f(d)≈-0.08, σ=0.9 → χ≈-0.03. Net positive coordination with minimal extraction overhead.
constraint_indexing:constraint_classification(wpl_scotland, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PUBLIC TRANSPORT PROVIDER / CYCLING ADVOCACY (TANGLED ROPE) — Organized beneficiaries of WPL revenue but constrained by execution: must deliver capacity/safety improvements to justify extraction from employers. If revenue doesn't translate to transit frequency or cycling infrastructure, WPL becomes pure extraction. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.13. Low extraction if delivery is credible; high if diverted to general council budgets.
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PARKING INDUSTRY / COMMERCIAL PROPERTY OWNERS (PITON) — Historically benefited from unlimited parking supply as a market commodity. WPL degrades their extraction model (paid parking was their lever; now mandatory WPL takes that function). Theater ratio high (0.52): much WPL rhetoric emphasizes 'environmental/health messaging' but the mechanism is purely revenue/supply reduction. Parking industry persists through inertia (employers maintain some supply voluntarily) despite functional decline. d≈0.78, f(d)≈1.07, σ=0.9 → χ≈0.45.
constraint_indexing:constraint_classification(wpl_scotland, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: LARGE URBAN EMPLOYER WITH TRANSIT ACCESS (SCAFFOLD) — Has genuine exit option: relocate to lower-WPL jurisdiction or invest in shuttle/cycle infrastructure to reduce parking dependency. WPL imposes temporary extraction (levy cost), but the constraint has a structural sunset: as public transit capacity improves (funded by WPL revenue), the employer's need to provide free parking declines, and the levy's extraction force diminishes. d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.20. Temporary suppression (high levies + parking reduction pressure) but mobile exit and declining harm over time.
constraint_indexing:constraint_classification(wpl_scotland, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a national/civilizational view, WPL is a genuine hybrid: coordination function (solves parking oversupply + funds transit) is real AND extraction function (costs to employers, especially outside central urban cores) is real. NOT a false summit (would require ε≤0.25, suppression≤0.05). The constraint legitimately coordinates urban transport while extracting from geographically disadvantaged workers. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wpl_scotland_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wpl_scotland, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wpl_scotland, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wpl_scotland, TR),
    TR >= 0.70.

:- end_tests(wpl_scotland_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. WPL imposes real costs on employers (direct levy) and on workers without transit alternatives (parking reduction + cost pass-through), but the extraction is not severe (ε>0.46) because the levy rates are capped (~£5-10/space/year historically), incentives for evasion/avoidance are limited, and the coordination function is genuine. The interval trajectory (0.22→0.38) reflects accumulation: early WPL implementation had lower extraction (pilot phase, reduced coverage) but as councils expanded and rates increased, extraction rose. Suppression (0.48): Moderate-high. Real barriers include parking dependency for immobile workers, employer coordination failure (individual incentive to maintain supply), and political resistance. But suppression is not total—employers have options (reduce supply, invest in transit subsidies, relocate) and workers can shift modality over time. Theater ratio (0.52): Moderate-high and rising. WPL implementation messaging emphasizes 'carbon reduction' and 'sustainability' goals, but the actual mechanism is revenue extraction and supply management. This gap between stated purpose and actual lever is theater. The rise from 0.35→0.52 reflects cumulative goal-substitution: as revenue targets became more salient than original trip-reduction targets, the performative content increased. Theater_ratio=0.52 is below piton threshold (0.70) because the constraint still has functional impact (parking does reduce, some modal shift occurs); it's not purely ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   The suburban/rural worker sees a snare (no exit, full extraction). The urban employer sees tangled rope (extraction is real but coordination function is also real; they benefit from reduced parking infrastructure burden). The local council sees rope (pure coordination solution to collective action problem). The public transport provider sees tangled rope (benefits from revenue but constrained by implementation pace and potential revenue diversion). The parking industry sees its own degraded extraction mechanism (piton). The large urban employer sees scaffold (temporary extraction with a sunset path as transit improves). The analytical observer sees the true tangled rope: both functions are structurally real, but they fall on different populations. This is NOT a false summit or a fully symmetric coordination problem—it is genuinely hybrid with distributional asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Suburban/rural workers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Urban employers: Victim + constrained BUT also beneficiary (long-term parking infrastructure relief) → d≈0.68, f(d)≈1.02. Moderate extraction. Local councils: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net positive. Public transport providers: Beneficiary + constrained (dependent on revenue flow, must deliver to justify extraction) → d≈0.35, f(d)≈0.32. Low extraction. Parking industry: Previously beneficiary → now victim of extraction mechanism degradation → d≈0.78, f(d)≈1.07. Moderate extraction. Large urban employers: Victims of immediate levy + beneficiaries of long-term transit improvement + have mobile options → d≈0.45, f(d)≈0.48. Moderate extraction with exit path. The directionality chain captures the distributional split: extraction is concentrated (geographic and skill-based) while coordination benefits are dispersed.
 *
 * MANDATROPHY ANALYSIS:
 *   WPL RESOLVES THE MANDATROPHY by showing that 'coordination vs extraction' is not binary but distributional. The constraint simultaneously solves the commons problem (parking oversupply) and redistributes costs regionally. The false naïve reading: 'WPL is pure extraction (snare)—councils extract revenue from employers without corresponding benefit.' Reality: councils solve a coordination failure that individual employers cannot solve; parking supply does reduce; congestion does decline. The other false reading: 'WPL is pure coordination (rope)—the system works smoothly because all parties benefit.' Reality: workers without transit alternatives bear costs with no corresponding benefit; suburban employment becomes less attractive; equity impacts are negative. The correct framing (tangled rope) acknowledges both: WPL genuinely coordinates urban transport AND it genuinely extracts from geographically immobile workers. The theater_ratio rise (0.35→0.52) indicates mandatrophy risk: if WPL rhetoric increasingly emphasizes 'carbon reduction' goals (performance metric) rather than 'actual transit capacity' goals (functional metric), the constraint risks degrading into piton (pure extraction maintained through environmental theater). The omega variables test whether the scaffold sunset (WPL→transit improvement→reduced extraction) is real or aspirational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_transit_feasibility,
    'Over the WPL implementation horizon (10-20 years), how much of the suburban/rural workforce can actually shift to public transit given capital investment rates from WPL revenue?',
    'Longitudinal tracking of transit accessibility (time to employment center by bus/rail) in WPL jurisdictions; correlation with parking dependency decline and wage/job quality changes in affected regions',
    'If >60% transition feasible: snare → tangled_rope for suburban workers. If <40% feasible: snare persists; WPL becomes regressive extraction from immobile workforce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transit_feasibility, empirical, 'Whether public transit expansion from WPL revenue enables suburban worker mobility').

omega_variable(
    revenue_diversion_risk,
    'Does WPL revenue stay dedicated to transport/cycling infrastructure or does it get diverted into general council budgets (particularly during austerity periods)?',
    'Audit of WPL revenue allocation across councils over 5-10 year implementation period; comparison of dedicated vs general fund treatment',
    'If dedicated: tangled_rope for employers (coordination + extraction both real). If diverted: snare (pure extraction with no corresponding infrastructure benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_diversion_risk, empirical, 'Whether WPL revenue remains dedicated to transport investment').

omega_variable(
    employment_location_response,
    'Does WPL induce employers to relocate to lower-levy jurisdictions (including England), and if so, does this offset transport benefits through job loss?',
    'Regional employment data pre/post-WPL implementation; employer relocation surveys; inter-jurisdictional job migration analysis',
    'If relocation minimal (<5% job loss): coordination function dominates. If substantial (>15% loss): extraction function dominates and spillover effects create negative-sum outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_location_response, empirical, 'Whether WPL causes significant employer relocation').

omega_variable(
    equity_incidence_reality,
    'Do WPL cost pass-throughs to workers flatten or amplify existing wage/income inequality between urban/suburban and skilled/unskilled cohorts?',
    'Wage survey data by geography and skill level; econometric analysis of WPL implementation timing against wage changes; inequality metric tracking (Gini coefficient, percentile ratios)',
    'If amplified: WPL is regressive extraction on vulnerable workers; victim classification strengthens. If flattened: progressive wage adjustment suggests bargaining power; victim → moderate classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equity_incidence_reality, empirical, 'Whether WPL implementation increases income inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wpl_scotland, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpl_tr_t0, wpl_scotland, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wpl_tr_t5, wpl_scotland, theater_ratio, 5, 0.48).
narrative_ontology:measurement(wpl_tr_t10, wpl_scotland, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(wpl_be_t0, wpl_scotland, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wpl_be_t5, wpl_scotland, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(wpl_be_t10, wpl_scotland, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wpl_scotland, resource_allocation).
narrative_ontology:affects_constraint(wpl_scotland, urban_parking_oversupply).
narrative_ontology:affects_constraint(wpl_scotland, transport_modal_split).
narrative_ontology:affects_constraint(wpl_scotland, regional_employment_equity).

% DUAL FORMULATION NOTE:
% WPL is downstream of the urban parking commons problem (unsustainable oversupply due to employer coordination failure) and upstream of actual modal shift outcomes. The constraint's classification depends critically on whether revenue translates to sufficient transit capacity (tangled rope with sunset) or becomes permanent regressive taxation (snare). Related constraints: urban_parking_oversupply (ε≈0.08, rope—the coordination problem WPL solves), transport_modal_split (ε≈0.35, tangled rope—the outcome that determines WPL's functional success), regional_employment_equity (ε≈0.52, snare—if WPL costs concentrate on disadvantaged workers without corresponding benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wpl_scotland, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
