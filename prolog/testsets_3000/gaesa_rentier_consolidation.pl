% ============================================================================
% CONSTRAINT STORY: gaesa_rentier_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaesa_rentier_consolidation, []).

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
 *   constraint_id: gaesa_rentier_consolidation
 *   human_readable: Gaesa Rentier Consolidation and State Revenue Capture
 *   domain: economic_policy/state_capture
 *
 * SUMMARY:
 *   Gaesa (Grupo de Administración Empresarial, S.A.) is a state-owned
 *   holding company that consolidates control over Cuba's agricultural
 *   distribution, input supply, and much of the production apparatus. The
 *   consolidation began in the 1990s as a genuine response to food security
 *   crisis following Soviet collapse — centralized procurement enabled
 *   rationing and price control during scarcity. Over 25 years, the
 *   coordination function has atrophied while extraction mechanisms have
 *   intensified. Independent producers face mandatory sales channels at
 *   state-controlled prices with no legal exit. Rural labor is trapped in
 *   state-controlled enterprises with wage suppression and restricted
 *   mobility. Gaesa leadership benefits from monopoly pricing and protected
 *   market position. The state Ministry relies on Gaesa rents for fiscal
 *   stability but this dependency compromises long-term productivity. The
 *   constraint demonstrates how coordination mechanisms can calcify into
 *   extraction apparatus through institutional inertia, with performative
 *   food security narratives masking declining actual function.
 *
 * KEY AGENTS:
 *   - Independent Agricultural Producers: Primary victims (powerless/trapped) — forced sales to Gaesa at controlled prices with no market exit
 *   - Rural Labor Force: Primary victims (powerless/trapped) — wage suppression, restricted mobility, no alternative employment
 *   - Gaesa Corporate Leadership: Primary beneficiary (institutional/arbitrage) — captures monopoly rents and supply chain control
 *   - State Financial Ministry: Constrained beneficiary (institutional/constrained) — benefits from consolidated revenue but fiscally dependent on extraction sustainability
 *   - Agricultural Sector Collectively: Locked-in actor (organized/constrained) — former coordination mechanism now inertial extraction apparatus
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies false food security framing concealing pure rentierism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaesa_rentier_consolidation, 0.68).
domain_priors:suppression_score(gaesa_rentier_consolidation, 0.72).
domain_priors:theater_ratio(gaesa_rentier_consolidation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaesa_rentier_consolidation, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaesa_rentier_consolidation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gaesa_rentier_consolidation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaesa_rentier_consolidation, snare).
narrative_ontology:human_readable(gaesa_rentier_consolidation, "Gaesa Rentier Consolidation and State Revenue Capture").
narrative_ontology:topic_domain(gaesa_rentier_consolidation, "economic_policy/state_capture").

domain_priors:requires_active_enforcement(gaesa_rentier_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaesa_rentier_consolidation, gaesa_corporate_leadership).
narrative_ontology:constraint_beneficiary(gaesa_rentier_consolidation, state_financial_ministry).
narrative_ontology:constraint_victim(gaesa_rentier_consolidation, cuban_agricultural_sector).
narrative_ontology:constraint_victim(gaesa_rentier_consolidation, independent_producers).
narrative_ontology:constraint_victim(gaesa_rentier_consolidation, rural_labor_force).
narrative_ontology:constraint_victim(gaesa_rentier_consolidation, state_fiscal_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT PRODUCERS (SNARE) — Face total extraction through mandatory sales channels to Gaesa at state-controlled prices, with no legal exit. Cannot sell directly to market, export independently, or seek alternative buyers. Suppression is maximal: regulatory prohibition + economic dependency + geographic isolation from alternative markets. Extractiveness experienced as total — all surplus captured.
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL LABOR FORCE (SNARE) — Structurally dependent on employment in state-controlled agricultural enterprises. Wages and conditions are set unilaterally by state/Gaesa without negotiation. Trapped by lack of urban alternatives, migration barriers, and skill specificity. Suppression includes wage controls, restricted labor mobility, and state surveillance of organizing attempts. No exit options — exit would require abandoning rural location or illegally relocating.
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GAESA CORPORATE LEADERSHIP (ROPE) — Experiences the consolidation as coordination mechanism for managing agricultural supply chains and capturing rents. Arbitrage exit available (international food commodity markets, alternative management arrangements). Net beneficiary — extraction flows toward this agent from producers and labor. Coordination function exists: Gaesa does provide distribution infrastructure and market aggregation. But the extraction far exceeds coordination costs.
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE FINANCIAL MINISTRY (TANGLED ROPE) — Benefits from consolidated revenue collection through Gaesa without requiring expanded tax bureaucracy. Genuine coordination benefit: centralized procurement for state food security and price control. But constrained by structural dependency on Gaesa rents to fund state operations; fiscal sustainability is compromised because Gaesa extraction is treated as sustainable revenue rather than temporary rent-capture. Cannot exit dependency without fiscal restructuring. Mixed classification reflects both coordination benefit (consolidated supply) and structural extraction trap (fiscal lock-in).
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AGRICULTURAL SECTOR (PITON) — The consolidation began with genuine coordination problem (post-1990s food security crisis required centralized procurement). The constraint persists through institutional inertia — the original coordination function (ensuring food supply during scarcity) has largely been replaced by performative state food security claims, while extraction mechanisms (monopoly pricing, forced channeling) have intensified. Theater ratio (0.58) reflects that formal price control and strategic reserve narratives obscure extractive mechanisms. Sector is locked into the consolidated structure despite reduced coordination benefit.
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Identifies the constraint as a pure extraction mechanism disguised as state capacity. From a civilizational/global perspective, agricultural consolidation appears as state-level rentierism — the apparatus captures value without creating it, and the extraction is sustained by eliminating structural alternatives. No genuine coordination function remains at this scale; the constraint is maintained by suppressing market alternatives and independent producer networks. The 'food security' framing naturalizes what is actually monopolistic extraction.
constraint_indexing:constraint_classification(gaesa_rentier_consolidation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaesa_rentier_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaesa_rentier_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaesa_rentier_consolidation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaesa_rentier_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gaesa_rentier_consolidation, TR),
    TR >= 0.70.

:- end_tests(gaesa_rentier_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, with upward trajectory. The consolidation began with legitimate coordination function (ε ≈ 0.45 in period 0, addressing 1990s scarcity). Over 25 years, food security improved (scarcity ended) but consolidation persisted and extraction intensified. Current extractiveness reflects monopoly pricing without proportional coordination benefit — producers can no longer freely market output, labor faces wage suppression without compensation in service quality, and the state increasingly relies on rents rather than productivity gains. The 0.68 score reflects that primary beneficiaries extract substantial surplus but coordination infrastructure (distribution, aggregation) still provides some genuine function. Suppression (0.72): Very high. Multiple enforcement mechanisms operate: regulatory prohibition on independent sales, administrative allocation of inputs, restricted labor mobility, surveillance of informal market activity, and geographic isolation from alternative markets. But suppression is not absolute (0.90+) because some informal activity persists and producer exit is theoretically legal (individuals can leave rural sector). The high score reflects that formal barriers are comprehensive and cost of exit is severe. Theater ratio (0.58): Moderate-high. The constraint's legitimating narratives emphasize state food security, strategic reserves, and price stability. These claims have some truth-value (food is available, prices are controlled) but obscure the mechanism: consolidation achieves these outcomes through extraction from producers and labor, not through efficient supply chain design. As agricultural productivity declined (a consequence of suppressed producer incentives), theater increased — the state invested more in narrative maintenance (food security rhetoric) while actual function declined.
 *
 * PERSPECTIVAL GAP:
 *   Snare perspective (producers, analysts) sees total extraction + total suppression + no genuine coordination benefit remaining. Rope perspective (Gaesa) sees coordination benefits + protected market position + sustainable revenue. Tangled Rope perspective (Ministry) sees both coordination (supply stability) and extraction trap (fiscal dependency). Piton perspective (sector) sees inertial persistence despite degraded function. The gap indicates that the constraint's legitimating narratives (food security, state capacity) are perspectival distortions from beneficiary position. Producers and analysts, looking at the same facts (consolidation, pricing, suppression), see pure extraction. Beneficiaries, looking at the same facts from inside the apparatus, see coordination + justifiable compensation. Neither is wrong — they are measuring from different structural positions. The gap itself is diagnostic: where there is no perspectival gap (all observers see Rope), the constraint genuinely coordinates value. Where the gap is maximum (beneficiary sees Rope, victims see Snare), the constraint is extractive apparatus sustained by asymmetric power.
 *
 * DIRECTIONALITY LOGIC:
 *   Producers are locked in as victims (trapped exit) with high power asymmetry (powerless status). This derives d ≈ 0.95 → f(d) ≈ 1.42, producing high experienced extractiveness χ. Gaesa leadership are beneficiaries with arbitrage options (exit via international markets is theoretically available). This derives d ≈ 0.10 → f(d) ≈ -0.05, producing negative or minimal χ. The Ministry occupies intermediate position: institutional status suggests lower d, but constrained exit (fiscal dependency) elevates d. The perspectival gap reveals that the beneficiary's arbitrage exit is contingent on state protection — if consolidation were removed, the constraint could not sustain itself through market mechanisms alone. This indicates the constraint is not a natural market phenomenon but an enforced institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through temporal analysis. The constraint began as Tangled Rope (genuine food security coordination function + extraction from producers to fund centralization). Over 25 years, the coordination function has declined (scarcity ended, agricultural productivity stagnated, distribution efficiency plateaued) while extraction mechanisms have intensified. The primary beneficiary (Gaesa) has shifted from active manager to rentier. The constraint now appears as Snare from victim perspectives (total extraction) and Piton from sector perspective (theatrical maintenance of degraded function). The state's Tangled Rope position is unstable: fiscal dependency on extraction rents creates incentive to intensify suppression, which reduces agricultural productivity, which requires more intensive extraction to maintain revenues. This is the classic doom spiral of rentier consolidation. Resolution requires distinguishing remaining coordination function (distribution infrastructure, aggregation) from extraction mechanism (monopoly pricing, forced channels). If coordination function is genuinely necessary, restructuring into Scaffold (temporary authority with sunset clause) would enable transition. If coordination function is vestigial, dismantling the apparatus would increase overall productivity despite short-term disruption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    food_security_necessity,
    'What portion of Gaesa''s consolidation reflects genuine food security coordination vs. pure rentier extraction?',
    'Comparative analysis of Cuban food production efficiency, price volatility, and nutritional outcomes vs. peer nations with different supply chain architectures; counterfactual modeling of decentralized procurement outcomes',
    'If food security is 60%+ of consolidation function: reclassify as Tangled Rope (substantial coordination). If <30%: classify as Snare (primary extraction). Current framing assumes coordination function has largely atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_security_necessity, empirical, 'Ratio of food security coordination to pure extraction in consolidation apparatus').

omega_variable(
    producer_exit_feasibility,
    'Could independent producers exit Gaesa system through legal or semi-legal channels (private plot expansion, cooperative formation, underground markets)?',
    'Regulatory audit of private agricultural permit structures; ethnographic documentation of informal market activity; comparison with post-reform access periods (1990s-2000s)',
    'If exit channels exist at reasonable cost: reclassify trapped agents as constrained (higher agency). If prohibitions are absolute: confirms trapped classification and validates maximum suppression score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(producer_exit_feasibility, empirical, 'Whether exit channels exist for agricultural producers').

omega_variable(
    state_fiscal_dependency_depth,
    'To what extent is state fiscal sustainability dependent on Gaesa extraction? Could state restructure revenue sources if consolidation were dismantled?',
    'Fiscal accounting analysis of Gaesa contribution to state budget; modeling of alternative revenue sources (tax restructuring, sectoral reorientation); analysis of hidden fiscal costs (lost agricultural productivity, resource misallocation)',
    'If state is structurally dependent (>40% of agricultural revenue from Gaesa rents): Ministry becomes trapped agent, reclassifying as Snare or Tangled Rope victim. If diversifiable: Ministry remains constrained beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_fiscal_dependency_depth, empirical, 'State fiscal dependency on Gaesa extraction rents').

omega_variable(
    international_arbitrage_capacity,
    'Could Gaesa leadership restructure business model to operate on international commodity markets if domestic consolidation were removed?',
    'Comparative analysis of Gaesa''s administrative structure vs. international agribusiness firms; assessment of export capacity and international market access; analysis of skill transferability',
    'If true arbitrage capacity exists: Gaesa leadership would maintain Rope classification in post-consolidation scenario. If dependent on state protection: reclassifies as Snare beneficiary (extraction sustained only by state enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_arbitrage_capacity, empirical, 'Whether Gaesa leadership has true arbitrage exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaesa_rentier_consolidation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaesa_tr_t0, gaesa_rentier_consolidation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gaesa_tr_t5, gaesa_rentier_consolidation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gaesa_tr_t10, gaesa_rentier_consolidation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(gaesa_tr_t15, gaesa_rentier_consolidation, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(gaesa_be_t0, gaesa_rentier_consolidation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gaesa_be_t5, gaesa_rentier_consolidation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gaesa_be_t10, gaesa_rentier_consolidation, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gaesa_be_t15, gaesa_rentier_consolidation, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaesa_rentier_consolidation, resource_allocation).
narrative_ontology:boltzmann_floor_override(gaesa_rentier_consolidation, 0.18).
narrative_ontology:affects_constraint(gaesa_rentier_consolidation, cuban_agricultural_productivity_decline).
narrative_ontology:affects_constraint(gaesa_rentier_consolidation, state_fiscal_sustainability_trap).
narrative_ontology:affects_constraint(gaesa_rentier_consolidation, rural_labor_mobility_restrictions).

% DUAL FORMULATION NOTE:
% Gaesa consolidation is downstream of 1990s food security crisis and upstream of current agricultural stagnation. The constraint represents a coordination solution that has calcified into extraction apparatus. Decomposition note: the genuine food security coordination function (ε ≈ 0.20, Rope) should be separated from the rentier extraction mechanism (ε ≈ 0.70, Snare) in future analysis. Current story models them as single constraint because institutional apparatus bundles both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaesa_rentier_consolidation, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
