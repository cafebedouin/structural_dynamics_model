% ============================================================================
% CONSTRAINT STORY: global_stimulus_spree
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_stimulus_spree, []).

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
 *   constraint_id: global_stimulus_spree
 *   human_readable: The 2026 Global Fiscal Stimulus Surge
 *   domain: economic/political
 *
 * SUMMARY:
 *   In 2024–2026, major developed economies (US, EU, Japan, China) deployed
 *   coordinated multitrillion-dollar fiscal stimulus packages ostensibly
 *   targeting AI infrastructure, green energy transitions, and defense
 *   modernization. The constraint arises from the structural tension between
 *   legitimate coordination needs (AI transition is real; climate investment
 *   is real; geopolitical competition is real) and the extraction mechanism
 *   embedded in how these objectives are funded. Governments are financing
 *   investments through permanent debt issuance while central banks maintain
 *   artificially low rates, creating a hidden tax on future generations and
 *   developing economies via capital flight. The stimulus is simultaneously a
 *   coordination mechanism (addressing genuine collective action problems)
 *   and an extraction mechanism (benefiting incumbent firms, externalizing
 *   costs to future taxpayers). The theater ratio reflects that rhetoric
 *   emphasizes 'temporary emergency measures' and 'investment in future
 *   competitiveness' while structural policy shifts (monetary subordination,
 *   entitlement expansion, fiscal dominance) become permanent.
 *
 * KEY AGENTS:
 *   - Multinational Tech Corporations & Defense Contractors: Primary beneficiaries (organized/arbitrage) — direct procurement subsidies and R&D grants; can arbitrage to private markets if stimulus contracts end
 *   - Future Taxpayers: Primary victims (powerless/trapped) — inherit debt service obligations; no voice in current decisions; generational time horizon ensures full cost externalization
 *   - Fiscal Sustainability (Abstract): Secondary victim — macroeconomic constraint violated; debt thresholds crossed; no mechanism for voluntary rollback
 *   - Developing Economies: Secondary victims (moderate/constrained) — face capital flight when developed markets raise rates; currency depreciation; debt servicing crises; constrained exit via capital controls
 *   - Central Banks: Institutional actors (institutional/constrained) — inflation-targeting mandate abandoned; yield-curve control locks them into permanent rate suppression; arbitrage limited by fiscal dominance
 *   - Incumbent Energy & Auto Firms: Beneficiaries with constraint (institutional/constrained) — capture green energy subsidies but face stranded asset risk; constrained by regulatory mandates; mixed coordination-extraction position
 *   - Analytical Observer: Civilizational perspective — must distinguish temporary stimulus (scaffold with sunset) from permanent fiscal dominance (snare)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_stimulus_spree, 0.58).
domain_priors:suppression_score(global_stimulus_spree, 0.68).
domain_priors:theater_ratio(global_stimulus_spree, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_stimulus_spree, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_stimulus_spree, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_stimulus_spree, tangled_rope).
narrative_ontology:human_readable(global_stimulus_spree, "The 2026 Global Fiscal Stimulus Surge").
narrative_ontology:topic_domain(global_stimulus_spree, "economic/political").

domain_priors:requires_active_enforcement(global_stimulus_spree).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_stimulus_spree, multinational_tech_corporations).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, defense_contractors).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, incumbent_energy_firms).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, institutional_investors).
narrative_ontology:constraint_victim(global_stimulus_spree, future_taxpayers).
narrative_ontology:constraint_victim(global_stimulus_spree, fiscal_sustainability).
narrative_ontology:constraint_victim(global_stimulus_spree, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE TAXPAYER (SNARE) — Children born in 2030+ inherit multitrillion-dollar sovereign debt with no exit option. No voice in current stimulus decisions; full cost exposure across decades. Maximum extraction: externalized via temporal arrow.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FISCAL SUSTAINABILITY (SNARE) — Abstract constraint violated. Debt-to-GDP ratios exceeded sustainable thresholds in 2024–2026 across OECD. No mechanism for rollback once structural deficits are locked in. Pure extraction from long-term macroeconomic commons.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING ECONOMIES (TANGLED ROPE) — Face capital flight as developed markets raise rates to finance stimulus debt. Currency depreciation, debt servicing crises, constrained exit (capital controls create perverse incentives). Also benefit via technology spillover and infrastructure investments channeled through multilateral banks. Asymmetric extraction but not total — some coordination mechanisms exist.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TECH CORPORATIONS & DEFENSE CONTRACTORS (ROPE) — Primary beneficiaries. Government procurement and R&D subsidies directed explicitly to AI chips, autonomous vehicles, and hypersonic systems. Arbitrage exit: they can pivot to private funding or international markets if domestic stimulus contracts. Net positive flow — coordination mechanism (stimulus targets genuine national objectives) aligned with extraction benefit.
constraint_indexing:constraint_classification(global_stimulus_spree, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANKS (PITON) — Traditional inflation-fighting mandate abandoned. Quantitative easing and yield-curve control maintain artificially low rates to finance fiscal stimulus. Theater ratio high (0.61): rhetoric emphasizes 'temporary emergency measures' while structural rate suppression continues indefinitely. Function degraded — price stability mandate is performative.
constraint_indexing:constraint_classification(global_stimulus_spree, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT ENERGY & AUTO FIRMS (TANGLED ROPE) — Benefit from green energy subsidies but face transition risk. Extraction: they capture transition rents while fossil fuel assets remain stranded. Constrained exit: regulatory mandates lock them into expensive retooling. Both coordination (stimulus toward decarbonization) and asymmetric extraction (winners picked by government allocation).
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: OPEN ECONOMY ANALYTICAL VIEW (SCAFFOLD) — Stimulus surge is temporary response to AI-driven labor disruption and geopolitical competition. If technology adoption accelerates productivity sufficiently (and if political will aligns to restructure entitlements), debt ratios revert to sustainability within 15-20 years. Sunset clause: depends on whether real innovation converts stimulus-driven R&D into genuine new economic capacity. Currently aspirational.
constraint_indexing:constraint_classification(global_stimulus_spree, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: MACROECONOMIC REALIST (TANGLED ROPE) — Stimulus surge reflects genuine coordination problem: AI transition is real, defense competition is real, climate investment is real. But the solution chosen (multitrillion fiscal transfers) also exhibits pure extraction: incumbent firms capture rents, future taxpayers bear costs, monetary-fiscal coordination creates hidden taxation via inflation. Both coordination function and asymmetric extraction present simultaneously.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_stimulus_spree_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_stimulus_spree, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_stimulus_spree, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_stimulus_spree, TR),
    TR >= 0.70.

:- end_tests(global_stimulus_spree_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Base extraction reflects that stimulus-driven rents flow to incumbent firms and institutional investors while future taxpayers bear debt service. Not maximum (0.75+) because the stimulus partly targets genuine public goods (AI infrastructure is useful; climate investment is necessary). The extraction increases over the 6-year interval as initial 'temporary measures' rhetoric solidifies into permanent structural changes. Suppression (0.68): High. Barriers to exit include capital controls preventing capital flight; regulatory mandates locking firms into government-directed sectors; fiscal dominance preventing central bank independence; political difficulty of debt reversals. Future taxpayers have no exit whatsoever. Developing economies face constrained exit via lack of policy autonomy. Theater ratio (0.61): Moderate-high. Government communications emphasize urgency, competitiveness, and temporary crisis response. Central bank rhetoric maintains fiction of eventual rate normalization and quantitative tightening. Actual policy (permanent accommodative monetary stance, entitlement expansion, picking winners in AI/defense/energy) is far more sticky than rhetoric suggests. The ratio rises over the interval as the gap between stated intentions and actual persistence becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum perspectival divergence because observers at different power levels and time horizons have genuinely different experiences. A tech corporation executive (institutional, immediate, arbitrage) sees Rope — a well-designed coordination mechanism that solves the real problem of underinvestment in AI. A future taxpayer (powerless, generational, trapped) sees Snare — pure extraction with no coordination benefit. A developing economy central banker (moderate, biographical, constrained) sees Tangled Rope — some spillover benefit from AI infrastructure and green energy investment, but the dominant experience is capital flight and constrained policy autonomy. A macroeconomic analyst (analytical, civilizational) sees either Scaffold (if productivity gains materialize and debt becomes manageable) or Snare (if debt persists and fiscal dominance is permanent). The perspectival gap is not observational ambiguity but structural reality: the stimulus mechanism genuinely benefits some agents and harms others along different time horizons. Future costs are externalized to agents without current political voice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's effective extraction (chi) is computed from base extractiveness (0.58), their directional relationship (d), and scope modifier. Beneficiaries like tech corporations have low d (they benefit from stimulus flow) and experience chi near or below zero — the constraint subsidizes them. Future taxpayers have d ≈ 1.0 (full targets) with f(d) amplifying chi. Developing economies face d ≈ 0.85 (mostly victims, some spillover) and experience high chi due to constrained exit options. Central banks have d ≈ 0.55 (symmetric: they benefit from fiscal coordination but lose independence) and experience moderate chi. The divergence is driven by beneficiary/victim declarations (tech corporations are beneficiaries; taxpayers are victims) and by exit options (tech corporations have arbitrage; future taxpayers have trapped exit; developing economies have constrained exit). The engine derives d automatically from these structural parameters.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy (confusion between coordination and extraction) is resolved by recognizing that BOTH functions are genuinely present. The stimulus is NOT pure coordination masquerading as extraction, nor pure extraction disguised as coordination. It is a Tangled Rope: it solves the real coordination problem of AI transition, climate investment, and geopolitical competition (justifying some fiscal expansion). But the method of finance (permanent debt issuance, monetary subordination, rent-picking) is extraction (benefiting incumbent firms and institutional investors while externalizing costs to future taxpayers and developing economies). The mandatrophy is resolved by declaring beneficiaries explicitly (tech corporations, defense contractors, institutional investors) and victims explicitly (future taxpayers, fiscal sustainability, developing economies). The presence of both beneficiaries AND victims, plus active enforcement (government procurement policies, regulatory mandates, central bank coordination), satisfies the Tangled Rope canonical gate. The high mandatrophy_resolved flag (true) reflects that the stimulus surge exhibits genuine coordination function AND asymmetric extraction in a structurally entangled system. The analytical observer must resist both the 'this is just necessary investment' framing (which naturalizes the extraction) and the 'this is pure rent-seeking' framing (which erases the real coordination problem). The true structure is hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_productivity_realization,
    'Will AI-driven productivity gains materialize sufficiently to justify multitrillion-dollar investment and debt accumulation?',
    'Total Factor Productivity (TFP) measurements 2026–2035; labor displacement rates vs retraining success; real wage growth by sector',
    'If realized: constraint shifts toward Scaffold (temporary, productive). If unrealized: constraint remains Snare (pure extraction disguised as investment).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_productivity_realization, empirical, 'Whether AI productivity gains justify stimulus magnitude').

omega_variable(
    fiscal_dominance_threshold,
    'At what debt-to-GDP ratio does fiscal dominance force central banks to permanently abandon inflation targeting?',
    'Historical breakpoint analysis; central bank forward guidance shifts; inflation expectations survey data; term premium widening',
    'If threshold crossed: hidden inflation tax becomes explicit, eroding nominal debt but generating supply shocks. Shifts from Tangled Rope to Snare for developed-economy savers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_dominance_threshold, empirical, 'Debt threshold forcing permanent monetary subordination').

omega_variable(
    coordinated_stimulus_legitimacy,
    'Is the stimulus genuinely coordinated (all governments targeting same AI/climate/defense objectives) or competitive rent-seeking (each grabbing subsidies for local champions)?',
    'Comparative analysis of stimulus allocation across countries; patent filing rates by subsidized sector; antitrust enforcement against foreign tech in stimulus beneficiary countries',
    'If coordinated: extraction is lower (resources allocated to genuine public goods). If competitive: extraction is higher (zero-sum subsidy warfare, deadweight loss).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_stimulus_legitimacy, empirical, 'Whether stimulus reflects coordination or competitive rent-seeking').

omega_variable(
    developing_economy_contagion,
    'How many developing economies will experience debt crises or currency collapse due to stimulus-driven capital flight and rate hiking?',
    'Sovereign default counts 2026–2030; currency depreciation severity; multilateral bailout activation rates',
    'If < 5 countries: contagion manageable, developing economies see some spillover benefit. If > 10 countries: cascading crises, stimulus extraction becomes explicit imperialism via financial channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_economy_contagion, empirical, 'Developing economy crisis severity from developed-market stimulus').

omega_variable(
    inflation_reversion_dynamics,
    'If CPI reaccelerates above 4% in 2027–2028, will governments abandon fiscal stimulus or double down with price controls and income policies?',
    'Central bank policy rate trajectories; government rhetoric shifts; price control implementations; real wage behavior',
    'If abandoned: shorter extraction window, Scaffold logic validated. If doubled down: stagflation risk, Snare extraction locked in across supply chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_reversion_dynamics, preference, 'Political response if inflation resurges').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_stimulus_spree, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stim_tr_t0, global_stimulus_spree, theater_ratio, 0, 0.48).
narrative_ontology:measurement(stim_tr_t3, global_stimulus_spree, theater_ratio, 3, 0.56).
narrative_ontology:measurement(stim_tr_t6, global_stimulus_spree, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(stim_be_t0, global_stimulus_spree, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stim_be_t3, global_stimulus_spree, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(stim_be_t6, global_stimulus_spree, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_stimulus_spree, resource_allocation).
narrative_ontology:affects_constraint(global_stimulus_spree, sovereign_debt_spiral).
narrative_ontology:affects_constraint(global_stimulus_spree, developing_economy_capital_flight).
narrative_ontology:affects_constraint(global_stimulus_spree, monetary_fiscal_dominance).
narrative_ontology:affects_constraint(global_stimulus_spree, ai_winner_picking).

% DUAL FORMULATION NOTE:
% The global stimulus surge decomposes into multiple dependent constraints: (1) resource allocation mechanism (this constraint) with ε=0.58, (2) sovereign debt accumulation (ε=0.72, downstream Snare), (3) capital flight mechanics (ε=0.65, Snare from developing-economy perspective), (4) monetary subordination (ε=0.51, Piton). These are structurally linked — the stimulus constraint triggers debt accumulation, which forces monetary dominance, which concentrates extraction. Each has its own ε and perspectives; the network captures causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_stimulus_spree, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
