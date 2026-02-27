% ============================================================================
% CONSTRAINT STORY: geopolitical_insularity_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_insularity_2026, []).

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
 *   constraint_id: geopolitical_insularity_2026
 *   human_readable: Geopolitical Nationalist Insularity
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The 2026 Great Realignment represents a structural shift in global
 *   institutional architecture where nationalist security doctrine becomes
 *   the primary organizing principle of trade, immigration, and technology
 *   policy. This constraint arises from the strategic decision by major
 *   powers to treat cross-border trust as a zero-sum nationalist asset: what
 *   strengthens one nation's security (supply chain autonomy, border control,
 *   technology decoupling) weakens others' economic efficiency and openness.
 *   The constraint exhibits classic tangled-rope structure: it provides
 *   genuine coordination benefit to nationalist security apparatuses
 *   (de-risked supply chains, border sovereignty) while extracting from those
 *   dependent on open trade (workers, export economies, multilateral
 *   institutions). The theater ratio (0.65) reflects the performative
 *   dimension of nationalist rhetoric—institutional actors invoke security
 *   necessity while actual policy outcomes often reflect incumbent firm
 *   protection and regulatory capture. The rising trajectory of both theater
 *   and extractiveness indicates increasing institutional inertia: as supply
 *   chain fragmentation becomes embedded, it becomes harder to reverse even
 *   if security rationale weakens.
 *
 * KEY AGENTS:
 *   - Nationalist Security Apparatus: Primary beneficiary (institutional/arbitrage) — military, intelligence, homeland security gain legitimacy and operational control from insularity mandate
 *   - Concentrated Domestic Capital: Secondary beneficiary (powerful/arbitrage) — strategic sector firms (energy, defense, semiconductors, infrastructure) benefit from tariff protection and supply chain reorientation
 *   - Global Supply Chain Workers: Primary victim (powerless/trapped) — factory workers, logistics employees, cross-border traders trapped by tariffs and visa restrictions with no exit
 *   - Dependent Export-Oriented Economies: Secondary victim (moderate/constrained) — nations structurally dependent on open trade face tariffs, capital flight, and economic contraction; cannot simply pivot to autarky
 *   - Regional Trading Blocs: Inter-institutional actor (organized/constrained) — EU, ASEAN, MERCOSUR experience insularity as mixed coordination-extraction within bloc boundaries
 *   - Multilateral Institutions: Institutional degradation (institutional/arbitrage) — WTO, IMF, World Bank maintain formal authority but lack enforcement against nationalist defection; sustain through theater
 *   - Transnational Climate & Tech Coalition: Organized exit path (organized/mobile) — scientists, green-tech firms perceive supply chain re-integration pathway through renewable energy transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_insularity_2026, 0.58).
domain_priors:suppression_score(geopolitical_insularity_2026, 0.68).
domain_priors:theater_ratio(geopolitical_insularity_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_insularity_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_insularity_2026, tangled_rope).
narrative_ontology:human_readable(geopolitical_insularity_2026, "Geopolitical Nationalist Insularity").
narrative_ontology:topic_domain(geopolitical_insularity_2026, "geopolitical/economic").

domain_priors:requires_active_enforcement(geopolitical_insularity_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, nationalist_security_apparatus).
narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, domestic_capital_concentrated_sectors).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, global_trade_networks).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, multilateral_institutions).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, open_border_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN WORKERS (SNARE) — Caught in nationalist insularity with no exit. Trade barriers, supply chain fragmentation, and protectionist enforcement trap workers in lower-wage domestic sectors or unemployment. No arbitrage available; mobility constrained by visa nationalism. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT EXPORT-ORIENTED ECONOMIES (SNARE) — Nations with structural dependence on open trade face escalating tariffs, supply chain reorientation, and capital flight. Constrained exit: cannot simply pivot to autarky without internal collapse. d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL TRADING BLOCS (TANGLED ROPE) — EU, ASEAN, MERCOSUR experience insularity as mixed: regionalism provides coordination (internal market deepening) but creates extraction asymmetries (core vs periphery nations, rules-setting power concentration). Active enforcement of bloc identity replaces global coordination. d≈0.58, f(d)≈0.72, σ=1.1 → χ≈0.46.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: NATIONALIST SECURITY APPARATUS (ROPE) — Military, intelligence, homeland security see insularity as pure coordination: borders are secured, supply chains de-risked, strategic autonomy enhanced. No extraction perceived; institutional actors capture legitimacy from nationalist mandate. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.005. Net beneficiary.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONCENTRATED DOMESTIC CAPITAL (SNARE ADJACENT / ROPE HYBRID) — Large domestic firms in protected sectors (energy, defense, infrastructure, tech champions) benefit from tariffs and supply chain reorientation. Can arbitrage between protected home market and selective exports. However, forced local sourcing and technology transfer requirements create extraction burden. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.09. Moderate beneficiary.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL INSTITUTIONS (PITON) — WTO, IMF, World Bank maintain rule-making and dispute resolution functions but lack enforcement power against nationalist defection. Theater ratio = 0.65: dispute panels issue rulings that nations ignore; institutions perform legitimacy while losing functional authority. d≈0.10, f(d)≈-0.06, σ=1.2 → χ≈-0.005. Degraded from their post-1945 coordination role.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: TRANSNATIONAL CLIMATE & TECH COALITION (SCAFFOLD) — Scientists, green-tech firms, digital platforms see nationalist insularity as a temporary bottleneck with a sunset. Carbon pricing and renewable supply chains create incentives for re-integration. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.30. Low effective extraction because coalition perceives exit path through green transition.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / REALIST IR (FALSE MOUNTAIN) — From civilizational view, anarchy in international relations creates inherent security dilemma: states must assume worst-case intent from rivals. Trust is genuinely zero-sum under anarchy. However, structural data (ε=0.58, suppression=0.68, theater=0.65) shows this is contingent institutional choice, not natural law. The 'realist' mountain is a false summit revealing how ideology naturalizes extraction.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_insularity_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_insularity_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_insularity_2026, TR),
    TR >= 0.70.

:- end_tests(geopolitical_insularity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint creates genuine asymmetry in welfare: nationalist security gains ≈ 0.20 per capita for core security actors; extraction costs ≈ 0.15-0.25 per capita for dependent economies and supply chain workers. The net global welfare effect is negative (standard trade models predict 1-2% GDP loss from regionalization). However, the constraint is not maximally extractive (ε ≤ 0.66 for snare) because: (1) regional blocs provide some coordination benefit internally; (2) security benefits are real, not pure theater; (3) exit options exist but are costly rather than impossible. Suppression (0.68): High. Multiple enforcement mechanisms: tariffs and trade enforcement; visa restrictions and border controls; technology decoupling and supply chain rules; sanctions against defectors. Suppression increased sharply 2024-2026 as nationalist governments hardened policy and reversed prior trade commitments. Theater ratio (0.65): High-moderate. Reflects that security rhetoric often masks incumbent firm protection (regulatory capture), but security benefits are also genuine. Multilateral institutions perform authority they no longer wield. Regional blocs perform unity they strain to maintain. Theater increased from 0.35 to 0.65 as policies became more entrenched and performative (border theater, nationalist speeches) replaced genuine security analysis.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. Nationalist security apparatus (rope/beneficiary) sees pure coordination: borders secured, supply chains de-risked, strategic autonomy gained. Concentrated capital (rope/beneficiary) sees protection: tariffs maintain margins, competitors locked out, market share secured. Supply chain workers (snare/victim) see pure extraction: reduced wages, unemployment, no exit. Dependent export economies (snare/victim) see existential threat: tariffs destroy their model, capital flees, domestic instability rises. Regional blocs (tangled rope) see mixed: internal coordination deepens, but extraction asymmetries emerge (large vs small nations within bloc, core vs periphery). Multilateral institutions (piton) see their own degradation: they issue rulings that nations ignore, they coordinate at edges while centers defect. Transnational climate coalition (scaffold) sees a temporary problem with a sunset: renewable supply chains force re-integration. Analytical observer (false mountain/realist IR) naturalizes the constraint as inherent anarchy but structural data reveals it as contingent policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Nationalist security apparatus: Beneficiary + arbitrage → d≈0.08. Derived from institutional power + arbitrage exit + beneficiary role. Net beneficiary with negative χ. Concentrated capital: Beneficiary + arbitrage, but with forced local sourcing → d≈0.25. Partial extraction burden from regulatory capture. Global supply chain workers: Victim + trapped → d≈0.92. Powerless agents with no meaningful exit from tariffs/visas. Maximum effective extraction. Dependent export economies: Victim + constrained → d≈0.85. Cannot exit (no alternative market access) but some policy lever (RCEP, USMCA, regional negotiation). High effective extraction. Regional blocs: Mixed (coordination + asymmetric power) → d≈0.58 for smaller members, 0.25 for bloc cores. Tangled rope classification reflects this internal divergence. Multilateral institutions: Institutional + arbitrage, but degraded → d≈0.10, theater masks loss of function. Transnational coalition: Organized + mobile → d≈0.45, sees exit path via green transition.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLVED: The constraint satisfies all three gates: (1) Beneficiaries present (nationalist apparatus, concentrated capital) and derive genuine coordination benefit (supply chain autonomy, border control); (2) Victims present (supply chain workers, export economies) and bear genuine extraction costs (wage pressure, tariff damage); (3) Active enforcement required and present (tariff agencies, border control, technology restrictions, sanctions). The coordination function is NOT incidental—de-risking supply chains and securing borders solve real problems for state security actors. The extraction function is NOT accidental—it emerges from the asymmetry between those who benefit from autarky (security apparatus, incumbent firms) and those who lose from trade reduction (workers, export-dependent nations). The mandatrophy is resolved by recognizing that insularity is not 'coordination masquerading as extraction' (which would be snare) nor 'extraction masquerading as coordination' (which would be regulation failure). It is genuinely both: a coordination mechanism for nationalist security that structurally requires extraction from open-trade beneficiaries. This is the definition of tangled rope: hybrid coordination-extraction with asymmetric power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_dilemma_irreducibility,
    'Is nationalist insularity a response to genuine security interdependence (structural anarchy) or a policy choice that could be reversed through institutional design?',
    'Historical counterfactual analysis: comparison of security outcomes under open vs closed trade regimes; game-theoretic modeling of trust-building mechanisms under different institutional frameworks',
    'If irreducible: mountain classification valid. If policy choice: tangled rope classification confirmed; insularity is contingent extraction wrapped in security rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_irreducibility, conceptual, 'Whether insularity is structurally necessary or institutionally chosen').

omega_variable(
    supply_chain_fragmentation_cost,
    'What is the actual welfare cost of supply chain fragmentation compared to the security benefit of autarky/nearshoring?',
    'Macroeconomic modeling of productivity loss from supply chain re-duplication; measurement of security premium paid by nations adopting reshoring policies; comparison of inflation/growth rates pre- and post-insularity',
    'If cost > security benefit: extraction mechanism is masked by security narrative. If cost ≈ benefit: genuine mixed coordination-extraction (tangled rope confirmed). If security benefit > cost: coordination value is underestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_fragmentation_cost, empirical, 'Welfare trade-off between supply chain autarky and security').

omega_variable(
    institutional_capture_nationalism,
    'To what extent does nationalist rhetoric serve as cover for regulatory capture by incumbent domestic firms seeking tariff protection?',
    'Analysis of protectionist policy outcomes: sectoral distribution of tariffs and subsidies; correlation between trade barriers and firm profitability; lobbying expenditure by protected industries',
    'If capture > 40%: insularity is primarily a snare disguised as rope (security narrative masks extraction). If capture < 20%: security rationale is more genuine than critics claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_nationalism, empirical, 'Degree of incumbent firm capture in protectionist policy').

omega_variable(
    visa_nationalism_migration_trap,
    'Are visa restrictions and border nationalism creating a permanent trap for global supply chain workers, or are they temporary institutional responses that will eventually ease?',
    'Trend analysis of visa approvals, refugee admissions, and labor mobility metrics; assessment of political economy drivers of restrictionism vs reopening; demographic and labor shortage pressure modeling',
    'If permanent trap: victims are genuinely trapped (d≈0.95). If temporary: exit options improve to ''constrained'' or ''mobile'' as labor scarcity forces policy revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visa_nationalism_migration_trap, preference, 'Whether visa nationalism is temporary or structural').

omega_variable(
    multilateral_institution_revival,
    'Can WTO, IMF, and World Bank regain functional authority through reformed rules that accommodate regional blocs and security exemptions?',
    'Tracking of institution reform proposals; evidence of nations re-committing to dispute resolution; measurement of compliance rates with institutional rulings post-reform',
    'If revival succeeds: piton transitions back to rope; multilateral coordination recovers. If revival fails: piton persists or degrads further to theater-only (ε→0, theater→1.0).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_institution_revival, preference, 'Whether multilateral institutions can recover from nationalist defection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_insularity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geopol_insul_tr_t0, geopolitical_insularity_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(geopol_insul_tr_t5, geopolitical_insularity_2026, theater_ratio, 5, 0.52).
narrative_ontology:measurement(geopol_insul_tr_t10, geopolitical_insularity_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(geopol_insul_be_t0, geopolitical_insularity_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(geopol_insul_be_t5, geopolitical_insularity_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(geopol_insul_be_t10, geopolitical_insularity_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_insularity_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, semiconductor_supply_chain_dependency).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, rare_earth_monopoly_extraction).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, carbon_tariff_enforcement).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, digital_sovereignty_balkanization).

% DUAL FORMULATION NOTE:
% Geopolitical insularity represents a meta-constraint on multiple downstream constraints (supply chains, technology, climate, digital infrastructure). Each downstream constraint experiences insularity's enforcement mechanism differently depending on whether it aligns with nationalist security doctrine (carbon tariffs aligned = low ε impact; semiconductor independence mandates aligned = medium ε impact; open digital platforms misaligned = high ε impact). The network effects are asymmetric: insularity constraints ALL downstream constraints, but not all downstream constraints equally reinforce insularity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_insularity_2026, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
