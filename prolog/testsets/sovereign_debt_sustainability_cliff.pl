% ============================================================================
% CONSTRAINT STORY: sovereign_debt_sustainability_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_debt_sustainability_cliff, []).

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
 *   constraint_id: sovereign_debt_sustainability_cliff
 *   human_readable: Sovereign Debt Sustainability Cliff
 *   domain: macroeconomics/sovereign_finance
 *
 * SUMMARY:
 *   Sovereign debt sustainability creates a structural asymmetry where
 *   creditor institutions capture benefits from capital flows while debtor
 *   nations bear risks of fiscal constraint, austerity, and service
 *   interruption. The constraint operates through the interaction of three
 *   mechanisms: (1) creditor enforcement threats (market access denial, asset
 *   seizure, legal suits), (2) denominated currency dependency (foreign
 *   currency debt amplifies sovereignty loss), and (3) capital flight (wealth
 *   holders exit in advance of fiscal crisis, leaving the nation bearing net
 *   liability). The sustainability cliff is not a fixed mathematical property
 *   but emerges from political relationships between creditors (who can
 *   restructure, demand collateral, or accept losses) and debtors (who cannot
 *   unilaterally exit the creditor relationship). The theater ratio (0.58)
 *   reflects that institutional claims of absolute payment obligation coexist
 *   with historical patterns of successful default and debt forgiveness — the
 *   constraint persists through maintained fiction that payment is
 *   non-negotiable, not through enforcement mechanisms that make non-payment
 *   impossible.
 *
 * KEY AGENTS:
 *   - Debtor Nation Population: Primary victim (powerless/trapped) — bears austerity, service reduction, currency collapse; no exit mechanism from national fiscal trajectory
 *   - Public Service Capacity: Structural victim (moderate/constrained) — healthcare, education, infrastructure deteriorate under servicing burden; constrained by revenue capture demands
 *   - Creditor Institutions: Primary beneficiary (institutional/arbitrage) — capture interest returns, restructuring fees, asset sales during crises; mobile through diversification and secondary markets
 *   - International Financial System: Organized beneficiary (organized/constrained) — has genuine stability coordination interest but enforces asymmetric conditionality; both benefits from coordination and extracts through structural adjustment
 *   - Regional Restructuring Coalitions: Temporary relief organizers (organized/mobile) — Paris Club, Brady agreements represent organizational attempts to create sunset mechanisms; empirically often recreate similar constraints
 *   - Gold Standard Debt Servicing Norm: Institutional theatrical actor (institutional/arbitrage) — maintains fiction of absolute payment obligation; persists through inertia despite empirical counterexamples of successful default
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political relationships (creditor power, currency regime, capital flight) as immutable mathematical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_debt_sustainability_cliff, 0.68).
domain_priors:suppression_score(sovereign_debt_sustainability_cliff, 0.72).
domain_priors:theater_ratio(sovereign_debt_sustainability_cliff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_debt_sustainability_cliff, extractiveness, 0.68).
narrative_ontology:constraint_metric(sovereign_debt_sustainability_cliff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereign_debt_sustainability_cliff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_debt_sustainability_cliff, snare).
narrative_ontology:human_readable(sovereign_debt_sustainability_cliff, "Sovereign Debt Sustainability Cliff").
narrative_ontology:topic_domain(sovereign_debt_sustainability_cliff, "macroeconomics/sovereign_finance").

domain_priors:requires_active_enforcement(sovereign_debt_sustainability_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability_cliff, creditor_institutions).
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability_cliff, external_lenders).
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability_cliff, international_financial_institutions).
narrative_ontology:constraint_victim(sovereign_debt_sustainability_cliff, debtor_nation_population).
narrative_ontology:constraint_victim(sovereign_debt_sustainability_cliff, public_service_capacity).
narrative_ontology:constraint_victim(sovereign_debt_sustainability_cliff, domestic_fiscal_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION POPULATION (SNARE) — Citizens of a sovereign defaulting regime face domestic austerity, currency collapse, and loss of access to international markets. No exit mechanism; cannot leave the nation's fiscal trajectory. Bears maximum extraction through reduced social services, unemployment, and wealth destruction. Suppression is structural and comprehensive.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SERVICE CAPACITY (SNARE) — Healthcare, education, and infrastructure deteriorate under debt servicing burden. Constrained by revenue capture demands; degradation is slow but inevitable. Can be partially arrested through external grants or debt forgiveness, but these come with enforcement conditions that recreate the constraint. Generational horizon reveals compounding damage.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR INSTITUTIONS (ROPE) — From immediate perspective, debt servicing is coordination: capital flows are reciprocated through scheduled payments. Arbitrage options abundant: creditors can diversify, demand collateral, restructure terms, or exit through secondary markets. Net beneficiary experiencing the constraint as beneficial coordination mechanism. High effective negative extraction (subsidy).
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL FINANCIAL SYSTEM STABILITY (TANGLED ROPE) — The global financial system has genuine coordination interest in sustainable debt trajectories — contagion and cascading defaults harm all participants. But this coordination interest coexists with asymmetric extraction: IMF/World Bank impose structural adjustment conditionality, requiring privatization, labor deregulation, and social service reduction. Organized actors (central banks, multilateral institutions) benefit from enforcement; debtor nations bear costs. Constraints binding through treaty obligations and market access dependencies.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL DEBT RESTRUCTURING COALITION (SCAFFOLD) — Organized coalitions (Paris Club, Brady Plan countries, ASEAN initiatives) represent temporary coordination mechanisms with genuine sunset clauses. Restructuring agreements are time-bound; conditionality is meant to be temporary. But empirically, restructuring often recreates similar constraints with different lenders — the scaffold persists. Classified as scaffold because the organizing logic is temporary relief, not permanent extraction, even though the empirical outcome often shows recurrence.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: GOLD STANDARD DEBT SERVICING NORM (PITON) — The institutional mandate that sovereign debts must be serviced regardless of domestic welfare cost persists through inertia. Empirically, many sovereigns have successfully defaulted and recovered without compliance to external enforcement; the norm is no longer functionally necessary. But multilateral institutions and creditors maintain the theatrical assertion that non-payment is unthinkable, supported by market access denial. Theater ratio reflects the gap between the ritual assertion of absolute obligation and the empirical reality of successful debt forgiveness and restructuring. Piton classification derives from high theater, not from high experienced extraction per se.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, debt sustainability appears as a mathematical law: interest accumulation at rates above growth creates inevitable default. This naturalizes a contingent institutional arrangement (creditor-favoring contract terms, capital flight, currency denomination dependency) as an immutable economic fact. The engine's false summit detector will flag this as naturalization. Debt sustainability is not a law of nature but a property of political relationships, interest rates, and currency regimes — all contingent.
constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_debt_sustainability_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_debt_sustainability_cliff, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_debt_sustainability_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_debt_sustainability_cliff, TR),
    TR >= 0.70.

:- end_tests(sovereign_debt_sustainability_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting strong asymmetry in who benefits vs. who bears costs. Creditor institutions capture returns and restructuring value; debtor populations bear austerity and welfare loss. The value increases over time (0.38 → 0.68) as debt accumulates and servicing crowds out public spending. This is not a fixed constraint but an accumulating one — extraction intensity rises as the sustainability cliff approaches. Suppression (0.72): Very high. Mechanisms include: (a) legal/treaty binding (debt contracts are internationally enforceable through creditor legal actions), (b) market access denial (non-payment triggers capital markets closure), (c) currency dependency (foreign-denominated debt makes local monetary sovereignty partial), (d) capital flight (wealthy holders exit in advance, concentrating losses on general population). No single mechanism is absolute, but collectively they are comprehensive. Theater ratio (0.58): Moderate-high, reflecting the gap between the institutional theater of absolute payment obligation and the empirical reality of successful defaults (Argentina 2001, Greece 2010s, Ecuador 2008). The theater maintains institutional credibility through selective enforcement (some debtors are punished, others forgiven) and the 'exceptional circumstance' framing (claiming each default is unique rather than predictable from structural conditions). The theater increases over time (0.42 → 0.58) as the gap between payment fiction and default reality widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Creditor institutions see coordination (Rope) — capital flows are beneficial, reciprocated through scheduled payments, diversifiable through secondary markets. Debtors see extraction (Snare) — payments are mandatory, non-negotiable, and force welfare reduction. The international financial system sees both — genuine coordination interest in stability coexists with enforcement extraction. Organized debtor coalitions see a solvable problem (Scaffold) — temporary restructuring can reset dynamics. But the gold standard servicing norm sees an inert ritual (Piton) — the assertion that non-payment is unthinkable persists despite historical counterexamples. The civilizational analytical view risks false mountain classification, treating the constraint as a mathematical law of debt accumulation, which naturalizes what are actually political relationships (creditor enforcement capacity, currency regime choice, capital flight regulation). The perspectival gap reveals the constraint is not a law but a contingent institutional arrangement sustained by maintained fiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's structural relationship to the constraint. Creditor institutions derive d from beneficiary status + arbitrage exit options → low d → negative f(d) → negative χ (they experience subsidy). Debtor populations derive d from victim status + trapped exit options → high d → high f(d) → high χ (maximum experienced extraction). Organized coalitions (international financial system, regional restructuring groups) have mixed d values: they benefit from coordination (low d from beneficiary side) but also extract through enforcement conditions (high d from victim side) — this is the tangled rope directionality. The scope modifier σ(S) also matters: national-scope measurement (single debtor nation) understates extraction because it misses the global creditor coordination benefits; global-scope measurement correctly shows the asymmetry (creditors coordinate across nations while each debtor is isolated).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint resolves the tension between classification as pure extraction (snare) vs. coordination (rope) by showing that both are legitimate perspectival readings. The mandatrophy is resolved through multi-perspective indexing: the snare classification is the debtor population's structural reality; the rope classification is the creditor institution's structural reality; the tangled rope is the international financial system's mixed interest; the scaffold reflects debtor coalition organizing; the piton reveals institutional inertia in enforcement theater; the mountain is a false summit (naturalization of political relationships as mathematical laws). No single type 'solves' the constraint — the constraint exists because the perspectives are incommensurable. Creditors benefit from the current structure; debtors do not. No reformulation of the constraint resolves this asymmetry within the existing institutional framework. This is why the constraint is classified as a snare from the powerless perspective — it is not resolvable through better coordination or information, only through redistribution of bargaining power (debt forgiveness, creditor loss acceptance, debtor coalition formation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_enforcement_capacity,
    'What enforcement mechanisms actually prevent sovereign default, and how robust are they to coalitional defection?',
    'Historical analysis of successful defaults vs. enforced repayment; correlation between enforcement threats and payment behavior; empirical estimation of creditor loss under non-compliance scenarios',
    'If enforcement capacity is robust: snare classification is accurate. If creditors can be defected from: the trap has exit routes, reclassifying toward tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_enforcement_capacity, empirical, 'Creditor enforcement capacity and creditor coalition robustness').

omega_variable(
    debt_servicing_welfare_tradeoff,
    'At what ratio of debt service to GDP does optimal sovereign policy shift from payment to default?',
    'Empirical estimation of welfare-maximizing debt service thresholds; comparison of long-term outcomes for nations that serviced vs. defaulted at different ratios; analysis of conditional debt forgiveness effectiveness',
    'If threshold is low (< 20% of revenue): snare classification is firm — payment obligation creates unambiguous welfare loss. If threshold is high (> 40%): payment may be rational welfare choice, weakening snare and suggesting constrained/tangled_rope dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_servicing_welfare_tradeoff, empirical, 'Welfare-optimal debt service vs. payment threshold').

omega_variable(
    structural_adjustment_conditionality_effectiveness,
    'Do IMF/World Bank structural adjustment conditions actually improve long-term fiscal sustainability, or do they reduce short-term extraction surface without changing underlying dynamics?',
    'Longitudinal comparison of post-adjustment outcomes; analysis of debt recurrence following restructuring; estimation of whether conditionality reduces or redistributes extraction',
    'If conditions improve sustainability: tangled_rope classification confirmed (genuine coordination + extraction). If conditions are theater: reclassify toward piton (performative enforcement with degraded function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_adjustment_conditionality_effectiveness, empirical, 'Structural adjustment conditionality effectiveness for sustainability').

omega_variable(
    currency_regime_exit_availability,
    'Can a sovereign meaningfully exit debt servicing obligations through currency regime change (dollarization, hyperinflation, monetary sovereignty)?',
    'Historical analysis of currency regimes and debt default patterns; estimation of exit costs vs. servicing costs; analysis of dollarized economies and their debt trajectories',
    'If currency exit is available and low-cost: exit_options upgrade from trapped to constrained/mobile, reclassifying from snare toward tangled_rope. If currency is locked (dollarization, reserve currency dependency): trap remains firm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(currency_regime_exit_availability, empirical, 'Currency regime flexibility for debt exit').

omega_variable(
    capital_flight_extraction_interaction,
    'Does the debt sustainability constraint enable or require capital flight, and does flight itself constitute a primary extraction mechanism separate from debt servicing?',
    'Estimation of capital flight volumes relative to debt service; tracking of wealth transfers during debt crises; analysis of whether capital flight precedes or follows debt constraint binding',
    'If flight is endogenous to the constraint: extractiveness may be understated (flow includes both debt service + capital flight). If flight is exogenous: constraint is narrower than appears. Classification may shift toward snare if extraction is higher than estimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_extraction_interaction, empirical, 'Capital flight as endogenous vs. exogenous extraction mechanism').

omega_variable(
    collective_default_coalition_formation,
    'Under what conditions do debtor sovereigns successfully form coalitions to reject creditor enforcement?',
    'Historical analysis of debtor cartels and default coordination; estimation of creditor losses under coalition scenarios; tracking of coalition formation dynamics during debt crises',
    'If coalitions are feasible and low-cost to form: snare shifts toward tangled_rope (extraction persists but victims have exit option through coordination). If coalition formation is suppressed: snare is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_default_coalition_formation, empirical, 'Debtor coalition capacity for coordinated default').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_debt_sustainability_cliff, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_tr_t0, sovereign_debt_sustainability_cliff, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sdc_tr_t10, sovereign_debt_sustainability_cliff, theater_ratio, 10, 0.5).
narrative_ontology:measurement(sdc_tr_t20, sovereign_debt_sustainability_cliff, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(sdc_be_t0, sovereign_debt_sustainability_cliff, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sdc_be_t10, sovereign_debt_sustainability_cliff, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sdc_be_t20, sovereign_debt_sustainability_cliff, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_debt_sustainability_cliff, resource_allocation).
narrative_ontology:affects_constraint(sovereign_debt_sustainability_cliff, capital_flight_suppression).
narrative_ontology:affects_constraint(sovereign_debt_sustainability_cliff, currency_sovereignty_subordination).
narrative_ontology:affects_constraint(sovereign_debt_sustainability_cliff, structural_adjustment_enforcement).

% DUAL FORMULATION NOTE:
% The sovereign debt sustainability cliff decomposed into three downstream constraints: (1) capital_flight_suppression (wealth extraction mechanism independent of debt servicing), (2) currency_sovereignty_subordination (loss of monetary policy autonomy through foreign-denominated debt), (3) structural_adjustment_enforcement (conditionality extraction layer imposed during restructuring). Each has distinct ε; this story captures the aggregate constraint. Network linkage enables contamination propagation: if one constraint (e.g., currency_sovereignty) is relaxed, pressure shifts to others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_debt_sustainability_cliff, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
