% ============================================================================
% CONSTRAINT STORY: central_bank_independence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_central_bank_independence, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: central_bank_independence
 *   human_readable: Central Bank Independence as Institutional Constraint
 *   domain: political_economy/monetary_policy
 *
 * SUMMARY:
 *   Central bank independence (CBI) represents a global institutional
 *   innovation where monetary policy authority is constitutionally or legally
 *   separated from electoral control. The constraint operates through a
 *   formal mechanism (central bank charters and legislation) while its
 *   legitimacy rests on technocratic credentials and inflation-control
 *   outcomes. CBI exhibits the complete spectrum of DR classification from
 *   different perspectives: appears as natural law (mountain) from
 *   time-consistency theory, as pure extraction (snare) for
 *   employment-maximizing populations, as legitimate coordination (rope) for
 *   creditor classes and technocrats, as mixed coordination-extraction
 *   (tangled rope) for fiscal policymakers and international capital markets,
 *   as degraded ritual (piton) for democratic electoral processes, and as
 *   temporarily contestable (scaffold) for dual-mandate reform movements. The
 *   constraint's extractiveness has increased over the 20-year interval (0.35
 *   → 0.52) as globalization intensified capital mobility enforcement, and
 *   theater_ratio has risen (0.35 → 0.58) as electoral rituals continue while
 *   substantive monetary choices are excluded from democratic contestation.
 *   This story demonstrates how an institutional arrangement justified by
 *   technical necessity (time-consistency problem-solving) structurally
 *   benefits creditors while suppressing employment maximization, with the
 *   suppression masked as insulation from democratic pressure.
 *
 * KEY AGENTS:
 *   - Creditor Class & Inflation-Averse Constituencies: Primary beneficiary (institutional/arbitrage) — benefit from price stability protection of real asset values; high exit optionality via capital mobility
 *   - Debtor Population: Primary victim (powerless/trapped) — bears full cost of tight monetary policy with no electoral remedy; trapped in national currency and fiscal regime
 *   - Employment Maximizers: Secondary victim (powerless/trapped) — structural inability to enforce full employment mandate; policy preferences systematically suppressed
 *   - Central Bank Technocrats: Secondary beneficiary (institutional/arbitrage) — benefit from professional insulation and international central banking networks; experience constraint as legitimacy mechanism
 *   - Fiscal Policymakers: Mixed position (moderate/constrained) — constrained by credibility requirements but also gain coordination benefits from monetary anchor
 *   - International Capital Markets: Enforcer (powerful/mobile) — coordinate on independence norm while extracting from non-compliant jurisdictions through capital reallocation
 *   - Democratic Electoral Systems: Marginalized participant (institutional/constrained) — formal legitimacy mechanism (voting) divorced from substantive monetary control; piton classification
 *   - Dual-Mandate Reform Movements: Counter-coalition (organized/constrained) — building organized pressure for mandate expansion with generational sunset horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(central_bank_independence, 0.52).
domain_priors:suppression_score(central_bank_independence, 0.48).
domain_priors:theater_ratio(central_bank_independence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(central_bank_independence, extractiveness, 0.52).
narrative_ontology:constraint_metric(central_bank_independence, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(central_bank_independence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(central_bank_independence, tangled_rope).
narrative_ontology:human_readable(central_bank_independence, "Central Bank Independence as Institutional Constraint").
narrative_ontology:topic_domain(central_bank_independence, "political_economy/monetary_policy").

domain_priors:requires_active_enforcement(central_bank_independence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(central_bank_independence, creditor_class).
narrative_ontology:constraint_beneficiary(central_bank_independence, inflation_averse_constituencies).
narrative_ontology:constraint_beneficiary(central_bank_independence, central_bank_technocrats).
narrative_ontology:constraint_victim(central_bank_independence, employment_maximizers).
narrative_ontology:constraint_victim(central_bank_independence, debtor_populations).
narrative_ontology:constraint_victim(central_bank_independence, fiscal_policymakers).
narrative_ontology:constraint_victim(central_bank_independence, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR POPULATION (SNARE) — Trapped by monetary policy decisions made without electoral accountability. Cannot exit national currency or fiscal regime. Bears full cost of tight monetary policy (unemployment, wage suppression, debt service burden) with no exit mechanism. Maximum extraction from constrained agent.
constraint_indexing:constraint_classification(central_bank_independence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYMENT MAXIMIZERS (SNARE) — Structural inability to enforce full employment mandate. Central bank independence explicitly removes employment as a coordinating goal. Trapped by constitutional/legal barriers to policy coordination. Experience pure extraction: their policy preferences are systematically suppressed regardless of electoral outcomes.
constraint_indexing:constraint_classification(central_bank_independence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL POLICYMAKERS (TANGLED ROPE) — Constrained by credibility requirements imposed by independent monetary policy, but also benefit from the inflation-control coordination function that CBI provides. Cannot coordinate directly with central bank (legal barrier) but gain policy credibility from the monetary anchor. Mixed extraction and coordination.
constraint_indexing:constraint_classification(central_bank_independence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDITOR CLASS (ROPE) — Primary beneficiary. CBI solves the time-consistency problem of monetary policy: independent central banks deliver low inflation, protecting real asset values and purchasing power of savings. Experience the constraint as coordination mechanism enabling multi-period contracting. Net beneficiary with high exit optionality (capital mobility, currency arbitrage).
constraint_indexing:constraint_classification(central_bank_independence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANK TECHNOCRATS (ROPE) — Experience CBI as enabling coordination: independence from short-term political pressure creates space for expertise-driven policy. Benefit from the insulation from electoral cycles and populist pressure. High exit optionality via international central banking networks and academic/private sector mobility. Perceive constraint as professional legitimacy mechanism.
constraint_indexing:constraint_classification(central_bank_independence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CAPITAL MARKETS (TANGLED ROPE) — Both beneficiary and enforcer. Capital flows reward CBI credibly signal low inflation risk; flows punish abandoned independence. Markets coordinate on the independence norm while extracting from non-compliant jurisdictions through capital flight and currency depreciation. Active enforcement occurs through capital reallocation rather than formal mechanism.
constraint_indexing:constraint_classification(central_bank_independence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: DEMOCRATIC ELECTORAL SYSTEMS (PITON) — CBI represents institutionalized removal of monetary policy from electoral accountability. Appears as coordination mechanism protecting monetary credibility, but the functional coordination (price stability) is decoupled from the formal mechanism (electoral legitimacy). Theater_ratio high: elections continue but exclude central monetary choices. Theater persists through inertia — the legitimacy ritual (voting) cannot influence the constraint's operation.
constraint_indexing:constraint_classification(central_bank_independence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: DUAL-MANDATE REFORMERS (SCAFFOLD) — Organized movements to amend central bank mandates to include employment/climate alongside inflation control. See CBI as a temporary institutional arrangement with a sunset clause embedded in emerging policy consensus. Constrained by central bank resistance and creditor opposition, but coalition-building (labor unions, environmental movements, progressive economists) creates exit pathways. Sunset horizon: 10-20 years as climate/employment crises force mandate expansion.
constraint_indexing:constraint_classification(central_bank_independence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / TIME-CONSISTENCY VIEW (MOUNTAIN) — From universal/civilizational perspective, CBI represents an immutable natural law of monetary economics: any central bank without independence faces the Kydland-Prescott time-consistency problem — short-term incentives to inflate ex-post conflict with long-term price stability, making credible commitments impossible. Independence is necessary structural feature of monetary policy, not contingent institutional choice. Engine will detect this as false summit — time-consistency problem is real but policy solutions are contingent.
constraint_indexing:constraint_classification(central_bank_independence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(central_bank_independence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(central_bank_independence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(central_bank_independence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(central_bank_independence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(central_bank_independence, TR),
    TR >= 0.70.

:- end_tests(central_bank_independence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint transfers monetary policy authority from elected representatives to appointed technocrats, with effects that concentrate benefits on creditors (lower inflation risk) and costs on debtors (higher unemployment, lower wages). The extractiveness is not as severe as pure snare (0.70+) because some coordination benefit is genuine — price stability does enable long-term contracting and prevents runaway inflation. But the extraction is substantial because the beneficiaries are concentrated (creditor class, capital-intensive firms) while costs are dispersed (employment losses, wage suppression) yet individually catastrophic. The rising trajectory reflects increasing capital market enforcement as globalization reduces exit options for non-compliant jurisdictions. Suppression (0.48): Moderate. Multiple barriers prevent electoral reversal: constitutional embedding (difficult amendment), creditor market discipline (capital flight if independence is weakened), international coordination (IMF/World Bank requirements), and technocratic institutional culture. But suppression is not total — some jurisdictions have weakened CBI (Argentina, Venezuela) at cost of capital flight; European Central Bank has expanded its mandate under pressure; Federal Reserve has historically navigated dual mandates. Theater ratio (0.58): Moderate-high and rising. Democratic elections continue but exclude monetary policy choices. The electoral ritual persists despite substantive exclusion of core economic policy from voter control. Theater has increased as political pressure on central banks has grown (inflation debates, climate mandates) without corresponding electoral mechanisms to resolve them — the gap between electoral expectation and actual control has widened.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap is between the analytical observer's mountain (time-consistency as natural law) and the organized victim perspective (employment suppression as contingent extraction). The mountain classification appears in high theory — Kydland and Prescott proved that central banks with political pressure will over-inflate ex-post, so pre-commitment via independence is necessary. But this natural law operates at a level of abstraction (rational expectations, time-inconsistency problem) that leaves invisible the specific institutional forms of commitment. Other mechanisms could solve time-consistency: implicit inflation contracts, reputation effects, fiscal rules anchoring expectations, policy committees with explicit multi-objective mandates. That CBI became the dominant solution reflects not natural law but specific historical circumstances (Volcker inflation crisis, neoliberal consensus, globalized capital markets), political power (creditors > workers in policy influence), and institutional path dependence (Bundesbank model). The perspectival gap is analytically productive: the mountain view explains why some form of commitment mechanism is necessary; the snare view explains why the specific form chosen benefits creditors at expense of workers. Resolving this gap requires moving from timeless theory (natural law) to historical-institutional analysis (contingent choice among alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain operates as follows: (1) Identify beneficiary groups: creditor class, capital-intensive firms, wealth-holders benefit from price stability through asset protection and long-term contracting security. Derived d for beneficiaries: 0.10-0.25 (low targets). (2) Identify victim groups: employment-maximizing workers, debtor populations, fiscal authorities benefit from higher employment at cost of higher inflation. Derived d for victims: 0.80-0.95 (high targets). (3) Assess exit options: beneficiaries have high mobility (capital flight, currency arbitrage, international finance), so exit_options = arbitrage. Victims have low mobility (national currency, geography-dependent employment), so exit_options = trapped or constrained. (4) Apply sigmoid f(d): beneficiaries with d≈0.15 get f(d)≈-0.01 (negative extraction — constraint flows toward them). Victims with d≈0.90 get f(d)≈1.28 (high extraction against them). (5) Scale by scope: national scope σ=1.0, so no additional scaling. (6) Result: same constraint produces χ ≈ 0.00 for beneficiaries (rope experience) and χ ≈ 0.67 for victims (snare experience). The perspectival gap is mathematically encoded in the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   CBI resolves mandatrophy by disambiguating the referent: the question 'Is CBI coordination or extraction?' presupposes a single answer, but CBI IS both. For creditors and technocrats, it solves a real coordination problem (time-consistency) that enables multi-period contracting. For employment-maximizing populations, it is pure extraction of monetary policy authority. Both perspectives are structurally accurate — they describe genuine features of the same institutional arrangement. The mandatrophy dissolves when we recognize that CBI is tangled rope (genuine coordination function + asymmetric extraction of policy authority), not pure rope and not pure snare. The classification depends critically on the perspective (beneficiary vs victim), the time horizon (immediate coordination vs generational wealth distribution), and the exit options (arbitrage vs trapped). The analytical observer's mountain view is a false summit because it naturalizes what is contingent: the time-consistency problem is real, but CBI is one solution among several, and the selection of CBI reflects creditor power in policy-making, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    time_consistency_necessity,
    'Is central bank independence a necessary solution to the Kydland-Prescott time-consistency problem, or is it one contingent institutional solution among multiple possibilities?',
    'Comparative institutional analysis: identification of alternative time-consistency mechanisms (implicit inflation contracts, reputation effects, fiscal rules, policy committees, transparency requirements) and their effectiveness across different political-economic contexts',
    'If necessary: mountain classification confirmed; CBI is immutable law of monetary policy. If contingent: mountain is false summit; alternatives exist and different political contexts may employ different solutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_consistency_necessity, conceptual, 'Whether CBI is necessary or contingent solution to time-consistency').

omega_variable(
    democracy_compatibility,
    'Is removal of monetary policy from electoral accountability compatible with democratic legitimacy, or does it represent structural erosion of democratic control?',
    'Normative analysis: comparative democratic theory; empirical analysis of public support for CBI in different countries; outcome comparison of high-CBI vs low-CBI democracies on democratic health indicators',
    'If compatible: CBI is democratically legitimate delegation. If incompatible: CBI is democratically illegitimate extraction masked as technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democracy_compatibility, preference, 'Compatibility of CBI with democratic legitimacy').

omega_variable(
    dual_mandate_feasibility,
    'Can a central bank credibly maintain price stability while simultaneously targeting full employment or climate transition, or does multi-objective mandates collapse into political control and lose the time-consistency benefit?',
    'Historical precedent analysis: Federal Reserve pre-1980s dual mandate performance; comparative analysis of countries with explicit dual mandates (European Central Bank secondary mandate, Bank of England expanded mandate); simulation models of multi-objective policy under political pressure',
    'If feasible: scaffold sunset is real; dual-mandate reform can succeed without sacrificing credibility. If infeasible: current independence structure is load-bearing; dual-mandate movements face fundamental trade-offs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_mandate_feasibility, empirical, 'Whether dual-mandate central banking maintains price stability credibility').

omega_variable(
    global_capital_mobility_enforcement,
    'How much of CBI enforcement occurs through international capital market discipline vs domestic legal/institutional mechanisms? Would CBI persist if global capital mobility were constrained?',
    'Comparative institutional analysis: CBI strength in high-capital-mobility vs capital-controlled jurisdictions; historical cases of CBI abandonment (Argentina 2002, Venezuela, Turkey); simulation of CBI compliance under capital controls regime',
    'If capital markets dominant: CBI extractiveness increases with globalization; scaffold movements may underestimate cost. If domestic mechanisms dominant: CBI is more durable but theater-ratio may underestimate institutional coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_capital_mobility_enforcement, empirical, 'Relative enforcement by capital markets vs domestic institutions').

omega_variable(
    extractiveness_measurement_variability,
    'Does measured extractiveness depend on whether we evaluate CBI through inflation outcomes, employment outcomes, or distributional consequences? Are these measuring different constraints?',
    'Decomposition analysis: separate constraint stories for (a) price stability commitment, (b) employment suppression mechanism, (c) wealth redistribution channel. Compare epsilon values for each.',
    'If single constraint: current story is correct. If three constraints: ε-invariance principle requires decomposition into separate stories per measurement basis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_variability, conceptual, 'Whether extractiveness depends on measurement choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(central_bank_independence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbi_tr_t0, central_bank_independence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cbi_tr_t10, central_bank_independence, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cbi_tr_t20, central_bank_independence, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(cbi_be_t0, central_bank_independence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbi_be_t10, central_bank_independence, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cbi_be_t20, central_bank_independence, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(central_bank_independence, enforcement_mechanism).
narrative_ontology:affects_constraint(central_bank_independence, monetary_policy_transmission).
narrative_ontology:affects_constraint(central_bank_independence, fiscal_space_constraint).
narrative_ontology:affects_constraint(central_bank_independence, inflation_targeting_regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
