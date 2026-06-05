% ============================================================================
% CONSTRAINT STORY: monetary_union_exit_costs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_union_exit_costs, []).

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
 *   constraint_id: monetary_union_exit_costs
 *   human_readable: Monetary Union Exit Costs and Lock-In
 *   domain: economic/political/institutional
 *
 * SUMMARY:
 *   The monetary union constraint creates a structural tension between the
 *   coordination benefits of a shared currency and the loss of macroeconomic
 *   adjustment mechanisms for member states. Once a state joins a monetary
 *   union with sufficient financial integration and debt accumulation, exit
 *   costs rise nonlinearly — not because of immutable laws, but because
 *   institutional arrangements (debt denomination, financial interconnection,
 *   policy coordination mechanisms) make departure catastrophically
 *   expensive. The constraint exhibits different types from different
 *   perspectives: pure extraction (snare) for periphery states locked into
 *   uncompetitive positions; mixed coordination-extraction (tangled rope) for
 *   labor markets and intermediate economies; pure coordination (rope) for
 *   creditor states; institutional degradation (piton) as the reserve
 *   currency function atrophies; and ambitious reform pathways (scaffold) as
 *   deeper integration and fiscal coordination mechanisms emerge. The rising
 *   extractiveness trajectory (0.35 → 0.58) reflects that the lock-in deepens
 *   as debt accumulates and financial systems intertwine — exit costs grow
 *   over time, not because the constraint is immutable, but because
 *   contingent institutional choices compound. The theater ratio rise (0.28 →
 *   0.45) reflects that macro-policy coordination narratives increasingly
 *   substitute for actual flexibility as adjustment mechanisms harden.
 *
 * KEY AGENTS:
 *   - Periphery Member States: Primary victims (powerless/trapped) — economically vulnerable states locked into uncompetitive positions within the union; face catastrophic costs if attempting exit via debt redenomination and capital flight
 *   - Labor Markets: Secondary victims (moderate/constrained) — workers benefit from union labor mobility and stability but lose currency adjustment mechanisms; must absorb shocks via unemployment or wage pressure
 *   - Union Core States: Primary beneficiaries (institutional/arbitrage) — strong economies (Germany, Netherlands) experience low inflation lock-in and trade benefits; control ECB governance; can tighten policy to shift costs periphery-ward
 *   - Financial Institutions: Secondary beneficiaries (institutional/arbitrage) — creditors benefit from centralized currency; profit from periphery borrowing and subsequent austerity-driven bond appreciation
 *   - Eurozone Institutional Reform Coalition: Organized reformers (organized/mobile) — ECB, EU Commission, progressive actors building deeper fiscal integration and common bonds as exit-cost reduction mechanisms
 *   - Debt Markets: Systemic actor (institutional/arbitrage) — credit flows favor core states, punish periphery; redenomination risk is priced into borrowing costs, making exit formally rational but structurally impossible
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional lock-in as immutable law of monetary union dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_union_exit_costs, 0.58).
domain_priors:suppression_score(monetary_union_exit_costs, 0.68).
domain_priors:theater_ratio(monetary_union_exit_costs, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_union_exit_costs, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_union_exit_costs, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monetary_union_exit_costs, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_union_exit_costs, tangled_rope).
narrative_ontology:human_readable(monetary_union_exit_costs, "Monetary Union Exit Costs and Lock-In").
narrative_ontology:topic_domain(monetary_union_exit_costs, "economic/political/institutional").

domain_priors:requires_active_enforcement(monetary_union_exit_costs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_union_exit_costs, union_core_states).
narrative_ontology:constraint_beneficiary(monetary_union_exit_costs, financial_institutions).
narrative_ontology:constraint_beneficiary(monetary_union_exit_costs, debt_holders).
narrative_ontology:constraint_victim(monetary_union_exit_costs, periphery_states).
narrative_ontology:constraint_victim(monetary_union_exit_costs, labor_markets).
narrative_ontology:constraint_victim(monetary_union_exit_costs, fiscal_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PERIPHERY STATE (SNARE) — A smaller or economically vulnerable member state faces overwhelming exit costs if it exits the monetary union: debt denominated in the shared currency becomes unserviceable if redenominated, capital flight accelerates, banking system collapses. No material pathway to exit without catastrophic domestic costs. The union membership that was supposed to enable economic convergence has become an inescapable trap. Maximum extraction from powerless agent with no exit alternatives.
constraint_indexing:constraint_classification(monetary_union_exit_costs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONSTRAINED LABOR MARKET (TANGLED ROPE) — Workers benefit from the monetary union's stability, low interest rates, and access to broader labor markets (via migration within the union), but lose the ability to adjust via currency devaluation when demand collapses. Real wage adjustment must occur via unemployment or downward nominal wage pressure. High exit costs (migration costs, skill transfer barriers) make workers constrained rather than mobile. Genuine coordination benefits exist (access to union labor market) alongside asymmetric extraction (inability to adjust via currency depreciation).
constraint_indexing:constraint_classification(monetary_union_exit_costs, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNION CORE AND CREDITOR STATES (ROPE) — Strong core economies (Germany, Netherlands, etc.) and creditor nations experience the monetary union as pure coordination: a single currency reduces transaction costs, enables trade, and locks in monetary credibility that benefits them (low inflation expectations, low debt servicing costs). These states have arbitrage options (can tighten policy, dominate ECB governance) and experience net benefit. From their structural position, the constraint is coordination with no extraction toward them.
constraint_indexing:constraint_classification(monetary_union_exit_costs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INTERMEDIATE REGIONAL ECONOMIES (TANGLED ROPE) — Mid-tier economies (Spain, France, Italy) experience genuine coordination benefits (trade, capital access) but face real constraints on fiscal policy and nominal adjustment. They benefit from the monetary union's stability but cannot fully exit (debt costs, trade dependency, political integration) nor fully optimize within it (constrained by creditor-country policy preferences). Requires active enforcement of fiscal rules while coordination functions persist.
constraint_indexing:constraint_classification(monetary_union_exit_costs, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EUROZONE INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (ECB, EU Commission, reform advocates) see exit costs as a temporary institutional design problem with a sunset: deeper fiscal integration, common bonds, and expanded monetary policy flexibility are building alternative coordination pathways that reduce exit costs while maintaining union benefits. As these reforms mature, the trap mechanism weakens — states gain more flexibility without losing monetary stability. Low effective extraction because organized agents perceive and are actively building an exit ramp.
constraint_indexing:constraint_classification(monetary_union_exit_costs, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: GLOBAL RESERVE CURRENCY ILLUSION (PITON) — The euro's status as a major global reserve currency creates theater around union membership: states maintain euro participation partly because breaking with the world's second-largest reserve currency appears unthinkable. But the actual functional coordination (settling international transactions, denominating debt) has degraded since crypto-assets and alternative settlement mechanisms emerged. The union membership is increasingly performative — maintained through institutional inertia and political symbolism rather than genuine functionality. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(monetary_union_exit_costs, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IRREDUCIBLE LOCK-IN VIEW (MOUNTAIN) — From a civilizational perspective, once a monetary union reaches sufficient scale and integration, some exit costs become structurally irreducible: debt redenomination cascades, financial system interlocking, and path-dependent institutional dependencies create a coordination trap that cannot be unwound without systemic damage. The lock-in appears immutable — a natural law of monetary union dynamics. However, this perspective risks naturalizing contingent institutional choices (debt structure, financial integration pathways) as irreducible constraints.
constraint_indexing:constraint_classification(monetary_union_exit_costs, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_union_exit_costs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_union_exit_costs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_union_exit_costs, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_union_exit_costs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_union_exit_costs, TR),
    TR >= 0.70.

:- end_tests(monetary_union_exit_costs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from periphery states and labor markets via credible exit threats. The original research on euro exit costs estimates 5-15% GDP shock from redenomination, capital flight, and financial system disruption. However, extractiveness is not at snare maximum (0.70+) because: (1) creditor states do benefit from union stability, so some of the extraction is asymmetric rather than pure; (2) reform pathways (fiscal union, common bonds) are genuinely reducing exit costs over time; (3) actual exits (if they occur) might be less catastrophic than feared. Suppression (0.68): High. The barriers to exit include: debt redenomination cascades (any exiting state's debt becomes unserviceable if foreign creditors refuse to accept new-currency denomination); financial system interconnection (eurozone banks hold massive amounts of each others' sovereign debt); capital flight incentives (rational actors flee currencies expected to devalue); political pressure from core states; institutional lock-in (decades of policy coordination and regulatory harmonization). The suppression is not total (0.95) because some technical exit pathways exist (capital controls, debt restructuring with creditor coordination), but activating them requires policy courage and faces organized resistance. Theater ratio (0.45): Moderate-low. The constraint contains genuine coordination functions (trade facilitation, monetary credibility, financial market integration) that are not purely performative, but an increasing share of macro-policy coordination rhetoric is theater — central bank 'whatever it takes' statements and fiscal rule enforcement narratives substitute for actual policy flexibility. The rising theater reflects that members are increasingly constrained (inability to deliver promised flexibility) yet committed to performative narratives (reaffirming union solidarity, euro permanence).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position. A creditor state and core economy (institutional/arbitrage) genuinely sees rope — coordination benefits from a single currency, low inflation lock-in, trade efficiency gains. They have exit options (can tighten policy, threaten to leave) even if exit is ultimately undesirable. A periphery state (powerless/trapped) sees snare — debt becomes unserviceable, capital flees, financial system collapses if it tries to exit. They have no functional exit options despite union membership that was supposed to enable convergence. A labor market (moderate/constrained) sees tangled rope — benefits from union labor mobility and access, but cannot use currency adjustment to escape unemployment when demand collapses. The analytical observer risks seeing a natural law (mountain) — 'monetary unions have irreducible exit costs' — but the structural data reveals this as contingent institutional lock-in. Countries that have exited monetary arrangements (Argentina, Iceland, Czechoslovakia) recovered; the costs are severe but not immutable. The false summit occurs when institutional complexity is mistaken for physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across institutional actors. Periphery states in the powerless/trapped position experience d ≈ 0.90 — almost pure targets, with minimal benefit relative to extraction. Their beneficiary status (trade, capital access) is overwhelmed by victim status (loss of adjustment mechanism). Creditor institutional states experience d ≈ 0.15 — they are net beneficiaries with arbitrage options (can tighten policy, exit rhetoric is credible threat). Intermediate states and labor markets occupy d ≈ 0.55 — mixed positions with real coordination benefits (access to capital, trade) but real constraints (fiscal rules, unable to devalue). The engine applies the sigmoid f(d) to produce effective extractiveness chi. For the periphery (d ≈ 0.90), f(d) ≈ 1.28, scaling base_extractiveness by moderate multiple. For creditors (d ≈ 0.15), f(d) ≈ -0.01, producing negative effective extraction (they benefit). The large perspectival gap (snare to rope) reflects these directional divergences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from extraction mechanism. The coordination function is genuine: a single currency solves collective action problems for trade, finance, and price stability. But over time, without corresponding fiscal union mechanisms, the coordination function becomes asymmetric. Core states capture stability benefits; periphery states lose adjustment mechanisms without gaining fiscal transfers. The constraint is tangled rope, not pure snare or pure rope, because both functions persist simultaneously. The extraction (inability to devalue, vulnerability to austerity, lost fiscal autonomy) is real. The coordination (trade facilitation, capital access, monetary credibility) is real. The misclassification risk is treating the coordination function's legitimacy as proof that the constraint is entirely beneficial, or conversely, treating the extraction mechanism as proof that the coordination never existed. The accurate classification holds both: genuine coordination with asymmetric distribution that requires active enforcement of debt rules and policy coordination to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_redenomination_cascade,
    'How much of the measured exit cost derives from genuine financial system interconnection vs. coordinated creditor refusal to restructure debt in alternative currencies?',
    'Historical analysis of prior monetary union exits (Czechoslovakia, Yugoslavia, currency board collapses); counterfactual modeling of orderly redenomination with creditor coordination; empirical measurement of automatic cascade cascades vs. policy-driven decisions.',
    'If genuine interconnection dominates: exit costs are near-structural (snare classification sustained). If coordinated refusal dominates: exit costs are enforceable extraction (snare mechanism becomes piton when coordination breaks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_redenomination_cascade, empirical, 'Whether debt redenomination cascades are structural or policy-enforced').

omega_variable(
    fiscal_union_sufficiency,
    'Would transfer mechanisms or common fiscal backstops (pooled unemployment insurance, shared debt instruments) reduce exit costs enough to convert the snare into a rope?',
    'Comparative analysis of US federal fiscal transfers vs. eurozone; empirical testing of states'' revealed preferences for common fiscal mechanisms; scenario modeling of eurozone with and without transfer union architecture.',
    'If fiscal union sufficient: exit costs are contingent institutional design choices (scaffold sunset is real). If insufficient: exit costs persist even with fiscal pooling (architectural constraint deeper than fiscal coordination alone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_union_sufficiency, empirical, 'Whether fiscal union mechanisms can eliminate exit cost traps').

omega_variable(
    alternative_currency_viability,
    'Could a periphery state successfully exit and establish an alternative currency without complete financial system collapse? What are the genuine lower bounds on exit costs?',
    'Simulation modeling of exit scenarios with varying degrees of policy coordination and capital controls; historical case study of Argentina''s peso exit (2001-2002) and subsequent recovery trajectory; comparative analysis of Iceland''s 2008 currency collapse recovery vs. eurozone periphery trajectories.',
    'If exit costs have modest lower bounds: snare classification is correct but may degrade to tangled_rope under different policy frameworks. If exit costs are catastrophic even with policy support: snare trap is near-immutable within timeframe analyzed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_currency_viability, empirical, 'Lower bound on exit costs under optimal exit conditions').

omega_variable(
    monetary_policy_flexibility_substitution,
    'Can micro-level policy tools (labor market reforms, targeted industrial policy, internal devaluation) substitute sufficiently for macroeconomic adjustment via currency depreciation?',
    'Longitudinal comparison of adjustment trajectories: countries that remained in fixed exchange rate systems vs. those that could devalue; empirical measurement of adjustment speed and employment cost under each pathway; identification of specific thresholds where micro-policy substitution fails.',
    'If sufficient substitution: extraction mechanism is less severe (tangled_rope classification more accurate than snare). If substitution fails: lock-in via inability to adjust is structural (snare sustained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_policy_flexibility_substitution, empirical, 'Whether micro-policy tools can substitute for currency adjustment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_union_exit_costs, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(muec_tr_t0, monetary_union_exit_costs, theater_ratio, 0, 0.28).
narrative_ontology:measurement(muec_tr_t5, monetary_union_exit_costs, theater_ratio, 5, 0.35).
narrative_ontology:measurement(muec_tr_t10, monetary_union_exit_costs, theater_ratio, 10, 0.45).
narrative_ontology:measurement(muec_tr_t15, monetary_union_exit_costs, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(muec_be_t0, monetary_union_exit_costs, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(muec_be_t5, monetary_union_exit_costs, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(muec_be_t10, monetary_union_exit_costs, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(muec_be_t15, monetary_union_exit_costs, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_union_exit_costs, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_union_exit_costs, 0.12).
narrative_ontology:affects_constraint(monetary_union_exit_costs, austerity_mandate_enforcement).
narrative_ontology:affects_constraint(monetary_union_exit_costs, fiscal_transfer_resistance).
narrative_ontology:affects_constraint(monetary_union_exit_costs, sovereign_debt_cascade_risk).
narrative_ontology:affects_constraint(monetary_union_exit_costs, labor_market_wage_depression).

% DUAL FORMULATION NOTE:
% Monetary union exit costs are structurally linked to austerity mandates and debt cascade risks. The upstream constraint is the decision architecture (debt denomination, financial integration pathways) that creates exit costs; the downstream constraints (austerity enforcement, wage depression) are mechanisms through which the exit cost trap operates. All members of this constraint family should declare network links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_union_exit_costs, powerful, 0.45).
constraint_indexing:directionality_override(monetary_union_exit_costs, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
