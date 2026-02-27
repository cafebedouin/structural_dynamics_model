% ============================================================================
% CONSTRAINT STORY: endowment_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endowment_effect, []).

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
 *   constraint_id: endowment_effect
 *   human_readable: The Endowment Effect
 *   domain: economic/cognitive
 *
 * SUMMARY:
 *   The endowment effect is a robust finding in behavioral economics:
 *   individuals ascribe more value to objects merely because they own them,
 *   exhibiting higher willingness-to-accept (WTA) prices for owned goods than
 *   willingness-to-pay (WTP) prices for acquiring equivalent unowned goods.
 *   This constraint exhibits all six DR types from different perspectives,
 *   revealing a fundamental tension between loss-aversion cognition
 *   (apparently universal and immutable) and market efficiency
 *   (institutionally contingent). From the perspective of a rational
 *   individual trader, endowment effect is a snare: it extracts from the
 *   trader's own capacity for efficient exchange. From the perspective of a
 *   market participant or financial intermediary, it is tangled rope: it
 *   serves a coordination function (psychological attachment enables
 *   long-term holding and reduces panic selling) while simultaneously
 *   creating extraction through bilateral inefficiency (both buyer and seller
 *   experience bias, preventing mutually beneficial trades). From the
 *   perspective of behavioral economists, it is tangled rope with
 *   institutional enforcement: the field benefits from the effect as a
 *   robust, replicable finding that distinguishes behavioral from classical
 *   economics, but this creates enforcement barriers to alternative
 *   hypotheses. From the perspective of classical economics, it is a piton: a
 *   degraded institutional framework where rationality assumptions persist
 *   despite contradictory evidence, maintained through departmental
 *   separation and mutual non-falsification rather than research progress.
 *   From the analytical civilizational perspective, it risks appearing as a
 *   mountain — an immutable law of cognition — but the structural data
 *   reveals this as a false summit: the effect's magnitude varies
 *   dramatically with framing, market design, and ownership duration,
 *   indicating institutional rather than architectural determination.
 *
 * KEY AGENTS:
 *   - Individual Trader: Primary victim (powerless/trapped) — trapped by own loss aversion and reference dependence; systematically overpays and undersells
 *   - Market Participant: Secondary victim (moderate/constrained) — experiences both coordination benefits (psychological attachment to holdings) and extraction (bilateral inefficiency prevents mutually beneficial trades)
 *   - Financial Intermediary: Primary beneficiary (institutional/arbitrage) — benefits from reduced portfolio turnover (lower costs) and long-term holding (stable returns); designs products around endowment effect psychology
 *   - Behavioral Economics Community: Organized beneficiary (organized/constrained) — benefits from endowment effect as publishable, fundable research; enforces field boundaries through citation patterns
 *   - Classical Economics Establishment: Institutional actor (institutional/arbitrage) — maintains rationality assumptions as foundational doctrine despite endowment effect evidence; sustains piton through departmental separation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent psychological bias as immutable law of cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endowment_effect, 0.38).
domain_priors:suppression_score(endowment_effect, 0.42).
domain_priors:theater_ratio(endowment_effect, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endowment_effect, extractiveness, 0.38).
narrative_ontology:constraint_metric(endowment_effect, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(endowment_effect, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endowment_effect, tangled_rope).
narrative_ontology:human_readable(endowment_effect, "The Endowment Effect").
narrative_ontology:topic_domain(endowment_effect, "economic/cognitive").

domain_priors:requires_active_enforcement(endowment_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endowment_effect, incumbent_asset_holders).
narrative_ontology:constraint_beneficiary(endowment_effect, loss_aversion_exploitation).
narrative_ontology:constraint_victim(endowment_effect, price_discovery_mechanism).
narrative_ontology:constraint_victim(endowment_effect, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL TRADER (SNARE) — Individual trapped by own cognitive bias. Cannot exit loss-aversion response or overcome ownership attachment. Systematically overpays to acquire and undersells to divest. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.51. Pure extraction: the endowment effect extracts from the trader's own rationality.
constraint_indexing:constraint_classification(endowment_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARKET PARTICIPANT (TANGLED ROPE) — Benefits from psychological attachment enabling long-term holding (reduces panic selling during downturns; coordinates portfolio stability). But extraction occurs through bilateral inefficiency — both buyer and seller experience endowment bias, creating deadweight loss. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.37. Mixed: genuine coordination function (stability) married to extraction mechanism (bilateral inefficiency).
constraint_indexing:constraint_classification(endowment_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARY (ROPE) — Benefits from endowment effect as coordination mechanism. Psychological attachment to assets enables passive indexing strategy (buy-and-hold reduces transaction costs). Lower portfolio turnover = lower fees, lower market impact, more stable long-term returns. The constraint solves a collective action problem: preventing panic selling during volatility. d≈0.15, f(d)≈0.05, σ=0.9 → χ≈0.02. Net beneficiary; sees coordination value.
constraint_indexing:constraint_classification(endowment_effect, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BEHAVIORAL ECONOMICS RESEARCH COMMUNITY (TANGLED ROPE) — Organized agents benefit from endowment effect as robust, replicable finding that distinguishes behavioral from classical economics. Drives research funding, publication, careers. But extraction occurs through institutional enforcement: once endowment effect is 'established truth,' alternative hypotheses (rational attachment, information asymmetry) face barriers to publication. Requires active enforcement through citation patterns and reputation mechanisms. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.13. Low-to-moderate extraction; community has agency but faces coordination lock-in.
constraint_indexing:constraint_classification(endowment_effect, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CLASSICAL ECONOMICS ESTABLISHMENT (PITON) — Institutional inertia. Classical models assume rational preferences (WTA=WTP; endowment irrelevant). The endowment effect contradicts canonical theory but has not driven paradigm replacement. Instead, classical economics maintains rationality assumptions while behavioral economics exists as a separate subdiscipline. theater_ratio=0.55 reflects this split: classical models perform their function (general equilibrium, welfare analysis) with reduced functionality (omit behavioral detail). Maintained through departmental separation and mutual non-falsification.
constraint_indexing:constraint_classification(endowment_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some degree of loss aversion and reference dependence may be inherent to valuation cognition: all value is relative to a reference point, and losses loom larger than equivalent gains. If true, endowment effect would be immutable — an architectural feature of preference formation, not a contingent bias. However, structural data (ε=0.38, suppression=0.42) contradicts a mountain classification. The effect has behavioral correlates (affects pricing, trade volume) that institutions can partially overcome (auctions, liquid markets, reminders of sunk-cost fallacy). The engine will mark this as a false summit.
constraint_indexing:constraint_classification(endowment_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endowment_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(endowment_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(endowment_effect, TR),
    TR >= 0.70.

:- end_tests(endowment_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The endowment effect creates deadweight loss through bilateral inefficiency — both buyer and seller experience valuation gaps relative to classical predictions, preventing mutually beneficial trades. However, the extraction is not severe (ε < 0.46) because the effect is modest in magnitude (~20-30% of price) and institutional mechanisms (auctions, liquid markets) can substantially mitigate it. The value reflects that the bias is real and systematic but not insurmountable. Suppression (0.42): Moderate. Barriers to overcoming endowment effect include cognitive automaticity (loss aversion is pre-reflective), institutional lock-in (markets are structured around expected endowment bias), and equilibrium effects (if most traders exhibit bias, rational traders cannot fully arbitrage it away). But suppression is not total: repeated trading, sophisticated market institutions, and explicit training on sunk-cost fallacy can reduce endowment effects. Theater ratio (0.55): Moderate. Classical economics continues to teach WTA=WTP and preference rationality despite decades of endowment effect evidence. The persistence of classical models in textbooks and policy analysis despite contradictory evidence suggests performative rather than functional status. However, the theater is not extreme (0.55 vs piton minimum 0.70) because classical models do perform some functions: they provide tractable frameworks for general equilibrium and welfare analysis, even if behavioral detail is omitted. The theater has increased over the interval (0.35→0.55) as the empirical conflict has become undeniable yet institutionally unresolved.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full DR spectrum. The rational trader experiences pure extraction (snare): endowment bias directly undermines their capacity to trade efficiently. The market participant experiences mixed coordination-extraction (tangled rope): psychological attachment serves a genuine function (portfolio stability) but creates inefficiency (prevents beneficial trades). The financial intermediary experiences coordination (rope): they benefit from reduced turnover and can design products that exploit endowment psychology. The behavioral economics community experiences mixed enforcement-benefit (tangled rope): the field benefits from a publishable finding but enforces institutional boundaries that suppress alternative explanations. Classical economics experiences institutional degradation (piton): rationality assumptions persist despite evidence, maintained through separation rather than synthesis. The analytical observer risks a false summit: naturalizing loss aversion as an immutable architectural feature when institutional evidence shows it is modifiable. The perspectival gap reveals that endowment effect is not a natural law but a social-institutional phenomenon: its magnitude, scope, and effects depend critically on market design, framing, and institutional context.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual trader: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction: trader cannot exit loss-aversion cognition or correct the bias in real time. Financial intermediary: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Minimal extraction: intermediary can exploit endowment psychology and structure products accordingly. Market participant: Mixed victim/beneficiary + constrained → d≈0.70, f(d)≈1.08. Moderate extraction: participant experiences both benefits (stability) and costs (inefficiency), with constrained exit options. Behavioral economists: Mixed beneficiary/enforcer + constrained → d≈0.35, f(d)≈0.35. Low-to-moderate extraction: field benefits from publishable effect but faces coordination lock-in and enforcement pressure. Classical economists: Beneficiary (through institutional persistence) + arbitrage → d≈0.10, f(d)≈0.02. Minimal extracted extraction despite piton classification (piton comes from theater gate, not chi). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification risk: observer may naturalize contingent bias as universal law.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint's mandatrophy is resolved through perspectival decomposition. The claim 'endowment effect is a universal law of cognition' (mountain) and the claim 'endowment effect is an institutional extraction mechanism' (snare/tangled rope) are both partially true but frame different structural levels. At the individual cognitive level, loss aversion and reference dependence are architectural features of valuation cognition — these support the mountain classification. At the market-institutional level, endowment effect is contingent on market design, framing, and repeated-trading opportunity — these support snare/tangled rope classification. The resolver: Kahneman & Tversky's discovery that endowment effect magnitude is highly plastic to framing procedures (selling-as-loss vs buying-as-gain framing changes the effect by 50%+), and that liquid markets substantially eliminate endowment effects despite individual-level loss aversion persisting. This shows the effect is not architectural but institutional. The piton perspective resolves the mandatrophy at the meta-level: classical economics maintains rationality doctrine while behavioral economics documents violations; neither integrates the other. The institutional separation sustains the ambiguity (piton = theatrical persistence of conflicting paradigms) rather than forcing resolution toward a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reference_point_determination,
    'Is the endowment effect driven by loss aversion relative to current ownership (contingent psychological bias) or by reference-point construction inherent to valuation cognition (irreducible architectural feature)?',
    'Neuroscience imaging of valuation circuits; cross-cultural studies of endowment effect magnitude; tests of reference-point plasticity across different ownership framing procedures',
    'If contingent bias: endowment effect is Snare/Tangled Rope across most perspectives; institutional design can mitigate it. If architectural: endowment effect approaches Mountain status; market institutions must accommodate rather than eliminate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reference_point_determination, empirical, 'Whether endowment effect is contingent psychological bias or architectural valuation feature').

omega_variable(
    market_efficiency_decomposition,
    'How much of observed price dispersion (bid-ask spreads, volume asymmetries) is attributable to endowment effect versus information asymmetry, transaction costs, or strategic behavior?',
    'Structural estimation of valuation models; comparison of endowment effect magnitude across market types (thick markets vs thin markets, information-rich vs information-poor); instrumentation of ownership duration and acquisition context',
    'If endowment effect is primary driver: extraction is substantial (χ increases). If secondary to transaction costs: extraction is moderate and potentially addressed through market design. If confounded with information asymmetry: extraction may be mislabeled (should be separate constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_decomposition, empirical, 'Decomposition of price dispersion among endowment effect, information asymmetry, and transaction costs').

omega_variable(
    institutional_overcoming_mechanisms,
    'What institutional designs (auction mechanisms, liquid markets, repeated trading, algorithmic valuation) most effectively mitigate endowment effect, and at what cost in transaction complexity or information disclosure?',
    'Comparative study of endowment effect magnitude across market institutions; correlation between market design sophistication and price efficiency; measurement of compliance costs',
    'If effective low-cost mechanisms exist: scaffold perspective is realistic (sunset pathway via market institutional design). If mechanisms are unavailable or costly: tangled rope and snare perspectives dominate; endowment effect persists as structural extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_overcoming_mechanisms, empirical, 'Effectiveness and costs of institutional mechanisms to overcome endowment effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endowment_effect, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endow_tr_t0, endowment_effect, theater_ratio, 0, 0.35).
narrative_ontology:measurement(endow_tr_t50, endowment_effect, theater_ratio, 50, 0.48).
narrative_ontology:measurement(endow_tr_t100, endowment_effect, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(endow_be_t0, endowment_effect, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(endow_be_t50, endowment_effect, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(endow_be_t100, endowment_effect, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endowment_effect, resource_allocation).
narrative_ontology:affects_constraint(endowment_effect, loss_aversion_bias).
narrative_ontology:affects_constraint(endowment_effect, sunk_cost_fallacy).
narrative_ontology:affects_constraint(endowment_effect, status_quo_bias).

% DUAL FORMULATION NOTE:
% Endowment effect overlaps with but is structurally distinct from loss aversion bias (ε=0.15, Mountain), sunk-cost fallacy (ε=0.35, Snare), and status quo bias (ε=0.42, Tangled Rope). The endowment effect is downstream of loss aversion (loss aversion is necessary but not sufficient) and upstream of status quo bias (endowment effect explains why people maintain ownership). Each constraint has different ε reflecting different institutional modifiability: loss aversion is architectural; endowment effect is behavioral-plastic; sunk-cost and status-quo biases are institutional-contingent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(endowment_effect, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
