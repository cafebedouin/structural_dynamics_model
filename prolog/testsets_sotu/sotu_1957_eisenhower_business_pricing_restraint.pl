% ============================================================================
% CONSTRAINT STORY: sotu_1957_eisenhower_business_pricing_restraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1957_eisenhower_business_pricing_restraint, []).

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
 *   constraint_id: sotu_1957_eisenhower_business_pricing_restraint
 *   human_readable: Eisenhower's Business Pricing Restraint Norm (1957)
 *   domain: economics/macroeconomic_policy
 *
 * SUMMARY:
 *   Eisenhower's 1957 State of the Union address articulates a normative
 *   constraint on business pricing behavior whereby firms should voluntarily
 *   avoid opportunistic price increases during supply-constrained periods,
 *   even where such increases are technically profitable and legally
 *   permissible. This constraint substitutes moral suasion, patriotic
 *   appeals, and reputational pressure for regulatory price controls,
 *   preserving the ideological commitment to free enterprise while achieving
 *   macroeconomic objectives (price stability, inflation containment) that
 *   might otherwise require direct government intervention. The constraint
 *   operates through a hybrid mechanism: genuine coordination benefit (stable
 *   pricing prevents hyperinflation that erodes consumer purchasing power and
 *   firm asset values) coupled with asymmetric extraction (firms forgo
 *   short-term profit maximization). Its structural role is transitional —
 *   addressing supply constraints expected to resolve as post-Korean War
 *   production normalized. The constraint's degradation trajectory (theater
 *   ratio rising from 0.35 to 0.68 over 8 years, extractiveness peak at year
 *   4 then slight decline as supply normalized) reflects both increasing
 *   performative content (firms' compliance becomes more theatrical as
 *   external pressure mounts) and the sunset logic of temporary shortage
 *   conditions.
 *
 * KEY AGENTS:
 *   - Consumers: Primary victims (powerless/trapped) — lack market alternatives during supply shortages; depend entirely on firm forbearance from opportunistic pricing
 *   - Profit-Maximizing Firms: Primary victim of restraint (powerful/mobile) — bear extraction cost through foregone profits during shortage window; face reputational and political pressure to comply
 *   - General Public: Secondary beneficiary (powerless/constrained) — benefits from price stability and inflation avoidance but lacks direct enforcement mechanism
 *   - Federal Government (Eisenhower Administration): Primary beneficiary (institutional/arbitrage) — achieves macroeconomic stability via voluntary mechanism rather than legal price controls; preserves free-enterprise narrative
 *   - Media and Corporate Reputation System: Enforcement apparatus (organized/constrained) — applies reputational pressure and surveillance; generates performative compliance through public commitment
 *   - Domestic Manufacturing Sector (Aggregated): Dual perspective (organized/constrained) — simultaneously faces coordination incentive (inflation avoidance protects asset values) and extraction cost (profit restraint)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1957_eisenhower_business_pricing_restraint, 0.38).
domain_priors:suppression_score(sotu_1957_eisenhower_business_pricing_restraint, 0.42).
domain_priors:theater_ratio(sotu_1957_eisenhower_business_pricing_restraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1957_eisenhower_business_pricing_restraint, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_business_pricing_restraint, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_business_pricing_restraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1957_eisenhower_business_pricing_restraint, tangled_rope).
narrative_ontology:human_readable(sotu_1957_eisenhower_business_pricing_restraint, "Eisenhower's Business Pricing Restraint Norm (1957)").
narrative_ontology:topic_domain(sotu_1957_eisenhower_business_pricing_restraint, "economics/macroeconomic_policy").

domain_priors:requires_active_enforcement(sotu_1957_eisenhower_business_pricing_restraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_business_pricing_restraint, consumers).
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_business_pricing_restraint, general_public).
narrative_ontology:constraint_victim(sotu_1957_eisenhower_business_pricing_restraint, profit_maximizing_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-CONSCIOUS CONSUMER (SNARE) — Trapped in the supply-constrained market with no exit option. Cannot substitute away from essential goods during shortages. Bears full cost of pricing decisions made by firms; has no mechanism to enforce restraint. Experiences the constraint as purely extractive from the firm's perspective (firms could raise prices but choose not to — or claim they do) but lacks enforcement mechanism when firms defect.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC MANUFACTURING SECTOR (TANGLED ROPE) — Faces both coordination benefit (stable pricing maintains consumer purchasing power and avoids hyperinflation that erodes asset values) and extraction cost (voluntary restraint forgoes short-term profit maximization during shortage conditions). Constrained by macroeconomic consequences of defection (if firms collectively raise prices, inflation accelerates, monetary policy tightens, demand collapses). Active enforcement via patriotic appeals, media pressure, political scrutiny.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — Benefits from voluntary pricing restraint as alternative to imposed price controls (preserves free-enterprise narrative while achieving inflation containment). Can arbitrage between regulatory and voluntary approaches. Experiences the constraint as pure coordination: achieving macroeconomic stability through moral suasion rather than legal mandate. Low extraction because government has multiple exit paths (regulation, direct price controls, purchasing leverage).
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE INDUSTRIAL CORPORATIONS (SCAFFOLD) — Powerful agents facing temporary supply constraints (Korean War recovery, post-war transition) see the restraint norm as time-limited. Sunset logic: as supply normalizes and competition returns, pricing power reverts to market forces automatically. The constraint requires active corporate restraint only during the shortage window. Mobile exit: firms can relocate production, diversify supplies, or wait out the shortage. Theater component: patriotic rhetoric and public commitment substitute for enforceable legal obligation, creating performative display of restraint.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MORAL SUASION INSTITUTIONAL APPARATUS (PITON) — The constraint's enforcement mechanism (patriotic appeals, corporate reputation management, media pressure, political rhetoric) is substantially performative. Once supply normalizes and shortage conditions end, the mechanism degrades to theater — firms no longer face reputational cost for price increases in competitive markets. The moral suasion apparatus persists through inertia even after its functional role expires. By the 1960s, this constraint had become largely ceremonial while firms resumed profit-maximizing pricing.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears to violate fundamental economic laws: prices coordinate supply and demand; restraining prices during shortages creates deadweight loss and perpetuates supply gaps. The analytical observer risks naturalizing the market-efficiency framing as immutable law. However, structural data contradicts this: the constraint benefits identifiable agents (consumers, government), is enforced through contingent institutional mechanisms (patriotic appeals), and has a historical sunset (disappeared as supply normalized). Engine detects false summit: the 'economic law' naturalizes a contingent moral and political choice.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1957_eisenhower_business_pricing_restraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_business_pricing_restraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1957_eisenhower_business_pricing_restraint, TR),
    TR >= 0.70.

:- end_tests(sotu_1957_eisenhower_business_pricing_restraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from firms by restricting profit-maximizing pricing during shortage conditions when demand elasticity is low. However, extraction is partial and conditional: (1) it applies only during supply-constrained periods, not permanently; (2) firms retain legal right to raise prices (enforcement is reputational, not legal); (3) firms benefit from the coordination outcome (inflation avoidance, demand preservation). The rising extractiveness trajectory (0.22 → 0.38) reflects increasing enforcement intensity as shortage persists and firms' forbearance becomes costly. The slight decline after year 4 reflects early normalization of supply and weakening reputational enforcement. Suppression (0.42): Moderate. Multiple suppression mechanisms operate: (1) reputational damage for defection (media scrutiny, corporate image harm); (2) political pressure (government procurement leverage for compliant firms); (3) collective action coordination (firms fear mutual defection spiral that would trigger price-control legislation); (4) cognitive framing (patriotic duty narrative reduces salience of foregone profits). Suppression is not total because firms retain legal exit (can raise prices and accept reputational cost) and can substitute (diversify supplies, reduce production, relocate). Theater ratio (0.58): Moderate-high and rising. Corporate compliance increasingly becomes performative over time: early compliance reflects genuine belief in patriotic duty and macroeconomic coordination; later compliance becomes ritualized (firms maintain price restraint publicly while implementing selective increases, quality degradation, or supply restrictions to capture foregone margin). The theater rise from 0.35 to 0.68 reflects the shift from functional restraint to ceremonial display as the constraint's macroeconomic necessity declines and supply normalizes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates one of the clearest perspectival gaps in the corpus: consumers experience snare (trapped, powerless, extraction without benefit), while government experiences rope (coordination benefit without extraction cost). The manufacturing sector occupies the middle ground (tangled rope) because firms both benefit from inflation avoidance (coordination) and suffer from profit restraint (extraction), with the balance shifting over time as supply normalizes. The large industrial corporation perspective (scaffold) captures the sunset logic: firms see the constraint as temporary, expecting automatic reversion to profit-maximizing pricing once supply constraints end. The piton perspective registers that the enforcement apparatus (patriotic appeals, media pressure, political rhetoric) becomes increasingly theatrical over time, persisting through institutional inertia after the shortage conditions have normalized. The mountain perspective risks naturalizing the constraint as economic law ('prices must restrain during shortages to prevent inflation') when the constraint is actually a contingent political choice (Eisenhower administration could have imposed price controls; instead chose moral suasion). The engine's false summit detector identifies this naturalization as premature: structural data shows identifiable beneficiaries (government, consumers) and losers (firms), active enforcement mechanisms (political pressure, reputational surveillance), and an identifiable sunset (supply normalization). None of these are features of natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional distribution of the constraint runs from powerless consumers (d ≈ 0.95, high extraction experienced) to institutional government (d ≈ 0.05, low extraction, high benefit). Intermediate organizational actors (firms, media) experience symmetric or near-symmetric d (d ≈ 0.45-0.55) because they face both coordination incentives and extraction costs. The beneficiary/victim declaration reflects this: consumers and general public are victims (they would pay higher prices without restraint but cannot enforce it); firms are victims of the restraint (they forgo profits) and beneficiaries of the coordination outcome (inflation avoidance protects asset values); government is the primary beneficiary (achieves macro stability without legal price controls). The sigmoid function f(d) amplifies this asymmetry: powerless consumers experience high f(d) ≈ 1.42 (maximum experienced extraction); government experiences f(d) ≈ -0.12 (institutional beneficiary, negative chi). The directionality derivation captures why the same structural constraint appears as snare from the consumer perspective (high d, high f(d)) and rope from the government perspective (low d, negative f(d)): the constraint's directionality is genuinely asymmetric, and the perspectives correctly register this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that hybrid classification (tangled_rope) correctly captures the constraint's dual nature: genuine coordination benefit (inflation avoidance) coupled with asymmetric extraction (firm profit restriction). The misclassifications and their diagnostic value: (1) SNARE UNDERESTIMATE: Classifying from consumer perspective alone underestimates the coordination function — consumers do benefit from stable prices and inflation avoidance, not just suffer extraction. The constraint is not pure snare because it solves a real collective action problem. (2) ROPE OVERESTIMATE: Classifying from government perspective alone overestimates purity — the government achieves its goals but firms bear asymmetric costs. The constraint is not pure rope because the cost allocation is not voluntary. (3) MOUNTAIN NATURALIZATION: Attempting to classify as natural economic law ('shortages naturally drive price increases; restraint is economically impossible') commits the false summit error — the constraint is enforceable and contingent on political choice, not inevitable. The tangled_rope classification correctly identifies that the constraint solves a coordination problem (inflation prevention) while imposing asymmetric costs (firm profit restriction), and that enforcement requires active institutional mechanisms (political pressure, media surveillance, patriotic rhetoric). The theater ratio rising over time signals the onset of Goodhart drift: as supply normalizes and the macroeconomic justification for restraint weakens, the constraint's enforcement becomes increasingly performative rather than functional, indicating degradation toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restraint_enforcement_mechanism,
    'What mechanism actually enforces voluntary pricing restraint when firms can profit from defection?',
    'Historical price data analysis comparing actual prices during shortage period to pre-shortage and post-shortage baselines; quantification of foregone profits; media analysis of corporate communications and reputation tracking',
    'If mechanism is primarily reputational: constraint is rope (coordination function is real). If mechanism is weak and firms defect secretly or gradually: constraint degrades to piton (theater dominates). If mechanism is explicit legal threat: constraint becomes snare or tangled rope with active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restraint_enforcement_mechanism, empirical, 'Actual enforcement mechanism for voluntary pricing restraint').

omega_variable(
    counterfactual_inflation_impact,
    'Would unconstrained pricing during shortages have produced materially higher inflation, or is the restraint effect negligible?',
    'Econometric analysis comparing inflation rates in sectors subject to moral suasion vs. sectors without such appeals; international comparison with countries lacking equivalent patriotic constraint; simulation models of shortage-driven pricing',
    'If restraint materially reduces inflation: tangible coordination benefit exists (tangled rope confirmed). If inflation effect is minimal: government benefits are rhetorical (constraint tilts toward snare — extraction without coordination)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_inflation_impact, empirical, 'Macroeconomic impact of pricing restraint on inflation').

omega_variable(
    firm_identity_lock_supply_shortage,
    'Do firms internalize the patriotic restraint norm as identity (corporate citizenship duty), or maintain it purely under external pressure?',
    'Corporate archival analysis of internal documents, board minutes, strategic planning; post-shortage behavior when external pressure was removed; longitudinal tracking of firms that maintained restraint vs. immediately resumed profit-maximizing pricing',
    'If identity-locked: constraint persists beyond supply shortage via internalized norms (identity_locked exit; classification shifts to rope at organizational level). If purely external pressure: constraint collapses immediately when shortage ends (piton confirmed; theater ratio rises to 0.85+)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_identity_lock_supply_shortage, empirical, 'Whether firms internalize patriotic restraint as corporate identity or maintain it under external pressure only').

omega_variable(
    distributional_incidence_restraint,
    'Who actually benefits from price restraint — broad consumer base or specific industrial/purchasing groups?',
    'Income-distribution analysis of actual consumers vs. constrained firms; identification of government procurement advantages (military-industrial complex); sectoral benefit analysis (defense contractors, allied suppliers vs. isolated consumers)',
    'If broad consumer benefit: constraint is genuine tangled rope (coordination with asymmetric cost allocation). If benefit concentrates in government/allied sectors: constraint becomes snare for broad consumers and rope for privileged beneficiaries (false coordination narrative)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_restraint, empirical, 'Distributional incidence of pricing restraint benefits and costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1957_eisenhower_business_pricing_restraint, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eisenhower_pricing_tr_t0, sotu_1957_eisenhower_business_pricing_restraint, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eisenhower_pricing_tr_t2, sotu_1957_eisenhower_business_pricing_restraint, theater_ratio, 2, 0.48).
narrative_ontology:measurement(eisenhower_pricing_tr_t4, sotu_1957_eisenhower_business_pricing_restraint, theater_ratio, 4, 0.58).
narrative_ontology:measurement(eisenhower_pricing_tr_t6, sotu_1957_eisenhower_business_pricing_restraint, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(eisenhower_pricing_be_t0, sotu_1957_eisenhower_business_pricing_restraint, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eisenhower_pricing_be_t2, sotu_1957_eisenhower_business_pricing_restraint, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(eisenhower_pricing_be_t4, sotu_1957_eisenhower_business_pricing_restraint, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(eisenhower_pricing_be_t6, sotu_1957_eisenhower_business_pricing_restraint, base_extractiveness, 6, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1957_eisenhower_business_pricing_restraint, resource_allocation).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_business_pricing_restraint, postwar_inflation_policy).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_business_pricing_restraint, business_confidence_macroeconomic_stability).

% DUAL FORMULATION NOTE:
% This constraint is upstream of broader postwar macroeconomic policy but represents a distinct mechanism: voluntary pricing restraint as substitute for price controls. The constraint's extraction (0.38) reflects the cost allocation between firms and consumers; its coordination function (suppression 0.42, theater 0.58) reflects the inflation-prevention mechanism. Decomposed from broader Eisenhower-era economic policy framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1957_eisenhower_business_pricing_restraint, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
