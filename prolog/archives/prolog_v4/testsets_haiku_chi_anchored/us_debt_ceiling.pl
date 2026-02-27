% ============================================================================
% CONSTRAINT STORY: us_debt_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_debt_ceiling, []).

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
 *   constraint_id: us_debt_ceiling
 *   human_readable: US Debt Ceiling
 *   domain: political/fiscal
 *
 * SUMMARY:
 *   The US debt ceiling is a legal authorization limit on federal borrowing,
 *   ostensibly enacted to provide fiscal oversight. Since its creation in
 *   1917, it has evolved from a routine procedural mechanism into a recurring
 *   instrument of political leverage. The constraint exhibits structural
 *   properties of both coordination (it forces periodic negotiation of fiscal
 *   priorities) and extraction (it concentrates coercive power in the hands
 *   of a legislative minority capable of threatening default). The asymmetry
 *   is severe: powerless actors (federal employees, benefit recipients, bond
 *   markets) bear the cost of brinkmanship while the negotiating coalitions
 *   reap political concessions. The theater ratio (0.65) reflects substantial
 *   performative content: much of each crisis is staged negotiation, with
 *   actual default probability lower than perceived. Over four decades, the
 *   ceiling has transitioned from a coordination mechanism with minimal
 *   extraction toward a hybrid tangled rope with increasing snare
 *   characteristics.
 *
 * KEY AGENTS:
 *   - Federal Employees and Benefit Recipients: Powerless/trapped — face wage delays and benefit interruption with no negotiating power; bear full extraction cost.
 *   - Bond Markets and Institutional Investors: Moderate/constrained — cannot exit Treasury exposure; extraction via uncertainty premium and potential default losses.
 *   - Congressional Majority Coalition: Organized/mobile — can raise ceiling but faces internal coalition costs; benefits from coordination mechanism.
 *   - Congressional Spending Restraint Coalition: Organized/mobile — extracts fiscal concessions (spending caps, benefit reforms) via leverage; benefits from ceiling as negotiation point.
 *   - Federal Reserve and Treasury Department: Institutional/arbitrage — has technical tools to navigate ceiling without default; perceives constraint as pure coordination.
 *   - Fiscal Restraint Reform Movement: Organized/mobile — sees ceiling as temporary scaffold for broader fiscal reform; believes structural alternatives can achieve coordination without extraction.
 *   - Constitutional Original Intent School: Institutional/arbitrage — observes that ceiling's original function (periodic fiscal review) has atrophied; classifies as piton (degraded institution).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_debt_ceiling, 0.58).
domain_priors:suppression_score(us_debt_ceiling, 0.68).
domain_priors:theater_ratio(us_debt_ceiling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_debt_ceiling, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_debt_ceiling, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_debt_ceiling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_debt_ceiling, tangled_rope).
narrative_ontology:human_readable(us_debt_ceiling, "US Debt Ceiling").
narrative_ontology:topic_domain(us_debt_ceiling, "political/fiscal").

domain_priors:requires_active_enforcement(us_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_debt_ceiling, congressional_hardliners).
narrative_ontology:constraint_beneficiary(us_debt_ceiling, deficit_hawks).
narrative_ontology:constraint_beneficiary(us_debt_ceiling, ideological_spending_opponents).
narrative_ontology:constraint_victim(us_debt_ceiling, fiscal_continuity).
narrative_ontology:constraint_victim(us_debt_ceiling, bond_markets).
narrative_ontology:constraint_victim(us_debt_ceiling, federal_workforce).
narrative_ontology:constraint_victim(us_debt_ceiling, benefit_recipients).
narrative_ontology:constraint_victim(us_debt_ceiling, economic_growth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEES & BENEFIT RECIPIENTS (SNARE) — No ability to exit. During debt ceiling crises, payment is uncertain; cannot negotiate individual exemptions. Bears full cost (delayed wages, benefit interruption). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOND MARKETS & INSTITUTIONAL INVESTORS (SNARE) — Constrained: cannot exit Treasury obligations without realizing losses. Default or delayed payment destroys collateral value. Faces recurring extraction during ceiling negotiations. d≈0.80, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL MAJORITY COALITION (TANGLED ROPE) — Can exit (raise ceiling), but costs include internal coalition collapse and primary challenges. Benefits from coordination on fiscal priorities, but extracts from powerless actors via ceiling brinkmanship. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SPENDING RESTRAINT COALITION (TANGLED ROPE) — Benefits from coordination mechanism (ceiling negotiation as leverage point for spending controls). Extracts concessions (spending caps, benefit reforms) from broader electorate and opposing coalition. Mobile exit if leverage fails. d≈0.45, f(d)≈0.60, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESERVE & TREASURY (ROPE) — Institutional actors with arbitrage options (delay spending, manage cash, coordinate with Congress). Experience constraint as pure coordination mechanism for budgetary communication. Can navigate ceiling without catastrophic failure through technical maneuvers. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(us_debt_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL ORIGINAL INTENT SCHOOL (PITON) — Theater ratio ≥ 0.70. The ceiling's original function (periodic review of federal borrowing) has atrophied; it persists through institutional inertia and political theater. Legal scholars note the constraint is vestigial relative to its stated purpose. d≈0.15, f(d)≈0.08, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(us_debt_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: FISCAL RESTRAINT REFORM MOVEMENT (SCAFFOLD) — Sees ceiling as temporary scaffolding for broader fiscal reform. If constitutional amendment or multi-year budget rules replace the ceiling, the constraint's extraction mechanism becomes obsolete. Has sunset logic: temporary hardship to incentivize permanent structural change. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(us_debt_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: FISCAL STRUCTURE ANALYST (TANGLED ROPE) — Pure analytical view. The ceiling is a genuine coordination mechanism (forces periodic negotiation of fiscal priorities) coupled with asymmetric extraction (leverages powerless actors for political concessions). Both functions are real. ε=0.58, suppression=0.68 confirm the hybrid. d≈0.70, f(d)≈1.10, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_debt_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_debt_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_debt_ceiling, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_debt_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_debt_ceiling, TR),
    TR >= 0.70.

:- end_tests(us_debt_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint enables extraction through default threat — minority coalitions can force concessions from the broader electorate and federal constituencies. Historical trend shows extractiveness increasing from 0.25 (1980s, routine raises) to 0.58 (2020s, weaponized brinksmanship). The 2011, 2013, and 2023 crises all yielded fiscal concessions from non-negotiating constituencies. Suppression (0.68): High. Multiple barriers prevent escape: Congress cannot simply vote without the ceiling (statute requires authorization); Treasury has limited discretionary tools; Federal Reserve cannot unilaterally raise ceiling. Default threat suppresses alternatives (emergency spending, bond issuance). Theater ratio (0.65): Moderate-high. Each ceiling crisis involves substantial performative negotiation: public grandstanding, deadline extensions, last-minute deals announced at 11:59 PM. Yet the underlying threat is real (default cascade is possible, though low-probability). The rise from 0.35 (1980s) to 0.65 (2020s) reflects increased theatrical performance as partisan polarization made the mechanism more contentious.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. Federal employees see a pure snare (trapped, no exit, bear full cost). Bond markets see snare-to-tangled-rope (constrained exit, significant extraction). Congressional majorities see tangled rope (mix of coordination and extraction). Spending restraint coalitions see rope-to-scaffold (genuine fiscal leverage enabling policy change). Treasury/Fed see rope (coordination mechanism). Constitutional scholars see piton (degraded institution, maintained by inertia). Fiscal reform advocates see scaffold (temporary hardship incentivizing structural change). The analytical observer sees tangled rope (both coordination and asymmetric extraction are structurally real). The gap between powerless actors and organized coalitions is maximal: the former experience pure harm, the latter experience net benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal employees/benefit recipients: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Bond markets: Victim + constrained → d≈0.80, f(d)≈1.18. High extraction. Congressional majority: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Symmetric (some benefit, some cost from coalition fragility). Spending restraint coalition: Beneficiary + mobile → d≈0.45, f(d)≈0.60. Net beneficiary (extracts fiscal concessions). Federal Reserve/Treasury: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary (can navigate ceiling, coordinate policy). Fiscal reform movement: Ambiguous + mobile → d≈0.35, f(d)≈0.30. Sees constraint as temporary harness for change. Constitutional scholars: Institutional + arbitrage → d≈0.15, f(d)≈0.08. Piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The debt ceiling exemplifies the mandatrophy trap for coordination mechanisms. Initially (1917), it was intended as pure coordination — periodic congressional review of borrowing. Over time, it became weaponized: minority coalitions learned to use the ceiling as a leverage point for unrelated concessions (spending caps, benefit reforms, ideological demands). The constraint's evolution from rope → tangled rope → snare (depending on perspective) reveals the mechanism: once a coordination tool is in place, actors with higher risk tolerance can exploit it for extraction. The mandatrophy is resolved by acknowledging that the ceiling contains BOTH genuine coordination (forcing fiscal negotiation) AND genuine extraction (enabling minorities to harm powerless constituencies). The classification as tangled rope captures this hybrid nature. The rising theater ratio (0.35 → 0.65) reflects Goodhart drift: as the mechanism was politicized, its performative content increased, suggesting the functional coordination benefit is declining while the extraction mechanism persists. If theater continues rising above 0.75, the constraint will degrade toward piton (maintained by institutional inertia, functional content atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ratio,
    'What fraction of the debt ceiling''s political utility comes from genuine fiscal coordination versus extracting concessions via default threat?',
    'Historical analysis of ceiling negotiations: compare fiscal outcomes when ceiling was raised routinely vs when it was weaponized; measure policy concessions achieved vs fiscal transparency/oversight gained.',
    'If coordination > 60%: Rope classification from more perspectives. If extraction > 60%: Snare classification from more perspectives. Current assumption is ~50/50 (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ratio, empirical, 'Relative contribution of coordination vs extraction to ceiling''s political function').

omega_variable(
    default_cascade_probability,
    'What is the true probability of catastrophic default cascading from a missed ceiling deadline, given Treasury cash management and Federal Reserve tools?',
    'Stress-test simulation; analysis of prior near-misses (2011, 2013, 2023) to estimate actual vs perceived risk; review of Treasury contingency protocols.',
    'If true probability < 5%: suppression is inflated (theater > real coercive power). Snare classifications weaken. If true probability > 20%: suppression is validated; snare classifications strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_cascade_probability, empirical, 'True catastrophic default risk given Treasury tools and Federal Reserve capacity').

omega_variable(
    alternative_fiscal_mechanism_viability,
    'Would a shift to multi-year budget approval or constitutional fiscal rules replace the ceiling''s coordination function without replicating its extraction mechanism?',
    'Comparative institutional analysis: countries with constitutional budget rules (Germany, Switzerland); long-term budget frameworks (EU); assess whether these achieve fiscal transparency without debt ceiling brinkmanship.',
    'If viable alternatives exist: scaffold sunset logic is real; constraint has structural exit path. If no viable alternatives: constraint is entrenched; scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fiscal_mechanism_viability, conceptual, 'Whether fiscal alternatives can replace debt ceiling''s coordination without its extraction').

omega_variable(
    partisan_weaponization_threshold,
    'At what level of partisan polarization does the ceiling transition from coordination mechanism to pure extraction tool?',
    'Quantitative measure of partisan gap on fiscal policy over time; correlation with frequency and severity of ceiling crises; historical turning points (1995, 2011, 2013, 2023).',
    'If weaponization threshold already crossed: current classification underestimates snare component. If threshold is forward-looking: tangled rope classification holds but may degrade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_weaponization_threshold, empirical, 'Partisan polarization level triggering shift from coordination to extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_debt_ceiling, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usdc_theater_1980s, us_debt_ceiling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(usdc_theater_2000s, us_debt_ceiling, theater_ratio, 5, 0.5).
narrative_ontology:measurement(usdc_theater_2020s, us_debt_ceiling, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(usdc_extractiveness_1980s, us_debt_ceiling, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(usdc_extractiveness_2000s, us_debt_ceiling, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(usdc_extractiveness_2020s, us_debt_ceiling, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_debt_ceiling, enforcement_mechanism).
narrative_ontology:affects_constraint(us_debt_ceiling, federal_fiscal_discretion).
narrative_ontology:affects_constraint(us_debt_ceiling, bond_market_stability).
narrative_ontology:affects_constraint(us_debt_ceiling, monetary_policy_independence).

% DUAL FORMULATION NOTE:
% The debt ceiling is often conflated with broader fiscal policy (spending, taxation, deficits). These are structurally distinct constraints. The ceiling is specifically a borrowing authorization limit; it affects fiscal outcomes but is not identical to fiscal policy itself. This story focuses on the ceiling's structural properties (coordination + extraction). Upstream constraints include specific fiscal demands (spending caps, benefit reforms) that are leveraged via ceiling negotiations but exist independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_debt_ceiling, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
