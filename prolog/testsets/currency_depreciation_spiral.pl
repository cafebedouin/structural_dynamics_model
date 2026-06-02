% ============================================================================
% CONSTRAINT STORY: currency_depreciation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_depreciation_spiral, []).

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
 *   constraint_id: currency_depreciation_spiral
 *   human_readable: Currency Depreciation Spiral in Emerging Market Economies
 *   domain: economic/monetary/international
 *
 * SUMMARY:
 *   A currency depreciation spiral occurs when initial exchange rate
 *   depreciation triggers a self-reinforcing cycle: (1) depreciation raises
 *   import prices → (2) inflation accelerates → (3) expectations of further
 *   depreciation form → (4) capital flight accelerates → (5) further
 *   depreciation occurs → loop repeats. This constraint is neither purely
 *   extractive nor purely coordinative. It has genuine coordination
 *   components: exchange rate signals allocate resources between tradable and
 *   non-tradable sectors, price signals guide investment, and currency
 *   movements can correct trade imbalances. But the cycle creates asymmetric
 *   extraction: domestic wage earners experience collapsing purchasing power
 *   while foreign creditors benefit from debt denominated in appreciating
 *   foreign currency, and export-oriented firms benefit from lower
 *   local-currency costs. The spiral's self-reinforcing nature comes from the
 *   speed of capital flows (fast) relative to wage adjustment (slow) and the
 *   structure of debt (often foreign-currency-denominated). The constraint
 *   traps countries in a state where policy intervention faces a credibility
 *   trap: stabilization requires painful domestic adjustment, which erodes
 *   political support, which makes capital markets doubt the commitment,
 *   which accelerates capital flight, which intensifies depreciation, which
 *   requires more adjustment. The measurements show extractiveness rising
 *   from 0.32 to 0.72 over 24 months as the spiral accelerates, and
 *   suppression rising from 0.35 to 0.78 as capital controls, wage controls,
 *   and import restrictions intensify to arrest the cycle.
 *
 * KEY AGENTS:
 *   - Domestic Wage Earners: Primary victims (powerless/trapped) — real purchasing power collapses; wages sticky downward; no exit options
 *   - Import-Dependent Small Firms: Secondary victims (moderate/constrained) — input costs rise; limited financing; cannot raise prices competitively
 *   - Local Savers: Victims (moderate/constrained) — purchasing power erodes; cannot easily convert to foreign currency; savings evaporate in real terms
 *   - Foreign Creditors: Primary beneficiary (institutional/arbitrage) — debt denominated in foreign currency becomes easier to service; can hedge or exit
 *   - Export-Oriented Firms: Secondary beneficiary (institutional/arbitrage) — depreciation reduces local costs; improves competitiveness; can arbitrage global supply chains
 *   - Multinational Capital: Beneficiary (institutional/arbitrage) — can time entries/exits; invest in collapsing assets; extract value through financial arbitrage
 *   - Central Bank: Organized actor (organized/constrained) — attempts to stabilize through intervention; limited by reserve capacity; faces credibility trap
 *   - IMF/International Lenders: Organized actor (organized/arbitrage) — enforce stabilization programs; condition lending on austerity; benefit from policy control leverage
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mechanical self-reinforcement; genuine coordination and extraction mechanisms intertwined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_depreciation_spiral, 0.58).
domain_priors:suppression_score(currency_depreciation_spiral, 0.65).
domain_priors:theater_ratio(currency_depreciation_spiral, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_depreciation_spiral, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_depreciation_spiral, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(currency_depreciation_spiral, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_depreciation_spiral, tangled_rope).
narrative_ontology:human_readable(currency_depreciation_spiral, "Currency Depreciation Spiral in Emerging Market Economies").
narrative_ontology:topic_domain(currency_depreciation_spiral, "economic/monetary/international").

domain_priors:requires_active_enforcement(currency_depreciation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, foreign_creditors).
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, export_oriented_firms).
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, multinational_capital).
narrative_ontology:constraint_victim(currency_depreciation_spiral, domestic_wage_earners).
narrative_ontology:constraint_victim(currency_depreciation_spiral, import_dependent_sectors).
narrative_ontology:constraint_victim(currency_depreciation_spiral, local_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC WAGE EARNER (SNARE) — Trapped in collapsing purchasing power. Wages are sticky downward; import prices rise immediately; real purchasing power deteriorates month-over-month. No exit option: cannot relocate without capital, cannot arbitrage into foreign currency without income, cannot change sector quickly. Experiences maximum extraction as the cycle self-reinforces.
constraint_indexing:constraint_classification(currency_depreciation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPORT-DEPENDENT SMALL FIRM (TANGLED ROPE) — Coordinates with suppliers through currency hedging and supply-chain adjustment, but faces significant extraction through rising input costs. Can theoretically relocate supply chains or raise prices, but faces market constraints and competitive pressure. Suppression is high (limited financing, currency controls) but not total. Experiences mixed coordination benefit (access to imports) and asymmetric extraction (cost inflation).
constraint_indexing:constraint_classification(currency_depreciation_spiral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOREIGN CREDITOR (ROPE) — Receives debt repayment in depreciated currency but had lent in prior period at earlier exchange rates. Net benefit from the spiral: debt obligations shrink in real terms (in foreign currency), while the country's collapsing import capacity forces prioritization of debt service. Arbitrage options are abundant: currency hedging, portfolio reallocation, exit into other emerging markets. Experiences the constraint as favorable coordination.
constraint_indexing:constraint_classification(currency_depreciation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXPORT-ORIENTED FIRM (ROPE) — Depreciation reduces local labor and input costs in foreign currency terms, improving profit margins on exports. Can arbitrage global supply chains. Experiences the constraint as pure coordination: depreciation aligns their interests with the global market. No suppression experienced; the cycle benefits them directly.
constraint_indexing:constraint_classification(currency_depreciation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STABILIZATION INTERVENTION COALITION (SCAFFOLD) — Central banks, IMF programs, currency pegs, and capital controls represent organized attempts to arrest the depreciation spiral through temporary constraints with intended sunset. Intervention creates artificial barriers (lower theater) to further depreciation. Success rate varies: some stabilizations hold (Czech koruna), others delay and amplify collapse (Argentine peso). The coalition sees the spiral as a temporary coordination failure solvable through policy coordination and external support.
constraint_indexing:constraint_classification(currency_depreciation_spiral, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PURCHASING POWER PARITY DOCTRINE (PITON) — The theoretical framing that currency depreciation is self-correcting — depreciation makes exports cheaper, raising demand, raising prices, until equilibrium restores. This theory persists in policy discourse despite decades of evidence that the cycle is unstable at the timescale of domestic life (wages do not adjust; expectations are not rational; capital flight dominates). Theater ratio is moderate (0.48) because the stabilization math is real but its functional relevance is degraded by behavioral dynamics and capital market structure. PPP doctrine maintains legitimacy through repetition despite repeated falsification.
constraint_indexing:constraint_classification(currency_depreciation_spiral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, currency depreciation spirals are self-reinforcing mechanical processes with genuine coordination functions (price signals, capital allocation) embedded within extraction mechanisms (wealth transfer from wage earners to asset holders, debt relief for borrowers in foreign currency). The spiral is neither purely extractive nor purely coordinative — the mechanics of the cycle require both functions to persist. The observer sees high suppression (capital controls, wage stickiness, asymmetric information) enabling extraction, and genuine coordination benefits (price signals) masked by extraction distribution.
constraint_indexing:constraint_classification(currency_depreciation_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_depreciation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_depreciation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_depreciation_spiral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_depreciation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_depreciation_spiral, TR),
    TR >= 0.70.

:- end_tests(currency_depreciation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The spiral extracts real purchasing power from wage earners and savers while transferring wealth to foreign creditors and exporters. The measurement reflects that extraction accelerates over time (0.32 → 0.72 over 24 months) as the cycle self-reinforces. Base extractiveness at 0.58 represents the midpoint institutional perspective — policy-makers perceive genuine coordination benefits (price signals, trade adjustment) embedded within extraction (debt burden, capital flight). Suppression (0.65): High. Capital controls, wage stickiness, information asymmetry, and political constraints prevent exit. Domestic agents cannot freely convert to foreign currency, cannot quickly relocate production, and face legal and political barriers to capital flight. But suppression is not total (0.95+) because some exit is possible through black markets, informal channels, and strategic negotiation. Theater ratio (0.48): Moderate. Central bank stabilization programs are partially theater (PPP doctrine suggests stability should follow automatically, but it doesn't) and partially functional (reserve accumulation and interest rate defense do temporarily arrest depreciation). The ratio reflects that policy interventions have real mechanics but operate against self-reinforcing expectations and capital flows that can overwhelm them. The piton perspective (PPP doctrine) has moderate theater because theoretical equilibrium predictions have repeatedly failed empirically across dozens of emerging market crises, yet the doctrine persists in policy discourse.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between beneficiaries' experience (coordination, favorable adjustment) and victims' experience (extraction, collapsing purchasing power). The beneficiary sees:depreciation improves their competitive position (exporters) or their debt burden (foreign creditors). The victim sees: rising import costs, wage stagnation, capital flight, policy austerity, and no mechanism to exit or recover. The analytical observer sees both are describing the same mechanical process — but the distribution of costs and benefits is so asymmetric that calling it 'coordination' from the beneficiary perspective and 'extraction' from the victim perspective are equally valid. The constraint is a tangled rope because it genuinely coordinates (allocates resources between tradable/non-tradable, maintains price signals) while simultaneously extracting (transferring wealth from wage earners to creditors). The perspectives reveal this is not a conceptual confusion but a structural fact: the same mechanism that makes foreign credit cheaper (for creditors) makes imported goods expensive (for wage earners).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from their structural relationship: Foreign creditors (beneficiaries, arbitrage exit) → low d → experience depreciation as beneficial. Export firms (beneficiaries, arbitrage exit) → low d → experience depreciation as beneficial. Wage earners (victims, trapped) → high d → experience maximum extraction. Import firms (victims, constrained exit) → moderate-high d → experience significant extraction with some recovery options. The central bank (organized intervention capacity) → moderate d → intermediate extraction experience, though constrained by institutional limits. The engine computes these values from the beneficiary/victim declarations and exit option context, producing a natural perspectival gradient from Rope (for institutional beneficiaries) through Tangled Rope (for moderate agents) to Snare (for powerless victims). No override needed — the structural facts (who benefits, who loses, who can exit) determine the directionality correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that classification varies legitimately by perspective because the structural relationship to the spiral is fundamentally asymmetric. The question is not 'which type is the spiral?' but 'from which position are you observing?' The beneficiary's Rope (depreciation is favorable coordination) and the victim's Snare (collapsing purchasing power, no exit) are both correct descriptions of the same mechanical process. The Tangled Rope classification from institutional perspectives and the Scaffold from intervention coalitions are not compromises or hedges — they are accurate descriptions of positions where genuine coordination functions coexist with asymmetric extraction. The Piton classification for PPP doctrine reflects the observation that theoretical equilibrium predictions have been empirically falsified across dozens of crises, yet the doctrine persists in policy institutions maintained by inertia rather than predictive power. The mandatrophy is resolved by understanding that the constraint's classification is determinate from any fixed perspective (there is no ambiguity within a position) but varies systematically across positions because the spiral's mechanics genuinely distribute benefits and costs asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expectations_formation_mechanism,
    'Do capital flight expectations drive depreciation, or does actual depreciation create expectations retroactively?',
    'Temporal sequence analysis: compare timing of capital outflows to currency depreciation; examine pre-depreciation forward guidance; assess whether expectation surveys precede or follow depreciation acceleration',
    'If expectations-driven: the spiral is more extractive (psychological lock-in). If depreciation-driven: coordination can arrest it through credible policy signals. If bidirectional feedback: neither causal story is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expectations_formation_mechanism, empirical, 'Causal order of expectations vs depreciation').

omega_variable(
    wage_price_pass_through_lag,
    'How quickly do import price increases translate into wage demands, and how much lag exists between import inflation and wage adjustment?',
    'Monthly data: CPI, import price indices, wage rates; cross-lag correlation; impulse response to currency shocks',
    'If lag > 12 months and wages underadjust: snare classification is correct (permanent real wage loss). If lag < 3 months and wages fully adjust: tangled_rope is correct (temporary extraction). If wages never fully adjust: extraction is cumulative and permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_price_pass_through_lag, empirical, 'Speed and completeness of wage adjustment to import inflation').

omega_variable(
    central_bank_capacity_constraint,
    'What is the actual upper limit on central bank intervention capacity — foreign reserves, credible commitment, political independence — before collapse becomes irreversible?',
    'Comparative case study: Argentina, Brazil, Turkey, Mexico, Korea. Identify intervention capacity thresholds and outcomes when capacity is exhausted.',
    'If capacity is substantial: scaffold perspective is viable (intervention can arrest spiral). If capacity is illusory: scaffold is aspirational theater, and snare/tangled_rope classify more accurately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_capacity_constraint, empirical, 'Central bank intervention capacity limits').

omega_variable(
    debt_denominated_foreign_currency_ratio,
    'What proportion of government and private debt is denominated in foreign currency versus local currency, and does this ratio determine whether depreciation is extraction or relief?',
    'National debt composition analysis; debt service burden pre- and post-depreciation; comparison of countries with high FX debt (extraction mechanism) vs low FX debt (depreciation as relief)',
    'High FX debt ratio: depreciation worsens spiral (extraction mechanism dominates). Low FX debt ratio: depreciation may relieve debt burden (mechanism inverts). The measurement is structural fact, not interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_denominated_foreign_currency_ratio, empirical, 'Proportion of foreign-currency-denominated debt').

omega_variable(
    asset_holder_exit_velocity,
    'How quickly do domestic asset holders move capital offshore once depreciation expectations form, and does this velocity exceed policy-maker intervention speed?',
    'Cross-border payment flow data; hot money tracking; timing of capital controls implementation vs capital flight acceleration',
    'If exit velocity > intervention speed: capital controls cannot prevent spiral (suppression mechanism fails). If exit velocity < intervention speed: policy can arrest expectations (scaffold viable). Structural property, not interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_holder_exit_velocity, empirical, 'Speed of capital flight relative to intervention capacity').

omega_variable(
    external_debt_service_priority_enforcement,
    'When domestic resources collapse due to depreciation, what actually enforces prioritization of external debt service over domestic spending? Is enforcement political (IMF conditionality, sanctions threat) or mechanical (inability to print foreign currency)?',
    'Historical case analysis: which countries maintained debt service during collapse (Argentina 2001, Turkey 2018, Ukraine 2015); what mechanisms enforced prioritization; what were consequences of default',
    'If mechanical: snare classification is correct (foreigners cannot be paid without external reserves, creating cascade). If political: structural choice masquerading as law; classification depends on whose interests the policy choice serves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_debt_service_priority_enforcement, conceptual, 'Enforcement mechanism for external debt service priority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_depreciation_spiral, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cds_tr_t0, currency_depreciation_spiral, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cds_tr_t12, currency_depreciation_spiral, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(cds_be_t0, currency_depreciation_spiral, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cds_be_t6, currency_depreciation_spiral, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(cds_be_t12, currency_depreciation_spiral, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(cds_be_t24, currency_depreciation_spiral, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cds_su_t0, currency_depreciation_spiral, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cds_su_t6, currency_depreciation_spiral, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(cds_su_t12, currency_depreciation_spiral, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(cds_su_t24, currency_depreciation_spiral, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_depreciation_spiral, resource_allocation).
narrative_ontology:affects_constraint(currency_depreciation_spiral, foreign_debt_trap).
narrative_ontology:affects_constraint(currency_depreciation_spiral, capital_flight_cascade).
narrative_ontology:affects_constraint(currency_depreciation_spiral, wage_price_spiral).

% DUAL FORMULATION NOTE:
% Currency depreciation spiral is a composite mechanism with multiple decomposable constraints. The upstream constraint (capital_flight_cascade) drives expectations; the parallel constraint (wage_price_spiral) handles inflation adjustment dynamics; the downstream constraint (foreign_debt_trap) models debt service mechanics. All three affect the spiral; the spiral affects all three. Prefer decomposition when analyzing specific policy interventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
