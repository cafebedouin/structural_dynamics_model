% ============================================================================
% CONSTRAINT STORY: fiscal_dominance_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_dominance_trap, []).

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
 *   constraint_id: fiscal_dominance_trap
 *   human_readable: The Debt-Monetary Bind (Fiscal Dominance Trap)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The fiscal dominance trap emerges when a sovereign's accumulated debt
 *   burden becomes so large that interest rate increases required for
 *   inflation control would trigger immediate debt service crises, leading
 *   the central bank to abandon price-stability mandates in favor of debt
 *   sustainability. This constraint exemplifies how coordination mechanisms
 *   (preventing debt spirals) can be structurally hijacked to extract from
 *   those dependent on currency stability. The constraint has deepened over
 *   the 30-year interval as debt-to-GDP ratios increased (post-2008 crisis,
 *   COVID spending, structural deficits), narrowing the central bank's exit
 *   space and increasing the extraction experienced by savers and
 *   fixed-income earners. The theater ratio of 0.48 reflects that while
 *   formal central bank independence remains institutionally intact, the
 *   actual decision-making is increasingly subordinated to fiscal
 *   sustainability concerns, creating a gap between proclaimed autonomy and
 *   structural reality.
 *
 * KEY AGENTS:
 *   - Savers and Fixed-Income Earners: Primary victims (powerless/trapped) — inflation erodes purchasing power; central bank rate increases blocked
 *   - Price-Stability Mandate: Primary victim (powerless/trapped) — institutional goal becomes non-binding; credibility erosion
 *   - Central Bank: Primary actor (moderate/constrained) — experiences both coordination function and extraction; constrained exit if rates increase
 *   - Treasury / Government: Primary beneficiary (institutional/arbitrage) — sustained accommodative policy enables deficit financing; can arbitrage through inflation or currency depreciation
 *   - Deficit-Financed Constituencies: Secondary beneficiary (organized/constrained) — organized enough to demand continued spending; trapped within inflation regime
 *   - Central Bank Independence Ideal: Institutional fiction (institutional/arbitrage) — formal independence persists but is performative; theater disguises structural subordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_dominance_trap, 0.58).
domain_priors:suppression_score(fiscal_dominance_trap, 0.65).
domain_priors:theater_ratio(fiscal_dominance_trap, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_dominance_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(fiscal_dominance_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fiscal_dominance_trap, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_dominance_trap, tangled_rope).
narrative_ontology:human_readable(fiscal_dominance_trap, "The Debt-Monetary Bind (Fiscal Dominance Trap)").
narrative_ontology:topic_domain(fiscal_dominance_trap, "economic/political").

domain_priors:requires_active_enforcement(fiscal_dominance_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiscal_dominance_trap, treasury_government).
narrative_ontology:constraint_beneficiary(fiscal_dominance_trap, large_deficit_financed_constituencies).
narrative_ontology:constraint_victim(fiscal_dominance_trap, inflation_targets).
narrative_ontology:constraint_victim(fiscal_dominance_trap, currency_stability).
narrative_ontology:constraint_victim(fiscal_dominance_trap, savers_fixed_income_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAVER / FIXED-INCOME EARNER (SNARE) — Bears full extraction through inflation erosion of purchasing power. Trapped by dependence on nominal savings; cannot exit currency system. Central bank rate increases are structurally blocked, ensuring suppression of real returns. Maximum experienced extraction — no meaningful exit options within the national monetary system.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRICE-STABILITY STANDARD (SNARE) — Abstract mandate for price stability becomes non-binding; central bank legally committed to inflation control but structurally prevented from executing. Institutional goal is held hostage; bears extraction through loss of credibility and mandate erosion. Trapped — cannot exit the constraint without currency regime change.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (TANGLED ROPE) — Experiences both coordination and extraction. The constraint serves coordination function: it aligns monetary policy with fiscal sustainability, preventing independent rate increases that would destabilize debt dynamics. But the coordination is asymmetrically enforced — the central bank bears the cost of foregone inflation control while treasury captures the benefit. Constrained exit: can raise rates, but only at catastrophic fiscal cost (debt spiral). Mixed experience: coordination necessity + extraction mechanism.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TREASURY / GOVERNMENT (ROPE) — Benefits from constraint as pure coordination. The bind locks the central bank into accommodative policy, preventing rate spikes that would trigger debt crises. Treasury experiences this as solving a coordination problem: how to sustain large deficits without central bank discipline triggering fiscal insolvency. Beneficiary with arbitrage options — can manage debt by choosing inflation over austerity; exit is available through currency depreciation or default (though costly).
constraint_indexing:constraint_classification(fiscal_dominance_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFICIT-FINANCED CONSTITUENCIES (TANGLED ROPE) — Organized beneficiaries of sustained deficit spending. The constraint enables continued transfer payments, public sector employment, and social programs that would face austerity if the central bank could freely raise rates. Experience mixed: benefits from coordination (guaranteed continued spending) but also victims of extraction (inflation erodes nominal wages; purchasing power of benefits declines). Constrained exit — organized enough to pressure for continued spending, but trapped within inflation regime.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CENTRAL BANK INDEPENDENCE IDEAL (PITON) — The institutional fiction of central bank independence persists as pure theater. Formal legal autonomy (independent board, statutory price-stability mandate) remains in place, but the structural constraint has rendered independence performative. The central bank maintains the appearance of autonomy through communication rituals (forward guidance, policy statements) while actual decisions are dominated by fiscal sustainability concerns. Theater ratio reflects the gap between formal independence and structural subordination. Piton classification: degraded mandate maintained through institutional inertia rather than function.
constraint_indexing:constraint_classification(fiscal_dominance_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, fiscal dominance represents a genuine hybrid: it solves a coordination problem (preventing debt-deflationary spirals through accommodative monetary policy) while simultaneously extracting from those dependent on currency stability and savers. The constraint has both functional elements (it does coordinate monetary-fiscal interaction) and extractive elements (it distributes costs asymmetrically toward those without political voice). The bind is not a mountain (natural law) — it is structurally contingent on institutional design choices around debt issuance and central bank mandates. Not a pure rope (pure coordination would have symmetric benefits) nor pure snare (the government's choice is not irreversible). A genuine tangled rope: coordination mechanism + asymmetric extraction.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_dominance_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiscal_dominance_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_dominance_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiscal_dominance_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiscal_dominance_trap, TR),
    TR >= 0.70.

:- end_tests(fiscal_dominance_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from savers and inflation-target credibility, transferring purchasing power and policy autonomy toward deficit financiers and the government. The extraction is not total (0.66+) because: (1) some inflation is demand-driven and would occur even with independent monetary policy, (2) governments and deficit constituencies also bear costs (currency depreciation, international borrowing costs), and (3) exit options exist (currency union, financial repression vs open defaults). Suppression (0.65): High. Significant barriers prevent savers from exiting inflation: currency system dependency, no alternative stores of value without cost, central bank legally committed to price stability but structurally prevented from delivering it. The constraint explicitly suppresses rate increases. Theater ratio (0.48): Moderate. Central bank independence remains formally intact through legal autonomy, policy communications, and institutional aesthetics. But the performance increasingly diverges from structure — forward guidance and policy statements must now accommodate fiscal sustainability, creating a gap between communicated autonomy and actual constraints.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals sharp perspectival divergence across power levels. Powerless agents (savers, price-stability mandate) see extraction (Snare) with no exit. Moderate agents (central bank) see a mixed arrangement requiring difficult tradeoffs (Tangled Rope). Institutional beneficiaries (treasury, organized constituencies) see either pure coordination (Rope) or continued access to deficit financing (Tangled Rope with net benefit). The central bank independence ideal sees its own degradation as theater (Piton). The analytical observer recognizes this as a genuine tangled rope: a hybrid structure solving a coordination problem (preventing debt-deflationary spirals) while asymmetrically extracting from those without political voice. The gap reflects the asymmetric distribution of exit options — governments and large constituencies can influence fiscal policy; individual savers cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the constraint's extraction flow. Savers are full targets: they bear inflation costs and have trapped exit (d ≈ 0.95). The price-stability mandate is a victim: it exists as a legal commitment but is structurally overridden (d ≈ 0.90). The central bank occupies a hybrid position: it benefits from the coordination function (preventing debt crises) but is extracted from through loss of autonomy (d ≈ 0.55). The treasury is a full beneficiary: accommodative policy enables deficit spending without triggering rate spikes (d ≈ 0.10). Organized constituencies benefit from continued transfers but are trapped in inflation regime (d ≈ 0.65). The central bank independence ideal benefits from the fiction of autonomy but is victimized by its own structural subordination (d ≈ 0.70).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that fiscal dominance is NOT a false natural law (mountain) but a genuine hybrid structure (tangled rope). The temptation toward mountain classification arises from framing the constraint as inherent to sovereign debt dynamics — 'every government must eventually face the debt-monetary bind.' This naturalization conceals the contingent institutional design choices: (1) the size and structure of public debt issuance, (2) the scope of central bank mandates, (3) the political institutions governing fiscal policy, (4) the degree of currency sovereignty. Tangled rope classification preserves both the genuine coordination function (the constraint does prevent debt spirals) and the genuine extraction (it asymmetrically distributes costs). The mandatrophy is resolved by recognizing that this is a design problem, not a law of nature — different institutional architectures (currency unions, rules-based fiscal frameworks, debt constraints) can change the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_sustainability_threshold,
    'At what debt-to-GDP ratio does the fiscal dominance trap become self-reinforcing (interest rates rising even at accommodative monetary policy)?',
    'Empirical analysis of sovereign debt crises; correlation between debt ratios and interest rate dynamics; modeling of debt service costs under various inflation/growth scenarios',
    'If threshold < 80% debt-to-GDP: trap activates early; many high-income countries vulnerable. If threshold > 120%: trap requires extreme debt levels; current situation may not yet be self-reinforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt-to-GDP threshold for self-reinforcing dominance trap').

omega_variable(
    inflation_persistence_mechanism,
    'Is the inflation caused by monetary accommodation (fiscal dominance) or by supply shocks, wage-price spirals, and expectations anchoring independent of central bank behavior?',
    'Decomposition of inflation into demand-pull vs cost-push components; analysis of inflation expectations surveys; comparison of countries with vs without fiscal dominance',
    'If monetary accommodation is primary cause: central bank could reduce inflation by raising rates despite fiscal costs (snare from saver perspective is extraction-driven). If supply shocks dominate: rate increases would have minimal inflation impact and maximum debt crisis risk (snare perspective reveals structural inescapability rather than extraction choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_persistence_mechanism, empirical, 'Whether inflation is primarily from monetary accommodation or supply factors').

omega_variable(
    credibility_loss_irreversibility,
    'Once central bank credibility is lost to fiscal dominance, is it recoverable without structural reform (e.g., fiscal consolidation, debt restructuring)?',
    'Historical case studies of central banks regaining inflation credibility; time-series analysis of inflation expectations reversal; policy experiment records (Volcker era, euro crisis episodes)',
    'If credible recovery is possible: piton classification may be too pessimistic (independence ideal can be revived). If recovery requires severe restructuring: the constraint is nearly irreversible (closer to snare than tangled rope for long horizons).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credibility_loss_irreversibility, empirical, 'Whether central bank credibility can be recovered without structural reform').

omega_variable(
    coalition_escape_via_currency_union,
    'Can fiscal dominance be escaped by joining a currency union (euro, SDR) with external monetary discipline?',
    'Analysis of euro member states'' escape from national fiscal dominance; comparison of pre-euro vs post-euro constraints; assessment of ECB insulation from national fiscal pressure',
    'If currency union escape is effective: powerless agents have arbitrage options (mobile exit); constraint is more snare-like for those without union access, rope-like for those with union exit. If ECB faces same dominance pressures: exit does not resolve constraint (network constraint on all participants).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_escape_via_currency_union, empirical, 'Whether currency union membership enables escape from fiscal dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_dominance_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisc_tr_t0, fiscal_dominance_trap, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fisc_tr_t15, fiscal_dominance_trap, theater_ratio, 15, 0.4).
narrative_ontology:measurement(fisc_tr_t30, fiscal_dominance_trap, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fisc_be_t0, fiscal_dominance_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fisc_be_t15, fiscal_dominance_trap, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(fisc_be_t30, fiscal_dominance_trap, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_dominance_trap, resource_allocation).
narrative_ontology:affects_constraint(fiscal_dominance_trap, inflation_credibility_trap).
narrative_ontology:affects_constraint(fiscal_dominance_trap, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(fiscal_dominance_trap, currency_depreciation_spiral).

% DUAL FORMULATION NOTE:
% Fiscal dominance is the upstream constraint that determines central bank policy space. Downstream constraints include inflation expectations credibility (which fails if the bind persists) and currency depreciation spirals (which accelerate if dominance traps become self-evident). Decomposition: the bind itself (this story, ε=0.58) is structurally distinct from its proximate outcomes (inflation targeting failure, currency crises) which should be modeled as downstream constraints with higher extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiscal_dominance_trap, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
