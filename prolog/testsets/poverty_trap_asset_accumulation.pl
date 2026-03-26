% ============================================================================
% CONSTRAINT STORY: poverty_trap_asset_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_poverty_trap_asset_accumulation, []).

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
 *   constraint_id: poverty_trap_asset_accumulation
 *   human_readable: Poverty Trap Asset Accumulation Barrier
 *   domain: economic/poverty
 *
 * SUMMARY:
 *   The poverty trap asset accumulation barrier is a structural constraint
 *   that prevents low-income households from accumulating wealth sufficient
 *   to escape poverty, even when income is sufficient for basic consumption.
 *   The constraint operates through multiple mechanisms: high transaction
 *   costs (predatory lending, check-cashing fees), exclusion from capital
 *   markets (credit requirements, minimum deposits), regulatory barriers
 *   (welfare asset limits), and opportunity costs (time spent accessing
 *   financial services vs income generation). The extractiveness increases
 *   over the measurement interval (0.52 → 0.68) as compound effects of debt
 *   accumulation and foregone investment returns accelerate the wealth gap.
 *   Theater ratio (0.55) reflects that while enforcement effort is
 *   substantial (asset limit policing, income verification), the stated
 *   rationale (fraud prevention, incentive preservation) is only partially
 *   accurate — the primary effect is exit suppression, not fraud prevention.
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victim (powerless/trapped) — bear full cost of asset accumulation barriers through fees, opportunity costs, and foregone compounding
 *   - Financial Intermediaries: Primary beneficiary (institutional/arbitrage) — extract spreads, fees, and volumes from high-cost financial services; can exit at any time
 *   - Community Development Organizations: Secondary actor (moderate/constrained) — provide genuine coordination through matched savings and microfinance but operate within funding constraints and overhead burdens
 *   - Welfare Bureaucracy: Institutional actor (institutional/arbitrage) — maintains asset limit enforcement; benefits from simplified administration; arbitrage enables exit (could remove limits)
 *   - Policy Reform Coalition: Organized actor (organized/constrained) — advocates for asset-building programs (CDAs, matched accounts, expanded IDAs) with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as a structural feature of wealth-accumulation regimes requiring capital to generate returns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(poverty_trap_asset_accumulation, 0.68).
domain_priors:suppression_score(poverty_trap_asset_accumulation, 0.75).
domain_priors:theater_ratio(poverty_trap_asset_accumulation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poverty_trap_asset_accumulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(poverty_trap_asset_accumulation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(poverty_trap_asset_accumulation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(poverty_trap_asset_accumulation, snare).
narrative_ontology:human_readable(poverty_trap_asset_accumulation, "Poverty Trap Asset Accumulation Barrier").
narrative_ontology:topic_domain(poverty_trap_asset_accumulation, "economic/poverty").

domain_priors:requires_active_enforcement(poverty_trap_asset_accumulation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(poverty_trap_asset_accumulation, high_net_worth_investors).
narrative_ontology:constraint_beneficiary(poverty_trap_asset_accumulation, financial_intermediaries).
narrative_ontology:constraint_victim(poverty_trap_asset_accumulation, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLD (SNARE) — Trapped by capital requirements, high transaction costs, and lack of financial access. Each micro-decision (buy vs rent, save vs consume) involves extraction: predatory payday loans, check-cashing fees, overdraft charges. No exit option exists without asset base or credit history. Maximum experienced extraction.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY DEVELOPMENT ORGANIZATION (TANGLED ROPE) — Constrained by regulatory framework and funding dependencies, but also provides genuine coordinating function (matching savings, microfinance, asset-building programs). Extraction exists through overhead capture and foundation funding leverage, but real benefit to target population through coordinated access. Mixed mechanism.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARIES (ROPE) — Benefits from spreads, fees, and volume. Experiences the constraint as a coordination mechanism: channeling capital to low-income borrowers at high rates is a valid market service from their structural position. Arbitrage enables exit — can redirect capital flows at any time. Net extraction flows toward this agent.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY REFORM COALITION (SCAFFOLD) — Organized agents (nonprofits, advocates, progressive legislators) see the asset accumulation barrier as a temporary institutional failure with policy remedies: matched savings accounts, down payment assistance, asset limits reform. Sunset mechanism visible: if policy reforms (Child Development Accounts, expanded IDA programs) mature, the extraction mechanism loses force. Current suppression high, but coalition perceives declining extraction path.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE ASSET LIMIT SYSTEM (PITON) — Historical artifact from mid-20th century paternalism: asset limits on means-tested benefits (TANF, SNAP) prevent households from accumulating resources while claiming assistance. Theater ratio high — the justification (prevent cheating, preserve incentives) persists despite evidence that small asset accumulation correlates with economic stability, not fraud. The system persists through bureaucratic inertia despite contradicting its stated goal. Theater_ratio reflects that enforcement effort vastly exceeds actual fraud prevented.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — At civilizational scale, the poverty trap is a structural feature of capital accumulation regimes where compound returns require initial capital. The barrier to entry (first asset) is mathematically irreducible in wealth-generating systems. This is not naturalization — it is accurate description of the constraint at its deepest structural level. Exit requires external intervention (wealth transfer, subsidized asset programs) that individual agents cannot unilaterally access.
constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poverty_trap_asset_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poverty_trap_asset_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(poverty_trap_asset_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(poverty_trap_asset_accumulation, TR),
    TR >= 0.70.

:- end_tests(poverty_trap_asset_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The low-income household faces extraction at multiple points in every financial transaction: payday loan fees (380% APR), check-cashing charges (2-3% per check), overdraft penalties ($30-35 per incident), lack of investment access. These charges directly reduce principal available for accumulation. The 0.52 → 0.68 trajectory reflects compounding: as debt accumulates from high-cost borrowing, opportunity cost of servicing debt rises, crowding out any asset-building capacity. Suppression (0.75): Very high. Barriers to exit include: (a) structural — no credit history, no collateral, no minimum deposits required for market-rate accounts; (b) regulatory — welfare asset limits prevent asset accumulation while receiving benefits; (c) informational — financial system complexity and hidden fees; (d) time — transactions required to access alternative financial services consume hours per month. Total barrier is not insurmountable but requires external assistance. Theater ratio (0.55): Moderate. Welfare asset limits are justified (in official rhetoric) by fraud prevention and incentive maintenance, but empirical fraud rates in asset-building programs are minimal. The enforcement burden vastly exceeds actual fraud prevented. Theater here reflects that the institutional mechanism's stated purpose is secondary to its actual function (exit suppression). The theater ratio is lower than in verification constraints because the financialization mechanism is genuinely functional (banks do lend, albeit at extraction rates) — it is not purely performative, only partly so.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between beneficiaries and victims. Financial intermediaries see a legitimate service at market rates (rope/arbitrage). Low-income households see a predatory extraction mechanism (snare/trapped). The intermediaries experience low effective extraction (arbitrage exit available); the households experience maximum extraction (no exit without external help). Both descriptions are accurate from their respective positions — the gap reveals the asymmetry. The policy coalition introduces a third position: reformable scaffold with sunset logic. This challenges both the beneficiary claim ('these are natural market rates') and the victim fatalism ('the system is unchangeable'). The scaffold perspective shows that policy intervention can reduce extraction by expanding asset access — but only if organized actors invest resources to build alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position: trapped agents with no exit bear maximum extraction (d ≈ 0.95, f(d) ≈ 1.42). Financial intermediaries with arbitrage options experience negative effective extraction (d ≈ 0.15, f(d) ≈ -0.01) — extraction flows toward them. Community organizations with constrained exit and mixed beneficiary/victim status sit mid-range (d ≈ 0.55, f(d) ≈ 0.75). The analytical observer (d ≈ 0.72, f(d) ≈ 1.15) recognizes the structural barrier but is not directly trapped by it. Suppression is unscaled — the 0.75 value is a structural property of the capital access regime, independent of who observes it.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The poverty trap is unambiguously a snare at the victim's perspective (powerless/trapped/biographical), but this does not resolve the mandatrophy — mandatrophy asks whether the constraint is misclassified as coordination (rope) when it is actually extraction (snare). The evidence resolves this: (1) beneficiaries are not dependent on victims' participation (banks can exclude or charge rates independently); (2) victims have no exit even if coordination goal is achieved (matched savings still requires capital to match, still requires discipline despite material insecurity); (3) suppression is active and high (regulatory limits, fee structures, credit requirements all actively prevent exit); (4) the constraint persists due to asymmetric power (intermediaries can exit, victims cannot). The mandatrophy is resolved by confirming the snare classification and rejecting rope. The scaffold perspective does not negate this — it shows that policy can convert snare → scaffold by reducing suppression, but while the asset limits remain and high-cost lending is the primary access mechanism, the constraint is a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asset_limit_rationality,
    'Do welfare asset limits reflect genuine anti-fraud necessity or primarily serve as barrier to exit from poverty?',
    'Comparative analysis: fraud rates in programs with vs without asset limits; comparison of program integrity costs vs prevented fraud; longitudinal tracking of asset-building outcomes',
    'If limits serve anti-fraud: snare classification weakens (limits are justified coordination cost). If limits are primarily exit-suppression: snare classification confirmed (the stated rationale naturalizes extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asset_limit_rationality, empirical, 'Whether asset limits serve fraud prevention or exit suppression').

omega_variable(
    capital_access_structural_ceiling,
    'Is the asset accumulation barrier a structural property of capital markets or a contingent institutional design choice?',
    'Comparative institutional analysis: countries/regions with different asset access regimes; examination of CDAs, subsidized savings, and wealth transfer policies; modeling of counterfactual regimes',
    'If structural: snare is partially immutable (requires system-level reform). If contingent: snare is entirely engineered and fully reversible through policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_access_structural_ceiling, conceptual, 'Whether asset accumulation ceiling is structural or institutional').

omega_variable(
    intergenerational_trap_amplification,
    'Does the poverty trap constraint operate identically across generations or does each generation face compounded extraction from prior cohort failure to accumulate?',
    'Intergenerational wealth transfer data; comparison of asset trajectories for children of asset-poor vs asset-rich households; modeling of compounding effects',
    'If compounding: effective extractiveness rises across generations (χ increases at each time point). Constraint becomes more severe, not less, absent intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_trap_amplification, empirical, 'Whether poverty trap intensifies across generations').

omega_variable(
    identity_lock_internalization,
    'Do low-income households internalize the belief that asset accumulation is impossible or undeserved, creating identity-lock suppression beyond material barriers?',
    'Psychological measurement: internalized scarcity narratives, aspirational suppression; comparison of asset-building behavior when material barriers removed vs internalized belief intact; post-program narrative analysis',
    'If present: suppression metric underestimates total constraint — internalized component persists after material barriers removed. Effective suppression higher than structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether identity-based suppression amplifies material barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(poverty_trap_asset_accumulation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ptas_tr_t0, poverty_trap_asset_accumulation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ptas_tr_t5, poverty_trap_asset_accumulation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ptas_tr_t10, poverty_trap_asset_accumulation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ptas_tr_t15, poverty_trap_asset_accumulation, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(ptas_be_t0, poverty_trap_asset_accumulation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ptas_be_t5, poverty_trap_asset_accumulation, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ptas_be_t10, poverty_trap_asset_accumulation, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(ptas_be_t15, poverty_trap_asset_accumulation, base_extractiveness, 15, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(poverty_trap_asset_accumulation, resource_allocation).
narrative_ontology:affects_constraint(poverty_trap_asset_accumulation, welfare_asset_limits).
narrative_ontology:affects_constraint(poverty_trap_asset_accumulation, predatory_lending_regulation).
narrative_ontology:affects_constraint(poverty_trap_asset_accumulation, credit_system_exclusion).

% DUAL FORMULATION NOTE:
% The poverty trap decomposes into three linked constraints: (1) welfare asset limits (ε=0.35, Piton) — institutional inertia maintaining asset caps; (2) predatory lending (ε=0.72, Snare) — high-cost financial access as primary vehicle for trapped households; (3) credit system exclusion (ε=0.58, Tangled Rope) — credit bureaus and underwriting standards coordinate lending but asymmetrically extract from unestablished borrowers. The asset accumulation barrier is the emergent property when these three operate together. Each has distinct ε because each is structurally independent (asset limits could be removed without affecting lending; lending could be regulated without affecting credit systems; credit standards could be reformed independently). Together they form the poverty trap. Upstream of all three is the capital requirement itself (mathematical necessity for wealth generation in market regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(poverty_trap_asset_accumulation, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
