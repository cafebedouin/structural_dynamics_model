% ============================================================================
% CONSTRAINT STORY: emerging_market_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emerging_market_debt_trap, []).

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
 *   constraint_id: emerging_market_debt_trap
 *   human_readable: Emerging Market Debt Trap
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The emerging market debt trap is a structural mechanism through which
 *   capital-exporting nations and international financial institutions
 *   systematize wealth extraction from developing countries through debt
 *   service obligations, austerity conditionality, and currency mismatch
 *   vulnerabilities. The constraint operates across multiple reinforcing
 *   mechanisms: (1) currency mismatch — borrowing in foreign currency creates
 *   irreducible vulnerability to exchange rate shocks that debtors cannot
 *   control; (2) austerity conditionality — IMF programs enforce pro-cyclical
 *   fiscal contraction during downturns, deepening recessions and reducing
 *   growth capacity; (3) policy subordination — creditor nations and IFIs
 *   gain leverage to impose structural reforms that shift resources from
 *   domestic social spending to debt service; (4) creditor coordination —
 *   holdout strategies and litigation prevent unilateral debt restructuring,
 *   forcing compliance on creditor terms. The trap exhibits all six
 *   constraint types from different perspectives: to the debtor population it
 *   is a snare (no exit, maximum extraction); to the debtor government it is
 *   a tangled rope (genuine capital benefit alongside severe extraction); to
 *   capital exporters it is rope (pure coordination with benefit); to IFIs it
 *   is rope (mandate fulfillment with coordination function); to alternative
 *   finance coalitions it is a tangled rope with sunset (building exit
 *   pathways). The analytical observer risks naturalizing contingent
 *   institutional arrangements (currency mismatch, conditionality
 *   enforcement, creditor coordination) as immutable laws of capitalism. The
 *   measurements show a rising trajectory: extractiveness increased from 0.35
 *   to 0.58 over the interval; suppression rose from 0.55 to 0.72; theater
 *   remained moderate (0.35 to 0.55), reflecting that the extraction is
 *   relatively transparent — austerity cuts are visible, debt service
 *   obligations are explicit — but suppression is high because exit options
 *   are genuinely constrained by structural mechanisms, not by information
 *   asymmetry.
 *
 * KEY AGENTS:
 *   - Debtor Nation Populations: Primary victim (powerless/trapped) — bears extraction through reduced social spending, currency devaluation, and constrained development opportunity
 *   - Debtor Governments: Secondary victim and conditional participant (moderate/constrained) — receives capital inflows and development benefit but loses fiscal policy autonomy and faces austerity enforcement
 *   - Capital-Exporting Nations: Primary beneficiary (institutional/arbitrage) — gains capital returns and geopolitical leverage; experience no extraction because they hold exit options and power
 *   - International Financial Institutions (IMF/World Bank): Primary beneficiary (institutional/arbitrage) — gains institutional mandate expansion, program fees, and policy influence; frames extraction as rational financial discipline
 *   - Multinational Creditors: Secondary beneficiary (powerful/constrained) — captures interest margin and spreads but faces default/currency risk; balanced between extraction benefit and portfolio concentration risk
 *   - Debt Relief / Alternative Finance Coalition: Organized opposition (organized/mobile) — building alternative coordination mechanisms (debt relief, non-conditionality lending, regional development banks) that reduce extraction and create exit pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emerging_market_debt_trap, 0.58).
domain_priors:suppression_score(emerging_market_debt_trap, 0.72).
domain_priors:theater_ratio(emerging_market_debt_trap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emerging_market_debt_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(emerging_market_debt_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(emerging_market_debt_trap, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emerging_market_debt_trap, tangled_rope).
narrative_ontology:human_readable(emerging_market_debt_trap, "Emerging Market Debt Trap").
narrative_ontology:topic_domain(emerging_market_debt_trap, "economic/geopolitical").

domain_priors:requires_active_enforcement(emerging_market_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emerging_market_debt_trap, capital_exporting_nations).
narrative_ontology:constraint_beneficiary(emerging_market_debt_trap, international_financial_institutions).
narrative_ontology:constraint_beneficiary(emerging_market_debt_trap, multinational_creditors).
narrative_ontology:constraint_victim(emerging_market_debt_trap, developing_country_populations).
narrative_ontology:constraint_victim(emerging_market_debt_trap, domestic_fiscal_capacity).
narrative_ontology:constraint_victim(emerging_market_debt_trap, national_policy_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION POPULATION (SNARE) — Citizens of countries locked into debt service cycles experience maximal extraction with minimal exit options. Currency mismatch creates irreducible vulnerability; austerity conditionality forces cuts to healthcare, education, and social services. The population bears costs while decision-making power is externalized to IMF/creditors. No structural path to exit without sovereign default (which carries severe costs). Maximum f(d) at trapped + victim status.
constraint_indexing:constraint_classification(emerging_market_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEBTOR GOVERNMENT (TANGLED ROPE) — National governments receive genuine benefits from capital inflows (infrastructure, development) but face severe extraction through debt service obligations and austerity conditionality. Exit options are highly constrained: default damages creditworthiness and triggers capital flight; compliance perpetuates subordination. Active enforcement through IMF program conditions. Coordination function (capital mobilization) exists alongside asymmetric extraction (policy control).
constraint_indexing:constraint_classification(emerging_market_debt_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-EXPORTING NATIONS (ROPE) — Wealthy nations benefit from both capital returns and the coordination function of international finance (capital allocation, risk pooling). They experience the constraint as coordination: lending mobilizes capital to productive uses and earns returns. Exit options exist (arbitrage to alternative investment markets). The constraint provides genuine benefit without perception of extraction — this is their structural position as beneficiary with mobility.
constraint_indexing:constraint_classification(emerging_market_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL FINANCIAL INSTITUTIONS (ROPE) — IMF/World Bank experience the constraint as coordination: they solve the collective action problem of capital-creditor coordination (ensuring repayment through conditionality). Benefits accrue through institutional prestige, expanded mandate, and the efficiency of standardized austerity programs. High exit optionality (can shift lending focus, adjust terms). Net beneficiary. From their perspective, they are enforcing the rules of rational financial discipline, not extracting.
constraint_indexing:constraint_classification(emerging_market_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL CREDITORS (TANGLED ROPE) — Large foreign creditors (banks, investment funds) benefit from high interest rates on emerging market debt. But they also face constraints: currency risk, political risk of default, contagion from systemic crises. Coordination function exists (debt contracts allocate capital); asymmetric extraction (margin capture). Exit options constrained by portfolio concentration and systemic interconnection. They perceive lower extraction than debtor nations because they hold power, but higher extraction than capital-exporters because they face default risk.
constraint_indexing:constraint_classification(emerging_market_debt_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: DEBT RELIEF / ALTERNATIVE FINANCE COALITION (TANGLED ROPE) — Civil society organizations, alternative development banks (New Development Bank, Asian Infrastructure Investment Bank), and some G20 actors are building alternative coordination mechanisms (debt relief campaigns, green bonds, non-conditionality lending). They see the traditional debt trap as extractive and are creating exit pathways. Coordination benefit (alternative financing) exists alongside extraction reduction. Lower theater than traditional IMF programs because alternative pathways emphasize transparency. Mobile exit options for borrowers using these channels.
constraint_indexing:constraint_classification(emerging_market_debt_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical stance, debt service is an immutable feature of capital markets: borrowing always creates repayment obligations; interest always flows toward capital suppliers; developing countries always lack capital relative to growth potential. This perspective naturalizes the extraction as the inherent structure of capitalism itself. However, structural data contradicts the mountain classification — the engine will compute this as a false summit. Currency mismatch, austerity conditionality, and IMF program enforcement are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(emerging_market_debt_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emerging_market_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emerging_market_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emerging_market_debt_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emerging_market_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(emerging_market_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint captures significant wealth transfer from debtor nations to creditors through multiple mechanisms: interest margins, debt service obligations during downturns, and policy subordination. The value reflects that extraction is substantial but not absolute — debtor nations do receive capital inflows and development benefit, so pure rent-seeking does not fully characterize the relationship. The rising trajectory (0.35 → 0.58) indicates accumulating extraction as debt stocks grow and refinancing becomes costlier. Suppression (0.72): High. Multiple barriers prevent debtor exit: currency mismatch creates vulnerability to shocks beyond debtor control; austerity conditionality creates perverse incentives (contraction during downturns deepens distress); capital controls are constrained by WTO/IMF rules; litigation by holdout creditors prevents unilateral restructuring; political pressure from creditor nations constrains policy autonomy. These are structural barriers, not information asymmetries. Theater ratio (0.55): Moderate. The constraint is relatively transparent — austerity cuts are visible, debt service flows are explicit, conditionality programs are public documents. Theater is not high because IFIs do not need to conceal the mechanism; they can openly defend austerity as economically rational. Theater is not low because the mechanism includes performance: debt restructurings are negotiated as technical exercises in burden-sharing, but distributional outcomes favor creditors disproportionately.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The debtor population sees a snare (trapped, no exit, maximum experienced extraction). The debtor government sees a tangled rope (genuine capital benefit but severe loss of fiscal autonomy). Capital exporters see rope (pure coordination with benefit and exit options). IFIs see rope (mandate fulfillment and rational enforcement). The alternative finance coalition sees a tangled rope with sunset (building exit pathways that will reduce extraction over time). The civilizational analytical observer risks seeing a mountain (debt service as inherent to capitalism) — but structural data contradicts this: currency mismatch, austerity conditionality, and creditor coordination are institutional choices, not natural laws. The perspectival gap reveals that 'debt' is not a single constraint but multiple overlapping constraints with different ε values, depending on whether you measure the coordination function (capital mobilization, risk pooling) or the extraction mechanism (austerity enforcement, policy subordination). The trap works because beneficiaries can claim genuine coordination function (they are solving the real problem of capital allocation) while extracting through mechanisms that are orthogonal to coordination (austerity timing, currency exposure, policy conditions).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural relationship to the constraint: beneficiaries of capital inflows with arbitrage options (capital exporters) get low d (0.05-0.15) because they can exit; victims with no exit (debtor populations) get high d (0.90-0.98); mixed actors with constrained options (debtor governments, multinational creditors) get moderate-high d (0.55-0.75). The sigmoid f(d) then maps this structural position to experienced extractiveness chi. Debtor populations experience maximum chi because they combine high ε with high f(d) from trapped + victim status. Capital exporters experience minimum chi because they combine moderate ε with negative f(d) from beneficiary + arbitrage status. Debtor governments face intermediate chi because they are both beneficiary (capital inflows) and victim (austerity conditionality), resulting in moderate d and moderate chi. The engine derives d automatically from beneficiary/victim declarations and exit options; no override is needed here because the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the emerging market debt trap is a genuine tangled rope — it combines a real coordination function (capital mobilization, inter-temporal smoothing) with asymmetric extraction (austerity enforcement, policy subordination, interest margins). The mislabeling risk is significant: (1) pure-extraction frame (snare) misses the genuine coordination benefit of capital inflows and overstates the extractiveness; (2) pure-coordination frame (rope) misses the austerity conditionality and policy subordination that are orthogonal to capital allocation. The tangled rope classification captures both: beneficiaries and victims exist; active enforcement through conditionality programs is required; coordination function (capital provision) is genuine; extraction mechanism (austerity timing, currency exposure) is equally real. The rising theater ratio (0.35 → 0.55) indicates that performative elements are accumulating — debt restructurings are framed as technical exercises, austerity is presented as inevitable macroeconomic discipline, policy conditions are justified as rational reform. This rises toward the piton floor (degradation of functional coordination into theater) but has not yet crossed it. The alternative finance coalition's scaffold perspective is structurally real: debt relief initiatives, non-conditionality lending, and regional development banks are building credible exit pathways that reduce extraction and lower suppression. If these mature to scale (high empirical uncertainty), the trap classification could shift from tangled rope toward rope (coordination without extraction) for adopting nations. This would represent a genuine sunset, not aspirational reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    currency_mismatch_alternatives,
    'Is currency mismatch an irreducible feature of international capital flows or a contingent institutional choice?',
    'Historical comparison: periods and jurisdictions with local-currency debt markets vs. dollar-denominated debt; counterfactual analysis of capital flows under alternative currency regimes (SDR-denominated bonds, regional currency unions)',
    'If irreducible: currency vulnerability is structural (mountain feature). If contingent: currency mismatch is institutional design that could be reformed (lowers extraction floor). Changes classification from universal mountain to negotiable institutional constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(currency_mismatch_alternatives, empirical, 'Whether currency mismatch is structural or institutional').

omega_variable(
    austerity_conditionality_necessity,
    'Does IMF austerity conditionality actually improve debt sustainability, or does pro-cyclical contraction worsen recovery and increase default risk?',
    'Meta-analysis of IMF program outcomes; comparison of countries with vs. without austerity conditions; counterfactual analysis of counter-cyclical alternatives; correlation between program compliance and subsequent economic growth',
    'If austerity improves sustainability: conditionality is functional coordination (justifies tangled_rope classification). If pro-cyclical: austerity is extractive enforcement divorced from coordination benefit (upgrades to snare). Changes whether beneficiaries can claim genuine coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(austerity_conditionality_necessity, empirical, 'Whether IMF austerity actually improves debt sustainability').

omega_variable(
    alternative_finance_viability,
    'Can alternative development finance (New Development Bank, green bonds, non-conditionality lending) provide a credible exit from the traditional debt trap at scale?',
    'Tracking capital flows to emerging markets through alternative channels; monitoring debt maturity profiles and interest rate spreads in alternative vs. traditional finance; assessing borrower switching rates and refinancing outcomes',
    'If viable at scale: scaffold classification for alternative channels is correct, sunset is real, and exit options upgrade from trapped to constrained for adopting nations. If limited: traditional trap remains dominant, exit options stay trapped, alternative coalition becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_finance_viability, empirical, 'Whether alternative finance can provide scale exit from debt trap').

omega_variable(
    debt_distress_threshold_definition,
    'What quantitative threshold distinguishes manageable debt service from extractive debt distress?',
    'Cross-national comparison of debt-to-revenue ratios, interest payment burdens, and outcomes (growth, social spending, default rates); identification of threshold where policy space collapses and austerity becomes self-defeating',
    'If threshold exists and is predictive: enables early detection and targeted relief. If threshold is blurry or country-specific: extraction continues because distress is defined ex-post (after harm accumulates)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_distress_threshold_definition, empirical, 'Quantitative threshold for debt distress').

omega_variable(
    creditor_coordination_mechanism,
    'Is the debt trap maintained by active coordination among creditors (formal arrangements, cartel behavior) or by passive structural incentives (individual rational actors pursuing interest maximization)?',
    'Investigation of creditor communication channels, debt restructuring negotiations, and lending syndication patterns; analysis of whether creditors actively prevent debtor exit (holdout strategies, litigation, policy pressure) or simply react to incentives',
    'If active coordination: extraction is enforced through mechanism (snare features prominent). If passive incentives: extraction emerges from distributed action without conscious conspiracy (reduces the claim that beneficiaries are intentionally extracting)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coordination_mechanism, empirical, 'Active vs. passive creditor coordination mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emerging_market_debt_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emdt_tr_t0, emerging_market_debt_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emdt_tr_t10, emerging_market_debt_trap, theater_ratio, 10, 0.48).
narrative_ontology:measurement(emdt_tr_t20, emerging_market_debt_trap, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(emdt_be_t0, emerging_market_debt_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emdt_be_t10, emerging_market_debt_trap, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(emdt_be_t20, emerging_market_debt_trap, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(emdt_su_t0, emerging_market_debt_trap, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(emdt_su_t10, emerging_market_debt_trap, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(emdt_su_t20, emerging_market_debt_trap, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emerging_market_debt_trap, resource_allocation).
narrative_ontology:affects_constraint(emerging_market_debt_trap, fiscal_austerity_enforcement).
narrative_ontology:affects_constraint(emerging_market_debt_trap, currency_mismatch_vulnerability).
narrative_ontology:affects_constraint(emerging_market_debt_trap, creditor_coordination_cartel).
narrative_ontology:affects_constraint(emerging_market_debt_trap, policy_conditionality_subordination).
narrative_ontology:affects_constraint(emerging_market_debt_trap, structural_adjustment_programs).

% DUAL FORMULATION NOTE:
% The emerging market debt trap decomposes into five downstream constraints with distinct ε values: fiscal austerity enforcement (ε ≈ 0.65, high extraction), currency mismatch vulnerability (ε ≈ 0.45, high structural barrier), creditor coordination cartel (ε ≈ 0.70, pure extraction), policy conditionality subordination (ε ≈ 0.60, asymmetric control), and structural adjustment programs (ε ≈ 0.55, mixed coordination and enforcement). Each operates with its own mechanisms, metrics, and perspectives. The parent constraint (emerging_market_debt_trap) models the integrated system; the children model component mechanisms. The trap's extractiveness represents the aggregate effect of these mechanisms working in combination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emerging_market_debt_trap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
