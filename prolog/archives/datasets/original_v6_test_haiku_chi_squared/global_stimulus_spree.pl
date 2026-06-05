% ============================================================================
% CONSTRAINT STORY: global_stimulus_spree
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_stimulus_spree, []).

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
 *   constraint_id: global_stimulus_spree
 *   human_readable: The 2026 Global Fiscal Stimulus Surge
 *   domain: economic/political
 *
 * SUMMARY:
 *   The 2026 global fiscal stimulus surge represents a coordinated
 *   multitrillion-dollar deployment across developed economies, targeting AI
 *   infrastructure, green energy transition, and defense rearmament.
 *   Ostensibly a response to demand-deficiency and climate urgency, the
 *   stimulus exhibits significant extractive properties: concentration of
 *   benefits among defense contractors, renewable energy oligopolies, and AI
 *   platform corporations; concentration of costs among non-subsidized
 *   sectors, emerging markets, and future generations bearing debt-servicing
 *   obligations. The constraint is a textbook Tangled Rope: it solves a
 *   genuine coordination problem (credible long-term demand commitment
 *   enables private investment in lumpy technologies) while simultaneously
 *   extracting through sectoral mismatch, currency distortion, and
 *   intergenerational debt transfer. The theater ratio (0.65) reflects the
 *   gap between stated macroeconomic rationales (demand stimulus, climate
 *   necessity, security imperative) and actual distributional mechanisms
 *   (oligopolistic procurement, subsidized capacity auctions, defense
 *   contracts). Central banks maintain the theatrical facade of independence
 *   while de facto subordinating monetary policy to fiscal dominance,
 *   creating a secondary Piton layer. The constraint will resolve as either a
 *   true Scaffold (if productivity materializes and stimulus is reversed) or
 *   an entrenched Snare (if fiscal dominance becomes permanent and
 *   oligopolistic rents crystallize).
 *
 * KEY AGENTS:
 *   - Defense Contractors / Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — receive direct procurement stimulus with guaranteed 5-10 year revenue streams
 *   - AI Platform Oligopolies (OpenAI, DeepSeek, etc.): Primary beneficiary (institutional/arbitrage) — capture subsidized cloud infrastructure spending, preferred customer status
 *   - Renewable Energy Oligopolies: Primary beneficiary (institutional/arbitrage) — guaranteed offtake agreements and capacity subsidies ensure 15-20% returns
 *   - Future Generations: Primary victim (powerless/trapped) — born into accumulated debt obligations with no exit or choice
 *   - Non-Subsidized Sectors (Healthcare, Education, Housing, Social Infrastructure): Secondary victim (moderate/constrained) — crowded out of capital markets by government borrowing
 *   - Emerging Markets / Peripheral Economies: Secondary victim (moderate/constrained) — experience capital flight, currency depreciation, commodity price inflation
 *   - Central Banks: Institutional actor (institutional/arbitrage) — coordinate stimulus through accommodation while maintaining independence theater
 *   - Mid-Tier Developed Economies (Euro area, Japan, UK, Canada): Organized actor (organized/constrained) — forced to match stimulus to maintain currency stability despite fiscal constraints
 *   - Fiscal Sustainability Coalition: Organized actor (organized/constrained) — IMF, fiscal hawks, debt-ceiling advocates attempting to constrain escalation with sunset triggers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees stimulus as legitimate cyclical policy with extractive side effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_stimulus_spree, 0.58).
domain_priors:suppression_score(global_stimulus_spree, 0.68).
domain_priors:theater_ratio(global_stimulus_spree, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_stimulus_spree, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_stimulus_spree, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_stimulus_spree, tangled_rope).
narrative_ontology:human_readable(global_stimulus_spree, "The 2026 Global Fiscal Stimulus Surge").
narrative_ontology:topic_domain(global_stimulus_spree, "economic/political").

domain_priors:requires_active_enforcement(global_stimulus_spree).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_stimulus_spree, defense_contractors).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, renewable_energy_oligopolies).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, ai_platform_corporations).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, incumbent_financial_institutions).
narrative_ontology:constraint_victim(global_stimulus_spree, future_generations_debt_burden).
narrative_ontology:constraint_victim(global_stimulus_spree, non_subsidized_sectors).
narrative_ontology:constraint_victim(global_stimulus_spree, currency_stability).
narrative_ontology:constraint_victim(global_stimulus_spree, fiscal_sovereignty_small_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Born into debt-servicing obligations with no exit. Trapped by intergenerational fiscal transfers. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Pure extraction from the powerless.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-SUBSIDIZED SMALL BUSINESSES (SNARE) — Crowded out of capital markets as government borrowing drives up rates. Trapped in local economies with no arbitrage exit. d≈0.93, f(d)≈1.38, σ=0.8 → χ≈0.64. High extraction through interest rate mechanism.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MID-TIER MANUFACTURING / NON-TECH WORKERS (TANGLED ROPE) — Benefit from stimulus-driven aggregate demand and employment; constrained by sectoral mismatch (stimulus targets AI/green/defense, not their sectors). d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.58. Mixed: coordination (employment) + extraction (sectoral bias).
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS / AI OLIGOPOLIES (ROPE) — Direct stimulus beneficiaries. Solve the coordination problem: governments credibly commit to long-term procurement and R&D funding. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary; experiences constraint as pure coordination.
constraint_indexing:constraint_classification(global_stimulus_spree, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RENEWABLE ENERGY OLIGOPOLIES (ROPE) — Stimulus provides guaranteed offtake agreements and subsidy mechanisms. Solves the coordination failure: cheap capital + long-term revenue certainty enable massive capacity deployment. d≈0.10, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(global_stimulus_spree, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CENTRAL BANKS / MONETARY POLICY (PITON) — Coordinate the stimulus surge through forward guidance and liquidity provision. Maintain the appearance of tightening while de facto facilitating fiscal dominance. Theater: central bank independence rhetoric masks subordination to fiscal policy; stress-test rigor masks accommodation. theater_ratio=0.65. Institutional inertia: inflation targets and rate-setting procedures persist despite hollowed-out functional independence. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(global_stimulus_spree, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: MID-TIER DEVELOPED ECONOMIES (TANGLED ROPE) — Constrained by currency-stability concerns; must match stimulus to avoid capital flight. Benefit from coordinated global demand stimulus. Coordination problem: competitive stimulus prevents any nation from unilateral fiscal consolidation without losing competitiveness. Extraction: reserve-currency nations (US, Euro) export inflation/currency depreciation to periphery. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: EMERGING MARKETS / PERIPHERAL ECONOMIES (SNARE) — Import inflation from stimulus-driven commodity prices; capital flight as rates rise in developed markets; currency depreciation trapped by dollar denomination of foreign debt. Constrained but not trapped — some policy autonomy remains. d≈0.82, f(d)≈1.25, σ=0.9 → χ≈0.68. Severe extraction through terms-of-trade and currency mechanisms.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 9: FISCAL SUSTAINABILITY MONITORING COALITION (SCAFFOLD) — IMF, academic economists, fiscal-responsibility advocates constraining indefinite stimulus through reporting requirements, debt-ceiling triggers, sunset provisions. Sunset logic: if inflation targets are met and productivity gains from AI/green investment materialize, stimulus can be phased out. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.31. Coordination with enforcement mechanism, but enforcement is weak (reputational, not binding).
constraint_indexing:constraint_classification(global_stimulus_spree, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / MACROECONOMIC CYCLE (TANGLED ROPE) — From a cyclical perspective, stimulus is a legitimate coordination mechanism (solving the demand-deficiency problem) with extractive side effects (redistribution to asset holders, moral hazard for future borrowing). The constraint is real but not a false summit: stimulus is both coordination and extraction. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_stimulus_spree_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_stimulus_spree, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_stimulus_spree, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_stimulus_spree, TR),
    TR >= 0.70.

:- end_tests(global_stimulus_spree_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The stimulus is designed to solve a coordination problem (credible long-term demand requires government backing for private AI/green investment), but the mechanism of solving creates extractive outcomes. Beneficiaries (defense/AI/energy oligopolies) capture rents worth 2-3% of stimulus deployment; non-beneficiaries (general population, small business, emerging markets) bear costs through inflation, crowdout, and debt burden. The 0.58 reflects that extraction is substantial (more than a pure coordination mechanism) but not maximal (some legitimate demand-deficiency justification, some real productivity potential). Suppression (0.68): High. Multiple suppression mechanisms: (1) sectoral targeting removes non-subsidized competitors' access to capital; (2) intergenerational transfer traps future generations with no exit; (3) currency/commodity linkages suppress emerging market policy autonomy; (4) political dynamics make stimulus reversal (the theoretical exit) extremely costly for elected officials. Theater ratio (0.65): Moderate-high. Central bank independence theater is significant: inflation targeting and rate-setting procedures persist despite observable fiscal dominance. Macroeconomic justifications (demand deficiency, climate urgency, security imperative) are legitimate but incomplete — they obscure the sectoral concentration and oligopolistic beneficiary structure. The trajectory shows increasing theater over the measurement interval as the gap between stated rationales and actual distributional mechanisms widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full range of classification from different structural positions. The beneficiary oligopolies (defense, AI, energy) see pure coordination: stimulus solves the credibility problem and enables investment at scale. They experience the constraint as Rope. Non-subsidized actors (small business, healthcare, education) see extraction and crowdout: they see Tangled Rope or Snare. Future generations see pure intergenerational extraction: they see Snare with no exit. The fiscal sustainability coalition sees a temporary problem with enforcement mechanisms: they see Scaffold (sunset logic). Central banks perform independence while accommodating fiscal dominance: they appear Piton (degraded function). Emerging markets see currency and commodity extraction: they see Snare. Mid-tier developed economies see forced matching: they see Tangled Rope (coordination + constraint). The analytical observer sees mixed: legitimate cyclical policy with rent-seeking side effects, classifying as Tangled Rope. This perspectival divergence is not a measurement artifact — it reflects genuine structural differences in who benefits, who pays, and who has exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense contractors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Strong net beneficiary. AI oligopolies: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Strong net beneficiary. Renewable oligopolies: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Strong net beneficiary. Future generations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit from debt obligation. Non-subsidized sectors: Victim + constrained → d≈0.78, f(d)≈1.15. High extraction; some policy exit (sectoral relocation, competitive pressure) but difficult and costly. Emerging markets: Victim + constrained → d≈0.82, f(d)≈1.25. High extraction through external shocks (commodity prices, capital flows). Mid-tier developed economies: Victim + constrained → d≈0.68, f(d)≈1.05. Moderate-high extraction; must match stimulus despite fiscal space constraints. Fiscal coalition: Organized + constrained → d≈0.45, f(d)≈0.45. Low-moderate extraction; coalition has enforcement mechanisms (reporting, triggers) but limited binding power. Central banks: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary (accommodating stimulus preserves institutional relevance), but Piton classification comes from theater gate, not chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: The stimulus surge would naively classify as pure extraction (Snare, χ ≈ 0.85 from the future-generations perspective), but mandatrophy forces decomposition into multiple perspectives that reveal the hybrid structure. The beneficiary perspectives (institutional/arbitrage) see pure coordination (Rope, χ ≈ -0.06). The victim perspectives see high extraction (Snare/Tangled Rope, χ ≈ 0.68-0.98). The analytical observer sees both: genuine coordination problem + extractive mechanism. The mandatrophy is resolved by recognizing that the constraint IS both — it genuinely solves a coordination failure (credible long-term demand) while simultaneously extracting (through oligopolistic procurement and intergenerational transfer). The classification is Tangled Rope (0.40 ≤ χ ≤ 0.90, with coordination + enforcement + asymmetric extraction). This prevents the false positive of calling it pure Snare and the false negative of calling it pure Rope. The tension is real: the more effective the stimulus is at solving demand-deficiency and enabling productive investment, the more it also enables extractive rent-capture by oligopolies. Productivity gains and extraction are mechanically coupled in the sectoral-targeting structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_materialization,
    'Will AI and green energy stimulus actually drive productivity gains sufficient to service new debt, or is it extractive transfers to rent-seeking oligopolies?',
    'Long-term TFP growth measurement (2030-2035); productivity gap between stimulus-targeted sectors and economy-wide baseline; real wage growth decomposition',
    'If productivity materializes: constraint reclassifies as Scaffold with true sunset logic. If extractive: constraint becomes pure Snare with unsustainable debt accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_materialization, empirical, 'Whether stimulus-driven investment yields real productivity gains or extractive rents').

omega_variable(
    inflation_containment_capacity,
    'Can central banks maintain inflation below 3% while accommodating multitrillion stimulus, or will they be forced to finance inflation indefinitely?',
    'PCE inflation trajectory through 2028; central bank balance sheet expansion rate; real interest rate decomposition (policy rates minus expected inflation)',
    'If inflation contained: narrative justifies stimulus (productivity justifies price increases). If inflation accelerates: stimulus becomes explicit intergenerational transfer, extraction severity increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_containment_capacity, empirical, 'Whether central banks can contain inflation under stimulus regime').

omega_variable(
    sectoral_substitution_vs_crowdout,
    'Does stimulus to AI/defense/green sectors crowd out capital in healthcare, education, and social infrastructure, or does it genuinely expand the investment frontier?',
    'Capital allocation analysis: interest-rate elasticity of non-subsidized sectors; sovereign credit spread widening for peripheral borrowers; real investment levels in education/healthcare through 2028',
    'If crowdout dominates: constraint is pure Snare for non-subsidized sectors. If frontier expansion: constraint is legitimate Rope (coordination without severe extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_substitution_vs_crowdout, empirical, 'Whether stimulus crowds out investment in non-prioritized sectors').

omega_variable(
    currency_regime_stability,
    'Will the dollar maintain reserve currency status under multitrillion US stimulus and balance-sheet expansion, or will it trigger a currency regime shift and debt crisis?',
    'Dollar share of global reserves; US real yields vs competing currencies; inflation expectations 5-10 year forward; capital flow reversals; fiscal dominance measurability',
    'If dollar holds: constraint remains manageable (financialization can absorb stimulus). If regime shifts: constraint becomes acute crisis, extraction collapses into default or hyperinflation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(currency_regime_stability, empirical, 'Whether dollar reserve currency regime survives stimulus-driven fiscal dominance').

omega_variable(
    political_sunset_enforcement,
    'Will governments actually enforce sunset clauses or stimulus phase-outs once they experience electoral benefits of spending, or will stimulus become permanent entitlement?',
    'Comparative analysis of stimulus rolloff timelines; political pressure for extension documentation; structural budget evolution 2026-2030',
    'If sunset enforced: constraint classifies as genuine Scaffold (temporary coordination problem). If permanent: constraint becomes Snare (intergenerational extraction disguised as temporary stimulus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_sunset_enforcement, preference, 'Whether governments enforce promised stimulus sunset clauses').

omega_variable(
    oligopolistic_consolidation_rate,
    'Does stimulus accelerate monopolistic consolidation in AI, renewable energy, and defense (extracting rents indefinitely), or does it create competitive markets that moderate prices?',
    'Market concentration indices (HHI) for AI infrastructure, battery manufacturing, defense procurement through 2028; pricing power evolution; barriers to entry analysis',
    'If consolidation accelerates: extraction severity increases (permanent oligopoly rents). If competition emerges: extraction diminishes (Scaffold sunset logic becomes real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopolistic_consolidation_rate, empirical, 'Whether stimulus consolidates or fragments market power in key sectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_stimulus_spree, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stim_tr_t0, global_stimulus_spree, theater_ratio, 0, 0.52).
narrative_ontology:measurement(stim_tr_t3, global_stimulus_spree, theater_ratio, 3, 0.61).
narrative_ontology:measurement(stim_tr_t6, global_stimulus_spree, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(stim_be_t0, global_stimulus_spree, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stim_be_t3, global_stimulus_spree, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(stim_be_t6, global_stimulus_spree, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_stimulus_spree, resource_allocation).
narrative_ontology:affects_constraint(global_stimulus_spree, emerging_market_currency_fragility).
narrative_ontology:affects_constraint(global_stimulus_spree, ai_chip_supply_concentration).
narrative_ontology:affects_constraint(global_stimulus_spree, renewable_capacity_oligopoly).
narrative_ontology:affects_constraint(global_stimulus_spree, fiscal_dominance_central_banking).
narrative_ontology:affects_constraint(global_stimulus_spree, intergenerational_debt_dynamics).

% DUAL FORMULATION NOTE:
% The global stimulus surge should be decomposed into two structurally distinct constraints: (1) Demand-deficiency coordination (ε≈0.15, Rope) — the genuine macroeconomic problem requiring credible fiscal backing; (2) Oligopolistic rent-capture (ε≈0.72, Snare) — the sectoral concentration and procurement bias that extracts via government mechanism. These have different ε values because they have different resolution paths: demand coordination is solved by effective stimulus and productivity gains; oligopolistic extraction is solved by competition policy and procurement reform. The unified story presents them as tangled (Tangled Rope, ε≈0.58) because they are mechanically coupled in the implementation — you cannot solve one without enabling the other. Downstream constraints (currency fragility, supply concentration, capacity oligopoly) are affected by the choice of which component dominates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_stimulus_spree, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
