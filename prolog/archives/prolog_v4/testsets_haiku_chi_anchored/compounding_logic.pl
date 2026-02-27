% ============================================================================
% CONSTRAINT STORY: compounding_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compounding_logic, []).

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
 *   constraint_id: compounding_logic
 *   human_readable: The Law of Compounding Returns
 *   domain: economic/financial_accumulation
 *
 * SUMMARY:
 *   Compounding returns represent a mathematical constraint on wealth
 *   accumulation where earnings are reinvested to generate further earnings,
 *   creating exponential growth trajectories. This constraint exhibits
 *   asymmetric structural effects depending on initial capital position:
 *   those with capital to reinvest experience it as a beneficial coordination
 *   mechanism enabling wealth growth, while those without initial capital
 *   experience it as an extraction mechanism that systematically widens the
 *   wealth gap across generations. The constraint is often naturalized as a
 *   mathematical law (mountain perspective) when viewed from a
 *   civilizational/analytical standpoint, but the structural data reveals it
 *   as a contingent institutional arrangement: compounding extraction depends
 *   critically on (a) unequal initial capital distribution, (b) suppressed
 *   access to reinvestment mechanisms for wage workers, and (c) institutional
 *   maintenance of capital-biased financial infrastructure. The theater ratio
 *   reflects that financial institutions maintain compounding machinery
 *   through significant performative activity: marketing campaigns
 *   emphasizing the 'power' of compound interest, fee structures that obscure
 *   actual returns, and recommendation systems that serve institutional
 *   interests rather than client outcomes. The trajectory shows decreasing
 *   theater ratio as transparency mechanisms (disclosure requirements,
 *   robo-advisors, financial literacy campaigns) have emerged, but the
 *   underlying extraction mechanism remains enforceable.
 *
 * KEY AGENTS:
 *   - Early Capital Holders: Primary beneficiary (institutional/arbitrage) — capture exponential wealth growth through reinvestment of existing capital; face minimal suppression due to arbitrage optionality
 *   - Wage-Dependent Workers: Primary victim (powerless/trapped) — structurally excluded from compounding advantage due to consumption-based income; lack initial capital surplus and accessible reinvestment pathways
 *   - Latecomer Investors: Secondary victim (moderate/constrained) — can invest but face time-value disadvantage; later entry creates compounding deficit relative to early entrants
 *   - Tax Policy Coalition: Organized intervener (organized/constrained) — advocates progressive taxation, capital gains taxation, inheritance taxation to redistribute compounding advantage; sees sunset mechanism in policy reform
 *   - Financial Institutions: Institutional maintainer (institutional/arbitrage) — operate compounding infrastructure (interest accrual, reinvestment protocols, account structures); maintain theatrical justification of fee structures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional inequality as mathematical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compounding_logic, 0.35).
domain_priors:suppression_score(compounding_logic, 0.42).
domain_priors:theater_ratio(compounding_logic, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compounding_logic, extractiveness, 0.35).
narrative_ontology:constraint_metric(compounding_logic, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(compounding_logic, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compounding_logic, tangled_rope).
narrative_ontology:human_readable(compounding_logic, "The Law of Compounding Returns").
narrative_ontology:topic_domain(compounding_logic, "economic/financial_accumulation").

domain_priors:requires_active_enforcement(compounding_logic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compounding_logic, capital_holders).
narrative_ontology:constraint_beneficiary(compounding_logic, early_investors).
narrative_ontology:constraint_victim(compounding_logic, latecomer_participants).
narrative_ontology:constraint_victim(compounding_logic, wage_dependent_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — Structurally excluded from capital accumulation. Income is consumption-based; compounding advantage requires initial capital surplus. No exit option from dependency on labor income. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(compounding_logic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATECOMER INVESTOR (TANGLED ROPE) — Can invest but faces compounding disadvantage from delayed entry. Benefits from coordination function (capital markets exist); bears asymmetric extraction (time-value gap compounds against them). d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(compounding_logic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY CAPITAL HOLDER (ROPE) — Benefits from first-mover advantage in capital accumulation. Sees compounding as pure coordination: capital markets enable reinvestment. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary from the constraint structure.
constraint_indexing:constraint_classification(compounding_logic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICAL INEVITABILITY (MOUNTAIN) — From a mathematical/logical perspective, compounding follows directly from exponential growth in interest-bearing systems. The constraint appears as a natural law: if earnings reinvest and generate returns, exponential growth is inevitable. Accessibility collapse ≥0.85 (mathematical proof), resistance ≤0.15 (logical necessity). However, base metrics (ε=0.35, suppression=0.42) contradict mountain classification — this is a false summit naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(compounding_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: TAX POLICY COALITION (TANGLED ROPE) — Organized actors (progressive tax advocates, redistribution coalitions) see compounding advantage as enforceable via policy (capital gains taxation, inheritance taxation). Benefits from participation in market economy; bears costs of asymmetric wealth accumulation. Sunset mechanism: progressive taxation can reduce compounding inequality. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(compounding_logic, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FINANCIAL INSTITUTION (PITON) — Banks and investment firms maintain compounding machinery (interest, reinvestment protocols, account structures) through institutional inertia. The theater ratio reflects that much institutional activity around compounding is performative: marketing 'power of compound interest', fee structures that obscure actual returns, and recommendation systems that serve institutional interests rather than client outcomes. theater_ratio=0.38 reflects emerging transparency (disclosure requirements, calculator tools), but substrate remains theater-heavy.
constraint_indexing:constraint_classification(compounding_logic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compounding_logic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(compounding_logic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(compounding_logic, TR),
    TR >= 0.70.

:- end_tests(compounding_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The compounding advantage creates real wealth asymmetry, but the extraction is not as severe as a pure snare (ε≥0.46) because (a) compounding is mathematically available to any agent with capital and reinvestment access, and (b) institutional mechanisms (index funds, employer matching) partially democratize access, reducing the extraction coefficient. The 0.35 value reflects that while compounding advantage is real and compounds inequality, it operates through a coordination mechanism (capital markets) that participants can theoretically access. Suppression (0.42): Moderate-high. Significant barriers exist: initial capital requirement, financial literacy requirements, time-horizon constraints (compounding requires decades), wage compression that eliminates surplus for reinvestment, and institutional design that privileges early entrants. However, suppression is not total — employer matching, low-cost index funds, and retirement accounts partially reduce barriers. Theater ratio (0.38): Moderate. The financial industry maintains substantial performative infrastructure: marketing emphasizing compound interest 'power', fee opacity, and recommendation systems designed to maximize institutional revenue rather than client compounding. However, the trajectory shows declining theater as transparency mechanisms (fee disclosure, index fund adoption, fintech disruption) have increased. The current ratio reflects both the remaining institutional theater and the emerging transparency infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Early capital holders see pure coordination (Rope) — they benefit from having investment mechanisms available. Wage workers see pure extraction (Snare) — they are trapped without initial capital. Latecomers see mixed extraction and coordination (Tangled Rope) — they can participate but at asymmetric time-value cost. The organized coalition sees a reformable constraint with sunset potential (Tangled Rope with policy mechanisms). The financial system sees itself as maintaining a degraded ritual (Piton) — the theater around compounding has declined as transparency increased, yet the institutional machinery persists through inertia. The civilizational analytical view risks seeing mathematical inevitability (Mountain) — but the structural data reveals this as a false summit: the extractiveness and suppression metrics indicate a contingent institutional arrangement, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Early capital holders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; arbitrage exit gives them choice to deploy capital or not. Wage-dependent workers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit from consumption-based income and suppressed reinvestment access. Latecomer investors: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction; can invest but face time-value deficit and cannot exit without foregoing wealth accumulation. Tax policy coalition: Organized + constrained → d≈0.45, f(d)≈0.45. Moderate extraction; coalition has agency and sees policy levers (sunset mechanism) even though implementing them is constrained by political capital. Financial institutions: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.38 ≤ 0.70 fails the gate, but institutional perspective maintains the machinery through inertia). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification (natural law view) is perspectival and false; the engine's false summit detector identifies the conflict between the mountain claim and the moderate suppression/extractiveness metrics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    initial_capital_source,
    'Is the compounding disadvantage for non-capital-holders an intrinsic mathematical property or a contingent result of unequal initial distribution?',
    'Comparative analysis: societies with high initial capital redistribution vs low redistribution; universal basic capital schemes (sovereign wealth funds, endowments distributed per capita)',
    'If mathematical inevitability: compounding inequality is unavoidable even with redistribution. If contingent: compounding becomes neutral if initial capital is equalized; the constraint''s extractiveness would drop to ≈0.05 (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(initial_capital_source, empirical, 'Whether compounding disadvantage derives from mathematics or initial inequality').

omega_variable(
    reinvestment_accessibility,
    'Can wage-dependent workers access compounding returns through accessible instruments (index funds, automated saving, employer matching)?',
    'Historical comparison of compounding outcomes for workers with access to employer 401(k) matching vs without; correlation between account accessibility and accumulated wealth inequality over 30-year careers',
    'If accessible mechanisms exist and are utilized: suppression drops (≤0.30); constraint reclassifies as Rope from more perspectives. If inaccessible due to cognitive/financial barriers: suppression remains high; snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinvestment_accessibility, empirical, 'Whether compounding is accessible to wage-dependent participants').

omega_variable(
    inflation_erosion_asymmetry,
    'Does compounding advantage remain stable across inflation regimes, or does wage-labor income compression erase worker compounding despite nominal returns?',
    'Real-return analysis: nominal vs inflation-adjusted compounding outcomes for workers vs capital holders across high-inflation vs low-inflation periods',
    'If real returns remain positive for workers: compounding is available if implemented. If inflation disproportionately erodes worker capital: suppression mechanism tightens; extractiveness may increase to ≈0.50 (pure snare territory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_erosion_asymmetry, empirical, 'Whether compounding advantage persists across inflation regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compounding_logic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compound_tr_t0, compounding_logic, theater_ratio, 0, 0.55).
narrative_ontology:measurement(compound_tr_t5, compounding_logic, theater_ratio, 5, 0.45).
narrative_ontology:measurement(compound_tr_t10, compounding_logic, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(compound_be_t0, compounding_logic, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(compound_be_t5, compounding_logic, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(compound_be_t10, compounding_logic, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compounding_logic, resource_allocation).
narrative_ontology:affects_constraint(compounding_logic, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(compounding_logic, intergenerational_capital_transfer).
narrative_ontology:affects_constraint(compounding_logic, financial_system_architecture).

% DUAL FORMULATION NOTE:
% Compounding logic is decomposed into two structurally distinct claims: (1) Mathematical compounding: the exponential growth property of interest-bearing systems (ε≈0.08, Mountain). (2) Institutional compounding advantage: the asymmetric extraction that arises when initial capital is unequally distributed (ε=0.35, Tangled Rope). This story addresses the institutional claim. The mathematical claim is upstream and affects this constraint through the derivation chain: compounding advantage depends on access to the mathematical mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(compounding_logic, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
