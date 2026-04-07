% ============================================================================
% CONSTRAINT STORY: institutional_investor_governance_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_investor_governance_power, []).

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
 *   constraint_id: institutional_investor_governance_power
 *   human_readable: Institutional Investor Governance Power and Corporate Control
 *   domain: financial/corporate_governance
 *
 * SUMMARY:
 *   Institutional investor governance power represents a structural
 *   constraint on corporate capital allocation where large asset managers
 *   (Vanguard, BlackRock, State Street, Fidelity) exercise concentrated
 *   voting control over portfolio companies. The constraint exhibits hybrid
 *   coordination-extraction dynamics: asset managers provide genuine
 *   coordination functions through pooled capital, standardized governance
 *   frameworks, and economies of scale, while simultaneously concentrating
 *   governance power in ways that extract value from minority shareholders,
 *   workers, and other stakeholders. The extractiveness has grown from 0.35
 *   in 2004 (when passive index management was less dominant) to 0.58 in 2024
 *   (as index fund dominance reached 45% of US equity markets). Theater ratio
 *   reflects that while corporate boards and shareholder votes remain
 *   structurally intact, their substantive outcomes are pre-determined by
 *   institutional block voting, rendering democratic shareholder governance
 *   increasingly performative.
 *
 * KEY AGENTS:
 *   - Large Asset Managers (Vanguard, BlackRock, State Street): Institutional beneficiaries (institutional/arbitrage) — exercise concentrated governance control while maintaining arbitrage flexibility through capital reallocation.
 *   - Minority Shareholders: Primary victims (powerless/trapped) — retail investors and small funds with no meaningful exit from concentrated institutional control.
 *   - Workers: Secondary victims (powerless/trapped) — employees subject to wage suppression and benefit cuts driven by asset manager profit maximization directives.
 *   - Mid-Sized Asset Managers: Constrained institutional actors (moderate/constrained) — benefit from passive management coordination but constrained by dominance of largest three firms.
 *   - Regulatory Bodies (SEC, DOJ, state AGs): Organized advocates (organized/mobile) — see concentration as problematic but politically constrained by fintech lobbying and legislative gridlock.
 *   - Corporate Boards: Institutional theater (institutional/arbitrage) — maintain performance of independent governance while actually executing asset manager directives.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination (capital pooling enables efficient markets) and genuine extraction (concentrated voting power enables asymmetric value transfer).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_investor_governance_power, 0.58).
domain_priors:suppression_score(institutional_investor_governance_power, 0.68).
domain_priors:theater_ratio(institutional_investor_governance_power, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_investor_governance_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_investor_governance_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_investor_governance_power, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_investor_governance_power, tangled_rope).
narrative_ontology:human_readable(institutional_investor_governance_power, "Institutional Investor Governance Power and Corporate Control").
narrative_ontology:topic_domain(institutional_investor_governance_power, "financial/corporate_governance").

domain_priors:requires_active_enforcement(institutional_investor_governance_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_investor_governance_power, large_asset_managers).
narrative_ontology:constraint_beneficiary(institutional_investor_governance_power, institutional_investors).
narrative_ontology:constraint_victim(institutional_investor_governance_power, minority_shareholders).
narrative_ontology:constraint_victim(institutional_investor_governance_power, workers).
narrative_ontology:constraint_victim(institutional_investor_governance_power, stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY SHAREHOLDER (SNARE) — Individual investors have no meaningful exit from concentrated institutional control. Voting power is diluted; institutions vote blocks that maximize their own returns regardless of minority interests. Trapped by market structure and information asymmetries. Bears extraction with no recourse.
constraint_indexing:constraint_classification(institutional_investor_governance_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKER (SNARE) — Employment remains at institutional investor discretion. No exit from wage suppression, benefit cuts, or layoffs driven by asset manager profit maximization. Structurally trapped by labor market concentration and capital control. Maximum extraction with zero negotiating power.
constraint_indexing:constraint_classification(institutional_investor_governance_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SIZED ASSET MANAGER (TANGLED ROPE) — Benefits from coordination of voting blocks and governance coordination among peer institutions. Also constrained by larger asset managers' dominance (Vanguard, BlackRock, State Street) and regulatory oversight. Mixed coordination function and asymmetric extraction. Can exit only at significant competitive cost.
constraint_indexing:constraint_classification(institutional_investor_governance_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE ASSET MANAGER (ROPE) — Experiences institutional investor governance as pure coordination: pooled voting enables economies of scale and passive index management. Net beneficiary with arbitrage exit options (capital deployment flexibility). Low suppression for this actor.
constraint_indexing:constraint_classification(institutional_investor_governance_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — SEC, DOJ, state attorneys general see concentrated asset manager control as both a coordination mechanism (for market transparency) and an extractive mechanism (for minority shareholder harm). Organized but politically constrained by fintech lobbying. Mobile exit path through antitrust enforcement but faces institutional resistance.
constraint_indexing:constraint_classification(institutional_investor_governance_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CORPORATE BOARD SYSTEM (PITON) — Board governance persists through institutional theater: directors claim independence while institutional investors drive actual control. Shareholder voting is substantively performative — outcomes pre-determined by asset manager blocs. Theater ratio of 0.55 reflects that while boards still deliberate, their decisions are constrained by prior institutional block voting.
constraint_indexing:constraint_classification(institutional_investor_governance_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, institutional investor governance coordinates capital allocation (genuine coordination function) while concentrating control in asset manager hands (asymmetric extraction). Both functions are real and inseparable. The constraint is neither pure coordination nor pure extraction but a hybrid where concentration enables coordination efficiency at the cost of democratic capital control.
constraint_indexing:constraint_classification(institutional_investor_governance_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_investor_governance_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_investor_governance_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_investor_governance_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_investor_governance_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_investor_governance_power, TR),
    TR >= 0.70.

:- end_tests(institutional_investor_governance_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and rising. The base measure reflects that institutional investor voting directly produces measurable minority shareholder harm: depressed dividend policies favoring capital appreciation (taxed in asset manager hands), executive compensation that ignores worker wages, strategic decisions prioritizing short-term returns over long-term stakeholder value. The trajectory from 0.35 to 0.58 over 20 years tracks the rise of index dominance (2004: 13% of US equities; 2024: 45%), which reduced the competitive discipline that active managers previously provided. Suppression (0.68): High and structural. Minority shareholders face information asymmetries (opaque beneficial ownership through mutual funds), voting power concentration (institutions hold blocking positions), and exit barriers (transaction costs, tax consequences). Workers face labor market concentration (asset managers increasingly own both competitors in an industry, preventing wage arbitrage) and capital-mobility threat (firms are restructured for financial extraction). Theater ratio (0.55): Moderate. Corporate boards conduct genuine deliberation and shareholder votes are mechanically real, but outcomes are pre-determined by institutional block voting. The theater is not zero because boards do occasionally resist or negotiate with asset managers; it is not high (>0.70) because institutional control is structurally dominant, not merely nominal.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between large asset managers and powerless agents. Asset managers experience the constraint as coordination: pooled voting power enables scale economies, passive indexing provides transparent benchmarks, institutional discipline improves governance compliance. Minority shareholders and workers experience the same constraint as extraction: concentrated block voting suppresses dissent, passive indexing removes competitive oversight of compensation and strategy, institutional discipline is enforced downward toward workers rather than upward toward shareholders. Regulatory bodies see the gap as an antitrust problem (concentration), but the underlying structure is more complex: even if Vanguard were divided, the remaining entities would still coordinate voting blocks and produce similar outcomes. The Piton perspective (degraded ritual of board governance) reveals that the theater has grown as institutional power has concentrated — boards deliberate more performatively, not less, because outcomes are predetermined. The Scaffold perspective is absent because there is no sunset clause: unlike temporary regulatory measures, institutional investor governance is structural and permanent under current market conditions.
 *
 * DIRECTIONALITY LOGIC:
 *   Large asset managers derive d ≈ 0.15 (institutional power + arbitrage exit = beneficiary directionality): they capture value through governance control while maintaining exit flexibility through capital reallocation. Minority shareholders derive d ≈ 0.92 (powerless + trapped = maximum target directionality): they bear governance extraction with no exit. Workers derive d ≈ 0.95 (powerless + trapped = maximum target directionality): they experience wage suppression and restructuring with no meaningful exit. Mid-sized asset managers derive d ≈ 0.65 (moderate power + constrained exit = mixed): they benefit from passive management coordination but are subordinated to the largest three firms. Regulatory bodies derive d ≈ 0.58 (organized + mobile = moderate target directionality despite advocacy role): they have structural capacity to enforce antitrust but face political constraints. The perspectival gap is large: large asset managers perceive Rope (coordination), while powerless agents perceive Snare (pure extraction). The analytical observer perceives Tangled Rope because both functions are empirically present and inseparable.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is not a false labeling of pure extraction as coordination (Snare masquerading as Rope) nor a false labeling of pure coordination as extraction. Asset managers genuinely provide coordination functions (pooled capital, standardized governance frameworks, economies of scale) AND they genuinely extract value (concentrated voting, minority shareholder harm, worker wage suppression). The constraint is a true Tangled Rope because: (1) Coordination function is real and structural — breaking the asset manager oligopoly would fragment capital allocation, reduce market transparency, increase governance compliance costs; (2) Extraction is real and measurable — minority shareholding returns underperform peer group medians, worker wages lag productivity, strategic decisions prioritize asset manager IRR over stakeholder value; (3) Active enforcement is required — asset managers must explicitly coordinate voting blocks (Institutional Shareholder Services governance letters, BlackRock engagement programs, Vanguard proxy advisory coordination) to maintain both coordination and extraction; (4) Neither function can be removed without disabling the other. The mandatrophy is avoided because the analytical observer correctly identifies both as present, rather than labeling the constraint as 'just coordination' (which would justify high concentration) or 'just extraction' (which would miss the genuine coordination benefits that asset manager consolidation provides).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficial_ownership_obscurity,
    'Is the extraction mechanism the concentrated asset manager control, or the obscured beneficial ownership that prevents retail investors from understanding their own capital flows?',
    'Empirical analysis of retail investor behavior when beneficial ownership is transparent (e.g., direct indexing disclosures) vs opaque (mutual fund abstraction). Measurement of voting participation rates before/after SEC transparency rules.',
    'If ownership obscurity is primary: constraint relaxes significantly with disclosure. If institutional control is primary: transparency alone does not redistribute power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_ownership_obscurity, empirical, 'Whether suppression is concentrated control or obscured ownership').

omega_variable(
    passive_versus_active_extraction,
    'Does passive index management inherently extract from minority shareholders, or do active asset managers extract specifically through discretionary voting?',
    'Comparative analysis of voting outcomes and minority harm across passive vs active-managed portfolios. Longitudinal tracking of minority shareholder outcomes (dividend payments, board diversity, long-term value) in passive vs actively managed indexes.',
    'If passive extraction is structural: constraint applies to all asset management. If active discretion is extractive: constraint could be partially addressed through governance reforms on voting direction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(passive_versus_active_extraction, empirical, 'Whether extraction is inherent to passive management or specific to active voting').

omega_variable(
    antitrust_feasibility,
    'Is asset manager concentration the constraint, or is market structure (the concentration of capital itself) the irreducible constraint?',
    'Analysis of hypothetical antitrust breakup scenarios. Would dividing Vanguard/BlackRock reduce institutional extraction, or would the divided entities still coordinate through peer voting? Game-theoretic modeling of n-player voting cartels.',
    'If concentration is the constraint: antitrust enforcement is viable remedy. If market structure is the constraint: breakup provides no relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antitrust_feasibility, conceptual, 'Whether constraint is concentration or market structure itself').

omega_variable(
    stakeholder_coordination_necessity,
    'Do workers and minority shareholders have genuine coordination functions in corporate governance, or are they purely targets of extraction?',
    'Comparative analysis of firms with genuine stakeholder governance (Germany, Scandinavia) vs shareholder-only governance. Measurement of firm performance, worker productivity, innovation metrics across governance models.',
    'If stakeholders provide coordination function: constraint could be reframed as Scaffold or reformed Tangled Rope. If purely extraction: Snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_coordination_necessity, empirical, 'Whether stakeholders serve coordination or extraction function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_investor_governance_power, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iigp_tr_t0, institutional_investor_governance_power, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iigp_tr_t10, institutional_investor_governance_power, theater_ratio, 10, 0.48).
narrative_ontology:measurement(iigp_tr_t20, institutional_investor_governance_power, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(iigp_be_t0, institutional_investor_governance_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iigp_be_t10, institutional_investor_governance_power, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(iigp_be_t20, institutional_investor_governance_power, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_investor_governance_power, resource_allocation).
narrative_ontology:affects_constraint(institutional_investor_governance_power, labor_market_monopsony_power).
narrative_ontology:affects_constraint(institutional_investor_governance_power, pension_fund_governance_capture).
narrative_ontology:affects_constraint(institutional_investor_governance_power, index_fund_market_concentration).

% DUAL FORMULATION NOTE:
% Institutional investor governance power decomposes into three structurally distinct constraints: (1) asset manager coordination of capital allocation (Rope, ε≈0.10), (2) institutional investor concentration of voting control (Snare, ε≈0.72), (3) worker wage suppression through portfolio cross-ownership (Tangled Rope, ε≈0.55). This story addresses the hybrid constraint at the market level. The upstream constraint is index fund market concentration (ε≈0.45, Tangled Rope); the downstream constraints are labor market effects and pension fund capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_investor_governance_power, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
