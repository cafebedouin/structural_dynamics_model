% ============================================================================
% CONSTRAINT STORY: y_combinator
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-04-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_y_combinator, []).

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
 *   constraint_id: y_combinator
 *   human_readable: Y Combinator Standard Equity Terms
 *   domain: economic/venture_capital/startup_funding
 *
 * SUMMARY:
 *   Y Combinator's standardized SAFE (Simple Agreement for Future Equity)
 *   terms and equity structure create a constraint on early-stage startups
 *   that exhibits both coordination and extraction. The constraint operates
 *   at the intersection of capital scarcity, information asymmetry, and
 *   venture capital's institutional dominance. Founders seeking growth
 *   capital at scale face a choice between accepting YC's standard terms
 *   (7-9% dilution, valuation cap typically $5-15M) or pursuing alternative
 *   funding sources with higher friction. The structural tension is between
 *   YC's genuine coordination value (network effects, credibility cascade to
 *   later-stage investors, access to peer founders and operational expertise)
 *   and its extractive mechanism (standardized terms capture excess founder
 *   value during early verification period when alternatives are scarce). The
 *   constraint has intensified over the past decade as YC's brand has become
 *   more dominant, founders face greater pressure to raise, and alternative
 *   funding sources (though growing) have not yet achieved parity in terms of
 *   downstream Series A valuations and reputation. The theater ratio (0.48)
 *   reflects moderate performativity: much of the 'YC value' is real
 *   (network, legitimacy), but some is positional (brand advantage that
 *   persists even as alternative funding matures).
 *
 * KEY AGENTS:
 *   - Bootstrapped First-Time Founder: Primary victim (powerless/trapped) — no alternative access to scaled capital; must accept YC's standardized SAFE terms or forgo growth capital
 *   - Y Combinator Fund: Primary beneficiary (institutional/arbitrage) — extracts value through standardized equity terms; benefits from information asymmetry and capital scarcity
 *   - Engineering Team: Secondary victim (organized/constrained) — diluted by founder dilution and option pool structures; benefits from recruitment and network access but faces underwater grants by Series A
 *   - Series A Investors: Secondary beneficiary (powerful/arbitrage) — benefit from YC's valuation caps that lock in discounts relative to true Series A pricing
 *   - Alternative Funding Coalition: Emerging actor (organized/constrained) — rolling funds, syndicates, revenue-based financing, international VCs provide alternative pathways; sunset logic applies as they mature
 *   - Repeat Founder: Peer manipulator (powerful/mobile) — has negotiation leverage and alternative funding access; experiences constraint as extractive but with agency to resist
 *   - Traditional Banks: Institutional excluded actor (institutional/arbitrage) — could provide growth capital at lower dilution but are excluded by venture capital's control structures and equity-based governance preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(y_combinator, 0.52).
domain_priors:suppression_score(y_combinator, 0.68).
domain_priors:theater_ratio(y_combinator, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(y_combinator, extractiveness, 0.52).
narrative_ontology:constraint_metric(y_combinator, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(y_combinator, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(y_combinator, tangled_rope).
narrative_ontology:human_readable(y_combinator, "Y Combinator Standard Equity Terms").
narrative_ontology:topic_domain(y_combinator, "economic/venture_capital/startup_funding").

domain_priors:requires_active_enforcement(y_combinator).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(y_combinator, y_combinator).
narrative_ontology:constraint_beneficiary(y_combinator, early_stage_investors).
narrative_ontology:constraint_victim(y_combinator, founder_dilution).
narrative_ontology:constraint_victim(y_combinator, employee_option_pools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOOTSTRAPPED FOUNDER (SNARE) — No alternative access to capital at scale. Trapped by venture capital's requirement to accept standardized SAFE terms as condition of funding. Accepts 7-9% dilution with valuation cap (typically $5-15M for early YC cohorts) despite creating asymmetric downside. Cannot negotiate terms meaningfully or exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72. Pure extraction from structural position.
constraint_indexing:constraint_classification(y_combinator, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENGINEERING TEAM (TANGLED ROPE) — Benefits from YC's credibility, network access, and recruitment infrastructure (coordination). But also victimized by dilution and option pool structures that reduce long-term ownership. SAFE agreements cascade to employee option pools, often with underwater grants by Series A. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.58. Mixed coordination and extraction.
constraint_indexing:constraint_classification(y_combinator, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: Y COMBINATOR FUND (ROPE) — Operates as pure coordination mechanism from its own perspective: distributes capital to promising founders, creates a cohort structure that facilitates peer learning and investor access, defines standardized terms to reduce transaction costs. YC benefits from brand value and founder network as externality. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary; the constraint is their coordination tool.
constraint_indexing:constraint_classification(y_combinator, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE FUNDING COALITION (SCAFFOLD) — Emerging alternatives (rolling funds, syndicates, revenue-based financing, international VCs) offer parallel pathways that reduce YC's monopolistic position. These alternatives have explicit sunset logic: as founder information improves and capital distribution becomes more granular, YC's standardized SAFE advantage declines. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.17. Low effective extraction because alternative pathways create exit options.
constraint_indexing:constraint_classification(y_combinator, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL BANK LENDING (PITON) — Venture capital as a replacement for debt-based financing was once genuinely innovative (1990s-2000s). Now it's largely performative: venture capital claims to provide risk capital for unproven founders, but in practice funds companies already validated by YC batch selection. The equity-dilution mechanism persists through institutional inertia rather than because it's the only viable path. Theater_ratio=0.48 (moderate); equity is theatrical governance rather than necessary risk-bearing. Banks could provide growth capital at lower dilution but are excluded by venture capital's control structures.
constraint_indexing:constraint_classification(y_combinator, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REPEAT FOUNDER (TANGLED ROPE) — Experienced founders see both coordination (network access, credibility cascade) and extraction (dilution mechanism). But they have mobile exit options: can negotiate slightly better SAFE terms, raise earlier from angels, or bootstrap. Experiences the constraint as asymmetric extraction but with agency. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26. Tangled rope: receives coordination benefits but accepts extraction because alternatives exist at higher founder experience.
constraint_indexing:constraint_classification(y_combinator, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL MARKET VIEW (MOUNTAIN) — From first-principles venture capital economics, early-stage capital is inherently illiquid, asymmetric information is structural, and equity-sharing is a natural law of risk allocation. Some dilution is irreducible. However, the structural data (ε=0.52, suppression=0.68, theater=0.48) contradicts the mountain classification — the engine will compute this as a false summit. The 'natural' equity dilution is actually a contingent institutional arrangement (venture capital's dominance, YC's standardization power, absence of regulatory alternatives to equity).
constraint_indexing:constraint_classification(y_combinator, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(y_combinator_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(y_combinator, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(y_combinator, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(y_combinator, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(y_combinator, TR),
    TR >= 0.70.

:- end_tests(y_combinator_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The SAFE mechanism with valuation caps extracts approximately 7-9% of founder equity at a discount to Series A pricing (typically 30-50% below Series A valuation). This is not extreme extraction (which would be 0.70+), but it is significant. The extractiveness reflects that founders have limited alternatives when seeking $500K-$2M at early stage. The measurement trajectory (0.35 → 0.52 over 10 years) shows increasing extraction as YC's dominance has grown and alternative funding sources have remained less developed than the venture capital ecosystem. Suppression (0.68): High. Barriers to exit include: (1) Information asymmetry — early-stage founders don't know true market valuation of their company or their alternatives; (2) Capital scarcity — access to $500K-$2M in capital for unproven founders is limited outside venture capital; (3) Institutional coordination — Series A investors discount companies without YC pedigree, creating downstream consequences for rejecting YC's terms; (4) Network effects — YC's investor network becomes self-reinforcing (Series A firms bias toward YC founders because they know YC's selection process). Suppression is not total (revenue-based financing exists, bootstrapping is possible), but it is substantial. Theater ratio (0.48): Moderate. The YC network value is genuinely real — alumni founder connections, operational expertise sharing, Series A introductions actually function. But the equity terms themselves are somewhat theatrical: they claim to represent risk-bearing alignment but often function as a sunk-cost extraction mechanism once the founder has accepted YC's terms and lost bargaining power. The theater_ratio increase (0.32 → 0.48) reflects that as YC's brand became dominant, the performative aspect increased — founders accept the terms for the brand and network, not because the equity structure is uniquely fair.
 *
 * PERSPECTIVAL GAP:
 *   The bootstrapped founder sees a Snare: trapped by capital scarcity, forced to accept terms they don't fully understand, with limited ability to exit. YC sees a Rope: pure coordination mechanism that allocates capital efficiently and reduces founder transaction costs. The engineering team sees a Tangled Rope: real network benefits but also real dilution cascades. The repeat founder (experienced operator) sees a different Tangled Rope: can negotiate slightly better terms and has mobile exit options, so the extraction is asymmetric but not total. The alternative funding coalition sees a Scaffold: temporary bottleneck being solved by market competition. The analytical observer sees a Mountain (natural law of early-stage capital risk) but the structural data contradicts this — the dilution rate is not physically inevitable, but rather institutionally contingent. This perspectival gap is the core diagnostic: the same equity terms appear as inevitable risk-sharing (mountain) from a capital-market view, but as extractive power play (snare) from a founder without alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Bootstrapped founder: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction directionality — no alternatives. Y Combinator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary — arbitrage exit means they can redeploy capital elsewhere if YC portfolio is not attractive. Engineering team: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal — they can sometimes negotiate equity terms, can exit to other startups, benefit from YC network effects. Series A investors: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Beneficiary through discounted conversion from SAFE to Series A. Repeat founder: Mixed (powerful/mobile) → d≈0.42, f(d)≈0.42. Moderate extraction because they have alternatives and negotiation leverage. Alternative funding: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction relative to YC because they represent genuine competition.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED (0.52 < 0.70). The constraint exhibits genuine Tangled Rope properties: (1) Coordination function: YC provides network effects, Series A pathway credibility, peer founder learning, operational expertise. This is real coordination, not theater. (2) Asymmetric extraction: SAFE terms capture founder value; valuation caps discount Series A pricing; dilution compounds. This is real extraction. (3) Active enforcement: YC requires SAFE acceptance as a condition of funding and cohort participation; the constraint persists through institutional power, not just contractual design. However, the extraction is not total — founders do receive genuine coordination value, and experienced operators can negotiate or exit. The constraint would become a Snare (pure extraction, ε > 0.70) if: (1) Alternative funding sources did not mature (founders had zero alternatives), OR (2) YC began using equity terms to control founder decisions post-funding (governance through dilution). Current evidence suggests neither has fully occurred. The scaffold perspective (alternative funding coalition with sunset logic) is plausible but not yet confirmed. Mandatrophy remains unresolved because the true ε lies in the ambiguity: if alternative funding matures, ε drops to ~0.35 (pure Rope); if it stagnates, ε rises to ~0.65+ (Snare). The empirical resolution of the three high-confidence omegas (valuation cap sustainability, alternative funding maturation, network value quantification) will resolve the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_cap_sustainability,
    'Does the valuation cap (typically $5-15M for YC early batches) reflect genuine early-stage risk or extract founder value through post-hoc repricing?',
    'Historical analysis of Series A repricing relative to initial SAFE caps; comparison of ultimate founder ownership percentages vs equivalent risk profiles in alternative funding structures (revenue-based, debt, equity crowdfunding)',
    'If justified: dilution reflects real risk-bearing (Rope). If extractive: SAFE terms systematically undervalue early-stage potential (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(valuation_cap_sustainability, empirical, 'Whether valuation caps reflect actual risk or extract value through repricing').

omega_variable(
    alternative_funding_maturation,
    'Will revenue-based financing, syndicates, and international VCs mature fast enough to create genuine alternative pathways for founders, or will YC''s standardization and brand remain dominant?',
    'Tracking founder awareness of alternatives, adoption rates of non-YC funding by comparable-quality startups, Series A valuations and dilution rates for YC vs non-YC cohorts',
    'If alternatives mature: scaffold classification confirmed, sunset is real. If YC remains dominant: bottleneck becomes more snare-like, limiting founder optionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_maturation, empirical, 'Whether alternative funding sources will mature to offer genuine competition').

omega_variable(
    network_value_quantification,
    'How much of YC''s value is coordination (network, credibility cascade) versus extraction (brand rent-seeking on standardized terms)?',
    'Cohort analysis: founders with pre-existing networks (serial entrepreneurs, Stanford affiliates) vs founders without; comparison of downstream outcomes (Series A valuations, exit valuations, survival rates) controlling for founder quality',
    'If coordination dominates: constraint is Rope with some extraction (Tangled Rope). If extraction dominates: constraint is Snare with facade of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_value_quantification, empirical, 'Ratio of genuine coordination value to rent-seeking extraction in YC network').

omega_variable(
    dilution_irreversibility,
    'Once founders accept SAFE dilution at early stage, how much of the resulting wealth concentration (to YC, early investors) becomes locked into institutional structures that prevent founder recovery?',
    'Follow-on funding analysis: percentage of startups that recover founder ownership through later fundraising vs those whose dilution compounds; comparison of founder wealth at exit for equivalent-outcome companies with different SAFE histories',
    'If recoverable: dilution is temporary coordination cost (Scaffold with sunset). If locked: dilution becomes permanent extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dilution_irreversibility, empirical, 'Whether early-stage dilution becomes locked or recoverable through later funding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(y_combinator, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yc_equity_tr_t0, y_combinator, theater_ratio, 0, 0.32).
narrative_ontology:measurement(yc_equity_tr_t5, y_combinator, theater_ratio, 5, 0.4).
narrative_ontology:measurement(yc_equity_tr_t10, y_combinator, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(yc_equity_be_t0, y_combinator, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(yc_equity_be_t5, y_combinator, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(yc_equity_be_t10, y_combinator, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(y_combinator, resource_allocation).
narrative_ontology:affects_constraint(y_combinator, series_a_dilution_cascade).
narrative_ontology:affects_constraint(y_combinator, founder_option_pool_underwater_grants).
narrative_ontology:affects_constraint(y_combinator, venture_capital_standardization).

% DUAL FORMULATION NOTE:
% Y Combinator equity terms decompose into three related constraints: (1) yc_equity_squeeze (this story): the initial SAFE dilution and valuation cap mechanism, ε=0.52, Tangled Rope at baseline. (2) series_a_dilution_cascade (downstream): the downstream dilution from follow-on rounds compounding the initial SAFE discount, ε=0.58, Snare from founder perspective. (3) founder_option_pool_underwater_grants (collateral): the employee option pool structures that cascade YC dilution to engineering teams, ε=0.48, Tangled Rope from employee perspective. These are distinct constraints (different ε, different structural targets) but causally linked: YC's SAFE structure creates the conditions for Series A dilution, which creates the conditions for underwater option pools. All three belong to the venture_capital_standardization family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(y_combinator, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
