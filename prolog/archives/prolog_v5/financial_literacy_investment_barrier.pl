% ============================================================================
% CONSTRAINT STORY: financial_literacy_investment_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_literacy_investment_barrier, []).

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
 *   constraint_id: financial_literacy_investment_barrier
 *   human_readable: Financial Literacy Investment Barrier
 *   domain: economics/financial_access
 *
 * SUMMARY:
 *   The financial literacy investment barrier constrains wealth-building
 *   access for populations lacking formal financial education. Low-income
 *   households, first-generation investors, and excluded communities face
 *   knowledge requirements that are partly genuine (understanding risk,
 *   diversification, compounding) and partly artificial gatekeeping
 *   (complexity of terminology, licensing requirements, platform barriers).
 *   This creates a hybrid coordination-extraction constraint: markets require
 *   some minimum literacy to function efficiently (Rope coordination), but
 *   the actual literacy requirement is inflated by industry interests in
 *   maintaining information asymmetry and professional scarcity (Snare
 *   extraction). The constraint exhibits all six classification types
 *   depending on perspective. Technological disruption (robo-advisors,
 *   fractional shares, simplified platforms) and regulatory pressure
 *   (fiduciary standards, plain-language requirements) are reducing barriers,
 *   creating a genuine sunset clause for the most extractive elements.
 *
 * KEY AGENTS:
 *   - Low-income households and retail investors: Primary victims (powerless/trapped) — face systemic exclusion from wealth-building mechanisms due to literacy barriers and capital minimums
 *   - Financial services industry: Primary beneficiary (institutional/arbitrage) — captures extraction through complexity maintenance, advisory fees, and information asymmetry
 *   - Self-taught investors: Secondary actor (moderate/constrained) — can gradually accumulate literacy but face high opportunity costs and time barriers
 *   - Financial inclusion movement: Organized force (organized/constrained) — nonprofits, fintechs, regulators actively building accessibility infrastructure
 *   - Traditional advisory establishment: Institutional defender (institutional/arbitrage) — maintains gatekeeping through licensing, jargon, and complexity rituals
 *   - Analytical observer: Civilizational context (analytical/analytical) — risks naturalizing constructed barriers as inherent economic properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_literacy_investment_barrier, 0.58).
domain_priors:suppression_score(financial_literacy_investment_barrier, 0.65).
domain_priors:theater_ratio(financial_literacy_investment_barrier, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_literacy_investment_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_literacy_investment_barrier, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(financial_literacy_investment_barrier, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_literacy_investment_barrier, tangled_rope).
narrative_ontology:human_readable(financial_literacy_investment_barrier, "Financial Literacy Investment Barrier").
narrative_ontology:topic_domain(financial_literacy_investment_barrier, "economics/financial_access").

domain_priors:requires_active_enforcement(financial_literacy_investment_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_literacy_investment_barrier, financial_services_industry).
narrative_ontology:constraint_beneficiary(financial_literacy_investment_barrier, wealth_advisors).
narrative_ontology:constraint_beneficiary(financial_literacy_investment_barrier, institutional_investors).
narrative_ontology:constraint_victim(financial_literacy_investment_barrier, low_income_households).
narrative_ontology:constraint_victim(financial_literacy_investment_barrier, retail_investors).
narrative_ontology:constraint_victim(financial_literacy_investment_barrier, financial_inclusion_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED RETAIL INVESTOR (SNARE) — Trapped by knowledge barriers, terminology complexity, and information asymmetry. Cannot access investment opportunities or build wealth without literacy. No exit path available within their socioeconomic context. Maximum experienced extraction as they bear full cost of exclusion while institutional actors capture wealth-building returns.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELF-TAUGHT INVESTOR (TANGLED ROPE) — Can accumulate literacy through YouTube, online courses, and community learning, but faces high time and opportunity costs. Genuine coordination function exists (markets require some level of retail participation for liquidity), but extraction persists through complexity gatekeeping. Educational resources exist but are partially obfuscated by industry jargon.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SERVICES INSTITUTION (ROPE) — Experiences literacy barrier as coordination mechanism: standardized terminology enables market functioning and risk management. Benefits from first-mover advantage and network effects in wealth management. Can arbitrage between different markets and regulatory regimes. Net beneficiary position through complexity that creates professional necessity.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL INCLUSION MOVEMENT (SCAFFOLD) — Organized efforts (fintech democratization, financial wellness programs, community banking initiatives) view literacy barriers as temporary problems with sunset logic. Distributed financial education and simplified investment platforms (robo-advisors, index funds, fractional shares) are reducing barriers. Active enforcement (regulation requiring plain-language disclosures, fiduciary standards) creates pressure toward accessibility. Sunset clause embedded in technological and regulatory trend.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL FINANCE GATEKEEPING (PITON) — Financial terminology and licensing requirements (Series 7, CFP certifications) persist partially through institutional inertia. These serve real regulatory and risk functions but are also performative gatekeeping that excludes talent and creates artificial scarcity. Advisors are increasingly acknowledging that simplification (index funds, fee-only models) produces better client outcomes, yet the complexity ritual persists due to industry structure and profit incentives from information asymmetry.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some investment literacy requirement is inherent to market participation: economic principles, risk-return tradeoffs, and compound growth mechanics are genuine knowledge requirements. This perspective risks naturalizing what is partly a structural artifact of institutional design. The engine's false summit detector will identify this as naturalization of contingent complexity.
constraint_indexing:constraint_classification(financial_literacy_investment_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_literacy_investment_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_literacy_investment_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_literacy_investment_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_literacy_investment_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_literacy_investment_barrier, TR),
    TR >= 0.70.

:- end_tests(financial_literacy_investment_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts in multiple dimensions: (1) wealth-building exclusion prevents accumulated assets during prime earning years, compounding into lifetime wealth gaps; (2) exploitation through predatory financial products targeting the non-literate (payday loans, subprime mortgages, high-fee investment products); (3) opportunity cost as time spent on literacy acquisition diverts from productive work. The value reflects both genuine coordination costs (literacy genuinely required) and extractive gatekeeping (excess complexity beyond necessity). Suppression (0.65): Significant. Barriers include: capital minimums ($100+ to start with traditional brokers), access costs (premium advisory fees), time requirements (literacy acquisition burden falls on victims), learning costs (educational resources vary in quality), and psychological barriers (belief in personal incapacity). The measurement increased from 0.55 to 0.65 over the interval due to rising complexity as financial products proliferate faster than retail education. Theater ratio (0.48): Moderate. Some gatekeeping is performative (licensing exams measure compliance more than competency; complexity language serves exclusion rather than risk management), but genuine risk-management coordination exists. The slight decline over the interval reflects fintech platforms reducing performative barriers while regulatory scrutiny increases demand for actual transparency.
 *
 * PERSPECTIVAL GAP:
 *   The gap between victim (Snare) and beneficiary (Rope) perspectives reveals the extractive-coordinative hybrid. The powerless retail investor sees a barrier that prevents wealth-building access — pure extraction with no escape route. The financial services institution sees the same barrier as a market-making coordination mechanism — knowledge requirements that enable efficient pricing and risk management. The self-taught investor (Tangled Rope) has found a middle path but still bears disproportionate time costs. The fintech disruption (Scaffold) is reducing barriers through technology (robo-advisors, index funds) and regulation (fiduciary standards), validating the sunset logic. The traditional advisory establishment (Piton) sees its gatekeeping role as degraded — they acknowledge that simplified index funds often outperform managed accounts, yet maintain complex advisory structures through institutional inertia. The civilizational analytical observer (Mountain) risks naturalizing this as inherent economic law, when it is substantially an artifact of institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies sharply by agent type. Powerless/trapped retail investors: high d (0.90+), maximum experienced extraction through f(d), because they are trapped by knowledge requirements with no arbitrage option. Moderate/constrained self-taught investors: medium d (0.55-0.65), partial extraction because they can accumulate literacy but face high opportunity costs. Institutional financial services: low d (0.05-0.15), net beneficiary through arbitrage options (they can operate in multiple regulatory and market contexts), capturing extraction benefits. Organized financial inclusion actors: medium d (0.50-0.60), constrained ability to fully resolve the barrier but organized enough to reduce it. The Piton perspective's d reflects that the gatekeeping ritual itself is partially internalized by institutions that have 'become' complexity maintenance. The Mountain perspective's analytical d (0.72) should be examined for false summit: if literacy requirement is genuinely natural law, barrier persists even in optimized systems; if partly constructed, it can be materially reduced.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the literacy barrier into genuine coordination (markets require some financial knowledge to function) and extractive gatekeeping (industry inflates requirements and maintains artificial scarcity). The Tangled Rope classification correctly identifies both functions coexisting. The false summit (Mountain perspective) illustrates why indexical classification matters: a civilization-scale observer might see literacy requirements as natural and immutable, naturalizing what is partly a designed barrier. The Scaffold perspective with sunset logic validates that the extractive elements are contingent on current institutional arrangements — technology and regulation can reduce them. The Piton perspective reveals that the gatekeeping ritual itself is being challenged by within-institution actors (advisors, platforms) who recognize that simplification serves clients better. The constraint's mandatrophy is resolved by showing that all six types coexist as legitimate readings, each capturing genuine structural features from its observational position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_requirement_vs_gatekeeping,
    'What portion of the literacy barrier reflects genuine knowledge requirements versus deliberate complexity for gatekeeping?',
    'Comparative analysis of investor outcomes with minimal literacy (index fund investors) versus complex literacy (active traders); correlation between advice complexity and client returns; international regulatory sandbox data on simplified investment frameworks',
    'If gatekeeping dominates: extractiveness should be 0.70+, classification shifts toward Snare. If literacy genuinely required: extractiveness justified at current 0.58, Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_requirement_vs_gatekeeping, empirical, 'Portion of literacy barrier that is genuine versus gatekeeping').

omega_variable(
    fintech_disruption_timeline,
    'Will technology platforms reduce literacy barriers to sub-critical levels within 10-15 years, validating the Scaffold sunset logic?',
    'Trend analysis of robo-advisor adoption, fractional share platforms, and AI-driven financial guidance; measurement of wealth-building outcomes for users of simplified platforms versus traditional advisory; regulatory evolution toward standardized disclosures and simplified products',
    'If sunset confirmed: Scaffold classification is structural rather than aspirational. If technology fails to significantly reduce barriers: sunset is hypothetical, and constraint persists as Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fintech_disruption_timeline, empirical, 'Timeline for technological reduction of literacy barriers').

omega_variable(
    suppression_mechanism_internalization,
    'To what degree is the literacy barrier internalized (belief in personal incapacity, learned helplessness) versus structural (access, time, cost barriers)?',
    'Post-intervention measurement: tracking belief change and participation rates when structural barriers are removed (free access to education, simplified platforms, financial incentives); psychological assessment of financial self-efficacy before and after literacy intervention; comparison of participation rates across income groups with equivalent access',
    'If heavily internalized: suppression persists even after structural barriers removed; constraint is more resistant than metrics suggest. If primarily structural: removing barriers (regulation, platforms, education) reduces suppression measurably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree to which suppression is internalized versus structural').

omega_variable(
    regulatory_enforcement_trajectory,
    'Will regulatory enforcement (plain-language requirements, fiduciary standards, disclosure mandates) become effective enough to shift the constraint from extractive to coordinative?',
    'Tracking regulatory compliance and effectiveness; measurement of investor protection outcomes under different regulatory regimes; analysis of whether plain-language disclosures actually reduce information asymmetry or merely shift complexity',
    'If enforcement effective: suppression drops, extractiveness reduces, constraint moves toward Rope. If enforcement is performative: theater_ratio increases, classification degrades toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_trajectory, empirical, 'Effectiveness of regulatory enforcement on reducing extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_literacy_investment_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finlit_tr_t0, financial_literacy_investment_barrier, theater_ratio, 0, 0.55).
narrative_ontology:measurement(finlit_tr_t5, financial_literacy_investment_barrier, theater_ratio, 5, 0.48).
narrative_ontology:measurement(finlit_tr_t10, financial_literacy_investment_barrier, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(finlit_be_t0, financial_literacy_investment_barrier, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(finlit_be_t5, financial_literacy_investment_barrier, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(finlit_be_t10, financial_literacy_investment_barrier, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_literacy_investment_barrier, information_standard).
narrative_ontology:affects_constraint(financial_literacy_investment_barrier, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(financial_literacy_investment_barrier, retail_investor_exploitation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_literacy_investment_barrier, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
