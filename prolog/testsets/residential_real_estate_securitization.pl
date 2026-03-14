% ============================================================================
% CONSTRAINT STORY: residential_real_estate_securitization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_residential_real_estate_securitization, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: residential_real_estate_securitization
 *   human_readable: Residential Real Estate Securitization as Extractive Constraint
 *   domain: financial_systems/housing_markets
 *
 * SUMMARY:
 *   Residential real estate securitization emerged in the 1980s as a
 *   coordination mechanism to distribute housing finance risk across capital
 *   markets, enabling portfolio diversification for institutional investors
 *   and expanding credit access for homebuyers. By the 2000s, it had
 *   transformed into a pure extraction mechanism targeting subprime borrowers
 *   through predatory origination, opaque risk transfer, and information
 *   asymmetry between borrowers and investors. The 2008 financial crisis
 *   revealed the system's extractive core: originators had no skin in the
 *   game, rating agencies had misaligned incentives, investors had no
 *   transparency, and borrowers had no exit. Post-Dodd-Frank regulations
 *   imposed compliance theater (standardized documentation, qualified
 *   mortgage rules, skin-in-the-game requirements) without addressing the
 *   structural extractive mechanisms, which adapted into private-label
 *   securitization and alternative lending channels. The constraint exhibits
 *   all six classification types from different structural positions:
 *   powerless subprime borrowers experience it as a snare; working-class
 *   homebuyers experience tangled rope (coordination benefit + extraction);
 *   originators and investment banks experience rope (pure coordination
 *   benefit); institutional investors experience tangled rope (benefit +
 *   information trap); the regulatory system has become a piton (performative
 *   theater); and the analytical observer risks false natural law (treating
 *   information asymmetry as immutable rather than contingent).
 *
 * KEY AGENTS:
 *   - Subprime Borrowers: Primary victims (powerless/trapped) — bear full extraction through predatory loan terms, hidden fees, and absence of alternatives
 *   - Working-Class Homebuyers: Secondary victims (moderate/constrained) — benefit from homeownership access but face extraction through higher rates and aggressive servicing
 *   - Mortgage Originators: Primary beneficiaries (institutional/arbitrage) — capture origination fees, underwriting spreads, and trading profits; minimal risk exposure
 *   - Investment Banks: Primary beneficiaries (institutional/arbitrage) — profit from structuring, distributing, and trading securitized mortgages; insulated from default risk
 *   - Institutional Investors: Moderate beneficiaries with information trap (powerful/mobile) — capture yield pickup but trapped in information asymmetry; suppressed by rating agency conflicts
 *   - Rating Agencies: Beneficiary accomplices (institutional/arbitrage) — profit from rating fees; incentives misaligned with investor interests; enforce information opacity
 *   - Neighborhoods: Collective victims (powerless/trapped) — concentrated subprime lending causes systematic extraction through default cascades and disinvestment
 *   - Regulatory Agencies: Institutional theater (institutional/arbitrage) — maintain performative oversight while extractive mechanisms persist through regulatory arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(residential_real_estate_securitization, 0.68).
domain_priors:suppression_score(residential_real_estate_securitization, 0.72).
domain_priors:theater_ratio(residential_real_estate_securitization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(residential_real_estate_securitization, extractiveness, 0.68).
narrative_ontology:constraint_metric(residential_real_estate_securitization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(residential_real_estate_securitization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(residential_real_estate_securitization, snare).
narrative_ontology:human_readable(residential_real_estate_securitization, "Residential Real Estate Securitization as Extractive Constraint").
narrative_ontology:topic_domain(residential_real_estate_securitization, "financial_systems/housing_markets").

domain_priors:requires_active_enforcement(residential_real_estate_securitization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(residential_real_estate_securitization, mortgage_originators).
narrative_ontology:constraint_beneficiary(residential_real_estate_securitization, investment_banks).
narrative_ontology:constraint_beneficiary(residential_real_estate_securitization, rating_agencies).
narrative_ontology:constraint_beneficiary(residential_real_estate_securitization, institutional_investors).
narrative_ontology:constraint_victim(residential_real_estate_securitization, subprime_borrowers).
narrative_ontology:constraint_victim(residential_real_estate_securitization, working_class_homebuyers).
narrative_ontology:constraint_victim(residential_real_estate_securitization, neighborhoods_subject_to_predatory_lending).
narrative_ontology:constraint_victim(residential_real_estate_securitization, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBPRIME BORROWER (SNARE) — Trapped by information asymmetry, predatory loan terms, and the absence of alternative financing pathways. Bears full extraction cost through high interest rates, balloon payments, and hidden fees. No material exit option exists; defaulting means homelessness. Maximum experienced extraction with high suppression.
constraint_indexing:constraint_classification(residential_real_estate_securitization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS HOMEBUYER (TANGLED ROPE) — Experiences genuine coordination benefit (access to homeownership, wealth accumulation potential) alongside asymmetric extraction (higher interest rates than prime borrowers, aggressive servicing practices). Constrained exit: can walk away through foreclosure, but at catastrophic cost to credit and family stability. Suppression operates through debt dependency and social pressure to maintain homeownership.
constraint_indexing:constraint_classification(residential_real_estate_securitization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MORTGAGE ORIGINATOR AND INVESTMENT BANK (ROPE) — Experiences the securitization system as pure coordination: originating loans, bundling them into securities, selling to institutional investors, and distributing risk through the financial system. Net beneficiary through origination fees, underwriting spreads, and trading profits. The system coordinates capital flow toward housing; the beneficiary captures asymmetric returns. From their perspective, suppression is minimal — they have full agency and clear exit options through market arbitrage.
constraint_indexing:constraint_classification(residential_real_estate_securitization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEIGHBORHOOD COMMUNITY CAPITAL (SNARE) — Concentrated subprime lending in specific neighborhoods creates systematic extraction: predatory origination -> default -> foreclosure -> neighborhood destabilization -> depressed property values -> accelerated disinvestment. The community bears extraction costs (lost tax base, school funding collapse, social capital erosion) without benefit. No exit mechanism; neighborhoods cannot relocate.
constraint_indexing:constraint_classification(residential_real_estate_securitization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTORS / ASSET MANAGERS (TANGLED ROPE) — Benefit from securitization (portfolio diversification, yield pickup over Treasuries, access to illiquid residential mortgages). But also trapped in information asymmetry: cannot verify loan quality, must rely on rating agencies and originators' representations. High suppression through rating agency capture and opaque securitization documentation. Mobile exit in principle (can move to other asset classes), but constrained by client mandates and yield pressure. Experienced extraction arises from rating agency conflicts of interest and information opacity.
constraint_indexing:constraint_classification(residential_real_estate_securitization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Post-2010 regulations (Dodd-Frank, qualified mortgages, skin-in-the-game requirements) are largely performative theater: originators have adapted to compliance requirements while maintaining extractive practices through alternative channels (private label securitization, non-QM lending, rate-and-term refinancing traps). The regulatory apparatus persists through institutional inertia despite degraded function. Theater ratio high because compliance metrics (standardized documentation, loan-to-value ratios, debt-to-income caps) substitute for substantive verification of borrower capacity or fraud prevention. Theater_ratio = 0.65 reflects this: regulatory bodies maintain the appearance of oversight while extractive mechanisms persist.
constraint_indexing:constraint_classification(residential_real_estate_securitization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW — From a civilizational view, information asymmetry in housing markets might appear immutable: lenders always know more than borrowers, valuation is inherently subjective, default risk cannot be fully modeled. This perspective risks naturalizing what are contingent institutional choices: the choice to allow non-recourse mortgages, the choice to permit securitization without originator liability, the choice to delegate underwriting standards to rating agencies. The engine's false summit detector will flag this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(residential_real_estate_securitization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(residential_real_estate_securitization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(residential_real_estate_securitization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(residential_real_estate_securitization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(residential_real_estate_securitization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(residential_real_estate_securitization, TR),
    TR >= 0.70.

:- end_tests(residential_real_estate_securitization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial value from borrowers through above-market interest rates, hidden fees, and loan terms optimized for default rather than performance. Originators extract 2-3% origination fees plus servicing rights; investment banks extract structuring and trading spreads; rating agencies extract rating fees. The trajectory shows extractiveness rising from 0.35 (2000) to 0.68 (2008) as origination standards degraded and high-risk lending concentrated in securitized vehicles. Post-2008 regulations reduced extractiveness slightly to 0.62 (2015) through loan-level transparency and originator liability, but private-label securitization and alternative lending channels have enabled extraction to persist. Suppression (0.72): High. Barriers to exit include: information asymmetry (borrowers cannot evaluate loan terms; investors cannot verify loan quality), debt dependency (default means homelessness), credit market concentration (few alternative lenders), regulatory complexity (understands loan documentation requires expert counsel), and social pressure (homeownership is culturally mandated). Refinancing is suppressed by prepayment penalties and rate lock-in. Theater ratio (0.65): Moderate-high. Post-Dodd-Frank compliance metrics (QM standards, debt-to-income caps, documentation requirements) create the appearance of prudent underwriting while extractive mechanisms persist. Private-label securitization and non-QM lending operate outside the compliance theater. Rating agencies maintain the theatrical apparatus of credit analysis despite well-documented misalignment of incentives. The theater has increased over time as regulatory complexity has deepened without reducing extractive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The most severe perspectival gap exists between the subprime borrower (snare: trapped, powerless, maximum extraction) and the mortgage originator (rope: beneficiary, institutional, arbitrage). The originator experiences the system as coordination — solving the problem of matching borrowers with lenders — and capturing legitimate first-mover returns. The borrower experiences the same system as pure extraction: predatory underwriting, hidden risk transfer, and absence of alternatives. The institutional investor occupies the tangled rope middle: they benefit from yield pickup (coordination function) but are trapped in information asymmetry created by rating agency conflicts and securitization opacity. The working-class homebuyer also experiences tangled rope: genuine benefit (access to homeownership) alongside extraction (higher rates, aggressive servicing). The neighborhood-level perspective reveals a fourth manifestation: distributed extraction concentrated on specific geographic areas through predatory origination targeting low-income communities. The regulatory perspective (piton) shows that post-2008 reforms created an apparatus of compliance theater without eliminating the extraction mechanism. The analytical observer risks naturalizing the information asymmetry as inherent to housing markets rather than recognizing it as a contingent institutional choice to privatize gains and socialize losses.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position: who benefits and who bears costs. Subprime borrowers have d ≈ 0.95 (full target): they receive predatory loans at above-market rates and bear default risk. Originating institutions have d ≈ 0.10 (near-full beneficiary): they capture origination fees, underwriting spreads, and servicing rights with minimal risk. Institutional investors have d ≈ 0.60 (moderate target): they benefit from yield but are trapped in information asymmetry. The engine's sigmoid function f(d) amplifies the experienced extraction for high-d agents (powerless borrowers with d=0.95 experience f(d)≈1.42x base extraction). Low-d agents (institutional beneficiaries with d=0.10 experience f(d)≈-0.01x, meaning they experience negative effective extraction — benefit flows toward them). The scope modifier σ(S) for national scope (σ=1.0) does not scale the effective extraction, but the base extractiveness of 0.68 already reflects the system's severity. Geographic concentration in specific neighborhoods would justify a local scope modifier (σ=0.8) at the neighborhood level, which would reduce apparent but increase actual extraction (the suppression mechanism is localized), but the national-scope perspective captures the systemic view.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION VALIDATES: Base extraction ε=0.68 > 0.46 threshold (snare minimum). Suppression σ=0.72 > 0.60 threshold. Effective extraction χ calculated from powerless/trapped perspective: χ = 0.68 × f(0.95) × σ(national) ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (well above χ ≥ 0.66 snare threshold). The mandatrophy is resolved by the perspectival diversity: from the beneficiary perspective, the system appears as rope (coordination benefit with asymmetric returns). From the victim perspective, it appears as snare (pure extraction with high suppression). The tangled rope perspectives (working-class homebuyers, institutional investors) reveal the hybrid: genuine coordination benefits (access to capital, risk distribution) co-existing with extraction mechanisms. The piton perspective (regulatory apparatus) shows how post-crisis reforms created theater without eliminating extraction. The false natural law perspective risks treating information asymmetry as inevitable rather than institutional. The mandatrophy is resolved by recognizing that securitization DOES coordinate capital flow (rope benefit) AND DOES extract from subprime borrowers (snare harm) in the same transaction — the constraint is genuinely tangled rope at the system level, but appears as snare from the borrower's trapped perspective and rope from the beneficiary's institutional perspective. The extractiveness gradient (0.35 → 0.68 → 0.62) shows the system was designed for coordination, corrupted into extraction, and partially reformed but not cured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originator_liability_mechanism,
    'If originators retained full securitization risk (no put-back rights cap), would subprime lending volumes collapse or merely shift to different borrower segments?',
    'Counterfactual simulation of origination volumes under full liability; historical analysis of pre-2007 recourse mortgage markets; comparison with international markets with originator liability',
    'If collapse: securitization is extraction mechanism dependent on liability shields. If shift: extraction persists through different mechanisms. Classification likely remains snare in both cases, but intensity moderates if volumes crash.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originator_liability_mechanism, empirical, 'Whether originator liability would eliminate or relocate the extractive mechanism').

omega_variable(
    rating_agency_independence_sufficiency,
    'Can rating agency incentive reforms (investor-pay model, liability exposure, algorithmic transparency) eliminate information asymmetry, or is the asymmetry structural to mortgage complexity?',
    'Comparison of rating accuracy post-Dodd-Frank reforms vs pre-2008; analysis of whether rating errors are driven by misaligned incentives or fundamental valuation difficulty; stress-test performance across rating agencies with different fee structures',
    'If incentive-driven: reforms could shift classification toward rope (coordination with transparent pricing). If structural: classification remains snare despite regulatory reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rating_agency_independence_sufficiency, empirical, 'Whether rating agency reform can eliminate information asymmetry').

omega_variable(
    subprime_expansion_coordination_vs_predation,
    'Did securitization enable access to credit for previously excluded populations (coordination benefit) or primarily concentrate predatory lending on vulnerable borrowers (extraction)?',
    'Historical comparison of subprime borrower outcomes with and without securitization; analysis of borrower sophistication and loan terms by origination period; counterfactual: what credit access would exist without securitization infrastructure',
    'If coordination-dominant: classification shifts toward tangled rope across more perspectives. If predation-dominant: confirms snare classification and implies benefits were concentrated among beneficiaries, not borrowers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subprime_expansion_coordination_vs_predation, empirical, 'Whether securitization expansion primarily enabled access or enabled predation').

omega_variable(
    systemic_risk_externality_internalization,
    'Are systemic risk costs (financial crisis, recession spillovers, government bailouts) properly internalized in pricing and risk allocation, or do they represent unpriced externalities captured by borrowers and taxpayers?',
    'Cost accounting of 2008 financial crisis (foreclosures, unemployment, government transfers); comparison of securitization benefits to originators vs systemic costs; analysis of whether risk concentration in mortgage securities was reflected in pricing',
    'If externalized: validates snare classification at macro scale — system operates profitably for beneficiaries while costs are socialized. If internalized: suggests more efficient market pricing, though distributional extraction remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_risk_externality_internalization, empirical, 'Whether systemic risk costs are internalized in securitization pricing').

omega_variable(
    borrower_sophistication_heterogeneity,
    'Are subprime borrowers behaviorally incapable of comprehending mortgage terms (true information asymmetry trap) or making rational decisions despite comprehension?',
    'Analysis of loan shopping behavior, refinancing decisions, complaint patterns; comparison of borrower outcomes for identical loan types across cognitive ability proxies; behavioral testing of financial literacy vs actual decision quality',
    'If incapable: suppression mechanism is cognitive; exit requires education/transparency. If rational despite information gaps: suppression is structural (no alternatives exist); exit requires market design changes. Both scenarios sustain snare classification but point to different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrower_sophistication_heterogeneity, empirical, 'Whether borrower suppression is cognitive or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(residential_real_estate_securitization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rres_theater_2000, residential_real_estate_securitization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rres_theater_2005, residential_real_estate_securitization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(rres_theater_2008, residential_real_estate_securitization, theater_ratio, 8, 0.55).
narrative_ontology:measurement(rres_theater_2015, residential_real_estate_securitization, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(rres_extractiveness_2000, residential_real_estate_securitization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rres_extractiveness_2005, residential_real_estate_securitization, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(rres_extractiveness_2008, residential_real_estate_securitization, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(rres_extractiveness_2015, residential_real_estate_securitization, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(residential_real_estate_securitization, resource_allocation).
narrative_ontology:boltzmann_floor_override(residential_real_estate_securitization, 0.12).
narrative_ontology:affects_constraint(residential_real_estate_securitization, subprime_credit_expansion).
narrative_ontology:affects_constraint(residential_real_estate_securitization, rating_agency_capture).
narrative_ontology:affects_constraint(residential_real_estate_securitization, housing_market_destabilization).
narrative_ontology:affects_constraint(residential_real_estate_securitization, financial_system_systemic_risk).

% DUAL FORMULATION NOTE:
% Residential real estate securitization decomposes into multiple constraint stories: (1) securitization_as_coordination (ε≈0.15, Rope) — the legitimate capital-distribution mechanism; (2) securitization_predatory_origination (ε≈0.72, Snare) — targeting subprime borrowers; (3) rating_agency_information_opacity (ε≈0.58, Tangled Rope) — misaligned incentives creating investor information trap; (4) regulatory_compliance_theater (ε≈0.42, Piton) — post-Dodd-Frank performative oversight. The present story integrates these into a single snare classification at the system level, reflecting the dominant structural outcome post-2000. Each sub-story can be authored separately with different ε values and beneficiary/victim declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(residential_real_estate_securitization, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
