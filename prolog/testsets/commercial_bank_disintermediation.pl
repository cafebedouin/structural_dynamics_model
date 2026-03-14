% ============================================================================
% CONSTRAINT STORY: commercial_bank_disintermediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commercial_bank_disintermediation, []).

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
 *   constraint_id: commercial_bank_disintermediation
 *   human_readable: Commercial Bank Disintermediation Constraint
 *   domain: financial_economics/banking_structure
 *
 * SUMMARY:
 *   Commercial bank disintermediation — the process by which large
 *   corporations and institutional investors access capital directly through
 *   financial markets rather than via bank lending — creates a structural
 *   constraint that exhibits simultaneous coordination and extraction. Banks
 *   coordinate the matching of depositors and borrowers, assess credit risk,
 *   manage liquidity, and distribute systemic risk. As information technology
 *   and regulatory changes reduce these coordination barriers,
 *   disintermediation extracts value from retail depositors (who face
 *   declining returns on deposits) and small businesses (who lose access to
 *   relationship lending). The constraint is neither pure coordination nor
 *   pure extraction: it coordinates capital formation for institutional
 *   actors while extracting from retail actors, sustained by regulatory
 *   structures (deposit insurance, capital requirements, Fed backstop) that
 *   maintain bank viability despite declining functional necessity.
 *   Extractiveness has increased from 0.28 to 0.52 over the 20-year interval,
 *   while theater ratio has declined from 0.42 to 0.35, indicating that the
 *   real coordination function has diminished while extraction has increased
 *   — consistent with Goodhart-drift dynamics and regulatory capture.
 *
 * KEY AGENTS:
 *   - Retail Depositors: Primary victim (powerless/trapped) — earn below-market returns in constrained deposit market while banks shift profitable lending to capital markets
 *   - Small Business Borrowers: Secondary victim (moderate/constrained) — lose access to relationship lending as banks disintermediate; face rising spreads and rationing
 *   - Large Corporations and Institutional Investors: Primary beneficiary (institutional/arbitrage) — access efficient capital markets, bypass intermediation spread, obtain competitive pricing
 *   - Commercial Banks: Institutional actor (institutional/arbitrage) — maintain presence through regulatory protection and deposit insurance; core function atrophied
 *   - Fintech and Alternative Lenders: Organized actors (organized/constrained) — capture market share and growth but face mounting regulatory harmonization pressures
 *   - Regulatory Authority / Central Bank: Organized actor (organized/constrained) — maintains system through mandated coordination; views disintermediation as temporary challenge
 *   - Banking System Stability: Collective victim (institutional/constrained) — bears systemic cost through procyclicality amplification and funding fragility during stress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commercial_bank_disintermediation, 0.52).
domain_priors:suppression_score(commercial_bank_disintermediation, 0.48).
domain_priors:theater_ratio(commercial_bank_disintermediation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commercial_bank_disintermediation, extractiveness, 0.52).
narrative_ontology:constraint_metric(commercial_bank_disintermediation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commercial_bank_disintermediation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commercial_bank_disintermediation, tangled_rope).
narrative_ontology:human_readable(commercial_bank_disintermediation, "Commercial Bank Disintermediation Constraint").
narrative_ontology:topic_domain(commercial_bank_disintermediation, "financial_economics/banking_structure").

domain_priors:requires_active_enforcement(commercial_bank_disintermediation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commercial_bank_disintermediation, large_corporations).
narrative_ontology:constraint_beneficiary(commercial_bank_disintermediation, institutional_investors).
narrative_ontology:constraint_beneficiary(commercial_bank_disintermediation, fintech_platforms).
narrative_ontology:constraint_victim(commercial_bank_disintermediation, retail_depositors).
narrative_ontology:constraint_victim(commercial_bank_disintermediation, small_business_borrowers).
narrative_ontology:constraint_victim(commercial_bank_disintermediation, banking_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL DEPOSITOR (SNARE) — Trapped in low-yield deposit environment as banks redirect profitable lending to large corporations via capital markets. Cannot exit to direct lending markets (requires credit expertise, regulatory licensing, capital reserves). Faces forced subsidy of bank operations through below-market deposit rates while banks extract via disintermediation spread.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS BORROWER (TANGLED ROPE) — Benefits from bank-provided coordination of credit assessment and capital access. Simultaneously bears extraction through rising spreads and rationing as banks prioritize large corporate clients. High switching costs to alternative lenders; constrained by creditworthiness requirements and lack of direct capital market access. Coordination function real but increasingly asymmetric.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE CORPORATION / INSTITUTIONAL INVESTOR (ROPE) — Experiences constraint as pure coordination: banks facilitate efficient capital sourcing from institutional investors, matching maturity profiles and risk preferences. Net beneficiary with multiple exit options (commercial paper, bond markets, direct lending platforms). Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY / CENTRAL BANK (SCAFFOLD) — Views disintermediation as a temporary challenge with structural sunset. Basel capital requirements, deposit insurance mandates, and Fed emergency facilities coordinate an exit path: as fintech and alternative lenders scale, traditional bank intermediation transitions to niche activity. Sunset rationale: digitization and regulatory competition will mature alternative credit markets within 15-25 years, eliminating the coordination problem that banks solve.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BANKING INSTITUTION (PITON) — Traditional commercial bank sees its core function (credit intermediation between depositors and borrowers) as atrophied. Maintains presence through regulatory protection, deposit insurance guarantees, and incumbent relationships despite declining functional value. Theater ratio (0.35) reflects genuine operational content — interest rate risk management, credit assessment infrastructure, liquidity provision — but this function is increasingly separable and provided by specialized firms. Bank persists through institutional inertia.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: BANKING SYSTEM STABILITY (TANGLED ROPE) — Disintermediation creates genuine coordination function (capital formation, risk distribution) alongside extraction mechanism (concentration risk, procyclicality amplification, fragile market-based credit supply). System stability bears asymmetric cost when funding markets seize (2008, 2020, 2023 banking episodes). Constrained by interconnectedness and policy lock-in despite agency to redesign system structure. Analytical classification shows hybrid extraction-coordination character.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — Risks naturalizing disintermediation as an inherent consequence of financial market development. Information asymmetry between lenders and borrowers creates 'natural' need for intermediaries; information technology reduces that asymmetry and thus disintermediates. This reads as a natural law view. However, structural data reveals regulatory choices (deposit insurance, capital requirements, monetary policy transmission mechanisms) actively maintain banking-centered intermediation against technological and market forces. False summit: contingent policy that appears inevitable.
constraint_indexing:constraint_classification(commercial_bank_disintermediation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commercial_bank_disintermediation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commercial_bank_disintermediation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commercial_bank_disintermediation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commercial_bank_disintermediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commercial_bank_disintermediation, TR),
    TR >= 0.70.

:- end_tests(commercial_bank_disintermediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts from retail depositors through deposit rate suppression and from small business borrowers through credit rationing and spread widening. Large corporations and institutional investors capture the extracted value through direct capital market access. The 0.52 value reflects genuine extraction asymmetry: deposit rate suppression alone produces 15-40 basis points annual transfer from retail to wholesale functions; small business lending withdrawal reduces credit availability by 20-30% in some sectors. Suppression (0.48): Moderate-high. Retail depositors cannot exit to direct lending markets (regulatory barriers, expertise requirements, capital minimums). Small businesses cannot access capital markets directly (credit rating requirements, issuance costs, scale minimums). But suppression is not total — some retail alternatives exist (money market funds, Treasury bonds, high-yield savings accounts), though with reduced convenience and yield. Theater ratio (0.35): Relatively low. The constraint retains genuine coordination function — bank credit assessment, liquidity management, settlement infrastructure — despite declining necessity. Theater is higher than it would be in pure market-based system (0.15-0.25) but lower than pre-disintermediation banking (0.50-0.60).
 *
 * PERSPECTIVAL GAP:
 *   The constraint simultaneously appears as snare, tangled rope, rope, scaffold, and piton depending on structural position. The gap between powerless/trapped (snare) and institutional/arbitrage (rope) perspectives is maximum (~0.90 classification distance). This reflects that the same structural mechanism — capital market development and information technology — extracts value from retail actors while benefiting institutional actors. The regulatory and central bank scaffold perspective is structurally real but aspirational: it assumes that fintech and alternative lenders will mature sufficiently to provide small business credit and retail savings options at comparable cost and quality. If this assumption fails, the scaffold collapses into snare or persistent tangled rope. The analytical observer's mountain is a false summit: disintermediation appears inevitable only if we naturalize regulatory and institutional constraints as immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from who benefits and who bears costs. Retail depositors (beneficiaries of traditional banking, now victims of disintermediation): d ≈ 0.95 (full target). Small business borrowers (mixed): d ≈ 0.75 (heavy target, some benefits from credit access). Large corporations (beneficiaries): d ≈ 0.10 (full beneficiary with arbitrage). Commercial banks (captured beneficiaries, losing function): d ≈ 0.38 (override from canonical ~0.15 to reflect regulatory capture and constrained agency). Regulatory authority (constrained institutional actor maintaining system stability): d ≈ 0.45. Banking system stability (collective victim of procyclical risk concentration): d ≈ 0.72 (target with organized agency). The directionality override for commercial banks reflects their status as captured institutional actors: they appear to be beneficiaries of their own disintermediation, but regulatory dependence and functional loss mean they are actually constrained victims of their own extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing between coordination function and extraction mechanism. Banks genuinely coordinate depositor-borrower matching, credit assessment, and liquidity management. This coordination function persists at ε ≈ 0.28 (baseline). But disintermediation adds extraction layer (asymmetric benefit to large actors, cost to retail actors, regulatory protection of declining function) that increases visible extractiveness to 0.52. The tangled rope classification correctly captures this hybrid: 0.40 ≤ χ ≤ 0.90, with genuine coordination function (beneficiaries in base_properties) and asymmetric extraction (victims in base_properties and suppression ≥ 0.40) coexisting. The snare perspective (from powerless/trapped retail depositors) represents the extraction component maximized: they see no coordination benefit, only extraction. The rope perspective (from institutional beneficiaries) represents the coordination component: they see pure coordination of capital supply. The mandatrophy is resolved by recognizing that both readings are correct — the constraint is genuinely hybrid, and the perspectival gap between them is the diagnostic signal of the tangled rope class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_moral_hazard_vs_stability,
    'Does deposit insurance create the disintermediation dynamic by distorting risk pricing, or does it prevent systemic collapse that would otherwise result from unregulated disintermediation?',
    'Counterfactual analysis: compare deposit insurance regimes with uninsured systems; measure bank risk-taking behavior and system stability outcomes',
    'If insurance is primary driver: disintermediation is policy artifact, easily reversible. If insurance prevents worse collapse: disintermediation reflects genuine preference for market-based allocation despite risks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_moral_hazard_vs_stability, empirical, 'Whether deposit insurance drives or prevents disintermediation dynamic').

omega_variable(
    credit_assessment_separability,
    'Can credit assessment be effectively separated from capital provision, or does bank relationship lending provide value that market-based assessment cannot match?',
    'Performance comparison: default rates and recovery on bank-originated vs market-assessed credit; longitudinal tracking of loan quality across intermediation models',
    'If separable: disintermediation reduces real friction; constraint is pure extraction. If inseparable: disintermediation creates adverse selection; snare classification underestimates systemic risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credit_assessment_separability, empirical, 'Whether credit assessment can be separated from intermediation').

omega_variable(
    systemic_funding_cliff_risk,
    'Does market-based intermediation provide materially higher fragility during funding stress compared to bank-based intermediation?',
    'Cross-sectional comparison of funding cost and availability during stress periods (2008, 2020, 2023 banking crises); measure market-based funding vs bank lending cost volatility',
    'If significantly higher fragility: systemic cost of disintermediation is real; suppression value underestimates structural constraint. If comparable: disintermediation is functionally equivalent intermediation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_funding_cliff_risk, empirical, 'Whether market-based funding shows higher fragility during stress').

omega_variable(
    fintech_regulatory_arbitrage_sustainability,
    'Do fintech platforms and non-bank lenders persistently enjoy regulatory arbitrage advantages, or does regulatory convergence eventually equalize cost structures?',
    'Historical analysis of regulatory arbitrage duration in prior financial innovations; current trajectory of fintech capital requirements and compliance costs vs bank equivalents',
    'If arbitrage is persistent: disintermediation is structural regulatory capture. If convergence occurs: disintermediation is temporary competitive advantage, scaffold sunset logic holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fintech_regulatory_arbitrage_sustainability, empirical, 'Sustainability of fintech regulatory arbitrage advantages').

omega_variable(
    small_business_credit_substitution,
    'Can alternative lenders (fintech, community banks, credit unions) provide small business credit at quality and cost comparable to pre-disintermediation commercial banks, or is this market segment permanently underserved?',
    'Longitudinal tracking of small business credit availability, cost, and default rates across lending models; assessment of alternative lender capacity growth vs historical bank share loss',
    'If substitution occurs: small business victim status is temporary (scaffold logic). If market fails: small business snare classification is persistent; policy intervention required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_business_credit_substitution, empirical, 'Whether alternative lenders can replace bank credit for small business').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commercial_bank_disintermediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disbk_tr_t0, commercial_bank_disintermediation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(disbk_tr_t10, commercial_bank_disintermediation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(disbk_tr_t20, commercial_bank_disintermediation, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(disbk_be_t0, commercial_bank_disintermediation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(disbk_be_t10, commercial_bank_disintermediation, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(disbk_be_t20, commercial_bank_disintermediation, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commercial_bank_disintermediation, resource_allocation).
narrative_ontology:boltzmann_floor_override(commercial_bank_disintermediation, 0.18).
narrative_ontology:affects_constraint(commercial_bank_disintermediation, systemic_procyclicality).
narrative_ontology:affects_constraint(commercial_bank_disintermediation, retail_financial_exclusion).
narrative_ontology:affects_constraint(commercial_bank_disintermediation, small_business_credit_rationing).

% DUAL FORMULATION NOTE:
% Disintermediation is downstream of information technology and regulatory policy changes (deposit insurance, capital requirements, monetary policy framework). It is upstream of systemic fragility outcomes (funding market seizures, credit rationing during stress). The constraint family includes: technology-driven intermediation substitutability (Mountain), regulatory structure maintenance (Piton), retail market exit (Snare), and systemic stability trade-offs (Tangled Rope). Each story has distinct ε: technology substitutability ε ≈ 0.08 (near-inevitable), regulatory maintenance ε ≈ 0.70 (extractive institutional inertia), retail exit ε ≈ 0.85 (snare), systemic trade-off ε ≈ 0.52 (this story).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commercial_bank_disintermediation, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
