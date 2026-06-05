% ============================================================================
% CONSTRAINT STORY: pe_rental_market_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pe_rental_market_lockin, []).

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
 *   constraint_id: pe_rental_market_lockin
 *   human_readable: Private Equity Lock-in of Single-Family Rental Market
 *   domain: economic/housing_markets
 *
 * SUMMARY:
 *   Following the 2008 financial crisis, large private equity firms
 *   (Invitation Homes, American Homes 4 Rent, Starwood Waypoint,
 *   Blackstone/Invitation Homes merger) began acquiring single-family homes
 *   at scale, converting them from owner-occupied to rental inventory. By
 *   2023, PE and institutional investors owned approximately 28-32% of
 *   single-family rental stock in major acquisition corridors (Atlanta,
 *   Phoenix, Tucson, Las Vegas, Miami). This constraint exhibits sharp
 *   perspectival divergence: PE firms experience the mechanism as rational
 *   portfolio coordination and arbitrage access; first-time homebuyers
 *   experience it as market foreclosure; renters experience it as systematic
 *   extraction through rent escalation; local housing affordability
 *   experiences it as structural victimization with no voice. The
 *   extractiveness has risen from moderate coordination (0.22 during
 *   post-crisis recovery, 2008-2012) to high extraction (0.58 at present
 *   maturity, 2023+), indicating that the crisis-recovery narrative no longer
 *   justifies the asymmetry. Theater ratio remains low (0.45), indicating
 *   that PE firms justify rent increases through explicit efficiency claims
 *   and market logic rather than performative narrative — the extraction is
 *   blunt, not theatrical.
 *
 * KEY AGENTS:
 *   - Institutional PE Firms: Primary beneficiary (institutional/arbitrage) — capture capital gains, operational leverage, regulatory arbitrage via single-family exemptions
 *   - First-Time Homebuyers: Primary victim (powerless/trapped) — priced out of ownership opportunity, cannot organize, cannot exit region without employment loss
 *   - Existing Renters: Secondary victim (moderate/constrained) — systematic rent escalation, reduced maintenance, eviction velocity; can move but at high cost
 *   - Local Housing Affordability Commons: Tertiary victim (powerless/trapped) — abstract collective good; bears intergenerational wealth exclusion costs
 *   - Housing Advocacy & Tenant Coalitions: Organized opposition (organized/constrained) — achieved rent control in some states but face structural capital-mobility disadvantage
 *   - Distressed Mortgage Holders (2008-2012): Former primary beneficiary (now marginalized) — PE entry created liquidity relief but no long-term benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_rental_market_lockin, 0.58).
domain_priors:suppression_score(pe_rental_market_lockin, 0.68).
domain_priors:theater_ratio(pe_rental_market_lockin, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_rental_market_lockin, extractiveness, 0.58).
narrative_ontology:constraint_metric(pe_rental_market_lockin, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pe_rental_market_lockin, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pe_rental_market_lockin, snare).
narrative_ontology:human_readable(pe_rental_market_lockin, "Private Equity Lock-in of Single-Family Rental Market").
narrative_ontology:topic_domain(pe_rental_market_lockin, "economic/housing_markets").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, institutional_pe_firms).
narrative_ontology:constraint_victim(pe_rental_market_lockin, first_time_homebuyers).
narrative_ontology:constraint_victim(pe_rental_market_lockin, renters_in_acquisition_corridors).
narrative_ontology:constraint_victim(pe_rental_market_lockin, local_housing_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST-TIME HOMEBUYERS IN ACQUISITION CORRIDORS (SNARE) — Priced out of ownership market as PE firms consolidate inventory. Trapped: cannot afford purchase, cannot exit region without employment disruption, cannot organize collective counter-action. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RENTERS IN PE-CONSOLIDATED MARKETS (SNARE) — Systematic rent escalation, reduced maintenance responsiveness (economies of scale prioritize asset extraction over service quality), eviction velocity increases. Constrained exit: relocation costs, limited alternatives in same region, employment ties. d≈0.85, f(d)≈1.18, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL PE FIRM (ROPE) — Experiences constraint as coordination mechanism: portfolio consolidation, bulk mortgage access, regulatory arbitrage (single-family rentals exempt from many multi-family protections). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; effective extraction is negative because arbitrage access is subvention.
constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOUSING MARKET SYSTEMICALLY (TANGLED ROPE) — PE consolidation provides coordination benefit (liquidity injection, stabilization of distressed portfolios post-2008) but creates asymmetric extraction through rent escalation dynamics and owner-to-renter conversion. Both functions present: coordination during recovery phase + extraction during maturity. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCAL HOUSING AFFORDABILITY COMMONS (SNARE) — Abstract collective good with no organized voice. Systematic extraction via rent escalation, reduced supply of ownership-track housing, and intergenerational exclusion from wealth accumulation through homeownership. Trapped: cannot exit, organize, or negotiate. d≈0.94, f(d)≈1.42, σ=0.9 → χ≈0.76.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: HOUSING ADVOCACY & TENANT COALITIONS (TANGLED ROPE) — Organized opposition has achieved state-level rent control and anti-eviction laws in some regions (California, Oregon, New York), creating partial coordination function (collective bargaining, standard-setting). But fundamental asymmetry remains: PE firms have exit (can move capital to higher-yield regions), advocates face coordination costs. d≈0.58, f(d)≈0.77, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_rental_market_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pe_rental_market_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pe_rental_market_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High, reflecting systematic rent escalation in PE-consolidated markets (15-25% above single-landlord comparables) and supply reduction (conversion of owner-track inventory to permanent rental). The trajectory from 0.22 (recovery phase) to 0.58 (maturity) reveals that the crisis-recovery justification has expired — extraction intensified as markets stabilized. Suppression (0.68): High. Barriers include (1) capital requirements for competing with PE bulk-purchase capacity, (2) regulatory fragmentation (state/local patchwork protection), (3) collective action costs for renters, (4) employment-region coupling that prevents exit, (5) information asymmetries (rent justification opacity). Theater ratio (0.45): Moderate-low. PE firms do not rely on performative narrative — they justify rent increases through explicit efficiency claims (scale economies, professional management, maintenance coordination) and market logic (supply-demand, asset appreciation). The theater is lower than traditional institutional constraints because extraction is defended in market-rational rather than philanthropic-seeming terms.
 *
 * PERSPECTIVAL GAP:
 *   PE firm sees Rope (coordination benefit: portfolio consolidation, capital mobilization, post-crisis stabilization). First-time homebuyer sees Snare (foreclosure from ownership path, permanent renter status, trapped by employment). Existing renter sees Snare (rent escalation, reduced exit options, eviction velocity). Local housing affordability sees Snare (intergenerational wealth exclusion, no mechanism for voice or exit). Housing advocates see Tangled Rope (achieved some regulatory gains but remain structurally disadvantaged by capital mobility). Systemic market view sees Tangled Rope (coordination during recovery + extraction during maturity). The widest gap is between PE firm (rope/arbitrage) and powerless agents (snare/trapped) — institutional d≈0.08 vs powerless d≈0.92, spanning 0.84 units of directionality space.
 *
 * DIRECTIONALITY LOGIC:
 *   PE firms: Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.10. Negative effective extraction reflects that capital mobility is a form of subsidy — can withdraw if returns decline. First-time homebuyers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction: cannot compete with PE scale, cannot relocate without employment loss, cannot organize due to geographic dispersion. Renters: Victim + constrained exit → d≈0.85, f(d)≈1.18. High extraction: can relocate but at significant cost (moving expenses, lease breaks, local disruption). Local housing affordability: Victim + trapped → d≈0.94, f(d)≈1.42. Maximum extraction: abstract collective with no organized exit or negotiation capacity. Housing advocates: Organized + constrained → d≈0.58, f(d)≈0.77. Moderate extraction: achieved legal wins but face structural disadvantage — PE can exit to high-yield regions; advocates face coordination costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is correctly classified as Snare, NOT as pure Rope or false coordination. The initial post-crisis narrative framed PE entry as benign market coordination (stabilizing distressed inventory, providing liquidity). The extraction is real and asymmetric: (1) PE firms benefit from capital appreciation + operational leverage + regulatory arbitrage; (2) first-time homebuyers lose ownership opportunity; (3) renters bear systematic extraction through rent escalation; (4) local housing stability loses an entire wealth-building cohort. The coordination narrative becomes untenable once markets stabilize (extractiveness trajectory: 0.22→0.58 reveals shifting justification). The Snare classification captures that the constraint persists not because it solves a coordination problem (that was solved by 2014) but because it benefits PE firms enough to maintain suppression barriers (regulatory fragmentation, scale advantages, capital mobility). Regulatory efforts (rent control in CA/OR/NY, anti-eviction, licensing) represent attempts to convert Snare to Tangled Rope or Scaffold, but the structural data shows they have not yet succeeded — extraction remains high, suggesting PE firms retain superior exit capacity. The false coordinate would be classifying this as Rope (pure coordination without asymmetry) or Scaffold (temporary problem with sunset). Neither fits: the extraction is persistent, systematic, and structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_crisis_recovery_necessity,
    'Was PE rental consolidation a necessary coordination mechanism for stabilizing the distressed housing market post-2008, or a disguised extraction enabled by crisis conditions?',
    'Comparative analysis: markets with high PE penetration vs low PE penetration; correlation between PE entry timing and employment recovery vs rent escalation; counterfactual recovery models',
    'If necessary: constraint classifies as Scaffold with sunset (crisis phase now ended). If disguised extraction: constraint is pure Snare with false coordination narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_crisis_recovery_necessity, empirical, 'Whether PE consolidation was crisis-necessary coordination or opportunistic extraction').

omega_variable(
    single_family_rental_inherent_efficiency,
    'Do single-family rental portfolios under PE management achieve meaningful efficiency gains (maintenance coordination, bulk purchasing, risk pooling) that justify rent premiums, or are rent increases purely extraction with no functional offset?',
    'Comparative service metrics: maintenance response times, rent-to-cost ratios, resident satisfaction scores between PE and small-landlord portfolios; accounting for market-level concentration effects',
    'If efficiency gains real: extraction justified as Tangled Rope (coordination + asymmetry). If purely extraction: classification confirms Snare with false efficiency narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(single_family_rental_inherent_efficiency, empirical, 'Whether PE portfolio management creates actual efficiency gains').

omega_variable(
    regulatory_capacity_structural,
    'Can state and local regulatory action (rent control, anti-eviction, licensing requirements) actually constrain PE extraction, or do PE firms structurally outpace regulatory capacity due to capital mobility and legal resources?',
    'Historical analysis of regulatory effectiveness: states with strong rent control vs weak; PE firm response patterns; regulatory capture indicators (campaign financing, revolving door); interstate capital migration tracking',
    'If regulation effective: constraint transitions to manageable Tangled Rope. If PE structurally outpaces: constraint is structural Snare and advocacy is performative (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_structural, empirical, 'Whether regulatory action can constrain PE extraction dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pe_rental_market_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pe_rental_tr_t0, pe_rental_market_lockin, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pe_rental_tr_t5, pe_rental_market_lockin, theater_ratio, 5, 0.4).
narrative_ontology:measurement(pe_rental_tr_t10, pe_rental_market_lockin, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(pe_rental_be_t0, pe_rental_market_lockin, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pe_rental_be_t5, pe_rental_market_lockin, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pe_rental_be_t10, pe_rental_market_lockin, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pe_rental_market_lockin, resource_allocation).
narrative_ontology:affects_constraint(pe_rental_market_lockin, housing_affordability_crisis).
narrative_ontology:affects_constraint(pe_rental_market_lockin, intergenerational_wealth_inequality).
narrative_ontology:affects_constraint(pe_rental_market_lockin, regulatory_fragmentation_housing).

% DUAL FORMULATION NOTE:
% PE rental consolidation can be decomposed into two structurally distinct phases: (1) Post-crisis recovery (2008-2014): ε≈0.22, crisis-necessary coordination (Rope/Scaffold). (2) Market maturity (2015-present): ε≈0.58, pure extraction enabled by structural advantages (Snare). These are linked: the recovery phase legitimized institutional presence that enabled the extraction phase. The network upstream includes the housing affordability crisis (primary victim) and intergenerational wealth inequality (systemic effect). Downstream includes regulatory fragmentation efforts (attempted Snare-to-Tangled Rope conversion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pe_rental_market_lockin, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
