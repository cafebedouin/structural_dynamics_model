% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__institutional_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutional Construction of Housing Price Formation
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   Housing price formation is contested across four readings of a single
 *   kernel: naturalist (prices reflect objective scarcity), georgist (prices
 *   separate unearned land rent from earned improvement value), institutional
 *   (prices are constructed by policy), and financialization (prices are
 *   driven by credit expansion and asset speculation). This constraint story
 *   instantiates the INSTITUTIONAL READING: price formation is constructed by
 *   zoning restrictions (supply constraints), lending standards (debt
 *   gatekeeping), tax treatment (capital gains and mortgage interest
 *   subsidies), and intermediary platforms (MLS, brokerage networks). The
 *   reading characterizes the arrangement as TANGLED ROPE: genuine
 *   coordination problems are solved (stable property markets, credit
 *   allocation, prevention of nuisance externalities) but are inseparably
 *   coupled with asymmetric extraction (incumbent owners and lenders benefit
 *   from scarcity and high prices; renters and first-time buyers bear the
 *   costs). The institutional reading competes with the naturalist reading
 *   (which treats prices as reflecting natural scarcity) and the
 *   financialization reading (which emphasizes credit expansion over zoning).
 *   The claim/metric gap is intentional: the constraint is CLAIMED as tangled
 *   rope (coordination for market stability + extraction of surplus) while
 *   authored metrics show extractiveness at 0.68, suppression at 0.71, and
 *   theater at 0.42 — indicating that a growing share of enforcement activity
 *   defends the extraction mechanism rather than solving the founding
 *   coordination problems.
 *
 * KEY AGENTS:
 *   - Incumbent owners: beneficiaries via appreciation, politically organized as 'neighborhood character' coalitions
 *   - Mortgage lenders: beneficiaries via origination fees, interest revenue, and high loan balances; agenda-setters via lending standards
 *   - Real estate intermediaries: beneficiaries via price-proportional commissions; platform operators controlling price discovery
 *   - Municipal zoning authorities: agenda-setters controlling supply; responsive to incumbent-owner electoral dominance
 *   - First-time buyers: victims via inflated prices and high debt-to-income ratios; constrained exit
 *   - Renters: victims via high rents and exclusion from ownership; powerless, trapped
 *   - Aspiring homeowners in supply-constrained markets: victims, identity-locked (homeownership narrative + debt structure)
 *   - Policy analysts: observers; the institutional reading competes with naturalist, georgist, and financialization readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '1fefcefe-076d-4e99-90f0-8fc45bf74eae').
narrative_ontology:cs_kernel_codification('1fefcefe-076d-4e99-90f0-8fc45bf74eae', distributed).
narrative_ontology:cs_authority_grounding('1fefcefe-076d-4e99-90f0-8fc45bf74eae', extraction).
narrative_ontology:cs_interpretation_layer_present('1fefcefe-076d-4e99-90f0-8fc45bf74eae').
narrative_ontology:cs_reading_relation('1fefcefe-076d-4e99-90f0-8fc45bf74eae', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1fefcefe-076d-4e99-90f0-8fc45bf74eae', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fefcefe-076d-4e99-90f0-8fc45bf74eae', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('1fefcefe-076d-4e99-90f0-8fc45bf74eae', foundational, institutional_mechanisms_primary_price_driver).
narrative_ontology:cs_axiom_status(institutional_mechanisms_primary_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('1fefcefe-076d-4e99-90f0-8fc45bf74eae', institutional_mechanisms_primary_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('1fefcefe-076d-4e99-90f0-8fc45bf74eae', foundational, prices_above_equilibrium_due_to_supply_restriction).
narrative_ontology:cs_axiom_status(prices_above_equilibrium_due_to_supply_restriction, holdable).
narrative_ontology:cs_axiom_grounding('1fefcefe-076d-4e99-90f0-8fc45bf74eae', prices_above_equilibrium_due_to_supply_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('1fefcefe-076d-4e99-90f0-8fc45bf74eae', policy_constructed_housing_markets).
narrative_ontology:cs_drift_state('1fefcefe-076d-4e99-90f0-8fc45bf74eae', contemporary_post_2008_financialization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1fefcefe-076d-4e99-90f0-8fc45bf74eae', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_owners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, municipal_zoning_authorities).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, aspiring_homeowners_in_supply_constrained_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, credit_rating_agencies_and_financial_engineers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, construction_workers_and_developers).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, institutions_shape_economic_outcomes).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, regulatory_design_distributes_rents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit directly from price appreciation driven by supply restrictions and favorable tax treatment (capital gains exclusion, mortgage interest deduction). They accumulate wealth through appreciation that is structurally embedded in zoning and lending policy. Their portfolios are defended by incumbent-owner political coalitions that resist density and restrict new supply in desirable locations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_owners, beneficiary,
    organized, generational, mobile, national).

% Profit from high nominal prices that support larger outstanding loan balances, higher origination fees, and increased interest revenue. They enforce lending standards that effectively require down payments and credit scores that poor and working-class buyers cannot meet. They lobby for favorable tax treatment of mortgage debt and securitization infrastructure. Their business model depends on housing prices remaining high relative to incomes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Collect commissions proportional to transaction price: higher prices = higher revenue. They benefit from price appreciation and supply scarcity (fewer sales but higher per-transaction fees). They also bear some cost from market volatility and regulation. They operate platforms (MLS, brokerage portals, property data systems) that mediate price discovery and coordinate seller-side interests.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediaries, payer).

% Control supply through zoning restrictions (single-family zoning, lot-size minimums, height/density caps). They respond to incumbent homeowner political pressure to restrict supply and preserve neighborhood character. Zoning restrictions are framed as protecting property value and community stability; the distributional consequences (supply scarcity, rising prices, exclusion of lower-income household) are externalized. The zoning authority's legitimacy derives from local democratic process but is constrained by incumbent-homeowner electoral dominance.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_zoning_authorities, agenda_setter,
    institutional, biographical, analytical, local).

% Face prices driven up by supply restrictions, forcing larger mortgages relative to income. They must meet lender standards (down payment, credit score, income verification) that exclude lower-income households entirely. They bear the extraction in the form of inflated prices, decades-long debt, and constrained location choice. Their options are: take on debt at current prices, rent indefinitely, or relocate to distant, supply-abundant markets with poor job access.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, national).

% Priced out of ownership by institutional price construction. High sale prices drive up rental prices as competing investors treat housing as financial asset. They bear extraction through high rent payments that flow to landlords and investors, have no equity stake, and face eviction risk. Their exit options are constrained by geography (job proximity, family networks), credit access (rental screening), and the tight rental supply in high-price markets. They accumulate no wealth.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, local).

% Wish to buy in economically vibrant, supply-restricted cities but are priced out. They face a choice: accept decades of debt, relocate to distant supply-abundant markets (at employment/social cost), or remain renters indefinitely. Identity-locked because homeownership is deeply tied to cultural narratives of success, family stability, and wealth accumulation. Their constraints are structural (zoning, lending, tax policy) but appear to many as personal financial failure. This identity lock is sustained by the institutional framing that prices are natural scarcity signals, not policy artifacts.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, aspiring_homeowners_in_supply_constrained_markets, payer,
    powerless, biographical, identity_locked, local).

% Construction employment and development profit are constrained by zoning restrictions and high land cost. They would gain from relaxed supply constraints but are often politically outmatched by incumbent-owner coalitions. Some developers profit from scarcity (smaller supply means higher per-unit prices), creating a complex internal dynamic. Most construction workers face intermittent employment due to supply constraints.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, construction_workers_and_developers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, construction_workers_and_developers, observer).

% Profit from mortgage securitization, complex financial instruments, and the need to assess risk in a system where prices are inflated by institutional policy rather than reflecting economic fundamentals. They create products that allow lenders to exit origination risk and institutional investors to treat housing as financial asset. They benefit from price volatility and the opacity of price formation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, credit_rating_agencies_and_financial_engineers, beneficiary,
    institutional, biographical, arbitrage, global).

% Analyze the constraint's operation. They observe price formation and debate whether it reflects scarcity or policy. The institutional reading competes with naturalist, georgist, and financialization readings—this observer seat sees the institutional mechanisms but not all will agree on their significance.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, policy_analysts_and_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_owners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Combines land-use governance, credit allocation, and tax incentives into a coordinated system that: (1) protects existing property owners from value-diluting competition; (2) ensures lenders have collateral-backed borrowers with institutional standards; (3) distributes real estate intermediation through standardized commission structures; (4) funds municipal government through property tax. The coordination achieves stable, predictable property markets from the perspective of incumbents and creditors.
% TRANSFER_FUNCTION: Transfers wealth from renters and first-time buyers to incumbent owners, lenders, and intermediaries. The transfer mechanism: supply restrictions raise prices → high prices require large mortgages → lenders profit from origination fees and interest → incumbent owners accumulate appreciation → renters pay inflated rents with no equity → aspiring buyers either defer purchase indefinitely or carry decades of debt. Tax subsidies (mortgage interest deduction, capital gains exemption) direct public revenue to owner-beneficiaries. Intermediary commissions scale with price.
% ABSENT_VOICES: Prospective residents priced out of markets, aspiring buyers unable to meet lending standards, construction workers facing supply-constrained employment, renters organized as a constituency (generally fragmented and politically weak), advocates for density and mixed-income housing (structurally excluded from zoning processes that privilege incumbent owner input). The institutional reading itself is marginalized in mainstream housing discourse, which frames prices as natural market outcomes.
% DISAPPEARANCE_RATIONALE: If the institutional constraint (zoning restrictions, lending standards, tax treatment, intermediary platforms) disappeared overnight: supply would increase rapidly in desirable locations; prices would fall toward cost-plus-normal-profit; lender origination revenue would decline; incumbent owners would lose appreciation gains; renters would have vastly larger choice sets and lower rents; first-time buyers would face lower entry costs; construction employment would surge. The entire allocation of housing stock, wealth distribution, and resource flows would reorganize within 5–10 years.
% FOUNDING_PROBLEM: Early zoning emerged to separate industrial from residential areas and prevent nuisance externalities (smoke, noise, congestion). Tax incentives for owner-occupied housing were introduced post-WWII to encourage homeownership and suburban stability. Mortgage lending standards were formalized to manage credit risk and protect depositor funds. Real estate intermediation evolved to reduce search costs and information asymmetry in an illiquid market. Each mechanism was justified as solving a specific coordination or efficiency problem.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent owners and lenders attest the founding problems remain live: zoning prevents nuisance/sprawl, lending standards prevent defaults, tax incentives encourage stability, intermediaries reduce transaction costs. Institutional economists, housing advocates, and renters' groups attest the founding problems are substantially solved and the mechanisms now function primarily as wealth-extraction and supply-restriction devices. Cross-jurisdictional evidence from less-zoned cities (e.g., Minneapolis post-2020 zoning reform, Houston's minimal zoning) and historical analysis from the 1950s-60s show supply-coordination problems are solvable without current restrictiveness levels. No mainstream corroboration from outside benefiting parties endorses the current configuration as necessary to the founding problems.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-high because the arrangement systematically raises prices above equilibrium levels that would obtain with relaxed zoning and open lending. The mechanism: zoning restrictions reduce supply in desirable markets → limited supply pushes prices up → high prices justify large mortgages → lenders profit from origination and interest on inflated loan balances → incumbent owners accumulate appreciation → renters are priced out and pay inflated rents → first-time buyers must accept decades of debt. The extraction is not incidental to the coordination; it is structurally embedded. Suppression (0.71) is high because the arrangement must actively exclude alternatives to persist: anti-density zoning requires enforcement (planning boards, appeals processes, incumbent-homeowner political mobilization); lending standards require continuous gatekeeping (credit scoring, down-payment verification); tax subsidies require legislative defense against reform. The theater ratio (0.42) reflects that a growing share of enforcement activity defends the extraction mechanism (zoning boards defending supply restrictions) rather than solving the founding coordination problems (preventing industrial nuisance). Measurements show extraction rising from 0.51 to 0.68 over 40 years and plateauing, suggesting the institutional arrangement has matured and stabilized at a high-extraction equilibrium. Theater ratio rises from 0.32 to 0.42 and plateaus, consistent with an increasingly theatrical performance of 'protecting community character' that is revealed as supply-restriction and wealth protection as price-to-income ratios diverge from historical norms.
 *
 * PERSPECTIVAL GAP:
 *   The structural divergence runs along the beneficiary/victim divide. Incumbent owners and lenders frame the constraint as legitimate coordination: zoning protects neighborhoods, lending standards prevent defaults, tax incentives encourage homeownership and family stability. From their seats, the constraint solves real problems (sprawl, credit risk, market stability) and deserves to persist. First-time buyers and renters frame the constraint as predatory extraction: zoning is supply restriction, lending standards are gatekeeping, tax incentives subsidize the wealthy, intermediary platforms limit competition. From their seats, the constraint persists not because it solves problems but because beneficiaries have political power to defend it. The engine should compute these divergences from power/exit/beneficiary-victim data. The institutional reading's claim (tangled rope) asserts that BOTH frames capture something true: the coordination function is real AND the extraction is real AND they are inseparably coupled. The metrics support this: if the constraint were pure rope (coordination), we would expect low suppression and no theatrical enforcement. If it were pure snare (extraction), we would expect very high suppression and high theater. The measured values (0.71 suppression, 0.42 theater) sit between the extremes and indicate a hybrid in which enforcement defends both coordination infrastructure AND extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent owners occupy the beneficiary end of the directionality spectrum (d near 0.0): they benefit directly from appreciation, have high exit options (arbitrage — they can sell and move), and have organized political power. Mortgage lenders and intermediaries also sit near the beneficiary end (d ~ 0.1–0.2): they benefit from high loan volumes and transaction prices, have arbitrage options (can exit product lines or markets), and have institutional power. Municipal zoning authorities are agenda-setters (d ~ 0.3–0.4): they set the rules but are responsive to incumbent-owner pressure, so they have moderate extraction directed at non-voters (renters, prospective residents). First-time buyers sit at moderate extraction (d ~ 0.6–0.7): they bear inflated prices and decades of debt, have constrained exit (can only go to supply-abundant distant markets or remain renters), and moderate power (can organize politically but are outmatched by incumbent-owner coalitions). Renters sit at the target end (d ~ 0.85–0.95): they are priced out of ownership entirely, trapped by job/family geography and credit constraints, powerless individually (though potentially powerful if organized), and bear extraction through high rents with no equity stake. Aspiring homeowners in supply-constrained markets sit at very high extraction (d ~ 0.90–1.0): identity-locked (homeownership is culturally essential), trapped by job/family ties, and facing the choice to accept decades of debt, relocate, or remain renters indefinitely. The per-seat computation should show divergence: from the municipal authority's seat, the constraint may compute as rope (coordination for community stability); from the renter's seat it computes as snare (pure extraction with limited alternatives); from the first-time buyer's seat it computes as tangled rope (coordination benefits are real — stable markets, credit access — but extraction is severe and active enforcement is required to sustain it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was to prevent industrial-residential nuisance externalities (zoning's original purpose) and to encourage post-WWII suburban stability and homeownership (tax incentives' original purpose). By 2000, these problems were substantially solved: zoning prevented nuisance, tax incentives had achieved high homeownership rates (~69% peak in 2004), lending standards had enabled mass-market credit access. However, the institutional mechanisms persisted and were strengthened: zoning became increasingly restrictive (moving beyond nuisance prevention into aesthetic/character preservation), tax incentives were defended even as homeownership stalled after 2008, lending standards hardened after 2008 crisis (higher credit score requirements, larger down payments), intermediary platform control increased (algorithmic price discovery, investor dominance). The mandatrophy is real: the founding problems are dead or substantially solved, but the mechanisms remain active and have been repurposed toward wealth extraction and supply restriction. The founding_problem_status is 'contested' because incumbent owners and lenders argue the problems are live (sprawl risk, credit risk, market instability), while institutional economists and renters argue the problems are solved or manageable and the mechanisms now serve primarily extractive ends. The contradiction between founding_problem_status=dead and disappearance_verdict=world_rearranges is the key mandatrophy indicator: if the founding problem is dead but the world would dramatically rearrange if the constraint disappeared, the constraint is performing work that is NOT captured by its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_natural_price_formation,
    'Is the observed price gradient between supply-restricted and supply-abundant cities evidence that institutional constraints (zoning, lending, tax policy) are the PRIMARY drivers of price formation, or do they amplify an underlying natural scarcity gradient?',
    'Controlled-policy experiments (e.g., Minneapolis zoning reform, Houston minimal-zoning baseline) tracking price and supply response; econometric decomposition of price variance attributable to institutional vs. natural constraints; cross-national comparison of price-to-income ratios in cities with identical natural scarcity but different institutional regimes.',
    'If institutional constraints are primary (R² > 0.6 in explaining price variance), the institutional reading strengthens and the naturalist reading becomes descriptively inaccurate. If natural scarcity dominates (institutional constraints explain < 0.3 of variance), institutions are secondary and the naturalist reading''s core premise holds. Moderate attribution (~0.3–0.6) would support the integrated view that institutions amplify natural gradients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_natural_price_formation, empirical, 'Whether observed prices are constructed by institutions or reflect natural scarcity.').

omega_variable(
    kernel_reading_contest_frame,
    'Does this constraint exhaust the meaning of the price-formation kernel, or does it instantiate one reading among genuinely incommensurable alternatives?',
    'The sibling readings (naturalist, georgist, financialization) are authored as separate constraint files, each with its own ε, beneficiary/victim structure, and cs_structure. The contest is resolved not by merging readings but by cross-reading comparison: do they occupy the same structural space or different ones? If institutional constraints and financialization mechanisms are coupled (high correlation in when zoning-driven supply restrictions overlap with mortgage securitization expansion), the readings may be causally entangled. If they operate independently, they are distinct constraints.',
    'If readings are incommensurable (each logically defensible within its own framework but contradicting siblings at the core), price formation is intrinsically contested — no reading can be falsified without changing the frame. If readings are empirically distinguishable (different ε ranges, different beneficiary sets under different conditions), the institutional reading''s truth-value depends on measurable facts about the world. The kernel_reading_contest_frame omega documents the irreducibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, conceptual, 'Whether price-formation readings are incommensurable or empirically distinguishable.').

omega_variable(
    identity_lock_mechanism_durability,
    'Is the identity lock binding aspiring homeowners in supply-constrained markets a structural feature of the institutional arrangement, or a cultural narrative that could shift if price trajectories changed?',
    'Historical analysis of homeownership narratives in periods of affordable supply (1950s–60s) vs. later unaffordability; survey data on aspiring buyers'' stated reasons for seeking ownership (wealth accumulation, cultural narrative, family stability) and how reasons shift when affordability improves; case studies from deregulated housing markets (Tokyo, Singapore, Vienna) where homeownership rates are lower but reported life satisfaction is similar.',
    'If identity lock is structural to institutional design (framing homeownership as the wealth-building path, embedding it in credit architecture), the institutional reading''s characterization of suppression as both structural and internalized is accurate. If identity lock is primarily cultural and shifts with affordability, the institutional constraint operates primarily through structural barriers (zoning, lending) and the identity component is secondary and malleable. This affects the diagnosis: structural suppression requires institutional redesign; internalized suppression requires also addressing narrative and cultural normalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_durability, empirical, 'Whether identity lock is structurally embedded in the institutional arrangement or culturally contingent.').

omega_variable(
    beneficiary_coalition_stability,
    'Is the beneficiary coalition (incumbent owners + lenders + intermediaries + municipal authorities) stable and actively maintained, or is it showing signs of fragmentation as financialization and investor dominance shift incentive structures?',
    'Political-economy analysis of coalition dynamics: when do incumbent homeowners'' interests align with investor-landlords vs. diverge (e.g., landlords benefit from high rents, homeowners benefit from price appreciation but may oppose investor competition for limited supply)? Do municipal zoning authorities align with incumbent-owner interests consistently, or do they face fiscal pressure to relax zoning for tax-base expansion? Survey data on coalition-member attitudes toward policy change (would lenders support density if it stabilized prices at current nominal levels? would incumbent owners?). Case studies of reform attempts (Minneapolis 2020, YIMBYism) showing who defends the status quo.',
    'If the coalition is stable and actively maintained, the institutional reading''s characterization of active enforcement is accurate. If the coalition is fragmenting (internal contradictions between sub-beneficiaries), the constraint may be transitioning from actively enforced tangled_rope toward piton (persisting by inertia despite weakening beneficiary support). Coalition fragmentation would appear in measurements as suppression_requirement rising (harder to maintain) while beneficiary consensus declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coalition_stability, empirical, 'Whether the beneficiary coalition is stable or fragmenting under internal contradictions.').

omega_variable(
    referent_reading_boundary,
    'Does this institutional reading''s referent (the standing arrangement under contest) include the entire portfolio of policies (zoning + lending + tax treatment + intermediary platforms) as a unified constraint, or are these separable constraints that appear unified only from the beneficiary viewpoint?',
    'Decomposition test: can zoning operate independently of lending standards? Can lending standards be reformed while zoning remains restrictive? Do tax incentives remain necessary if zoning relaxes? If components are functionally separable (each could be reformed independently and produce independent effects on price formation), they are separate constraints and the institutional reading conflates them. If they are deeply coupled (tax incentives only make sense given lending standards given zoning patterns), they form one unified system and the reading is justified in treating them as one constraint.',
    'If the components are separable, the institutional reading is over-aggregated and should decompose into multiple stories (constraint_zoning_supply_restriction, constraint_lending_standards_gatekeeping, constraint_tax_subsidy_incumbent_wealth, constraint_real_estate_platform_intermediation), each with different ε and stakeholder structures. If they are unified (mutual reinforcement), the reading''s aggregation is justified and ε captures the system-level extraction. This omega documents the ambiguity in what the constraint REFERS TO — a fundamental question about the unit of analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(referent_reading_boundary, conceptual, 'Whether institutional policies form one unified constraint or multiple separable constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__institutional_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(pric_tr_t5, observed).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__institutional_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(pric_tr_t10, observed).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__institutional_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(pric_tr_t15, observed).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__institutional_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(pric_tr_t20, observed).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__institutional_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(pric_tr_t25, observed).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__institutional_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(pric_tr_t30, observed).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(pric_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__institutional_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(pric_be_t5, observed).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__institutional_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(pric_be_t10, observed).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__institutional_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(pric_be_t15, observed).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__institutional_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(pric_be_t20, observed).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__institutional_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(pric_be_t25, observed).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__institutional_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(pric_be_t30, observed).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(pric_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__institutional_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(pric_su_t5, observed).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__institutional_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(pric_su_t10, observed).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__institutional_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(pric_su_t15, observed).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__institutional_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(pric_su_t20, observed).
narrative_ontology:measurement(pric_su_t25, price_formation_kernel__institutional_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(pric_su_t25, observed).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__institutional_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(pric_su_t30, observed).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pric_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, zoning_supply_restriction__local_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mortgage_lending_standards__gatekeeping_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, housing_tax_subsidy__wealth_concentration_reading).

% DUAL FORMULATION NOTE:
% The price-formation kernel is decomposed into four readings: institutional, naturalist, georgist, and financialization. Each reading holds a different mechanism constant and produces a different ε. The institutional reading emphasizes policy construction (zoning, lending, tax, intermediaries); the naturalist reading emphasizes natural scarcity; the georgist reading emphasizes unearned land rent; the financialization reading emphasizes credit expansion. These readings are NOT alternative measurements of the same constraint—they are distinct constraints instantiating different causal hypotheses about the same kernel. The institutional reading INFLUENCES the other three: if institutional constraints are the primary driver, the other readings (naturalist, georgist, financialization) are either secondary to institutional construction or describe processes that operate WITHIN the institutional frame. If institutional constraints are NOT primary, the institutional reading becomes descriptively inaccurate and the other readings capture the dominant mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
