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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutional Price Formation in Housing Markets
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   Housing price formation in developed economies is presented by
 *   policymakers and intermediaries as a natural equilibrium process (the
 *   naturalist reading). This story instantiates the institutional reading:
 *   prices are constructed by zoning (supply restriction), lending standards
 *   (demand expectation), tax treatment (return expectations), and
 *   intermediary fee structures (value capture). The institutional reading
 *   coexists with the naturalist reading in policy discourse — most
 *   policymakers hold elements of both. The constraint is CLAIMED as Tangled
 *   Rope: it serves a coordination function (matching capital to development,
 *   standardizing underwriting) AND extracts asymmetrically (incumbent owners
 *   and lenders benefit, first-time buyers and renters pay). The measurement
 *   series show extractiveness rising from 0.45 to 0.67 over 40 years, with
 *   theater ratio rising faster (0.25 to 0.42), suggesting institutional
 *   drift toward greater extraction and performative activity. Suppression is
 *   high (0.71 at endpoint) and rising, reflecting the political power of
 *   incumbent owners and intermediaries to maintain zoning and lending
 *   standards despite affordability crises.
 *
 * KEY AGENTS:
 *   - incumbent_property_owners: Primary beneficiary (asset appreciation), organized through neighborhood associations and lobbies; sets agenda alongside policymakers
 *   - institutional_lenders: Primary beneficiary (higher nominal mortgages, fee capture), sets lending standards that enforce price expectations; institutional power
 *   - real_estate_intermediaries: Beneficiary (commissions scale with price), organized through professional associations (NAR, local boards), lobby against price transparency
 *   - first_time_buyers: Primary victim (trapped by lending standards requiring high down payments, debt-to-income ratios that presume high prices); powerless, no collective voice
 *   - renters: Primary victim (rents track prices, locked into position by employment and family), powerless; identity-locked by place ties
 *   - zoning_authorities: Agenda-setter (enforce scarcity); politically captured by incumbent owners (largest voter bloc in property-tax jurisdictions)
 *   - financial_regulators: Agenda-setter (maintain loose lending standards); face pressure from lenders and fear asset-price instability more than household debt
 *   - tax_policymakers: Agenda-setter (maintain provisions that embed prices); political cost of changing provisions is high due to incumbent-owner backlash
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.67).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Price Formation in Housing Markets").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '06db1040-d0c4-4a44-87b1-f1f6be38e5fb').
narrative_ontology:cs_kernel_codification('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', fixed_text).
narrative_ontology:cs_authority_grounding('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', extraction).
narrative_ontology:cs_interpretation_layer_present('06db1040-d0c4-4a44-87b1-f1f6be38e5fb').
narrative_ontology:cs_reading_relation('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', price_formation_kernel__georgist_reading, forecloses).
narrative_ontology:cs_reading_relation('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', foundational, zoning_enables_supply_coordination).
narrative_ontology:cs_axiom_status(zoning_enables_supply_coordination, holdable).
narrative_ontology:cs_axiom_grounding('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', zoning_enables_supply_coordination, conventional).
narrative_ontology:cs_axiom('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', foundational, lending_standards_manage_default_risk).
narrative_ontology:cs_axiom_status(lending_standards_manage_default_risk, overridden).
narrative_ontology:cs_axiom_grounding('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', lending_standards_manage_default_risk, empirically_contingent).
narrative_ontology:cs_axiom('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', secondary, tax_treatment_implements_ownership_preference).
narrative_ontology:cs_axiom_status(tax_treatment_implements_ownership_preference, holdable).
narrative_ontology:cs_axiom_grounding('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', tax_treatment_implements_ownership_preference, deontological).
narrative_ontology:cs_reference_frame('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', zoning_as_coordination_mechanism).
narrative_ontology:cs_drift_state('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', contemporary_affordability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06db1040-d0c4-4a44-87b1-f1f6be38e5fb', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_property_owners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, institutional_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, young_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from capital appreciation driven by regulatory scarcity (zoning restrictions that limit supply) and tax treatment (mortgage interest deduction, long-term capital gains rates). Can exit by selling at inflated prices but have strong incentive to maintain the system that preserves their asset values. Organize through neighborhood associations, homeowner lobbies, and conservation coalitions that lobby for restrictive zoning.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_property_owners, beneficiary,
    organized, generational, mobile, national).

% Benefit from high nominal prices by originating larger mortgages and collecting fees on origination, servicing, and refinancing. Set lending standards that require substantial down payments and income multiples, enforcing price expectations. Can exit the housing market entirely (and do, when portfolio composition shifts) but maintain leverage through regulatory capture and the subordination of borrower to collateral valuation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, institutional_lenders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, institutional_lenders, agenda_setter).

% Collect commissions (typically 6% on sale, plus origination and servicing fees) that rise proportionally with price. Have strong incentive to defend high prices and restrict supply (which tightens the market and increases per-transaction value). Organize through professional associations (NAR, local boards) that lobby for zoning preservation and campaign against price transparency.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Set and enforce zoning codes that restrict supply by limiting building height, mandating large lot sizes, requiring parking, and excluding multifamily housing. Face political pressure from incumbent owners (the largest voting bloc in property-tax-dependent jurisdictions) and real estate intermediaries. Enforcement mechanisms include permitting delays, design review, and neighborhood opposition (NIMBY coalitions).
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, zoning_authorities, agenda_setter,
    institutional, biographical, constrained, local).

% Face prices that reflect institutional construction (zoning scarcity, lending standards that assume high prices, tax incentives embedded in the price signal) rather than production cost or wage income. Must borrow at 3-4x the mortgage-to-income ratio that prevailed in prior generations. Exit options are: (1) accept prices, (2) rent indefinitely, (3) move to lower-cost jurisdictions (geographic exit, costly in job loss and social dislocation), or (4) cohabitate with family (delayed household formation). No collective voice in zoning; politically weak.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    powerless, biographical, trapped, national).

% Pay rising rents that track (with a lag) rising prices, reflecting landowner expectations of future price appreciation and the scarcity premium built by zoning. Locked into position by employment ties, family networks, school district enrollment, and insufficient savings to exit. Face eviction threats, unstable tenure, and limited legal recourse in many jurisdictions. Cannot organize across fragmented rental markets.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, identity_locked, local).

% Face a choice between (1) accepting inflated house prices and household-debt ratios unprecedented in their cohort, (2) delaying or refusing household formation, or (3) moving to lower-cost regions (geographic exit with career and social costs). Organize through generational discourse (millennials, Gen Z) but lack institutional voice in housing policy. Income does not scale with price appreciation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, young_workers, payer,
    moderate, biographical, constrained, regional).

% Maintain tax provisions (mortgage interest deduction, capital gains exclusion, 1031 exchanges) that are embedded in the price signal and in incumbent owners' expectation of future returns. Reduction or elimination of these provisions would trigger immediate property-value losses, creating political backlash from the largest voter bloc. Extraction depends on maintaining these provisions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, tax_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Set lending standards and macroprudential policy that accept housing-as-collateral-for-credit expansion as normal. Have incrementally loosened lending standards since the 1990s (down-payment requirements falling from 20% to 3-5%, debt-to-income ratios rising). Face pressure from lenders to maintain loose standards and from housing advocates to tighten them. Fear asset-price instability more than household debt.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, financial_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Document the institutional construction of price (zoning scarcity, lending standards, tax treatment) and advocate for policy remedies (zoning deregulation, lending reform, tax changes). Have no enforcement power and limited electoral leverage; proposals are consistently blocked by incumbent-owner coalitions and real-estate lobbies. Present in the analytical conversation but excluded from the policy negotiation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, observer_housing_advocates, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_property_owners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a centralized system for matching residential capital (mortgage lending) with demand (buyers and renters), standardizing underwriting criteria across markets, and managing neighborhood development through zoning predictability.
% TRANSFER_FUNCTION: Moves wealth from first-time buyers and renters (who pay prices that embed zoning scarcity and lending-standard expectations) to incumbent owners (whose assets appreciate by policy-maintained scarcity), institutional lenders (who originate and service larger mortgages and collect fees), and real-estate intermediaries (whose commissions scale with prices). Tax provisions directly transfer federal revenue to homeowners.
% ABSENT_VOICES: Future residents who will inherit the constraint. Renters in unincorporated areas not represented in zoning boards. Immigrants and workers relocating to high-cost regions have no voice in communities where they seek housing. Landowners in downzoned areas are structurally positioned to lose negotiation outcomes.
% DISAPPEARANCE_RATIONALE: Removal of institutional construction (zoning deregulation, lending-standard reversion to historical norms, elimination of tax provisions) would cause prices to fall toward production cost within 3-5 years. Incumbent owners would incur substantial paper losses; renters and first-time buyers would access housing at affordability levels consistent with income. Financial institutions would recalibrate lending portfolios. The entire institutional chain sustaining current price levels would reorganize.
% FOUNDING_PROBLEM: Post-WWII housing shortage requiring mass construction and standardized financing; need for safe payment systems (mortgage products), neighborhoods with investment certainty (zoning predictability), and managed credit expansion.
% FOUNDING_PROBLEM_CORROBORATION: Independent housing researchers (academic economists, federal housing-policy agencies) document that housing shortages ended in most developed economies by the 1980s. Construction has exceeded demographic growth for 40+ years. Vacancy rates are positive. The founding coordination problems are solved. Incumbent owners and intermediaries contest this verdict and argue shortages persist. Financial regulators split: they maintain loose lending standards to prevent asset-price collapse, treating the founding problem as latently live, but acknowledge in internal documents that supply is adequate to demand outside of severe-restriction jurisdictions.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).

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
 *   Extractiveness is measured at 0.67 (high) because the constraint transfers wealth from payers (first-time buyers, renters, young workers) to beneficiaries (incumbent owners, lenders, intermediaries) through mechanisms that are not transparent to payers and are defended as natural or inevitable rather than as policy choice. The beneficiaries did not invent price scarcity but actively maintain it through zoning preservation, lending standards that presume high prices, and tax provisions that embed price expectations. Suppression is 0.71 (high) because enforcement depends on active political maintenance: incumbent owners lobby against zoning changes, intermediaries lobby against fee transparency and alternative credit products, lenders lobby against standard loosening. Theater ratio at 0.42 (moderate, rising) reflects the rhetorical cover: zoning is justified as neighborhood preservation and environmental protection; lending standards are justified as prudential risk management; tax provisions are justified as encouraging ownership. Each justification contains some truth, but a growing portion of the enforcement activity (particularly zoning and lending standards) defends extraction and scarcity, not coordination. The measurement series show extractiveness saturating at 0.67 after 35 years (suggesting the constraint reached a political equilibrium where further extraction faces increasing resistance), while theater ratio continues to rise (institutional maintenance becomes more performative as the coordination justification weakens). Accessibility collapse and stakes inflation both rise at the individual level (0.50→0.70 and 0.48→0.72) as first-time buyers and renters face narrowing options and rising cost-of-entry. Resistance is highest at the organizational level (homeowner and renter coalitions, housing advocates) and rises across the interval, indicating strengthening pushback on the institutional reading.
 *
 * PERSPECTIVAL GAP:
 *   The institutional reading (this constraint) and the naturalist reading (sibling constraint) are held by different parties and compute to different types. From the incumbent-owner and lender seat, price formation reflects equilibrium (naturalist): policy provides the frame, but prices clear markets reflecting genuine scarcity (limited buildable land, high construction costs, preference for ownership). From the first-time buyer and renter seat, price formation is institutional extraction: policy choices (zoning, lending standards, tax provisions) create artificial scarcity and extract rents. The institutional policymakers split the difference: they treat price formation as quasi-natural (equilibrium within policy constraints) while maintaining the constraints because they prevent perceived threats (neighborhood change, financial instability). The engine computes per-seat directionality from beneficiary/victim declarations: incumbents and lenders get low directionality (d ~0.1-0.3, beneficiary end), buyers and renters get high directionality (d ~0.7-0.9, target end). The agenda-setters (zoning authorities, regulators, tax policymakers) get moderate directionality (d ~0.4-0.6) because they actively maintain the system but do not collect the extraction. The perspectival gap surfaces because the institutional reading asserts beneficiaries and victims exist (enabling asymmetric directionality), while the naturalist reading does not (everyone is coordinated by equilibrium, directionality is symmetric). These are genuinely different constraints, not two measures of the same thing.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent owners: d~0.15 (beneficiary end). They set zoning policy alongside authorities, capture its benefits (asset appreciation), and have low exit friction (can sell at inflated prices if they choose). Power is high (organized, voting bloc), exit is mobile (can exit by selling), so derived directionality favors beneficiary. Institutional lenders: d~0.10 (beneficiary end). They set lending standards, originate larger mortgages at profitable terms, and face no enforcement cost (they simply refuse credit). Power is institutional, exit is arbitrage (can shift portfolio away from housing), directionality strongly favors beneficiary. Real estate intermediaries: d~0.12 (beneficiary end). Commissions scale with price, they lobby against transparency, power is organized (professional associations), exit is mobile (commissions exist in any market, just compressed in competitive ones). First-time buyers: d~0.82 (target end). Trapped by lending standards requiring high down payments and debt-to-income multiples presume high prices; they must accept the price signal as given or exit geographically at high cost. Power is powerless, exit is trapped (employment is local, moving is costly), directionality strongly favors target. Renters: d~0.88 (target end). Identity-locked into place by employment, family, school district; rents track prices; no voice in zoning; cannot organize across fragmented rental markets. Power is powerless, exit is identity-locked (moving breaks social ties), directionality maximally favors target. Zoning authorities: d~0.55 (near-symmetric). They enforce zoning (paying enforcement costs) but do not collect extraction; political pressure from incumbents (low enforcement cost) and housing advocates (high enforcement cost) creates symmetric exposure. Power is institutional, exit is constrained (elected office depends on political coalition), directionality stays near center. Regulators and tax policymakers: d~0.50 (symmetric). They maintain standards but face pressure from opposite sides; fear of asset-price collapse restrains tightening, fear of affordability crises restrains loosening. Directionality is symmetric because the extraction and enforcement are distributed across multiple institutional seats with conflicting pressures. No overrides are needed; the structural derivation produces the correct d-values from beneficiary/victim + exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem (post-WWII housing shortage) is STATUS=DEAD. Residential construction has met and exceeded demographic demand in most developed economies since the 1980s; vacancy rates are positive; no shortage persists in the aggregate. The constraint PERSISTS despite the dead founding problem, indicating mandate drift. The constraint is NOT a piton (performing without benefit) because beneficiaries (incumbent owners, lenders, intermediaries) collect substantial rents and actively maintain it. Instead, the constraint is a clear Tangled Rope: the coordination function (standardizing credit, matching capital to development) was real and necessary in the founding period, but the constraint now performs dual function — coordination + extraction. The extraction is the reason persistence is mandated. The theater ratio rising faster than extractiveness (0.25→0.42 vs 0.45→0.67) indicates growing rhetorical cover and declining real coordination function. If the founding problem were live (supply shortage, coordination failure), we would expect theater ratio to remain low and stable. Instead, theater ratio rises because the institutional frame (zoning scarcity, lending standards) must be justified increasingly through narrative (neighborhood preservation, prudential risk) rather than through demonstrable coordination benefit. This is the classic mandatrophy signature: the constraint survives its founding problem by layering extraction onto a weakened coordination function. Institutional reading captures this pattern; the naturalist reading (founding problem still live, equilibrium persists) misses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_construction_vs_market_equilibrium,
    'Does the price level reflect institutional choice (zoning restriction, lending standards, tax incentives) or equilibrium supply-demand balance? Can the equilibrium be meaningfully separated from the institutions that constrain both supply and demand perception?',
    'Comparative analysis across jurisdictions with different institutional arrangements but similar local conditions (climate, topography, employment patterns). Natural experiment when a jurisdiction deregulates zoning: if prices fall while demand persists, institutional restriction was the marginal constraint.',
    'If institutional construction is dominant, price formation is a policy choice (Tangled Rope classification holds). If equilibrium persists across different institutional arrangements, the constraint is closer to natural (shifts classification toward Mountain or invalidates institutional reading). The naturalist and institutional readings differ fundamentally on this question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_construction_vs_market_equilibrium, empirical, 'Whether price reflects institutional construction or equilibrium scarcity').

omega_variable(
    beneficiary_capture_vs_coordination_benefit,
    'Do institutional actors (lenders, intermediaries, policymakers) maintain lending and zoning standards because the standards serve genuine coordination (stable supply, efficient matching of borrowers to credit), or because the standards generate capture rents from scarcity?',
    'Decompose lending-standard drift: did standards tighten after housing crises (indicating risk management) or loosen when asset prices rose (indicating competitive loosening to maintain volume)? Historical analysis of zoning board minutes: do discussions focus on infrastructure capacity and neighborhood matching, or on preserving property values and preventing supply competition?',
    'If coordination dominates, Tangled Rope classification is correct (coordination + asymmetric extraction). If capture dominates, classification should shift toward Snare (extraction wearing coordination cover). The distinction rides on beneficiary motivation, which is opaque.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_coordination_benefit, empirical, 'Whether institutional standards serve coordination or extract from scarcity').

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the price-formation kernel is correct: institutional (this story), naturalist (equilibrium reflection), georgist (land-rent separation), or financialization (credit-expansion feedback)? These readings are incommensurable on the mechanism but could coexist for different market segments or time periods.',
    'Kernel readings are distinguished by what drives marginal prices: institutional reading says policy choice and regulatory scarcity; naturalist says preference-and-endowment equilibrium; georgist says unearned land rent; financialization says credit conditions and asset-price expectations. Test each by historical analysis of policy changes (zoning deregulation, lending-standard shifts, tax changes) and their price impacts. If prices move with policy more than with income or population, institutional reading is supported. If prices decouple from institutional changes, sibling readings gain ground.',
    'The kernel reading fundamentally shapes classification: institutional reading points to Tangled Rope (policy-constructed beneficiaries and victims); naturalist points to Mountain (equilibrium process, no extractiveness beyond scarcity); georgist points to Snare (land rent extraction masked as natural scarcity); financialization points to Snare (credit expansion as pure extraction). The four readings cannot coexist in a single constraint; they are alternative constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which kernel reading (institutional, naturalist, georgist, financialization) is the correct framing of price formation').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression (0.71) structural (incumbent owners have legal and political power to maintain zoning, lenders have legal power to set standards, intermediaries have market power to resist transparency) or internalized (renters and first-time buyers have accepted scarcity as inevitable, believe prices reflect true value, have internalized the constraint''s frame)?',
    'Post-reform suppression trajectory: if zoning deregulation or lending-standard loosening occurs, does suppression persist in the behavior and expectations of renters and buyers (indicating internalization)? Interview and survey evidence on belief in price inevitability and scarcity narratives. Willingness-to-pay analysis when alternatives become available (do people exit the constraint, or do they remain despite new options).',
    'If suppression is primarily structural, the constraint''s effective extractiveness is what the metric reports (0.67 base extractiveness x high directionality for targets = high effective extraction). If suppression is partially internalized, the targets carry the suppression with them after structural reform (limiting the benefit of institutional change), and the constraint''s true reach is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether suppression is structural institutional power or internalized belief in inevitability').

omega_variable(
    temporal_drift_interpretation,
    'Does the rising extractiveness (0.45→0.67 over 40 years) and rising theater ratio (0.25→0.42) indicate (A) legitimate institutional changes responding to real housing challenges (adding regulations to manage growth), or (B) institutional drift toward pure extraction (regulations layered to extract rents rather than solve coordination problems)?',
    'Decompose regulatory additions: were they adopted in response to measurable coordination failures (affordability crises, default waves, supply instability)? Do the regulations address the stated coordination problem or create new extraction opportunities? Cost-benefit analysis of each major institutional change (e.g., loan-level pricing adjustments, zoning code modifications). Testimony from policymakers on intent and from affected parties on impact.',
    'If interpretation (A) is correct, rising metrics reflect legitimate institutional response and the constraint remains Tangled Rope at elevated but defensible extraction levels. If interpretation (B) is correct, the drift indicates mission creep and the constraint is transitioning toward Snare. The theater ratio rising faster than extractiveness (0.25→0.42 vs 0.45→0.67) suggests some theater is being added, consistent with interpretation (B).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_drift_interpretation, empirical, 'Whether institutional drift reflects problem-solving or rent-extraction layering').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__institutional_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(pric_tr_t5, observed).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__institutional_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(pric_tr_t10, observed).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__institutional_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(pric_tr_t15, observed).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__institutional_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(pric_tr_t25, observed).
narrative_ontology:measurement(pric_tr_t35, price_formation_kernel__institutional_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(pric_tr_t35, observed).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(pric_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__institutional_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(pric_be_t5, observed).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__institutional_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(pric_be_t10, observed).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__institutional_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(pric_be_t15, observed).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__institutional_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(pric_be_t25, observed).
narrative_ontology:measurement(pric_be_t35, price_formation_kernel__institutional_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(pric_be_t35, observed).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(pric_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__institutional_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(pric_su_t5, observed).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__institutional_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(pric_su_t10, observed).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__institutional_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(pric_su_t15, observed).
narrative_ontology:measurement(pric_su_t25, price_formation_kernel__institutional_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(pric_su_t25, observed).
narrative_ontology:measurement(pric_su_t35, price_formation_kernel__institutional_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(pric_su_t35, observed).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pric_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(pric_grid_01, price_formation_kernel__institutional_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(pric_grid_02, price_formation_kernel__institutional_reading, accessibility_collapse(class), 40, 0.68).
narrative_ontology:measurement(pric_grid_03, price_formation_kernel__institutional_reading, accessibility_collapse(individual), 0, 0.5).
narrative_ontology:measurement(pric_grid_04, price_formation_kernel__institutional_reading, accessibility_collapse(individual), 40, 0.7).
narrative_ontology:measurement(pric_grid_05, price_formation_kernel__institutional_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(pric_grid_06, price_formation_kernel__institutional_reading, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(pric_grid_07, price_formation_kernel__institutional_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(pric_grid_08, price_formation_kernel__institutional_reading, accessibility_collapse(structural), 40, 0.62).
narrative_ontology:measurement(pric_grid_09, price_formation_kernel__institutional_reading, resistance(class), 0, 0.55).
narrative_ontology:measurement(pric_grid_10, price_formation_kernel__institutional_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(pric_grid_11, price_formation_kernel__institutional_reading, resistance(individual), 0, 0.45).
narrative_ontology:measurement(pric_grid_12, price_formation_kernel__institutional_reading, resistance(individual), 40, 0.38).
narrative_ontology:measurement(pric_grid_13, price_formation_kernel__institutional_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(pric_grid_14, price_formation_kernel__institutional_reading, resistance(organizational), 40, 0.55).
narrative_ontology:measurement(pric_grid_15, price_formation_kernel__institutional_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(pric_grid_16, price_formation_kernel__institutional_reading, resistance(structural), 40, 0.62).
narrative_ontology:measurement(pric_grid_17, price_formation_kernel__institutional_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(pric_grid_18, price_formation_kernel__institutional_reading, stakes_inflation(class), 40, 0.7).
narrative_ontology:measurement(pric_grid_19, price_formation_kernel__institutional_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(pric_grid_20, price_formation_kernel__institutional_reading, stakes_inflation(individual), 40, 0.72).
narrative_ontology:measurement(pric_grid_21, price_formation_kernel__institutional_reading, stakes_inflation(organizational), 0, 0.6).
narrative_ontology:measurement(pric_grid_22, price_formation_kernel__institutional_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(pric_grid_23, price_formation_kernel__institutional_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(pric_grid_24, price_formation_kernel__institutional_reading, stakes_inflation(structural), 40, 0.65).
narrative_ontology:measurement(pric_grid_25, price_formation_kernel__institutional_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(pric_grid_26, price_formation_kernel__institutional_reading, suppression(class), 40, 0.72).
narrative_ontology:measurement(pric_grid_27, price_formation_kernel__institutional_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(pric_grid_28, price_formation_kernel__institutional_reading, suppression(individual), 40, 0.74).
narrative_ontology:measurement(pric_grid_29, price_formation_kernel__institutional_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(pric_grid_30, price_formation_kernel__institutional_reading, suppression(organizational), 40, 0.7).
narrative_ontology:measurement(pric_grid_31, price_formation_kernel__institutional_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(pric_grid_32, price_formation_kernel__institutional_reading, suppression(structural), 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, zoning_scarcity_maintenance).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mortgage_lending_standards).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, housing_tax_provision_embedding).

% DUAL FORMULATION NOTE:
% The price_formation_kernel decomposes into four structurally distinct readings: institutional (this story), naturalist, georgist, financialization. The institutional reading claims price is constructed by policy choices; the naturalist reading claims price reflects equilibrium; the georgist reading isolates land-rent extraction; the financialization reading emphasizes credit dynamics. Sibling relationships: institutional_reading coexists_with naturalist_reading (held by different policy coalitions simultaneously), forecloses pure_georgist_reading (georgist mechanisms are now institutional-mediated, not direct land-rent collection), influences financialization_reading (credit standards are set by the same institutions that enforce zoning). The four readings cannot coexist in a single constraint; they are alternative ε-invariant constraints with different beneficiary/victim structures and different mechanisms. Each sibling reading's ε value is distinct. Institutional reading claims moderate-to-high extractiveness (0.67 at endpoint) because policy mechanisms actively construct beneficiaries and victims. Naturalist reading would claim lower extractiveness (pure equilibrium has no designed beneficiaries, only differential impact). Georgist reading would claim high extractiveness (land rent is extracted from everyone but concentrated on landowners). Financialization reading would claim high extractiveness (credit expansion extracts through debt service and asset-price volatility). All four stories must be authored separately and linked through this network block. The institutional reading is the entry point to the family; other readings feed into or diverge from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
