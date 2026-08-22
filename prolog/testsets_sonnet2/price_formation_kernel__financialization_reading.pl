% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__financialization_reading, []).

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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation via Credit Expansion and Asset-Price Feedback (Financialization Reading)
 *   domain: political_economy/housing_markets/finance
 *
 * SUMMARY:
 *   This story is the financialization reading of the price formation kernel:
 *   it treats housing prices as substantially determined by credit expansion,
 *   asset-price feedback (rising prices enable more borrowing, more borrowing
 *   supports further price rises), and demand for housing as a financial
 *   asset rather than as shelter. Under this reading, the
 *   mortgage-and-securitization apparatus performs a genuine coordination
 *   function (pooling dispersed capital to finance individual home purchases)
 *   while simultaneously generating an asymmetric extraction structure —
 *   financial intermediaries and asset-holding incumbents capture origination
 *   fees, transaction volume, and appreciation gains, while later-entering
 *   households, renters, and eventually taxpayers bear debt service,
 *   affordability compression, and crash-cycle costs. This is a Tangled Rope:
 *   the coordination function (capital pooling for home purchase) and the
 *   extraction function (rent extraction through leverage-driven price
 *   inflation) run through the identical structure and cannot be separated
 *   without dismantling the arrangement itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.62).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation via Credit Expansion and Asset-Price Feedback (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/finance").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'c3e7e788-2cf7-466e-aa51-7e033c9d27fc').
narrative_ontology:cs_kernel_codification('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', distributed).
narrative_ontology:cs_authority_grounding('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', distributed).
narrative_ontology:cs_reading_relation('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_axiom('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', foundational, price_is_leverage_dependent_not_equilibrium_neutral).
narrative_ontology:cs_axiom_status(price_is_leverage_dependent_not_equilibrium_neutral, holdable).
narrative_ontology:cs_axiom_grounding('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', price_is_leverage_dependent_not_equilibrium_neutral, empirically_contingent).
narrative_ontology:cs_axiom('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', foundational, asset_price_feedback_generates_asymmetric_intermediary_extraction).
narrative_ontology:cs_axiom_status(asset_price_feedback_generates_asymmetric_intermediary_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', asset_price_feedback_generates_asymmetric_intermediary_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', postwar_mortgage_intermediation_baseline).
narrative_ontology:cs_drift_state('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', post_2008_global_financial_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3e7e788-2cf7-466e-aa51-7e033c9d27fc', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_originators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, securitization_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, incumbent_homeowners_with_leverage).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_real_estate_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyer_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, over_leveraged_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, post_crash_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originates and prices mortgage credit, setting the effective leverage terms that feed directly into what buyers can bid. Earns origination fees and interest spread scaled to loan volume, so expanding credit availability — even when it inflates prices rather than expanding shelter supply — increases revenue. Can adjust underwriting standards in response to regulatory or market signals faster than households can adjust their housing decisions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_originators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_originators, beneficiary).

% Bundles and resells mortgage debt into tradeable instruments, converting local housing debt into globally distributed financial assets. Profits from transaction volume and spread rather than from the underlying shelter value, and can exit or hedge exposure ahead of a price correction using instruments unavailable to the households whose mortgages constitute the underlying asset.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, securitization_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Already owns property purchased before or during earlier phases of credit expansion; benefits as asset-price feedback (rising prices support more borrowing, which supports further price rises) inflates paper equity. Can sell into the run-up, borrow against appreciated equity, or ride out a downturn using accumulated equity as a buffer, none of which is available to buyers entering after prices have risen.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, incumbent_homeowners_with_leverage, beneficiary,
    moderate, biographical, mobile, regional).

% Deploys pooled capital into residential real estate as a financial asset class, competing directly with owner-occupant buyers for the same housing stock and treating price appreciation, not shelter, as the return objective. Can shift capital across metro areas or asset classes in response to yield changes, absorbing local downturns that would devastate a household confined to one home.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_real_estate_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Must borrow at whatever leverage terms and price levels the credit-and-asset-feedback cycle has produced by the time they enter the market. Bears the full debt-service burden on prices set by the interaction of credit supply and prior asset appreciation, not by local wages or construction cost, with no ability to time entry or exit.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyer_households, payer,
    powerless, biographical, trapped, local).

% Pays rent set with reference to the financialized purchase price of the asset (landlord's required return on inflated acquisition cost) rather than to the underlying cost of providing shelter. Has no ownership stake to benefit from the appreciation that drives their rent upward and cannot exit the local housing market without abandoning employment, schooling, or social ties.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renter_households, payer,
    powerless, immediate, trapped, local).

% Purchased near the peak of a credit-driven price run-up using maximum available leverage; when the feedback loop reverses, carries debt exceeding the asset's collapsed value. Cannot sell without realizing a loss that exceeds available savings, and cannot easily refinance once the collateral value has fallen below the loan balance.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, over_leveraged_homeowners, payer,
    powerless, biographical, trapped, local).

% Bears the fiscal cost of stabilizing the financial system when the credit-asset feedback loop unwinds — bailouts, deposit guarantees, monetary easing that reinflates asset prices again. Pays through taxation and currency debasement regardless of whether they ever participated in the housing market as buyer, renter, or investor.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, post_crash_taxpayers, payer,
    powerless, generational, trapped, national).

% Sets interest rates and macroprudential lending rules that directly govern how much credit expansion feeds into the price-formation loop. Can tighten or loosen the constraint's operating conditions but has historically prioritized financial-system stability over housing affordability, making it both an observer of the dynamic and a co-architect of its persistence.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_bank_and_credit_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, central_bank_and_credit_regulators, agenda_setter).

% Studies the correlation between credit availability, leverage terms, and price levels independent of local shelter costs. Produces evidence used by both critics and defenders of the arrangement but has no direct power to alter lending standards or credit policy.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_policy_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mortgage credit intermediation genuinely solves a real problem: most households cannot pay for housing in cash, and pooling capital through banks and securitization allows large numbers of buyers to be financed from dispersed savings and investment capital. This coordination function is real and would need to be replaced by something if the current arrangement vanished.
% TRANSFER_FUNCTION: Moves origination fees, interest spread, and transaction volume from borrowing households to lenders and securitization intermediaries; moves paper equity gains from later entrants to earlier entrants and incumbent asset holders as the feedback loop inflates prices; moves systemic crash risk from those who profited during the expansion phase to taxpayers and over-leveraged households when the cycle reverses.
% ABSENT_VOICES: Future households who have not yet entered the housing market have no voice in current credit-expansion and lending-standard decisions that will determine the price level they face; renters as a class are rarely represented in monetary policy deliberation that treats asset-price stability as a proxy for household welfare.
% DISAPPEARANCE_RATIONALE: If credit-driven asset-price feedback in housing were removed overnight — for instance through a hard cap on loan-to-income ratios decoupling price from leverage availability — mortgage origination volume, securitization markets, and paper household wealth built on appreciated home equity would all contract sharply; price levels would fall toward a level tracking local income and construction cost rather than available credit, restructuring household balance sheets, bank capital positions, and municipal tax bases built on assessed property values.
% FOUNDING_PROBLEM: Housing finance systems were built to solve a genuine capital problem: individual households cannot save enough cash to buy a home outright, and mortgage credit intermediation allows dispersed savings to be channeled into long-duration housing loans, enabling broad-based homeownership.
% FOUNDING_PROBLEM_CORROBORATION: Central bank officials and mortgage industry representatives attest the founding problem — enabling homeownership through credit — remains live and is being served. Independent housing economists, post-2008 financial crisis inquiry commissions, and international bodies such as the IMF and BIS attest, from outside the lending and securitization industries, that the arrangement has shifted from enabling homeownership to amplifying speculative price cycles, citing the decoupling of price-to-income ratios from historical norms across multiple credit cycles.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__financialization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) reflects the widening gap between price levels and local income/construction cost as leverage availability becomes the dominant price-setting variable rather than shelter utility. Suppression (0.62) is moderate-high: exit is constrained mainly by the practical unavailability of unlevered alternatives (renting recycles the same financialized price signal; building new supply is constrained by land and zoning, which this reading treats as background, not causal). Theater ratio (0.4) captures the growing share of policy and industry communication that frames credit expansion as 'expanding access to homeownership' even as it increasingly serves paper-wealth generation and transaction volume for intermediaries. All three time series share one grid (0, 8, 16, 24, 32, 40) tracing multiple credit cycles.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (mortgage originators, regulators), expanding credit access looks like coordination success — more households financed, more transaction volume, deeper capital markets. From the payer seats (first-time buyers, renters, over-leveraged owners), the identical credit expansion appears as an engine that raises the price they must pay faster than their income can track, converting a coordination mechanism into an extraction mechanism from their vantage point. The engine computes this divergence from the structural power/exit data; the claimed type does not resolve it in advance.
 *
 * DIRECTIONALITY LOGIC:
 *   Mortgage originators, securitization intermediaries, incumbent leveraged homeowners, and institutional investors are structural beneficiaries: each captures fee income, spread, or appreciation from the credit-price feedback loop and holds exit options (arbitrage or mobility) unavailable to those without existing equity or institutional balance sheets. First-time buyers, renters, over-leveraged late entrants, and post-crash taxpayers are structural targets: each bears debt service, rent, negative equity, or fiscal cost set by dynamics they cannot individually influence and cannot exit without abandoning employment, housing, or citizenship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling broad homeownership through pooled credit — was real and, under this reading, has been substantially captured: the mechanism built to solve household capital constraints now amplifies speculative price cycles that make the same capital constraint worse for later entrants. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (mortgage credit does let dispersed savings finance individual purchases) while still registering the asymmetric extraction, avoiding both the error of dismissing housing finance as pure predation and the error of treating persistent affordability decline as natural market outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_vs_supply_causal_weight,
    'How much of observed housing price appreciation is attributable to credit expansion and leverage availability versus genuine supply constraints (zoning, construction cost, land scarcity) that the institutional and georgist readings foreground?',
    'Cross-national comparison of price-to-income ratios controlling for credit availability and supply elasticity; natural experiments from macroprudential lending tightening episodes (e.g. debt-to-income caps) that isolate the credit channel from the supply channel.',
    'If credit expansion explains the dominant share of price variance even in supply-elastic markets, the financialization reading is strongly supported over the naturalist and institutional readings for those markets. If supply constraints dominate even under expansive credit, the institutional reading better explains the same price data using the identical observable — which is why these are authored as separate constraints rather than one story with a shared parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_vs_supply_causal_weight, empirical, 'Whether credit expansion or supply constraint is the dominant causal driver of price formation.').

omega_variable(
    coordination_extraction_separability_financialization,
    'Is the fee/spread/appreciation-capture structure of mortgage securitization separable from its capital-pooling coordination function, or does the coordination function require the extraction as its funding mechanism?',
    'Comparative study of housing finance systems with different securitization intensity (e.g. jurisdictions with covered-bond systems retaining originator risk versus originate-to-distribute securitization) and their respective price-volatility and extraction profiles.',
    'If lower-extraction credit intermediation models achieve comparable homeownership coordination without the same asset-price feedback intensity, this reading''s extraction component is contingent rather than structurally necessary, strengthening reform arguments; if not, the tangled_rope classification''s coordination/extraction fusion is closer to irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_financialization, empirical, 'Whether the credit-expansion extraction mechanism is separable from its coordination function.').

omega_variable(
    kernel_framing_indeterminacy_financialization,
    'Given that the same observed price data (rising price-to-income ratios, volatile boom-bust cycles) is claimed by this reading as evidence of credit-driven feedback and by the institutional reading as evidence of zoning/tax-driven scarcity, is there a decisive observable that would favor one reading''s causal structure over the other, or does the choice of kernel reading remain underdetermined by the price data alone?',
    'This is a conceptual omega, not resolvable by additional price data alone; it would require either a structural model that decomposes price variance by verified causal channel (credit terms vs. permitting restrictions) with strong identification, or an acknowledgment that the readings are genuinely incommensurable framings serving different policy communities.',
    'If the readings remain underdetermined by available data, both this story and the institutional_reading persist as live, contested alternative constraints rather than one being empirically superseded by the other — consistent with treating them as distinct constraint stories under the ε-invariance principle rather than resolving them into a single hybrid classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_indeterminacy_financialization, conceptual, 'Whether the financialization and institutional readings of price formation are empirically distinguishable or genuinely incommensurable framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__financialization_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__financialization_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__financialization_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__financialization_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__financialization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__financialization_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__financialization_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__financialization_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__financialization_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__financialization_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__financialization_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__financialization_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.12).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the price_formation_kernel, each authored as a separate constraint story per the ε-invariance principle: financialization_reading (this file, tangled_rope, high extraction concentrated in financial intermediaries), institutional_reading (constructed-scarcity account foregrounding zoning/lending/tax), naturalist_reading (equilibrium account treating price as reflecting objective scarcity/preference, plausibly a mountain or rope claim with much lower authored extraction), and georgist_reading (land-rent decomposition account, likely tangled_rope or snare with land-value beneficiaries as the extraction locus rather than credit intermediaries). Each story has its own ε, own beneficiary/victim structure, and own claimed_type; none averages or blends the others. The naturalist_reading is expected to be the most contested sibling from this reading's perspective, since this reading's foundational axiom directly denies naturalist price-formation neutrality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
