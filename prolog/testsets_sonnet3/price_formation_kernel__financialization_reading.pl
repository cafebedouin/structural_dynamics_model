% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   This story instantiates the financialization reading of the
 *   price_formation_kernel: housing prices are driven primarily by credit
 *   expansion, asset-price feedback (rising prices increase collateral value,
 *   which increases available credit, which increases prices further), and
 *   demand for housing as a financial asset rather than as consumed shelter.
 *   Under this reading, mortgage originators, securitizers, and institutional
 *   investors form a genuine coordination structure (matching long-duration
 *   savings to long-duration consumption) that has become substantially
 *   extractive: transaction volume and fee income scale with credit expansion
 *   regardless of whether shelter value has changed, while debt-service
 *   burden and crash-tail risk concentrate on households and, ultimately,
 *   future taxpayers. This is a reading, not the only structurally coherent
 *   account of the same phenomenon — sibling readings (naturalist,
 *   institutional, georgist) describe the same price data through different
 *   causal mechanisms and would assign different beneficiary/victim
 *   structures and different epsilon. This story's epsilon is fixed to the
 *   financialization mechanism's own operation and does not average across
 *   readings.
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
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'b5ac10c9-27b5-412b-84eb-df8dec550310').
narrative_ontology:cs_kernel_codification('b5ac10c9-27b5-412b-84eb-df8dec550310', distributed).
narrative_ontology:cs_authority_grounding('b5ac10c9-27b5-412b-84eb-df8dec550310', distributed).
narrative_ontology:cs_reading_relation('b5ac10c9-27b5-412b-84eb-df8dec550310', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b5ac10c9-27b5-412b-84eb-df8dec550310', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5ac10c9-27b5-412b-84eb-df8dec550310', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('b5ac10c9-27b5-412b-84eb-df8dec550310', foundational, price_level_tracks_leverage_not_scarcity).
narrative_ontology:cs_axiom_status(price_level_tracks_leverage_not_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('b5ac10c9-27b5-412b-84eb-df8dec550310', price_level_tracks_leverage_not_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('b5ac10c9-27b5-412b-84eb-df8dec550310', secondary, asset_price_feedback_is_endogenous_to_credit_supply).
narrative_ontology:cs_axiom_status(asset_price_feedback_is_endogenous_to_credit_supply, holdable).
narrative_ontology:cs_axiom_grounding('b5ac10c9-27b5-412b-84eb-df8dec550310', asset_price_feedback_is_endogenous_to_credit_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('b5ac10c9-27b5-412b-84eb-df8dec550310', credit_availability_price_regime).
narrative_ontology:cs_drift_state('b5ac10c9-27b5-412b-84eb-df8dec550310', post_2008_financial_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5ac10c9-27b5-412b-84eb-df8dec550310', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_originators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, securitization_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_real_estate_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, over_leveraged_recent_purchasers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, future_taxpayers_bailout_liability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originate mortgage credit and set underwriting standards that determine how much leverage households can access. Earn origination fees and interest income scaled to loan volume; benefit directly whenever credit expansion pushes transaction volume and price levels up, independent of whether the underlying shelter value changed. Can tighten or loosen standards countercyclically and can hedge or offload risk through securitization long before a downturn reaches them.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_originators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_originators, beneficiary).

% Package and resell mortgage debt into tradeable instruments, capturing fees on volume and spread on structuring. Benefit from continued credit expansion and asset-price feedback because both increase the flow of originations to securitize. Face limited downside exposure to any single housing market because risk is distributed globally to investors who bought the paper.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, securitization_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Already own housing purchased with leverage; benefit from the feedback loop as rising asset prices increase their equity and borrowing capacity. Also exposed as payers if the same leverage-driven price level collapses in a credit contraction, since their equity is the first thing wiped out. Exit from the price regime would mean selling into a market whose price they depend on.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents, payer).

% Deploy pooled capital (REITs, private equity housing funds, sovereign wealth allocations) into residential real estate specifically because it behaves as a leveraged financial asset with credit-driven appreciation, not because of rental yield alone. Can enter and exit regional markets rapidly in response to interest-rate and credit-availability signals, extracting price appreciation that displaces local buyers.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_real_estate_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Must purchase shelter at whatever price the credit-and-feedback-driven market has produced, competing against leveraged incumbents and institutional capital that treat the same units as investment vehicles. Cannot exit the housing market entirely without giving up shelter itself; can only substitute toward renting, which shifts the cost rather than escaping it.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    powerless, biographical, trapped, regional).

% Bear rent levels that track the same asset-price feedback loop, since landlord acquisition costs and expected appreciation are priced into rent. Have no leverage-based mechanism of their own to participate in the appreciation they are indirectly financing through rent, and geographic mobility is constrained by employment, family, and local labor markets.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renter_households, payer,
    powerless, biographical, trapped, regional).

% Bought near a credit-expansion peak with maximal leverage; carry the full debt-service obligation regardless of whether the underlying asset price later corrects. If a credit contraction phase arrives, they absorb negative equity and default risk first, while the intermediaries who originated and sold the debt have already been paid and exited the risk position.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, over_leveraged_recent_purchasers, payer,
    moderate, biographical, trapped, regional).

% Bear the contingent liability for systemic intervention (bailouts, deposit insurance backstops, central bank balance sheet expansion) if credit-driven price feedback loops end in a financial crisis, even though they neither originated the credit nor captured the appreciation gains along the way. Have no seat in the decisions that expand credit and no mechanism to decline the eventual liability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, future_taxpayers_bailout_liability, payer,
    powerless, generational, trapped, national).

% Set the interest-rate and macroprudential conditions (capital requirements, loan-to-value limits, stress tests) that determine how much credit expansion the system permits. Observe the feedback loop's systemic risk but face political and employment-mandate pressure that historically favors accommodating credit expansion during upswings and only tightening after damage is visible.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_bank_and_prudential_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, central_bank_and_prudential_regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mortgage credit markets genuinely solve a real coordination problem: they let households consume decades of shelter now against decades of future income, and let capital seeking long-duration, collateralized returns fund that consumption. Without some credit mechanism, homeownership would require accumulated cash savings few households have.
% TRANSFER_FUNCTION: The arrangement moves debt-service payments and crash-tail risk from households (especially recent, highly leveraged purchasers and non-owners) to financial intermediaries as fee and spread income during expansion, and moves the residual systemic risk of a credit contraction onto future taxpayers and renters who never captured the appreciation upside.
% ABSENT_VOICES: Renters and first-time buyers as a class have no seat in mortgage-standard-setting, securitization structuring, or interest-rate policy deliberation despite bearing the price level those decisions produce; future taxpayers who would fund any systemic backstop are, by construction, not yet a political constituency capable of objecting in advance.
% DISAPPEARANCE_RATIONALE: If credit-expansion-driven price formation vanished overnight and housing prices reverted to being anchored purely to rental-equivalent shelter value, mortgage origination volumes, securitization markets, and leveraged-incumbent equity would all contract sharply; institutional real estate capital would redeploy elsewhere; but shelter would become dramatically more accessible to first-time buyers and renters, and systemic crash-tail risk carried by future taxpayers would largely disappear.
% FOUNDING_PROBLEM: Mortgage credit markets were built to solve the genuine problem that shelter is a large, lumpy, long-lived good that few households can purchase outright from savings, and that matching long-duration household consumption needs with long-duration investor capital requires standardized, tradeable credit instruments.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and housing-finance industry associations attest the founding problem (illiquid, lumpy shelter finance) remains fully live and justifies continued credit expansion. Independent sources outside the beneficiary set — including post-2008 financial stability reviews, IMF and BIS housing-finance working papers, and academic work on the credit-availability theory of house prices — corroborate that the mechanism has substantially decoupled from the founding problem: price levels now track leverage availability and investor demand for a financial asset more than they track the cost of solving households' shelter-finance problem.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises across the interval (0.42 to 0.78) tracking a credit-expansion cycle: early periods show moderate extraction consistent with genuine intermediation function, later periods show the feedback loop dominating, consistent with the credit-availability theory of house prices (price level tracks leverage availability, not rental-equivalent shelter cost). Theater ratio rises moderately (0.18 to 0.40) as underwriting and stress-testing activity increasingly serves compliance appearance rather than actually constraining credit expansion during the upswing phase. Suppression rises (0.35 to 0.62) as exit options for non-leveraged households narrow — rents track the same feedback loop, so substitution away from ownership does not escape the price regime.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (mortgage originators, regulators during the expansion phase), the arrangement reads as legitimate coordination: it is solving the real problem of matching illiquid long-duration household consumption to long-duration investor capital. From the payer seats (first-time buyers, renters, over-leveraged recent purchasers), the same structure reads as an engine that produces a price level detached from the shelter value they are trying to buy, using credit terms they did not set. The engine should compute these as genuinely different seat-level classifications from the same structural data — that divergence is the point of a tangled_rope classification, not an error to be resolved toward one reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Mortgage originators and securitization intermediaries are structural beneficiaries: fee and spread income scale with volume, and risk is distributed or offloaded before any downturn reaches them, placing them near the full-beneficiary end of directionality. Institutional real estate investors are similarly structured beneficiaries with arbitrage-grade exit — they can redeploy capital across regions and asset classes as credit conditions shift. First-time homebuyers, renters, and over-leveraged recent purchasers are structural targets: they pay the price level the feedback loop produces without having captured any of the appreciation that produced it, and their exit options (trapped) reflect that shelter is not a discretionary purchase. Future taxpayers are targets by construction of contingent liability, with no seat in the decisions that create the exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (matching long-duration shelter consumption to long-duration investor capital) remains partially live — credit intermediation still performs real service — which is why this reading is authored as tangled_rope rather than snare: there is a genuine coordination function underneath the extraction, not pure extraction wearing a coordination costume. The mandatrophy risk is that the coordination framing (mortgage markets solve the illiquidity problem) is used to justify a feedback-loop dynamic (credit expansion drives price appreciation drives more credit capacity) that has decoupled substantially from the founding problem's original scale. Classifying this as tangled_rope rather than snare preserves the distinction between the genuine underlying coordination function and the extractive feedback layered onto it — collapsing it to snare would erase the real service mortgage credit performs; collapsing it to rope would erase the asymmetric extraction the metrics describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financialization_vs_institutional_causal_priority,
    'Is credit expansion the primary causal driver of housing price formation, or is it itself downstream of institutional choices (zoning restrictions on supply, tax treatment of mortgage interest, lending-standard deregulation) that the institutional_reading identifies as prior?',
    'Comparative cross-jurisdiction analysis: compare price trajectories in regions with similar credit-availability conditions but different zoning/supply regimes, and regions with similar zoning regimes but different credit-expansion histories, to isolate which factor explains more price variance.',
    'If institutional constraints are shown to be the binding constraint and credit expansion merely amplifies a scarcity institutional policy created, this reading''s beneficiary/victim structure would need revision to locate the deeper agenda-setter in zoning and tax-policy bodies rather than mortgage originators; if credit expansion drives price independent of supply constraints (as in markets with elastic supply that still show credit-correlated price swings), this reading''s causal priority is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financialization_vs_institutional_causal_priority, conceptual, 'Whether financialization or institutional construction is the deeper causal layer of price formation.').

omega_variable(
    genuine_intermediation_vs_pure_extraction_boundary,
    'How much of mortgage-market fee and spread income reflects the real cost of maturity-transformation and risk-bearing service, versus rent extracted from an information or regulatory advantage over borrowers?',
    'Compare origination and securitization margins across jurisdictions with different disclosure and competition regimes; a persistent margin gap uncorrelated with default risk would indicate rent extraction rather than service cost.',
    'A high genuine-service share would support classifying the beneficiary side closer to legitimate coordination profit; a high rent-extraction share would push the classification toward snare at the mortgage-origination seat specifically, even while the broader tangled_rope classification holds at the system level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_intermediation_vs_pure_extraction_boundary, empirical, 'Whether intermediary income reflects genuine service cost or extracted rent.').

omega_variable(
    feedback_loop_reversibility,
    'Is the credit-expansion/asset-price feedback loop a stable long-run equilibrium mechanism, or does it necessarily terminate in periodic credit contractions (as in 2008) that redistribute the accumulated extraction back onto the intermediaries and beneficiaries who profited during expansion?',
    'Longitudinal study of multiple full credit cycles across different national housing-finance systems, tracking whether intermediary profits are clawed back during contraction phases or remain banked from the expansion phase.',
    'If contraction phases substantially claw back intermediary gains (through litigation, capital requirements, or market share loss), the effective extraction is lower than a snapshot at cycle peak suggests; if intermediary gains from expansion are permanent regardless of subsequent contraction, the measured extraction at cycle peak is a lower bound on total extraction, not an overstatement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feedback_loop_reversibility, empirical, 'Whether credit-cycle contractions redistribute extracted gains back from beneficiaries.').


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
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__financialization_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__financialization_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__financialization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__financialization_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__financialization_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__financialization_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__financialization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__financialization_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__financialization_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__financialization_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.12).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the price_formation_kernel, each authored as a separate, ε-invariant constraint per the ε-invariance principle. The financialization_reading (this file) locates the primary mechanism in credit expansion and asset-price feedback, authoring high extractiveness (0.78) concentrated on mortgage originators/securitizers as beneficiaries and leveraged/non-owning households as payers. The naturalist_reading would author near-zero extraction (price as objective equilibrium, no identifiable beneficiary/victim structure). The institutional_reading would relocate the agenda-setter role to zoning boards, tax authorities, and lending-standard regulators rather than financial intermediaries, likely also producing a tangled_rope or scaffold classification but with a different beneficiary set. The georgist_reading would decompose the same price level into land-rent (unearned, attributable to location monopoly) and improvement value (earned), likely classifying the land-rent component as a snare or tangled_rope with landowners as beneficiaries and everyone paying location rent as victims — a different beneficiary/victim structure entirely from this reading's finance-sector beneficiaries. All four are linked via affects_constraints; none supersedes another, and no story's ε is averaged with a sibling's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
