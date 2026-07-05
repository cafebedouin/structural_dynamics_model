% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   domain: political economy / housing markets / institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the financialization reading of the price
 *   formation kernel: housing prices are set predominantly by the
 *   availability and terms of credit, and by feedback loops in which rising
 *   prices justify further credit extension, which further raises prices,
 *   with housing functioning primarily as a leveraged financial asset rather
 *   than a consumption good tied to shelter value. This is one of four
 *   structurally distinct readings of the same kernel; the naturalist reading
 *   (equilibrium of scarcity and preference), institutional reading
 *   (zoning/lending/tax construction), and georgist reading (unearned land
 *   rent vs. earned improvement value) are separate constraint stories with
 *   their own ε and stakeholder structures, linked here via
 *   network.affects_constraints. This story's ε is stable at the level of the
 *   credit-feedback mechanism only — it does not average across readings.
 *
 * KEY AGENTS:
 *   - mortgage_originators: agenda-setter/beneficiary (institutional/arbitrage) — sets credit supply, earns from volume
 *   - securitization_intermediaries: beneficiary (institutional/arbitrage) — converts local credit into global tradeable asset, exits before local collapse
 *   - leveraged_homeowner_incumbents: beneficiary (moderate/constrained) — benefits from appreciation driven by later entrants' leverage
 *   - institutional_real_estate_investors: beneficiary (institutional/arbitrage) — treats housing as yield asset, competes with occupant buyers
 *   - first_time_buyer_households: payer (powerless/trapped) — faces price set by others' credit access, not own income
 *   - renter_households: payer (powerless/trapped) — excluded from price-setting market entirely
 *   - over_leveraged_late_cycle_buyers: payer (powerless/trapped) — bears crash risk asymmetrically
 *   - downstream_taxpayers_at_crisis_resolution: payer (powerless/trapped) — backstops systemic failure with no role in causing it
 *   - central_banks_and_credit_regulators: agenda-setter (institutional/analytical) — controls the leverage tap but faces pressure not to disrupt inflated asset values
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
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political economy / housing markets / institutional analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'f2f7f789-2768-4654-880e-3b3204f70f59').
narrative_ontology:cs_kernel_codification('f2f7f789-2768-4654-880e-3b3204f70f59', distributed).
narrative_ontology:cs_authority_grounding('f2f7f789-2768-4654-880e-3b3204f70f59', distributed).
narrative_ontology:cs_reading_relation('f2f7f789-2768-4654-880e-3b3204f70f59', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f2f7f789-2768-4654-880e-3b3204f70f59', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('f2f7f789-2768-4654-880e-3b3204f70f59', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('f2f7f789-2768-4654-880e-3b3204f70f59', foundational, leverage_availability_is_dominant_price_determinant).
narrative_ontology:cs_axiom_status(leverage_availability_is_dominant_price_determinant, holdable).
narrative_ontology:cs_axiom_grounding('f2f7f789-2768-4654-880e-3b3204f70f59', leverage_availability_is_dominant_price_determinant, empirically_contingent).
narrative_ontology:cs_axiom('f2f7f789-2768-4654-880e-3b3204f70f59', secondary, housing_price_is_decoupled_from_shelter_use_value).
narrative_ontology:cs_axiom_status(housing_price_is_decoupled_from_shelter_use_value, holdable).
narrative_ontology:cs_axiom_grounding('f2f7f789-2768-4654-880e-3b3204f70f59', housing_price_is_decoupled_from_shelter_use_value, empirically_contingent).
narrative_ontology:cs_reference_frame('f2f7f789-2768-4654-880e-3b3204f70f59', income_scaled_amortized_credit_financing).
narrative_ontology:cs_drift_state('f2f7f789-2768-4654-880e-3b3204f70f59', post_2008_leverage_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f2f7f789-2768-4654-880e-3b3204f70f59', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_originators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, securitization_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_real_estate_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyer_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, over_leveraged_late_cycle_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, downstream_taxpayers_at_crisis_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originate and distribute mortgage credit, setting underwriting standards that expand or contract the credit supply feeding housing demand. Earn origination fees and volume-based revenue that rises directly with transaction turnover and loan size, independent of whether the resulting price level reflects shelter value. Can tighten or loosen standards pro-cyclically and exit into other lending lines when housing volume falls.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_originators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_originators, beneficiary).

% Package mortgage debt into tradeable instruments, converting local housing credit into a global asset class. Capture fees at each securitization step and benefit from the volume the feedback loop generates. Positioned to move capital elsewhere once a given housing market's price-appreciation narrative decays, largely insulated from the localized collapse that follows.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, securitization_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Already own housing purchased before the current leverage cycle intensified; benefit from asset-price appreciation driven by expanding credit access for later entrants, which inflates the value of their existing holding. Their gains are realized on paper and through home-equity extraction; they have some exit via sale but face reinvestment risk in the same inflated market.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_homeowner_incumbents, beneficiary,
    moderate, generational, constrained, national).

% Deploy pooled capital to acquire housing stock as a yield-bearing asset class, competing directly with owner-occupant buyers and treating price appreciation as the primary return driver rather than shelter provision. Can reallocate capital across metros and asset classes at will, arbitraging local price cycles other participants cannot exit.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_real_estate_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Must borrow at prevailing leverage terms to enter a market whose price level is set by the credit availability and investor demand of prior entrants, not by their own income or the shelter value of the unit. Priced out at the margin as feedback loops push valuations upward, or forced into maximal debt service when they do enter, with no comparable arbitrage exit.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyer_households, payer,
    powerless, biographical, trapped, local).

% Excluded from ownership by rising entry prices driven by the credit-and-investment feedback loop, and pay rents that institutional landlords set partly to service the same asset-price expectations. Have essentially no influence over local price formation and limited mobility given housing scarcity elsewhere.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renter_households, payer,
    powerless, biographical, trapped, local).

% Enter near the peak of the credit-driven appreciation cycle, taking on debt sized to inflated valuations. When the feedback loop reverses, they carry negative equity and debt service obligations that do not adjust downward with the collapsing asset price, and cannot exit without realizing the loss.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, over_leveraged_late_cycle_buyers, payer,
    powerless, biographical, trapped, local).

% Bear the fiscal cost when credit-driven housing cycles collapse and require public backstops of lenders, guarantors, or the broader financial system, despite having no direct role in setting underwriting standards or investment flows that produced the fragility.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, downstream_taxpayers_at_crisis_resolution, payer,
    powerless, generational, trapped, national).

% Set interest rates and macroprudential credit standards that directly govern how much leverage is available to housing markets, and therefore how strongly the credit-asset-price feedback loop can operate. Can tighten or loosen the loop's fuel supply but face political and financial-stability pressure not to disrupt asset prices once households and institutions are leveraged against them.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks_and_credit_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, securitization_intermediaries).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Credit intermediation genuinely solves a real problem: matching household demand for shelter finance with capital willing to fund long-duration housing debt, at a scale individual buyer-seller transactions could not achieve without organized lending and secondary markets.
% TRANSFER_FUNCTION: Moves debt-service payments and crash risk from financial intermediaries and institutional investors (who capture fees and appreciation gains during expansion) onto later-entering and lower-power households (who bear entry prices set by leverage availability rather than income, and absorb losses when the credit cycle reverses) and ultimately onto taxpayers at points of systemic backstop.
% ABSENT_VOICES: Future households not yet in the housing market have no voice in current underwriting standards that will determine the price level they face; renters as a class are structurally outside the price-setting process entirely, since price formation occurs in the transaction market they cannot access.
% DISAPPEARANCE_RATIONALE: If credit-driven asset-price feedback in housing were removed overnight — for example through a hard leverage cap decoupling price from credit availability — origination and securitization volumes would collapse, existing leveraged homeowners would see equity gains vanish, institutional investors would exit the asset class, and price levels would fall toward income-supportable or replacement-cost levels; a large share of current financial-sector housing revenue would disappear.
% FOUNDING_PROBLEM: Individual households cannot self-finance housing purchases from savings alone; mortgage credit intermediation was built to pool capital and extend long-duration financing so home purchase was possible on an income stream rather than requiring full cash payment.
% FOUNDING_PROBLEM_CORROBORATION: Central bank financial-stability reports and academic housing-finance economists outside the origination and securitization industries attest that credit intermediation's original financing function persists but has been substantially overtaken by asset-appreciation-driven demand and speculative leverage cycles; mortgage originators and securitization intermediaries themselves attest the arrangement remains purely functional financing, which is the self-interested reading this corroboration is checked against.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) and rising over the interval because the mechanism's defining feature is that price gains accrue disproportionately to intermediaries and existing leveraged holders while entry costs and crash risk concentrate on later, less powerful entrants — this is a widening structural asymmetry, not a stable coordination cost. Suppression (0.62) reflects that alternatives to leveraged entry (e.g., income-scaled pricing, non-debt-financed housing markets) are actively foreclosed once credit-driven appreciation becomes the dominant price-setting mechanism, since abstaining from leverage means being priced out entirely. Theater ratio (0.4) captures that a substantial share of 'prudential' credit-market activity (rating agency certification, underwriting compliance theater) increasingly serves to legitimate continued volume rather than to constrain the feedback loop. All three tracked metrics share one time grid across 6 points from t=0 to t=40.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, credit expansion is genuine market-deepening: more capital available to more buyers, more efficient price discovery, more liquid secondary markets. From the payer seats, the identical credit expansion is what sets an unaffordable price floor, since their purchasing power is now benchmarked against leveraged competitors and cheap capital rather than income. The engine should compute these as diverging seat classifications from the same structural facts, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mortgage originators, securitization intermediaries, institutional investors, existing leveraged homeowners) sit near the beneficiary end of directionality: they capture fees, appreciation, or both, and hold arbitrage-grade or near-arbitrage exit options that let them extract value from the cycle without bearing its downside symmetrically. Payer seats (first-time buyers, renters, late-cycle buyers, downstream taxpayers) are structurally trapped: they cannot opt out of the price level the feedback loop sets, since housing is a near-mandatory good, and they bear either inflated entry costs, exclusion, crash losses, or fiscal backstop costs. Central banks are placed as agenda-setters with analytical exit, reflecting their capacity to alter the credit supply mechanically even though political economy constrains their willingness to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling home purchase via income-financed credit rather than requiring full cash payment — remains genuinely live for many households, which prevents this constraint from being classified as a pure snare with no coordination function; that is precisely why it is authored as tangled_rope rather than snare. But the founding_problem_status is contested rather than dead or unambiguously live: the credit intermediation function has been substantially overtaken by asset-appreciation-driven and speculative dynamics that serve intermediary volume and existing-holder wealth rather than the marginal buyer's financing need. The coordination function and the extraction mechanism run through the identical structure (mortgage credit issuance), which is the tangled-rope signature — the same lending pipe that finances a family's home purchase is the pipe that fuels the feedback loop pricing the next family out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_vs_scarcity_causal_weight,
    'How much of observed housing price variance is attributable to credit-supply/leverage-availability shifts versus genuine scarcity (the naturalist reading) or institutional constraints on supply (the institutional reading)?',
    'Comparative empirical study across jurisdictions with similar scarcity/zoning profiles but divergent credit-availability regimes (e.g., loan-to-value cap variation); if price divergence tracks credit terms more than physical supply, the financialization reading''s causal claim strengthens.',
    'If credit expansion explains only a modest share of price variance relative to scarcity or zoning, this reading''s extraction claim weakens substantially and the constraint may be better characterized closer to the naturalist or institutional reading''s classification; if credit dominates, the tangled_rope classification with high extractiveness is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_vs_scarcity_causal_weight, empirical, 'Whether credit expansion or scarcity/institutional constraints is the dominant price-formation driver.').

omega_variable(
    reading_kernel_disaggregation_location,
    'Where exactly does the financialization reading''s claim diverge from the institutional reading''s claim, given that credit standards are themselves an institutional choice (set by regulators and originators)?',
    'Distinguish the causal claim (credit availability drives price via leverage-feedback dynamics, this story) from the constructedness claim (zoning/tax/lending-standard architecture is deliberately engineered, the institutional reading) — the two can be jointly true, but this story isolates the feedback-loop mechanism specifically, not the full architecture that permits it.',
    'If the two readings are found to be non-severable at the mechanism level, they may need to be merged or one subsumed into the other''s network edge as a direct causal dependency rather than a parallel sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_disaggregation_location, conceptual, 'Where the boundary sits between the credit-feedback mechanism (this reading) and the broader institutional architecture (sibling reading) that enables it.').

omega_variable(
    central_bank_agenda_setter_capture,
    'Do central banks and credit regulators function as genuinely independent agenda-setters capable of deflating the feedback loop, or are they functionally captured by financial-stability concerns that make them de facto co-beneficiaries of continued asset-price support?',
    'Examine historical instances of tightening cycles: did regulators sustain tightening through a genuine price correction, or reverse course at the first sign of asset-price stress, revealing an asymmetric reaction function that favors incumbents?',
    'If regulators are functionally captured, they should be reclassified from agenda_setter/analytical toward a beneficiary-adjacent directionality, which would raise the constraint''s effective extractiveness further; if genuinely independent, the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_agenda_setter_capture, empirical, 'Whether monetary and macroprudential authorities are independent agenda-setters or functionally aligned with financial-sector beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__financialization_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__financialization_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__financialization_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__financialization_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__financialization_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__financialization_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__financialization_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__financialization_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__financialization_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__financialization_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__financialization_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__financialization_reading, suppression_requirement, 32, 0.59).
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
% This story is one of four constraint files decomposing the natural-language concept 'housing price formation' per the ε-invariance principle. Each sibling reading (naturalist, institutional, georgist) locates the causal/extractive mechanism differently and carries its own ε, beneficiary/victim structure, and classification. This file's claim is specifically that credit-expansion and asset-price feedback dynamics are the dominant price-setting mechanism and that this mechanism is substantially extractive (tangled_rope); it does not assert this is the only valid description of housing price formation, only that it is a structurally distinct and internally coherent one requiring its own ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
