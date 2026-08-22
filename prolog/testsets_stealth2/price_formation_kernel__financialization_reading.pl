% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Credit-Led Housing Price Formation (Financialization Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   In advanced economies since the 1980s, housing prices have formed
 *   predominantly through credit availability, collateral feedback loops, and
 *   demand for housing as a financial asset rather than shelter alone:
 *   mortgage credit expands against rising collateral values, appreciation
 *   draws further leveraged demand, and price levels decouple from rental
 *   value and construction cost. This file instantiates the financialization
 *   reading of the contested price_formation_kernel as a single
 *   epsilon-invariant constraint: the sibling readings (naturalist,
 *   institutional, georgist) are separate stories with their own structures,
 *   and the reading contest is routed to the kernel_reading_position omega
 *   rather than folded into this classification. Claim and metrics are
 *   independent authored facts: the constraint is CLAIMED as tangled_rope
 *   (genuine long-horizon credit coordination plus enforced asymmetric
 *   extraction), and the metrics below describe the arrangement's actual
 *   operation without tuning toward any computed verdict.
 *
 * KEY AGENTS:
 *   - - mortgage_lending_institutions: Primary beneficiary and agenda-setter (institutional/arbitrage) — originates and securitizes; collects interest and fees on volume
 *   - - central_banks_and_regulators: Enforcement arm (institutional/constrained) — sets rates, standards, and crisis backstops
 *   - - existing_homeowners: Secondary beneficiary (organized/mobile) — holds the appreciating asset while carrying debt
 *   - - institutional_landlords: Concentrated beneficiary (powerful/arbitrage) — acquires housing stock as yield assets
 *   - - first_time_buyers: Primary target (moderate/trapped) — takes maximal leverage at peak prices
 *   - - renter_households: Primary target (powerless/constrained) — rents track asset values, no equity accrues
 *   - - priced_out_young_households: Excluded voice (powerless/trapped) — absent from the forums where standards are set
 *   - - international_stability_bodies: Analytical observer (institutional/analytical) — documents decoupling without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.74).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.62).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Credit-Led Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "economic/political").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'a04c9d16-d494-41ef-949a-f86bd2c28519').
narrative_ontology:cs_kernel_codification('a04c9d16-d494-41ef-949a-f86bd2c28519', distributed).
narrative_ontology:cs_authority_grounding('a04c9d16-d494-41ef-949a-f86bd2c28519', distributed).
narrative_ontology:cs_reading_relation('a04c9d16-d494-41ef-949a-f86bd2c28519', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a04c9d16-d494-41ef-949a-f86bd2c28519', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('a04c9d16-d494-41ef-949a-f86bd2c28519', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('a04c9d16-d494-41ef-949a-f86bd2c28519', foundational, credit_expansion_primary_price_driver).
narrative_ontology:cs_axiom_status(credit_expansion_primary_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('a04c9d16-d494-41ef-949a-f86bd2c28519', credit_expansion_primary_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('a04c9d16-d494-41ef-949a-f86bd2c28519', foundational, housing_valued_as_collateral_not_shelter).
narrative_ontology:cs_axiom_status(housing_valued_as_collateral_not_shelter, holdable).
narrative_ontology:cs_axiom_grounding('a04c9d16-d494-41ef-949a-f86bd2c28519', housing_valued_as_collateral_not_shelter, empirically_contingent).
narrative_ontology:cs_reference_frame('a04c9d16-d494-41ef-949a-f86bd2c28519', credit_led_price_formation).
narrative_ontology:cs_drift_state('a04c9d16-d494-41ef-949a-f86bd2c28519', contemporary_post_2020, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a04c9d16-d494-41ef-949a-f86bd2c28519', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_lending_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, existing_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_landlords).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renter_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, existing_homeowners).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, minsky_financial_instability_hypothesis).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, collateral_feedback_amplification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originate mortgages and package them into securities; earn interest spreads plus origination and servicing fees on every transaction. Revenue scales with lending volume, so expanded credit and rising collateral values enlarge the business. Default losses are partly passed to bond investors and, in crises, to public backstops. Capital can be redeployed across borders and asset classes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_lending_institutions, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_lending_institutions, beneficiary).

% Set policy rates, capital and underwriting standards, and crisis liquidity facilities. Stability and employment mandates commit them to supporting collateral values in downturns. They cannot step outside the system they manage; their instruments operate through the credit channels the arrangement runs on.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Hold the appreciating asset; paper gains fund retirement, mobility, and inheritance. They also carry mortgage debt and bear repricing risk on recent purchases. Organized as an electoral bloc and through neighborhood associations to defend price-supporting policies. Selling captures gains into the same market.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, existing_homeowners, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, existing_homeowners, payer).

% Acquire housing as yield-bearing portfolio assets at financing scales households cannot reach; returns arrive as rent streams and appreciation. Portfolios rebalance across metros and currencies, converting local housing into a globally traded asset class.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_landlords, beneficiary,
    powerful, generational, arbitrage, global).

% Buy at prevailing prices on prevailing credit terms, which means taking on maximum leverage when prices are highest. Debt service absorbs a large income share and repricing risk lands with the thinnest equity cushion. Stepping aside means indefinite exclusion from owned housing where they work.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyers, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, first_time_buyers, beneficiary).

% Pay rents that track asset values and landlord financing costs; no equity accrues. Moving relocates them within the same price structure. Their occupancy is what makes asset yields real, but they hold no seat in credit or planning decisions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renter_households, payer,
    powerless, immediate, constrained, local).

% Cannot clear down-payment and debt-service thresholds at current price-to-income levels; they delay household formation, commute from cheaper peripheries, or leave. Absent from the forums where lending standards and tax treatment are set.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, priced_out_young_households, excluded,
    powerless, generational, trapped, national).

% Document price-to-income decoupling, credit-to-GDP gaps, and cross-border flows into housing; publish warnings and comparative analysis. No enforcement power over national arrangements; the seat is diagnostic.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, international_stability_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, mortgage_lending_institutions).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels household savings into residential construction and spreads home purchase over decades through standardized long-term credit; provides payment infrastructure, underwriting, and liquidity that no individual household could assemble alone.
% TRANSFER_FUNCTION: Moves debt-service payments and origination and servicing fees from borrowing households to lenders and investors; moves appreciation claims to incumbent owners and institutional holders; in downturns, moves crash losses from private balance sheets to public backstops.
% ABSENT_VOICES: Renters and priced-out young households hold no seat where lending standards, tax treatment, and crisis backstops are designed; future generations inherit the debt stock and the repriced market without representation; tenant organizations are consulted late or not at all.
% DISAPPEARANCE_RATIONALE: If credit-driven price formation stopped overnight, prices would re-anchor toward rental value and replacement cost, incumbent balance sheets would absorb large losses, lender revenue models would collapse, and construction finance would reorganize around non-leveraged channels — the entire housing-finance economy depends on the arrangement.
% FOUNDING_PROBLEM: Postwar housing policy needed to finance mass homeownership: mobilize savings into construction, stretch purchase costs over working lifetimes, and standardize underwriting so ordinary households could borrow safely.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and industry bodies attest the financing function remains live (credit access still gates ownership). Corroboration from outside the benefiting parties: legislative affordability inquiries, IMF and BIS financial-stability analyses documenting price-to-income decoupling, and post-crisis commissions such as the US Financial Crisis Inquiry Commission attest that the arrangement now operates substantially as asset-price support rather than ownership broadening.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.74: price-to-income ratios across advanced economies roughly doubled after 2000 while real wages stagnated; debt service absorbs record household income shares; origination, refinancing churn, and securitization spreads convert every price increase into interest and fee flow. Suppression 0.62 is structural, not theatrical: every household must house itself, both tenure channels price off the same leveraged asset market, and exit means leaving asset accumulation entirely — suppression is authored as raw unscaled structure. Theater 0.31: underwriting, payment rails, and liquidity provision are real functions, but a growing share of activity is performative — risk models that rated crisis-era instruments top-grade, responsible-lending rituals layered over volume-driven incentives. Accessibility_collapse 0.52: once the leverage mechanism is visible, individual alternatives (renting, waiting) do not escape the structure, and collective alternatives (land-value taxation, public housing, hard credit caps) remain politically blocked rather than physically impossible. Resistance 0.58: affordability politics, tenant organizing, and macroprudential pushback are persistent but have not displaced the arrangement. Temporal grid: T0-T40 index 1980-2020 in eight-year steps; all points observed, one shared grid across all three tracked metrics. The extractiveness series rises secularly with a 2004 peak-bubble high, a 2012 crash-repricing dip, and a 2020 policy-reinflated new high. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: securitization infrastructure, the 2008 bailout precedent, and quantitative-easing commitments hardened the enforcement machinery through 2012 before partial normalization. Receipt: gains demonstrably accrue to the lending complex (interest, fees, securitization spreads), so gain_flow names that seat rather than diffuse; fixing is prohibitive because unwinding collateral dependence impairs household wealth, bank solvency, and sovereign fiscal exposure simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent classifications from identical structural data. From the lending seat the arrangement is a functioning credit market it operates and profits from — coordination on favorable terms. From the incumbent-owner seat it is a wealth engine with manageable tail risk. From the first-time-buyer and renter seats the same structure presents as a price wall that converts their necessary demand into someone else's yield. From the central-bank seat it is a stability-management burden whose tools are captive to the very channels they must police. The engine computes these per-seat results from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations anchor the derivation. Lenders and institutional landlords sit nearest the beneficiary pole — both declared beneficiaries with arbitrage-grade exit, which pushes them further toward subsidy. Incumbent homeowners are genuinely dual-positioned (role beneficiary, secondary payer): appreciation accrues to them while debt service and repricing risk flow out, so their effective position is beneficiary-leaning but not full-subsidy; the secondary_role declaration lets the derivation register the mix. First-time buyers and renters sit near the target pole; trapped and constrained exit respectively amplify their effective extraction. Central banks occupy an enforcement seat with near-symmetric mandate exposure but structural commitment to collateral values. Power-atom-keyed overrides were considered and rejected: the institutional atom spans lenders, regulators, and observers whose directionalities differ, so a single override would misstate at least two of them; the per-agent secondary-role mechanism handles the one genuinely mixed seat instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the arrangement as pure snare erases the real coordination function — standardized long-horizon credit is why ordinary households can own homes at all, and abolishing the credit channel would not return prices to shelter value but would freeze ownership access. Reading it as pure rope erases the enforced asymmetry — the same channel that finances ownership systematically transfers appreciation to incumbents and lenders and socializes crash losses. The founding problem (financing mass ownership) is contested rather than dead: the financing need persists, but the arrangement's center of gravity has shifted toward asset-price maintenance. Because the arrangement is actively maintained, profitable, and politically defended, nothing here supports a mandatrophy declaration — this is living extraction riding living coordination, not inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the financialization reading of the price_formation_kernel. Would the sibling readings (naturalist, institutional, georgist) assign a different epsilon and a different beneficiary/victim structure to the same price episodes?',
    'Cross-reading comparison on shared price episodes: fit each reading''s causal account to the same decoupling events and compare the extraction attribution each implies.',
    'If the naturalist reading held, measured extraction would be misattributed (scarcity rents rather than leverage rents); if the georgist reading held, extraction would concentrate in land rent and the remedy set would shift toward land-value taxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a four-reading kernel; sibling readings would restructure beneficiaries and epsilon.').

omega_variable(
    credit_price_causal_direction,
    'Does credit expansion drive prices, or do rising prices draw credit through collateral valuation?',
    'Identification from credit-supply shocks (policy discontinuities, credit-register variation) versus price shocks; lead-lag and structural evidence on the credit-price loop.',
    'If prices drive credit, the financial sector sits nearer mirror than agenda-setter and effective extraction falls; if credit leads, the sector''s agenda-setting position and the measured extraction stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_price_causal_direction, empirical, 'Causal direction of the credit-price feedback loop.').

omega_variable(
    coordination_extraction_separability,
    'Is long-horizon mortgage credit separable from the asset-price feedback amplification that inflates prices?',
    'Macroprudential natural experiments (debt-service and loan-to-value caps in Canada, New Zealand, Korea): if ownership access holds while price growth slows, the functions are separable.',
    'If separable, the feedback component is removable excess and the arrangement is repairable coordination-plus-rent; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the credit-access function and the price-feedback amplifier are structurally separable.').

omega_variable(
    crash_loss_incidence,
    'Who ultimately bears crash losses — taxpayer backstops or creditor haircuts — and is the incidence systematic across cycles?',
    'Compare loss allocation across episodes (US 2008 bailouts versus Swedish 1992 nationalizations with shareholder wipeouts); trace fiscal cost recovery over subsequent decades.',
    'Systematically upward incidence would push the arrangement toward snare drift (privatized gains, socialized losses); episodic incidence keeps it a hybrid of coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crash_loss_incidence, empirical, 'Cycle-phase symmetry of crash-loss allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__financialization_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(pric_tr_t8, observed).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__financialization_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(pric_tr_t16, observed).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__financialization_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(pric_tr_t24, observed).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__financialization_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement_basis(pric_tr_t32, observed).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(pric_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__financialization_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(pric_be_t8, observed).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__financialization_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(pric_be_t16, observed).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__financialization_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(pric_be_t24, observed).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__financialization_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement_basis(pric_be_t32, observed).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(pric_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__financialization_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(pric_su_t8, observed).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__financialization_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(pric_su_t16, observed).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__financialization_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(pric_su_t24, observed).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__financialization_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement_basis(pric_su_t32, observed).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(pric_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how housing prices form' decomposes into four structurally distinct readings of one kernel: naturalist (equilibrium/scarcity), institutional (zoning/tax/platform construction), georgist (land-rent versus improvement separation), and this financialization reading (credit expansion and asset-feedback). Each carries its own epsilon, beneficiary/victim structure, and classification; this file instantiates only the financialization reading. Kinship edges: the naturalist reading supplies the equilibrium baseline against which this reading measures decoupling; the institutional reading shares the constructed-prices premise and differs on which constructor dominates; the georgist reading offers an orthogonal decomposition of the same asset base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
