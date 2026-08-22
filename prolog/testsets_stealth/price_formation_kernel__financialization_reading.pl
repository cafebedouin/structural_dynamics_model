% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Credit-Driven Housing Price Formation (Financialization Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This story instantiates the financialization reading of the contested
 *   price_formation_kernel: housing prices are formed by credit expansion,
 *   asset-price feedback loops, and demand for housing as a financial asset,
 *   rather than by shelter scarcity and preference alone. The standing
 *   arrangement under contest — the credit-channel price-formation regime
 *   built out since the 1970s through securitization, global savings
 *   intermediation into mortgage assets, tax privileges for leveraged
 *   ownership, and explicit central-bank backstops — is the extractiveness
 *   referent, assessed by this reading's own lights: the reading itself
 *   asserts the arrangement extracts from households, so extractiveness is
 *   authored high. The kernel decomposes into four sibling constraints
 *   (naturalist, institutional, georgist readings), each with its own
 *   extractiveness and stakeholder structure; this file authors only the
 *   financialization reading and links the family through
 *   network.affects_constraints. Claim/metric independence: the claimed type
 *   (tangled_rope) reflects this reading's structural assessment — genuine
 *   credit intermediation fused with asymmetric extraction — while the
 *   metrics are authored from the arrangement's observable operation
 *   (price-to-income decoupling, debt-service burdens, backstop machinery);
 *   the engine computes per-seat classifications from the structural data,
 *   and any divergence between claim and computed type is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - central_banks_and_financial_regulators: agenda setter (institutional / identity_locked) — administers the rate environment, collateral rules, and crisis backstops that keep credit flowing into housing
 *   - mortgage_lending_institutions: primary beneficiary and product administrator (institutional / arbitrage) — originates, securitizes, and collects the debt-service stream; designs the leverage products
 *   - institutional_asset_managers: secondary beneficiary (institutional / mobile) — accumulate housing as an asset class; fee income scales with asset prices
 *   - real_estate_transaction_intermediaries: transaction-volume beneficiary (organized / constrained) — collect per-transaction fees that scale with churn and price levels
 *   - leveraged_existing_homeowners: dual-positioned incumbent asset holders (organized / constrained) — collect appreciation, pay debt service, bear concentrated crash risk
 *   - first_time_homebuyers: primary payer (moderate / trapped) — access shelter only through maximum leverage at decoupled prices
 *   - renter_households: primary payer (powerless / trapped) — bear the price level through rent pass-through without holding the asset
 *   - post_crash_distressed_borrowers: crash-risk bearer (powerless / trapped) — hold negative equity and foreclosure exposure in downturns
 *   - financial_stability_economists: analytical observer (analytical / analytical) — map the credit channel and its failure modes from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.76).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.72).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Credit-Driven Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "economic/political").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '92bcc223-e36c-400c-a0e3-e4c7910ea90e').
narrative_ontology:cs_kernel_codification('92bcc223-e36c-400c-a0e3-e4c7910ea90e', distributed).
narrative_ontology:cs_authority_grounding('92bcc223-e36c-400c-a0e3-e4c7910ea90e', diffuse_epistemic).
narrative_ontology:cs_reading_relation('92bcc223-e36c-400c-a0e3-e4c7910ea90e', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('92bcc223-e36c-400c-a0e3-e4c7910ea90e', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('92bcc223-e36c-400c-a0e3-e4c7910ea90e', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_axiom('92bcc223-e36c-400c-a0e3-e4c7910ea90e', foundational, credit_availability_sets_housing_price_level).
narrative_ontology:cs_axiom_status(credit_availability_sets_housing_price_level, holdable).
narrative_ontology:cs_axiom_grounding('92bcc223-e36c-400c-a0e3-e4c7910ea90e', credit_availability_sets_housing_price_level, empirically_contingent).
narrative_ontology:cs_axiom('92bcc223-e36c-400c-a0e3-e4c7910ea90e', foundational, housing_priced_as_financial_asset_not_shelter).
narrative_ontology:cs_axiom_status(housing_priced_as_financial_asset_not_shelter, holdable).
narrative_ontology:cs_axiom_grounding('92bcc223-e36c-400c-a0e3-e4c7910ea90e', housing_priced_as_financial_asset_not_shelter, empirically_contingent).
narrative_ontology:cs_axiom('92bcc223-e36c-400c-a0e3-e4c7910ea90e', secondary, collateral_feedback_amplifies_price_movements).
narrative_ontology:cs_axiom_status(collateral_feedback_amplifies_price_movements, holdable).
narrative_ontology:cs_axiom_grounding('92bcc223-e36c-400c-a0e3-e4c7910ea90e', collateral_feedback_amplifies_price_movements, empirically_contingent).
narrative_ontology:cs_reference_frame('92bcc223-e36c-400c-a0e3-e4c7910ea90e', endogenous_credit_asset_price_formation).
narrative_ontology:cs_drift_state('92bcc223-e36c-400c-a0e3-e4c7910ea90e', post_2008_macroprudential_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('92bcc223-e36c-400c-a0e3-e4c7910ea90e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_lending_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_asset_managers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, real_estate_transaction_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, leveraged_existing_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, post_crash_distressed_borrowers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leveraged_existing_homeowners).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, wealth_effect_transmission_doctrine).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, collateral_channel_lending_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, minsky_financial_instability_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the interest-rate environment, the collateral and capital rules for mortgage lending, and the crisis backstop — lender-of-last-resort facilities, asset-purchase programs, bailout precedent — that keeps credit flowing into housing. Their policy doctrine ties financial stability to asset-price stability, and each successive crisis response has been built around re-inflating housing collateral. Stepping off the asset-price channel is not a menu option they treat as available: their legitimacy has been constructed on it across cycles.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks_and_financial_regulators, agenda_setter,
    institutional, generational, identity_locked, global).

% Originate mortgages, package them into securities, and collect interest and fees on the flow. They design the leverage products — high loan-to-value loans, interest-only terms, investor lending — and their income scales with credit volume and asset prices. They can distribute risk off balance sheet through securitization, and in downturns they have been recapitalized from public funds while household losses stood.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_lending_institutions, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_lending_institutions, agenda_setter).

% Purchase and hold housing — single-family rental portfolios, multifamily buildings, mortgage-backed securities — as an asset class alongside bonds and equities. Fee income scales with assets under management, which scales with asset prices, so appreciation is the revenue model. Capital rotates freely across asset classes and borders; housing is one allocation among many.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_asset_managers, beneficiary,
    institutional, generational, mobile, global).

% Brokers, agents, and title and escrow services collect a fee on every transaction. Income scales with transaction volume and price levels rather than with shelter outcomes. Skills and licenses are industry-specific, so alternative income paths run through the same market.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, real_estate_transaction_intermediaries, beneficiary,
    organized, biographical, constrained, national).

% Hold homes bought with mortgage debt. In rising markets they collect capital gains on a leveraged position and can borrow against the collateral; across the loan's life they pay interest to lenders, and on any move they pay transaction costs, while carrying the risk that the asset falls below the loan balance. Selling means re-entering the same market or becoming a renter. They vote and organize around property values, zoning, and tax treatment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_existing_homeowners, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, leveraged_existing_homeowners, payer).

% Need shelter and can reach ownership only through the mortgage channel. Prices set by the credit cycle determine the size of the loan they must carry; price-to-income ratios push them to the top of their borrowing capacity, often at cycle peaks. Their alternatives are renting longer, moving regions, or delaying household formation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    moderate, biographical, trapped, national).

% Occupy housing without owning it. Rents track landlords' financing costs and asset yield expectations, so they pay the price level set by the credit cycle without collecting any appreciation. Moving is costly and every location prices through the same channel; organizing is difficult across high tenant turnover.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renter_households, payer,
    powerless, biographical, trapped, national).

% Bought at or near cycle peaks with high loan-to-value ratios. In downturns they hold negative equity, face foreclosure, and absorb the losses that deleveraging requires. Public rescue programs in past crises directed most support to financial institutions and current asset holders rather than to this cohort.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, post_crash_distressed_borrowers, payer,
    powerless, immediate, trapped, national).

% Researchers inside and outside central banks who map the credit channel, price-to-rent and price-to-income divergence, and the distribution of crash losses. They publish, advise, and testify; they hold no position in the flows they analyze.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_stability_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, mortgage_lending_institutions).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Intermediates household savings into mortgage credit at scale: standardized underwriting, liquidity for long-duration loans, intertemporal transfer that lets households occupy shelter before accumulating its full price, and a deep collateral market that supports the broader financial system.
% TRANSFER_FUNCTION: Moves household income as mortgage debt service and transaction fees from mortgaged households and renters (via landlord cost-of-capital pass-through) to lenders, securitizers, and intermediaries; moves capital gains to incumbent asset holders; in downturns, moves losses from asset holders and lenders to taxpayers and to the most-leveraged borrowers through public backstops.
% ABSENT_VOICES: Households not yet in the market (future first-time buyers), the post-crash foreclosed cohort, and non-market housing advocates — social housing providers, limited-equity co-ops, community land trusts — are outside the conversation, which is conducted among central banks, lenders, incumbent asset holders, and treasuries. Their objection, that shelter access should not run through leverage, holds no seat at credit-policy tables.
% DISAPPEARANCE_RATIONALE: If the credit-driven price-formation arrangement vanished overnight, prices would fall toward income-anchored shelter values, household balance sheets would restructure, bank collateral bases and fund asset allocations would shrink, the transaction-fee economy would contract, and the political coalition organized around homeowner appreciation would dissolve — the housing political economy reorganizes around the credit channel's absence.
% FOUNDING_PROBLEM: Mid-20th-century housing finance could not fund mass homeownership: savings institutions lacked liquidity for 25-to-30-year loans, credit was rationed locally, and no standardized secondary market existed. The arrangement was built to solve mortgage liquidity and scale.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and central bank archives — outside the beneficiary set — corroborate both the founding problem's existence and its substantial solution: mortgage liquidity is no longer scarce. Academic housing economics (price-to-income and price-to-rent series) and Bank for International Settlements financial-stability research corroborate that current price formation has decoupled from that founding function. Lenders and asset managers attest the problem is still live; no beneficiary-internal source attests the decoupling.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.76 at interval end) because the arrangement's price output has decoupled from shelter value while household access runs through maximum leverage; debt-service ratios and price-to-income multiples are the observable face of the transfer. Suppression (0.72) is structural rather than coercive: there is no standing non-credit path to ownership at scale, and persistence depends on an actively maintained backstop apparatus — which is why the suppression_requirement series is authored at all: the enforcement picture is this story's central dynamic, tracing the machinery from rationing-era restraint (0.30 in 1970) through deregulated permissiveness to the explicit post-2008 backstop (0.70 by 2012). Theater (0.33) captures the affordability-product layer — high-LTV 'access' products, shared-equity and rent-to-own schemes — marketed as expanding access while extending leverage; the 2007 peak (0.38) marks the maximum of the 'financial innovation democratizes homeownership' rhetoric, and the post-crash dip and re-rise track the reform-and-regression cycle. Accessibility collapse is moderate (0.58): alternatives exist (cash purchase, limited-equity co-ops, community land trusts, social housing) but are marginalized to a small share of the stock, so alternatives collapse only partly. Resistance (0.52) is real but fragmented: affordability movements, tenant organizing, and macroprudential advocacy meet an incumbent coalition of lenders, asset managers, and homeowner voters. All three series share one time grid; base_extractiveness is non-monotonic (crash deleveraging dips it 2007 to 2012) before re-ratcheting — the dip is the cycle, not the trend.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement is financial-stability infrastructure: the central bank experiences the credit channel as its transmission mechanism and its crisis tool, and its institutional identity has fused with asset-price maintenance — if that identity frame broke, the backstop could be withdrawn and the arrangement's enforcement would collapse. From the lender seat it is a market it serves and designs products for. From the payer seats it is the price of existing: buyers experience it as the loan they must carry, renters as the rent they cannot escape. Same-level divergence is sharpest within the household class: leveraged incumbents and first-time buyers face the same prices with opposite balance-sheet positions — incumbents collect the appreciation the buyers must finance, and an incumbent's exit (selling) re-enters the very market the buyer is trying to enter, so neither seat has a clean exit the other lacks. Renters sit below both: they bear the price level with no asset at all. Suppression here is predominantly structural (no non-credit path to ownership, backstop dependence) with a secondary internalized component (the homeownership-as-wealth norm that makes exit thinkable only as ownership); the structural share dominates the authored value.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (lenders, asset managers, intermediaries, leveraged incumbents) derive directionality near the beneficiary end; declared victims (first-time buyers, renters, distressed borrowers) derive near the target end. The dual position of leveraged incumbents — appreciation collected, debt service paid, crash risk concentrated — is carried by their secondary payer role and by the homeowner_net_position_ambiguity omega rather than by a directionality override: the override mechanism is keyed by power atom, and the only atom the incumbents share (organized) is also held by transaction intermediaries, whose position is unambiguously beneficiary-side, so an override would misapply. The agenda-setter seat (central banks and regulators) is neither declared beneficiary nor victim; it is administrative, and its identity lock is documented in the perspectival commentary rather than in a directionality value. Receipt is distinct from benefit here: the debt-service stream — the core transfer — lands on mortgage_lending_institutions, while appreciation gains accrue diffusely to incumbent asset holders and fee flows to intermediaries and asset managers.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim is what keeps both mislabels out. A pure-extraction reading would erase the genuine coordination function — intermediating savings into 25-to-30-year mortgage credit solved a real problem and still does; a pure-coordination reading would erase the asymmetric extraction — the same channel that provides liquidity sets a price level decoupled from shelter value and socializes crash losses upward. The founding problem (mortgage liquidity at scale) is contested rather than dead: intermediation remains live, but the arrangement's observable persistence increasingly rides on asset-price maintenance rather than on funding shelter, which is why founding_problem_status is authored contested against a world_rearranges disappearance verdict — that mismatch is the flag to watch. Mandatrophy is not declared resolved: no structural fact here establishes that the founding function has atrophied; the record shows extraction accumulating on top of a live function, which is the tangled-rope signature, not the piton's cost-asymmetry — the administrator (central banks) could change the arrangement, but the collateral-deflation cost of fixing exceeds what it bears, hence fixing_cost prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_formation_kernel_reading_position,
    'This constraint is one reading (financialization_reading) of the contested price_formation_kernel; which reading a party adopts determines the causal driver of the price level and therefore the entire victim structure — what would each sibling reading change structurally?',
    'Adoption of a sibling reading is a framing commitment, not a data outcome: it resolves when a party, discipline, or policy framework commits to a different reading — a Georgist land-tax regime, an efficient-markets naturalist consensus, an institutionalist regulatory frame — at which point the sibling''s constraint story supersedes this one''s classification.',
    'Under the naturalist reading the arrangement dissolves into a natural process with no victim set; under the georgist reading the victim set splits between landholders and improvers and the extraction target becomes capitalized land rent; under the institutional reading the driver relocates to zoning, lending rules, tax treatment, and platforms, and the agenda-setter set expands to legislatures and platforms. This file''s extractiveness and classification hold only within the financialization reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_formation_kernel_reading_position, conceptual, 'Kernel-membership omega: this story instantiates one of four competing readings of the price_formation_kernel; the disagreement is located in the causal driver of the price level.').

omega_variable(
    decoupling_persistence_question,
    'Is the decoupling of housing prices from shelter value (price-to-income and price-to-rent divergence) a persistent structural feature of the arrangement, or a cyclical deviation that mean-reverts across full credit cycles?',
    'Long-run cross-country price-to-rent and price-to-income series observed through complete credit cycles; persistence with ratcheting floors establishes structure, full reversion would establish cycle.',
    'Persistent decoupling supports the high extractiveness measure and the tangled-rope reading; full mean reversion would recast the arrangement as coordination with episodic overshoot, lowering extractiveness and softening the victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_persistence_question, empirical, 'Whether the price level''s decoupling from shelter value is structural or cyclical.').

omega_variable(
    backstop_commitment_status,
    'Is the public backstop of housing collateral — lender-of-last-resort practice, bailout precedent, asset-purchase programs — a committed structural feature of the arrangement, or a contingent emergency response that could in principle be withheld?',
    'Legislative and central-bank doctrine history: if each crisis response has been normalized into standing facilities and forward guidance, the backstop is committed; if each was ad hoc and legally contingent, it is contingent.',
    'A committed backstop makes suppression structural and supports the enforcement-driven classification; a contingent backstop would make suppression episodic — spiking only in crises — and lower the standing suppression measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backstop_commitment_status, empirical, 'Whether crisis backstops are standing structure or contingent response.').

omega_variable(
    homeowner_net_position_ambiguity,
    'Are leveraged existing homeowners net beneficiaries or net victims once debt service, transaction costs, and concentrated crash risk are priced across a full credit cycle?',
    'Distributional wealth accounting across complete credit cycles, including foreclosure losses and bailout incidence, split by entry cohort and leverage at entry.',
    'If net victims, the arrangement''s victim set expands to include its most politically organized defenders and the structure reads more purely extractive; if net beneficiaries, the defending coalition is materially stable and extraction concentrates on entrants and renters. The current authoring assumes the split answer — beneficiary in expansion, crash-risk bearer in downturn — which the beneficiary/victim derivation cannot express; no directionality override is used because the override mechanism is keyed by power atom, and the organized atom also holds unambiguously beneficiary-side intermediaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeowner_net_position_ambiguity, empirical, 'Net position of leveraged homeowners across the full credit cycle.').

omega_variable(
    shelter_value_referent,
    'What benchmark defines ''shelter value'' against which the price level''s decoupling is measured — rent equivalence, replacement construction cost, or income multiple — and does the choice change the magnitude of the measured decoupling?',
    'Comparative estimation of the decoupling under each benchmark on the same price series; convergence across benchmarks fixes the magnitude, divergence bounds it.',
    'The qualitative structure — decoupling present, credit-driven — is stable across benchmarks, but the extractiveness magnitude moves with the referent; this omega bounds the extractiveness estimate rather than the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shelter_value_referent, empirical, 'Benchmark ambiguity for measuring price decoupling from shelter value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__financialization_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__financialization_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__financialization_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(pric_tr_t2007, price_formation_kernel__financialization_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(pric_tr_t2012, price_formation_kernel__financialization_reading, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(pric_tr_t2019, price_formation_kernel__financialization_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__financialization_reading, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__financialization_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(pric_be_t2007, price_formation_kernel__financialization_reading, base_extractiveness, 2007, 0.72).
narrative_ontology:measurement(pric_be_t2012, price_formation_kernel__financialization_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(pric_be_t2019, price_formation_kernel__financialization_reading, base_extractiveness, 2019, 0.72).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__financialization_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__financialization_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.36).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.43).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(pric_su_t2007, price_formation_kernel__financialization_reading, suppression_requirement, 2007, 0.62).
narrative_ontology:measurement(pric_su_t2012, price_formation_kernel__financialization_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(pric_su_t2019, price_formation_kernel__financialization_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__financialization_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'housing price formation' decomposes into four structurally distinct claims — one per reading of the price_formation_kernel — each with its own extractiveness, beneficiary/victim structure, and classification. This file authors the financialization_reading only. A naturalist_reading story would show negligible extraction and a mountain-like profile; a georgist_reading story would split the victim set between landholders and improvers; an institutional_reading story would relocate the driver to zoning, lending rules, tax treatment, and platforms and expand the agenda-setter set. The family is linked through affects_constraints so adoption and contamination analysis can propagate across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
