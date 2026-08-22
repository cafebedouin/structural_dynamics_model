% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutional Construction of Housing Prices (Zoning, Credit Standards, Tax Treatment, Listing Platforms)
 *   domain: political economy / housing markets / institutional analysis
 *
 * SUMMARY:
 *   Housing prices in supply-constrained metropolitan areas are formed inside
 *   an institutional apparatus: municipal zoning gates physical supply,
 *   federal and bank underwriting standards gate who can bid and with what
 *   leverage, tax treatment shapes holding and transaction incentives, and
 *   listing platforms gate market information. This story instantiates the
 *   institutional reading of the price-formation kernel and authors epsilon
 *   for THAT reading only; the sibling readings are separate stories linked
 *   through the network block. KEY AGENTS (by structural relationship): -
 *   municipal_zoning_authorities: agenda setter (institutional/constrained) —
 *   administers the supply gate - federal_credit_regulators: agenda setter
 *   (institutional/constrained) — administers the credit gate -
 *   incumbent_homeowners: primary beneficiary (organized/arbitrage) —
 *   collects appreciation, dominates the approving electorate -
 *   mortgage_lenders and real_estate_brokerages and
 *   listing_platform_operators: secondary beneficiaries
 *   (institutional-organized/mobile-arbitrage) — collect interest,
 *   commissions, and platform rents scaled to the price level -
 *   renter_households: primary target (powerless/constrained) — pays rents
 *   under gated supply - first_time_buyers: target with delayed beneficiary
 *   flip (moderate/constrained) - prospective_in_migrants: excluded voice
 *   (powerless/mobile) — bound by rules they never voted on -
 *   academic_housing_economists: analytical observer — sees the full
 *   structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.66).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.61).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Prices (Zoning, Credit Standards, Tax Treatment, Listing Platforms)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political economy / housing markets / institutional analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '792226a5-8ae8-4b66-afe0-e10cdd17a35f').
narrative_ontology:cs_kernel_codification('792226a5-8ae8-4b66-afe0-e10cdd17a35f', distributed).
narrative_ontology:cs_authority_grounding('792226a5-8ae8-4b66-afe0-e10cdd17a35f', distributed).
narrative_ontology:cs_reading_relation('792226a5-8ae8-4b66-afe0-e10cdd17a35f', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('792226a5-8ae8-4b66-afe0-e10cdd17a35f', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('792226a5-8ae8-4b66-afe0-e10cdd17a35f', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('792226a5-8ae8-4b66-afe0-e10cdd17a35f', foundational, prices_are_policy_constructed).
narrative_ontology:cs_axiom_status(prices_are_policy_constructed, holdable).
narrative_ontology:cs_axiom_grounding('792226a5-8ae8-4b66-afe0-e10cdd17a35f', prices_are_policy_constructed, empirically_contingent).
narrative_ontology:cs_axiom('792226a5-8ae8-4b66-afe0-e10cdd17a35f', secondary, institutional_wedges_dominate_fundamentals).
narrative_ontology:cs_axiom_status(institutional_wedges_dominate_fundamentals, holdable).
narrative_ontology:cs_axiom_grounding('792226a5-8ae8-4b66-afe0-e10cdd17a35f', institutional_wedges_dominate_fundamentals, empirically_contingent).
narrative_ontology:cs_reference_frame('792226a5-8ae8-4b66-afe0-e10cdd17a35f', externality_control_and_credit_stabilization_regime).
narrative_ontology:cs_drift_state('792226a5-8ae8-4b66-afe0-e10cdd17a35f', contemporary_affordability_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('792226a5-8ae8-4b66-afe0-e10cdd17a35f', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_brokerages).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, listing_platform_operators).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, prospective_in_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, first_time_buyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% City councils and planning boards adopt density limits, minimum lot sizes, parking mandates, height caps, and discretionary review procedures, and decide every variance. Nearly all new housing supply passes through their approval gate. They are bound by state enabling law and answer to an electorate dominated by resident homeowners; their fiscal position is tied to the property-tax base the current price level sustains.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_zoning_authorities, agenda_setter,
    institutional, generational, constrained, local).

% Set underwriting standards, conforming loan limits, capital and documentation requirements, and operate or oversee guarantee channels that price mortgage credit nationwide. Their rules determine who can borrow, how much, and against what collateral. They adjust through slow rulemaking and are accountable to systemic-stability mandates rather than to any single class of market participant.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, federal_credit_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Hold homes whose resale value depends on restricted supply in their metro. They organize into neighborhood associations, dominate attendance and voting in local land-use decisions, and pay property taxes that are small relative to appreciation. Their exit is favorable: they can sell into the appreciated market, capture the gain, and relocate to a cheaper region.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, biographical, arbitrage, regional).

% Originate loans sized against collateral whose value the surrounding rules sustain; interest and origination income scale with balance sizes. Securitization transfers default risk onward. They can redirect lending toward other asset classes if housing returns deteriorate, and they help write the underwriting standards they operate under.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, national).

% Earn commissions calculated as a percentage of transaction prices, so revenue rises with the price level the broader apparatus sustains. They maintain MLS access norms that condition participation, lobby against commission compression and unlicensed competition, and can shift staff toward adjacent services if transaction volume falls.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_brokerages, beneficiary,
    organized, biographical, mobile, national).

% Control the search interfaces, listing feeds, and lead-generation pipelines through which buyers encounter inventory. They monetize placement and agent referrals, decide which listings and which agents get visibility, and negotiate data-access terms with brokerages. Their product choices shape what market information reaches which participants.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, listing_platform_operators, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, listing_platform_operators, agenda_setter).

% Pay rents set in a market where supply growth is gated by approval processes they do not control. They are excluded from the appreciation the price level generates, and their savings chase down-payment thresholds that rise faster than they can save. Moving away means leaving jobs, commuting ranges, and family networks; relocating to a cheaper metro usually means accepting lower wages.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renter_households, payer,
    powerless, biographical, constrained, regional).

% Must enter the regulated market to reach owner-occupancy and its tax and credit advantages, taking large leveraged positions at prevailing prices. At entry they bear the full constructed premium; once purchased, the same rules that raised their entry cost begin working to preserve their asset value. Their position flips across the threshold they are trying to cross.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, first_time_buyers, beneficiary).

% Would move to high-productivity metros if housing existed there at reachable cost. They appear in none of the hearings, comment periods, or ballots that set the rules binding them, because they do not yet reside in the jurisdiction. Their available exit is to go somewhere else, which is precisely the relocation the rules price them out of avoiding.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, prospective_in_migrants, excluded,
    powerless, biographical, mobile, national).

% Estimate supply elasticities, decompose price wedges between construction cost and sale price, and evaluate reform pilots. They testify in legislative proceedings and publish outside any market position. They hold no housing stake that the rules under examination enrich or burden.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, academic_housing_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The apparatus solves three real problems simultaneously: land-use externality management (separating incompatible uses, matching development to infrastructure capacity, fire and sanitation standards), mortgage credit risk pooling (underwriting and guarantee standards that rebuilt a collapsed mortgage market and have prevented Depression-scale foreclosure cascades since), and information aggregation (shared listing databases that connect dispersed buyers to dispersed inventory).
% TRANSFER_FUNCTION: Moves housing wealth from renter households and first-time buyers to incumbent owners through rents and purchase prices set well above replacement cost in supply-restricted metros; moves interest and fee income to lenders on balances sized against sustained collateral values; moves price-scaled commissions and platform revenues to brokerages and listing operators.
% ABSENT_VOICES: Prospective in-migrants priced out of opportunity metros are absent from every proceeding that binds them; future market entrants inherit scarcity rules adopted decades before they reached adulthood; renters attend hearings but hold no vote over the councils that decide, and their organized voice is thin relative to homeowner associations.
% DISAPPEARANCE_RATIONALE: If the rules vanished overnight, administered scarcity would collapse: prices in restrictive metros would fall toward replacement cost plus location rent, trillions in incumbent home equity would compress, mortgage underwriting would briefly vanish and default rates would spike until private standards re-formed, and listing information would fragment before substitute aggregators emerged. Household balance sheets, metro population distributions, and municipal finances would all reorganize around the new price level.
% FOUNDING_PROBLEM: Early-to-mid twentieth century land-use and credit chaos: industrial nuisances adjacent to dwellings, fire and sanitation risk in dense districts, racially discriminatory redlining that had destroyed credit access, and the foreclosure cascades of the Depression that had wiped out the mortgage market.
% FOUNDING_PROBLEM_CORROBORATION: Public-health and planning historians attest that the nuisance-separation and basic-safety functions were largely achieved and partly superseded by state and federal environmental law; banking historians attest that the credit-stabilization function remained demonstrably live through the 2008 crisis; land-use economists outside the beneficiary set, measuring regulatory restrictiveness against price wedges across metros, attest that the apparatus now operates substantially beyond its founding justification. No source outside the benefiting parties attests that the whole apparatus, as currently configured, still serves its founding problem.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.66: in restrictive metros, sale prices and rents run far above replacement cost plus modest location rent, and the wedge is sustained by enforceable rules rather than by physical scarcity; the value scales with regulatory restrictiveness, hence moderate-high rather than extreme. Suppression is 0.61 and is authored as a raw structural property, unscaled by power or scope: the apparatus suppresses alternatives through permit denial, litigation exposure under review statutes, credit gatekeeping, and MLS/platform access norms — coercion exercised by denying approvals rather than by punishing possession. Theater ratio is 0.34: affordability programs, inclusionary zoning set-asides, and first-buyer assistance produce visible activity while the blanket supply restriction that sets the price level persists untouched; the safety-code and credit-stabilization cores remain functional. Accessibility collapse is 0.45: once the construction of prices is understood, alternatives (upzoning, land-value taxation, public development, open listing protocols) remain conceptually available and legible — they are politically blocked, not logically impossible, so alternatives do not collapse the way they do under a natural law. Resistance is 0.55: YIMBY organizing, tenant unions, state preemption fights, and antitrust actions against commission structures meet the apparatus continuously and occasionally win. The temporal series run on ONE shared grid (1970, 1980, 1990, 2000, 2010, 2017, 2025) with every tracked metric authored at every point; the suppression_requirement series is included deliberately because the story traces enforcement-capacity build-out (discretionary review expansion, environmental-review litigation, post-2008 compliance hardening, platform data gatekeeping), not merely shifting extraction. Endpoint values equal the base_properties scalars by construction of the grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the incumbent-homeowner seat the apparatus presents as a legitimate coordination it participates in and defends — safety, orderly development, neighborhood stability — with the appreciation experienced as earned return. From the renter and first-time-buyer seats the same structure presents as a gated market whose gatekeepers collect the difference between administered and replacement-cost prices. From the two agenda-setter seats it presents as administration of mandated processes. The engine derives these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for incumbent_homeowners, mortgage_lenders, real_estate_brokerages, and listing_platform_operators — each collects a flow scaled to the price level, and each holds favorable exit (homeowners arbitrage their equity out; lenders securitize and redirect; brokerages and platforms pivot products). Victim declarations drive high directionality for renter_households, first_time_buyers, and prospective_in_migrants — they pay the constructed premium, and their exits are constrained or consist of leaving entirely. The two agenda-setter seats sit near symmetric: they collect no principal extraction flow (municipal fiscal gains are indirect and second-order), and their position is administrative. first_time_buyers carry a genuine dual position — full target at entry, beneficiary after purchase — which the derivation approximates from their current victim declaration; the residual error is noted rather than overridden, since a power-atom-keyed override cannot distinguish them from other moderate-power agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits rather than resolves: the nuisance-separation limb is largely dead (achieved, then superseded by higher-level environmental law), while the credit-stabilization limb is demonstrably live (post-2008 standards prevented cascade failures). Status is therefore contested, not dead — so the dead-mandate-plus-world-rearranges mismatch flag does not fire cleanly, and that is the honest reading. The tangled_rope classification is what prevents mislabeling in both directions: a pure-coordination reading would erase the measurable wedge between administered prices and replacement cost and the identifiable households who pay it; a pure-extraction reading would erase the safety codes and underwriting standards whose absence historically produced fire deaths and foreclosure depressions. The apparatus persists not because anyone theatricality maintains a corpse but because fixing it is prohibitively costly for the agenda setters who could fix it: municipal councils depend on homeowner electorates, and federal regulators answer to stability mandates that the current configuration satisfies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story is one reading of price_formation_kernel. Would the naturalist, georgist, or financialization readings, applied to the same price observables, produce different beneficiary/victim structures and materially different epsilon?',
    'Generate the three sibling stories over the same observables and compare computed per-seat classifications; locate the disagreement in whichever structural element (existence of identifiable constructors, land-rent share, credit-loop share) the readings diverge on.',
    'If the naturalist reading computes negligible extraction, the entire classification turns on whether identifiable agents construct scarcity — the deepest fork in the family. If the georgist reading computes higher epsilon concentrated on land, the institutional reading understates extraction by spreading it across instruments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame uncertainty: which reading of the price-formation kernel the evidence best supports.').

omega_variable(
    jurisdictional_restrictiveness_variance,
    'How much of the measured extractiveness reflects the restrictive-jurisdiction tail (coastal superstar metros) versus permissive regions, given that the same national instruments operate at very different intensities locally?',
    'Stratified measurement across regulatory-regime strata (permitting timelines, density caps, effective elasticity estimates) rather than a single national scalar.',
    'Epsilon plausibly spans roughly 0.35 in permissive strata to above 0.8 in the most restrictive; per-stratum classification could differ, with the restrictive tail computing as pure extraction riding on a thin coordination core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_restrictiveness_variance, empirical, 'Jurisdictional heterogeneity of the constraint''s intensity.').

omega_variable(
    protective_core_separability,
    'Are the protective functions (fire/sanitation codes, counter-cyclical underwriting) structurally separable from the exclusionary functions (density caps, lot-size minimums, commission floors, platform gatekeeping), or does extraction ride inseparably on the coordination?',
    'Natural experiments from upzoning and commission-rule reforms that held safety codes constant: if safety and credit-stability outcomes hold while prices fall toward replacement cost, the functions are separable.',
    'If separable, most of the measured extraction is removable without sacrificing the coordination core; if inseparable, part of epsilon is the irreducible price of the coordination itself and the tangled-rope reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_core_separability, conceptual, 'Whether the coordination core and the extraction shell can be unbundled.').

omega_variable(
    platform_layer_attribution,
    'How much of the post-2010 rise in extractiveness is attributable to intermediary-platform gatekeeping (data access, lead monetization, commission maintenance) versus the older zoning and credit instruments?',
    'Difference-in-differences around platform market-power events and commission-litigation outcomes, holding zoning regimes fixed.',
    'Shifts attribution among the four beneficiary seats and changes which instrument reforms would bite first; does not change the victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_layer_attribution, empirical, 'Relative contribution of the newest instrument layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfk_institutional_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(pfk_institutional_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(pfk_institutional_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(pfk_institutional_tr_t2000, price_formation_kernel__institutional_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(pfk_institutional_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(pfk_institutional_tr_t2017, price_formation_kernel__institutional_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement(pfk_institutional_tr_t2025, price_formation_kernel__institutional_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(pfk_institutional_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.44).
narrative_ontology:measurement(pfk_institutional_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.49).
narrative_ontology:measurement(pfk_institutional_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement(pfk_institutional_be_t2000, price_formation_kernel__institutional_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(pfk_institutional_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(pfk_institutional_be_t2017, price_formation_kernel__institutional_reading, base_extractiveness, 2017, 0.64).
narrative_ontology:measurement(pfk_institutional_be_t2025, price_formation_kernel__institutional_reading, base_extractiveness, 2025, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(pfk_institutional_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(pfk_institutional_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(pfk_institutional_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(pfk_institutional_su_t2000, price_formation_kernel__institutional_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(pfk_institutional_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(pfk_institutional_su_t2017, price_formation_kernel__institutional_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(pfk_institutional_su_t2025, price_formation_kernel__institutional_reading, suppression_requirement, 2025, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'housing price formation' decomposes into four structurally distinct claims (epsilon-invariance principle): the naturalist reading (negligible extraction, mountain-like if true), the georgist reading (extraction concentrated on unearned land rent), the financialization reading (extraction via credit expansion and feedback loops), and this institutional reading (extraction via enforceable rules, moderate-high, scaling with restrictiveness). Each is authored as its own story with its own epsilon, beneficiaries, and victims; they are linked here because the upstream readings are cited as evidence within the downstream disputes. This file authors ONLY the institutional reading; its epsilon refers to the standing institutional apparatus as this reading assesses it, not to any sibling's endorsed alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
