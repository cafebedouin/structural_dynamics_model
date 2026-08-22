% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutionally Constructed Housing Price Formation (Zoning, Lending, Tax, Platforms)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the institutional reading of the price-formation
 *   kernel: prices in housing markets are not read off an underlying
 *   scarcity/preference equilibrium but are constructed by a specific bundle
 *   of institutional levers — zoning codes that cap legal supply, lending
 *   standards that gate effective demand, tax treatment that subsidizes
 *   ownership over renting, and intermediary platforms that mediate price
 *   discovery and extract transaction fees. Each lever has a genuine
 *   coordination rationale (externality management, systemic risk control,
 *   tenure stability incentives, information matching), which is why this
 *   reads as tangled rope rather than pure snare — but the same structure
 *   that solves those coordination problems also channels appreciation and
 *   fee income toward incumbents, lenders, and intermediaries while renters
 *   and first-time buyers bear the resulting scarcity rents and entry costs.
 *   This is a sibling reading, not a rival description of the same claim: the
 *   naturalist reading treats price formation as an equilibrium outcome of
 *   objective scarcity, the Georgist reading isolates land rent from
 *   improvement value as the analytically relevant split, and the
 *   financialization reading locates the driver in credit expansion and
 *   asset-price feedback loops rather than in zoning/lending/tax/platform
 *   design per se. Each of those is authored as its own constraint with its
 *   own epsilon; this file's epsilon is about the institutional-construction
 *   claim only, assessed by its own lights.
 *
 * KEY AGENTS:
 *   - incumbent_homeowners: primary beneficiary (organized/arbitrage) — appreciation and political capture of zoning process
 *   - mortgage_lenders: beneficiary and co-agenda-setter (institutional/arbitrage) — sets effective demand via underwriting
 *   - real_estate_intermediary_platforms: beneficiary and co-agenda-setter (institutional/arbitrage) — earns fees scaled to price level
 *   - local_zoning_authorities: primary agenda-setter (institutional/analytical) — legally caps supply
 *   - renters: primary target (powerless/constrained) — pays scarcity rent with no equity upside
 *   - first_time_buyers: primary target (powerless/trapped) — priced out by the same appreciation dynamic
 *   - informal_and_manufactured_housing_residents: most severely constrained target (powerless/trapped) — excluded housing forms
 *   - housing_policy_researchers: analytical observer — attributes price levels to institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.61).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.58).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutionally Constructed Housing Price Formation (Zoning, Lending, Tax, Platforms)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4').
narrative_ontology:cs_kernel_codification('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', distributed).
narrative_ontology:cs_authority_grounding('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', distributed).
narrative_ontology:cs_reading_relation('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', foundational, prices_are_administratively_constructed_not_discovered).
narrative_ontology:cs_axiom_status(prices_are_administratively_constructed_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', prices_are_administratively_constructed_not_discovered, empirically_contingent).
narrative_ontology:cs_axiom('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', secondary, regulatory_design_choices_are_the_primary_lever_for_affordability).
narrative_ontology:cs_axiom_status(regulatory_design_choices_are_the_primary_lever_for_affordability, holdable).
narrative_ontology:cs_axiom_grounding('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', regulatory_design_choices_are_the_primary_lever_for_affordability, instrumental).
narrative_ontology:cs_reference_frame('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', mid_century_zoning_and_underwriting_settlement).
narrative_ontology:cs_drift_state('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', contemporary_affordability_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32c10e2d-b88d-4e70-88bc-15f3b9b1e3b4', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, municipal_tax_bases).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, informal_and_manufactured_housing_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold equity that appreciates as zoning restricts new supply and as mortgage-interest deductions and capital-gains exclusions favor ownership over renting. Vote in local land-use hearings and homeowners' associations that set the permitting rules governing whether more housing can be built nearby. Can sell into the appreciated market or refinance against the gain; largely insulated from the price dynamics they help set.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, generational, arbitrage, regional).

% Set underwriting standards, loan-to-value ratios, and qualifying debt-to-income thresholds that determine who can borrow and how much, directly shaping effective demand and clearing prices. Earn origination fees and interest income scaled to loan size; securitize risk onward, largely decoupling their return from long-run affordability outcomes in the neighborhoods they lend into.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Operate the listing, comparable-sales, and transaction infrastructure that most buyers and sellers rely on to discover prices; earn commissions or fees as a percentage of transaction value, so higher prices directly increase intermediary revenue. Have shaped commission norms and data access in ways that raise switching costs for both buyers and sellers.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms, agenda_setter).

% Write and enforce land-use codes — minimum lot sizes, density caps, parking mandates, use restrictions — that determine how much housing supply can legally exist in a given area. Respond primarily to organized incumbent-resident political pressure rather than to renters or prospective entrants, who are rarely represented at hearings.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_zoning_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Pay rents set within a supply envelope constrained by zoning and within a comparable-sales framework shaped by intermediary platforms, without holding any equity claim on the asset whose scarcity they finance through rent. Exit means relocating, often to a lower-opportunity region, since housing is not fungible across locations the way most consumer goods are.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, regional).

% Must qualify under lending standards calibrated to protect lenders, not affordability, and must purchase at prices set by a supply-constrained, intermediary-mediated market they had no role in constructing. Priced out by the same appreciation that benefits incumbent owners; delaying entry only compounds the gap between wages and prices over time.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    powerless, biographical, trapped, regional).

% Occupy housing forms explicitly disfavored or excluded by zoning codes (manufactured homes, accessory units, informal subdivisions), bearing the most severe access constraints from the same land-use rules that protect incumbent property values. Have essentially no voice in the zoning process that determines whether their housing form remains legal.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, informal_and_manufactured_housing_residents, payer,
    powerless, immediate, trapped, local).

% Study how zoning restrictiveness, credit availability, and tax treatment jointly determine observed prices, comparing jurisdictions with different regulatory regimes. Produce the empirical basis for attributing price levels to institutional design choices rather than to underlying scarcity or preference alone.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_policy_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zoning coordinates land-use expectations across neighbors, lending standards coordinate risk-pricing across a national credit market, tax treatment coordinates incentives toward long-term housing investment, and intermediary platforms coordinate price discovery across otherwise fragmented, illiquid local markets. Each piece solves a genuine information or commitment problem in isolation.
% TRANSFER_FUNCTION: The combined system transfers appreciation and rent income toward incumbent owners, interest and fee income toward lenders, and commission income toward intermediaries, funded by renters paying scarcity rents and by first-time buyers paying inflated entry prices relative to what an unconstrained supply response would produce.
% ABSENT_VOICES: Renters, first-time buyers, and residents of disfavored housing forms are structurally underrepresented in the zoning hearings, lending-standard rulemaking, and platform-commission-setting processes that jointly determine prices; incumbent owners and industry incumbents dominate each venue.
% DISAPPEARANCE_RATIONALE: If zoning restrictions, current lending standards, tax preferences for ownership, and intermediary commission structures were all simultaneously removed, housing supply, financing availability, and price discovery mechanisms would all change substantially — prices would reset along a different, more supply-responsive equilibrium, and the current distribution of housing wealth would be disrupted.
% FOUNDING_PROBLEM: Zoning was built to separate incompatible land uses and manage negative externalities (industrial pollution near homes); lending standards were built to prevent systemic bank failure from reckless underwriting; mortgage tax treatment was built to encourage stable homeownership; intermediary platforms were built to solve the information problem of matching buyers and sellers in a fragmented, illiquid market.
% FOUNDING_PROBLEM_CORROBORATION: Urban planning historians and housing economists outside the real estate and lending industries (e.g., research documenting exclusionary zoning's shift from externality management to property-value protection) attest that the original externality and safety-soundness rationales have substantially receded relative to their present function of restricting supply and protecting incumbent equity; industry associations and homeowner groups continue to assert the original rationales are still primary.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at moderate-high (0.61) and rising over the interval, reflecting a genuine multi-decade pattern in many metro housing markets: as zoning has become more restrictive and lending/tax/platform structures have hardened around ownership-favoring norms, the gap between construction cost and clearing price has widened, and an increasing share of that gap is captured as land-value appreciation and fee income rather than reflecting real resource scarcity. Suppression (0.58) reflects the structural underrepresentation of renters and first-time buyers in the zoning and standard-setting venues that determine the constraint's shape, not any single coercive actor. Theater ratio (0.34) is moderate: the externality-management and systemic-risk rationales for zoning and lending standards are not pure theater — they retain real function — but a growing share of enforcement energy (design review, parking minimums, minimum-lot-size litigation) increasingly protects incumbent property values rather than the original stated purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, lenders, and intermediary platforms sit near the beneficiary end: each collects a rent-like or fee-like return whose magnitude scales with the very price level the institutional bundle helps set, and each has meaningful exit or arbitrage options (sell into appreciation, adjust underwriting terms, adjust commission structure) that renters and first-time buyers lack. Renters and first-time buyers sit near the target end: they bear the price level as a cost with no offsetting equity claim, and their exit options are constrained (relocation, often to lower-opportunity regions) or trapped (must qualify under standards they cannot influence). Informal and manufactured housing residents sit at the most extreme target position because the same zoning apparatus that protects incumbent value explicitly disfavors or excludes their housing form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems behind each lever (externality management, systemic risk control, tenure stability, information matching) have not vanished, but their scope has expanded well past the original rationale — density caps and parking minimums now do far more property-value protection than externality management, and platform commission norms persist at levels that predate the informational scarcity they were built to solve. Classifying this as tangled rope rather than snare preserves the fact that real coordination functions remain live (some zoning genuinely manages true externalities; some lending standards genuinely prevent systemic risk); classifying it as tangled rope rather than rope acknowledges that the same structure now channels a substantial, growing transfer toward incumbents at the direct expense of renters and first-time buyers who have no seat at the table where the rules are set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_natural_scarcity_boundary,
    'How much of the observed price level reflects institutionally constructed supply/demand constraints (zoning caps, lending gates, tax subsidies, platform fees) versus genuine underlying scarcity of well-located land that would persist under any institutional regime?',
    'Comparative analysis of metro areas with materially different zoning restrictiveness, lending regimes, and platform structures but similar underlying geographic/amenity scarcity; natural experiments from major zoning reform episodes (e.g., statewide upzoning) isolating the institutional contribution to price change.',
    'If institutional factors explain most of the price gap relative to construction cost, the tangled_rope classification is well-supported and the extraction is substantially remediable by policy change. If underlying scarcity dominates even after institutional reform, this reading overstates the constructed share and understates the naturalist reading''s explanatory force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_natural_scarcity_boundary, empirical, 'How much of price formation is institutionally constructed versus naturally scarce.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the four kernel readings (naturalist, georgist, institutional, financialization) actually disagree — is it about which causal factor dominates, or about which factor is analytically prior?',
    'This is the committer-structure question: each reading is instantiated as a separate constraint file with its own epsilon and stakeholder set, per Rule 1. The disagreement is located at the level of causal attribution and analytical framing, not at the level of observed price data, which all four readings share. The naturalist reading treats the price level itself as legitimate signal; the georgist reading treats the land-rent component specifically as the unearned residual regardless of institutional cause; the institutional reading (this file) treats the zoning/lending/tax/platform bundle as the constructive mechanism; the financialization reading treats credit-driven asset-price feedback as the primary driver, with lending standards as the shared causal thread connecting it to this reading.',
    'A sibling reading (e.g. financialization_reading) would change the beneficiary attribution: this reading places co-primary responsibility on zoning authorities and incumbent homeowners, whereas the financialization reading would place primary responsibility on lenders and capital markets, shifting victim-relief policy prescriptions from upzoning toward credit-cycle regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Committer-structure omega: locates where the four sibling readings of the price-formation kernel actually diverge.').

omega_variable(
    zoning_purpose_drift,
    'Has the primary function of exclusionary zoning genuinely shifted from externality management to incumbent property-value protection, or does the externality-management rationale remain the dominant live function in most jurisdictions?',
    'Historical and legal analysis of zoning board rationale statements and litigation records over time; comparison of stated purposes at adoption versus enforcement patterns and variance-denial rationales today.',
    'If purpose has substantially drifted, the tangled_rope classification''s coordination-function claim weakens over time and the constraint drifts toward piton/snare; if the externality rationale remains dominant, the coordination function is more robust than the extraction narrative suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zoning_purpose_drift, empirical, 'Whether zoning''s coordination rationale has substantially decayed relative to its property-protection function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.15).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the price_formation_kernel (naturalist, georgist, institutional, financialization), each authored as a separate constraint story with its own epsilon per the epsilon-invariance principle. The institutional reading shares a causal thread with the financialization reading via lending standards, but attributes primary construction responsibility to zoning, tax treatment, and intermediary platforms in addition to credit, whereas the financialization reading centers credit expansion and asset-price feedback as the dominant driver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
