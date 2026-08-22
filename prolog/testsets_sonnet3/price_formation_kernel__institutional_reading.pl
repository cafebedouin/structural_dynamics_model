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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Housing Price Formation as Institutional Construction (Zoning, Lending, Tax, Platforms)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This is the institutional reading of the price-formation kernel: housing
 *   prices are not a discovered fact about scarcity and preference (the
 *   naturalist reading) nor purely a function of credit-cycle asset dynamics
 *   (the financialization reading) nor a separable
 *   land-rent-versus-improvement-value phenomenon (the Georgist reading), but
 *   the constructed output of four interacting institutional apparatuses —
 *   municipal zoning codes, mortgage lending standards, property and income
 *   tax treatment of ownership, and intermediary listing/valuation platforms.
 *   Each apparatus has a genuine coordination function (land-use planning,
 *   credit-risk management, local fiscal funding, search-cost reduction), but
 *   their combined operation systematically transfers wealth from renters and
 *   first-time buyers to incumbent owners, lenders, and platforms, and
 *   requires continuous active maintenance (zoning enforcement, underwriting
 *   policy, tax assessment cycles, platform algorithm governance) to persist.
 *   This reading treats price as a construction with identifiable authors,
 *   not a discovery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.62).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.58).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation as Institutional Construction (Zoning, Lending, Tax, Platforms)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, 'f0646e5b-3152-49c4-a789-509616192eaf').
narrative_ontology:cs_kernel_codification('f0646e5b-3152-49c4-a789-509616192eaf', distributed).
narrative_ontology:cs_authority_grounding('f0646e5b-3152-49c4-a789-509616192eaf', distributed).
narrative_ontology:cs_reading_relation('f0646e5b-3152-49c4-a789-509616192eaf', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f0646e5b-3152-49c4-a789-509616192eaf', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('f0646e5b-3152-49c4-a789-509616192eaf', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('f0646e5b-3152-49c4-a789-509616192eaf', foundational, prices_are_institutionally_constructed_not_discovered).
narrative_ontology:cs_axiom_status(prices_are_institutionally_constructed_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('f0646e5b-3152-49c4-a789-509616192eaf', prices_are_institutionally_constructed_not_discovered, empirically_contingent).
narrative_ontology:cs_axiom('f0646e5b-3152-49c4-a789-509616192eaf', foundational, regulatory_apparatus_generates_identifiable_beneficiaries_and_victims).
narrative_ontology:cs_axiom_status(regulatory_apparatus_generates_identifiable_beneficiaries_and_victims, holdable).
narrative_ontology:cs_axiom_grounding('f0646e5b-3152-49c4-a789-509616192eaf', regulatory_apparatus_generates_identifiable_beneficiaries_and_victims, empirically_contingent).
narrative_ontology:cs_reference_frame('f0646e5b-3152-49c4-a789-509616192eaf', midcentury_exclusionary_zoning_settlement).
narrative_ontology:cs_drift_state('f0646e5b-3152-49c4-a789-509616192eaf', contemporary_housing_affordability_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f0646e5b-3152-49c4-a789-509616192eaf', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, municipal_tax_base_administrators).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, informal_and_manufactured_housing_residents).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, local_land_use_autonomy_doctrine).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, homeownership_as_wealth_building_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold appreciating equity whose growth is substantially produced by zoning restrictions (minimum lot sizes, single-family-only districts, permitting friction) that they vote to preserve at the municipal level. Mortgage interest deductions and capital gains exclusions on primary residences further subsidize their position. They can sell into the same restricted market they helped construct, realizing gains without personally 'setting' a price.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, generational, mobile, regional).

% Set underwriting standards, loan-to-value ratios, and credit-scoring criteria that determine who can access mortgage credit at what price, which directly shapes effective demand and therefore observed transaction prices. Securitize and sell loan risk, largely insulating themselves from local price declines. Government-sponsored enterprise backing further reduces their downside.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Operate listing aggregation, valuation algorithms (automated valuation models), and transaction brokerage that both reflect and actively influence asking and closing prices. Collect commissions and data-licensing revenue regardless of whether prices rise or fall. Their algorithmic estimates become self-referential inputs that buyers, sellers, and even lenders anchor to.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms, agenda_setter).

% Depend on rising assessed property values to fund local budgets without raising nominal tax rates, which creates an institutional incentive to preserve zoning restrictions and resist upzoning that would depress per-unit land values even while increasing overall housing supply.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_tax_base_administrators, beneficiary,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, municipal_tax_base_administrators, agenda_setter).

% Pay rents set within a supply envelope constrained by zoning and permitting rules they had no vote in shaping, and face landlords whose costs (including financing and taxes) are passed through. Exit means relocating away from jobs, schools, and social networks, which is not a real option for most within a metro area's labor market.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, local).

% Compete for a supply artificially restricted by zoning, against buyers with existing home equity from incumbents, using credit terms set by lenders whose standards they do not influence, in a marketplace whose price discovery is mediated by platforms that also serve seller and lender interests. Down payment and underwriting friction disproportionately excludes them relative to cash-rich incumbents and investors.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).

% Live in housing forms often excluded or restricted by zoning codes and denied conventional mortgage financing on favorable terms, effectively priced out of the institutional price-formation system entirely and pushed into higher-cost, less-protected financing arrangements or informal tenure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, informal_and_manufactured_housing_residents, payer,
    powerless, immediate, trapped, local).

% Study the interaction of zoning, credit access, tax treatment, and platform intermediation to explain price divergence across otherwise similar metros. Their findings inform (but do not control) policy debate over zoning reform, lending regulation, and property tax design.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zoning stabilizes neighborhood character and infrastructure load planning; lending standards manage default risk and systemic credit exposure; property tax assessment funds local services; intermediary platforms reduce search and information costs in an otherwise opaque market. Each function, taken alone, solves a real coordination problem.
% TRANSFER_FUNCTION: The combined institutional apparatus moves wealth from renters and first-time buyers (who pay elevated prices/rents produced by artificially constrained supply and credit-gated demand) to incumbent owners (equity appreciation), lenders (interest and securitization spreads), and intermediary platforms (commissions and data rents), while also shifting fiscal burden away from rising assessed values onto transaction-stage buyers.
% ABSENT_VOICES: Renters and prospective first-time buyers rarely have standing votes in municipal zoning hearings, which are structurally dominated by existing homeowners; informal and manufactured housing residents are almost entirely absent from both zoning deliberation and mortgage-market design.
% DISAPPEARANCE_RATIONALE: If zoning restrictions, current lending standards, favorable tax treatment of ownership, and platform-mediated valuation all vanished simultaneously, effective housing supply would expand, credit access would broaden or contract unpredictably, assessed-value-driven municipal budgets would need restructuring, and price discovery would shift to a fundamentally different (and currently untested) mechanism — incumbent equity gains, lender securitization margins, and platform commission structures would all be disrupted.
% FOUNDING_PROBLEM: Zoning arose to separate incompatible land uses and manage infrastructure and public health externalities; lending standards arose to manage default and systemic financial risk after credit crises; property tax arose to fund local government without direct central transfers; intermediary platforms arose to reduce information asymmetry and search costs in a highly heterogeneous, illiquid asset market.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning bodies and lender trade associations attest the founding problems (land-use conflict, systemic credit risk) remain live and justify current structures. Independent housing economists, federal housing finance researchers, and legislative testimony in multiple jurisdictions attest that the original coordination functions have been substantially captured by exclusionary and rent-preserving purposes — supply restriction functioning as a wealth-preservation mechanism for incumbents rather than a genuine land-use or risk-management necessity.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.62 (moderate-high) reflecting that the transfer is real and substantial but partially offset by genuine coordination value in each component apparatus — this is not pure extraction. Suppression is authored at 0.58: zoning hearings, underwriting criteria, and platform algorithms are not fully open to challenge by those they price out, but formal legal avenues for zoning reform and lending-discrimination litigation exist and are sometimes used. Theater ratio is moderate-low (0.30) because the coordination functions (public health zoning, default-risk underwriting) are substantively real, not merely performative, even though their current configuration serves exclusionary ends. Resistance (0.55) and accessibility_collapse (0.50) reflect an actively contested arrangement — YIMBY movements, lending-discrimination suits, and platform-transparency campaigns represent real, ongoing resistance, not a fully settled structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, lenders, intermediary platforms, and tax administrators are beneficiaries: each collects rents (equity appreciation, interest/securitization spread, commission/data revenue, stable-or-rising tax base) through the same structure that constrains supply and gates credit access. Renters, first-time buyers, and informal/manufactured housing residents are victims: they pay elevated prices or rents, face credit gatekeeping they did not design, and in the case of informal housing residents are substantially excluded from the formal price-formation system altogether. The directionality derivation places incumbents and institutional actors near the beneficiary pole (low d) and renters/informal residents near the full-target pole (high d, trapped exit); first-time buyers sit closer to the target pole than incumbents but retain more mobility than renters.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) is essential here: each institutional component solves a problem that would recur if the institution vanished (land-use externalities, credit-risk management, local fiscal funding, information asymmetry in an illiquid market) — collapsing this into pure extraction would mislabel genuine coordination functions as if they had no legitimate basis. But treating it as a pure rope would erase the asymmetric transfer that the disappearance analysis and beneficiary/victim declarations make visible. The founding-problem interview shows contested status precisely because incumbents can honestly point to live coordination problems while excluded parties can honestly point to captured, rent-preserving operation — both readings of the same structure are simultaneously true from different seats, which is the tangled-rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_naturalist_boundary,
    'Is the price level observed in a given housing market better explained as an institutionally constructed outcome (this reading) or as a market-clearing response to underlying scarcity and preference that would emerge even under a laissez-faire regulatory baseline (naturalist_reading)?',
    'Comparative analysis of metros with substantially different zoning/lending/tax regimes but similar underlying scarcity and demographic pressure; natural experiments from major zoning reforms (e.g., statewide upzoning mandates) isolating the institutional contribution to price from the scarcity contribution.',
    'If price variance across similar-scarcity metros is dominated by regulatory variation, the institutional reading is strongly supported and the naturalist reading''s implicit claim of institutional neutrality is undermined. If price variance persists even after regulatory harmonization, the naturalist reading gains support and this reading''s beneficiary/victim framing overstates institutional causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_naturalist_boundary, empirical, 'Whether institutional construction or natural scarcity dominates observed price variance across markets.').

omega_variable(
    coordination_capture_separability,
    'Can the genuine coordination functions of zoning, underwriting, and tax assessment be structurally separated from their exclusionary/rent-preserving effects, or are the two inseparable in current institutional design?',
    'Track outcomes in jurisdictions that have attempted to preserve coordination functions (public health zoning, prudent underwriting) while removing exclusionary elements (e.g., by-right multifamily zoning, algorithmic fair-lending audits) — if coordination outcomes hold while exclusion falls, separability is demonstrated.',
    'If separable, current extractiveness is largely attributable to a removable exclusionary layer rather than the coordination function itself, supporting reform-oriented interventions. If inseparable, the measured extraction may be a structural feature of any workable version of these institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_capture_separability, conceptual, 'Whether coordination and extraction are separable within the institutional apparatus.').

omega_variable(
    reading_disagreement_locus,
    'Where exactly do the four kernel readings (institutional, naturalist, financialization, georgist) locate the causal mechanism of price formation, and can more than one be simultaneously correct about different components of the same observed price?',
    'Structural decomposition of a given metro''s price level into a land-rent component (georgist), a credit-expansion component (financialization), a regulatory-constraint component (institutional), and a residual scarcity/preference component (naturalist), using hedonic and time-series decomposition methods.',
    'If the decomposition yields a large regulatory-constraint coefficient, the institutional_reading''s tangled-rope classification is well-supported as the dominant mechanism; if the credit-expansion or land-rent coefficients dominate, the sibling readings'' classifications carry more explanatory weight for the same observed prices even though all four remain structurally distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_locus, conceptual, 'Locating where the four kernel readings'' causal claims agree, diverge, or are jointly non-exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.56).
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
% This is one of four sibling constraint stories decomposing the natural-language claim 'housing price formation' per the ε-invariance principle: naturalist_reading (mountain-flavored, low ε — price as equilibrium discovery), georgist_reading (decomposes land rent from improvement value), financialization_reading (credit-cycle asset-price dynamics), and this institutional_reading (tangled-rope, moderate-high ε — price as constructed by zoning/lending/tax/platform apparatus). Each reading has its own ε, beneficiary/victim structure, and claimed type; they are linked here because they compete for explanatory primacy over the same observed price phenomenon without being the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
