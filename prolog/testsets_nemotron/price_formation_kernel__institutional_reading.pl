% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: political_economy/housing/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_reading of the
 *   price_formation_kernel. It asserts that housing prices are primarily
 *   constructed by four institutional mechanisms: zoning (supply
 *   restriction), lending standards (demand subsidization and risk
 *   allocation), tax treatment (owner-occupier preference and investor
 *   advantage), and intermediary platforms (transaction cost extraction).
 *   These mechanisms coordinate real activity (land use, credit, search) but
 *   extract asymmetrically from renters and first-time buyers to incumbent
 *   owners, lenders, and intermediaries. The reading coexists with
 *   georgist_reading (different cut on the same phenomena) and influences
 *   naturalist_reading (undermines natural equilibrium claim) and
 *   financialization_reading (provides the regulatory infrastructure that
 *   financialization exploits).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing/institutional").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, 'bb61cd71-0d5b-4a7f-8945-bde3db27391b').
narrative_ontology:cs_kernel_codification('bb61cd71-0d5b-4a7f-8945-bde3db27391b', distributed).
narrative_ontology:cs_authority_grounding('bb61cd71-0d5b-4a7f-8945-bde3db27391b', practice).
narrative_ontology:cs_interpretation_layer_present('bb61cd71-0d5b-4a7f-8945-bde3db27391b').
narrative_ontology:cs_reading_relation('bb61cd71-0d5b-4a7f-8945-bde3db27391b', price_formation_kernel__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('bb61cd71-0d5b-4a7f-8945-bde3db27391b', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb61cd71-0d5b-4a7f-8945-bde3db27391b', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('bb61cd71-0d5b-4a7f-8945-bde3db27391b', foundational, price_formed_by_institutional_design).
narrative_ontology:cs_axiom_status(price_formed_by_institutional_design, holdable).
narrative_ontology:cs_axiom_grounding('bb61cd71-0d5b-4a7f-8945-bde3db27391b', price_formed_by_institutional_design, empirically_contingent).
narrative_ontology:cs_axiom('bb61cd71-0d5b-4a7f-8945-bde3db27391b', secondary, regulatory_restrictiveness_drives_extraction).
narrative_ontology:cs_axiom_status(regulatory_restrictiveness_drives_extraction, holdable).
narrative_ontology:cs_axiom_grounding('bb61cd71-0d5b-4a7f-8945-bde3db27391b', regulatory_restrictiveness_drives_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('bb61cd71-0d5b-4a7f-8945-bde3db27391b', institutional_neutrality_benchmark).
narrative_ontology:cs_drift_state('bb61cd71-0d5b-4a7f-8945-bde3db27391b', post_2008_regulatory_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bb61cd71-0d5b-4a7f-8945-bde3db27391b', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_owners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, property_tax_assessors).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, mobile_workers).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, institutional_price_construction).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, regulatory_capture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property in supply-constrained jurisdictions. Benefit from zoning restrictions that limit new supply, mortgage interest deductions, and capital gains exclusions. Can leverage equity for further investment. Exit via sale at appreciated values or geographic mobility.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_owners, beneficiary,
    organized, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, incumbent_owners, agenda_setter).

% Originate and securitize mortgages under government-sponsored enterprise standards. Benefit from standardized underwriting, implicit guarantees, and interest rate risk subsidies. Shape lending standards through regulatory capture of GSEs and banking regulators. Exit via portfolio diversification across asset classes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Earn commissions on transactions structured by platform listing services, MLS rules, and standardized contracts. Benefit from transaction volume driven by constrained supply and regulatory complexity. Exit constrained by licensing, network effects, and platform dependence.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    moderate, biographical, constrained, local).

% Set assessed values that determine tax revenue and indirectly validate price levels. Benefit from rising assessments expanding fiscal capacity. Constrained by political resistance to reassessment and statutory limits. Exit effectively trapped within jurisdictional mandate.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, property_tax_assessors, agenda_setter,
    institutional, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, property_tax_assessors, beneficiary).

% Pay rents that capitalize the full institutional premium (zoning scarcity, tax treatment, platform fees). No equity accumulation, no tax deductions, no control over supply. Exit constrained by job location, search costs, and universal application of the same institutional regime across jurisdictions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Face down payment barriers inflated by institutional price premiums, lending standards that price in regulatory risk, and competition from investor buyers advantaged by tax treatment. Identity-locked by cultural expectation of homeownership as adult milestone and wealth-building vehicle. Exit means accepting permanent renter status or geographic displacement.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, identity_locked, regional).

% Cannot access local housing markets due to institutional barriers (zoning, occupancy limits, credentialed labor markets). Pay spatial mismatch costs in commuting or foregone earnings. Excluded from the institutional coalition that sets local rules. Exit via geographic mobility — the only agent with genuine arbitrage-grade exit, but at high personal cost.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mobile_workers, payer,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mobile_workers, excluded).

% Control land use regulation that directly restricts housing supply. Composed of incumbent owners and their representatives. Benefit from maintaining scarcity. Trapped by the institutional logic: upzoning reduces their constituents' asset values and tax base.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, zoning_boards, agenda_setter,
    organized, generational, trapped, local).

% Study price formation mechanisms across institutional regimes. Provide empirical evidence on supply elasticity, regulatory incidence, and distributional effects. No direct stake in outcomes but shape the epistemic framework through which policymakers understand the constraint.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, academic_urban_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land use, credit allocation, and transaction infrastructure across fragmented jurisdictions to produce a functioning (if distorted) housing market. Zoning resolves externalities; lending standards coordinate risk; tax treatment coordinates investment; platforms coordinate search.
% TRANSFER_FUNCTION: Moves economic rent from renters and first-time buyers to incumbent owners, lenders, and intermediaries via: (1) zoning-induced scarcity rents capitalized into prices, (2) mortgage interest deduction and GSE subsidies flowing to lenders and leveraged buyers, (3) transaction costs extracted by intermediaries from every sale, (4) property tax assessment lag shifting burden to new buyers.
% ABSENT_VOICES: Future residents (unborn, not yet migrated) who would occupy housing if supply responded to demand. Homeless populations whose exclusion is the sharpest edge of the constraint. Non-local workers barred by occupational licensing and credentialing that interact with housing regulation. None have standing in local zoning hearings or GSE governance.
% DISAPPEARANCE_RATIONALE: If institutional price construction vanished overnight: zoning would liberalize, supply would expand, prices would fall toward construction cost, mortgage markets would reprice without GSE subsidies, transaction platforms would compete on fee rather than monopoly access. The spatial distribution of population, the wealth distribution across generations, and the financial system's collateral base would all reorganize.
% FOUNDING_PROBLEM: Post-WWII housing shortage required rapid coordination of land use, mortgage finance, and construction standards to house returning veterans and urbanizing populations. The institutional framework (Euclidean zoning, FHA/VA lending, GSEs, MLS) solved this coordination problem at the cost of embedding extraction mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Historical housing production data (1950-1970) shows the founding supply crisis was resolved by 1970. The institutional framework persisted and intensified after the founding problem disappeared. Corroborated by economic historians (e.g., Glaeser, Gyourko) and the National Bureau of Economic Research housing working groups — sources outside the beneficiary coalition.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the accumulated institutional premium: price-to-rent ratios far above construction cost in supply-constrained metros, mortgage subsidies capitalized into prices, transaction costs 5-6% per sale. Suppression (0.55) is moderate: alternatives exist (move, rent, don't buy) but are structurally constrained by the same institutional regime across jurisdictions. Theater ratio (0.28) captures the growing gap between stated purposes (neighborhood character, financial stability, homeownership promotion) and actual operation (asset inflation, rent extraction). Accessibility collapse (0.42) and resistance (0.58) reflect that alternatives are partially visible (Tokyo, Vienna, Houston) but politically blocked.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent owner seat: the constraint is a coordination success (neighborhood stability, asset appreciation). From the renter seat: it is a snare (permanent extraction, no voice). From the lender seat: it is a rope with extractive features (GSE guarantee enables lending but also subsidizes risk). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent owners and lenders are structural beneficiaries (d near 0): they collect rents, subsidies, and capital gains. Renters and first-time buyers are targets (d near 1): they pay the full institutional premium with no offsetting benefit. Intermediaries and assessors sit near agenda_setter (d ~ 0.3): they administer and profit from the constraint but depend on its persistence. Mobile workers have arbitrage exit (d ~ 0.2) but pay high personal costs. Zoning boards are trapped agenda_setters (d ~ 0.4): they could change the constraint but face concentrated opposition from their constituent-beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII housing shortage) was resolved by 1970. The institutional framework persisted and intensified, layering extraction onto coordination. Mandatrophy is declared resolved: the arrangement no longer serves its founding function but persists through institutional inertia and beneficiary capture. The dead founding_problem_status + world_rearranges disappearance_verdict mismatch flags this as a capture/zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_financial_extraction_boundary,
    'How much of the measured extractiveness belongs to institutional design (zoning, tax, platforms) versus financialization dynamics (credit cycles, asset pricing feedback)?',
    'Counterfactual simulation: hold financial variables constant while varying regulatory restrictiveness across jurisdictions; measure price variance explained by each.',
    'If institutional design explains most variance, this reading''s claimed_type (tangled_rope) is validated and financialization_reading is secondary. If financialization dominates, this reading over-claims and the kernel''s primary extraction driver is credit dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_financial_extraction_boundary, empirical, 'Attribution of extraction between institutional and financial layers of the kernel.').

omega_variable(
    coordination_necessity_of_restrictive_zoning,
    'Is restrictive zoning (beyond nuisance mitigation) structurally necessary for the coordination function, or is it pure extraction captured by incumbents?',
    'Natural experiment: compare jurisdictions with similar coordination needs (infrastructure, externalities) but different restrictiveness levels; measure coordination outcomes (sprawl, congestion, fiscal health).',
    'If coordination succeeds without restrictiveness, zoning''s extraction component is separable and the constraint is closer to snare. If coordination fails, the extraction is the price of the coordination function — tangled_rope validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_restrictive_zoning, conceptual, 'Whether the coordination function requires the extractive component or whether they are structurally separable.').

omega_variable(
    kernel_reading_underdetermination,
    'Does the institutional_reading''s framing (price formed by zoning/lending/tax/platforms) represent a distinct causal claim, or is it a descriptive layer that all readings accept but weight differently?',
    'Discourse analysis: do proponents of other readings deny the institutional mechanisms exist, or do they argue those mechanisms are epiphenomenal to deeper drivers (credit, land rent, preferences)?',
    'If other readings deny institutional mechanisms, this reading forecloses them. If they accept mechanisms but dispute primacy, readings coexist_with different weightings. Determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel''s readings are logically incompatible causal claims or competing weightings of shared mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__institutional_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__institutional_reading, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__institutional_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__institutional_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__institutional_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__institutional_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, zoning_supply_constraint).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mortgage_interest_deduction).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, gse_mortgage_guarantee).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mls_platform_monopoly).

% DUAL FORMULATION NOTE:
% Price formation kernel decomposes into four readings with distinct ε and beneficiary structures. This reading (institutional) emphasizes regulatory construction; financialization emphasizes credit cycles; georgist emphasizes land rent separation; naturalist denies construction. All four share the kernel_id but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, organized, 0.35).
constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, institutional, 0.25).
constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, powerless, 0.92).
constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
