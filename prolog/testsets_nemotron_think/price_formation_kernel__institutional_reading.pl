% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Housing price formation in the United States is not a natural equilibrium
 *   but a constructed outcome of four interlocking institutional pillars: (1)
 *   Euclidean zoning that restricts supply elasticity in high-productivity
 *   regions; (2) lending standards and GSE guarantees that channel credit
 *   into existing collateral rather than new production; (3) tax treatment
 *   (mortgage interest deduction, property tax caps, capital gains
 *   exclusions, 1031 exchanges) that subsidizes demand and locks in
 *   incumbents; (4) intermediary platforms (MLS, Zillow, CoreLogic, CoStar)
 *   that capture transaction rents and data rents while reinforcing opacity.
 *   The institutional reading holds that these are not separable
 *   'distortions' but a coherent regime whose function is to convert location
 *   rent into financialized asset appreciation for incumbent owners and their
 *   financial intermediaries. The constraint is a tangled rope: it genuinely
 *   coordinates land use, credit risk, and local finance (rope function), but
 *   does so through mechanisms that extract from renters, first-time buyers,
 *   and mobile workers (snare function), and requires active enforcement
 *   (zoning boards, GSE conservatorship, tax code, platform terms of service)
 *   to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "economic/political/social").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '0060dd77-c392-4ff2-a927-e743b2fcd730').
narrative_ontology:cs_kernel_codification('0060dd77-c392-4ff2-a927-e743b2fcd730', distributed).
narrative_ontology:cs_authority_grounding('0060dd77-c392-4ff2-a927-e743b2fcd730', extraction).
narrative_ontology:cs_interpretation_layer_present('0060dd77-c392-4ff2-a927-e743b2fcd730').
narrative_ontology:cs_reading_relation('0060dd77-c392-4ff2-a927-e743b2fcd730', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('0060dd77-c392-4ff2-a927-e743b2fcd730', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0060dd77-c392-4ff2-a927-e743b2fcd730', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('0060dd77-c392-4ff2-a927-e743b2fcd730', foundational, housing_price_is_institutionally_constructed).
narrative_ontology:cs_axiom_status(housing_price_is_institutionally_constructed, holdable).
narrative_ontology:cs_axiom_grounding('0060dd77-c392-4ff2-a927-e743b2fcd730', housing_price_is_institutionally_constructed, conventional).
narrative_ontology:cs_axiom('0060dd77-c392-4ff2-a927-e743b2fcd730', secondary, regulatory_restrictiveness_determines_extraction_magnitude).
narrative_ontology:cs_axiom_status(regulatory_restrictiveness_determines_extraction_magnitude, holdable).
narrative_ontology:cs_axiom_grounding('0060dd77-c392-4ff2-a927-e743b2fcd730', regulatory_restrictiveness_determines_extraction_magnitude, empirically_contingent).
narrative_ontology:cs_reference_frame('0060dd77-c392-4ff2-a927-e743b2fcd730', post_war_housing_settlement).
narrative_ontology:cs_drift_state('0060dd77-c392-4ff2-a927-e743b2fcd730', contemporary_financialized_housing, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0060dd77-c392-4ff2-a927-e743b2fcd730', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_owners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, property_tax_assessors).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, mobile_workers).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, housing_as_asset_paradigm).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, local_control_doctrine).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, mortgage_interest_deduction_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own homes in supply-constrained jurisdictions. Benefit from artificial scarcity that inflates asset values. Politically organized through homeowner associations and local electoral influence. Exit means selling and leaving the community they've invested in socially and financially.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_owners, beneficiary,
    organized, biographical, constrained, local).

% Originate and service mortgage credit under federal guarantees (GSEs, FHA) and regulatory frameworks they helped shape. Profit from volume and interest spread on loans whose collateral value is propped up by supply constraints. Can reallocate capital across markets; not tied to any single jurisdiction.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Agents, brokers, platforms (Zillow, Redfin), title companies, appraisers. Collect transaction fees proportional to price levels. Benefit from complexity and opacity they help maintain. Mobile across firms and platforms; tied to the transaction volume model.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, mobile, regional).

% Local government revenue depends on assessed values. Assessment methodologies often lag market appreciation, but the fiscal incentive aligns with rising prices. Administer the tax treatment (Prop 13-type caps, homestead exemptions) that locks in incumbent advantage.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, property_tax_assessors, agenda_setter,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, property_tax_assessors, beneficiary).

% Pay rents that capitalize the same scarcity premium owners capture. No equity accumulation, no tax advantages, no political voice in zoning decisions. Exit means moving to cheaper markets — often sacrificing labor market access, social networks, or school quality.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Face down-payment barriers inflated by price levels, lending standards that price risk onto borrowers, and competition from cash buyers. Tax treatment (MID) disproportionately benefits higher-income existing owners. Exit means delaying household formation or accepting long commutes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).

% Labor mobility constrained by housing costs in productive metros. Bear the spatial mismatch cost — lower wages or unemployment — because they cannot afford housing near jobs. Exit is their primary leverage: they can refuse to relocate, but at career cost.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mobile_workers, payer,
    moderate, biographical, mobile, national).

% Set land-use rules that determine supply elasticity. Captured by incumbent homeowner interests in practice. Their decisions are the primary structural enforcement mechanism for artificial scarcity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, zoning_boards, agenda_setter,
    institutional, generational, analytical, local).

% FHFA, CFPB, HUD — set lending standards, GSE conservatorship policy, fair lending rules. Their mandates are formally about stability and access; in practice they reproduce the asset-inflation logic by backstopping the credit system that finances it.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, federal_housing_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Study the mechanism, document the welfare loss, propose supply-side remedies. Their consensus (Glaeser, Gyourko, Saiz, etc.) is that zoning is the primary driver, but political economy blocks reform. No direct stake in outcomes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, urban_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates three distinct problems: (1) land-use externalities via zoning (nuisance, infrastructure loading); (2) mortgage credit risk distribution via lending standards and GSE guarantees; (3) fiscal stability of local government via property tax base. The arrangement solves genuine coordination needs but does so through mechanisms that structurally extract from non-owners.
% TRANSFER_FUNCTION: Moves wealth from renters and first-time buyers (via elevated rents and purchase prices) to incumbent owners (capital gains), mortgage lenders (interest volume on larger loans), and intermediaries (ad valorem fees). Tax treatment (mortgage interest deduction, property tax caps, 1031 exchanges) amplifies the transfer upward. Platform intermediation extracts data and transaction rents from both sides.
% ABSENT_VOICES: Future residents (unborn, not yet migrated) who will inherit the constrained supply; displaced low-income communities pushed to exurbs with worse access; homeless populations for whom the price floor is absolute exclusion; construction workers who would be employed under elastic supply. They are absent because they lack political standing in the jurisdictions that set the rules.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax preferences, and platform gatekeeping vanished overnight: land values would collapse in supply-constrained metros; mortgage volumes would shrink as collateral values fall; local fiscal models would crisis; construction would boom where labor markets are strong; rents would fall toward construction cost; wealth would shift from asset-holders to income-earners. The spatial economy of the US would reorganize fundamentally.
% FOUNDING_PROBLEM: The post-Great Depression settlement: (1) stabilize a collapsed housing finance system via federal mortgage insurance and secondary markets; (2) give local control over land use to manage growth externalities; (3) subsidize homeownership as a wealth-building vehicle for the middle class. The institutional architecture (FHA, VA loans, GSEs, Euclidean zoning, MID) was built to solve 1930s–1950s problems: credit freeze, urban overcrowding, returning veterans' housing.
% FOUNDING_PROBLEM_CORROBORATION: The original problems are historically documented (New Deal housing acts, 1926 Euclid v. Ambler, 1954 Berman v. Parker). Urban economists (Glaeser & Gyourko 2003, 2018; Hsieh & Moretti 2019) corroborate that the founding problems are substantially solved or transformed: credit is no longer frozen, zoning externalities are now dominated by exclusionary effects, homeownership wealth-building works only for those who already own. The institutional beneficiaries (NAR, MBA, NLC) assert the problems remain live; independent scholarship largely disagrees.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects the gap between marginal construction cost and market price in constrained metros (Gyourko-Molloy 2015: 50%+ of price is regulatory tax). Suppression (0.55) is moderate: alternatives exist (move, rent, commute) but are costly; the constraint does not rely on overt coercion but on structural exclusion. Theater ratio (0.42) is significant: zoning hearings, affordability mandates, GSE affordable housing goals, platform 'transparency' features perform coordination while the extraction machinery runs. Accessibility collapse (0.52) — alternatives are not fully collapsed (people do move, build ADUs, win lotteries) but the cost is high. Resistance (0.48) — YIMBL movements, state preemption laws (CA SB9, MT, FL), federal proposals (Housing Supply Act) show active but not yet decisive pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent owner seat, the constraint appears as rope: zoning protects neighborhood character, mortgage credit enables their children to buy, tax treatment rewards their investment. From the renter seat, it appears as snare: every pillar extracts with no offsetting benefit. From the lender seat, it appears as rope with extraction: they provide genuine credit intermediation but the regulatory substrate guarantees their margins. The engine will compute these divergences from the power/exit/beneficiary structure authored above.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent owners are structural beneficiaries (d ~ 0.15): they collect the scarcity rent via capital gains and imputed rent, and control the local political mechanism (zoning). Mortgage lenders are beneficiaries with arbitrage exit (d ~ 0.10): they profit from volume and spread, can reallocate nationally. Real estate intermediaries are beneficiaries with mobile exit (d ~ 0.20): they capture transaction fees, can shift platforms. Property tax assessors are agenda-setters with analytical exit (d ~ 0.25): they administer the fiscal lock-in. Renters are full targets (d ~ 0.90): trapped by labor markets, no equity, no voice. First-time buyers are constrained targets (d ~ 0.75): some escape via family transfer or high income. Mobile workers are mobile targets (d ~ 0.60): they can exit but at career cost. Zoning boards and federal regulators are agenda-setters with analytical exit (d ~ 0.30): they enforce the structure but are not its primary extractors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1930s credit freeze, 1950s veteran housing) is dead. The arrangement persists because the solution (supply restriction + credit expansion + tax subsidy) became the business model for the beneficiaries. Mandatrophy is resolved: the coordination function (credit access, land-use externalities) could be served by different mechanisms (by-right zoning, portfolio lending, land-value tax), but the extractive coalition blocks transition. The constraint is not a piton — it is actively maintained and expanded (e.g., GSE conservatorship 2008, platform monopolization 2010s) — but its coordination justification has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Which structural element of price formation do the four readings disagree on most fundamentally: the causal primacy of institutions vs. markets, the separability of land and improvement value, the role of credit vs. real fundamentals, or the normative status of the resulting distribution?',
    'Counterfactual simulation: remove each pillar (zoning, GSEs, MID, platforms) in a spatial equilibrium model and measure price decomposition. Compare with historical natural experiments (Houston no-zoning, Prop 13, GSE reform proposals).',
    'If institutional pillars explain >50% of price variance in constrained metros, institutional_reading gains causal primacy. If credit cycles explain more, financialization_reading gains. If land/improvement separation holds empirically, georgist_reading gains. If none dominate, naturalist_reading''s ''equilibrium'' claim is unfalsifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, empirical, 'Location of the structural disagreement among kernel readings').

omega_variable(
    coordination_extraction_boundary,
    'How much of the measured extractiveness (0.65) is the necessary cost of genuine coordination (land-use externalities, credit risk pooling, local public finance) versus pure rent extraction enabled by captured institutions?',
    'Decompose the ''regulatory tax'' (Gyourko-Molloy) into: (a) efficient Pigouvian pricing of externalities; (b) inefficient exclusionary zoning; (c) credit subsidies to incumbents; (d) tax expenditures captured by owners; (e) platform rents. Requires micro-data on zoning stringency, loan-level pricing, tax expenditure incidence, platform take-rates.',
    'If (a) is small relative to (b)-(e), the tangled_rope classification sharpens: coordination is a thin veneer on extraction. If (a) is large, the rope function is genuine and the extraction is a design flaw, not the purpose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Boundary between genuine coordination cost and extractive overhead in the institutional architecture').

omega_variable(
    incumbent_owner_identity_lock,
    'Are incumbent owners'' exit_options correctly characterized as ''constrained'' rather than ''identity_locked''? Their political identity (homeowner, neighborhood defender) may fuse with the constraint such that exit is psychologically unavailable even when financially possible.',
    'Survey/experiment: offer incumbent owners a buyout at 120% of market value contingent on upzoning their neighborhood. Measure acceptance vs. stated preferences. Track voting behavior on upzoning referenda vs. personal financial interest.',
    'If identity_locked, their directionality d shifts toward 0.5 (symmetric) — they bear psychic cost of the constraint they enforce. This would reduce effective extraction for the owner seat and increase the snare character for the renter seat (the extractor is also trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_owner_identity_lock, conceptual, 'Whether incumbent owners are identity-locked into the constraint they politically maintain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_formation_institutional_tr_t1950, price_formation_kernel__institutional_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(price_formation_institutional_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(price_formation_institutional_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(price_formation_institutional_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.33).
narrative_ontology:measurement(price_formation_institutional_tr_t2000, price_formation_kernel__institutional_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(price_formation_institutional_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(price_formation_institutional_tr_t2024, price_formation_kernel__institutional_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(price_formation_institutional_be_t1950, price_formation_kernel__institutional_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(price_formation_institutional_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(price_formation_institutional_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(price_formation_institutional_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(price_formation_institutional_be_t2000, price_formation_kernel__institutional_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(price_formation_institutional_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(price_formation_institutional_be_t2024, price_formation_kernel__institutional_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(price_formation_institutional_su_t1950, price_formation_kernel__institutional_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(price_formation_institutional_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(price_formation_institutional_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(price_formation_institutional_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(price_formation_institutional_su_t2000, price_formation_kernel__institutional_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(price_formation_institutional_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(price_formation_institutional_su_t2024, price_formation_kernel__institutional_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, zoning_restriction_regime).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mortgage_interest_deduction).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, gse_conservatorship).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, platform_intermediation_monopoly).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, property_tax_cap_prop13).

% DUAL FORMULATION NOTE:
% Part of the price_formation_kernel constraint family. This reading (institutional) emphasizes the regulatory and fiscal construction of prices. The naturalist_reading treats the same price outcomes as equilibrium phenomena. The georgist_reading decomposes the price into land rent vs. improvement value. The financialization_reading emphasizes credit cycles and asset-manager demand. All four readings share the kernel 'price_formation_kernel' but instantiate different constraints with different ε, beneficiaries, and victims. This reading's ε (0.65) is higher than naturalist_reading's (near 0) because it sees extraction where naturalist sees equilibrium. It is lower than financialization_reading's (which may exceed 0.8 in boom phases) because it treats credit as a pillar among others, not the sole driver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, organized, 0.15).
constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
