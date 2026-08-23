% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story models the institutional reading of the housing
 *   price formation kernel: the claim that prices are not natural scarcity
 *   signals but are actively constructed by zoning codes, lending standards,
 *   tax expenditures, and intermediary platforms. The arrangement coordinates
 *   some genuine functions (land-use compatibility, credit standardization)
 *   while simultaneously extracting scarcity rents from non-owners. The story
 *   is authored as a tangled rope: active enforcement is required to maintain
 *   the supply restrictions and preferential credit channels that keep prices
 *   above construction costs. Key agents include incumbent homeowners who
 *   capture appreciation, mortgage lenders and real estate intermediaries who
 *   scale revenue with price, local zoning authorities who administer the
 *   restrictions, and renters/first-time buyers who pay the extraction. This
 *   is one reading of a contested kernel; sibling readings (naturalist,
 *   georgist, financialization) are separate constraints.
 *
 * KEY AGENTS:
 *   - Incumbent homeowners (beneficiary/powerful/mobile): Capture land-value appreciation engineered by supply constraint; politically dominant in local zoning politics.
 *   - Mortgage lenders (beneficiary/institutional/arbitrage): Profit from larger loan volumes collateralized by artificially high prices; capital is mobile across asset classes.
 *   - Real estate intermediaries (beneficiary/organized/mobile): Collect fees proportional to transaction prices and volume.
 *   - Local zoning authorities (agenda_setter/institutional/constrained): Administer the supply constraint under statutory authority and homeowner political pressure.
 *   - Renters (payer/powerless/trapped): Pay scarcity rents without wealth accumulation; excluded from local political process.
 *   - First-time buyers (payer/moderate/constrained): Must over-leverage to enter a supply-constrained market.
 *   - Housing policy researchers (observer/analytical/analytical): Document the transfer but are politically marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.72).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '7f3457c5-fdf5-4237-97a0-32148229a047').
narrative_ontology:cs_kernel_codification('7f3457c5-fdf5-4237-97a0-32148229a047', formalized).
narrative_ontology:cs_authority_grounding('7f3457c5-fdf5-4237-97a0-32148229a047', lineage).
narrative_ontology:cs_interpretation_layer_present('7f3457c5-fdf5-4237-97a0-32148229a047').
narrative_ontology:cs_reading_relation('7f3457c5-fdf5-4237-97a0-32148229a047', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7f3457c5-fdf5-4237-97a0-32148229a047', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f3457c5-fdf5-4237-97a0-32148229a047', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('7f3457c5-fdf5-4237-97a0-32148229a047', foundational, housing_price_is_institutionally_constructed).
narrative_ontology:cs_axiom_status(housing_price_is_institutionally_constructed, holdable).
narrative_ontology:cs_axiom_grounding('7f3457c5-fdf5-4237-97a0-32148229a047', housing_price_is_institutionally_constructed, empirically_contingent).
narrative_ontology:cs_axiom('7f3457c5-fdf5-4237-97a0-32148229a047', foundational, local_regulatory_authority_over_land_use_is_legitimate).
narrative_ontology:cs_axiom_status(local_regulatory_authority_over_land_use_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7f3457c5-fdf5-4237-97a0-32148229a047', local_regulatory_authority_over_land_use_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('7f3457c5-fdf5-4237-97a0-32148229a047', institutional_price_governance).
narrative_ontology:cs_drift_state('7f3457c5-fdf5-4237-97a0-32148229a047', contemporary_affordability_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f3457c5-fdf5-4237-97a0-32148229a047', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold property whose nominal value rises when zoning restricts supply and tax treatment favors ownership. They capture unearned increments in land value and can realize gains by selling or borrowing against equity. Politically dominant in local zoning hearings.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    powerful, biographical, mobile, national).

% Originate and service loans collateralized by housing values that are inflated by supply constraints and preferential tax treatment. They benefit from larger loan volumes and interest payments tied to higher nominal prices. Capital can be redeployed to other asset classes if returns compress.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, national).

% Collect transaction fees and commissions proportional to sale prices. Their revenue scales with price level and turnover, giving them a structural interest in maintaining the institutional machinery that keeps prices elevated and transaction volume flowing.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Administer zoning codes, permitting, and land-use plans that directly constrain housing supply. They operate under state-enabling statutes and intense political pressure from incumbent homeowners. They set the formal rules but are themselves constrained by legal mandates and electoral politics.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_zoning_authorities, agenda_setter,
    institutional, biographical, constrained, local).

% Pay rents determined by artificially constrained supply in desirable jurisdictions. They bear the cost of exclusionary zoning without capturing appreciation. Most cannot afford to buy into the ownership system and lack political representation in local land-use decisions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, local).

% Must accumulate larger down payments and accept higher debt burdens to enter a market where prices are sustained by regulatory restriction and preferential credit allocation. They pay the extraction as mortgage debt and deferred wealth accumulation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, national).

% Study the relationship between zoning, lending standards, tax expenditure, and price outcomes. They document the transfer but do not participate in it directly. Their findings are often politically marginalized by incumbent coalitions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_policy_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land use across fragmented property ownership, channels credit through standardized underwriting, and stabilizes expectations about neighboring development so that long-term housing investments are not undermined by incompatible adjacent uses.
% TRANSFER_FUNCTION: Moves wealth from renters and prospective first-time buyers to incumbent owners, mortgage lenders, and real estate intermediaries by constraining supply, inflating collateral values, and capturing the resulting scarcity rents through asset appreciation and transaction fees.
% ABSENT_VOICES: Future residents and would-be in-migrants are excluded from local zoning hearings because they have no standing. Renters are systematically underrepresented relative to homeowners in local politics. The unborn generation that will inherit the affordability crisis has no voice.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax treatment, and intermediary controls vanished overnight, land would reallocate to highest use, housing supply would expand toward construction-cost limits, price gradients would flatten, and the wealth positions of incumbent owners and lenders would collapse while renters and new buyers gained entry.
% FOUNDING_PROBLEM: Preventing incompatible land uses that create negative externalities (nuisance, health hazards), solving information asymmetry in mortgage markets, and creating a stable tax base for local public goods.
% FOUNDING_PROBLEM_CORROBORATION: Urban historians and public-health scholars corroborate the original land-use and safety motivations. Housing economists outside the beneficiary coalition argue the founding problems have been solved and the constraints now function as extraction; incumbent homeowners and real estate interests attest the problems remain live.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is set at 0.68 because the gap between median home prices and replacement construction costs in restrictive jurisdictions is large and persistent, indicating substantial rent extraction above coordination cost. Suppression is 0.72 because the arrangement requires active enforcement: zoning boards must deny permits, lenders must apply underwriting standards selectively, and tax treatment must be maintained by statute. Theater ratio is 0.40 because a significant share of zoning and planning activity is performative community consultation that masks exclusionary intent, while some genuine safety and coordination functions remain. Accessibility collapse is 0.60 because alternatives (unrestricted private land use, non-mortgage housing finance) are legally foreclosed but remain imaginable. Resistance is 0.55 because renters and affordability advocates mount sustained but politically disadvantaged opposition. The measurement series show monotonic increases from interval start to end, reflecting the ratchet of accumulating regulation and financialization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (zoning authorities, incumbent owners, lenders) experience the constraint as necessary community protection and market stability. The payer seats (renters, first-time buyers) experience the same structure as exclusion and debt extraction. The engine computes this divergence from structural data: low directionality for mobile beneficiaries with arbitrage exit, high directionality for trapped and constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, mortgage lenders, and real estate intermediaries are declared beneficiaries with relatively mobile or arbitrage-grade exit options, placing their directionality near the beneficiary end (low d). Renters are declared payers with trapped exit, placing their directionality near the target end (high d). First-time buyers are payers with constrained exit, also high d. Local zoning authorities are agenda_setters with constrained exit, sitting nearer the symmetric middle because they administer rather than collect the extraction, though they are not its primary targets. Housing policy researchers are observers with analytical exit, directionality neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â coordinating land use and stabilizing mortgage markets â was genuinely live in the early twentieth century. The tangled rope classification captures that the coordination function has not fully atrophied (some genuine safety and compatibility regulation remains) but has become hybridized with extraction as incumbent coalitions captured the regulatory machinery. Without the tangled rope category, the constraint would be misread as either a pure rope (ignoring the extraction from non-owners) or a pure snare (ignoring the residual coordination function of building codes and credit standards). The R5 genealogy marks the founding problem as contested, preventing false naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_restriction_coordination_or_extraction,
    'Does restrictive zoning and lending policy serve a genuine coordination function (protecting neighborhood stability, ensuring credit safety) or has it become primarily a mechanism for scarcity rent extraction?',
    'Cross-jurisdictional natural experiment comparing housing cost outcomes in places with restrictive versus permissive land-use regulation, holding demand constant.',
    'If restriction is unnecessary for coordination, the constraint reclassifies toward snare; if it is essential, the coordination half of the tangled rope strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_restriction_coordination_or_extraction, empirical, 'Whether supply restriction is coordination or extraction.').

omega_variable(
    kernel_reading_boundary,
    'Does the institutional reading of price formation remain analytically separable from the financialization reading, or have lending standards and credit expansion become so intertwined that the readings describe the same mechanism?',
    'Decomposition analysis isolating the price contribution of zoning-induced supply constraint versus credit-induced demand expansion in overlapping jurisdictions.',
    'If inseparable, the institutional reading may need to merge with financialization into a single constraint; if separable, the kernel decomposition is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Boundary ambiguity between institutional and financialization readings.').

omega_variable(
    tax_treatment_benefit_diffusion,
    'Do mortgage interest deductions and capital-gains exclusions on primary residences function as diffuse subsidies to ownership, or do they capitalize into price and accrue to incumbent owners and lenders as concentrated rents?',
    'Econometric identification of tax-incidence shifts using reform episodes or spatial discontinuities in tax treatment.',
    'If benefits capitalize fully into price, the tax component is extractive transfer rather than broad-based coordination; if not, it retains a coordination-support character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_treatment_benefit_diffusion, empirical, 'Whether tax benefits accrue to owners or buyers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_formation_institutional_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(price_formation_institutional_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(price_formation_institutional_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(price_formation_institutional_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(price_formation_institutional_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(price_formation_institutional_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(price_formation_institutional_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(price_formation_institutional_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(price_formation_institutional_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(price_formation_institutional_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(price_formation_institutional_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(price_formation_institutional_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(price_formation_institutional_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(price_formation_institutional_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(price_formation_institutional_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(price_formation_institutional_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(price_formation_institutional_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(price_formation_institutional_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The price_formation_kernel decomposes into four structurally distinct constraints (naturalist, georgist, institutional, financialization readings) because each reading assigns a different epsilon, beneficiary/victim structure, and causal mechanism to price formation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
