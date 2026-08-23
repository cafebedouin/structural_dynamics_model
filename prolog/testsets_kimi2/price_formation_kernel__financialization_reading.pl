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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Housing Price Formation via Credit Expansion and Asset Financialization
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint is the financialization reading of the contested
 *   price_formation_kernel. It treats housing price formation as driven by
 *   credit expansion, asset-price feedback loops, and demand from
 *   housing-as-financial-asset buyers rather than by shelter scarcity, land
 *   rent, or institutional zoning alone. The kernel is contested: the
 *   naturalist reading sees price as scarcity equilibrium; the georgist
 *   reading sees land rent extraction; the institutional reading sees zoning
 *   and regulatory construction. This reading specifically instantiates the
 *   credit-money heterodox claim that leverage availability and speculative
 *   demand decouple price from use-value, creating a feedback loop where
 *   rising prices justify more credit, which drives further price increases.
 *   The financial sector benefits from transaction volume and securitization
 *   fees; households bear debt service and systemic crash risk. The
 *   constraint is claimed as tangled_rope because the mortgage channel
 *   initially solved a genuine capital-coordination problem for mass housing,
 *   but that function has atrophied while the extraction mechanism has
 *   intensified.
 *
 * KEY AGENTS:
 *   - central_bank_and_regulators: Agenda-setter (institutional/analytical) â administers the monetary and regulatory conditions for credit expansion.
 *   - mortgage_lending_sector: Beneficiary (institutional/arbitrage) â captures debt service and fees.
 *   - property_investors: Beneficiary (powerful/mobile) â captures leveraged appreciation.
 *   - equity_homeowners: Beneficiary (organized/constrained) â captures windfall gains and defends the regime politically.
 *   - leveraged_households: Payer (moderate/identity_locked) â bears debt burden and negative-equity risk.
 *   - prospective_first_time_buyers: Payer (powerless/trapped) â excluded or forced into extreme leverage.
 *   - long_term_renters: Payer (powerless/trapped) â pays rent inflated by asset-demand pricing.
 *   - housing_market_analysts: Observer (analytical/analytical) â evaluates fundamentals vs credit dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.82).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.65).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation via Credit Expansion and Asset Financialization").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '695aed44-7d77-4653-9541-79be8903ea86').
narrative_ontology:cs_kernel_codification('695aed44-7d77-4653-9541-79be8903ea86', implicit).
narrative_ontology:cs_authority_grounding('695aed44-7d77-4653-9541-79be8903ea86', diffuse_epistemic).
narrative_ontology:cs_reading_relation('695aed44-7d77-4653-9541-79be8903ea86', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('695aed44-7d77-4653-9541-79be8903ea86', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('695aed44-7d77-4653-9541-79be8903ea86', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('695aed44-7d77-4653-9541-79be8903ea86', foundational, asset_price_feedback_structural).
narrative_ontology:cs_axiom_status(asset_price_feedback_structural, holdable).
narrative_ontology:cs_axiom_grounding('695aed44-7d77-4653-9541-79be8903ea86', asset_price_feedback_structural, empirically_contingent).
narrative_ontology:cs_axiom('695aed44-7d77-4653-9541-79be8903ea86', foundational, shelter_value_price_decoupling).
narrative_ontology:cs_axiom_status(shelter_value_price_decoupling, holdable).
narrative_ontology:cs_axiom_grounding('695aed44-7d77-4653-9541-79be8903ea86', shelter_value_price_decoupling, empirically_contingent).
narrative_ontology:cs_reference_frame('695aed44-7d77-4653-9541-79be8903ea86', productive_credit_allocation).
narrative_ontology:cs_drift_state('695aed44-7d77-4653-9541-79be8903ea86', post_global_financial_crisis_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('695aed44-7d77-4653-9541-79be8903ea86', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_lending_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, property_investors).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, equity_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leveraged_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, prospective_first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, long_term_renters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets benchmark interest rates, macroprudential lending standards, and liquidity provisions that determine the volume and cost of mortgage credit. Their policy frameworks treat housing market stability as a financial stability objective, underwriting the credit-feedback loop through monetary policy and regulatory forbearance.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_bank_and_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Originates, securitizes, and services mortgage debt; collects origination fees, servicing spreads, and interest income. Benefits directly from transaction volume and balance-sheet expansion. Can shift capital across jurisdictions and asset classes if returns in housing credit decline.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_lending_sector, beneficiary,
    institutional, generational, arbitrage, global).

% Acquires residential assets for rental yield and capital appreciation. Benefits from leverage-fueled price appreciation that outpaces wage growth. Can liquidate positions or reallocate capital across markets, but remains dependent on continued credit availability to sustain valuations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, property_investors, beneficiary,
    powerful, biographical, mobile, national).

% Hold properties with low or no mortgage debt and benefit from price appreciation driven by leveraged demand. Their political lobbying defends tax treatments and zoning that restrict supply and amplify price feedback. Exit is constrained by transaction costs, capital gains exposure, and localized attachment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, equity_homeowners, beneficiary,
    organized, biographical, constrained, national).

% Carry large mortgage debt to access homeownership, paying debt service that transfers income to the financial sector. Bear negative equity risk and foreclosure exposure in downturns. Their self-concept and social standing are often fused with homeownership, making exit to renting or collective ownership feel like personal failure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_households, payer,
    moderate, biographical, identity_locked, national).

% Face prices decoupled from earnings, requiring extreme leverage or permanent exclusion. Must either accept unsustainable debt burdens or remain outside the ownership system. No meaningful arbitrage: waiting for correction means indefinite exclusion as credit expansion continually reprices entry.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, prospective_first_time_buyers, payer,
    powerless, biographical, trapped, national).

% Pay rents that track asset-price-driven landlord cost bases rather than shelter-value fundamentals. Bear the cost of price decoupling without accruing equity. Social housing alternatives have been systematically reduced, leaving private rental as the only fallback.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, long_term_renters, payer,
    powerless, generational, trapped, regional).

% Track credit flows, price-to-income ratios, and securitization volumes. Produce assessments of whether prices reflect fundamentals or leverage dynamics. Their frameworks are contested between neoclassical equilibrium models and credit-money heterodox frameworks.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_market_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes dispersed savings into long-term housing finance, enabling large capital deployment for residential construction and purchase without requiring buyers to hold full equity upfront.
% TRANSFER_FUNCTION: Moves debt service payments, origination fees, and securitization spreads from households to financial intermediaries, and moves wealth gains from future buyers and non-owners to existing asset holders through leveraged price appreciation.
% ABSENT_VOICES: Advocates for decommodified housingâsocial housing movements, community land trusts, and cooperative ownership modelsâare structurally excluded from price-setting mechanisms and policy design. Their absence naturalizes credit-driven allocation as the only viable housing distribution method.
% DISAPPEARANCE_RATIONALE: If credit expansion and asset-price feedback loops ceased to drive price formation, housing values would rapidly recouple to income and rent fundamentals; mortgage lending volumes and securitization fees would collapse; existing homeowner balance sheets would reprice downward; and capital would reallocating away from speculative residential assets.
% FOUNDING_PROBLEM: Post-war capital shortage for mass housing production and homeownership: mobilizing sufficient long-term credit to fund construction and enable purchase without 100% equity requirements.
% FOUNDING_PROBLEM_CORROBORATION: Housing historians and heterodox economists outside the financial sector attest that the initial coordination problemâcapital shortage for mass housingâwas solved decades ago in advanced economies. The financial sector claims the credit channel remains essential for supply; independent macroprudential research demonstrates that credit growth has decoupled from housing stock growth, indicating the founding coordination function has atrophied.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because price levels have decoupled from shelter value and income fundamentals, driven by leverage availability and speculative asset demand; the resulting transfer from households to financial intermediaries and asset holders is substantial. Suppression (0.65) reflects the active enforcement required to maintain the credit-feedback loopâmacroprudential forbearance, monetary policy accommodation, and the structural exclusion of decommodified alternatives. Theater ratio (0.50) indicates that a significant share of housing policy discourse (homeownership as wealth-building, housing ladder metaphors) performs ideological justification rather than describing the actual credit-driven extraction mechanism. Accessibility collapse (0.72) is high because once the financialization frame is accepted, viable alternatives (social housing, non-leveraged ownership) have been systematically dismantled or marginalized. Resistance (0.58) captures growing political contestationâtenant movements, macroprudential dissent, and generational pushbackâwithout yet overcoming the regime's institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (central banks, mortgage lenders, property investors, equity homeowners) experience the constraint as a necessary and largely beneficial coordination mechanism for capital allocation and wealth accumulation. The payer seats (leveraged households, prospective buyers, renters) experience the same structure as extractionâdebt service, exclusion, and rent burdens driven by leverage rather than wages. The engine computes this divergence from the structural data: identical housing prices appear as wealth to asset holders and as unpayable entry barriers to non-owners.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mortgage_lending_sector, property_investors, equity_homeowners) receive low directionality values because the constraint subsidizes or enriches themâcredit expansion generates fees, appreciation, and windfall gains. Payers (leveraged_households, prospective_first_time_buyers, long_term_renters) receive high directionality because the constraint extracts via debt service, inflated entry prices, and rent pass-through. The central_bank_and_regulators seat sits near symmetric: it neither collects rents nor pays them directly, but its institutional legitimacy is bound to the regime's stability. Exit differentiation is structural: financial intermediaries and investors have arbitrage-grade exit (global capital mobility), while households are identity_locked or trapped by the absence of decommodified alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the mortgage credit channel as either pure coordination (rope) or pure extraction (snare). The founding problemâmobilizing capital for mass housingâwas genuine and is corroborated by independent housing historians. However, the R5 genealogy interview records founding_problem_status as dead: credit expansion has decoupled from housing stock growth while the arrangement persists. The coordination function is real but atrophied; the extraction function has grown around it. This prevents the false benignity of calling it a rope (ignoring the household debt burden) and the false malignity of calling it a snare (ignoring the initial and residual coordination role).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint the financialization reading of the price_formation_kernel, or would a naturalist or georgist reading yield a structurally different classification?',
    'Comparative analysis of the sibling constraints (naturalist, georgist, institutional readings) within the same kernel family; evaluate whether credit expansion or scarcity/land-rent/institutional construction is the dominant price driver.',
    'If the naturalist reading is adopted, the constraint dissolves into a low-extraction mountain or rope; if the financialization reading holds, it remains a tangled_rope with high extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of a contested kernel with structurally divergent siblings.').

omega_variable(
    credit_mobilization_vs_extraction,
    'Does current mortgage credit expansion still perform a genuine housing-supply coordination function, or has it become purely extractive securitization machinery decoupled from stock growth?',
    'Time-series regression of credit flow against housing stock growth and construction starts, controlling for demographic demand; cross-jurisdiction comparison where credit channels were restricted.',
    'If credit flow no longer predicts supply expansion, the coordination story is cover and the constraint shifts toward snare; if supply remains credit-constrained, the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_mobilization_vs_extraction, empirical, 'Whether the coordination function has atrophied leaving only extraction.').

omega_variable(
    homeownership_ideology_internalization,
    'Is household participation in leveraged homeownership driven by internalized ideological commitment or by structural absence of alternatives?',
    'Comparative exit-pattern analysis in jurisdictions with robust social housing or cooperative sectors; survey of renter-status self-concept where decommodified alternatives exist.',
    'If internalized, effective extraction is higher than structural measures suggest because households carry the constraint with them; if purely structural, extraction is bounded by policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeownership_ideology_internalization, conceptual, 'Structural versus internalized suppression mechanism in housing participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__financialization_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__financialization_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__financialization_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__financialization_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__financialization_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__financialization_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__financialization_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__financialization_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__financialization_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__financialization_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__financialization_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__financialization_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
