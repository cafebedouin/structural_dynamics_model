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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation via Credit Expansion and Asset Financialization
 *   domain: political_economy/housing_markets/finance
 *
 * SUMMARY:
 *   Housing price formation under the financialization reading instantiates a
 *   causal chain in which credit availability, asset-price feedback loops,
 *   and speculative demand drive prices upward independently of shelter
 *   scarcity or household income. The financial sector—lenders, securitizers,
 *   derivatives traders—benefits from the volume and volatility enabled by
 *   leverage; households bear debt-service burden, crash risk, and equity
 *   loss. The founding coordination problem (enabling credit-constrained
 *   families to access shelter) is solved; persistence of the mechanism
 *   extracts wealth from future-debt-service payers and crash-loss absorbers
 *   toward financial intermediaries and existing asset holders. This reading
 *   coexists alongside three sibling readings (naturalist: price reflects
 *   preference+scarcity; georgist: price separates unearned land rent from
 *   earned improvement value; institutional: price is constructed by zoning,
 *   tax policy, and intermediary platforms). Each reading instantiates a
 *   different constraint; this story describes the financialization reading's
 *   specific ε-invariant structure.
 *
 * KEY AGENTS:
 *   - mortgage_lenders: Institutional agenda-setter (powerful, arbitrage exit) — sets lending standards that expand credit
 *   - financial_intermediaries: Institutional beneficiary (powerful, arbitrage exit) — extract fees and spreads throughout credit cycle
 *   - property_speculators: Powerful beneficiary (mobile exit) — profit from leverage-amplified capital gains
 *   - existing_asset_holders: Organized beneficiary (mobile exit) — benefit passively from wealth effect
 *   - first_time_homebuyers: Moderate payer (constrained exit) — must borrow at lender-set rates to enter market
 *   - renters_priced_out: Powerless victim (trapped) — excluded from asset ownership, no equity accumulation
 *   - debt_service_constrained_households: Moderate victim (identity_locked) — trapped by mortgage commitment and location identity
 *   - crash_loss_absorbers: Powerless victim (trapped) — absorb losses when credit cycles reverse
 *   - central_banks_and_regulators: Institutional agenda-setter + observer (analytical exit) — set rates and standards but face political pressure and internal conflicts
 *   - policy_advocates_for_restraint: Moderate excluded (constrained exit) — argue for tighter standards but marginalized in policy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation via Credit Expansion and Asset Financialization").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/finance").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'c699b5d5-82bb-4175-a19e-1ef34ad1523e').
narrative_ontology:cs_kernel_codification('c699b5d5-82bb-4175-a19e-1ef34ad1523e', distributed).
narrative_ontology:cs_authority_grounding('c699b5d5-82bb-4175-a19e-1ef34ad1523e', extraction).
narrative_ontology:cs_interpretation_layer_present('c699b5d5-82bb-4175-a19e-1ef34ad1523e').
narrative_ontology:cs_reading_relation('c699b5d5-82bb-4175-a19e-1ef34ad1523e', price_formation_kernel__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('c699b5d5-82bb-4175-a19e-1ef34ad1523e', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c699b5d5-82bb-4175-a19e-1ef34ad1523e', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_axiom('c699b5d5-82bb-4175-a19e-1ef34ad1523e', foundational, credit_expansion_primary_price_driver).
narrative_ontology:cs_axiom_status(credit_expansion_primary_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('c699b5d5-82bb-4175-a19e-1ef34ad1523e', credit_expansion_primary_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('c699b5d5-82bb-4175-a19e-1ef34ad1523e', foundational, leverage_feedback_loop_amplification).
narrative_ontology:cs_axiom_status(leverage_feedback_loop_amplification, holdable).
narrative_ontology:cs_axiom_grounding('c699b5d5-82bb-4175-a19e-1ef34ad1523e', leverage_feedback_loop_amplification, empirically_contingent).
narrative_ontology:cs_axiom('c699b5d5-82bb-4175-a19e-1ef34ad1523e', secondary, financial_extraction_from_debt_service).
narrative_ontology:cs_axiom_status(financial_extraction_from_debt_service, holdable).
narrative_ontology:cs_axiom_grounding('c699b5d5-82bb-4175-a19e-1ef34ad1523e', financial_extraction_from_debt_service, deontological).
narrative_ontology:cs_reference_frame('c699b5d5-82bb-4175-a19e-1ef34ad1523e', price_formation_from_earned_shelter_scarcity).
narrative_ontology:cs_drift_state('c699b5d5-82bb-4175-a19e-1ef34ad1523e', contemporary_post_2008_credit_expansion, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c699b5d5-82bb-4175-a19e-1ef34ad1523e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, property_speculators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, existing_asset_holders).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters_priced_out).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, debt_service_constrained_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, crash_loss_absorbers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).

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
 *   Extractiveness is high (0.78 at interval end) and rising over the measurement series because: (1) each credit expansion cycle transfers wealth from debt-service payers to financial intermediaries and speculators; (2) the mechanism persists despite crashes because institutional incentives reward origination volume over risk management; (3) price decoupling from income/shelter-value is the signature that coordination has given way to extraction. Suppression is elevated (0.71) because: (1) tighter lending standards or rate increases face political resistance from beneficiaries; (2) policy advocates for restraint are structurally excluded from agenda-setting; (3) the beneficiary coalition (lenders + speculators + existing holders) is powerful enough to prevent regulation that would moderate extraction. Theater ratio rises from 0.18 to 0.38 because: (1) the coordination rhetoric ('expanding access', 'serving underserved borrowers') persists as the official justification even as credit expansion becomes pure asset-inflation; (2) post-crash periods see regulatory theater (stricter standards announced, then quietly loosened) without structural change; (3) lenders present debt-driven price growth as 'market fundamentals' and 'rational expectations' rather than their own extraction mechanism. Accessibility collapse is moderate (0.62) because alternatives exist in principle (public housing, cooperative ownership, rental markets) but are suppressed by policy (mortgage-interest deductions, capital-gains preferences, zoning) that sustains the financialized model. Resistance is moderate-low (0.59) because: (1) existing asset holders acquiesce in price growth (wealth effect); (2) first-time buyers are individually powerless to resist lender rates; (3) collective resistance emerges only after crashes, when the constraint's persistence becomes undeniable, and is then absorbed by regulatory theater.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (lenders, speculators, existing holders) experience this as efficient price discovery and appropriate risk pricing—they see rising rates as market-driven compensation for leverage risk, not extraction. The payer seats experience the same structure as coercive: they cannot exit without losing housing or bearing massive transaction costs. Regulators are internally divided: their forecasting models acknowledge price-inflation risk from low rates, but their political constraints and pre-crash institutional incentives (Fed mandate for employment/growth) override tighter policy. The excluded voices (macroprudential advocates, housing researchers) see the constraint as extractive and unsustainable; the beneficiary seats dismiss them as ideological. The engine computes directionality from power + exit + beneficiary/victim facts: beneficiaries (institutional power, arbitrage exit) get low d; victims (powerless/moderate power, constrained/trapped/identity_locked exit) get high d. This per-seat computation reveals why the constraint persists despite periodic crashes: the beneficiary seats have political and economic power to reconstruct the mechanism after each crisis.
 *
 * DIRECTIONALITY LOGIC:
 *   Mortgage_lenders are full beneficiaries (d near 0.0): they set the rules (agenda_setter role), have institutional power and arbitrage exit, and collect origination fees + servicing spreads without bearing default risk. Financial_intermediaries are beneficiaries (d ~0.1): they profit from every transaction, securitization, and derivative without direct asset exposure. Property_speculators are beneficiaries (d ~0.15): they deploy leverage to amplify gains and can exit at peak; their time horizon and mobile exit mean they bear minimal long-term cycle risk. Existing_asset_holders are beneficiaries (d ~0.2): they benefit passively from wealth effect and have options to downsize/relocate if needed. First_time_homebuyers are targets (d ~0.75): they must borrow at lender-determined rates, carry the full mortgage duration, and absorb downside risk. Renters_priced_out are targets (d ~0.85): they are excluded from the asset-appreciation mechanism entirely and have trapped exit (geographic immobility). Debt_service_constrained_households are targets (d ~0.8): they are identity_locked (homeowner self-concept, community ties) and cannot exit the debt commitment without accepting massive losses. Crash_loss_absorbers are targets (d ~0.9): they absorb asymmetric downside when credit cycles reverse. Central_banks_and_regulators are mixed (d ~0.5): they set policy (agenda_setter role) but face political constraints that prevent tighter enforcement; they are both administrators and partially captured by beneficiary-seat pressure. Policy_advocates_for_restraint are observers (d ~0.5, analytical): they lack institutional power to set the constraint directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead (founding_problem_status = dead) but the constraint persists with high extractiveness. This indicates mandatrophy: the mechanism was justified as solving credit access for constrained households, but modern data show: (1) credit is abundant for qualified borrowers in most markets; (2) nominal prices have decoupled from real shelter scarcity (supply is less tight than price growth implies); (3) debt-service burdens have risen relative to income, compressing household consumption and savings. The classification as Tangled Rope (not pure Snare) is correct because genuine coordination functions remain at the mechanism's core: payment smoothing across time, collateral-based credit access, and market liquidity for property transactions. However, the coordination function is solved by the baseline lending mechanism (conforming mortgages, stable rates); extraction rides on top via credit expansion beyond that baseline. The mandatrophy dynamic explains policy persistence despite crashes: the beneficiary coalition (lenders, speculators, existing holders) is sufficiently powerful to reconstruct loose-lending conditions after every crash before the foundational problem could be re-examined. The theater_ratio rise (0.18 → 0.38) documents this: regulatory cycles oscillate between post-crash tightening (performative crackdown on standards) and pre-cycle loosening (return to 'normal' practices) without addressing the structural extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_causality_vs_correlation,
    'Is credit expansion the primary causal driver of housing price growth, or is credit expansion an endogenous response to rising demand (causality reversed)?',
    'Natural experiments from credit-supply shocks (changes in banking regulation, mortgage securitization availability, central bank policy) that affected lending without corresponding demand shifts. Instrumental-variables econometrics isolating credit-supply changes from demand-driven credit growth. International cross-country comparisons of credit cycles vs. price cycles.',
    'If credit causality is established (expansions cause price rises more than demand does), the financialization reading''s extraction mechanism is confirmed. If credit is primarily demand-driven (lenders respond to rising demand), the constraint becomes more institutional/zoning-driven (scarcity drives demand, which drives credit). Strong credit causality supports regulatory interventions (tighter standards, higher rates); weak causality favors supply-side solutions (zoning reform, construction subsidy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_causality_vs_correlation, empirical, 'Whether credit expansion is the primary causal driver or an endogenous response to demand.').

omega_variable(
    feedback_loop_reversibility,
    'Once credit-driven prices inflate asset values and collateral bases, is the feedback loop reversible without major losses? Can policy tighten without triggering cascade defaults?',
    'Stress-test modeling of rate increases and lending-standard tightening under current leverage conditions. Historical analysis of de-leveraging episodes (deleveraging from 2008-2012, comparison with other post-bubble periods). Household debt-service-to-income ratio tracking and household-balance-sheet fragility assessment.',
    'If feedback loops are highly reversible (households and lenders can adjust smoothly to tighter credit), the constraint is classified as more rope-like (coordination with temporary extraction). If reversal is catastrophic (tightening triggers cascading defaults, systemic risk), the constraint is more snare-like (extraction sustained by systemic risk to the financial system if unwound). High irreversibility supports gradual policy adjustment (very slow rate increases, smooth glide path) and may indicate the constraint is locked in by systemic-risk concerns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_reversibility, empirical, 'Whether the credit-price feedback loop can be reversed without triggering cascading defaults.').

omega_variable(
    beneficiary_capture_of_regulatory_authority,
    'Is regulatory moderation of credit expansion prevented by direct capture (lenders lobby, fund candidates, staff regulators) or by ideological capture (regulators believe low rates and credit expansion are necessary for growth/employment)?',
    'Revolving-door analysis: track career paths of regulatory staff before/after government service, employment at lenders. Campaign finance data: lending-sector donations to politicians overseeing lending policy. Rhetorical analysis: statements by Federal Reserve officials and banking regulators in public testimony and internal memos (Freedom of Information Act requests) about credit expansion and systemic risk.',
    'If direct capture dominates, policy solutions focus on structural separation (independent agencies, term limits, conflicts-of-interest enforcement). If ideological capture dominates (regulators truly believe loose credit is necessary), solutions require changing the macroeconomic consensus (different employment targets, different inflation tolerance, different growth assumptions). Mixed capture suggests both institutional and epistemic reform is needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_regulatory_authority, empirical, 'Whether regulatory constraint on credit is blocked by direct lobbying-capture or ideological consensus.').

omega_variable(
    housing_supply_elasticity_and_financialization_interaction,
    'If housing supply were radically more elastic (permissive zoning, fast construction, low transaction costs), would credit expansion still drive prices upward, or would supply accommodation dissipate price pressure?',
    'Natural experiments from zoning reform (Minneapolis 2021, Korea building deregulation, Japan supply elasticity). Comparative housing markets with very different supply elasticity but similar credit cycles (Australia vs. Canada). Agent-based modeling of credit dynamics under different supply regimes.',
    'If prices still rise sharply under elastic supply, credit expansion is the dominant mechanism (financialization reading strongly supported). If elastic supply dissipates price growth despite credit expansion, scarcity/institutional constraints are more important (georgist and institutional readings gain support). Moderate elasticity sensitivity suggests the readings are interactive: credit expansion matters more when supply is constrained, less when supply is elastic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_supply_elasticity_and_financialization_interaction, empirical, 'Whether credit-driven price growth persists under elastic housing supply.').

omega_variable(
    wealth_effect_vs_extraction_distribution,
    'Does the wealth effect from asset-price appreciation (existing holders spend more, consume more, save less) exceed the extraction burden on debt-service constrained households, so the economy gains net demand/growth? Or is extraction redistribution net-negative for growth?',
    'Household consumption surveys (marginal propensity to consume out of wealth gains vs. debt-service constraints). Decomposition of GDP growth attributable to construction/real-estate services vs. household balance-sheet damage. Debt-service burden tracking across income deciles. Foreclosure and default cascades'' impact on regional GDP.',
    'If wealth effect dominates, the constraint may be presented (and partially justified) as growth-enhancing despite extraction. If extraction burden dominates, the constraint is a net-negative growth drag (wealth redistribution reduces aggregate consumption). This affects whether the constraint is classified as justified coordination or pure extraction in political economy discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_effect_vs_extraction_distribution, empirical, 'Whether wealth-effect gains exceed extraction burden for aggregate growth.').

omega_variable(
    reading_contention_boundaries,
    'Which of the four readings (naturalist, georgist, institutional, financialization) can coexist in a single unified framework, and which readings truly foreclose each other?',
    'Philosophical analysis of axiomatic commitment: can a single framework hold both ''prices reflect preference+scarcity'' (naturalist) AND ''credit availability is the primary price driver'' (financialization)? Can it hold both ''land rent is the unearned component'' (georgist) AND ''credit drives all prices'' (financialization)? Empirical decomposition of price variance: how much is explained by scarcity, by land-rent dynamics, by institutional rules, by credit cycles, by speculation? If all four contribute, they coexist; if one dominates, others are foreclosed.',
    'If all four readings coexist as complementary mechanisms, the constraint family describes overlapping but separable causal processes. If some readings foreclose others, the kernel itself is contested at the foundational level and the readings should reflect that via forecloses relations in cs_structure. The contention boundary determines whether policy solutions are additive (regulate each mechanism separately) or substitutive (solve one mechanism and others fade).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contention_boundaries, conceptual, 'Whether the four readings of price formation coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfkfr_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pfkfr_tr_t5, price_formation_kernel__financialization_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(pfkfr_tr_t10, price_formation_kernel__financialization_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(pfkfr_tr_t15, price_formation_kernel__financialization_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(pfkfr_tr_t20, price_formation_kernel__financialization_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(pfkfr_tr_t25, price_formation_kernel__financialization_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(pfkfr_tr_t30, price_formation_kernel__financialization_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(pfkfr_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pfkfr_be_t5, price_formation_kernel__financialization_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(pfkfr_be_t10, price_formation_kernel__financialization_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(pfkfr_be_t15, price_formation_kernel__financialization_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(pfkfr_be_t20, price_formation_kernel__financialization_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(pfkfr_be_t25, price_formation_kernel__financialization_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(pfkfr_be_t30, price_formation_kernel__financialization_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pfkfr_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(pfkfr_su_t5, price_formation_kernel__financialization_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(pfkfr_su_t10, price_formation_kernel__financialization_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(pfkfr_su_t15, price_formation_kernel__financialization_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(pfkfr_su_t20, price_formation_kernel__financialization_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(pfkfr_su_t25, price_formation_kernel__financialization_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(pfkfr_su_t30, price_formation_kernel__financialization_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).

% DUAL FORMULATION NOTE:
% The price_formation_kernel is instantiated by four structurally distinct readings: this constraint (financialization_reading) posits credit expansion and asset-price feedback loops as the primary price driver; the naturalist_reading models price as market-clearing equilibrium reflecting preference + scarcity; the georgist_reading separates unearned land rent from earned improvement value; the institutional_reading attributes price to zoning, tax policy, and intermediary platforms. Each reading instantiates a different ε value and different beneficiary/victim structure. The financialization reading documents the case where credit mechanisms are the dominant driver and derive high extraction through debt-service burden on households + gains to financial intermediaries. All four readings are linked via network.affects_constraints as a family; they are alternative framings of a contested kernel, not measurements of the same constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
