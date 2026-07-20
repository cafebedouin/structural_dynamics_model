% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Financialized Housing Price Formation
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   This constraint story instantiates the financialization reading of the
 *   contested price_formation_kernel. It treats housing price formation as
 *   driven by credit expansion, asset-price feedback loops, and demand from
 *   actors treating housing as a financial asset rather than shelter. The
 *   constraint is a tangled rope: it coordinates capital allocation to
 *   housing (genuine coordination function) while asymmetrically extracting
 *   from household debtors and concentrating gains in the financial sector
 *   and property investors. The kernel has four competing readings:
 *   naturalist (equilibrium/scarcity), institutional (zoning/lending
 *   standards/tax), georgist (land rent/improvement), and this
 *   financialization reading. Each reading instantiates a structurally
 *   distinct constraint per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - financial_sector (institutional/arbitrage): Primary agenda-setter and beneficiary; designs credit instruments and captures regulatory frameworks.
 *   - property_investors (powerful/mobile): Beneficiary of asset-price feedback loops; treats housing as portfolio asset.
 *   - household_debtors (moderate/constrained): Primary payer; bear debt service, negative equity risk, and income capture.
 *   - monetary_authorities (institutional/constrained): Agenda-setter controlling credit conditions; operate within financialized policy paradigms.
 *   - shelter_seeking_households (powerless/trapped): Excluded from ownership and price-setting forums; pushed to rental margins.
 *   - housing_policy_analysts (analytical/analytical): Observer seat documenting decoupling of price from shelter cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.65).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Financialized Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '1778f2ad-6ff2-427c-a3f2-4678a6f80b31').
narrative_ontology:cs_kernel_codification('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', distributed).
narrative_ontology:cs_authority_grounding('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', distributed).
narrative_ontology:cs_reading_relation('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', foundational, housing_as_asset_takes_precedence).
narrative_ontology:cs_axiom_status(housing_as_asset_takes_precedence, holdable).
narrative_ontology:cs_axiom_grounding('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', housing_as_asset_takes_precedence, empirically_contingent).
narrative_ontology:cs_axiom('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', foundational, credit_creation_is_dominant_price_driver).
narrative_ontology:cs_axiom_status(credit_creation_is_dominant_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', credit_creation_is_dominant_price_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', endogenous_credit_money_system).
narrative_ontology:cs_drift_state('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', post_global_financial_crisis, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1778f2ad-6ff2-427c-a3f2-4678a6f80b31', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, property_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, household_debtors).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, minskyan_instability).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, endogenous_money_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs mortgage origination, securitization structures, and credit scoring systems. Profits from origination fees, interest spreads, and transaction volume. Lobbies for favorable lending standards and tax treatment of mortgage debt. Can rotate capital globally if returns in housing finance compress.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, financial_sector, beneficiary).

% Treats housing primarily as a financial asset for capital appreciation and rental yield. Demand is leveraged and sensitive to credit availability, tax advantages, and expected price growth. Benefits directly from asset-price feedback loops that inflate nominal values beyond shelter-use value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, property_investors, beneficiary,
    powerful, biographical, mobile, national).

% Access housing primarily through long-term mortgage leverage. Bear monthly debt service burdens, exposure to negative equity, and bankruptcy risk during price corrections. Income flows are captured by debt servicing; any nominal wealth gains are illiquid and often recycled into larger mortgages upon trade-up.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, household_debtors, payer,
    moderate, biographical, constrained, national).

% Set base interest rates and bank capital requirements, governing the macro conditions for credit expansion. Policy frameworks prioritize price stability and financial sector liquidity over housing affordability. Operate under political and statutory constraints that are themselves shaped by financial sector influence.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, monetary_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Require shelter but lack capital or credit access to participate in homeownership. Structurally excluded from price-setting mechanisms, mortgage origination forums, and housing policy debates. Pushed into rental markets or informal housing arrangements. Would advocate for non-market tenure if included.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, shelter_seeking_households, excluded,
    powerless, biographical, trapped, regional).

% Document and model the decoupling of housing prices from construction costs and local incomes. Produce counter-narratives to the owner-occupation wealth-creation discourse. Lack institutional power to alter lending standards or credit allocation but provide empirical accounts of distributional harm.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed capital into mortgage credit and channels it toward housing acquisition, enabling large-scale homeownership ahead of full income accumulation.
% TRANSFER_FUNCTION: Moves debt service payments and origination fees from households to the financial sector, and moves capital gains from future household buyers and renters to current property investors.
% ABSENT_VOICES: Public housing advocates, cooperative tenure organizers, and decommodification movements are structurally excluded from price-setting and credit-allocation forums; their absence ensures the debate stays between market-provision variants.
% DISAPPEARANCE_RATIONALE: If the credit-expansion feedback loop vanished, housing prices would recouple to local incomes and construction costs, the financial sector's mortgage-dependent revenue model would collapse, and household balance sheets would shift from leveraged speculation to income-based shelter budgeting.
% FOUNDING_PROBLEM: Mobilizing sufficient capital for housing construction and enabling households to access shelter before accumulating full purchase-price savings.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians attest to the original coordination problem of illiquid mortgage markets. Heterodox economists and housing advocates outside the benefiting parties attest that the problem has been solved by excess credit and now operates as an extraction mechanism; central bank working papers occasionally acknowledge the shift but policy frameworks remain aligned with the benefiting parties.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because price levels have decoupled from construction costs and local incomes, tracking credit availability instead. Suppression (0.65) reflects the active exclusion of non-market tenure forms and the discursive suppression of decommodification alternatives. Theater_ratio (0.45) captures the performative rhetoric of homeownership-as-wealth-creation that obscures the debt-service extraction engine. Accessibility_collapse (0.60) indicates that alternatives (public housing, cooperative finance, unmediated savings) have been systematically dismantled or marginalized. Resistance (0.40) is moderate: tenant movements and occasional regulatory pushback exist but are overwhelmed by financial sector political power.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (financial sector, monetary authorities) experience the constraint as necessary market coordination enabling capital mobility and homeownership. The payer seat (household debtors) experiences the same structure as extraction of future income through debt service. The beneficiary seat (property investors) experiences capital appreciation as natural market return. The engine computes this divergence from structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector and property investors are declared beneficiaries (low d, subsidized by the constraint's operation). Household debtors are declared victims (high d, targeted for extraction). Monetary authorities occupy an agenda-setter position with constrained exit; their directionality is structurally closer to the financial sector due to institutional capture but they do not directly capture gains, so no override is needed. Shelter-seeking households are excluded entirely, sitting outside the constraint's formal structure but bearing its externalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both genuine coordination (capital pooling for housing access) and asymmetric extraction (debt service flows to financial sector, crash risk to households). A pure rope would lack the victim structure and the decoupling of price from shelter value. A pure snare would lack the genuine coordination function that mortgage markets originally provided. The tangled rope classification captures the hybrid: the coordination mechanism has been captured and amplified into an extraction engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_necessity_vs_extraction,
    'Is leveraged mortgage credit expansion structurally necessary for housing provision, or has it become a gatekeeping mechanism extracting rent from a necessity?',
    'Cross-country comparison of housing credit-to-GDP ratios against shelter outcome metrics (price-to-income, homelessness rates, construction volumes).',
    'If high credit is necessary for adequate supply, extraction is partly coordination cost; if supply outcomes are worse in high-credit regimes, the constraint is dominantly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_necessity_vs_extraction, empirical, 'Whether mortgage credit serves coordination or extraction').

omega_variable(
    kernel_reading_independence,
    'Do the four readings of the price formation kernel represent distinct constraints with different epsilon values, or are they observer-relative framings of a single mechanism?',
    'Examine whether each reading produces different empirical predictions and policy prescriptions that cannot be reduced to one another.',
    'If distinct, the decomposition into four stories is warranted; if observer-relative, they should collapse into one constraint with high variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_independence, conceptual, 'Whether sibling readings are distinct constraints or framings').

omega_variable(
    financial_sector_capture,
    'To what extent do monetary authorities and regulatory agencies act as independent coordinators versus captured extensions of financial sector interests?',
    'Regulatory capture analysis comparing policy outcomes to financial sector lobbying intensity and revolving-door patterns.',
    'If capture is high, the coordination story is cover and the constraint trends toward snare; if independence is high, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_sector_capture, empirical, 'Regulatory capture degree in housing finance policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfk_fin_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pfk_fin_tr_t8, price_formation_kernel__financialization_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(pfk_fin_tr_t16, price_formation_kernel__financialization_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(pfk_fin_tr_t24, price_formation_kernel__financialization_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(pfk_fin_tr_t32, price_formation_kernel__financialization_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(pfk_fin_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(pfk_fin_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pfk_fin_be_t8, price_formation_kernel__financialization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(pfk_fin_be_t16, price_formation_kernel__financialization_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(pfk_fin_be_t24, price_formation_kernel__financialization_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(pfk_fin_be_t32, price_formation_kernel__financialization_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(pfk_fin_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pfk_fin_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pfk_fin_su_t8, price_formation_kernel__financialization_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(pfk_fin_su_t16, price_formation_kernel__financialization_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(pfk_fin_su_t24, price_formation_kernel__financialization_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(pfk_fin_su_t32, price_formation_kernel__financialization_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(pfk_fin_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.15).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, georgist_reading).

% DUAL FORMULATION NOTE:
% The price_formation_kernel decomposes into four structurally distinct constraints per the epsilon-invariance principle. The naturalist reading posits negligible extraction (mountain/rope candidate); the financialization reading posits high extraction (tangled_rope); the institutional and georgist readings occupy intermediate positions with different victim/beneficiary structures. They form a constraint family linked by shared domain but separated by divergent epsilon values and causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
