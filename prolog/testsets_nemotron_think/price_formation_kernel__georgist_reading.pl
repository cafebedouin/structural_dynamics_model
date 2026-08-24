% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__georgist_reading, []).

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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Georgist Reading of Price Formation: Land Rent Capture vs. Improvement Coordination
 *   domain: political_economy/housing/institutional_analysis
 *
 * SUMMARY:
 *   The georgist reading of price formation identifies a structural hybrid:
 *   land itself is a mountain (fixed supply, location scarcity is a physical
 *   fact), but the capture of land rent by private titleholders is a snare
 *   (pure extraction from labor and capital), while the improvement component
 *   is a rope (genuine coordination of production). The current price
 *   formation mechanism conflates these three, allowing the snare component
 *   to ride on the rope's legitimacy. This constraint story analyzes the
 *   standing arrangement — the actually existing system of private land rent
 *   capture — assessed from the georgist reading's lights. The claimed_type
 *   is tangled_rope because the system as a whole performs both coordination
 *   (improvements) and extraction (land rent) through the same price
 *   mechanism, requiring active enforcement (property law, zoning, mortgage
 *   credit) to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.72).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.68).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading of Price Formation: Land Rent Capture vs. Improvement Coordination").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, 'd0de80ea-1210-4382-89ea-9d2188f622b4').
narrative_ontology:cs_kernel_codification('d0de80ea-1210-4382-89ea-9d2188f622b4', formalized).
narrative_ontology:cs_authority_grounding('d0de80ea-1210-4382-89ea-9d2188f622b4', lineage).
narrative_ontology:cs_interpretation_layer_present('d0de80ea-1210-4382-89ea-9d2188f622b4').
narrative_ontology:cs_reading_relation('d0de80ea-1210-4382-89ea-9d2188f622b4', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d0de80ea-1210-4382-89ea-9d2188f622b4', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0de80ea-1210-4382-89ea-9d2188f622b4', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('d0de80ea-1210-4382-89ea-9d2188f622b4', foundational, land_value_is_unearned_increment).
narrative_ontology:cs_axiom_status(land_value_is_unearned_increment, holdable).
narrative_ontology:cs_axiom_grounding('d0de80ea-1210-4382-89ea-9d2188f622b4', land_value_is_unearned_increment, empirically_contingent).
narrative_ontology:cs_axiom('d0de80ea-1210-4382-89ea-9d2188f622b4', foundational, labor_deserves_full_product).
narrative_ontology:cs_axiom_status(labor_deserves_full_product, holdable).
narrative_ontology:cs_axiom_grounding('d0de80ea-1210-4382-89ea-9d2188f622b4', labor_deserves_full_product, deontological).
narrative_ontology:cs_axiom('d0de80ea-1210-4382-89ea-9d2188f622b4', secondary, single_tax_on_land_values_suffices).
narrative_ontology:cs_axiom_status(single_tax_on_land_values_suffices, holdable).
narrative_ontology:cs_axiom_grounding('d0de80ea-1210-4382-89ea-9d2188f622b4', single_tax_on_land_values_suffices, instrumental).
narrative_ontology:cs_reference_frame('d0de80ea-1210-4382-89ea-9d2188f622b4', classical_political_economy_land_rent_theory).
narrative_ontology:cs_drift_state('d0de80ea-1210-4382-89ea-9d2188f622b4', contemporary_financialized_housing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0de80ea-1210-4382-89ea-9d2188f622b4', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, rentier_capital).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, financial_institutions).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, producers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, first_time_buyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, producers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, urban_planners_developers).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_value_is_unearned_increment).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, labor_deserves_full_product).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, single_tax_on_land_values).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold title to land and capture location value created by community investment and population growth. Exercise political influence to maintain favorable tax treatment (property tax caps, Proposition 13-type measures, capital gains exemptions). Can sell or develop at will; exit means converting to financial assets, not leaving the system.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, landowners, agenda_setter).

% Deploy capital into land and rental housing portfolios, capturing the spread between financing costs and rental yields amplified by land appreciation. Benefit from leverage, tax shields (depreciation, interest deduction), and policy-backed asset inflation. Exit is liquid — can reallocate across asset classes and geographies.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, rentier_capital, beneficiary,
    institutional, biographical, arbitrage, global).

% Originate and securitize mortgages collateralized by land value, earning fees and interest on the capitalized rent stream. The land rent capture is the revenue base for the mortgage credit system. Regulatory frameworks (Basel risk weights, GSE guarantees) treat land-collateralized lending as low-risk, reinforcing the flow.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, financial_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Pay rent that capitalizes land value they had no hand in creating. Geographic mobility is limited by labor markets, social networks, and switching costs. No equity accumulation; rent increases absorb wage gains. Exit means displacement, not choice.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, immediate, constrained, local).

% Operate businesses or farms on leased or owned land. Pay ground rent (explicit or implicit via purchase price) that extracts from productive effort. Benefit from improvement coordination (infrastructure, agglomeration) but the land component is a pure cost. Exit means abandoning sunk capital or relocating operations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, producers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, producers, beneficiary).

% Wage earners whose housing cost share rises as land rent captures productivity gains. Real wages stagnate while location premiums grow. Cannot exit the land rent system — must live somewhere, and everywhere with jobs has captured rent. Political voice diluted by geographic dispersion.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor, payer,
    powerless, biographical, constrained, national).

% Face down-payment barriers inflated by capitalized land rent. Mortgage payments transfer future labor to past landowners via interest. Locked out of the asset class that would let them capture rent; forced to pay rent to those who already own. Intergenerational transfer masquerading as market price.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, first_time_buyers, payer,
    moderate, biographical, trapped, regional).

% Sets tax policy on land, improvements, income, and consumption. Could shift burden to land value (LVT) but faces concentrated opposition from beneficiaries. Captures some rent via property tax but at low rates on assessed land value. Institutional inertia and political economy lock in the current mix.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, government_tax_authority, agenda_setter,
    institutional, generational, analytical, national).

% Coordinate improvement production (buildings, infrastructure) — the rope function. Capture development profits but also pay land rent upfront. Their coordination role is genuine; their extraction is the land rent they pass through or capitalize. Zoning authority lets them restrict supply, amplifying land rent.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, urban_planners_developers, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, urban_planners_developers, beneficiary).

% Study the incidence, efficiency, and distributional effects of land rent capture. Provide the empirical and theoretical basis for alternative arrangements (LVT, community land trusts, public land banking). No material stake in the constraint's persistence.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, political_economy_analysts, observer,
    analytical, civilizational, analytical, universal).

% Inherit a world where land rent is capitalized into asset prices, raising entry costs for housing and production. No voice in current policy; their interests are represented only by proxy. Would object to the intergenerational transfer if they could.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and allocation of improvements (buildings, infrastructure, agglomeration economies) by providing price signals for where and what to build. The improvement component solves a genuine coordination problem: directing labor and capital to their most productive locations.
% TRANSFER_FUNCTION: Moves land rent — the unearned increment from location scarcity and community investment — from producers (tenants, businesses, labor) to landowners and their financial intermediaries. The price mechanism conflates improvement value (earned) with land rent (unearned), so every transaction transfers both.
% ABSENT_VOICES: Future generations who will inherit the capitalized rent burden; those completely priced out of productive regions and pushed to economic margins; renters in informal settlements without legal tenure. They are not in the room where zoning, tax, and monetary policy are made.
% DISAPPEARANCE_RATIONALE: If land rent capture vanished overnight (e.g., via full LVT), land prices would collapse to near zero, mortgage debt would be restructured, holding costs would shift from rent to tax, and location decisions would be driven purely by improvement productivity. The housing and credit systems would reorganize fundamentally.
% FOUNDING_PROBLEM: How to allocate scarce land efficiently while ensuring producers keep the full product of their labor and capital. The classical political economy problem: land is fixed, its value is social, but private appropriation of rent distorts production and distribution.
% FOUNDING_PROBLEM_CORROBORATION: Classical economists (Smith, Ricardo, Mill) identified land rent as unearned and distorting. Henry George proposed the remedy (single tax on land values) with mass political movements in late 19th/early 20th century. Modern LVT implementations (Pennsylvania split-rate, Estonia, Taiwan, Singapore land lease) corroborate the coordination function survives without rent capture. Neoclassical marginalism and financial interests contest the problem's relevance, claiming rent is efficient allocation.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__georgist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__georgist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the rent share of housing cost has risen structurally (land share of property value from ~30% to ~60% in major metros). Suppression (0.68) reflects the legal and financial architecture that prevents exit: property rights enforcement, zoning that restricts supply, mortgage credit that capitalizes rent into asset prices. Theater ratio (0.41) captures the performative 'market efficiency' framing that masks rent capture as price discovery. Accessibility collapse (0.58) — alternatives (LVT, community land trusts, public land banking) are known but politically suppressed. Resistance (0.55) — tenant movements, YIMBY/LVT coalitions, and some policy adoption show active but incomplete pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the landowner/rentier seat, the arrangement looks like rope (secure property rights enable investment) or even mountain (land ownership is a natural right). From the tenant/labor seat, it is snare (inescapable cost for permission to exist in productive space). The engine computes this divergence from the structural data — the georgist reading claims the extraction is the dominant structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners and rentier capital are structural beneficiaries (d near 0.0) — they collect the rent stream, control the political agenda, and have arbitrage-grade exit. Tenants, labor, and first-time buyers are structural targets (d near 1.0) — they pay the rent, have constrained or trapped exit, and bear the incidence. Producers and developers are dual-positioned: beneficiaries of improvement coordination, payers of land rent. Government is the agenda-setter with power to change the rule but captured by beneficiary coalitions. Financial institutions are beneficiaries with arbitrage exit. Analysts and future generations are observer/excluded seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient land allocation + labor's full product) was live in 1879. The georgist reading argues it is now dead — LVT solves it — but the arrangement persists because beneficiaries captured the state. Institutional_reading and financialization_reading treat the problem as live (we need market pricing to allocate land). This mismatch (founding_problem_status=dead vs disappearance_verdict=world_rearranges) flags mandatrophy: the arrangement survives its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_supply_fixed_vs_expandable,
    'Is land supply truly fixed (mountain) or can it be effectively expanded through density, technology, and transport?',
    'Empirical study of long-run land value gradients under different regulatory regimes; counterfactual simulation of floor-area-ratio liberalization.',
    'If land supply is elastically expandable, the mountain claim weakens and the snare component shrinks — rent becomes partly a policy choice (zoning) not pure nature. This shifts classification toward tangled_rope with larger policy-dependent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_supply_fixed_vs_expandable, empirical, 'Whether the mountain component (fixed land supply) holds under technological and regulatory change.').

omega_variable(
    rent_capture_as_allocation_mechanism,
    'Does private rent capture perform a genuine allocation function (directing land to highest-value use) that would be lost under LVT?',
    'Compare allocation efficiency (land use productivity, vacancy rates, misallocation metrics) in jurisdictions with high vs. low land value capture.',
    'If rent capture coordinates allocation, the snare claim weakens — extraction is the price of coordination. If LVT jurisdictions allocate equally well, rent capture is pure snare. This determines whether tangled_rope or snare is the true classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_capture_as_allocation_mechanism, conceptual, 'Whether the extraction component has an irreducible coordination function.').

omega_variable(
    kernel_reading_naturalist_foreclosure,
    'Does the georgist axiom ''land_value_is_unearned_increment'' logically foreclose the naturalist_reading''s core premise ''rent_is_factor_return'' within a single theoretical framework?',
    'Formal reconstruction of both readings'' axiomatic bases; test whether a single model can satisfy both without contradiction.',
    'If forecloses, the kernel has a genuine fault line — naturalist_reading cannot be held simultaneously with georgist_reading in one framework. If coexists_with, they are rival empirical claims about the same mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_naturalist_foreclosure, conceptual, 'Logical relationship between georgist and naturalist readings of the price formation kernel.').

omega_variable(
    financialization_amplification_mechanism,
    'Does financialization_reading describe a distinct constraint, or an amplification of the georgist rent capture mechanism?',
    'Decompose housing price appreciation into land rent growth vs. credit-driven multiple expansion; test whether financialization operates on the rent stream or creates independent demand.',
    'If amplification, financialization_reading is a downstream effect (influences relation). If distinct, it is a separate constraint in the family (coexists_with). Affects network.affects_constraints structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financialization_amplification_mechanism, empirical, 'Structural relationship between georgist rent capture and financialization dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_formation_georgist_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(price_formation_georgist_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(price_formation_georgist_tr_t60, price_formation_kernel__georgist_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(price_formation_georgist_tr_t90, price_formation_kernel__georgist_reading, theater_ratio, 90, 0.36).
narrative_ontology:measurement(price_formation_georgist_tr_t120, price_formation_kernel__georgist_reading, theater_ratio, 120, 0.39).
narrative_ontology:measurement(price_formation_georgist_tr_t150, price_formation_kernel__georgist_reading, theater_ratio, 150, 0.41).

% Extraction over time
narrative_ontology:measurement(price_formation_georgist_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(price_formation_georgist_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(price_formation_georgist_be_t60, price_formation_kernel__georgist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(price_formation_georgist_be_t90, price_formation_kernel__georgist_reading, base_extractiveness, 90, 0.64).
narrative_ontology:measurement(price_formation_georgist_be_t120, price_formation_kernel__georgist_reading, base_extractiveness, 120, 0.69).
narrative_ontology:measurement(price_formation_georgist_be_t150, price_formation_kernel__georgist_reading, base_extractiveness, 150, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(price_formation_georgist_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(price_formation_georgist_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(price_formation_georgist_su_t60, price_formation_kernel__georgist_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement(price_formation_georgist_su_t90, price_formation_kernel__georgist_reading, suppression_requirement, 90, 0.64).
narrative_ontology:measurement(price_formation_georgist_su_t120, price_formation_kernel__georgist_reading, suppression_requirement, 120, 0.66).
narrative_ontology:measurement(price_formation_georgist_su_t150, price_formation_kernel__georgist_reading, suppression_requirement, 150, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, land_value_tax_implementation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, zoning_regulation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, mortgage_credit_system).

% DUAL FORMULATION NOTE:
% Price formation kernel decomposes into four readings with distinct ε: naturalist (ε≈0.05, mountain), georgist (ε≈0.72, tangled_rope), institutional (ε≈0.55, tangled_rope), financialization (ε≈0.80, snare). The georgist reading identifies the land/improvement separation as the structural fault line; other readings emphasize different mechanisms. All four form a constraint family linked by shared referent (housing/land price formation) but different ε-invariant claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, organized, 0.15).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, institutional, 0.1).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, powerless, 0.95).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
