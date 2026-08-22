% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Georgist Reading: Land Rent Separation from Improvement Value
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   The Georgist reading of price formation claims that housing and land
 *   prices reflect two structurally distinct components: (1) location-based
 *   rent (unearned increment from location scarcity and public
 *   infrastructure, which should flow to public revenue) and (2) improvement
 *   value (earned return to labor and capital invested in construction and
 *   maintenance, which rightfully accrues to the builder/owner). The reading
 *   presents the bundled price (treating both components as private property
 *   value) as an extractive constraint that transfers public location wealth
 *   to private landowners while depressing wages and returns to labor. The
 *   key claim is not merely that landowners benefit—it is that they benefit
 *   WITHOUT producing the location value, and this asymmetry is sustained by
 *   property law that prevents the separation.
 *
 * KEY AGENTS:
 *   - incumbent_landowners: the structural beneficiary (extract unearned location rent via bundled prices and inherited property rights)
 *   - labor_and_capital_producers: the primary victims (wages and returns to investment suppressed by rent burden)
 *   - tenant_class: the deepest victims (trapped in location, pay full rent with zero ownership stake)
 *   - public_revenue_authority: the excluded party (should rightfully claim location rent under Georgist theory, but is prevented by property law)
 *   - housing_market_analyst: the analytical seat (observes the separation and measures the extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.72).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading: Land Rent Separation from Improvement Value").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '7c66f9b8-9cd8-4c91-a719-11245cb8e085').
narrative_ontology:cs_kernel_codification('7c66f9b8-9cd8-4c91-a719-11245cb8e085', formalized).
narrative_ontology:cs_authority_grounding('7c66f9b8-9cd8-4c91-a719-11245cb8e085', extraction).
narrative_ontology:cs_interpretation_layer_present('7c66f9b8-9cd8-4c91-a719-11245cb8e085').
narrative_ontology:cs_reading_relation('7c66f9b8-9cd8-4c91-a719-11245cb8e085', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7c66f9b8-9cd8-4c91-a719-11245cb8e085', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('7c66f9b8-9cd8-4c91-a719-11245cb8e085', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('7c66f9b8-9cd8-4c91-a719-11245cb8e085', foundational, land_value_separable_from_improvement).
narrative_ontology:cs_axiom_status(land_value_separable_from_improvement, holdable).
narrative_ontology:cs_axiom_grounding('7c66f9b8-9cd8-4c91-a719-11245cb8e085', land_value_separable_from_improvement, empirically_contingent).
narrative_ontology:cs_axiom('7c66f9b8-9cd8-4c91-a719-11245cb8e085', foundational, location_rent_unearned_natural_scarcity).
narrative_ontology:cs_axiom_status(location_rent_unearned_natural_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('7c66f9b8-9cd8-4c91-a719-11245cb8e085', location_rent_unearned_natural_scarcity, deontological).
narrative_ontology:cs_reference_frame('7c66f9b8-9cd8-4c91-a719-11245cb8e085', property_rights_framework_natural_scarcity).
narrative_ontology:cs_drift_state('7c66f9b8-9cd8-4c91-a719-11245cb8e085', contemporary_financialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c66f9b8-9cd8-4c91-a719-11245cb8e085', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, incumbent_landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, rentier_class).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_and_capital_producers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, aspiring_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenant_class).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_supply_fixed_by_nature).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, rent_is_unearned_increment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold land titles and extract location-based rent appreciation without producing improvements. Benefit from the constraint's distinction between land value (which they capture as unearned increment) and improvement value (which they externalize to others' labor). Control the framing of price formation through property rights law, zoning advocacy, and real estate markets.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, incumbent_landowners, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, incumbent_landowners, agenda_setter).

% Wages and investment returns are depressed by the requirement to purchase or rent land at prices that bundle location rent with improvement value. Workers and businesses cannot separate out what they earn (improvement value from their labor/capital) from what they pay for location access. Their exit is constrained by the location's economic necessity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_and_capital_producers, payer,
    organized, biographical, constrained, national).

% Must purchase bundled price (land rent + improvements) to gain shelter and property stake. Cannot disaggregate their down payment: they pay for location rent they do not produce, and for improvements they may or may not desire. If they relocate, they abandon location-specific accumulated equity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, aspiring_homeowners, payer,
    moderate, biographical, constrained, local).

% Pay rent that includes the full unearned location increment, which landlords have not produced. Rent is set by location scarcity and landlord property rights, not by the landlord's service to the tenant. Exit is trapped: leaving the location means losing all access to the location's economic opportunities and social infrastructure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenant_class, payer,
    powerless, immediate, trapped, local).

% Under Georgist analysis, should rightfully capture location rent (the 'single tax') to fund public goods. Is excluded from this revenue by the property rights framing and by political resistance from landowners. Could capture the unearned increment but is structurally prevented by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, public_revenue_authority, excluded,
    institutional, generational, trapped, national).

% Observes price formation and decomposes it into location rent and improvement value. From the Georgist reading's vantage, can measure the unearned increment captured by landowners and the extraction it represents. Measures the constraint's operation and documents the asymmetry.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, housing_market_analyst, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, incumbent_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent property rights framework for exchanging land and improvements, enabling investment in housing stock and urban infrastructure. Coordinates expectations about what can be owned, transferred, and inherited in relation to location.
% TRANSFER_FUNCTION: Moves location-based economic rent from potential public revenue (location value created by geography and public infrastructure investment) to incumbent private landowners. Also moves a portion of labor and capital returns to landowners via the bundled price mechanism.
% ABSENT_VOICES: Future generations and non-propertied classes whose housing security and wealth-building are constrained by the rent extraction. Public finance authorities who under Georgist theory should capture location rent for public purposes. Tenants with no ownership stake and no ability to benefit from appreciation.
% DISAPPEARANCE_RATIONALE: If the Georgist separation were enforced (unearned rent decoupled from improvement value and captured as public revenue), land prices would collapse to improvement-only value, housing would become dramatically cheaper, and the rentier class's wealth would face radical revaluation. The entire property system, tax structure, and wealth distribution would reorganize.
% FOUNDING_PROBLEM: Need to establish clear property rights and pricing signals for land and housing investment: coordinate expectations about what can be owned, what improvements generate returns, and how to value location for economic purposes.
% FOUNDING_PROBLEM_CORROBORATION: The Georgist reading asserts the founding problem is solved (property rights are established), and the persistent constraint now primarily serves to extract rent rather than coordinate. Naturalist and institutional readings assert the problem remains live (efficient price discovery requires the bundling). Competition for land and tenant testimony support the Georgist reading; neoclassical economics and real estate industry attestation support the competing readings.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the measured wealth transfer from labor/capital producers and tenants to landowners via bundled pricing. The Georgist reading asserts that location rent (~40-50% of housing prices in high-demand areas, per contemporary estimates) is unearned, making this transfer structurally asymmetric extraction. Suppression (0.72) reflects both structural (legal enforcement of property rights, zoning constraints on alternative arrangements) and internalized (belief in property legitimacy) barriers to alternatives. Theater ratio (0.48) indicates the constraint has genuine coordination content (property rights enable investment, markets function) but increasingly operates as performance maintenance—the coordination need is satisfied at lower bundled prices, and the excess is pure rent extraction. Accessibility collapse (0.71) is moderate-high: alternatives (public land banking, land-value taxation, commons arrangements) are conceptually available but politically and legally suppressed. Resistance (0.59) reflects the tenant and tenant-movement organizing against the constraint, balanced against landowner and institutional inertia. The measurement series track 1890–2026, showing extractiveness rising as urbanization increases location scarcity and as financialization adds leverage to the rent-capture mechanism. Theater ratio rises as the constraint increasingly performs legitimacy narratives (housing supply, market efficiency) while operating as pure rent extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the landowner seat, the constraint is rope: it coordinates property rights, enables investment in housing, and provides price signals for efficient land use. From the labor/capital seat, it is snare: the same mechanism transfers their earnings to a class that produced nothing. From the public-finance seat, it is pure extraction: location rent should flow as public revenue. The engine computes these divergences from the structural data—incumbent_landowners hold agenda_setter role with powerful/arbitrage position; labor_and_capital_producers hold payer role with constrained exit; the bundled-price mechanism benefits the former while suppressing exit for the latter. The Georgist reading's contribution is not inventing the divergence but NAMING the structural asymmetry: landowners extract an unearned component that others produced (public location value) or did not produce at all (location scarcity).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent landowners: d ≈ 0.1–0.2 (full beneficiary seat, benefits from constraint without bearing its costs, high arbitrage exit). Labor/capital producers: d ≈ 0.75–0.85 (target seat, constrained exit, wages suppressed by rent burden, organized enough to resist but institutional power imbalance is severe). Tenant class: d ≈ 0.9+ (deepest target, trapped exit, immediate time horizon, powerless, bear full cost of rent with no wealth accumulation path). Public revenue authority: excluded (would have d ≈ 1.0 if included, since Georgist theory says location rent should flow to public). The directionality gradient is extreme: the constraint's beneficiary seat has near-zero d (pure subsidy), while the deepest victim seat has near-complete d (pure extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing property rights and price signals for land and housing) was solved by ~1920 in most developed nations. By 2000, the constraint's primary function (coordination of investment, price discovery) was satisfied, but the extraction mechanism persisted and expanded. The theater ratio rises from 0.25 (1890) to 0.48 (2026), indicating the constraint increasingly performs legitimacy narratives—'market efficiency', 'property rights as natural law', 'housing shortage'—while operating as rent extraction. The Georgist reading detects mandatrophy: the constraint persists not because coordination requires the bundled-price mechanism but because landowners capture the political system and enforce the bundling despite its coordination function being separable. The founding problem status is 'contested' because institutional and naturalist readings maintain the problem remains live (efficient price discovery requires bundling); the Georgist reading claims it is dead and the constraint is now pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_component_natural_scarcity,
    'Is the location-value component (land rent) a natural consequence of fixed supply and location scarcity (a mountain), or is it sustained by constructed property-rights enforcement that could be otherwise organized (a snare)?',
    'Jurisdictional experiment: implement land-value taxation or public land ownership in a region and measure whether location scarcity disappears or merely changes beneficiary. Compare outcomes with jurisdictions that maintain private rent capture.',
    'If rent persist under public capture but flows differently, the constraint''s classification shifts toward snare — the extraction is sustainable but the beneficiary changed. If location scarcity produces equivalent extraction under any property regime, the land component is more mountain-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_component_natural_scarcity, empirical, 'Whether location scarcity is natural or constructed, and whether rent extraction is inevitable or contingent on property law.').

omega_variable(
    improvement_coordination_vs_land_rent_conflation,
    'To what extent does the bundling of land rent and improvement value in a single price signal support genuine coordination of housing investment, versus merely obscuring the extraction and preventing alternative property arrangements?',
    'Examine historical periods or jurisdictions where land and improvement values were separately priced or taxed, and measure whether coordination outcomes (housing quality, density, investment efficiency) were superior, equivalent, or inferior to bundled-price regimes.',
    'If separable pricing supports equal or better coordination, the improvement-value component can be labeled rope independently, and the bundling serves primarily to facilitate rent capture. If bundling is necessary for coordination, the tangled-rope classification is more robust; if separation improves it, the constraint''s true type is snare dressed as tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(improvement_coordination_vs_land_rent_conflation, empirical, 'Whether the bundled price is necessary for coordinating housing investment or whether it primarily serves to obscure and facilitate extraction.').

omega_variable(
    alternative_reading_boundary,
    'Is the Georgist reading''s claim (that unearned land rent can be separated from earned improvement value by reference to location scarcity alone) conceptually coherent, or does the institutional reading''s argument (that ''location value'' itself depends on constructed institutions like zoning, infrastructure policy, and legal infrastructure) dissolve the separation?',
    'Formalize both readings'' boundary-drawing: Georgist claims land value = value at the location with zero improvement; Institutional claims land value is only calculable relative to an institutional framework (zoning, infrastructure, legal rights). Test whether these yield different measurements on the same property.',
    'If the readings measure consistently but disagree about what to do with the measurement, they coexist. If the institutional reading''s framing makes the Georgist separation incoherent, the readings foreclose each other. If the institutional reading adds complexity but the Georgist separation holds under institutional constraints, the readings influence but do not foreclose each other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_boundary, conceptual, 'Whether the Georgist land-rent/improvement-value separation is conceptually independent of institutional framing or dependent on it.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.72) structural — constraints on alternative property regimes enforced by law and market power — or internalized — the belief that property-rent extraction is natural and legitimate, persisting even when structural barriers are removed?',
    'Post-reform trajectory: if suppression remains high after structural barriers (zoning reform, land-tax implementation) are removed, infer internalization; if suppression drops, infer structural origin.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and reform requires ideological and epistemic shifts, not legal change alone. If structural, targeted legal change may be sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternatives to the rent-extraction system is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1890, price_formation_kernel__georgist_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement_basis(pric_tr_t1890, observed).
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__georgist_reading, theater_ratio, 1950, 0.32).
narrative_ontology:measurement_basis(pric_tr_t1950, observed).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__georgist_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement_basis(pric_tr_t1980, observed).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__georgist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(pric_tr_t2000, observed).
narrative_ontology:measurement(pric_tr_t2015, price_formation_kernel__georgist_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement_basis(pric_tr_t2015, observed).
narrative_ontology:measurement(pric_tr_t2026, price_formation_kernel__georgist_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(pric_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t1890, price_formation_kernel__georgist_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement_basis(pric_be_t1890, observed).
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__georgist_reading, base_extractiveness, 1950, 0.51).
narrative_ontology:measurement_basis(pric_be_t1950, observed).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__georgist_reading, base_extractiveness, 1980, 0.59).
narrative_ontology:measurement_basis(pric_be_t1980, observed).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__georgist_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement_basis(pric_be_t2000, observed).
narrative_ontology:measurement(pric_be_t2015, price_formation_kernel__georgist_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(pric_be_t2015, observed).
narrative_ontology:measurement(pric_be_t2026, price_formation_kernel__georgist_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(pric_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1890, price_formation_kernel__georgist_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement_basis(pric_su_t1890, observed).
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__georgist_reading, suppression_requirement, 1950, 0.62).
narrative_ontology:measurement_basis(pric_su_t1950, observed).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__georgist_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement_basis(pric_su_t1980, observed).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__georgist_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement_basis(pric_su_t2000, observed).
narrative_ontology:measurement(pric_su_t2015, price_formation_kernel__georgist_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(pric_su_t2015, observed).
narrative_ontology:measurement(pric_su_t2026, price_formation_kernel__georgist_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(pric_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, land_value_taxation_policy).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, zoning_enforcement_constraint).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, housing_debt_accumulation).

% DUAL FORMULATION NOTE:
% The price_formation_kernel decomposes into four constraint stories: (1) georgist_reading—location rent is separable, unearned, and extractive; (2) naturalist_reading—price is natural equilibrium reflecting scarcity and preference; (3) institutional_reading—price is constructed by zoning, lending, taxation; (4) financialization_reading—price is driven by credit expansion and asset-price feedback. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and types. They share a referent (the observed housing prices of a given property) but measure extraction differently because they measure against different baseline claims about what price 'should' be. The Georgist reading measures against the improvement-value baseline (ε high: location rent is extraction). The naturalist reading measures against the equilibrium baseline (ε low: price reflects coordination of supply/demand). The institutional reading measures against the undistorted-market baseline (ε high or low depending on whether institutions generate coordination or capture). The financialization reading measures against the fundamentals-based baseline (ε high: credit dynamics inflate price above productive fundamentals). Network links indicate each reading influences the others—Georgist analysis highlights the location-rent component that financialization amplifies; institutional analysis explains which rules stabilize or destabilize Georgist separation; naturalist analysis claims the separation is incoherent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
