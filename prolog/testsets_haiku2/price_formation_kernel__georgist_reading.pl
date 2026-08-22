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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Land Rent Separation (Georgist Reading)
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   The Georgist reading of price formation asserts that housing prices
 *   conflate two structurally distinct components: earned improvement value
 *   (labor, capital, materials) and unearned land rent (location scarcity,
 *   pre-existing title, exclusionary law). This reading claims that price
 *   formation mechanisms obscure the distinction, making rent extraction
 *   appear as equilibrium and naturalizing what is actually a snare.
 *   Incumbent landowners and financial intermediaries benefit from this
 *   confusion; aspiring occupants and wage-dependent consumers bear the rent
 *   extraction cost. The constraint is CLAIMED as tangled_rope (containing
 *   genuine improvement coordination alongside asymmetric rent extraction)
 *   while acknowledging that the land-rent component alone (absent
 *   improvement) would classify as snare. This reading contests the
 *   naturalist and institutional readings: naturalism treats location
 *   scarcity as exogenous equilibrium (mountain frame); institutionalism
 *   treats the rent separation itself as constructed rather than inherent.
 *   The Georgist reading occupies a middle position: the physical scarcity of
 *   location is natural (mountain), but the legal and financial mechanisms
 *   that convert that scarcity into rent extraction are constructed (snare
 *   riding on the mountain).
 *
 * KEY AGENTS:
 *   - incumbent_landowners: Primary beneficiary; extract rents through title and exclusion (powerful, arbitrage exit)
 *   - financial_intermediaries: Secondary beneficiary; capitalize rent streams into loans (institutional, arbitrage exit)
 *   - land_aspiring_occupants: Primary victims; pay rent without choice (powerless, trapped exit)
 *   - labor_dependent_housing_consumers: Secondary victims; wage compression from rent burden (moderate power, constrained exit)
 *   - georgist_reformers: Excluded voice; would reorganize price formation if seated (moderate power, constrained exit)
 *   - labor_and_capital_improvers: Beneficiaries of improvement recognition; constrained by rent foundation (organized, constrained exit)
 *   - conventional_economists: Observers; legitimize rent as equilibrium (analytical power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Land Rent Separation (Georgist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '87fc3ff5-6a9a-442b-9ecc-39a003db529e').
narrative_ontology:cs_kernel_codification('87fc3ff5-6a9a-442b-9ecc-39a003db529e', fixed_text).
narrative_ontology:cs_authority_grounding('87fc3ff5-6a9a-442b-9ecc-39a003db529e', extraction).
narrative_ontology:cs_interpretation_layer_present('87fc3ff5-6a9a-442b-9ecc-39a003db529e').
narrative_ontology:cs_reading_relation('87fc3ff5-6a9a-442b-9ecc-39a003db529e', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('87fc3ff5-6a9a-442b-9ecc-39a003db529e', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('87fc3ff5-6a9a-442b-9ecc-39a003db529e', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('87fc3ff5-6a9a-442b-9ecc-39a003db529e', foundational, land_rent_is_unearned_extraction).
narrative_ontology:cs_axiom_status(land_rent_is_unearned_extraction, holdable).
narrative_ontology:cs_axiom_grounding('87fc3ff5-6a9a-442b-9ecc-39a003db529e', land_rent_is_unearned_extraction, deontological).
narrative_ontology:cs_axiom('87fc3ff5-6a9a-442b-9ecc-39a003db529e', foundational, improvement_value_is_earned_production).
narrative_ontology:cs_axiom_status(improvement_value_is_earned_production, holdable).
narrative_ontology:cs_axiom_grounding('87fc3ff5-6a9a-442b-9ecc-39a003db529e', improvement_value_is_earned_production, empirically_contingent).
narrative_ontology:cs_reference_frame('87fc3ff5-6a9a-442b-9ecc-39a003db529e', earned_improvement_unearned_rent_distinction).
narrative_ontology:cs_drift_state('87fc3ff5-6a9a-442b-9ecc-39a003db529e', contemporary_financialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87fc3ff5-6a9a-442b-9ecc-39a003db529e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, incumbent_landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, rent_capturing_financial_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, land_aspiring_occupants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_dependent_housing_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, financial_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, labor_and_capital_improvers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture unearned land rent through passive ownership of scarce locations. They receive appreciation from location demand without producing improvement value. Their position is protected by legal title, exclusion doctrine, and speculative demand. Exit is not a meaningful option — they hold and extract rents indefinitely.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, incumbent_landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Extract intermediation rents by financing land purchases at loan-to-value ratios that capitalize future rent streams. They benefit from the inability of aspiring occupants to distinguish earned-value debt from rent-extraction debt. Their enforcement interest is in collateralizing location scarcity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, financial_intermediaries, beneficiary,
    institutional, generational, arbitrage, global).

% Must pay for location scarcity (the unearned land component) to access any location at all. They cannot distinguish, in the price they are quoted, how much is rent extraction versus legitimate improvement cost. Their exit is geographically and economically constrained — they need to live somewhere, and all desirable locations are already titled to others who extract rents.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_aspiring_occupants, payer,
    powerless, biographical, trapped, national).

% Pay inflated housing costs because their wage income must service both legitimate improvement value AND unearned land rent. The constraint obscures the rent component, making it appear as if location scarcity is a natural equilibrium rather than an extractive arrangement. Their exit options are migration (costly, socially disruptive) or wage suppression (market pressure to accept lower wages to afford housing).
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_dependent_housing_consumers, payer,
    moderate, biographical, constrained, national).

% Would advocate for land-value taxation, rent separation, or public land ownership if they had standing in price formation. They are excluded because price formation is set by incumbent landowners and financial intermediaries; reformers are outside the transaction structure and can only lobby for institutional change. Their absence from the primary seats means rent-extraction normalization goes unchallenged at the price-formation moment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_reformers, excluded,
    moderate, biographical, constrained, national).

% Receive compensation for improvement value (the earned component) — construction workers, architects, engineers, material suppliers. They benefit from the separation claim insofar as it recognizes improvement as earned; but the constraint's suppression makes it difficult for them to organize politically because the rent component is naturalized and invisible. Their improvement work is real coordination, but it rides on an extractive rent foundation that they do not control.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_and_capital_improvers, beneficiary,
    organized, generational, constrained, national).

% Study price formation using equilibrium models that treat location scarcity as exogenous and unearned rents as equilibrium outcomes. They provide intellectual legitimacy to the rent-extraction arrangement by framing it as natural, not constructed. Their analytical distance enables them to see structure; their institutional position often depends on the arrangements they analyze.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, conventional_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, incumbent_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Improvement value coordination: labor, capital, materials, and land itself must be combined to produce housing. Price formation coordinates this production by signaling where improvements should occur and compensating those who undertake them. This is genuine coordination and produces earned value.
% TRANSFER_FUNCTION: Moves unearned land-rent surplus from aspiring occupants and wage-dependent consumers to incumbent landowners and financial intermediaries. The rent component is NOT payment for improvement production; it is extraction of value from location scarcity that the occupant did not create and could not avoid. A secondary transfer moves intermediation rents to financial institutions.
% ABSENT_VOICES: Georgist reformers, future generations whose location options will be constrained by today's rent capitalization, and non-landed labor (wage-workers whose housing cost absorbs rents they did not authorize). These voices are excluded because price formation occurs at the bilateral transaction level (landowner + buyer/renter + financier) without institutional space for dissent. Their absence enables rent naturalization.
% DISAPPEARANCE_RATIONALE: If the rent-separation constraint vanished and land rents were instead captured by public systems or redistributed, price formation would reorganize: housing prices would reflect only improvement costs, location scarcity would be managed differently (e.g., via auction, public ownership, or land-value tax), and the financial intermediation structure riding on rent capitalization would collapse or shift. The world would rearrange because today's price structure depends on rent extraction; removing that extraction changes every price signal.
% FOUNDING_PROBLEM: How are scarce locations allocated among competing uses? Early industrial societies needed a mechanism to signal where improvements should occur and to compensate improvement creators while dealing with pre-existing landholdings and exclusionary title.
% FOUNDING_PROBLEM_CORROBORATION: The allocation problem is live and real — scarcity persists. But who benefits from the current allocation mechanism is disputed: landowners and financiers attest the market mechanism is the only efficient solution; Georgists and reformers attest the mechanism is capturing unearned rents and should be restructured (with testimony from comparative housing markets, historical rent analysis, and political economy analysis outside the benefiting parties). Institutional economists attest the allocation is constructed by zoning and finance, not natural.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78 terminal) because the constraint captures unearned location rents without producing improvement. The measurement series shows accumulation: as financial intermediaries mature and rent capitalization deepens (credit expansion, asset pricing), the extraction rises from 0.58 to 0.78 over the interval. Suppression is substantial (0.71) because the rent-separation distinction must be actively obscured — if occupants clearly saw rent as a separate, unearned levy, resistance would spike. Theater rises from 0.25 to 0.42 because increasing effort goes into justifying rent as equilibrium, efficient pricing, market clearing, rather than defending it as extraction. The measurement grid is unified: all three metrics share the same six time points, enabling coherent temporal analysis. The constraint's hybrid nature (improvement rope + rent snare) is captured in the stakeholder structure: labor_and_capital_improvers benefit from recognition of earned value (genuine coordination), while incumbent_landowners extract unearned rents (asymmetric, coercive). The Georgist reading does not claim land scarcity itself is constructed — location is genuinely scarce (mountain foundation) — but claims the legal and financial mechanisms converting that scarcity into extractive rent are constructed and suppressible.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (incumbent landowner) will compute this constraint as rope or even natural-law-adjacent (they experience it as equilibrium, not enforcement). The payer seat (aspiring occupant) will compute it as snare (they experience active exclusion, payment extraction, and suppression of alternatives). The reformer seat (Georgist, excluded) will compute it as snare-with-institutional-capture (the constraint persists because beneficiaries control the political mechanism). The observer seat (economist) will compute it as mountain or rope depending on their theoretical commitments — naturalists treat scarcity as exogenous (mountain), Institutionalists treat price formation as constructed (snare or rope depending on the benefit asymmetry). The Georgist reading predicts and accepts these divergences; it does not claim all seats will agree.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent landowners derive d near 0.0 (full beneficiary): they control title, suffer no enforcement burden (exclusion is the state's job), and collect rents indefinitely. Their mobility is high (arbitrage: they can sell, hold, invest elsewhere) because they are on the beneficial side of the constraint. Financial intermediaries derive d near 0.1–0.2 (beneficiary-with-enforcement): they capture intermediation rents, but their benefit depends on the constraint persisting, so they bear some enforcement cost (lobbying, legitimation theater, maintaining collateral structures). Land-aspiring occupants derive d near 0.9–1.0 (full target): they cannot access location except through the rent-extraction mechanism, have no legal claim to any parcel, and face enforcement (eviction, foreclosure) if they resist. Their exit is trapped — mobility is zero in any meaningful sense (they cannot opt out of needing location). Labor-dependent consumers derive d near 0.75–0.85 (strong target): they pay housing costs that include hidden rents, but they also benefit (minimally) from the coordination of improvement (housing actually exists, is maintained). Their exit is constrained (they need housing, but can migrate or change jobs to pursue cheaper regions). Georgist reformers derive d near 0.5 (symmetric): they neither collect rents nor pay them directly (they are excluded from the transaction), but the constraint prevents their preferred institutional change, which would benefit them politically/ideologically. This directionality profile is not overridden — it derives cleanly from the beneficiary/victim structure and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocation of scarce locations) is live, but the current solution (price formation via exclusionary title and rent extraction) persists because it benefits incumbent landowners and financial intermediaries, not because it solves the founding problem well. Price formation DOES allocate locations (improvement value signals work), but it does so by extracting unearned rents from those who must live somewhere. A mandatrophy would be declared if the founding problem were dead (locations no longer scarce) but the extraction persisted — we are not at that point. However, the Georgist reading identifies the solution as increasingly disconnected from the problem: as rent capitalization deepens (financialization acceleration), the price signal reflects less of the true cost to allocate locations and more of the rent-extraction capacity of financiers. This is a drift toward mandatrophy: the coordinate mechanism (improvement pricing) persists but increasingly rides on extraction (rent capture) rather than on the allocation problem. The measurement series captures this drift: extractiveness rises as theater rises, indicating the justification (allocation efficiency) is doing less work relative to the extraction it covers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_scarcity_vs_constructed_exclusion,
    'Is the measured land rent extraction primarily due to natural scarcity of location (mountain foundation) or to constructed exclusion doctrines (legal title, zoning, financial qualification barriers)?',
    'Comparative institutional analysis: examine housing markets with different exclusion doctrines (public land, usufruct, strong rent controls, social housing) holding location scarcity constant. If rent extraction correlates with exclusion doctrine more than with scarcity, the construction is primary.',
    'If natural scarcity dominates, the rent is a natural equilibrium fee (mountain-adjacent) and the Georgist separation is less actionable. If constructed exclusion dominates, the rent is a policy artifact (snare) and land-value taxation or public ownership are directly applicable. This is the core contestation with the naturalist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_scarcity_vs_constructed_exclusion, empirical, 'Whether measured rent reflects natural scarcity or constructed exclusion.').

omega_variable(
    improvement_coordination_viability_without_rent,
    'Can improvement value coordination (housing production, maintenance, efficient location of development) function without unearned-rent capture? Is the rent component necessary for the coordination to work?',
    'Historical analysis and natural experiments: examine periods/places where land rents were taxed, captured publicly, or suppressed (e.g., post-war housing programs, land-value-tax jurisdictions, public-housing systems). Did improvement coordination persist? Did it become more or less efficient?',
    'If coordination persists without rent, the Georgist separation is empirically vindicated and rent capture is pure extraction riding on coordination. If coordination collapses, the rent serves as a necessary incentive and the distinction is less cleanly separable. The boundary between ''rope'' and ''tangled_rope'' turns on this question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(improvement_coordination_viability_without_rent, empirical, 'Whether improvement coordination is structurally dependent on unearned-rent capture.').

omega_variable(
    rent_visibility_and_suppression_internalization,
    'Is the measured suppression primarily structural (legal/financial barriers preventing occupants from opting out) or internalized (occupants accept rent extraction as natural/inevitable, having been conditioned into the framework)?',
    'Experimental and historical evidence: test whether occupants'' resistance changes when the rent distinction is explicitly taught (educational intervention, Georgist framing); examine whether resistance was higher in periods before rent naturalization (early industrial markets where rents were openly discussed as such).',
    'If primarily structural, the constraint can be changed by legal/institutional reform (land tax, public ownership). If primarily internalized, reform requires deconditioning and frame-shift first, lengthening the transition. This affects the fixing_cost and the ceiling for politically viable reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_visibility_and_suppression_internalization, empirical, 'Whether suppression of rent-separation knowledge is structural or internalized.').

omega_variable(
    georgist_reading_vs_institutional_reading_decomposition,
    'The Georgist and institutional readings differ on whether the rent-separation distinction is inherent or entirely constructed. Does the distinction stand on its own analytical merit, or is it an artifact of the Georgist framing?',
    'Comparative institutional analysis: if different institutional architectures (zoning regimes, tax systems, financial structures) produce different rent-extraction patterns while location scarcity remains constant, the distinction is inherent (supports Georgist reading). If all architectures converge on similar rent levels regardless of their legal form, the distinction may be an artifact of framing.',
    'If the distinction is inherent, Georgist reform (land taxation, public ownership) is a viable lever for rent reduction. If constructed, institutional reform (zoning deregulation, alternative financial structures) may be more primary. This determines which reading''s recommended policies would work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(georgist_reading_vs_institutional_reading_decomposition, conceptual, 'Whether land-rent separation is an inherent distinction or a Georgist framing artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__georgist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__georgist_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__georgist_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(pric_tr_t35, price_formation_kernel__georgist_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__georgist_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__georgist_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__georgist_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(pric_be_t35, price_formation_kernel__georgist_reading, base_extractiveness, 35, 0.77).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__georgist_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__georgist_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__georgist_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(pric_su_t35, price_formation_kernel__georgist_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The price_formation_kernel constraint family decomposes into four readings, each with different structural assumptions about whether price formation is a natural equilibrium (naturalist), a constructed institutional arrangement (institutional), a credit-dynamics phenomenon (financialization), or a separable hybrid of natural scarcity (land) and constructed extraction (rent via financial/legal mechanisms). This Georgist reading instantiates the fourth interpretation. All four readings share the same empirical domain (housing prices, which do exist and do coordinate allocation) but attribute different causal primacy: Georgist reading treats the land-scarcity foundation as mountain and the rent-extraction mechanism as snare; naturalist reading treats all as mountain; institutional reading treats all as snare-or-rope constructed by rules; financialization reading treats asset-price feedback as primary. Each reading has different policy implications (Georgist: land tax/public ownership; naturalist: deregulate/let markets clear; institutional: reform rules; financialization: credit control). The engine computes which reading's structural model best fits the data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
