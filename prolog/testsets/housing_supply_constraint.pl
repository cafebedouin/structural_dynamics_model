% ============================================================================
% CONSTRAINT STORY: housing_supply_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_housing_supply_constraint, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: housing_supply_constraint
 *   human_readable: Housing Supply Constraint in Urban Markets
 *   domain: economic/urban_policy
 *
 * SUMMARY:
 *   Housing supply constraints in urban markets create structural extraction
 *   from renters and first-time buyers toward incumbent property owners and
 *   local governments dependent on property tax revenue. The constraint
 *   exhibits characteristics of a tangled_rope: genuine coordination function
 *   exists (markets allocate scarce units), but alongside asymmetric
 *   extraction favoring those with existing capital. Zoning restrictions,
 *   permitting delays, and land-use regulations form the enforcement
 *   mechanism. The constraint's evolution shows increasing extractiveness
 *   (0.35→0.58 over 50 years) as property values accumulate and incumbent
 *   benefit compounds, while theater ratio remains moderate because policy
 *   interventions (zoning reform, density bonuses) do produce material supply
 *   changes, distinguishing this from pure performance theater. The
 *   perspectival gap is stark: incumbent owners experience rope-like
 *   coordination with personal benefit; renters experience snare-like
 *   entrapment; the reform coalition experiences a temporary constraint
 *   amenable to policy change (scaffold); the zoning board experiences
 *   institutionalized routine (piton); the civilizational view risks
 *   naturalizing what is actually contingent institutional arrangement (false
 *   mountain).
 *
 * KEY AGENTS:
 *   - Incumbent Property Owners: Primary beneficiary (institutional/arbitrage) — benefit from artificial scarcity driving appreciation; have options to relocate or invest elsewhere
 *   - Real Estate Development Firms: Secondary beneficiary (powerful/arbitrage) — benefit from restricted supply creating monopoly-like conditions and higher profit margins
 *   - Local Government: Secondary beneficiary (institutional/arbitrage) — property tax revenue tied to valuations; gains from scarcity-driven appreciation
 *   - Renters: Primary victim (powerless/trapped) — structurally immobile; face rising rents, bidding wars, credit barriers; cannot exit without major life disruption
 *   - First-Time Homebuyers: Secondary victim (moderate/constrained) — face high barriers but retain some agency; can relocate, extend timeline, or downgrade expectations
 *   - Supply-Side Reform Coalition: Organized agent (organized/constrained) — YIMBY advocates, housing nonprofits seeking zoning reform; see constraint as addressable through policy
 *   - Zoning and Planning Boards: Institutional actor (institutional/arbitrage) — maintain restrictive patterns; face political constraints from incumbent resident coalitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(housing_supply_constraint, 0.58).
domain_priors:suppression_score(housing_supply_constraint, 0.65).
domain_priors:theater_ratio(housing_supply_constraint, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(housing_supply_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(housing_supply_constraint, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(housing_supply_constraint, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(housing_supply_constraint, tangled_rope).
narrative_ontology:human_readable(housing_supply_constraint, "Housing Supply Constraint in Urban Markets").
narrative_ontology:topic_domain(housing_supply_constraint, "economic/urban_policy").

domain_priors:requires_active_enforcement(housing_supply_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(housing_supply_constraint, incumbent_property_owners).
narrative_ontology:constraint_beneficiary(housing_supply_constraint, real_estate_development_firms).
narrative_ontology:constraint_beneficiary(housing_supply_constraint, local_government_revenue_departments).
narrative_ontology:constraint_victim(housing_supply_constraint, first_time_homebuyers).
narrative_ontology:constraint_victim(housing_supply_constraint, renters).
narrative_ontology:constraint_victim(housing_supply_constraint, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RENTER WITHOUT CAPITAL (SNARE) — Faces insurmountable barriers: down payment requirements, credit barriers, competitive bidding wars, zoning restrictions that limit supply. No exit from rental dependency. Maximum suppression and extraction. Experienced chi approaches 1.0 despite moderate base extractiveness — the trapped exit status and victim classification amplify the effective extraction significantly through f(d).
constraint_indexing:constraint_classification(housing_supply_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FIRST-TIME HOMEBUYER (TANGLED ROPE) — Constrained by high entry costs and competitive bidding, but also participates in coordination: housing markets coordinate allocation of scarce housing units. Some agency exists (can shop in adjacent regions, save longer, accept lower quality). Experiences both the coordination function (price signals allocate units) and asymmetric extraction (favors those with existing wealth).
constraint_indexing:constraint_classification(housing_supply_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT PROPERTY OWNER (ROPE) — Benefits from artificial scarcity; experiences constraint as pure coordination with net-positive benefit. Has arbitrage options (can relocate, rent out, convert to higher-value use). Constraint preserves their asset value through supply restrictions. Derives beneficiary status, low directionality d, low chi.
constraint_indexing:constraint_classification(housing_supply_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SUPPLY-SIDE REFORM COALITION (SCAFFOLD) — Organized advocacy (YIMBY movements, housing nonprofits, developer associations seeking reduced friction) pushing zoning reform, density permits, streamlined approval. Sees the constraint as temporary and addressable through policy change. Theater ratio is moderate because policy reform genuinely changes underlying supply dynamics, unlike performative coordination. Sunset logic: as zoning restrictions ease and supply grows, the scarcity extraction mechanism weakens.
constraint_indexing:constraint_classification(housing_supply_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LOCAL ZONING BOARD (PITON) — Maintains restrictive zoning patterns through institutional inertia and historical precedent. Boards acknowledge that restrictions create affordability problems but claim they protect neighborhood character and infrastructure capacity. The rationale persists despite evidence of theater: many zoning restrictions predate modern conditions, are not consistently enforced, and don't correlate with stated neighborhood protection goals. Theater ratio is elevated because the board's justifications are largely performative — the real function is to maintain political coalition stability and incumbent property values.
constraint_indexing:constraint_classification(housing_supply_constraint, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, housing supply constraints reflect irreducible tradeoffs between density, environmental impact, infrastructure capacity, and community stability. Some restriction of housing supply is inherent to settled societies: communities must resolve collective action problems (infrastructure investment, environmental externalities, neighborhood cohesion). This perspective naturalizes the constraint as an immutable limit on urban coordination. However, the structural data contradicts this reading: supply restrictions vary dramatically by jurisdiction (Tokyo, Houston, Singapore have higher density with lower restrictions), indicating contingency rather than natural law.
constraint_indexing:constraint_classification(housing_supply_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(housing_supply_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(housing_supply_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(housing_supply_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(housing_supply_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(housing_supply_constraint, TR),
    TR >= 0.70.

:- end_tests(housing_supply_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Supply restrictions create measurable extraction: price premiums on land and housing, rent increases, shifted savings burden. The value reflects that extraction is real and growing (0.35→0.58 over interval) but not total — rents remain below theoretical replacement-cost-plus-normal-profit in most markets, indicating some competitive pricing persists. Suppression (0.65): High. Multiple barriers prevent exit or mitigation: down payment requirements eliminate low-wealth renters from ownership; geographic mobility is limited by employment concentration; credit barriers locked out by prior debt; zoning prevents alternative housing types. Suppression is not total because some exit options exist (cohabitation to pool resources, migration to low-restriction jurisdictions, informal housing arrangements). Theater ratio (0.48): Moderate. Unlike pure extraction mechanisms that rely entirely on narrative framing, housing policy discussions involve real supply/demand tradeoffs and genuine technical considerations (infrastructure capacity, environmental externalities). However, theater has increased over the interval as justifications become more formulaic ('protecting neighborhood character') despite evidence that character preservation correlates poorly with specific restrictions. The rising theater ratio (0.32→0.48) reflects increasing gap between stated (community protection) and revealed (value preservation) goals.
 *
 * PERSPECTIVAL GAP:
 *   The renter (snare) and incumbent owner (rope) experiences are inversely related through the beneficiary/victim declarations. The constraint that benefits owners must hurt renters — it is zero-sum at the core. The reform coalition's scaffold perspective is genuinely different: they see a third exit path (policy change) that neither renters nor owners perceive clearly. The piton perspective (zoning board) sees institutional obligation without functional necessity — the board performs its role despite recognizing it doesn't achieve stated goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural relationship to the supply constraint. Renters (trapped exit, victim status) experience d ≈ 0.92, producing f(d) ≈ 1.35, amplifying base extractiveness 0.58 to effective chi ~0.78. Incumbent owners (arbitrage exit, beneficiary status) experience d ≈ 0.12, producing f(d) ≈ -0.01, reducing base extractiveness to negative chi (they experience subsidy). First-time buyers (constrained exit, victim status) experience d ≈ 0.68, producing f(d) ≈ 1.00, keeping chi near base value ~0.58. Reform coalition (constrained exit, neither beneficiary nor victim in economic sense, but organized) experiences d ≈ 0.45, producing f(d) ≈ 0.50, reducing perceived extraction. Scope modifier σ(regional) = 0.9 applies uniformly, slightly dampening all chi values relative to national or global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The housing supply constraint resolves mandatrophy through decomposition by power level and exit options. No single type is correct; instead, the constraint exhibits all five types (mountain view is likely false summit) across different structural positions. RENTER: Snare classification is robust. High suppression (0.65), victim status, trapped exit, and extractiveness >0.46 all align. OWNER: Rope classification is robust. Beneficiary status, arbitrage exit, low suppression experienced (they benefit), coordination function apparent (supply allocation). FIRST-TIME BUYER: Tangled rope is correct intermediate. Hybrid experience reflects partially constrained exit (can save, relocate, wait) and dual relationship (victim via high costs, participant via market allocation). REFORM COALITION: Scaffold is correct because their classification depends on time horizon. At immediate (T=0): tangled_rope or snare (we face high costs today). At generational (T=30): scaffold (we see zoning reform pathway with realistic sunset). The temporal dependence is not an error — it is a feature of scaffold classification. ZONING BOARD: Piton classification reflects theater_ratio (0.48→0.55) and functional atrophy. Boards claim to protect community but restrictions pre-date modern conditions, are not consistently applied, and don't correlate with stated protection goals. The institutional role persists through inertia. The constraint thus demonstrates how DR classification works: not as a category label, but as a description of structural experience from specific indexed positions. The mandatrophy is resolved by recognizing that the constraint IS a tangled_rope from the system view (exhibiting both coordination and extraction), while agents at different positions within it experience pure snare, rope, scaffold, or piton depending on their power, exit options, time horizon, and scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_scarcity_vs_artificial_restriction,
    'Is the housing supply shortage driven by genuine scarcity (land, capital, labor) or by artificial institutional restrictions (zoning, permitting, land use regulations)?',
    'Cross-jurisdictional analysis: compare extractiveness and suppression metrics across cities with identical natural constraints (climate, geography, density) but different zoning regimes. High variance across jurisdictions indicates institutional, not natural, drivers.',
    'If natural scarcity dominates: constraint approaches mountain; extraction is inherent coordination cost. If institutional restrictions dominate: constraint is tangled_rope or snare; extraction is rent-seeking layered onto coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_scarcity_vs_artificial_restriction, empirical, 'Whether housing shortage is natural scarcity or artificial restriction').

omega_variable(
    incumbent_owner_consciousness_and_extraction,
    'Do incumbent property owners deliberately enforce scarcity-preservation tactics (opposing new construction, lobbying against zoning reform) as a conscious extraction mechanism, or do they rationalize opposition as community protection without recognizing the beneficiary status?',
    'Discourse analysis of public statements vs. revealed preferences; mapping of property owner political donations and zoning testimony against benefits received from restriction-driven appreciation; experimental vignettes testing whether framing benefits changes justification rhetoric.',
    'If conscious exploitation: constraint is clearly snare from incumbent perspective, with organized opposition to reform. If unconscious rationalization: constraint may appear rope-like to beneficiaries (they see only coordination benefits) while remaining snare/tangled_rope to victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_owner_consciousness_and_extraction, conceptual, 'Whether supply scarcity is consciously exploited or rationalized').

omega_variable(
    supply_elasticity_and_price_response,
    'What is the price elasticity of new housing supply in constrained markets? Will reducing institutional restrictions (zoning, permitting) actually increase supply substantially, or are natural constraints (labor, land cost, construction capital) the binding factor?',
    'Econometric analysis of supply response to zoning changes; panel data from jurisdictions undergoing zoning reform; time-series analysis of permits granted vs. units built vs. price changes.',
    'If zoning is binding constraint: easing restrictions will increase supply and reduce prices, validating the scaffold sunset logic. If natural constraints dominate: supply will remain restricted regardless of zoning changes, and the constraint persists, suggesting piton trajectory (theater of reform without effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_elasticity_and_price_response, empirical, 'Price elasticity of housing supply to zoning changes').

omega_variable(
    identity_lock_in_homeowner_identity,
    'For incumbent property owners and politically active residents, is opposition to housing supply expansion rooted in material interests (property value preservation) or in identity fusion (the neighborhood as constitutive of self-concept, resistance to supply increase as defense of ''who we are'')?',
    'Qualitative analysis of public statements and testimony for identity language vs. economic language; tracking whether opposition persists if property values were protected by alternative policy (e.g., real estate tax credits) that didn''t rely on scarcity; comparison of opposition strength between owners with high leverage (debt) vs. low leverage.',
    'If material interests dominate: reform is possible through policy design (protect property values through alternative mechanisms). If identity fusion dominates: reform faces deeper cognitive barriers; opposition persists despite material incentive removal, suggesting constraint has identity_coordination boltzmann type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_homeowner_identity, conceptual, 'Whether opposition is material interest or identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(housing_supply_constraint, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hous_tr_t0, housing_supply_constraint, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hous_tr_t15, housing_supply_constraint, theater_ratio, 15, 0.42).
narrative_ontology:measurement(hous_tr_t30, housing_supply_constraint, theater_ratio, 30, 0.48).
narrative_ontology:measurement(hous_tr_t45, housing_supply_constraint, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(hous_be_t0, housing_supply_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hous_be_t15, housing_supply_constraint, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(hous_be_t30, housing_supply_constraint, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hous_be_t45, housing_supply_constraint, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(housing_supply_constraint, resource_allocation).
narrative_ontology:affects_constraint(housing_supply_constraint, residential_wealth_gap).
narrative_ontology:affects_constraint(housing_supply_constraint, urban_migration_pressure).
narrative_ontology:affects_constraint(housing_supply_constraint, municipal_fiscal_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(housing_supply_constraint, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
