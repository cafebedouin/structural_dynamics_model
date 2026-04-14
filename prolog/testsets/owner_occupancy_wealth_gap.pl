% ============================================================================
% CONSTRAINT STORY: owner_occupancy_wealth_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_owner_occupancy_wealth_gap, []).

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
 *   constraint_id: owner_occupancy_wealth_gap
 *   human_readable: Owner-Occupancy Wealth Gap Constraint
 *   domain: economic/housing/policy
 *
 * SUMMARY:
 *   The owner-occupancy wealth gap is a structural constraint that channels
 *   housing and capital accumulation toward property owners while
 *   systematically excluding renters from wealth-building mechanisms. The
 *   constraint operates through multiple mechanisms: tax incentives (mortgage
 *   interest deduction, capital gains exclusion), credit allocation
 *   (favorable mortgage terms for owner-occupants, restrictive terms for
 *   rental borrowers), and behavioral anchoring (cultural narrative of
 *   homeownership as primary wealth vehicle). The constraint exhibits mixed
 *   coordination-extraction character: it does coordinate housing allocation
 *   (matching capital with properties) and solves genuine credit problems
 *   (mortgages enable housing access), but does so asymmetrically, extracting
 *   from renters through rent expenditure that prevents down-payment
 *   accumulation while extracting toward property owners through tax and
 *   credit benefits. The extractiveness has increased substantially over 20
 *   years as housing costs have escalated faster than renter incomes,
 *   creating a widening gap between owner and renter wealth accumulation.
 *   Theater ratio (0.65) reflects that many policy interventions (first-time
 *   buyer programs, down-payment assistance) are performative — designed to
 *   address the constraint symbolically rather than structurally, reaching
 *   <5% of eligible renters annually while down-payment gaps continue
 *   widening.
 *
 * KEY AGENTS:
 *   - Renters (trapped/powerless): Primary victims; cannot accumulate capital for down payment; rent consumes income that owner-occupants direct to equity build-up
 *   - Property owners (arbitrage/institutional): Primary beneficiaries; experience constraint as coordination; capture tax benefits, leverage advantages, and asset appreciation
 *   - Financial institutions (institutional/powerful): Secondary beneficiaries; capture mortgage servicing margins; allocate credit favorable to owner-occupants, restrictive to renters
 *   - First-time buyers (constrained/moderate): Transitional agents; experience tangled rope mix of debt access (coordination) and high burden (extraction)
 *   - Housing advocates/policy makers (organized/constrained): See scaffold potential; pursuing sunset mechanisms (community land trusts, inclusionary zoning, rent control)
 *   - Ideological homeownership narrative (institutional/arbitrage): Piton constraint; maintains cultural legitimacy despite functional limitations; performative policy interventions sustain appearance of accessibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(owner_occupancy_wealth_gap, 0.58).
domain_priors:suppression_score(owner_occupancy_wealth_gap, 0.72).
domain_priors:theater_ratio(owner_occupancy_wealth_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(owner_occupancy_wealth_gap, extractiveness, 0.58).
narrative_ontology:constraint_metric(owner_occupancy_wealth_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(owner_occupancy_wealth_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(owner_occupancy_wealth_gap, tangled_rope).
narrative_ontology:human_readable(owner_occupancy_wealth_gap, "Owner-Occupancy Wealth Gap Constraint").
narrative_ontology:topic_domain(owner_occupancy_wealth_gap, "economic/housing/policy").

domain_priors:requires_active_enforcement(owner_occupancy_wealth_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(owner_occupancy_wealth_gap, property_owners).
narrative_ontology:constraint_beneficiary(owner_occupancy_wealth_gap, financial_institutions).
narrative_ontology:constraint_victim(owner_occupancy_wealth_gap, renters).
narrative_ontology:constraint_victim(owner_occupancy_wealth_gap, excluded_populations).
narrative_ontology:constraint_victim(owner_occupancy_wealth_gap, intergenerational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RENTER (SNARE) — Structurally mobile economically but mathematically trapped: rent consumes 30-50% of income, preventing capital accumulation for down payment. Exit requires capital that the constraint structure prevents accumulating. No coordination benefit; pure extraction through structural debt-dependency. Generational horizon: wealth gap compounds across decades, trapping descendants of renters.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTY OWNER (ROPE) — Experiences the constraint as coordination: tax incentives, leverage via mortgage debt, and rent extraction solve the genuine problem of capital formation and housing allocation. Net beneficiary with arbitrage options (can buy/sell strategically). Experiences low effective extraction; the constraint extracts toward them.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FIRST-TIME BUYER (TANGLED ROPE) — Structurally mobile (can save, can borrow) but severely constrained by down-payment requirements, income qualification thresholds, and regional housing cost escalation. Experiences genuine coordination (mortgage debt allows housing access) alongside asymmetric extraction (debt burden, foreclosure risk, appreciation capture by earlier cohorts). High perceived extraction due to constrained options; some coordination benefit from debt access.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HOUSING POLICY COALITION (SCAFFOLD) — Organized agents (affordable housing advocates, some municipalities, first-time buyer programs) see a temporary coordination failure with sunsets: community land trusts, inclusionary zoning, first-time buyer assistance programs, and rent-to-own initiatives represent alternative pathways. Suppression remains high but is perceived as declining through policy intervention. Sunset clause implicit in transitional nature of affordability programs.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HOMEOWNERSHIP IDEOLOGY (PITON) — The cultural and policy narrative of homeownership as primary wealth-building vehicle persists despite structural barriers that make it inaccessible to renters. Theater ratio 0.65 reflects that much homeownership policy is performative: first-time buyer programs reach <5% of renters annually; down-payment assistance is insufficient given price escalation. The ideology maintains through institutional inertia and political coalition value, not because it functionally solves housing access.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — The risk of naturalizing owner-occupancy wealth gaps as immutable economic laws: capital accumulation requires starting capital (bootstraps problem), housing scarcity drives prices, individual financial responsibility determines access. This perspective risks seeing the constraint as emergent from nature (economics) rather than constructed (policy + incentives + credit allocation). The engine will flag this as a false summit: the structural data shows requirements_active_enforcement and beneficiaries/victims, contradicting mountain emergence criteria.
constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(owner_occupancy_wealth_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(owner_occupancy_wealth_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(owner_occupancy_wealth_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(owner_occupancy_wealth_gap, TR),
    TR >= 0.70.

:- end_tests(owner_occupancy_wealth_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting that the constraint transfers substantial economic value from renters to owners through systematic allocation of credit, tax incentives, and asset appreciation rights. The value is higher than a pure coordination mechanism (Rope ≤0.45) because extraction is asymmetric and structural. Suppression (0.72): High, reflecting multiple barriers to renter exit: down-payment requirements ($30-50k), income qualification thresholds (debt-to-income ≤43%), regional housing cost escalation (median prices rising 3-5% annually vs median renter income rise <2% annually), and intergenerational wealth dependency (inheritance of down-payment capital strongly predicts ownership). Theater ratio (0.65): Moderate-high, reflecting that policy interventions targeting the constraint are substantially performative. First-time buyer programs reach <5% of renters annually; down-payment assistance averages $8-15k in regions where median down-payment gaps exceed $80-120k; inclusionary zoning requirements cover <2% of new construction nationally. The theater has increased over the 20-year interval as policy rhetoric about 'expanding homeownership' has become disconnected from material accessibility.
 *
 * PERSPECTIVAL GAP:
 *   The owner-occupancy constraint demonstrates stark perspectival divergence. Property owners perceive it as pure coordination (Rope) — mortgages solve the capital problem, tax incentives reward responsibility, and the market fairly allocates housing. Renters perceive it as pure extraction (Snare) — rent expenditure mathematically prevents capital accumulation, and the system is designed to exclude them. Analytical observers risk a false mountain perspective (natural law) — capital accumulation requires starting capital, housing supply is scarce, individuals differ in financial responsibility. But the structural data reveals contingent institutional design: the tax code could treat renters and owners equally; credit allocation could be neutral on occupancy type; housing policy could prioritize accessibility over wealth concentration. The gap between beneficiary and victim perspectives reveals the constraint is not inevitable but enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values flow from structural position: Property owners (beneficiary + arbitrage exit) derive d ≈ 0.15 (low extraction experienced). Renters (victim + trapped exit) derive d ≈ 0.92 (maximum extraction experienced). Financial institutions (beneficiary + powerful context) derive d ≈ 0.05 (negative extraction — they are subsidized by the constraint structure). First-time buyers (mixed beneficiary/victim + constrained exit) derive d ≈ 0.65 (moderate extraction despite some benefit). The sigmoid f(d) amplifies extraction for trapped agents and dampens it for arbitrage agents, making the perspectival gap large. Scope σ(national=1.0) does not amplify or dampen; national housing markets scale uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in owner-occupancy is the risk of conflating 'market outcome' with 'coordination mechanism.' The constraint IS a coordination mechanism (mortgages do allocate capital to housing), but the mandatrophy reveals that coordination and extraction are NOT mutually exclusive. The constraint requires active policy enforcement (tax code, credit regulations, zoning, down-payment standards) — it does not emerge naturally. The beneficiary/victim structure is asymmetric: owners benefit from tax incentives and leverage, renters bear costs through price escalation and exclusion. If policy enforcement were removed (eliminate mortgage interest deduction, equalize credit terms, allow renter wealth alternatives), the constraint would collapse — which proves it is an artifact of policy design, not an emergent property of capital markets. The false mountain perspective naturalizes this constructed asymmetry as economic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_amplification_mechanism,
    'Is the wealth gap driven by differential access to credit leverage or by differential savings capacity?',
    'Decomposition of wealth accumulation: isolate mortgage leverage effects from rent-vs-own savings differences; control for down payment assistance programs',
    'If leverage: policy should equalize debt access (constrained credit expansion → reduces snare character). If savings: policy should raise renter income or reduce rent (trapped income allocation → strengthens snare character). Different mechanisms require different interventions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_amplification_mechanism, empirical, 'Debt leverage vs savings capacity as mechanism').

omega_variable(
    housing_supply_vs_demand_elasticity,
    'Does policy enforcement of owner-occupancy benefits (tax incentives, low down-payment mortgages) cause housing price escalation that neutralizes renter mobility?',
    'Price elasticity analysis; longitudinal housing cost tracking in regions with vs without aggressive homeownership promotion; correlation between tax incentive magnitude and down-payment barrier growth',
    'If yes: policy creates feedback loop where coordination (tax incentives) amplifies extraction (price escalation). Classification shifts toward Snare (extraction amplified by policy intent). If no: policy genuinely enables access. Classification remains Tangled Rope (mixed coordination-extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_supply_vs_demand_elasticity, empirical, 'Whether homeownership incentives cause price escalation feedback').

omega_variable(
    intergenerational_lock_in,
    'Is the owner-occupancy wealth gap self-perpetuating through inheritance and kinship wealth transfer, or can policy intervention break the cycle?',
    'Generational wealth transmission analysis; comparison of wealth mobility in cohorts with vs without inherited down-payment assistance; tracking of policy program graduates across 2-3 generations',
    'If self-perpetuating via inheritance: the constraint has mountain characteristics at generational scope (accessibility_collapse ≥ 0.85 if inheritance substitutes for policy). If breakable via policy: scaffold sunset is realistic. If inheritance-dependent but policy can substitute: Tangled Rope with active enforcement required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_lock_in, empirical, 'Generational lock-in vs policy-breakable cycle').

omega_variable(
    renter_preference_confound,
    'To what extent is the wealth gap a constraint that suppresses renter ownership, vs a market outcome of heterogeneous preferences (some agents prefer rental flexibility)?',
    'Preference studies controlling for income and availability; tracking of stated preferences for ownership pre-vs-post policy change; behavioral response to first-time buyer programs',
    'If preference-driven: extractiveness is lower (agent has mobile options aligned with preferences). If suppression-driven: extractiveness is higher (agent''s preference for ownership is blocked). This affects whether the snare classification is robust or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renter_preference_confound, conceptual, 'Preference heterogeneity vs structural suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(owner_occupancy_wealth_gap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(owne_tr_t0, owner_occupancy_wealth_gap, theater_ratio, 0, 0.4).
narrative_ontology:measurement(owne_tr_t10, owner_occupancy_wealth_gap, theater_ratio, 10, 0.55).
narrative_ontology:measurement(owne_tr_t20, owner_occupancy_wealth_gap, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(owne_be_t0, owner_occupancy_wealth_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(owne_be_t10, owner_occupancy_wealth_gap, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(owne_be_t20, owner_occupancy_wealth_gap, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(owner_occupancy_wealth_gap, resource_allocation).
narrative_ontology:affects_constraint(owner_occupancy_wealth_gap, housing_affordability_crisis).
narrative_ontology:affects_constraint(owner_occupancy_wealth_gap, intergenerational_wealth_mobility).
narrative_ontology:affects_constraint(owner_occupancy_wealth_gap, racial_wealth_gap).

% DUAL FORMULATION NOTE:
% Owner-occupancy wealth gap is downstream of specific policy decisions (tax code, credit allocation, zoning) but represents a distinct structural constraint. Related constraints in housing affordability and wealth mobility have their own extractiveness values reflecting domain-specific mechanisms; this constraint focuses on the coordination-extraction hybrid in capital allocation via homeownership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(owner_occupancy_wealth_gap, powerful, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
