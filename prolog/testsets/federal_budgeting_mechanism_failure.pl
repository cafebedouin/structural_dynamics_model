% ============================================================================
% CONSTRAINT STORY: federal_budgeting_mechanism_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_budgeting_mechanism_failure, []).

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
 *   constraint_id: federal_budgeting_mechanism_failure
 *   human_readable: Federal Budgeting Mechanism Failure
 *   domain: political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The federal budgeting mechanism failure represents a structural
 *   constraint in which formal institutional procedures (annual
 *   appropriations, mandatory spending caps, deficit limits) have degraded
 *   into performative theater while actual fiscal allocation is driven by
 *   political preferences unbound by long-term constraint. The constraint
 *   exhibits mixed coordination and extraction functions: it coordinates
 *   resource allocation (legitimate function) while enabling asymmetric
 *   distribution of benefits (present) and costs (future). This creates a
 *   tangled rope structure where a genuine coordination mechanism persists
 *   alongside extraction. The mechanism failure began with the erosion of
 *   binding constraints (Gramm-Rudman-Hollings repeal in 2002, PAYGO sunset
 *   in 2008, Budget Control Act caps waived 2013-2019) and accelerated via
 *   the normalization of continuing resolutions, supplemental appropriations,
 *   and baseline spending growth. Theater ratio increased from 0.42 (when
 *   budget process had teeth) to 0.68 (when constraints are routinely
 *   violated) as the ritual persists while real control mechanisms atrophied.
 *   Extractiveness increased from 0.35 (balanced budget era with genuine
 *   trade-offs) to 0.58 (current deficit spending with costs deferred to
 *   future). The constraint operates across six distinct perspectives:
 *   powerless agents (future generations, dependent populations) experience
 *   snare-level extraction with no exit; moderate agents (taxpayers)
 *   experience tangled rope with constrained exit; institutional
 *   beneficiaries (Congress, executive) experience rope with arbitrage exit;
 *   organized reformers experience tangled rope constrained by political
 *   opposition; the budget process itself is a piton (performative theater);
 *   the analytical observer risks false summit by naturalizing the constraint
 *   as immutable democratic feature.
 *
 * KEY AGENTS:
 *   - Congress and Executive Branch: Primary beneficiary (institutional/arbitrage) — capture immediate appropriations authority, shift constraints to future, can approve spending without binding fiscal consequence
 *   - Future Generations: Primary victim (powerless/trapped) — inherit accumulated debt, must service interest payments, constrained by fiscal legacy. No voice in current decisions, cannot exit.
 *   - Dependent Populations (SNAP, housing, disability): Secondary victim (powerless/trapped) — discretionary budget items targeted when constraints tighten; have no exit option from system dependence
 *   - Domestic Taxpayers: Mixed position (moderate/constrained) — pay taxes but also benefit from public services; constrained by tax obligation but benefit from coordination function
 *   - Entrenched Constituencies (defense, agriculture, wealthy retirees): Secondary beneficiary (institutional/arbitrage) — protect entitlements and directed spending through organized political power
 *   - Fiscal Reform Coalition: Organized opposition (organized/constrained) — watchdog groups, fiscal conservatives, intergenerational justice advocates see dysfunction but face organized opposition to reform
 *   - Budget Process Institutions: Institutional actor (institutional/arbitrage) — appropriations committees, CBO, OMB maintain ritual authority while losing real control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_budgeting_mechanism_failure, 0.58).
domain_priors:suppression_score(federal_budgeting_mechanism_failure, 0.65).
domain_priors:theater_ratio(federal_budgeting_mechanism_failure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_budgeting_mechanism_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_budgeting_mechanism_failure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federal_budgeting_mechanism_failure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_budgeting_mechanism_failure, tangled_rope).
narrative_ontology:human_readable(federal_budgeting_mechanism_failure, "Federal Budgeting Mechanism Failure").
narrative_ontology:topic_domain(federal_budgeting_mechanism_failure, "political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(federal_budgeting_mechanism_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_budgeting_mechanism_failure, executive_branch_discretion).
narrative_ontology:constraint_beneficiary(federal_budgeting_mechanism_failure, legislative_committees_with_jurisdiction).
narrative_ontology:constraint_beneficiary(federal_budgeting_mechanism_failure, entrenched_constituencies).
narrative_ontology:constraint_victim(federal_budgeting_mechanism_failure, fiscal_discipline).
narrative_ontology:constraint_victim(federal_budgeting_mechanism_failure, long_term_planning_capacity).
narrative_ontology:constraint_victim(federal_budgeting_mechanism_failure, intergenerational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Trapped by inherited debt and structural deficits. No exit option: they must inherit the constraint and its costs. Bears full extraction burden with no voice in current budget decisions. The mechanism ensures benefits flow to present, costs to future.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC TAXPAYERS (TANGLED ROPE) — Constrained by tax obligation (cannot opt out of national tax system without leaving jurisdiction), but also benefit from public services funded by the budget. Extraction exists (deficit spending socializes costs while concentrating benefits) but coordination function persists (shared infrastructure, security, welfare). Constrained exit drives moderate chi.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS / EXECUTIVE (ROPE) — Benefits from flexibility to appropriate funds without binding long-term constraints. The mechanism provides coordination function (allocates resources, enables response to crises) while concentrating power in budget-making bodies. Exit option of 'arbitrage' reflects ability to shift blame or defer hard choices. Net beneficiary position: can approve spending that benefits constituencies while deferring fiscal consequence.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: VULNERABLE POPULATIONS / DEPENDENT GROUPS (SNARE) — Trapped by dependence on discretionary budget items (SNAP, housing assistance, disability support). No exit option: cannot leave system. When budget constraints tighten, extraction mechanism targets the powerless (means testing, benefit cuts) while protecting entitlements to powerful constituencies. Maximum experienced extraction.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: BUDGET PROCESS RITUAL (PITON) — The formal budget process (appropriations committees, scoring, baseline assumptions, reconciliation) persists as largely performative theater. Theater ratio (0.68) reflects that: actual spending is driven by mandatory entitlements and continuing resolutions, not the annual authorization process; budget constraints are routinely violated through supplemental appropriations; long-term fiscal sustainability is not functionally enforced. The ritual maintains legitimacy of the process while its real control mechanisms have atrophied.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL REFORM ADVOCATES (TANGLED ROPE) — Organized agents (watchdog groups, fiscal conservatives, intergenerational justice advocates) see genuine coordination function (budget allocates resources, enables state function) alongside extraction mechanism (deficit spending, intergenerational transfer of costs). Constrained by political economy: reform proposals face organized opposition from beneficiary constituencies. See both function and dysfunction; have agency but face high resistance to exit from current mechanism.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational scope, the federal budgeting failure appears to be an immutable structural property of democratic governance: democracy requires representation of current preferences; deficit spending is the inevitable result of representing those preferences without binding constraint; structural reform is impossible because it requires those in power to limit their own power. This perspective risks naturalizing a contingent institutional arrangement as an immutable law.
constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_budgeting_mechanism_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_budgeting_mechanism_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_budgeting_mechanism_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_budgeting_mechanism_failure, TR),
    TR >= 0.70.

:- end_tests(federal_budgeting_mechanism_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables deficit spending that provides immediate benefits (stimulus, tax cuts, entitlements) to present constituencies while deferring costs (debt service, future adjustment, inflation) to future generations. The extraction is substantial but not maximal because some genuine coordination function persists (the mechanism allocates resources, enables crisis response, maintains public goods provision). The value reflects the balance: 35 years ago when constraints were binding, extractiveness was 0.35 (more coordination than extraction); today with constraints routinely violated, it is 0.58 (more extraction than initially meets the eye, though coordination function remains). Suppression (0.65): Moderate-high. Barriers to fiscal adjustment include: mandatory spending is path-dependent and politically difficult to restructure; tax increases face organized opposition; economic consequences of deficit reduction (austerity, unemployment) create political costs; future generations have no political voice; information about fiscal sustainability is complex and abstracted. Suppression is not maximal (0.80+) because reform coalitions exist and fiscal constraints could theoretically be imposed; it reflects real but not insurmountable barriers. Theater ratio (0.68): High and increasing. The budget process (annual appropriations, committee authorization, scoring rules) persists as ritual while actual control mechanisms have atrophied. Continuing resolutions bypass appropriations; supplemental appropriations waive caps; baseline assumptions are updated to justify growth; dynamic scoring enables partisan adjustments. The theater has increased over the interval (0.42 → 0.68) as the gap between formal process and actual allocation has widened.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between beneficiary and victim perspectives. Institutional beneficiaries (Congress) see rope (low extraction, genuine coordination) because arbitrage exit and beneficiary status produce d ≈ 0.20 → negative χ. Powerless victims (future generations) see snare (high extraction, no coordination) because trapped exit and victim status produce d ≈ 0.95 → χ ≈ 0.82. The gap is structural: the constraint concentrates decision power in the present, extraction costs in the future. Moderate taxpayers see tangled rope (mixed) because they occupy both beneficiary (public services) and victim (tax burden, inflation) positions simultaneously, with constrained exit. The coalition of fiscal reformers sees tangled rope + constrained agency (organized power trying to reform a mechanism that serves powerful constituencies). The budget process sees piton because the theater has risen (0.42 → 0.68) while real control has fallen — the ritual persists through institutional inertia, not functional necessity. The analytical observer's mountain perspective risks false summit: naturalizing as 'immutable feature of democracy' what is actually a contingent choice to allow deficit spending.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from structural position within the extraction flow. Congress/executive (beneficiaries with arbitrage exit) derive low d (≈0.15-0.25) → f(d) ≈ 0.0-0.2 → negative or minimal χ. Future generations (victims with trapped exit) derive high d (≈0.95) → f(d) ≈ 1.42 → high χ. Taxpayers (both beneficiary and victim, constrained exit) derive mid-high d (≈0.65-0.75) → f(d) ≈ 1.00-1.15 → moderate χ. Dependent populations (victims with trapped exit) derive very high d (≈0.98) → f(d) ≈ 1.47 → maximum χ. Scope modifier σ(S) = 1.0 (national scope). Effective extraction χ = ε × f(d) × σ(S): for beneficiary Congress, χ ≈ 0.58 × 0.1 × 1.0 ≈ 0.058; for victim future generations, χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82; for dependent populations, χ ≈ 0.58 × 1.47 × 1.0 ≈ 0.85. These χ values explain why perspectives with trapped exit see snare (χ ≥ 0.66) while beneficiaries see rope (χ ≤ 0.35). The directionality derivation is automatic from beneficiary/victim declarations and exit options — no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The federal budgeting mechanism fails mandatrophy resolution because extractiveness (0.58) exceeds 0.46, requiring both measurements and omegas. However, the constraint does NOT exceed 0.70, so mandatrophy_resolved is not required. The classification is TANGLED ROPE, not SNARE, because: (1) beneficiaries array is populated (executive branch discretion, legislative committees, entrenched constituencies) — genuine beneficiaries exist; (2) victims array is populated (fiscal discipline, long-term planning capacity, intergenerational equity) — extraction targets are identified; (3) requires_active_enforcement is true — the constraint requires Congress to appropriate funds and executive to spend them; (4) coordination function persists: the mechanism allocates resources, enables response to crises, provides public goods. The constraint is NOT snare (pure extraction) because some coordination function remains, some beneficiaries benefit through legitimate public service provision, and some victims (taxpayers) also benefit from the services funded. The omegas address: (1) whether the mechanism is designed to extract or whether it is politically chosen; (2) whether entitlements are real constraints or disguised transfers; (3) at what debt level the constraint transitions from tangled rope to snare; (4) whether fiscal constraint is inherently incompatible with democracy; (5) whether extraction is concentrated on the powerless. These omegas resolve whether the tangled rope classification is stable or whether it degrades toward snare as extractiveness increases (0.35 → 0.58 → 0.62 in measurements) and theater ratio rises (0.42 → 0.72). The measurements show acceleration in theater ratio and extractiveness, suggesting the constraint is drifting toward higher χ and higher snare risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deficit_attribution_mechanism,
    'Is deficit accumulation driven by structural design failure or by deliberate political choice to delay adjustment costs?',
    'Comparative institutional analysis: how many democracies maintain balanced budget rules? Do they achieve better fiscal outcomes? Historical analysis of deficit accumulation: did it accelerate after Gramm-Rudman-Hollings repeal (2002) and PAYGO sunset (2008), indicating mechanism degradation or deliberate removal of constraints?',
    'If structural failure: classify entire mechanism as snare (inevitable extraction). If deliberate choice: classify as tangled rope or rope depending on coordination function retained. If mechanism could be repaired: scaffold classification becomes viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_attribution_mechanism, empirical, 'Whether deficit accumulation is structural design failure or deliberate political choice').

omega_variable(
    entitlement_constraint_reality,
    'Are entitlements (Social Security, Medicare) constraints on the budget or disguised transfers that could be restructured if political will existed?',
    'Legal analysis of entitlement modification procedures; case studies of entitlement changes (Medicare Part D, Social Security benefit cuts in Australia/Chile); international comparison of countries that restructured obligatory spending. If restructuring is legally possible but politically blocked, identify the blocking coalition.',
    'If genuinely constrained: budget mechanism failure is inevitable (mountain or snare). If politically chosen: mechanism could be reformed via coalition building, enabling rope or scaffold classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entitlement_constraint_reality, empirical, 'Whether entitlements are immutable constraints or politically defended choices').

omega_variable(
    debt_trajectory_unsustainability,
    'At what debt-to-GDP ratio does the constraint transition from tangled rope (mixed coordination/extraction) to snare (pure extraction via crowding out or fiscal crisis)?',
    'Cross-national empirical analysis: identify countries that experienced fiscal crises (>90% debt-to-GDP, accelerating interest rates, loss of market access). Correlate crisis onset with prior budget mechanism characteristics. Model US trajectory under baseline assumptions vs reform scenarios.',
    'If US is on path to crisis: effective extraction rises toward snare threshold. Current tangled rope classification may degrade to snare within 10-20 years. Determines sunset timeline for reform scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_trajectory_unsustainability, empirical, 'Debt-to-GDP threshold at which constraint becomes extractively dominant').

omega_variable(
    democratic_representation_incompatibility,
    'Is fiscal constraint incompatible with democratic representation of current preferences, making reform politically impossible?',
    'Comparative analysis: democracies with binding fiscal rules (Germany''s constitutional debt brake, Switzerland''s cantonal limits). Do they enforce constraints? Do voters accept them? Historical analysis: countries that imposed fiscal discipline — did it require authoritarian or technocratic governance?',
    'If incompatible: mountain classification is correct — the constraint is an immutable feature of democratic governance. If compatible: reform is possible, escalating to scaffold or rope. Determines whether future generations are constitutionally locked into extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_representation_incompatibility, conceptual, 'Whether fiscal constraint is inherently incompatible with democratic preferences').

omega_variable(
    extraction_beneficiary_concentration,
    'Does the constraint concentrate extraction on powerless agents (future generations, dependent populations) while protecting powerful constituencies (defense contractors, wealthy entitlements)?',
    'Incidence analysis: Who benefits from deficit spending (stimulus timing, sector targeting, tax expenditures)? Who bears costs (future interest payments, inflation, currency debasement)? Cross-generational distribution: do present generations capture benefits while future generations bear costs?',
    'If asymmetric: confirms snare classification for powerless perspectives. If symmetric: mechanism is more rope than snare. Determines whether the constraint is fundamentally extractive (snare) or mixed (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_concentration, empirical, 'Concentration of extraction on powerless vs powerful constituencies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_budgeting_mechanism_failure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedbudg_tr_t0, federal_budgeting_mechanism_failure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fedbudg_tr_t10, federal_budgeting_mechanism_failure, theater_ratio, 10, 0.58).
narrative_ontology:measurement(fedbudg_tr_t20, federal_budgeting_mechanism_failure, theater_ratio, 20, 0.68).
narrative_ontology:measurement(fedbudg_tr_t30, federal_budgeting_mechanism_failure, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(fedbudg_be_t0, federal_budgeting_mechanism_failure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fedbudg_be_t10, federal_budgeting_mechanism_failure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fedbudg_be_t20, federal_budgeting_mechanism_failure, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fedbudg_be_t30, federal_budgeting_mechanism_failure, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_budgeting_mechanism_failure, resource_allocation).
narrative_ontology:affects_constraint(federal_budgeting_mechanism_failure, monetary_policy_constraint).
narrative_ontology:affects_constraint(federal_budgeting_mechanism_failure, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(federal_budgeting_mechanism_failure, intergenerational_inequity_mechanism).

% DUAL FORMULATION NOTE:
% The federal budgeting mechanism failure is downstream of institutional design choices (removing Gramm-Rudman-Hollings constraints, normalizing continuing resolutions) but upstream of fiscal crisis risk and monetary policy distortion. This story focuses on the constraint structure itself. The dual formulations are: (1) budgeting as coordination problem (rope view: allocating scarce resources) vs (2) budgeting as extraction mechanism (snare view: shifting costs to future). The network affects constraints in monetary policy (interest rates rise from debt service) and sovereign debt sustainability (debt trajectory determines whether fiscal adjustment is possible). The constraint also intersects with intergenerational equity as a structural upstream driver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_budgeting_mechanism_failure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
